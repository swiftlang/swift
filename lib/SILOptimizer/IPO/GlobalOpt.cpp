//===--- GlobalOpt.cpp - Optimize global initializers ---------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "globalopt"
#include "swift/Demangling/Demangle.h"
#include "swift/SIL/CFG.h"
#include "swift/SIL/DebugUtils.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SILOptimizer/Analysis/ColdBlockInfo.h"
#include "swift/SILOptimizer/Analysis/DominanceAnalysis.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/Local.h"
#include "swift/Demangling/Demangler.h"
#include "swift/Demangling/ManglingMacros.h"
#include "llvm/ADT/MapVector.h"
#include "llvm/ADT/SCCIterator.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "swift/AST/ASTMangler.h"
using namespace swift;

namespace {
/// Optimize the placement of global initializers.
///
/// TODO:
///
/// - Analyze the module to move initializers to the module's public
///   entry points.
///
/// - Convert trivial initializers to static initialization. This requires
///   serializing globals.
///
/// - For global "lets", generate addressors that return by value. If we also
///  converted to a static initializer, then remove the load from the addressor.
///
/// - When the addressor is local to the module, be sure it is inlined to allow
///   constant propagation in case of statically initialized "lets".
class SILGlobalOpt {
  SILModule *Module;
  DominanceAnalysis* DA;
  bool HasChanged = false;

  // Map each global initializer to a list of call sites.
  typedef SmallVector<ApplyInst *, 4> GlobalInitCalls;
  typedef SmallVector<LoadInst *, 4> GlobalLoads;
  llvm::MapVector<SILFunction*, GlobalInitCalls> GlobalInitCallMap;

  // The following mappings are used if this is a compilation
  // in scripting mode and global variables are accessed without
  // addressors.

  // Map each global let variable to a set of loads from it.
  llvm::MapVector<SILGlobalVariable*, GlobalLoads> GlobalLoadMap;
  // Map each global let variable to the store instruction which initializes it.
  llvm::MapVector<SILGlobalVariable*, StoreInst *> GlobalVarStore;
  // Variables in this set should not be processed by this pass
  // anymore.
  llvm::SmallPtrSet<SILGlobalVariable*, 16> GlobalVarSkipProcessing;

  // Mark any block that this pass has determined to be inside a loop.
  llvm::DenseSet<SILBasicBlock*> LoopBlocks;
  // Mark any functions for which loops have been analyzed.
  llvm::DenseSet<SILFunction*> LoopCheckedFunctions;
  // Keep track of cold blocks.
  ColdBlockInfo ColdBlocks;

  NominalTypeDecl *ArrayDecl;
  int GlobIdx = 0;

  // Whether we see a "once" call to callees that we currently don't handle.
  bool UnhandledOnceCallee = false;
  // Record number of times a globalinit_func is called by "once".
  llvm::DenseMap<SILFunction*, unsigned> InitializerCount;
public:
  SILGlobalOpt(SILModule *M, DominanceAnalysis *DA)
      : Module(M), DA(DA), ColdBlocks(DA),
        ArrayDecl(M->getASTContext().getArrayDecl()) {}

  bool run();

protected:
  void collectGlobalInitCall(ApplyInst *AI);
  void collectGlobalLoad(LoadInst *SI, SILGlobalVariable *SILG);
  void collectGlobalStore(StoreInst *SI, SILGlobalVariable *SILG);
  void collectGlobalAccess(GlobalAddrInst *GAI);

  bool isCOWType(SILType type) {
    return type.getNominalOrBoundGenericNominal() == ArrayDecl;
  }

  bool isValidUseOfObject(SILInstruction *Val, bool isCOWObject,
                          ApplyInst **FindStringCall = nullptr);

  bool getObjectInitVals(SILValue Val,
                         llvm::DenseMap<VarDecl *, StoreInst *> &MemberStores,
                         llvm::SmallVectorImpl<StoreInst *> &TailStores,
                         ApplyInst **FindStringCall);
  bool handleTailAddr(int TailIdx, SILInstruction *I,
                      llvm::SmallVectorImpl<StoreInst *> &TailStores);

  void
  optimizeObjectAllocation(AllocRefInst *ARI,
                           llvm::SmallVector<SILInstruction *, 4> &ToRemove);
  void replaceFindStringCall(ApplyInst *FindStringCall);

  SILGlobalVariable *getVariableOfGlobalInit(SILFunction *AddrF);
  bool isInLoop(SILBasicBlock *CurBB);
  void placeInitializers(SILFunction *InitF, ArrayRef<ApplyInst*> Calls);

  // Update UnhandledOnceCallee and InitializerCount by going through all "once"
  // calls.
  void collectOnceCall(BuiltinInst *AI);
  // Set the static initializer and remove "once" from addressor if a global can
  // be statically initialized.
  void optimizeInitializer(SILFunction *AddrF, GlobalInitCalls &Calls);
  void optimizeGlobalAccess(SILGlobalVariable *SILG, StoreInst *SI);
  // Replace loads from a global variable by the known value.
  void replaceLoadsByKnownValue(BuiltinInst *CallToOnce,
                                SILFunction *AddrF,
                                SILFunction *InitF,
                                SILGlobalVariable *SILG,
                                SingleValueInstruction *InitVal,
                                GlobalInitCalls &Calls);
};

/// Helper class to copy only a set of SIL instructions providing in the
/// constructor.
class InstructionsCloner : public SILClonerWithScopes<InstructionsCloner> {
  friend class SILInstructionVisitor<InstructionsCloner>;
  friend class SILCloner<InstructionsCloner>;

  ArrayRef<SILInstruction *> Insns;

  protected:
  SILBasicBlock *FromBB, *DestBB;

  public:
  // A map of old to new available values.
  SmallVector<std::pair<ValueBase *, SILValue>, 16> AvailVals;

  InstructionsCloner(SILFunction &F,
                     ArrayRef<SILInstruction *> Insns,
                     SILBasicBlock *Dest = nullptr)
    : SILClonerWithScopes(F), Insns(Insns), FromBB(nullptr), DestBB(Dest) {}

  void process(SILInstruction *I) { visit(I); }

  SILBasicBlock *remapBasicBlock(SILBasicBlock *BB) { return BB; }

  SILValue remapValue(SILValue Value) {
    return SILCloner<InstructionsCloner>::remapValue(Value);
  }

  void postProcess(SILInstruction *Orig, SILInstruction *Cloned) {
    DestBB->push_back(Cloned);
    SILClonerWithScopes<InstructionsCloner>::postProcess(Orig, Cloned);
    auto origResults = Orig->getResults(), clonedResults = Cloned->getResults();
    assert(origResults.size() == clonedResults.size());
    for (auto i : indices(origResults))
      AvailVals.push_back(std::make_pair(origResults[i], clonedResults[i]));
  }

  // Clone all instructions from Insns into DestBB
  void clone() {
    for (auto I : Insns)
      process(I);
  }
};

} // end anonymous namespace

/// If this is a call to a global initializer, map it.
void SILGlobalOpt::collectGlobalInitCall(ApplyInst *AI) {
  SILFunction *F = AI->getReferencedFunction();
  if (!F || !F->isGlobalInit())
    return;

  GlobalInitCallMap[F].push_back(AI);
}

/// If this is a read from a global let variable, map it.
void SILGlobalOpt::collectGlobalLoad(LoadInst *LI, SILGlobalVariable *SILG) {
  assert(SILG);
  //assert(SILG->isLet());

  // This is read from a let variable.
  // Figure out if the value of this variable is statically known.
  GlobalLoadMap[SILG].push_back(LI);
}

/// Remove an unused global token used by once calls.
static void removeToken(SILValue Op) {
  if (auto *ATPI = dyn_cast<AddressToPointerInst>(Op)) {
    Op = ATPI->getOperand();
    if (ATPI->use_empty())
      ATPI->eraseFromParent();
  }

  if (auto *GAI = dyn_cast<GlobalAddrInst>(Op)) {
    auto *Global = GAI->getReferencedGlobal();
    // If "global_addr token" is used more than one time, bail.
    if (!(GAI->use_empty() || GAI->hasOneUse()))
      return;
    // If it is not a *_token global variable, bail.
    if (!Global || Global->getName().find("_token") == StringRef::npos)
      return;
    GAI->getModule().eraseGlobalVariable(Global);
    GAI->replaceAllUsesWithUndef();
    GAI->eraseFromParent();
  }
}

static std::string mangleGetter(VarDecl *varDecl) {
  Mangle::ASTMangler Mangler;
  return Mangler.mangleGlobalGetterEntity(varDecl);
}

/// Generate getter from the initialization code whose
/// result is stored by a given store instruction.
static SILFunction *genGetterFromInit(StoreInst *Store,
                                      SILGlobalVariable *SILG) {
  auto *varDecl = SILG->getDecl();
  auto getterName = mangleGetter(varDecl);

  // Check if a getter was generated already.
  if (auto *F = Store->getModule().lookUpFunction(getterName))
    return F;

  // Find the code that performs the initialization first.
  // Recursively walk the SIL value being assigned to the SILG.

  auto V = Store->getSrc();

  SmallVector<SILInstruction *, 8> ReverseInsns;
  SmallVector<SILInstruction *, 8> Insns;
  ReverseInsns.push_back(Store);
  ReverseInsns.push_back(dyn_cast<SingleValueInstruction>(Store->getDest()));
  if (!analyzeStaticInitializer(V, ReverseInsns))
    return nullptr;

  // Produce a correct order of instructions.
  while (!ReverseInsns.empty()) {
    Insns.push_back(ReverseInsns.pop_back_val());
  }

  // Generate a getter from the global init function without side-effects.
  auto refType = varDecl->getInterfaceType()->getCanonicalType();
  // Function takes no arguments and returns refType
  SILResultInfo Results[] = { SILResultInfo(refType, ResultConvention::Owned) };
  SILFunctionType::ExtInfo EInfo;
  EInfo = EInfo.withRepresentation(SILFunctionType::Representation::Thin);
  auto LoweredType = SILFunctionType::get(nullptr, EInfo,
      SILCoroutineKind::None, ParameterConvention::Direct_Owned,
      /*params*/ {}, /*yields*/ {}, Results, None,
      Store->getModule().getASTContext());
  auto *GetterF = Store->getModule().getOrCreateFunction(
      Store->getLoc(),
      getterName, SILLinkage::Private, LoweredType,
      IsBare_t::IsBare, IsTransparent_t::IsNotTransparent,
      IsSerialized_t::IsSerialized);
  GetterF->setDebugScope(Store->getFunction()->getDebugScope());
  if (!Store->getFunction()->hasQualifiedOwnership())
    GetterF->setUnqualifiedOwnership();
  auto *EntryBB = GetterF->createBasicBlock();
  // Copy instructions into GetterF
  InstructionsCloner Cloner(*GetterF, Insns, EntryBB);
  Cloner.clone();
  GetterF->setInlined();

  // Find the store instruction and turn it into return.
  // Remove the alloc_global instruction.
  auto BB = EntryBB;
  SILValue Val;
  for (auto II = BB->begin(), E = BB->end(); II != E;) {
    auto &I = *II++;
    if (isa<AllocGlobalInst>(&I)) {
      I.eraseFromParent();
      continue;
    }
    if (auto *SI = dyn_cast<StoreInst>(&I)) {
      Val = SI->getSrc();
      SILBuilderWithScope B(SI);
      B.createReturn(SI->getLoc(), Val);
      eraseUsesOfInstruction(SI);
      recursivelyDeleteTriviallyDeadInstructions(SI, true);
      return GetterF;
    }
  }

  Store->getModule().getFunctionList().addNodeToList(GetterF);

  return GetterF;
}

/// If this is a read from a global let variable, map it.
void SILGlobalOpt::collectGlobalStore(StoreInst *SI, SILGlobalVariable *SILG) {

  if (GlobalVarStore.count(SILG)) {
    // There is more then one assignment to a given global variable.
    // Therefore we don't know its value.
    GlobalVarSkipProcessing.insert(SILG);
  }

  // Figure out if the value of this variable is statically known.
  GlobalVarStore[SILG] = SI;
}

/// Return the callee of a once call.
static SILFunction *getCalleeOfOnceCall(BuiltinInst *BI) {
  assert(BI->getNumOperands() == 2 && "once call should have 2 operands.");

  auto Callee = BI->getOperand(1);
  assert(Callee->getType().castTo<SILFunctionType>()->getRepresentation()
           == SILFunctionTypeRepresentation::CFunctionPointer &&
         "Expected C function representation!");

  if (auto *FR = dyn_cast<FunctionRefInst>(Callee))
    return FR->getReferencedFunction();

  return nullptr;
}

/// Update UnhandledOnceCallee and InitializerCount by going through all "once"
/// calls.
void SILGlobalOpt::collectOnceCall(BuiltinInst *BI) {
  if (UnhandledOnceCallee)
    return;

  const BuiltinInfo &Builtin = Module->getBuiltinInfo(BI->getName());
  if (Builtin.ID != BuiltinValueKind::Once)
    return;

  SILFunction *Callee = getCalleeOfOnceCall(BI);
  if (!Callee) {
    DEBUG(llvm::dbgs() << "GlobalOpt: unhandled once callee\n");
    UnhandledOnceCallee = true;
    return;
  }
  if (!Callee->getName().startswith("globalinit_"))
    return;

  // We currently disable optimizing the initializer if a globalinit_func
  // is called by "once" from multiple locations.
  if (!BI->getFunction()->isGlobalInit())
    // If a globalinit_func is called by "once" from a function that is not
    // an addressor, we set count to 2 to disable optimizing the initializer.
    InitializerCount[Callee] = 2;
  else
    InitializerCount[Callee]++;
}

/// return true if this block is inside a loop.
bool SILGlobalOpt::isInLoop(SILBasicBlock *CurBB) {
  SILFunction *F = CurBB->getParent();
  // Catch the common case in which we've already hoisted the initializer.
  if (CurBB == &F->front())
    return false;

  if (LoopCheckedFunctions.insert(F).second) {
    for (auto I = scc_begin(F); !I.isAtEnd(); ++I) {
      if (I.hasLoop())
        for (SILBasicBlock *BB : *I)
          LoopBlocks.insert(BB);
    }
  }
  return LoopBlocks.count(CurBB);
}

/// Returns true if the block \p BB is terminated with a cond_br based on an
/// availability check.
static bool isAvailabilityCheck(SILBasicBlock *BB) {
  auto *CBR = dyn_cast<CondBranchInst>(BB->getTerminator());
  if (!CBR)
    return false;
  
  auto *AI = dyn_cast<ApplyInst>(CBR->getCondition());
  if (!AI)
    return false;

  SILFunction *F = AI->getReferencedFunction();
  if (!F || !F->hasSemanticsAttrs())
    return false;

  return F->hasSemanticsAttrThatStartsWith("availability");
}

/// Returns true if there are any availability checks along the dominator tree
/// from \p From to \p To.
static bool isAvailabilityCheckOnDomPath(SILBasicBlock *From, SILBasicBlock *To,
                                         DominanceInfo *DT) {
  if (From == To)
    return false;

  auto *Node = DT->getNode(To)->getIDom();
  for (;;) {
    SILBasicBlock *BB = Node->getBlock();
    if (isAvailabilityCheck(BB))
      return true;
    if (BB == From)
      return false;
    Node = Node->getIDom();
    assert(Node && "Should have hit To-block");
  }
}

/// Optimize placement of initializer calls given a list of calls to the
/// same initializer. All original initialization points must be dominated by
/// the final initialization calls.
///
/// The current heuristic hoists all initialization points within a function to
/// a single dominating call in the outer loop preheader.
void SILGlobalOpt::placeInitializers(SILFunction *InitF,
                                     ArrayRef<ApplyInst*> Calls) {
  DEBUG(llvm::dbgs() << "GlobalOpt: calls to "
        << Demangle::demangleSymbolAsString(InitF->getName())
        << " : " << Calls.size() << "\n");
  // Map each initializer-containing function to its final initializer call.
  llvm::DenseMap<SILFunction*, ApplyInst*> ParentFuncs;
  for (auto *AI : Calls) {
    assert(AI->getNumArguments() == 0 && "ill-formed global init call");
    assert(cast<FunctionRefInst>(AI->getCallee())->getReferencedFunction()
           == InitF && "wrong init call");

    SILFunction *ParentF = AI->getFunction();
    DominanceInfo *DT = DA->get(ParentF);
    auto PFI = ParentFuncs.find(ParentF);
    ApplyInst *HoistAI = nullptr;
    if (PFI != ParentFuncs.end()) {
      // Found a replacement for this init call.
      // Ensure the replacement dominates the original call site.
      ApplyInst *CommonAI = PFI->second;
      assert(cast<FunctionRefInst>(CommonAI->getCallee())
             ->getReferencedFunction() == InitF &&
             "ill-formed global init call");
      SILBasicBlock *DomBB =
        DT->findNearestCommonDominator(AI->getParent(), CommonAI->getParent());
      
      // We must not move initializers around availability-checks.
      if (!isAvailabilityCheckOnDomPath(DomBB, CommonAI->getParent(), DT)) {
        if (DomBB != CommonAI->getParent()) {
          CommonAI->moveBefore(&*DomBB->begin());
          placeFuncRef(CommonAI, DT);
          
          // Try to hoist the existing AI again if we move it to another block,
          // e.g. from a loop exit into the loop.
          HoistAI = CommonAI;
        }
        AI->replaceAllUsesWith(CommonAI);
        AI->eraseFromParent();
        HasChanged = true;
      }
    } else {
      ParentFuncs[ParentF] = AI;
      
      // It's the first time we found a call to InitF in this function, so we
      // try to hoist it out of any loop.
      HoistAI = AI;
    }
    if (HoistAI) {
      // Move this call to the outermost loop preheader.
      SILBasicBlock *BB = HoistAI->getParent();
      typedef llvm::DomTreeNodeBase<SILBasicBlock> DomTreeNode;
      DomTreeNode *Node = DT->getNode(BB);
      while (Node) {
        SILBasicBlock *DomParentBB = Node->getBlock();
        if (isAvailabilityCheck(DomParentBB)) {
          DEBUG(llvm::dbgs() << "  don't hoist above availability check at bb"
                             << DomParentBB->getDebugID() << "\n");
          break;
        }
        BB = DomParentBB;
        if (!isInLoop(BB))
          break;
        Node = Node->getIDom();
      }
      if (BB == HoistAI->getParent()) {
        // BB is either unreachable or not in a loop.
        DEBUG(llvm::dbgs() << "  skipping (not in a loop): " << *HoistAI
              << "  in " << HoistAI->getFunction()->getName() << "\n");
      }
      else {
        DEBUG(llvm::dbgs() << "  hoisting: " << *HoistAI
              << "  in " << HoistAI->getFunction()->getName() << "\n");
        HoistAI->moveBefore(&*BB->begin());
        placeFuncRef(HoistAI, DT);
        HasChanged = true;
      }
    }
  }
}

/// Create a getter function from the initializer function.
static SILFunction *genGetterFromInit(SILFunction *InitF, VarDecl *varDecl) {
  // Generate a getter from the global init function without side-effects.

  auto getterName = mangleGetter(varDecl);

  // Check if a getter was generated already.
  if (auto *F = InitF->getModule().lookUpFunction(getterName))
    return F;

  auto refType = varDecl->getInterfaceType()->getCanonicalType();
  // Function takes no arguments and returns refType
  SILResultInfo Results[] = { SILResultInfo(refType, ResultConvention::Owned) };
  SILFunctionType::ExtInfo EInfo;
  EInfo = EInfo.withRepresentation(SILFunctionType::Representation::Thin);
  auto LoweredType = SILFunctionType::get(nullptr, EInfo,
      SILCoroutineKind::None, ParameterConvention::Direct_Owned,
      /*params*/ {}, /*yields*/ {}, Results, None,
      InitF->getASTContext());
  auto *GetterF = InitF->getModule().getOrCreateFunction(
      InitF->getLocation(),
      getterName, SILLinkage::Private, LoweredType,
      IsBare_t::IsBare, IsTransparent_t::IsNotTransparent,
      IsSerialized_t::IsSerialized);
  if (!InitF->hasQualifiedOwnership())
    GetterF->setUnqualifiedOwnership();

  auto *EntryBB = GetterF->createBasicBlock();
  // Copy InitF into GetterF
  BasicBlockCloner Cloner(&*InitF->begin(), EntryBB, /*WithinFunction=*/false);
  Cloner.clone();
  GetterF->setInlined();

  // Find the store instruction
  auto BB = EntryBB;
  SILValue Val;
  SILInstruction *Store;
  for (auto II = BB->begin(), E = BB->end(); II != E;) {
    auto &I = *II++;
    if (isa<AllocGlobalInst>(&I)) {
      I.eraseFromParent();
      continue;
    }

    if (auto *SI = dyn_cast<StoreInst>(&I)) {
      Val = SI->getSrc();
      Store = SI;
      continue;
    }

    if (auto *RI = dyn_cast<ReturnInst>(&I)) {
      SILBuilderWithScope B(RI);
      B.createReturn(RI->getLoc(), Val);
      eraseUsesOfInstruction(RI);
      recursivelyDeleteTriviallyDeadInstructions(RI, true);
      recursivelyDeleteTriviallyDeadInstructions(Store, true);
      return GetterF;
    }
  }
  InitF->getModule().getFunctionList().addNodeToList(GetterF);

  return GetterF;
}

/// Find the globalinit_func by analyzing the body of the addressor.
static SILFunction *findInitializer(SILModule *Module, SILFunction *AddrF,
                                    BuiltinInst *&CallToOnce) {
  // We only handle a single SILBasicBlock for now.
  if (AddrF->size() != 1)
    return nullptr;

  CallToOnce = nullptr;
  SILBasicBlock *BB = &AddrF->front();
  for (auto &I : *BB) {
    // Find the builtin "once" call.
    if (auto *BI = dyn_cast<BuiltinInst>(&I)) {
      const BuiltinInfo &Builtin = Module->getBuiltinInfo(BI->getName());
      if (Builtin.ID != BuiltinValueKind::Once)
        continue;

      // Bail if we have multiple "once" calls in the addressor.
      if (CallToOnce)
        return nullptr;

      CallToOnce = BI;
    }
  }
  if (!CallToOnce)
    return nullptr;
  return getCalleeOfOnceCall(CallToOnce);
}

/// Checks if a given global variable is assigned only once.
static bool isAssignedOnlyOnceInInitializer(SILGlobalVariable *SILG) {
  if (SILG->isLet())
    return true;
  // TODO: If we can prove that a given global variable
  // is assigned only once, during initialization, then
  // we can treat it as if it is a let.
  // If this global is internal or private, it should be
  return false;
}

/// Replace load sequence which may contain
/// a chain of struct_element_addr followed by a load.
/// The sequence is traversed starting from the load
/// instruction.
static SILValue convertLoadSequence(SILValue oldSequence,
                                    SILValue newRootValue,
                                    SILBuilder &B) {

  if (isa<GlobalAddrInst>(oldSequence))
    return newRootValue;

  if (auto *LI = dyn_cast<LoadInst>(oldSequence)) {
    auto newValue = convertLoadSequence(LI->getOperand(), newRootValue, B);
    LI->replaceAllUsesWith(newValue);
    return newValue;
  }

  // It is a series of struct_element_addr followed by load.
  if (auto *SEAI = dyn_cast<StructElementAddrInst>(oldSequence)) {
    auto newValue = convertLoadSequence(SEAI->getOperand(), newRootValue, B);
    newValue = B.createStructExtract(SEAI->getLoc(), newValue, SEAI->getField());
    return newValue;
  }

  if (auto *TEAI = dyn_cast<TupleElementAddrInst>(oldSequence)) {
    auto newValue = convertLoadSequence(TEAI->getOperand(), newRootValue, B);
    newValue = B.createTupleExtract(TEAI->getLoc(), newValue, TEAI->getFieldNo());
    return newValue;
  }

  llvm_unreachable("Unknown instruction sequence for reading from a global");
  return nullptr;
}

static SILGlobalVariable *getVariableOfStaticInitializer(SILFunction *InitFunc,
                                             SingleValueInstruction *&InitVal) {
  InitVal = nullptr;
  SILGlobalVariable *GVar = nullptr;
  // We only handle a single SILBasicBlock for now.
  if (InitFunc->size() != 1)
    return nullptr;

  SILBasicBlock *BB = &InitFunc->front();
  GlobalAddrInst *SGA = nullptr;
  bool HasStore = false;
  for (auto &I : *BB) {
    // Make sure we have a single GlobalAddrInst and a single StoreInst.
    // And the StoreInst writes to the GlobalAddrInst.
    if (isa<AllocGlobalInst>(&I) || isa<ReturnInst>(&I)
        || isa<DebugValueInst>(&I)) {
      continue;
    } else if (auto *sga = dyn_cast<GlobalAddrInst>(&I)) {
      if (SGA)
        return nullptr;
      SGA = sga;
      GVar = SGA->getReferencedGlobal();
    } else if (auto *SI = dyn_cast<StoreInst>(&I)) {
      if (HasStore || SI->getDest() != SGA)
        return nullptr;
      HasStore = true;
      SILValue value = SI->getSrc();

      // We only handle StructInst and TupleInst being stored to a
      // global variable for now.
      if (!isa<StructInst>(value) && !isa<TupleInst>(value))
        return nullptr;
      InitVal = cast<SingleValueInstruction>(value);
    } else if (!SILGlobalVariable::isValidStaticInitializerInst(&I,
                                                             I.getModule())) {
      return nullptr;
    }
  }
  if (!InitVal)
    return nullptr;
  return GVar;
}

namespace {

/// Utility class for cloning init values into the static initializer of a
/// SILGlobalVariable.
class StaticInitCloner : public SILCloner<StaticInitCloner> {
  friend class SILInstructionVisitor<StaticInitCloner>;
  friend class SILCloner<StaticInitCloner>;

  /// The number of not yet cloned operands for each instruction.
  llvm::DenseMap<SILInstruction *, int> NumOpsToClone;

  /// List of instructions for which all operands are already cloned (or which
  /// don't have any operands).
  llvm::SmallVector<SILInstruction *, 8> ReadyToClone;

public:
  StaticInitCloner(SILGlobalVariable *GVar)
      : SILCloner<StaticInitCloner>(GVar) { }

  /// Add \p InitVal and all its operands (transitively) for cloning.
  ///
  /// Note: all init values must are added, before calling clone().
  void add(SILInstruction *InitVal);

  /// Clone \p InitVal and all its operands into the initializer of the
  /// SILGlobalVariable.
  ///
  /// \return Returns the cloned instruction in the SILGlobalVariable.
  SingleValueInstruction *clone(SingleValueInstruction *InitVal);

  /// Convenience function to clone a single \p InitVal.
  static void appendToInitializer(SILGlobalVariable *GVar,
                                  SingleValueInstruction *InitVal) {
    StaticInitCloner Cloner(GVar);
    Cloner.add(InitVal);
    Cloner.clone(InitVal);
  }

protected:
  SILLocation remapLocation(SILLocation Loc) {
    return ArtificialUnreachableLocation();
  }
};

void StaticInitCloner::add(SILInstruction *InitVal) {
  // Don't schedule an instruction twice for cloning.
  if (NumOpsToClone.count(InitVal) != 0)
    return;

  ArrayRef<Operand> Ops = InitVal->getAllOperands();
  NumOpsToClone[InitVal] = Ops.size();
  if (Ops.empty()) {
    // It's an instruction without operands, e.g. a literal. It's ready to be
    // cloned first.
    ReadyToClone.push_back(InitVal);
  } else {
    // Recursively add all operands.
    for (const Operand &Op : Ops) {
      add(cast<SingleValueInstruction>(Op.get()));
    }
  }
}

SingleValueInstruction *
StaticInitCloner::clone(SingleValueInstruction *InitVal) {
  assert(NumOpsToClone.count(InitVal) != 0 && "InitVal was not added");
  // Find the right order to clone: all operands of an instruction must be
  // cloned before the instruction itself.
  while (!ReadyToClone.empty()) {
    SILInstruction *I = ReadyToClone.pop_back_val();

    // Clone the instruction into the SILGlobalVariable
    visit(I);

    // Check if users of I can now be cloned.
    for (SILValue result : I->getResults()) {
      for (Operand *Use : result->getUses()) {
        SILInstruction *User = Use->getUser();
        if (NumOpsToClone.count(User) != 0 && --NumOpsToClone[User] == 0)
          ReadyToClone.push_back(User);
      }
    }
  }
  assert(ValueMap.count(InitVal) != 0 &&
         "Could not schedule all instructions for cloning");
  return cast<SingleValueInstruction>(ValueMap[InitVal]);
}

} // end anonymous namespace

/// Replace loads from a global variable by the known value.
void SILGlobalOpt::
replaceLoadsByKnownValue(BuiltinInst *CallToOnce, SILFunction *AddrF,
                         SILFunction *InitF, SILGlobalVariable *SILG,
                         SingleValueInstruction *InitVal,
                         GlobalInitCalls &Calls) {
  assert(isAssignedOnlyOnceInInitializer(SILG) &&
         "The value of the initializer should be known at compile-time");
  assert(SILG->getDecl() &&
         "Decl corresponding to the global variable should be known");
  removeToken(CallToOnce->getOperand(0));
  eraseUsesOfInstruction(CallToOnce);
  recursivelyDeleteTriviallyDeadInstructions(CallToOnce, true);

  // Make this addressor transparent.
  AddrF->setTransparent(IsTransparent_t::IsTransparent);

  for (int i = 0, e = Calls.size(); i < e; ++i) {
    auto *Call = Calls[i];
    SILBuilderWithScope B(Call);
    SmallVector<SILValue, 1> Args;
    auto *NewAI = B.createApply(Call->getLoc(), Call->getCallee(), Args, false);
    Call->replaceAllUsesWith(NewAI);
    eraseUsesOfInstruction(Call);
    recursivelyDeleteTriviallyDeadInstructions(Call, true);
    Calls[i] = NewAI;
  }

  // Generate a getter from InitF which returns the value of the global.
  auto *GetterF = genGetterFromInit(InitF, SILG->getDecl());

  // Replace all calls of an addressor by calls of a getter .
  for (int i = 0, e = Calls.size(); i < e; ++i) {
    auto *Call = Calls[i];

    // Now find all uses of Call. They all should be loads, so that
    // we can replace it.
    bool isValid = true;
    for (auto Use : Call->getUses()) {
      if (!isa<PointerToAddressInst>(Use->getUser())) {
        isValid = false;
        break;
      }
    }

    if (!isValid)
      continue;

    SILBuilderWithScope B(Call);
    SmallVector<SILValue, 1> Args;
    auto *GetterRef = B.createFunctionRef(Call->getLoc(), GetterF);
    auto *NewAI = B.createApply(Call->getLoc(), GetterRef, Args, false);

    for (auto Use : Call->getUses()) {
      auto *PTAI = dyn_cast<PointerToAddressInst>(Use->getUser());
      assert(PTAI && "All uses should be pointer_to_address");
      for (auto PTAIUse : PTAI->getUses()) {
        replaceLoadSequence(PTAIUse->getUser(), NewAI, B);
      }
    }

    eraseUsesOfInstruction(Call);
    recursivelyDeleteTriviallyDeadInstructions(Call, true);
  }

  Calls.clear();
  StaticInitCloner::appendToInitializer(SILG, InitVal);
}

/// We analyze the body of globalinit_func to see if it can be statically
/// initialized. If yes, we set the initial value of the SILGlobalVariable and
/// remove the "once" call to globalinit_func from the addressor.
void SILGlobalOpt::optimizeInitializer(SILFunction *AddrF,
                                       GlobalInitCalls &Calls) {
  if (UnhandledOnceCallee)
    return;

  // Find the initializer and the SILGlobalVariable.
  BuiltinInst *CallToOnce;

  // If the addressor contains a single "once" call, it calls globalinit_func,
  // and the globalinit_func is called by "once" from a single location,
  // continue; otherwise bail.
  auto *InitF = findInitializer(Module, AddrF, CallToOnce);
  if (!InitF || !InitF->getName().startswith("globalinit_") ||
      InitializerCount[InitF] > 1)
    return;

  // If the globalinit_func is trivial, continue; otherwise bail.
  SingleValueInstruction *InitVal;
  SILGlobalVariable *SILG = getVariableOfStaticInitializer(InitF, InitVal);
  if (!SILG)
    return;

  DEBUG(llvm::dbgs() << "GlobalOpt: use static initializer for " <<
        SILG->getName() << '\n');

  // Remove "once" call from the addressor.
  if (!isAssignedOnlyOnceInInitializer(SILG) || !SILG->getDecl()) {
    removeToken(CallToOnce->getOperand(0));
    CallToOnce->eraseFromParent();
    StaticInitCloner::appendToInitializer(SILG, InitVal);
    HasChanged = true;
    return;
  }

  replaceLoadsByKnownValue(CallToOnce, AddrF, InitF, SILG, InitVal, Calls);
  HasChanged = true;
}

SILGlobalVariable *SILGlobalOpt::getVariableOfGlobalInit(SILFunction *AddrF) {
  if (AddrF->isGlobalInit()) {
    // If the addressor contains a single "once" call, it calls globalinit_func,
    // and the globalinit_func is called by "once" from a single location,
    // continue; otherwise bail.
    BuiltinInst *CallToOnce;
    auto *InitF = findInitializer(Module, AddrF, CallToOnce);

    if (!InitF || !InitF->getName().startswith("globalinit_")
        || InitializerCount[InitF] > 1)
      return nullptr;

    // If the globalinit_func is trivial, continue; otherwise bail.
    SingleValueInstruction *dummyInitVal;
    auto *SILG = getVariableOfStaticInitializer(InitF, dummyInitVal);
    if (!SILG || !SILG->isDefinition())
      return nullptr;

    return SILG;
  }
  return nullptr;
}

static bool canBeChangedExternally(SILGlobalVariable *SILG) {

  // Don't assume anything about globals which are imported from other modules.
  if (isAvailableExternally(SILG->getLinkage()))
    return true;

  // Use access specifiers from the declarations,
  // if possible.
  if (auto *Decl = SILG->getDecl()) {
    switch (Decl->getEffectiveAccess()) {
    case AccessLevel::Private:
    case AccessLevel::FilePrivate:
      return false;
    case AccessLevel::Internal:
      return !SILG->getModule().isWholeModule();
    case AccessLevel::Public:
    case AccessLevel::Open:
      return true;
    }
  }

  if (SILG->getLinkage() == SILLinkage::Private)
    return false;

  if (SILG->getLinkage() == SILLinkage::Hidden
      && SILG->getModule().isWholeModule()) {
    return false;
  }

  return true;
}

/// Check if instruction I is a load from instruction V or
/// or a struct_element_addr from instruction V.
/// returns instruction I if this condition holds, or nullptr otherwise.
static LoadInst *getValidLoad(SILInstruction *I, SILValue V) {
  if (auto *LI = dyn_cast<LoadInst>(I)) {
    if (LI->getOperand() == V)
      return LI;
  }

  if (auto *SEAI = dyn_cast<StructElementAddrInst>(I)) {
    if (SEAI->getOperand() == V && SEAI->hasOneUse())
      return getValidLoad(SEAI->use_begin()->getUser(), SEAI);
  }

  if (auto *TEAI = dyn_cast<TupleElementAddrInst>(I)) {
    if (TEAI->getOperand() == V && TEAI->hasOneUse())
      return getValidLoad(TEAI->use_begin()->getUser(), TEAI);
  }

  return nullptr;
}

/// If this is a read from a global let variable, map it.
void SILGlobalOpt::collectGlobalAccess(GlobalAddrInst *GAI) {
  auto *SILG = GAI->getReferencedGlobal();
  if (!SILG)
    return;

  if (!SILG->isLet()) {
    // We cannot determine the value for global variables which could be
    // changed externally at run-time.
    if (canBeChangedExternally(SILG))
      return;
  }

  if (GlobalVarSkipProcessing.count(SILG))
    return;

  if (!isSimpleType(SILG->getLoweredType(), *Module)) {
    GlobalVarSkipProcessing.insert(SILG);
    return;
  }

  // Ignore any accesses inside addressors for SILG
  auto *F = GAI->getFunction();
  auto GlobalVar = getVariableOfGlobalInit(F);
  if (GlobalVar == SILG)
    return;

  if (!SILG->getDecl())
    return;

  SILValue V = GAI;
  for (auto Use : getNonDebugUses(V)) {

    if (auto *SI = dyn_cast<StoreInst>(Use->getUser())) {
      if (SI->getDest() == GAI)
        collectGlobalStore(SI, SILG);
      continue;
    }

    if (auto *Load = getValidLoad(Use->getUser(), GAI)) {
      collectGlobalLoad(Load, SILG);
      continue;
    }

    // This global is not initialized by a simple
    // constant value at this moment.
    GlobalVarSkipProcessing.insert(SILG);
    break;
  }
}

/// Get all stored properties of a class, including it's super classes.
static void getFields(ClassDecl *Cl, SmallVectorImpl<VarDecl *> &Fields) {
  if (ClassDecl *SuperCl = Cl->getSuperclassDecl()) {
    getFields(SuperCl, Fields);
  }
  for (VarDecl *Field : Cl->getStoredProperties()) {
    Fields.push_back(Field);
  }
}

/// Check if \p V is a valid instruction for a static initializer, including
/// all its operands.
static bool isValidInitVal(SILValue V) {
  if (auto I = dyn_cast<SingleValueInstruction>(V)) {
    if (!SILGlobalVariable::isValidStaticInitializerInst(I, I->getModule()))
      return false;

    for (Operand &Op : I->getAllOperands()) {
      if (!isValidInitVal(Op.get()))
        return false;
    }
    return true;
  }
  return false;
}

/// Check if a use of an object may prevent outlining the object.
///
/// If \p isCOWObject is true, then the object reference is wrapped into a
/// COW container. Currently this is just Array<T>.
/// If a use is a call to the findStringSwitchCase semantic call, the apply
/// is returned in \p FindStringCall.
bool SILGlobalOpt::isValidUseOfObject(SILInstruction *I, bool isCOWObject,
                                      ApplyInst **FindStringCall) {
  switch (I->getKind()) {
  case SILInstructionKind::DebugValueAddrInst:
  case SILInstructionKind::DebugValueInst:
  case SILInstructionKind::LoadInst:
  case SILInstructionKind::DeallocRefInst:
  case SILInstructionKind::StrongRetainInst:
  case SILInstructionKind::StrongReleaseInst:
    return true;

  case SILInstructionKind::ReturnInst:
  case SILInstructionKind::TryApplyInst:
  case SILInstructionKind::PartialApplyInst:
  case SILInstructionKind::StoreInst:
    /// We don't have a representation for COW objects in SIL, so we do some
    /// ad-hoc testing: We can ignore uses of a COW object if any use after
    /// this will do a uniqueness checking before the object is modified.
    return isCOWObject;

  case SILInstructionKind::ApplyInst:
    if (!isCOWObject)
      return false;
    // There should only be a single call to findStringSwitchCase. But even
    // if there are multiple calls, it's not problem - we'll just optimize the
    // last one we find.
    if (cast<ApplyInst>(I)->hasSemantics("findStringSwitchCase"))
      *FindStringCall = cast<ApplyInst>(I);
    return true;

  case SILInstructionKind::StructInst:
    if (isCOWType(cast<StructInst>(I)->getType())) {
      // The object is wrapped into a COW container.
      isCOWObject = true;
    }
    break;

  case SILInstructionKind::UncheckedRefCastInst:
  case SILInstructionKind::StructElementAddrInst:
  case SILInstructionKind::AddressToPointerInst:
    assert(!isCOWObject && "instruction cannot have a COW object as operand");
    break;

  case SILInstructionKind::TupleInst:
  case SILInstructionKind::TupleExtractInst:
  case SILInstructionKind::EnumInst:
    break;

  case SILInstructionKind::StructExtractInst:
    // To be on the safe side we don't consider the object as COW if it is
    // extracted again from the COW container: the uniqueness check may be
    // optimized away in this case.
    isCOWObject = false;
    break;

  case SILInstructionKind::BuiltinInst: {
    // Handle the case for comparing addresses. This occurs when the Array
    // comparison function is inlined.
    auto *BI = cast<BuiltinInst>(I);
    BuiltinValueKind K = BI->getBuiltinInfo().ID;
    if (K == BuiltinValueKind::ICMP_EQ || K == BuiltinValueKind::ICMP_NE)
      return true;
    return false;
  }

  default:
    return false;
  }

  auto SVI = cast<SingleValueInstruction>(I);
  for (Operand *Use : getNonDebugUses(SVI)) {
    if (!isValidUseOfObject(Use->getUser(), isCOWObject, FindStringCall))
      return false;
  }
  return true;
}

/// Handle the address of a tail element.
bool SILGlobalOpt::handleTailAddr(int TailIdx, SILInstruction *TailAddr,
                              llvm::SmallVectorImpl<StoreInst *> &TailStores) {
  if (TailIdx >= 0 && TailIdx < (int)TailStores.size()) {
    if (auto *SI = dyn_cast<StoreInst>(TailAddr)) {
      if (!isValidInitVal(SI->getSrc()) || TailStores[TailIdx])
        return false;
      TailStores[TailIdx] = SI;
      return true;
    }
  }
  return isValidUseOfObject(TailAddr, /*isCOWObject*/false);
}

/// Get the init values for an object's stored properties and its tail elements.
bool SILGlobalOpt::getObjectInitVals(SILValue Val,
                        llvm::DenseMap<VarDecl *, StoreInst *> &MemberStores,
                        llvm::SmallVectorImpl<StoreInst *> &TailStores,
                        ApplyInst **FindStringCall) {
  for (Operand *Use : Val->getUses()) {
    SILInstruction *User = Use->getUser();
    if (auto *UC = dyn_cast<UpcastInst>(User)) {
      // Upcast is transparent.
      if (!getObjectInitVals(UC, MemberStores, TailStores, FindStringCall))
        return false;
    } else if (auto *REA = dyn_cast<RefElementAddrInst>(User)) {
      // The address of a stored property.
      for (Operand *ElemAddrUse : REA->getUses()) {
        SILInstruction *ElemAddrUser = ElemAddrUse->getUser();
        if (auto *SI = dyn_cast<StoreInst>(ElemAddrUser)) {
          if (!isValidInitVal(SI->getSrc()) || MemberStores[REA->getField()])
            return false;
          MemberStores[REA->getField()] = SI;
        } else if (!isValidUseOfObject(ElemAddrUser, /*isCOWObject*/false)) {
          return false;
        }
      }
    } else if (auto *RTA = dyn_cast<RefTailAddrInst>(User)) {
      // The address of a tail element.
      for (Operand *TailUse : RTA->getUses()) {
        SILInstruction *TailUser = TailUse->getUser();
        if (auto *IA = dyn_cast<IndexAddrInst>(TailUser)) {
          // An index_addr yields the address of any tail element. Only if the
          // second operand (the index) is an integer literal we can figure out
          // which tail element is refereneced.
          int TailIdx = -1;
          if (auto *Index = dyn_cast<IntegerLiteralInst>(IA->getIndex()))
            TailIdx = Index->getValue().getZExtValue();

          for (Operand *IAUse : IA->getUses()) {
            if (!handleTailAddr(TailIdx, IAUse->getUser(), TailStores))
              return false;
          }
        // Without an index_addr it's the first tail element.
        } else if (!handleTailAddr(/*TailIdx*/0, TailUser, TailStores)) {
          return false;
        }
      }
    } else if (!isValidUseOfObject(User, /*isCOWObject*/false, FindStringCall)) {
      return false;
    }
  }
  return true;
}

class GlobalVariableMangler : public Mangle::ASTMangler {
public:
  std::string mangleOutlinedVariable(SILFunction *F, int &uniqueIdx) {
    std::string GlobName;
    do {
      beginManglingWithoutPrefix();
      appendOperator(F->getName());
      appendOperator("Tv", Index(uniqueIdx++));
      GlobName = finalize();
    } while (F->getModule().lookUpGlobalVariable(GlobName));

    return GlobName;
  }
};

/// Try to convert an object allocation into a statically initialized object.
///
/// In general this works for any class, but in practice it will only kick in
/// for array buffer objects. The use cases are array literals in a function.
/// For example:
///     func getarray() -> [Int] {
///       return [1, 2, 3]
///     }
void SILGlobalOpt::optimizeObjectAllocation(
    AllocRefInst *ARI, llvm::SmallVector<SILInstruction *, 4> &ToRemove) {

  if (ARI->isObjC())
    return;

  // Check how many tail allocated elements are on the object.
  ArrayRef<Operand> TailCounts = ARI->getTailAllocatedCounts();
  SILType TailType;
  unsigned NumTailElems = 0;
  if (TailCounts.size() > 0) {
    // We only support a single tail allocated array.
    if (TailCounts.size() > 1)
      return;
    // The number of tail allocated elements must be constant.
    if (auto *ILI = dyn_cast<IntegerLiteralInst>(TailCounts[0].get())) {
      if (ILI->getValue().getActiveBits() > 20)
        return;
      NumTailElems = ILI->getValue().getZExtValue();
      TailType = ARI->getTailAllocatedTypes()[0];
    } else {
      return;
    }
  }
  SILType Ty = ARI->getType();
  ClassDecl *Cl = Ty.getClassOrBoundGenericClass();
  if (!Cl)
    return;
  llvm::SmallVector<VarDecl *, 16> Fields;
  getFields(Cl, Fields);

  // Get the initialization stores of the object's properties and tail
  // allocated elements. Also check if there are any "bad" uses of the object.
  llvm::DenseMap<VarDecl *, StoreInst *> MemberStores;
  llvm::SmallVector<StoreInst *, 16> TailStores;
  TailStores.resize(NumTailElems);
  ApplyInst *FindStringCall = nullptr;
  if (!getObjectInitVals(ARI, MemberStores, TailStores, &FindStringCall))
    return;

  // Is there a store for all the class properties?
  if (MemberStores.size() != Fields.size())
    return;

  // Is there a store for all tail allocated elements?
  for (auto V : TailStores) {
    if (!V)
      return;
  }

  DEBUG(llvm::dbgs() << "Outline global variable in " <<
        ARI->getFunction()->getName() << '\n');

  assert(Cl->hasFixedLayout(Module->getSwiftModule(),
                            ResilienceExpansion::Minimal) &&
    "constructor call of resilient class should prevent static allocation");

  // Create a name for the outlined global variable.
  GlobalVariableMangler Mangler;
  std::string GlobName =
    Mangler.mangleOutlinedVariable(ARI->getFunction(), GlobIdx);

  SILGlobalVariable *Glob =
    SILGlobalVariable::create(*Module, SILLinkage::Private, IsNotSerialized,
                              GlobName, ARI->getType());

  // Schedule all init values for cloning into the initializer of Glob.
  StaticInitCloner Cloner(Glob);
  for (VarDecl *Field : Fields) {
    StoreInst *MemberStore = MemberStores[Field];
    Cloner.add(cast<SingleValueInstruction>(MemberStore->getSrc()));
  }
  for (StoreInst *TailStore : TailStores) {
    Cloner.add(cast<SingleValueInstruction>(TailStore->getSrc()));
  }

  // Create the class property initializers
  llvm::SmallVector<SILValue, 16> ObjectArgs;
  for (VarDecl *Field : Fields) {
    StoreInst *MemberStore = MemberStores[Field];
    assert(MemberStore);
    ObjectArgs.push_back(Cloner.clone(
                           cast<SingleValueInstruction>(MemberStore->getSrc())));
    ToRemove.push_back(MemberStore);
  }
  // Create the initializers for the tail elements.
  unsigned NumBaseElements = ObjectArgs.size();
  for (StoreInst *TailStore : TailStores) {
    ObjectArgs.push_back(Cloner.clone(
                           cast<SingleValueInstruction>(TailStore->getSrc())));
    ToRemove.push_back(TailStore);
  }
  // Create the initializer for the object itself.
  SILBuilder StaticInitBuilder(Glob);
  StaticInitBuilder.createObject(ArtificialUnreachableLocation(),
                                 ARI->getType(), ObjectArgs, NumBaseElements);

  // Replace the alloc_ref by global_value + strong_retain instructions.
  SILBuilder B(ARI);
  GlobalValueInst *GVI = B.createGlobalValue(ARI->getLoc(), Glob);
  B.createStrongRetain(ARI->getLoc(), GVI, B.getDefaultAtomicity());
  llvm::SmallVector<Operand *, 8> Worklist(ARI->use_begin(), ARI->use_end());
  while (!Worklist.empty()) {
    auto *Use = Worklist.pop_back_val();
    SILInstruction *User = Use->getUser();
    switch (User->getKind()) {
      case SILInstructionKind::DeallocRefInst:
        ToRemove.push_back(User);
        break;
      default:
        Use->set(GVI);
    }
  }
  if (FindStringCall && NumTailElems > 16) {
    assert(&*std::next(ARI->getIterator()) != FindStringCall &&
           "FindStringCall must not be the next instruction after ARI because "
           "deleting it would invalidate the instruction iterator");
    replaceFindStringCall(FindStringCall);
  }

  ToRemove.push_back(ARI);
  HasChanged = true;
}

/// Replaces a call to _findStringSwitchCase with a call to
/// _findStringSwitchCaseWithCache which builds a cache (e.g. a Dictionary) and
/// stores it into a global variable. Then subsequent calls to this function can
/// do a fast lookup using the cache.
void SILGlobalOpt::replaceFindStringCall(ApplyInst *FindStringCall) {
  // Find the replacement function in the swift stdlib.
  SmallVector<ValueDecl *, 1> results;
  Module->getASTContext().lookupInSwiftModule("_findStringSwitchCaseWithCache",
                                              results);
  if (results.size() != 1)
    return;

  auto *FD = dyn_cast<FuncDecl>(results.front());
  if (!FD)
    return;

  SILDeclRef declRef(FD, SILDeclRef::Kind::Func);
  SILFunction *replacementFunc = Module->getOrCreateFunction(
      FindStringCall->getLoc(), declRef, NotForDefinition);

  SILFunctionType *FTy = replacementFunc->getLoweredFunctionType();
  if (FTy->getNumParameters() != 3)
    return;

  SILType cacheType = FTy->getParameters()[2].getSILStorageType().getObjectType();
  NominalTypeDecl *cacheDecl = cacheType.getNominalOrBoundGenericNominal();
  if (!cacheDecl)
    return;

  assert(cacheDecl->hasFixedLayout(Module->getSwiftModule(),
                                   ResilienceExpansion::Minimal));

  SILType wordTy = cacheType.getFieldType(
                            cacheDecl->getStoredProperties().front(), *Module);

  GlobalVariableMangler Mangler;
  std::string GlobName =
    Mangler.mangleOutlinedVariable(FindStringCall->getFunction(), GlobIdx);

  // Create an "opaque" global variable which is passed as inout to
  // _findStringSwitchCaseWithCache and into which the function stores the
  // "cache".
  SILGlobalVariable *CacheVar =
    SILGlobalVariable::create(*Module, SILLinkage::Private, IsNotSerialized,
                              GlobName, cacheType);

  SILLocation Loc = FindStringCall->getLoc();
  SILBuilder StaticInitBuilder(CacheVar);
  auto *Zero = StaticInitBuilder.createIntegerLiteral(Loc, wordTy, 0);
  StaticInitBuilder.createStruct(ArtificialUnreachableLocation(), cacheType,
                                 {Zero, Zero});

  SILBuilder B(FindStringCall);
  GlobalAddrInst *CacheAddr = B.createGlobalAddr(FindStringCall->getLoc(),
                                                 CacheVar);
  FunctionRefInst *FRI = B.createFunctionRef(FindStringCall->getLoc(),
                                             replacementFunc);
  ApplyInst *NewCall = B.createApply(FindStringCall->getLoc(), FRI,
                                     FindStringCall->getSubstitutions(),
                                     { FindStringCall->getArgument(0),
                                       FindStringCall->getArgument(1),
                                       CacheAddr },
                                     FindStringCall->isNonThrowing());

  FindStringCall->replaceAllUsesWith(NewCall);
  FindStringCall->eraseFromParent();
}

/// Optimize access to the global variable, which is known
/// to have a constant value. Replace all loads from the
/// global address by invocations of a getter that returns
/// the value of this variable.
void SILGlobalOpt::optimizeGlobalAccess(SILGlobalVariable *SILG,
                                        StoreInst *SI) {
  DEBUG(llvm::dbgs() << "GlobalOpt: use static initializer for " <<
        SILG->getName() << '\n');

  if (GlobalVarSkipProcessing.count(SILG))
    return;

  if (//!isAssignedOnlyOnceInInitializer(SILG) ||
      !SILG->getDecl()) {
    return;
  }

  if (!GlobalLoadMap.count(SILG))
    return;

  // Generate a getter only if there are any loads from this variable.
  SILFunction *GetterF = genGetterFromInit(SI, SILG);
  if (!GetterF)
    return;

  // Iterate over all loads and replace them by values.
  // TODO: In principle, we could invoke the getter only once
  // inside each function that loads from the global. This
  // invocation should happen at the common dominator of all
  // loads inside this function.
  for (auto *Load: GlobalLoadMap[SILG]) {
    SILBuilderWithScope B(Load);
    auto *GetterRef = B.createFunctionRef(Load->getLoc(), GetterF);
    auto *Value = B.createApply(Load->getLoc(), GetterRef, {}, false);

    convertLoadSequence(Load, Value, B);
    HasChanged = true;
  }

}

bool SILGlobalOpt::run() {
  for (auto &F : *Module) {

    // Don't optimize functions that are marked with the opt.never attribute.
    if (!F.shouldOptimize())
      continue;

    // Cache cold blocks per function.
    ColdBlockInfo ColdBlocks(DA);
    GlobIdx = 0;
    for (auto &BB : F) {
      bool IsCold = ColdBlocks.isCold(&BB);
      auto Iter = BB.begin();

      // We can't remove instructions willy-nilly as we iterate because
      // that might cause a pointer to the next instruction to become
      // garbage, causing iterator invalidations (and crashes).
      // Instead, we collect in a list the instructions we want to remove
      // and erase the BB they belong to at the end of the loop, once we're
      // sure it's safe to do so.
      llvm::SmallVector<SILInstruction *, 4> ToRemove;

      while (Iter != BB.end()) {
        SILInstruction *I = &*Iter;
        Iter++;
        if (auto *BI = dyn_cast<BuiltinInst>(I)) {
          collectOnceCall(BI);
        } else if (auto *AI = dyn_cast<ApplyInst>(I)) {
          if (!IsCold)
            collectGlobalInitCall(AI);
        } else if (auto *GAI = dyn_cast<GlobalAddrInst>(I)) {
          collectGlobalAccess(GAI);
        } else if (auto *ARI = dyn_cast<AllocRefInst>(I)) {
          if (!F.isSerialized()) {
            // Currently we cannot serialize a function which refers to a
            // statically initialized global. So we don't do the optimization
            // for serializable functions.
            // TODO: We may do the optimization _after_ serialization in the
            // pass pipeline.
            optimizeObjectAllocation(ARI, ToRemove);
          }
        }
      }
      for (auto *I : ToRemove)
        I->eraseFromParent();
    }
  }

  for (auto &InitCalls : GlobalInitCallMap) {
    // Optimize the addressors if possible.
    optimizeInitializer(InitCalls.first, InitCalls.second);
    placeInitializers(InitCalls.first, InitCalls.second);
  }

  for (auto &Init : GlobalVarStore) {
    // Optimize the access to globals if possible.
    optimizeGlobalAccess(Init.first, Init.second);
  }

  return HasChanged;
}

namespace {
class SILGlobalOptPass : public SILModuleTransform
{
  void run() override {
    DominanceAnalysis *DA = PM->getAnalysis<DominanceAnalysis>();
    if (SILGlobalOpt(getModule(), DA).run()) {
      invalidateAll();
    }
  }

};
} // end anonymous namespace

SILTransform *swift::createGlobalOpt() {
  return new SILGlobalOptPass();
}
