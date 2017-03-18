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

  // Whether we see a "once" call to callees that we currently don't handle.
  bool UnhandledOnceCallee = false;
  // Record number of times a globalinit_func is called by "once".
  llvm::DenseMap<SILFunction*, unsigned> InitializerCount;
public:
  SILGlobalOpt(SILModule *M, DominanceAnalysis *DA): Module(M), DA(DA),
                                                     ColdBlocks(DA) {}

  bool run();

protected:
  void collectGlobalInitCall(ApplyInst *AI);
  void collectGlobalLoad(LoadInst *SI, SILGlobalVariable *SILG);
  void collectGlobalStore(StoreInst *SI, SILGlobalVariable *SILG);
  void collectGlobalAccess(GlobalAddrInst *GAI);
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
                                GlobalInitCalls &Calls);
};

/// Helper class to copy only a set of SIL instructions providing in the
/// constructor.
class InstructionsCloner : public SILClonerWithScopes<InstructionsCloner> {
  friend class SILVisitor<InstructionsCloner>;
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
    AvailVals.push_back(std::make_pair(Orig, Cloned));
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

  if (GlobalAddrInst *GAI = dyn_cast<GlobalAddrInst>(Op)) {
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
  NewMangling::ASTMangler Mangler;
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
  ReverseInsns.push_back(dyn_cast<SILInstruction>(Store->getDest()));
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
      ParameterConvention::Direct_Owned, { }, Results, None,
      Store->getModule().getASTContext());
  auto *GetterF = Store->getModule().getOrCreateFunction(
      Store->getLoc(),
      getterName, SILLinkage::Private, LoweredType,
      IsBare_t::IsBare, IsTransparent_t::IsNotTransparent,
      IsFragile_t::IsFragile);
  GetterF->setDebugScope(Store->getFunction()->getDebugScope());
  if (Store->getFunction()->hasUnqualifiedOwnership())
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
    if (StoreInst *SI = dyn_cast<StoreInst>(&I)) {
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
           == SILFunctionTypeRepresentation::Thin &&
         "Expected thin function representation!");

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
  CondBranchInst *CBR = dyn_cast<CondBranchInst>(BB->getTerminator());
  if (!CBR)
    return false;
  
  ApplyInst *AI = dyn_cast<ApplyInst>(CBR->getCondition());
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
      ParameterConvention::Direct_Owned, { }, Results, None,
      InitF->getASTContext());
  auto *GetterF = InitF->getModule().getOrCreateFunction(
      InitF->getLocation(),
      getterName, SILLinkage::Private, LoweredType,
      IsBare_t::IsBare, IsTransparent_t::IsNotTransparent,
      IsFragile_t::IsFragile);
  if (InitF->hasUnqualifiedOwnership())
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

    if (StoreInst *SI = dyn_cast<StoreInst>(&I)) {
      Val = SI->getSrc();
      Store = SI;
      continue;
    }

    if (ReturnInst *RI = dyn_cast<ReturnInst>(&I)) {
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
    if (BuiltinInst *BI = dyn_cast<BuiltinInst>(&I)) {
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
static SILInstruction *convertLoadSequence(SILInstruction *I,
                                SILInstruction *Value,
                                SILBuilder &B) {

  if (isa<GlobalAddrInst>(I))
    return Value;

  if (auto *LI = dyn_cast<LoadInst>(I)) {
    Value =
        convertLoadSequence(cast<SILInstruction>(LI->getOperand()), Value, B);
    LI->replaceAllUsesWith(Value);
    return Value;
  }

  // It is a series of struct_element_addr followed by load.
  if (auto *SEAI = dyn_cast<StructElementAddrInst>(I)) {
    Value =
        convertLoadSequence(cast<SILInstruction>(SEAI->getOperand()), Value, B);
    auto *SEI = B.createStructExtract(SEAI->getLoc(), Value, SEAI->getField());
    return SEI;
  }

  if (auto *TEAI = dyn_cast<TupleElementAddrInst>(I)) {
    Value =
        convertLoadSequence(cast<SILInstruction>(TEAI->getOperand()), Value, B);
    auto *TEI = B.createTupleExtract(TEAI->getLoc(), Value, TEAI->getFieldNo());
    return TEI;
  }

  llvm_unreachable("Unknown instruction sequence for reading from a global");
  return nullptr;
}

/// Replace loads from a global variable by the known value.
void SILGlobalOpt::
replaceLoadsByKnownValue(BuiltinInst *CallToOnce, SILFunction *AddrF,
                         SILFunction *InitF, SILGlobalVariable *SILG,
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
  SILG->setInitializer(InitF);
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
  auto *SILG = SILGlobalVariable::getVariableOfStaticInitializer(InitF);
  if (!SILG || !SILG->isDefinition())
    return;

  DEBUG(llvm::dbgs() << "GlobalOpt: use static initializer for " <<
        SILG->getName() << '\n');

  // Remove "once" call from the addressor.
  if (!isAssignedOnlyOnceInInitializer(SILG) || !SILG->getDecl()) {
    removeToken(CallToOnce->getOperand(0));
    CallToOnce->eraseFromParent();
    SILG->setInitializer(InitF);
    HasChanged = true;
    return;
  }

  replaceLoadsByKnownValue(CallToOnce, AddrF, InitF, SILG, Calls);
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
    auto *SILG = SILGlobalVariable::getVariableOfStaticInitializer(InitF);
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
    case Accessibility::Private:
    case Accessibility::FilePrivate:
      return false;
    case Accessibility::Internal:
      return !SILG->getModule().isWholeModule();
    case Accessibility::Public:
    case Accessibility::Open:
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
static LoadInst *getValidLoad(SILInstruction *I, SILInstruction *V) {
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
    for (auto &BB : F) {
      bool IsCold = ColdBlocks.isCold(&BB);
      for (auto &I : BB)
        if (BuiltinInst *BI = dyn_cast<BuiltinInst>(&I)) {
          collectOnceCall(BI);
        } else if (ApplyInst *AI = dyn_cast<ApplyInst>(&I)) {
          if (!IsCold)
            collectGlobalInitCall(AI);
        } else if (GlobalAddrInst *GAI = dyn_cast<GlobalAddrInst>(&I)) {
            collectGlobalAccess(GAI);
        }
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

  StringRef getName() override { return "SIL Global Optimization"; }
};
} // end anonymous namespace

SILTransform *swift::createGlobalOpt() {
  return new SILGlobalOptPass();
}
