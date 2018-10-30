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
#include "swift/AST/ASTMangler.h"
#include "swift/Demangling/Demangle.h"
#include "swift/Demangling/Demangler.h"
#include "swift/Demangling/ManglingMacros.h"
#include "swift/SIL/CFG.h"
#include "swift/SIL/DebugUtils.h"
#include "swift/SILOptimizer/Utils/SILOptFunctionBuilder.h"
#include "swift/SIL/SILGlobalVariable.h"
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
  SILOptFunctionBuilder &FunctionBuilder;
  SILModule *Module;
  DominanceAnalysis *DA;
  bool HasChanged = false;

  typedef SmallVector<ApplyInst *, 4> GlobalInitCalls;
  typedef SmallVector<LoadInst *, 4> GlobalLoads;

  /// A map from each visited global initializer call to a list of call sites.
  llvm::MapVector<SILFunction *, GlobalInitCalls> GlobalInitCallMap;

  // The following mappings are used if this is a compilation
  // in scripting mode and global variables are accessed without
  // addressors.

  /// A map from each visited global let variable to its set of loads.
  llvm::MapVector<SILGlobalVariable *, GlobalLoads> GlobalLoadMap;

  /// A map from each visited global let variable to the store instructions
  /// which initialize it.
  llvm::MapVector<SILGlobalVariable *, StoreInst *> GlobalVarStore;

  /// A set of visited global variables that for some reason we have decided is
  /// not able to be optimized safely or for which we do not know how to
  /// optimize safely.
  ///
  /// Once a global variable is in this set, we no longer will process it.
  llvm::SmallPtrSet<SILGlobalVariable *, 16> GlobalVarSkipProcessing;

  /// The set of blocks that this pass has determined to be inside a loop.
  ///
  /// This is used to mark any block that this pass has determined to be inside
  /// a loop.
  llvm::DenseSet<SILBasicBlock *> LoopBlocks;

  /// The set of functions that have had their loops analyzed.
  llvm::DenseSet<SILFunction *> LoopCheckedFunctions;

  /// Whether we have seen any "once" calls to callees that we currently don't
  /// handle.
  bool UnhandledOnceCallee = false;

  /// A map from a globalinit_func to the number of times "once" has called the
  /// function.
  llvm::DenseMap<SILFunction *, unsigned> InitializerCount;
public:
  SILGlobalOpt(SILOptFunctionBuilder &FunctionBuilder, SILModule *M, DominanceAnalysis *DA)
      : FunctionBuilder(FunctionBuilder), Module(M), DA(DA) {}

  bool run();

protected:
  /// If this is a call to a global initializer, map it.
  void collectGlobalInitCall(ApplyInst *AI);

  /// If this load is a read from a global let variable, add the load to
  /// GlobalLoadMap[SILG].
  void collectGlobalLoad(LoadInst *SI, SILGlobalVariable *SILG);

  /// If this store is a write to a global let variable, add the store to
  /// GlobalStoreMap[SILG].
  void collectGlobalStore(StoreInst *SI, SILGlobalVariable *SILG);

  /// This is the main entrypoint for collecting global accesses.
  void collectGlobalAccess(GlobalAddrInst *GAI);

  /// Returns true if we think that \p CurBB is inside a loop.
  bool isInLoop(SILBasicBlock *CurBB);

  /// Given that we are trying to place initializers in new locations, see if
  /// we can hoist the passed in apply \p AI out of any loops that it is
  /// currently within.
  ApplyInst *getHoistedApplyForInitializer(
      ApplyInst *AI, DominanceInfo *DT, SILFunction *InitF,
      SILFunction *ParentF,
      llvm::DenseMap<SILFunction *, ApplyInst *> &ParentFuncs);

  void placeInitializers(SILFunction *InitF, ArrayRef<ApplyInst *> Calls);

  /// Update UnhandledOnceCallee and InitializerCount by going through all
  /// "once" calls.
  void collectOnceCall(BuiltinInst *AI);

  /// Set the static initializer and remove "once" from addressor if a global
  /// can be statically initialized.
  void optimizeInitializer(SILFunction *AddrF, GlobalInitCalls &Calls);

  /// Optimize access to the global variable, which is known to have a constant
  /// value. Replace all loads from the global address by invocations of a
  /// getter that returns the value of this variable.
  void optimizeGlobalAccess(SILGlobalVariable *SILG, StoreInst *SI);

  /// Replace loads from a global variable by the known value.
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
  /// A map of old to new available values.
  SmallVector<std::pair<ValueBase *, SILValue>, 16> AvailVals;

  InstructionsCloner(SILFunction &F,
                     ArrayRef<SILInstruction *> Insns,
                     SILBasicBlock *Dest = nullptr)
    : SILClonerWithScopes(F), Insns(Insns), FromBB(nullptr), DestBB(Dest) {}

  void process(SILInstruction *I) { visit(I); }

  SILBasicBlock *remapBasicBlock(SILBasicBlock *BB) { return BB; }

  SILValue getMappedValue(SILValue Value) {
    return SILCloner<InstructionsCloner>::getMappedValue(Value);
  }

  void postProcess(SILInstruction *Orig, SILInstruction *Cloned) {
    DestBB->push_back(Cloned);
    SILClonerWithScopes<InstructionsCloner>::postProcess(Orig, Cloned);
    auto origResults = Orig->getResults(), clonedResults = Cloned->getResults();
    assert(origResults.size() == clonedResults.size());
    for (auto i : indices(origResults))
      AvailVals.push_back(std::make_pair(origResults[i], clonedResults[i]));
  }

  /// Clone all instructions from Insns into DestBB
  void clone() {
    for (auto I : Insns)
      process(I);
  }
};

} // end anonymous namespace

// If this is a call to a global initializer, map it.
void SILGlobalOpt::collectGlobalInitCall(ApplyInst *AI) {
  SILFunction *F = AI->getReferencedFunction();
  if (!F || !F->isGlobalInit())
    return;

  GlobalInitCallMap[F].push_back(AI);
}

// Map the load if this load is a read from a global variable that is either a
// let or a global variable that can not be changed externally
void SILGlobalOpt::collectGlobalLoad(LoadInst *LI, SILGlobalVariable *SILG) {
  assert(SILG);

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

static SILFunction *getGlobalGetterFunction(SILOptFunctionBuilder &FunctionBuilder,
                                            SILModule &M,
                                            SILLocation loc,
                                            VarDecl *varDecl) {
  auto getterNameTmp = mangleGetter(varDecl);

  // Check if a getter was generated already.
  if (auto *F = M.lookUpFunction(getterNameTmp))
    return F;

  auto Linkage = (varDecl->getEffectiveAccess() >= AccessLevel::Public
                  ? SILLinkage::PublicNonABI
                  : SILLinkage::Private);
  auto Serialized = (varDecl->getEffectiveAccess() >= AccessLevel::Public
                     ? IsSerialized
                     : IsNotSerialized);

  auto refType = M.Types.getLoweredType(varDecl->getInterfaceType());

  // Function takes no arguments and returns refType
  SILResultInfo Results[] = { SILResultInfo(refType.getASTType(),
                                            ResultConvention::Owned) };
  SILFunctionType::ExtInfo EInfo;
  EInfo = EInfo.withRepresentation(SILFunctionType::Representation::Thin);
  auto LoweredType =
    SILFunctionType::get(nullptr, EInfo,
                         SILCoroutineKind::None,
                         ParameterConvention::Direct_Unowned,
                         /*params*/ {}, /*yields*/ {}, Results, None,
                         M.getASTContext());
  auto getterName = M.allocateCopy(getterNameTmp);
  return FunctionBuilder.getOrCreateFunction(loc, getterName, Linkage,
                                             LoweredType, IsBare,
                                             IsNotTransparent, Serialized);
}

/// Generate getter from the initialization code whose result is stored by a
/// given store instruction.
static SILFunction *genGetterFromInit(SILOptFunctionBuilder &FunctionBuilder,
                                      StoreInst *Store,
                                      SILGlobalVariable *SILG) {
  auto *varDecl = SILG->getDecl();

  // Find the code that performs the initialization first.
  // Recursively walk the SIL value being assigned to the SILG.

  auto V = Store->getSrc();

  SmallVector<SILInstruction *, 8> Insts;
  Insts.push_back(Store);
  Insts.push_back(cast<SingleValueInstruction>(Store->getDest()));
  if (!analyzeStaticInitializer(V, Insts))
    return nullptr;

  // Produce a correct order of instructions.
  std::reverse(Insts.begin(), Insts.end());

  auto *GetterF = getGlobalGetterFunction(FunctionBuilder,
                                          Store->getModule(),
                                          Store->getLoc(),
                                          varDecl);

  GetterF->setDebugScope(Store->getFunction()->getDebugScope());
  if (!Store->getFunction()->hasQualifiedOwnership())
    GetterF->setUnqualifiedOwnership();
  auto *EntryBB = GetterF->createBasicBlock();

  // Copy instructions into GetterF
  InstructionsCloner Cloner(*GetterF, Insts, EntryBB);
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

// If this is a write to a global let variable, map it.
void SILGlobalOpt::collectGlobalStore(StoreInst *SI, SILGlobalVariable *SILG) {

  if (GlobalVarStore.count(SILG)) {
    // There is more then one assignment to a given global variable.
    // Therefore we don't know its value.
    GlobalVarSkipProcessing.insert(SILG);
  }

  // Figure out if the value of this variable is statically known.
  GlobalVarStore[SILG] = SI;
}

// Update UnhandledOnceCallee and InitializerCount by going through all "once"
// calls.
void SILGlobalOpt::collectOnceCall(BuiltinInst *BI) {
  if (UnhandledOnceCallee)
    return;

  const BuiltinInfo &Builtin = Module->getBuiltinInfo(BI->getName());
  if (Builtin.ID != BuiltinValueKind::Once)
    return;

  SILFunction *Callee = getCalleeOfOnceCall(BI);
  if (!Callee) {
    LLVM_DEBUG(llvm::dbgs() << "GlobalOpt: unhandled once callee\n");
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

ApplyInst *SILGlobalOpt::getHoistedApplyForInitializer(
    ApplyInst *AI, DominanceInfo *DT, SILFunction *InitF, SILFunction *ParentF,
    llvm::DenseMap<SILFunction *, ApplyInst *> &ParentFuncs) {
  auto PFI = ParentFuncs.find(ParentF);
  if (PFI == ParentFuncs.end()) {
    ParentFuncs[ParentF] = AI;

    // It's the first time we found a call to InitF in this function, so we
    // try to hoist it out of any loop.
    return AI;
  }

  // Found a replacement for this init call. Ensure the replacement dominates
  // the original call site.
  ApplyInst *CommonAI = PFI->second;
  assert(
      cast<FunctionRefInst>(CommonAI->getCallee())->getReferencedFunction() ==
          InitF &&
      "ill-formed global init call");
  SILBasicBlock *DomBB =
      DT->findNearestCommonDominator(AI->getParent(), CommonAI->getParent());

  // We must not move initializers around availability-checks.
  if (isAvailabilityCheckOnDomPath(DomBB, CommonAI->getParent(), DT))
    return nullptr;

  ApplyInst *Result = nullptr;
  if (DomBB != CommonAI->getParent()) {
    CommonAI->moveBefore(&*DomBB->begin());
    placeFuncRef(CommonAI, DT);

    // Try to hoist the existing AI again if we move it to another block,
    // e.g. from a loop exit into the loop.
    Result = CommonAI;
  }

  AI->replaceAllUsesWith(CommonAI);
  AI->eraseFromParent();
  HasChanged = true;
  return Result;
}

/// Optimize placement of initializer calls given a list of calls to the
/// same initializer. All original initialization points must be dominated by
/// the final initialization calls.
///
/// The current heuristic hoists all initialization points within a function to
/// a single dominating call in the outer loop preheader.
void SILGlobalOpt::placeInitializers(SILFunction *InitF,
                                     ArrayRef<ApplyInst *> Calls) {
  LLVM_DEBUG(llvm::dbgs() << "GlobalOpt: calls to "
             << Demangle::demangleSymbolAsString(InitF->getName())
             << " : " << Calls.size() << "\n");
  // Map each initializer-containing function to its final initializer call.
  llvm::DenseMap<SILFunction *, ApplyInst *> ParentFuncs;
  for (auto *AI : Calls) {
    assert(AI->getNumArguments() == 0 && "ill-formed global init call");
    assert(cast<FunctionRefInst>(AI->getCallee())->getReferencedFunction()
           == InitF && "wrong init call");
    SILFunction *ParentF = AI->getFunction();
    DominanceInfo *DT = DA->get(ParentF);
    ApplyInst *HoistAI =
        getHoistedApplyForInitializer(AI, DT, InitF, ParentF, ParentFuncs);

    // If we were unable to find anything, just go onto the next apply.
    if (!HoistAI) {
      continue;
    }

    // Otherwise, move this call to the outermost loop preheader.
    SILBasicBlock *BB = HoistAI->getParent();
    typedef llvm::DomTreeNodeBase<SILBasicBlock> DomTreeNode;
    DomTreeNode *Node = DT->getNode(BB);
    while (Node) {
      SILBasicBlock *DomParentBB = Node->getBlock();
      if (isAvailabilityCheck(DomParentBB)) {
        LLVM_DEBUG(llvm::dbgs() << "  don't hoist above availability check "
                                   "at bb"
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
      LLVM_DEBUG(llvm::dbgs() << "  skipping (not in a loop): " << *HoistAI
                              << "  in " << HoistAI->getFunction()->getName()
                              << "\n");
      continue;
    }

    LLVM_DEBUG(llvm::dbgs() << "  hoisting: " << *HoistAI << "  in "
                       << HoistAI->getFunction()->getName() << "\n");
    HoistAI->moveBefore(&*BB->begin());
    placeFuncRef(HoistAI, DT);
    HasChanged = true;
  }
}

/// Create a getter function from the initializer function.
static SILFunction *genGetterFromInit(SILOptFunctionBuilder &FunctionBuilder,
                                      SILFunction *InitF, VarDecl *varDecl) {
  // Generate a getter from the global init function without side-effects.

  auto *GetterF = getGlobalGetterFunction(FunctionBuilder,
                                          InitF->getModule(),
                                          InitF->getLocation(),
                                          varDecl);
  if (!InitF->hasQualifiedOwnership())
    GetterF->setUnqualifiedOwnership();

  // Copy InitF into GetterF, including the entry arguments.
  SILFunctionCloner Cloner(GetterF);
  Cloner.cloneFunction(InitF);
  GetterF->setInlined();

  // Find the store instruction
  auto *BB = GetterF->getEntryBlock();
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
  auto *GetterF = genGetterFromInit(FunctionBuilder, InitF, SILG->getDecl());

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

    // FIXME: This is asserting that a specific SIL sequence follows an
    // addressor! SIL passes should never do this without first specifying a
    // structural SIL property independent of the SILOptimizer and enforced by
    // the SILVerifier.
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

  LLVM_DEBUG(llvm::dbgs() << "GlobalOpt: use static initializer for "
                          << SILG->getName() << '\n');

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

  for (auto *Op : getNonDebugUses(GAI)) {
    if (auto *SI = dyn_cast<StoreInst>(Op->getUser())) {
      if (SI->getDest() == GAI)
        collectGlobalStore(SI, SILG);
      continue;
    }

    if (auto *Load = getValidLoad(Op->getUser(), GAI)) {
      collectGlobalLoad(Load, SILG);
      continue;
    }

    // This global is not initialized by a simple
    // constant value at this moment.
    GlobalVarSkipProcessing.insert(SILG);
    break;
  }
}

// Optimize access to the global variable, which is known to have a constant
// value. Replace all loads from the global address by invocations of a getter
// that returns the value of this variable.
void SILGlobalOpt::optimizeGlobalAccess(SILGlobalVariable *SILG,
                                        StoreInst *SI) {
  LLVM_DEBUG(llvm::dbgs() << "GlobalOpt: use static initializer for "
                          << SILG->getName() << '\n');

  if (GlobalVarSkipProcessing.count(SILG))
    return;

  if (//!isAssignedOnlyOnceInInitializer(SILG) ||
      !SILG->getDecl()) {
    return;
  }

  if (!GlobalLoadMap.count(SILG))
    return;

  // Generate a getter only if there are any loads from this variable.
  SILFunction *GetterF = genGetterFromInit(FunctionBuilder, SI, SILG);
  if (!GetterF)
    return;

  // Iterate over all loads and replace them by values.
  // TODO: In principle, we could invoke the getter only once
  // inside each function that loads from the global. This
  // invocation should happen at the common dominator of all
  // loads inside this function.
  for (auto *Load : GlobalLoadMap[SILG]) {
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
      for (auto &I : BB) {
        if (auto *BI = dyn_cast<BuiltinInst>(&I)) {
          collectOnceCall(BI);
          continue;
        }

        if (auto *AI = dyn_cast<ApplyInst>(&I)) {
          if (!IsCold) {
            collectGlobalInitCall(AI);
          }
          continue;
        }

        auto *GAI = dyn_cast<GlobalAddrInst>(&I);
        if (!GAI) {
          continue;
        }

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

//===----------------------------------------------------------------------===//
//                           Top Level Entry Point
//===----------------------------------------------------------------------===//

namespace {

class SILGlobalOptPass : public SILModuleTransform {
  void run() override {
    auto *DA = PM->getAnalysis<DominanceAnalysis>();
    SILOptFunctionBuilder FunctionBuilder(*this);
    if (SILGlobalOpt(FunctionBuilder, getModule(), DA).run()) {
      invalidateAll();
    }
  }
};

} // end anonymous namespace

SILTransform *swift::createGlobalOpt() {
  return new SILGlobalOptPass();
}
