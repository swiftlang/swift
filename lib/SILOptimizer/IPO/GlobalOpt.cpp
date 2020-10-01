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
#include "swift/SIL/SILCloner.h"
#include "swift/SIL/SILGlobalVariable.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILInstructionWorklist.h"
#include "swift/SILOptimizer/Analysis/ColdBlockInfo.h"
#include "swift/SILOptimizer/Analysis/DominanceAnalysis.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/BasicBlockOptUtils.h"
#include "swift/SILOptimizer/Utils/InstOptUtils.h"
#include "swift/SILOptimizer/Utils/SILOptFunctionBuilder.h"
#include "swift/SILOptimizer/Utils/ConstantFolding.h"
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
  SILPassManager *PM;
  bool HasChanged = false;

  typedef SmallVector<ApplyInst *, 4> GlobalInitCalls;
  typedef SmallVector<BeginAccessInst *, 4> GlobalAccesses;
  typedef SmallVector<GlobalAddrInst *, 4> GlobalAddrs;

  /// A map from each visited global initializer call to a list of call sites.
  llvm::MapVector<SILFunction *, GlobalInitCalls> GlobalInitCallMap;

  // The following mappings are used if this is a compilation
  // in scripting mode and global variables are accessed without
  // addressors.

  /// A map from each visited global to its set of begin_access instructions.
  llvm::MapVector<SILGlobalVariable *, GlobalAccesses> GlobalAccessMap;

  /// A map from each visited global to all of its global address instructions.
  llvm::MapVector<SILGlobalVariable *, GlobalAddrs> GlobalAddrMap;

  /// A map from each visited global let variable to the store instructions
  /// which initialize it.
  llvm::MapVector<SILGlobalVariable *, StoreInst *> GlobalVarStore;

  /// A map for each visited global variable to the alloc instruction that
  /// allocated space for it.
  llvm::MapVector<SILGlobalVariable *, AllocGlobalInst *> AllocGlobalStore;

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

  llvm::SmallVector<SILInstruction *, 4> InstToRemove;
  llvm::SmallVector<SILGlobalVariable *, 4> GlobalsToRemove;

public:
  SILGlobalOpt(SILOptFunctionBuilder &FunctionBuilder, SILModule *M,
               DominanceAnalysis *DA, SILPassManager *PM)
      : FunctionBuilder(FunctionBuilder), Module(M), DA(DA), PM(PM) {}

  bool run();

protected:
  /// Checks if a given global variable is assigned only once.
  bool isAssignedOnlyOnceInInitializer(SILGlobalVariable *SILG,
                                       SILFunction *globalAddrF);

  /// Reset all the maps of global variables.
  void reset();

  /// Collect all global variables.
  void collect();

  void collectUsesOfInstructionForDeletion(SILInstruction *inst);

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

  /// Update UnhandledOnceCallee and InitializerCount by going through all
  /// "once" calls.
  void collectOnceCall(BuiltinInst *AI);

  /// Set the static initializer and remove "once" from addressor if a global
  /// can be statically initialized.
  bool optimizeInitializer(SILFunction *AddrF, GlobalInitCalls &Calls);

  /// If possible, remove global address instructions associated with the given
  /// global.
  bool tryRemoveGlobalAddr(SILGlobalVariable *global);

  /// If possible, remove global alloc instructions associated with the given
  /// global.
  bool tryRemoveGlobalAlloc(SILGlobalVariable *global, AllocGlobalInst *alloc);

  /// If a global has no uses, remove it.
  bool tryRemoveUnusedGlobal(SILGlobalVariable *global);

  /// Optimize access to the global variable, which is known to have a constant
  /// value. Replace all loads from the global address by invocations of a
  /// getter that returns the value of this variable.
  void optimizeGlobalAccess(SILGlobalVariable *SILG, StoreInst *SI);

  /// Replace loads from a global variable by the known value.
  void replaceLoadsByKnownValue(SILFunction *InitF,
                                SILGlobalVariable *SILG,
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
  if (!Callee->isGlobalInitOnceFunction())
    return;

  // We currently disable optimizing the initializer if a globalinit_func
  // is called by "once" from multiple locations.
  if (!BI->getFunction()->isGlobalInit())
    // If a globalinit_func is called by "once" from a function that is not
    // an addressor, we set count to 2 to disable optimizing the initializer.
    InitializerCount[Callee] = 2;
  else
    ++InitializerCount[Callee];
}

static bool isPotentialStore(SILInstruction *inst) {
  switch (inst->getKind()) {
    case SILInstructionKind::LoadInst:
      return false;
    case SILInstructionKind::PointerToAddressInst:
    case SILInstructionKind::StructElementAddrInst:
    case SILInstructionKind::TupleElementAddrInst:
      for (Operand *op : cast<SingleValueInstruction>(inst)->getUses()) {
        if (isPotentialStore(op->getUser()))
          return true;
      }
      return false;
    case SILInstructionKind::BeginAccessInst:
      return cast<BeginAccessInst>(inst)->getAccessKind() != SILAccessKind::Read;
    default:
      return true;
  }
}

/// return true if this block is inside a loop.
bool SILGlobalOpt::isInLoop(SILBasicBlock *CurBB) {
  SILFunction *F = CurBB->getParent();
  // Catch the common case in which we've already hoisted the initializer.
  if (CurBB == &F->front())
    return false;

  if (LoopCheckedFunctions.insert(F).second) {
    for (auto I = scc_begin(F); !I.isAtEnd(); ++I) {
      if (I.hasCycle())
        for (SILBasicBlock *BB : *I)
          LoopBlocks.insert(BB);
    }
  }
  return LoopBlocks.count(CurBB);
}

bool SILGlobalOpt::isAssignedOnlyOnceInInitializer(SILGlobalVariable *SILG,
                                                   SILFunction *globalAddrF) {
  if (SILG->isLet())
    return true;

  // If we should skip this, it is probably because there are multiple stores.
  // Return false if there are multiple stores or no stores.
  if (GlobalVarSkipProcessing.count(SILG) || !GlobalVarStore.count(SILG))
    return false;

  if (GlobalInitCallMap.count(globalAddrF)) {
    for (ApplyInst *initCall : GlobalInitCallMap[globalAddrF]) {
      for (auto *Op : getNonDebugUses(initCall)) {
        if (isPotentialStore(Op->getUser()))
          return false;
      }
    }
  }

  // Otherwise, return true if this can't be used externally (false, otherwise).
  return !isPossiblyUsedExternally(SILG->getLinkage(),
                                   SILG->getModule().isWholeModule());
}

/// Replace loads from \a addr by the \p initVal of a global.
///
/// Recuresively walk over all uses of \p addr and look through address
/// projections. The \p initVal is an instruction in the static initializer of
/// a SILGlobalVariable. It is cloned into the current function with \p cloner.
static void replaceLoadsFromGlobal(SILValue addr,
                                   SingleValueInstruction *initVal,
                                   StaticInitCloner &cloner) {
  for (Operand *use : addr->getUses()) {
    SILInstruction *user = use->getUser();
    if (auto *load = dyn_cast<LoadInst>(user)) {
      SingleValueInstruction *clonedInitVal = cloner.clone(initVal);
      load->replaceAllUsesWith(clonedInitVal);
      continue;
    }
    if (auto *seai = dyn_cast<StructElementAddrInst>(user)) {
      auto *si = cast<StructInst>(initVal);
      auto *member = cast<SingleValueInstruction>(
                          si->getOperandForField(seai->getField())->get());
      replaceLoadsFromGlobal(seai, member, cloner);
      continue;
    }
    if (auto *teai = dyn_cast<TupleElementAddrInst>(user)) {
      auto *ti = cast<TupleInst>(initVal);
      auto *member = cast<SingleValueInstruction>(
                          ti->getElement(teai->getFieldIndex()));
      replaceLoadsFromGlobal(teai, member, cloner);
      continue;
    }
    if (isa<BeginAccessInst>(user) || isa<PointerToAddressInst>(user)) {
      auto *svi = cast<SingleValueInstruction>(user);
      replaceLoadsFromGlobal(svi, initVal, cloner);
      continue;
    }
  }
}

/// Replace loads from a global variable by the known initial value.
void SILGlobalOpt::
replaceLoadsByKnownValue(SILFunction *InitF, SILGlobalVariable *SILG,
                         GlobalInitCalls &Calls) {
  LLVM_DEBUG(llvm::dbgs() << "GlobalOpt: replacing loads with known value for "
                          << SILG->getName() << '\n');

  for (ApplyInst *initCall : Calls) {
    auto *initVal =
          dyn_cast<SingleValueInstruction>(SILG->getStaticInitializerValue());
    if (!initVal) {
      // This should never happen. Just to be on the safe side.
      continue;
    }

    StaticInitCloner cloner(initCall);
    SmallVector<SILInstruction *, 8> insertedInsts;
    cloner.setTrackingList(&insertedInsts);
    cloner.add(initVal);

    // Replace all loads from the addressor with the initial value of the global.
    replaceLoadsFromGlobal(initCall, initVal, cloner);

    // Remove all instructions which are dead now.
    InstructionDeleter deleter;
    deleter.recursivelyDeleteUsersIfDead(initCall);
    if (initCall->use_empty()) {
      // The call to the addressor is dead as well and can be removed.
      auto *callee = dyn_cast<FunctionRefInst>(initCall->getCallee());
      deleter.forceDelete(initCall);
      if (callee)
        deleter.deleteIfDead(callee);
    }

    // Constant folding the global value can enable other initializers to become
    // constant, e.g.
    //    let a = 1
    //    let b = a + 1
    ConstantFolder constFolder(FunctionBuilder, PM->getOptions().AssertConfig);
    for (SILInstruction *inst : insertedInsts) {
      constFolder.addToWorklist(inst);
    }
    constFolder.processWorkList();
  }
  Calls.clear();
}

/// We analyze the body of globalinit_func to see if it can be statically
/// initialized. If yes, we set the initial value of the SILGlobalVariable and
/// remove the "once" call to globalinit_func from the addressor.
bool SILGlobalOpt::optimizeInitializer(SILFunction *AddrF,
                                       GlobalInitCalls &Calls) {
  if (UnhandledOnceCallee)
    return false;

  // Find the initializer and the SILGlobalVariable.
  BuiltinInst *CallToOnce;

  // If the addressor contains a single "once" call, it calls globalinit_func,
  // and the globalinit_func is called by "once" from a single location,
  // continue; otherwise bail.
  auto *InitF = findInitializer(AddrF, CallToOnce);
  if (!InitF || InitializerCount[InitF] > 1)
    return false;

  // If the globalinit_func is trivial, continue; otherwise bail.
  SingleValueInstruction *InitVal;
  SILGlobalVariable *SILG = getVariableOfStaticInitializer(InitF, InitVal);
  if (!SILG)
    return false;

  auto expansion = ResilienceExpansion::Maximal;
  if (hasPublicVisibility(SILG->getLinkage()))
    expansion = ResilienceExpansion::Minimal;

  auto &tl = Module->Types.getTypeLowering(
      SILG->getLoweredType(),
      TypeExpansionContext::noOpaqueTypeArchetypesSubstitution(expansion));
  if (!tl.isLoadable())
    return false;

  LLVM_DEBUG(llvm::dbgs() << "GlobalOpt: use static initializer for "
                          << SILG->getName() << '\n');

  // Remove "once" call from the addressor.
  removeToken(CallToOnce->getOperand(0));
  eraseUsesOfInstruction(CallToOnce);
  recursivelyDeleteTriviallyDeadInstructions(CallToOnce, true);

  // Create the constant initializer of the global variable.
  StaticInitCloner::appendToInitializer(SILG, InitVal);

  if (isAssignedOnlyOnceInInitializer(SILG, AddrF)) {
    replaceLoadsByKnownValue(InitF, SILG, Calls);
  }

  HasChanged = true;
  return true;
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

static bool canBeUsedOrChangedExternally(SILGlobalVariable *global) {
  if (global->isLet())
    return isPossiblyUsedExternally(global->getLinkage(),
                                    global->getModule().isWholeModule());
  return canBeChangedExternally(global);
}

static bool isSafeToRemove(SILGlobalVariable *global) {
  return global->getDecl() && !canBeUsedOrChangedExternally(global);
}

bool SILGlobalOpt::tryRemoveGlobalAlloc(SILGlobalVariable *global,
                                        AllocGlobalInst *alloc) {
  if (!isSafeToRemove(global))
    return false;

  // Make sure the global's address is never taken and we shouldn't skip this
  // global.
  if (GlobalVarSkipProcessing.count(global) ||
      (GlobalAddrMap[global].size() &&
       std::any_of(GlobalAddrMap[global].begin(), GlobalAddrMap[global].end(),
                   [=](GlobalAddrInst *addr) {
                     return std::find(InstToRemove.begin(), InstToRemove.end(),
                                      addr) == InstToRemove.end();
                   })))
    return false;

  InstToRemove.push_back(alloc);
  return true;
}

/// If there are no loads or accesses of a given global, then remove its
/// associated global addr and all asssociated instructions.
bool SILGlobalOpt::tryRemoveGlobalAddr(SILGlobalVariable *global) {
  if (!isSafeToRemove(global))
    return false;

  if (GlobalVarSkipProcessing.count(global) || GlobalAccessMap[global].size())
    return false;

  // Check if the address is used in anything but a store. If any global_addr
  // instruction associated with a global is used in anything but a store, we
  // can't remove ANY global_addr instruction associated with that global.
  for (auto *addr : GlobalAddrMap[global]) {
    for (auto *use : addr->getUses()) {
      if (!isa<StoreInst>(use->getUser()))
        return false;
    }
  }
  
  // Now that it's safe, remove all global addresses associated with this global
  for (auto *addr : GlobalAddrMap[global]) {
    InstToRemove.push_back(addr);
  }

  return true;
}

bool SILGlobalOpt::tryRemoveUnusedGlobal(SILGlobalVariable *global) {
  if (!isSafeToRemove(global))
    return false;

  if (GlobalVarSkipProcessing.count(global))
    return false;

  // If this global is used, check if the user is going to be removed.
  // Make sure none of the removed instructions are the same as this global's
  // alloc instruction
  if (AllocGlobalStore.count(global) &&
      std::none_of(InstToRemove.begin(), InstToRemove.end(),
                   [=](SILInstruction *inst) {
                     return AllocGlobalStore[global] == inst;
                   }))
    return false;

  if (GlobalVarStore.count(global) &&
      std::none_of(
          InstToRemove.begin(), InstToRemove.end(),
          [=](SILInstruction *inst) { return GlobalVarStore[global] == inst; }))
    return false;

  // Check if any of the global_addr instructions associated with this global
  // aren't going to be removed. In that case, we need to keep the global.
  if (GlobalAddrMap[global].size() &&
      std::any_of(GlobalAddrMap[global].begin(), GlobalAddrMap[global].end(),
                  [=](GlobalAddrInst *addr) {
                    return std::find(InstToRemove.begin(), InstToRemove.end(),
                                     addr) == InstToRemove.end();
                  }))
    return false;

  if (GlobalAccessMap[global].size() &&
      std::any_of(GlobalAccessMap[global].begin(),
                  GlobalAccessMap[global].end(), [=](BeginAccessInst *access) {
                    return std::find(InstToRemove.begin(), InstToRemove.end(),
                                     access) == InstToRemove.end();
                  }))
    return false;

  GlobalsToRemove.push_back(global);
  return true;
}

/// If this is a read from a global let variable, map it.
void SILGlobalOpt::collectGlobalAccess(GlobalAddrInst *GAI) {
  auto *SILG = GAI->getReferencedGlobal();
  if (!SILG)
    return;

  if (!SILG->getDecl())
    return;

  GlobalAddrMap[SILG].push_back(GAI);

  if (!SILG->isLet()) {
    // We cannot determine the value for global variables which could be
    // changed externally at run-time.
    if (canBeChangedExternally(SILG))
      return;
  }

  if (GlobalVarSkipProcessing.count(SILG))
    return;

  auto *F = GAI->getFunction();

  if (!SILG->getLoweredType().isTrivial(*F)) {
    LLVM_DEBUG(llvm::dbgs() << "GlobalOpt: type is not trivial: "
                          << SILG->getName() << '\n');
    GlobalVarSkipProcessing.insert(SILG);
    return;
  }

  // Ignore any accesses inside addressors for SILG
  auto GlobalVar = getVariableOfGlobalInit(F);
  if (GlobalVar == SILG)
    return;

  for (auto *Op : getNonDebugUses(GAI)) {
    SILInstruction *user = Op->getUser();
    auto *SI = dyn_cast<StoreInst>(user);
    if (SI && SI->getDest() == GAI && GlobalVarStore.count(SILG) == 0) {
      // The one and only store to global.
      GlobalVarStore[SILG] = SI;
      continue;
    }
    if (auto *beginAccess = dyn_cast<BeginAccessInst>(user)) {
      GlobalAccessMap[SILG].push_back(beginAccess);
    }
    if (isPotentialStore(user)) {
      // An unknown store or the second store we see.
      // If there are multiple stores to a global we cannot reason about the
      // value.
      GlobalVarSkipProcessing.insert(SILG);
    }
  }
}

// Optimize access to the global variable, which is known to have a constant
// value. Replace all loads from the global address by invocations of a getter
// that returns the value of this variable.
void SILGlobalOpt::optimizeGlobalAccess(SILGlobalVariable *SILG,
                                        StoreInst *SI) {
  LLVM_DEBUG(llvm::dbgs() << "GlobalOpt: use static initializer for "
                          << SILG->getName() << '\n');

  if (GlobalVarSkipProcessing.count(SILG)) {
    LLVM_DEBUG(llvm::dbgs() << "GlobalOpt: already decided to skip: "
                          << SILG->getName() << '\n');
    return;
  }

  if (GlobalAddrMap[SILG].empty()) {
    LLVM_DEBUG(llvm::dbgs() << "GlobalOpt: not in load map: "
                          << SILG->getName() << '\n');
    return;
  }

  auto *initVal = dyn_cast<SingleValueInstruction>(SI->getSrc());
  if (!initVal)
    return;

  SmallVector<SILInstruction *, 8> unused;
  if (!analyzeStaticInitializer(initVal, unused))
    return;

  // Iterate over all loads and replace them by values.
  for (auto *globalAddr : GlobalAddrMap[SILG]) {
    if (globalAddr->getFunction()->isSerialized())
      continue;

    StaticInitCloner cloner(globalAddr);
    cloner.add(initVal);

    // Replace all loads from the addressor with the initial value of the global.
    replaceLoadsFromGlobal(globalAddr, initVal, cloner);

    HasChanged = true;
  }
}

void SILGlobalOpt::reset() {
  AllocGlobalStore.clear();
  GlobalVarStore.clear();
  GlobalAddrMap.clear();
  GlobalAccessMap.clear();
  GlobalInitCallMap.clear();
}

void SILGlobalOpt::collect() {
  for (auto &F : *Module) {
    // Make sure to create an entry. This is important in case a global variable
    // (e.g. a public one) is not used inside the same module.
    if (F.isGlobalInit())
      (void)GlobalInitCallMap[&F];

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
            if (SILFunction *callee = AI->getReferencedFunctionOrNull()) {
              if (callee->isGlobalInit() && ApplySite(AI).canOptimize())
                GlobalInitCallMap[callee].push_back(AI);
            }
          }
          continue;
        }

        if (auto *GAI = dyn_cast<GlobalAddrInst>(&I)) {
          collectGlobalAccess(GAI);
          continue;
        }

        if (auto *allocGlobal = dyn_cast<AllocGlobalInst>(&I)) {
          AllocGlobalStore[allocGlobal->getReferencedGlobal()] = allocGlobal;
          continue;
        }
      }
    }
  }
}

bool SILGlobalOpt::run() {
  // Collect all the global variables and associated instructions.
  collect();

  // Iterate until a fixed point to be able to optimize globals which depend
  // on other globals, e.g.
  //   let a = 1
  //   let b = a + 10
  //   let c = b + 5
  //   ...
  bool changed = false;
  do {
    changed = false;
    for (auto &InitCalls : GlobalInitCallMap) {
      // Don't optimize functions that are marked with the opt.never attribute.
      if (!InitCalls.first->shouldOptimize())
        continue;

      // Try to create a static initializer for the global and replace all uses
      // of the global by this constant value.
      changed |= optimizeInitializer(InitCalls.first, InitCalls.second);
    }
  } while (changed);

  // This is similiar to optimizeInitializer, but it's for globals which are
  // initialized in the "main" function and not by an initializer function.
  for (auto &Init : GlobalVarStore) {
    // Don't optimize functions that are marked with the opt.never attribute.
    if (!Init.second->getFunction()->shouldOptimize())
      continue;

    optimizeGlobalAccess(Init.first, Init.second);
  }

  SmallVector<SILGlobalVariable *, 8> addrGlobals;
  for (auto &addrPair : GlobalAddrMap) {
    // Don't optimize functions that are marked with the opt.never attribute.
    bool shouldOptimize = true;
    for (auto *addr : addrPair.second) {
      if (!addr->getFunction()->shouldOptimize()) {
        shouldOptimize = false;
        break;
      }
    }
    if (!shouldOptimize)
      continue;

    addrGlobals.push_back(addrPair.first);
  }

  for (auto *global : addrGlobals) {
    HasChanged |= tryRemoveGlobalAddr(global);
  }

  SmallVector<std::pair<SILGlobalVariable *, AllocGlobalInst *>, 12>
      globalAllocPairs;
  for (auto &alloc : AllocGlobalStore) {
    if (!alloc.second->getFunction()->shouldOptimize())
      continue;
    globalAllocPairs.push_back(std::make_pair(alloc.first, alloc.second));
  }

  for (auto &allocPair : globalAllocPairs) {
    HasChanged |= tryRemoveGlobalAlloc(allocPair.first, allocPair.second);
  }

  // Erase the instructions that we have marked for deletion.
  for (auto *inst : InstToRemove) {
    eraseUsesOfInstruction(inst);
    inst->eraseFromParent();
  }

  for (auto &global : Module->getSILGlobals()) {
    HasChanged |= tryRemoveUnusedGlobal(&global);
  }

  for (auto *global : GlobalsToRemove) {
    Module->eraseGlobalVariable(global);
  }

  // Reset in case we re-run this function (when HasChanged is true).
  reset();
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
    if (SILGlobalOpt(FunctionBuilder, getModule(), DA, PM).run()) {
      invalidateAll();
    }
  }
};

} // end anonymous namespace

SILTransform *swift::createGlobalOpt() {
  return new SILGlobalOptPass();
}
