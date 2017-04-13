//===--- AccessEnforcementSelection.cpp - Select access enforcement -------===//
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
///
/// This pass eliminates 'unknown' access enforcement by selecting either
/// static or dynamic enforcement.
/// 
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "access-enforcement-selection"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"

using namespace swift;

static void setStaticEnforcement(BeginAccessInst *access) {
  // TODO: delete if we're not using static enforcement?
  access->setEnforcement(SILAccessEnforcement::Static);

  DEBUG(llvm::dbgs() << "Static Access: " << *access);
}

static void setDynamicEnforcement(BeginAccessInst *access) {
  // TODO: delete if we're not using dynamic enforcement?
  access->setEnforcement(SILAccessEnforcement::Dynamic);

  DEBUG(llvm::dbgs() << "Dynamic Access: " << *access);
}

namespace {
class SelectEnforcement {
  AllocBoxInst *Box;

  /// A state for tracking escape information about a variable.
  /// StateMap only has entries for blocks for which the variable
  /// has potentially escaped at exit.
  struct State {
    bool IsInWorklist = false;

    // At least one of the following must be true.
    bool HasEscape = false;
    bool HasPotentiallyEscapedAtEntry = false;

    // In a more advanced problem, this could easily be passed a State.
    bool adjustForEscapeInPredecessor() {
      bool updateSuccessors = false;

      if (!HasPotentiallyEscapedAtEntry) {
        HasPotentiallyEscapedAtEntry = true;
        updateSuccessors = !HasEscape;
      }

      return updateSuccessors;
    }
  };
  llvm::DenseMap<SILBasicBlock*, State> StateMap;

  /// All the accesses in the function.
  SmallVector<BeginAccessInst*, 8> Accesses;

  /// All the escapes in the function.
  SmallPtrSet<SILInstruction*, 8> Escapes;

  /// A worklist we use for various purposes.
  SmallVector<SILBasicBlock*, 8> Worklist;

public:
  SelectEnforcement(AllocBoxInst *box) : Box(box) {}

  void run();

private:
  void analyzeUsesOfBox();
  void analyzeProjection(ProjectBoxInst *projection);

  void propagateEscapes();
  void propagateEscapesFrom(SILBasicBlock *bb);

  bool hasPotentiallyEscapedAt(SILInstruction *inst);
  bool hasPotentiallyEscapedAtAnyReachableBlock(BeginAccessInst *access);

  void updateAccesses();
  void updateAccess(BeginAccessInst *access);
};
} // end anonymous namespace

void SelectEnforcement::run() {
  // Set up the data-flow problem.
  analyzeUsesOfBox();

  // Run the data-flow problem.
  propagateEscapes();

  // Update all the accesses.
  updateAccesses();
}

void SelectEnforcement::analyzeUsesOfBox() {
  // Collect accesses rooted off of projections.
  for (auto use : Box->getUses()) {
    auto user = use->getUser();

    if (auto projection = dyn_cast<ProjectBoxInst>(user)) {
      analyzeProjection(projection);
      continue;
    }
      
    // Ignore certain other uses that do not capture the value.
    if (isa<StrongRetainInst>(user) ||
        isa<StrongReleaseInst>(user) ||
        isa<DestroyValueInst>(user) ||
        isa<DeallocBoxInst>(user))
      continue;

    // Treat everything else as an escape:

    // Add it to the escapes set.
    Escapes.insert(user);

    // 
    auto userBB = user->getParent();
    auto &state = StateMap[userBB];
    if (!state.IsInWorklist) {
      state.HasEscape = true;
      state.IsInWorklist = true;
      Worklist.push_back(userBB);
    }
    assert(state.HasEscape);
    assert(state.IsInWorklist);
  }

  assert(!Accesses.empty() && "didn't find original access!");
}

void SelectEnforcement::analyzeProjection(ProjectBoxInst *projection) {
  for (auto use : projection->getUses()) {
    auto user = use->getUser();

    // Collect accesses.
    if (auto access = dyn_cast<BeginAccessInst>(user)) {
      if (access->getEnforcement() == SILAccessEnforcement::Unknown)
        Accesses.push_back(access);
    }
  }
}

void SelectEnforcement::propagateEscapes() {
  while (!Worklist.empty()) {
    auto bb = Worklist.pop_back_val();
    auto it = StateMap.find(bb);
    assert(it != StateMap.end() &&
           "block was in worklist but doesn't have a tracking state");
    auto &state = it->second;
    assert(state.HasEscape || state.HasPotentiallyEscapedAtEntry);
    state.IsInWorklist = false;
    propagateEscapesFrom(bb);
  }
}

/// Given that the box potentially escaped before we exited the
/// given block, propagate that information to all of its successors.
void SelectEnforcement::propagateEscapesFrom(SILBasicBlock *bb) {
  assert(StateMap.count(bb));

  // Iterate over the successors of the block.
  for (SILBasicBlock *succ : bb->getSuccessors()) {
    auto &succState = StateMap[succ];

    // If updating the successor changes it in a way that will
    // require us to update its successors, add it to the worklist.
    if (succState.adjustForEscapeInPredecessor()) {
      if (!succState.IsInWorklist) {
        succState.IsInWorklist = true;
        Worklist.push_back(succ);
      }
    }
  }
}

bool SelectEnforcement::hasPotentiallyEscapedAt(SILInstruction *point) {
  auto bb = point->getParent();

  // If we're not tracking anything for the whole block containing
  // the instruction, we're done; it hasn't escaped here.
  auto it = StateMap.find(bb);
  if (it == StateMap.end())
    return false;

  // If the tracking information says there are escapes before entry,
  // we're done; it has potentially escaped.
  const auto &state = it->second;
  if (state.HasPotentiallyEscapedAtEntry)
    return true;

  // Okay, there must be an escape within this block.
  assert(state.HasEscape);
  for (auto ii = point->getIterator(), ie = bb->begin(); ii != ie; ) {
    auto inst = &*--ii;

    // Maybe just record the first escape in the block and see if we
    // come after it?
    if (Escapes.count(inst))
      return true;
  }

  return false;
}

bool SelectEnforcement::hasPotentiallyEscapedAtAnyReachableBlock(
                                                      BeginAccessInst *access) {
  // Fast path: we're not tracking any escapes.  (But the box should
  // probably have been promoted to the stack in this case.)
  if (StateMap.empty())
    return false;

  auto bb = access->getParent();

  assert(Worklist.empty());
  SmallPtrSet<SILBasicBlock*, 8> visited;
  visited.insert(bb);
  Worklist.push_back(bb);

  while (!Worklist.empty()) {
    bb = Worklist.pop_back_val();
    assert(visited.count(bb));

    // If we're tracking information for this block, there's an escape.
    if (StateMap.count(bb))
      return true;

    // Add all reachable successors.
    for (SILBasicBlock *succ : bb->getSuccessors()) {
      if (visited.insert(succ).second)
        Worklist.push_back(succ);
    }
  }

  // No reachable block has an escape.
  return false;
}

void SelectEnforcement::updateAccesses() {
  for (auto access : Accesses) {
    updateAccess(access);
  }
}

void SelectEnforcement::updateAccess(BeginAccessInst *access) {
  assert(access->getEnforcement() == SILAccessEnforcement::Unknown);

  // Check whether the variable escaped before any of the end_accesses.
  bool anyDynamic = false;
  bool hasEndAccess = false;
  for (auto endAccess : access->getEndAccesses()) {
    hasEndAccess = true;
    if (hasPotentiallyEscapedAt(endAccess)) {
      anyDynamic = true;
      break;
    }
  }

  // If so, make the access dynamic.
  if (anyDynamic)
    return setDynamicEnforcement(access);

  // Otherwise, if there are no end_access instructions,
  // check the terminators of every reachable block.
  if (!hasEndAccess) {
    if (hasPotentiallyEscapedAtAnyReachableBlock(access))
      return setDynamicEnforcement(access);
  }

  // Otherwise, use static enforcement.
  setStaticEnforcement(access);
}

namespace {

/// The pass.
struct AccessEnforcementSelection : SILFunctionTransform {
  void run() override {
    DEBUG(llvm::dbgs() << "Access Enforcement Selection in "
          << getFunction()->getName() << "\n");

    for (auto &bb : *getFunction()) {
      for (auto ii = bb.begin(), ie = bb.end(); ii != ie; ) {
        SILInstruction *inst = &*ii;
        ++ii;

        if (auto access = dyn_cast<BeginAccessInst>(inst)) {
          handleAccess(access);
        }
      }
    }
    invalidateAnalysis(SILAnalysis::InvalidationKind::Instructions);
  }

  void handleAccess(BeginAccessInst *access) {
    if (access->getEnforcement() != SILAccessEnforcement::Unknown)
      return;

    auto address = access->getOperand();
    assert(!isa<MarkUninitializedInst>(address) &&
           "pass should be run after definite initialization");

    if (auto box = dyn_cast<ProjectBoxInst>(address)) {
      return handleAccessToBox(access, box);
    }
    if (auto arg = dyn_cast<SILFunctionArgument>(address)) {
      switch (arg->getArgumentConvention()) {
      case SILArgumentConvention::Indirect_Inout:
        // `inout` arguments are checked on the caller side, either statically
        // or dynamically if necessary. The @inout does not alias and cannot
        // escape within the callee, so static enforcement is always sufficient.
        setStaticEnforcement(access);
        break;
      case SILArgumentConvention::Indirect_InoutAliasable:
        // `inout_aliasable` are not enforced on the caller side. Dynamic
        // enforcement is required unless we have special knowledge of how this
        // closure is used at its call-site.
        //
        // TODO: optimize closures passed to call sites in which the captured
        // variable is not modified by any closure passed to the same call.
        setDynamicEnforcement(access);
        break;
      default:
        llvm_unreachable("Expecting an indirect argument.");
      }
      return;
    }

    // If we're not accessing a box or argument, we must've lowered to a stack
    // element. Other sources of access are either outright dynamic (GlobalAddr,
    // RefElementAddr), or only exposed after mandator inlining (nested
    // dependent BeginAccess).
    assert(isa<AllocStackInst>(address));
    setStaticEnforcement(access);
  }

  void handleAccessToBox(BeginAccessInst *access, ProjectBoxInst *projection) {
    // If we didn't allocate the box, assume that we need to use
    // dynamic enforcement.
    // TODO: use static enforcement in certain provable cases.
    auto box = dyn_cast<AllocBoxInst>(projection->getOperand());
    if (!box) {
      setDynamicEnforcement(access);
      return;
    }

    SelectEnforcement(box).run();
  }
};

} // end anonymous namespace

SILTransform *swift::createAccessEnforcementSelection() {
  return new AccessEnforcementSelection();
}
