//===--- StackPromotion.cpp - Promotes allocations to the stack -----------===//
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

#include "swift/SIL/BasicBlockUtils.h"
#include "swift/SIL/CFG.h"
#include "swift/SIL/DebugUtils.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SILOptimizer/Analysis/EscapeAnalysis.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/InstOptUtils.h"
#include "swift/SILOptimizer/Utils/StackNesting.h"
#include "swift/SILOptimizer/Utils/ValueLifetime.h"
#include "llvm/ADT/Statistic.h"

#define DEBUG_TYPE "stack-promotion"

STATISTIC(NumStackPromoted, "Number of objects promoted to the stack");

using namespace swift;

namespace {

/// Promotes heap allocated objects to the stack.
///
/// It handles alloc_ref instructions of native swift classes: if promoted,
/// the [stack] attribute is set in the alloc_ref and a dealloc_ref [stack] is
/// inserted at the end of the object's lifetime.
class StackPromotion : public SILFunctionTransform {

public:
  StackPromotion() {}

private:
  /// The entry point to the transformation.
  void run() override;

  /// Promotes allocations in \p BB.
  bool promoteInBlock(SILBasicBlock *BB, EscapeAnalysis *EA,
                      DeadEndBlocks &DEBlocks);

  /// Tries to promote the allocation \p ARI.
  bool tryPromoteAlloc(AllocRefInst *ARI, EscapeAnalysis *EA,
                       DeadEndBlocks &DEBlocks);

  /// Tries to promote the allocation \p ARI to an object. This optimization
  /// will only happen if the class type has a compiler-generated constructor
  /// and destructor. The promotion happens by scanning all uses in dominance
  /// order. If all members are accounted for by ref_element_addr instruction
  /// before we find any other use, then we can use those values to promote this
  /// alloc_ref to an object.
  bool tryPromoteToObject(AllocRefInst *allocRef,
                          ValueLifetimeAnalysis::Frontier &frontier);
};

void StackPromotion::run() {
  SILFunction *F = getFunction();
  // FIXME: We should be able to support ownership.
  if (F->hasOwnership())
    return;

  LLVM_DEBUG(llvm::dbgs() << "** StackPromotion in " << F->getName() << " **\n");

  auto *EA = PM->getAnalysis<EscapeAnalysis>();
  DeadEndBlocks DEBlocks(F);
  bool Changed = false;

  // Search the whole function for stack promotable allocations.
  for (SILBasicBlock &BB : *F) {
    Changed |= promoteInBlock(&BB, EA, DEBlocks);
  }
  if (!Changed)
    return;

  // Make sure that all stack allocating instructions are nested correctly.
  StackNesting SN;
  if (SN.correctStackNesting(F) == StackNesting::Changes::CFG) {
    invalidateAnalysis(SILAnalysis::InvalidationKind::BranchesAndInstructions);
  } else {
    invalidateAnalysis(SILAnalysis::InvalidationKind::Instructions);
  }
}

bool StackPromotion::promoteInBlock(SILBasicBlock *BB, EscapeAnalysis *EA,
                                    DeadEndBlocks &DEBlocks) {
  bool Changed = false;
  SmallVector<SILInstruction *, 64> allInstructions;
  for (SILInstruction &inst : *BB) {
    allInstructions.push_back(&inst);
  }
  for (auto *I : allInstructions) {
    if (auto *ARI = dyn_cast<AllocRefInst>(I)) {
      // Don't stack promote any allocation inside a code region which ends up
      // in a no-return block. Such allocations may missing their final release.
      // We would insert the deallocation too early, which may result in a
      // use-after-free problem.
      if (DEBlocks.isDeadEnd(BB))
        return false;

      Changed |= tryPromoteAlloc(ARI, EA, DEBlocks);
    }
  }
  return Changed;
}

bool StackPromotion::tryPromoteAlloc(AllocRefInst *ARI, EscapeAnalysis *EA,
                                     DeadEndBlocks &DEBlocks) {
  if (ARI->isObjC() || ARI->canAllocOnStack())
    return false;

  auto *ConGraph = EA->getConnectionGraph(ARI->getFunction());
  auto *contentNode = ConGraph->getValueContent(ARI);
  if (!contentNode)
    return false;

  // The most important check: does the object escape the current function?
  if (contentNode->escapes())
    return false;

  LLVM_DEBUG(llvm::dbgs() << "Promote " << *ARI);

  // Collect all use-points of the allocation. These are refcount instructions
  // and apply instructions.
  llvm::SmallVector<SILInstruction *, 8> UsePoints;
  ConGraph->getUsePoints(contentNode, UsePoints);

  ValueLifetimeAnalysis VLA(ARI, UsePoints);
  // Check if there is a use point before the allocation (this can happen e.g.
  // if the allocated object is stored into another object, which is already
  // alive at the allocation point).
  // In such a case the value lifetime extends up to the function entry.
  if (VLA.isAliveAtBeginOfBlock(ARI->getFunction()->getEntryBlock())) {
    LLVM_DEBUG(llvm::dbgs() << "  use before allocation -> don't promote");
    return false;
  }

  // Compute the places where the lifetime of the object ends.
  ValueLifetimeAnalysis::Frontier Frontier;
  if (!VLA.computeFrontier(Frontier, ValueLifetimeAnalysis::UsersMustPostDomDef,
                           &DEBlocks)) {
    LLVM_DEBUG(llvm::dbgs() << "  uses don't post-dom allocation -> don't promote");
    return false;
  }
  NumStackPromoted++;

  if (tryPromoteToObject(ARI, Frontier))
    return true;

  // We set the [stack] attribute in the alloc_ref.
  ARI->setStackAllocatable();

  /// And create dealloc_ref [stack] at the end of the object's lifetime.
  for (SILInstruction *FrontierInst : Frontier) {
    SILBuilder B(FrontierInst);
    B.createDeallocRef(ARI->getLoc(), ARI, true);
  }
  return true;
}

static void getOrderedNonDebugUses(SILValue v, DominanceInfo *domInfo,
                                   SmallVectorImpl<Operand *> &uses) {
  auto unsorted = getNonDebugUses(v);
  uses.append(unsorted.begin(), unsorted.end());
  llvm::sort(uses, [&domInfo](Operand *a, Operand *b) {
    return domInfo->dominates(a->getUser(), b->getUser());
  });
}

/// Gets the correct store ownership qualifier to initialize an address based on
/// the value. If the function has no ownership, \returns unqualified. If the
/// value is trivial, \returns trival. Otherwise, \returns init.
static StoreOwnershipQualifier storeInitQualifier(SILValue v) {
  if (v->getFunction()->hasOwnership())
    return v->getType().isTrivial(*v->getFunction())
               ? StoreOwnershipQualifier::Trivial
               : StoreOwnershipQualifier::Init;
  return StoreOwnershipQualifier::Unqualified;
}

static void copyTupleToRef(SILValue src, AllocRefInst *refDest,
                           SILBuilder &builder) {
  auto loc = src.getLoc();
  auto classDecl = refDest->getType().getClassOrBoundGenericClass();
  auto tupleSrc = dyn_cast<TupleInst>(src);
  // Handle if there is only one propery, the source could be any type.
  if (!tupleSrc) {
    assert(classDecl->getStoredProperties().size() == 1 &&
           "If the source is not a tuple, there must only be a single stored "
           "property.");
    auto onlyProp = classDecl->getStoredProperties().front();
    auto elementDest = builder.createRefElementAddr(loc, refDest, onlyProp);
    builder.createStore(loc, src, elementDest, storeInitQualifier(src));
    return;
  }
  // Otherwise, copy every tuple element into the class.
  unsigned i = 0;
  for (auto *prop : classDecl->getStoredProperties()) {
    auto elementDest = builder.createRefElementAddr(loc, refDest, prop);
    auto elementSrc = builder.createTupleExtract(loc, tupleSrc, i++);
    builder.createStore(loc, elementSrc, elementDest,
                        storeInitQualifier(elementSrc));
  }
}

bool StackPromotion::tryPromoteToObject(
    AllocRefInst *allocRef, ValueLifetimeAnalysis::Frontier &frontier) {
  if (allocRef->getTailAllocatedCounts().size())
    return false;

  DominanceInfo *domInfo =
      PM->getAnalysis<DominanceAnalysis>()->get(allocRef->getFunction());
  auto *classDecl = allocRef->getType().getClassOrBoundGenericClass();
  SmallVector<VarDecl *, 8> props;
  for (auto *prop : classDecl->getStoredProperties()) {
    props.push_back(prop);
  }

  SmallVector<Operand *, 24> uses;
  getOrderedNonDebugUses(allocRef, domInfo, uses);
  // Reverse the properties so we can pop_back a property that matches the next
  // initializer.
  std::reverse(props.begin(), props.end());
  SmallVector<RefElementAddrInst *, 8> propertyInitializers;
  for (auto *use : uses) {
    // Assume properties are initialized in order.
    auto propRef = dyn_cast<RefElementAddrInst>(use->getUser());
    if (!propRef)
      return false;

    auto f = props.pop_back_val();
    if (propRef->getField() != f)
      return false;

    propertyInitializers.push_back(propRef);

    if (props.empty())
      break;
  }
  // Bail if we haven't found all the properties.
  if (!props.empty())
    return false;

  // Collect the dead stores and values of the class class property
  // initializers.
  SmallVector<StoreInst *, 8> deadStores;
  SmallVector<SILValue, 8> elements;
  for (auto *init : propertyInitializers) {
    SmallVector<Operand *, 6> refElementUses;
    getOrderedNonDebugUses(init, domInfo, refElementUses);
    auto frontUser = refElementUses.front()->getUser();
    // Look through begin_access uses.
    if (auto *beginAccess = dyn_cast<BeginAccessInst>(frontUser)) {
      SmallVector<Operand *, 4> beginAccessUses;
      getOrderedNonDebugUses(beginAccess, domInfo, beginAccessUses);
      frontUser = beginAccessUses.front()->getUser();
    }
    // If the first use isn't a store, bail.
    if (auto *store = dyn_cast<StoreInst>(frontUser)) {
      elements.push_back(store->getSrc());
      deadStores.push_back(store);
    } else {
      return false;
    }
  }

  // Keep track of the last element so we know where to insert the tuple.
  SILInstruction *lastElement = nullptr;
  for (auto first = elements.rbegin(); first != elements.rend(); ++first) {
    auto inst = first->getDefiningInstruction();
    if (!inst)
      continue;

    if (!lastElement || domInfo->dominates(lastElement, inst))
      lastElement = inst;
  }

  // If we didn't find anything, that means that all the elements are arguments,
  // or there aren't any elements. Either way, we know that putting where the
  // alloc_ref is will work.
  if (!lastElement || domInfo->dominates(lastElement, allocRef))
    lastElement = allocRef;

  // Create a tuple to store the properties of the class. The tuple must go
  // after all element instructions.
  SILBuilder builder(std::next(lastElement->getIterator()));
  SILValue storedProps;
  // If there is only one element, we can't make a tuple so, just use that
  // element.
  if (elements.size() == 1)
    storedProps = elements[0];
  else
    // Otherwise, use a tuple to hold the stored properties.
    storedProps = builder.createTuple(lastElement->getLoc(), elements);
  // Make an alloc_stack so that we can replace ref_element_addr instructions
  // with tuple_element_addr instructions.
  auto storedPropsAddr =
      builder.createAllocStack(lastElement->getLoc(), storedProps->getType());
  auto refInitStore =
      builder.createStore(lastElement->getLoc(), storedProps, storedPropsAddr,
                          storeInitQualifier(storedProps));

  // Find the first use of the alloc_ref that isn't a ref_element_addr and
  // record where that use is. We use that as an upper bound for were we have to
  // stop replacing uses of the class reference.
  SmallVector<Operand *, 8> users;
  getOrderedNonDebugUses(allocRef, domInfo, users);
  SILInstruction *firstUnknownUse = nullptr;
  for (auto *use : users) {
    auto user = use->getUser();

    if (isa<SetDeallocatingInst>(user) || isa<DeallocRefInst>(user) ||
        isa<DebugValueInst>(user) || isa<StrongReleaseInst>(user) ||
        isa<RefElementAddrInst>(user))
      continue;

    firstUnknownUse = user;
    break;
  }

  // Keep track of the last instruction we've added so we know where to put the
  // final copy.
  SILInstruction *endUse = nullptr;
  for (auto *use : users) {
    auto user = use->getUser();

    // Either we'll remove the alloc ref in which case this needs to be removed,
    // or we'll add a dealloc_ref so we also need this to be removed.
    if (isa<StrongReleaseInst>(user)) {
      user->eraseFromParent();
      continue;
    }

    if (firstUnknownUse && domInfo->dominates(firstUnknownUse, user))
      continue;

    if (auto ref = dyn_cast<RefElementAddrInst>(user)) {
      // If this instruction is dead, remove it and continue. We do this so that
      // we can be sure endUse will be correct.
      if (ref->use_empty()) {
        ref->eraseFromParent();
        continue;
      }

      // Make sure all uses come before the first unknown use.
      for (auto *use : ref->getUses()) {
        if (firstUnknownUse &&
            domInfo->dominates(firstUnknownUse, use->getUser()))
          goto done;
        // Then update endUse so that we put the final copy in the right place.
        if (!endUse || domInfo->dominates(endUse, use->getUser()))
          endUse = use->getUser();
      }

      // Either replace ref with a tuple_element_addr or the alloc_stack if
      // there's only one element.
      if (isa<TupleInst>(storedProps)) {
        auto index = std::distance(
            classDecl->getStoredProperties().begin(),
            llvm::find(classDecl->getStoredProperties(), ref->getField()));
        builder.setInsertionPoint(std::next(refInitStore->getIterator()));
        auto tupleElementAddr = builder.createTupleElementAddr(
            ref->getLoc(), storedPropsAddr, index);
        ref->replaceAllUsesWith(tupleElementAddr);
      } else {
        ref->replaceAllUsesWith(storedPropsAddr);
      }
      ref->eraseFromParent();
    }
  done:
    continue;
  }

  // If there aren't any unknown uses, we will remove the alloc_ref so don't
  // bother copying the storage into it.
  if (firstUnknownUse) {
    // Copy the property storage to the alloc_ref after the last instruction
    // we've added. If none are added put it directly after the tuple
    // instruction. With LLVM optimization enabled, this will still be a
    // performance win in most (all?) cases.
    if (endUse) {
      builder.setInsertionPoint(std::next(endUse->getIterator()));
    } else {
      builder.setInsertionPoint(std::next(refInitStore->getIterator()));
    }
    copyTupleToRef(storedProps, allocRef, builder);
  }

  // Make sure we destroy/dealloc the property storage.
  for (SILInstruction *frontierInst : frontier) {
    SILBuilder deallocBuilder(frontierInst);
    deallocBuilder.createDestroyAddr(storedPropsAddr->getLoc(),
                                     storedPropsAddr);
    deallocBuilder.createDeallocStack(storedPropsAddr->getLoc(),
                                      storedPropsAddr);
  }

  // Cleanup the stores that are now dead.
  for (auto *store : deadStores) {
    store->eraseFromParent();
  }

  // If we don't have any unknown uses there's no reason for the class so still
  // exist so, remove it.
  if (!firstUnknownUse) {
    eraseUsesOfValue(allocRef);
    allocRef->eraseFromParent();
    // Return true so that we don't try to promote this alloc_ref to stack.
    return true;
  }

  // We were succesful in optimizing this alloc_ref but return false so that
  // StackPromotion will still try to promote this alloc_ref to stack.
  return false;
}

} // end anonymous namespace

SILTransform *swift::createStackPromotion() {
  return new StackPromotion();
}
