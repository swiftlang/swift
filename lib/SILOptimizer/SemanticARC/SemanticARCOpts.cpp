//===--- SemanticARCOpts.cpp ----------------------------------------------===//
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

#define DEBUG_TYPE "sil-semantic-arc-opts"

#include "swift/Basic/LLVM.h"

#include "OwnershipLiveRange.h"
#include "OwnershipPhiOperand.h"
#include "SemanticARCOptVisitor.h"

#include "swift/Basic/BlotSetVector.h"
#include "swift/Basic/FrozenMultiMap.h"
#include "swift/Basic/MultiMapCache.h"
#include "swift/Basic/STLExtras.h"
#include "swift/SIL/BasicBlockUtils.h"
#include "swift/SIL/DebugUtils.h"
#include "swift/SIL/LinearLifetimeChecker.h"
#include "swift/SIL/MemAccessUtils.h"
#include "swift/SIL/OwnershipUtils.h"
#include "swift/SIL/Projection.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILVisitor.h"
#include "swift/SILOptimizer/Analysis/PostOrderAnalysis.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/InstOptUtils.h"
#include "swift/SILOptimizer/Utils/ValueLifetime.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/CommandLine.h"

using namespace swift;
using namespace swift::semanticarc;

//===----------------------------------------------------------------------===//
//                               Implementation
//===----------------------------------------------------------------------===//

static llvm::cl::opt<bool>
    VerifyAfterTransform("sil-semantic-arc-opts-verify-after-transform",
                         llvm::cl::init(false), llvm::cl::Hidden);

static bool canEliminatePhi(
    SemanticARCOptVisitor::FrozenMultiMapRange optimizableIntroducerRange,
    ArrayRef<OwnershipPhiOperand> incomingValueOperandList,
    SmallVectorImpl<OwnedValueIntroducer> &ownedValueIntroducerAccumulator) {
  for (auto incomingValueOperand : incomingValueOperandList) {
    SILValue incomingValue = incomingValueOperand.getValue();

    // Before we do anything, see if we have an incoming value with trivial
    // ownership. This can occur in the case where we are working with enums due
    // to trivial non-payloaded cases. Skip that.
    if (incomingValue.getOwnershipKind() == ValueOwnershipKind::None) {
      continue;
    }

    // Then see if this is an introducer that we actually saw as able to be
    // optimized if we could flip this joined live range.
    //
    // NOTE: If this linear search is too slow, we can change the multimap to
    // sort the mapped to list by pointer instead of insertion order. In such a
    // case, we could then bisect.
    if (llvm::find(optimizableIntroducerRange,
                   incomingValueOperand.getOperand()) ==
        optimizableIntroducerRange.end()) {
      return false;
    }

    // Now that we know it is an owned value that we saw before, check for
    // introducers of the owned value which are the copies that we may be able
    // to eliminate. Since we do not look through joined live ranges, we must
    // only have a single introducer. So look for that one and if not, bail.
    auto singleIntroducer = getSingleOwnedValueIntroducer(incomingValue);
    if (!singleIntroducer.hasValue()) {
      return false;
    }

    // Then make sure that our owned value introducer is able to be converted to
    // guaranteed and that we found it to have a LiveRange that we could have
    // eliminated /if/ we were to get rid of this phi.
    if (!singleIntroducer->isConvertableToGuaranteed()) {
      return false;
    }

    // Otherwise, add the introducer to our result array.
    ownedValueIntroducerAccumulator.push_back(*singleIntroducer);
  }

#ifndef NDEBUG
  // Other parts of the pass ensure that we only add values to the list if their
  // owned value introducer is not used by multiple live ranges. That being
  // said, lets assert that.
  {
    SmallVector<OwnedValueIntroducer, 32> uniqueCheck;
    llvm::copy(ownedValueIntroducerAccumulator,
               std::back_inserter(uniqueCheck));
    sortUnique(uniqueCheck);
    assert(
        uniqueCheck.size() == ownedValueIntroducerAccumulator.size() &&
        "multiple joined live range operands are from the same live range?!");
  }
#endif

  return true;
}

static bool getIncomingJoinedLiveRangeOperands(
    SILValue joinedLiveRange,
    SmallVectorImpl<OwnershipPhiOperand> &resultingOperands) {
  if (auto *phi = dyn_cast<SILPhiArgument>(joinedLiveRange)) {
    return phi->visitIncomingPhiOperands([&](Operand *op) {
      if (auto phiOp = OwnershipPhiOperand::get(op)) {
        resultingOperands.push_back(*phiOp);
        return true;
      }
      return false;
    });
  }

  if (auto *svi = dyn_cast<SingleValueInstruction>(joinedLiveRange)) {
    return llvm::all_of(svi->getAllOperands(), [&](const Operand &op) {
      // skip type dependent operands.
      if (op.isTypeDependent())
        return true;

      auto phiOp = OwnershipPhiOperand::get(&op);
      if (!phiOp)
        return false;
      resultingOperands.push_back(*phiOp);
      return true;
    });
  }

  llvm_unreachable("Unhandled joined live range?!");
}

bool SemanticARCOptVisitor::performPostPeepholeOwnedArgElimination() {
  bool madeChange = false;

  // First freeze our multi-map so we can use it for map queries. Also, setup a
  // defer of the reset so we do not forget to reset the map when we are done.
  joinedOwnedIntroducerToConsumedOperands.setFrozen();
  SWIFT_DEFER { joinedOwnedIntroducerToConsumedOperands.reset(); };

  // Now for each phi argument that we have in our multi-map...
  SmallVector<OwnershipPhiOperand, 4> incomingValueOperandList;
  SmallVector<OwnedValueIntroducer, 4> ownedValueIntroducers;
  for (auto pair : joinedOwnedIntroducerToConsumedOperands.getRange()) {
    SWIFT_DEFER {
      incomingValueOperandList.clear();
      ownedValueIntroducers.clear();
    };

    // First compute the LiveRange for ownershipPhi value. For simplicity, we
    // only handle cases now where the result does not have any additional
    // ownershipPhi uses.
    SILValue joinedIntroducer = pair.first;
    OwnershipLiveRange joinedLiveRange(joinedIntroducer);
    if (bool(joinedLiveRange.hasUnknownConsumingUse())) {
      continue;
    }

    // Ok, we know that our phi argument /could/ be converted to guaranteed if
    // our incoming values are able to be converted to guaranteed. Now for each
    // incoming value, compute the incoming values ownership roots and see if
    // all of the ownership roots are in our owned incoming value array.
    if (!getIncomingJoinedLiveRangeOperands(joinedIntroducer,
                                            incomingValueOperandList)) {
      continue;
    }

    // Grab our list of introducer values paired with this SILArgument. See if
    // all of these introducer values were ones that /could/ have been
    // eliminated if it was not for the given phi. If all of them are, we can
    // optimize!
    {
      auto rawFoundOptimizableIntroducerArray = pair.second;
      if (!canEliminatePhi(rawFoundOptimizableIntroducerArray,
                           incomingValueOperandList, ownedValueIntroducers)) {
        continue;
      }
    }

    // Ok, at this point we know that we can eliminate this phi. First go
    // through the list of incomingValueOperandList and stash the value/set the
    // operand's stored value to undef. We will hook them back up later.
    SmallVector<SILValue, 8> originalIncomingValues;
    for (auto &incomingValueOperand : incomingValueOperandList) {
      originalIncomingValues.push_back(incomingValueOperand.getValue());
      incomingValueOperand.markUndef();
    }

    // Then go through all of our owned value introducers, compute their live
    // ranges, and eliminate them. We know it is safe to remove them from our
    // previous proofs.
    //
    // NOTE: If our introducer is a copy_value that is one of our
    // originalIncomingValues, we need to update the originalIncomingValue array
    // with that value since we are going to delete the copy_value here. This
    // creates a complication since we want to binary_search on
    // originalIncomingValues to detect this same condition! So, we create a
    // list of updates that we apply after we no longer need to perform
    // binary_search, but before we start RAUWing things.
    SmallVector<std::pair<SILValue, unsigned>, 8> incomingValueUpdates;
    for (auto introducer : ownedValueIntroducers) {
      SILValue v = introducer.value;
      OwnershipLiveRange lr(v);

      // For now, we only handle copy_value for simplicity.
      //
      // TODO: Add support for load [copy].
      if (introducer.kind == OwnedValueIntroducerKind::Copy) {
        auto *cvi = cast<CopyValueInst>(v);
        // Before we convert from owned to guaranteed, we need to first see if
        // cvi is one of our originalIncomingValues. If so, we need to set
        // originalIncomingValues to be cvi->getOperand(). Otherwise, weirdness
        // results since we are deleting one of our stashed values.
        auto iter = find(originalIncomingValues, cvi);
        if (iter != originalIncomingValues.end()) {
          // We use an auxillary array here so we can continue to bisect on
          // original incoming values. Once we are done processing here, we will
          // not need that property anymore.
          unsigned updateOffset =
              std::distance(originalIncomingValues.begin(), iter);
          incomingValueUpdates.emplace_back(cvi->getOperand(), updateOffset);
        }
        std::move(lr).convertToGuaranteedAndRAUW(cvi->getOperand(),
                                                 getCallbacks());
        continue;
      }
      llvm_unreachable("Unhandled optimizable introducer!");
    }

    // Now go through and update our original incoming value array now that we
    // do not need it to be sorted for bisection purposes.
    while (!incomingValueUpdates.empty()) {
      auto pair = incomingValueUpdates.pop_back_val();
      originalIncomingValues[pair.second] = pair.first;
    }

    // Then convert the phi's live range to be guaranteed.
    std::move(joinedLiveRange)
        .convertJoinedLiveRangePhiToGuaranteed(
            getDeadEndBlocks(), lifetimeFrontier, getCallbacks());

    // Now if our phi operand consumes/forwards its guaranteed input, insert a
    // begin_borrow along the incoming value edges. We have to do this after
    // converting the incoming values to be guaranteed to avoid tripping
    // SILBuilder checks around simple ownership invariants (namely that def/use
    // line up) when creating instructions.
    assert(incomingValueOperandList.size() == originalIncomingValues.size());
    while (!incomingValueOperandList.empty()) {
      auto incomingValueOperand = incomingValueOperandList.pop_back_val();
      SILValue originalValue = originalIncomingValues.pop_back_val();
      if (incomingValueOperand.isGuaranteedConsuming() &&
          originalValue.getOwnershipKind() != ValueOwnershipKind::None) {
        auto loc = RegularLocation::getAutoGeneratedLocation();
        SILBuilderWithScope builder(incomingValueOperand.getInst());
        originalValue = builder.createBeginBorrow(loc, originalValue);
      }
      incomingValueOperand.getOperand()->set(originalValue);
    }

    madeChange = true;
    if (VerifyAfterTransform) {
      F.verify();
    }
  }

  return madeChange;
}

bool SemanticARCOptVisitor::optimize() {
  bool madeChange = false;

  // First process the worklist until we reach a fixed point.
  madeChange |= processWorklist();

  {
    // If we made a change, set that we assume we are at fixed point and then
    // re-run the worklist so that we can
    // properly seeded the ARC peephole map.
    assumingAtFixedPoint = true;
    SWIFT_DEFER { assumingAtFixedPoint = false; };

    // Add everything in visitedSinceLastMutation to the worklist so we
    // recompute our fixed point.
    drainVisitedSinceLastMutationIntoWorklist();

    // Then re-run the worklist. We shouldn't modify anything since we are at a
    // fixed point and are just using this to seed the
    // joinedOwnedIntroducerToConsumedOperands after we have finished changing
    // things. If we did change something, we did something weird, so assert!
    bool madeAdditionalChanges = processWorklist();
    (void)madeAdditionalChanges;
    assert(!madeAdditionalChanges && "Should be at the fixed point");
  }

  // Then use the newly seeded peephole map to
  madeChange |= performPostPeepholeOwnedArgElimination();

  return madeChange;
}

bool SemanticARCOptVisitor::processWorklist() {
  // NOTE: The madeChange here is not strictly necessary since we only have
  // items added to the worklist today if we have already made /some/ sort of
  // change. That being said, I think there is a low cost to including this here
  // and makes the algorithm more correct, visually and in the face of potential
  // refactoring.
  bool madeChange = false;

  while (!worklist.empty()) {
    // Pop the last element off the list. If we were returned None, we blotted
    // this element, so skip it.
    SILValue next = worklist.pop_back_val().getValueOr(SILValue());
    if (!next)
      continue;

    // First check if this is a value that we have visited since the last time
    // we erased an instruction. If we have visited it, skip it. Every time we
    // modify something, we should be deleting an instruction, so we have not
    // found any further information.
    if (!visitedSinceLastMutation.insert(next).second) {
      continue;
    }

    // First check if this is an instruction that is trivially dead. This can
    // occur if we eliminate rr traffic resulting in dead projections and the
    // like.
    //
    // If we delete, we first add all of our deleted instructions operands to
    // the worklist and then remove all results (since we are going to delete
    // the instruction).
    if (auto *defInst = next->getDefiningInstruction()) {
      if (isInstructionTriviallyDead(defInst)) {
        assert(!assumingAtFixedPoint &&
               "Assumed was at fixed point and recomputing state?!");
        deleteAllDebugUses(defInst);
        eraseInstruction(defInst);
        madeChange = true;
        if (VerifyAfterTransform) {
          F.verify();
        }
        continue;
      }
    }

    // Otherwise, if we have a single value instruction (to be expanded later
    // perhaps), try to visit that value recursively.
    if (auto *svi = dyn_cast<SingleValueInstruction>(next)) {
      bool madeSingleChange = visit(svi);
      assert((!madeSingleChange || !assumingAtFixedPoint) &&
             "Assumed was at fixed point and modified state?!");
      madeChange |= madeSingleChange;
      if (VerifyAfterTransform && madeSingleChange) {
        F.verify();
      }
      continue;
    }
  }

  return madeChange;
}

//===----------------------------------------------------------------------===//
//                            Top Level Entrypoint
//===----------------------------------------------------------------------===//

namespace {

// Even though this is a mandatory pass, it is rerun after deserialization in
// case DiagnosticConstantPropagation exposed anything new in this assert
// configuration.
struct SemanticARCOpts : SILFunctionTransform {
  bool guaranteedOptsOnly;

  SemanticARCOpts(bool guaranteedOptsOnly)
      : guaranteedOptsOnly(guaranteedOptsOnly) {}

  void run() override {
    SILFunction &f = *getFunction();

    // Return early if we are not performing OSSA optimizations.
    if (!f.getModule().getOptions().EnableOSSAOptimizations)
      return;

    // Make sure we are running with ownership verification enabled.
    assert(f.getModule().getOptions().VerifySILOwnership &&
           "Can not perform semantic arc optimization unless ownership "
           "verification is enabled");

    SemanticARCOptVisitor visitor(f, guaranteedOptsOnly);

    // Add all the results of all instructions that we want to visit to the
    // worklist.
    for (auto &block : f) {
      for (auto &inst : block) {
        if (SemanticARCOptVisitor::shouldVisitInst(&inst)) {
          for (SILValue v : inst.getResults()) {
            visitor.worklist.insert(v);
          }
        }
      }
    }

    // Then process the worklist. We only destroy instructions, so invalidate
    // that. Once we modify the ownership of block arguments, we will need to
    // perhaps invalidate branches as well.
    if (visitor.optimize()) {
      invalidateAnalysis(
          SILAnalysis::InvalidationKind::BranchesAndInstructions);
    }
  }
};

} // end anonymous namespace

SILTransform *swift::createSemanticARCOpts() {
  return new SemanticARCOpts(false /*guaranteed*/);
}

SILTransform *swift::createGuaranteedARCOpts() {
  return new SemanticARCOpts(true /*guaranteed*/);
}
