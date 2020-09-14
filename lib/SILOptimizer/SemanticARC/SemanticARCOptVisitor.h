//===--- SemanticARCOptVisitor.h ------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SILOPTIMIZER_SEMANTICARC_SEMANTICARCOPTVISITOR_H
#define SWIFT_SILOPTIMIZER_SEMANTICARC_SEMANTICARCOPTVISITOR_H

#include "OwnershipLiveRange.h"

#include "swift/Basic/BlotSetVector.h"
#include "swift/Basic/FrozenMultiMap.h"
#include "swift/Basic/MultiMapCache.h"
#include "swift/SIL/BasicBlockUtils.h"
#include "swift/SIL/OwnershipUtils.h"
#include "swift/SIL/SILVisitor.h"
#include "swift/SILOptimizer/Utils/InstOptUtils.h"
#include "swift/SILOptimizer/Utils/ValueLifetime.h"

namespace swift {
namespace semanticarc {

extern bool VerifyAfterTransform;

bool constructCacheValue(
    SILValue initialValue,
    SmallVectorImpl<Operand *> &wellBehavedWriteAccumulator);

/// A visitor that optimizes ownership instructions and eliminates any trivially
/// dead code that results after optimization. It uses an internal worklist that
/// is initialized on construction with targets to avoid iterator invalidation
/// issues. Rather than revisit the entire CFG like SILCombine and other
/// visitors do, we maintain a visitedSinceLastMutation list to ensure that we
/// revisit all interesting instructions in between mutations.
struct LLVM_LIBRARY_VISIBILITY SemanticARCOptVisitor
    : SILInstructionVisitor<SemanticARCOptVisitor, bool> {
  /// Our main worklist. We use this after an initial run through.
  SmallBlotSetVector<SILValue, 32> worklist;

  /// A set of values that we have visited since the last mutation. We use this
  /// to ensure that we do not visit values twice without mutating.
  ///
  /// This is specifically to ensure that we do not go into an infinite loop
  /// when visiting phi nodes.
  SmallBlotSetVector<SILValue, 16> visitedSinceLastMutation;

  SILFunction &F;
  Optional<DeadEndBlocks> TheDeadEndBlocks;
  ValueLifetimeAnalysis::Frontier lifetimeFrontier;
  SmallMultiMapCache<SILValue, Operand *> addressToExhaustiveWriteListCache;

  /// Are we assuming that we reached a fix point and are re-processing to
  /// prepare to use the phiToIncomingValueMultiMap.
  bool assumingAtFixedPoint = false;

  /// A map from a value that acts as a "joined owned introducer" in the def-use
  /// graph.
  ///
  /// A "joined owned introducer" is a value with owned ownership whose
  /// ownership is derived from multiple non-trivial owned operands of a related
  /// instruction. Some examples are phi arguments, tuples, structs. Naturally,
  /// all of these instructions must be non-unary instructions and only have
  /// this property if they have multiple operands that are non-trivial.
  ///
  /// In such a case, we can not just treat them like normal forwarding concepts
  /// since we can only eliminate optimize such a value if we are able to reason
  /// about all of its operands together jointly. This is not amenable to a
  /// small peephole analysis.
  ///
  /// Instead, as we perform the peephole analysis, using the multimap, we map
  /// each joined owned value introducer to the set of its @owned operands that
  /// we thought we could convert to guaranteed only if we could do the same to
  /// the joined owned value introducer. Then once we finish performing
  /// peepholes, we iterate through the map and see if any of our joined phi
  /// ranges had all of their operand's marked with this property by iterating
  /// over the multimap. Since we are dealing with owned values and we know that
  /// our LiveRange can not see through joined live ranges, we know that we
  /// should only be able to have a single owned value introducer for each
  /// consumed operand.
  FrozenMultiMap<SILValue, Operand *> joinedOwnedIntroducerToConsumedOperands;

  /// If set to true, then we should only run cheap optimizations that do not
  /// build up data structures or analyze code in depth.
  ///
  /// As an example, we do not do load [copy] optimizations here since they
  /// generally involve more complex analysis, but simple peepholes of
  /// copy_values we /do/ allow.
  bool onlyGuaranteedOpts;

  using FrozenMultiMapRange =
      decltype(joinedOwnedIntroducerToConsumedOperands)::PairToSecondEltRange;

  explicit SemanticARCOptVisitor(SILFunction &F, bool onlyGuaranteedOpts)
      : F(F), addressToExhaustiveWriteListCache(constructCacheValue),
        onlyGuaranteedOpts(onlyGuaranteedOpts) {}

  DeadEndBlocks &getDeadEndBlocks() {
    if (!TheDeadEndBlocks)
      TheDeadEndBlocks.emplace(&F);
    return *TheDeadEndBlocks;
  }

  /// Given a single value instruction, RAUW it with newValue, add newValue to
  /// the worklist, and then call eraseInstruction on i.
  void eraseAndRAUWSingleValueInstruction(SingleValueInstruction *i,
                                          SILValue newValue) {
    worklist.insert(newValue);
    for (auto *use : i->getUses()) {
      for (SILValue result : use->getUser()->getResults()) {
        worklist.insert(result);
      }
    }
    i->replaceAllUsesWith(newValue);
    eraseInstructionAndAddOperandsToWorklist(i);
  }

  /// Add all operands of i to the worklist and then call eraseInstruction on
  /// i. Assumes that the instruction doesnt have users.
  void eraseInstructionAndAddOperandsToWorklist(SILInstruction *i) {
    // Then copy all operands into the worklist for future processing.
    for (SILValue v : i->getOperandValues()) {
      worklist.insert(v);
    }
    eraseInstruction(i);
  }

  /// Pop values off of visitedSinceLastMutation, adding .some values to the
  /// worklist.
  void drainVisitedSinceLastMutationIntoWorklist() {
    while (!visitedSinceLastMutation.empty()) {
      Optional<SILValue> nextValue = visitedSinceLastMutation.pop_back_val();
      if (!nextValue.hasValue()) {
        continue;
      }
      worklist.insert(*nextValue);
    }
  }

  /// Remove all results of the given instruction from the worklist and then
  /// erase the instruction. Assumes that the instruction does not have any
  /// users left.
  void eraseInstruction(SILInstruction *i) {
    // Remove all SILValues of the instruction from the worklist and then erase
    // the instruction.
    for (SILValue result : i->getResults()) {
      worklist.erase(result);
      visitedSinceLastMutation.erase(result);
    }
    i->eraseFromParent();

    // Add everything else from visitedSinceLastMutation to the worklist.
    drainVisitedSinceLastMutationIntoWorklist();
  }

  InstModCallbacks getCallbacks() {
    return InstModCallbacks(
        [this](SILInstruction *inst) { eraseInstruction(inst); },
        [](SILInstruction *) {}, [](SILValue, SILValue) {},
        [this](SingleValueInstruction *i, SILValue value) {
          eraseAndRAUWSingleValueInstruction(i, value);
        });
  }

  /// The default visitor.
  bool visitSILInstruction(SILInstruction *i) {
    assert(!isGuaranteedForwardingInst(i) &&
           "Should have forwarding visitor for all ownership forwarding "
           "instructions");
    return false;
  }

  bool visitCopyValueInst(CopyValueInst *cvi);
  bool visitBeginBorrowInst(BeginBorrowInst *bbi);
  bool visitLoadInst(LoadInst *li);
  static bool shouldVisitInst(SILInstruction *i) {
    switch (i->getKind()) {
    default:
      return false;
    case SILInstructionKind::CopyValueInst:
    case SILInstructionKind::BeginBorrowInst:
    case SILInstructionKind::LoadInst:
      return true;
    }
  }

#define FORWARDING_INST(NAME)                                                  \
  bool visit##NAME##Inst(NAME##Inst *cls) {                                    \
    for (SILValue v : cls->getResults()) {                                     \
      worklist.insert(v);                                                      \
    }                                                                          \
    return false;                                                              \
  }
  FORWARDING_INST(Tuple)
  FORWARDING_INST(Struct)
  FORWARDING_INST(Enum)
  FORWARDING_INST(OpenExistentialRef)
  FORWARDING_INST(Upcast)
  FORWARDING_INST(UncheckedRefCast)
  FORWARDING_INST(ConvertFunction)
  FORWARDING_INST(RefToBridgeObject)
  FORWARDING_INST(BridgeObjectToRef)
  FORWARDING_INST(UnconditionalCheckedCast)
  FORWARDING_INST(UncheckedEnumData)
  FORWARDING_INST(MarkUninitialized)
  FORWARDING_INST(SelectEnum)
  FORWARDING_INST(DestructureStruct)
  FORWARDING_INST(DestructureTuple)
  FORWARDING_INST(TupleExtract)
  FORWARDING_INST(StructExtract)
  FORWARDING_INST(OpenExistentialValue)
  FORWARDING_INST(OpenExistentialBoxValue)
  FORWARDING_INST(MarkDependence)
  FORWARDING_INST(InitExistentialRef)
  FORWARDING_INST(DifferentiableFunction)
  FORWARDING_INST(LinearFunction)
  FORWARDING_INST(DifferentiableFunctionExtract)
  FORWARDING_INST(LinearFunctionExtract)
#undef FORWARDING_INST

#define FORWARDING_TERM(NAME)                                                  \
  bool visit##NAME##Inst(NAME##Inst *cls) {                                    \
    for (auto succValues : cls->getSuccessorBlockArgumentLists()) {            \
      for (SILValue v : succValues) {                                          \
        worklist.insert(v);                                                    \
      }                                                                        \
    }                                                                          \
    return false;                                                              \
  }

  FORWARDING_TERM(SwitchEnum)
  FORWARDING_TERM(CheckedCastBranch)
  FORWARDING_TERM(Branch)
#undef FORWARDING_TERM

  bool isWrittenTo(LoadInst *li, const OwnershipLiveRange &lr);

  bool processWorklist();
  bool optimize();

  bool performGuaranteedCopyValueOptimization(CopyValueInst *cvi);
  bool eliminateDeadLiveRangeCopyValue(CopyValueInst *cvi);
  bool tryJoiningCopyValueLiveRangeWithOperand(CopyValueInst *cvi);
  bool performPostPeepholeOwnedArgElimination();
};

} // namespace semanticarc
} // namespace swift

#endif // SWIFT_SILOPTIMIZER_SEMANTICARC_SEMANTICARCOPTVISITOR_H
