//===--- SendNonSendable.cpp ----------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/AST/DiagnosticsSIL.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Type.h"
#include "swift/Basic/FrozenMultiMap.h"
#include "swift/SIL/BasicBlockData.h"
#include "swift/SIL/BasicBlockDatastructures.h"
#include "swift/SIL/MemAccessUtils.h"
#include "swift/SIL/NodeDatastructures.h"
#include "swift/SIL/OwnershipUtils.h"
#include "swift/SIL/SILBasicBlock.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/PartitionUtils.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/Support/Debug.h"

#define DEBUG_TYPE "send-non-sendable"

using namespace swift;

//===----------------------------------------------------------------------===//
//                              MARK: Utilities
//===----------------------------------------------------------------------===//

/// SILApplyCrossesIsolation determines if a SIL instruction is an isolation
/// crossing apply expression. This is done by checking its correspondence to an
/// ApplyExpr AST node, and then checking the internal flags of that AST node to
/// see if the ActorIsolationChecker determined it crossed isolation.  It's
/// possible this is brittle and a more nuanced check is needed, but this
/// suffices for all cases tested so far.
static bool SILApplyCrossesIsolation(const SILInstruction *inst) {
  if (ApplyExpr *apply = inst->getLoc().getAsASTNode<ApplyExpr>())
    return apply->getIsolationCrossing().has_value();

  // We assume that any instruction that does not correspond to an ApplyExpr
  // cannot cross an isolation domain.
  return false;
}

static AccessStorage getAccessStorageFromAddr(SILValue value) {
  assert(value->getType().isAddress());
  auto accessStorage = AccessStorage::compute(value);
  if (accessStorage && accessStorage.getRoot()) {
    if (auto definingInst = accessStorage.getRoot().getDefiningInstruction()) {
      if (isa<InitExistentialAddrInst, CopyValueInst>(definingInst))
        // look through these because AccessStorage does not
        return getAccessStorageFromAddr(definingInst->getOperand(0));
    }
  }

  return accessStorage;
}

static SILValue getUnderlyingTrackedValue(SILValue value) {
  if (!value->getType().isAddress()) {
    return getUnderlyingObject(value);
  }

  if (auto accessStorage = getAccessStorageFromAddr(value)) {
    if (accessStorage.getKind() == AccessRepresentation::Kind::Global)
      // globals don't reduce
      return value;
    assert(accessStorage.getRoot());
    return accessStorage.getRoot();
  }

  return value;
}

namespace {

struct TermArgSources {
  SmallFrozenMultiMap<SILValue, SILValue, 8> argSources;

  template <typename ValueRangeTy = ArrayRef<SILValue>>
  void addValues(ValueRangeTy valueRange, SILBasicBlock *destBlock) {
    for (auto pair : llvm::enumerate(valueRange))
      argSources.insert(destBlock->getArgument(pair.index()), pair.value());
  }
};

} // namespace

/// Used for generating informative diagnostics.
static Expr *getExprForPartitionOp(const PartitionOp &op) {
  SILInstruction *sourceInstr = op.getSourceInst(/*assertNonNull=*/true);
  Expr *expr = sourceInstr->getLoc().getAsASTNode<Expr>();
  assert(expr && "PartitionOp's source location should correspond to"
                 "an AST node");
  return expr;
}

//===----------------------------------------------------------------------===//
//                           MARK: Main Computation
//===----------------------------------------------------------------------===//

namespace {

static const char *SEP_STR = "╾──────────────────────────────╼\n";

using TrackableValueID = PartitionPrimitives::Element;
using Region = PartitionPrimitives::Region;

enum class TrackableValueFlag {
  /// Base value that says a value is uniquely represented and is
  /// non-sendable. Example: an alloc_stack of a non-Sendable type that isn't
  /// captured by a closure.
  None = 0x0,

  /// Set to true if this TrackableValue's representative is not uniquely
  /// represented so may have aliases. Example: a value that isn't an
  /// alloc_stack.
  isMayAlias = 0x1,

  /// Set to true if this TrackableValue's representative is Sendable.
  isSendable = 0x2,

  /// Set to true if this TrackableValue is a non-sendable object derived from
  /// an actor. Example: a value loaded from a ref_element_addr from an actor.
  ///
  /// NOTE: We track values with an actor representative even though actors are
  /// sendable to be able to properly identify values that escape an actor since
  /// if we escape an actor into a closure, we want to mark the closure as actor
  /// derived.
  isActorDerived = 0x4,
};

using TrackedValueFlagSet = OptionSet<TrackableValueFlag>;

class TrackableValueState {
  unsigned id;
  TrackedValueFlagSet flagSet = {TrackableValueFlag::isMayAlias};

public:
  TrackableValueState(unsigned newID) : id(newID) {}

  bool isMayAlias() const {
    return flagSet.contains(TrackableValueFlag::isMayAlias);
  }

  bool isNoAlias() const { return !isMayAlias(); }

  bool isSendable() const {
    return flagSet.contains(TrackableValueFlag::isSendable);
  }

  bool isNonSendable() const { return !isSendable(); }

  bool isActorDerived() const {
    return flagSet.contains(TrackableValueFlag::isActorDerived);
  }

  TrackableValueID getID() const { return TrackableValueID(id); }

  void addFlag(TrackableValueFlag flag) { flagSet |= flag; }

  void removeFlag(TrackableValueFlag flag) { flagSet -= flag; }

  void print(llvm::raw_ostream &os) const {
    os << "TrackableValueState[id: " << id
       << "][is_no_alias: " << (isNoAlias() ? "yes" : "no")
       << "][is_sendable: " << (isSendable() ? "yes" : "no")
       << "][is_actor_derived: " << (isActorDerived() ? "yes" : "no") << "].";
  }

  SWIFT_DEBUG_DUMP { print(llvm::dbgs()); }
};

/// A tuple consisting of a base value and its value state.
///
/// DISCUSSION: We are computing regions among equivalence classes of values
/// with GEPs like struct_element_addr being considered equivalent from a value
/// perspective to their underlying base value.
///
/// Example:
///
/// ```
/// %0 = alloc_stack $Struct
/// %1 = struct_element_addr %0 : $Struct.childField
/// %2 = struct_element_addr %1 : $ChildField.grandchildField
/// ```
///
/// In the above example, %2 will be mapped to %0 by our value mapping.
class TrackableValue {
  SILValue representativeValue;
  TrackableValueState valueState;

public:
  TrackableValue(SILValue representativeValue, TrackableValueState valueState)
      : representativeValue(representativeValue), valueState(valueState) {}

  bool isMayAlias() const { return valueState.isMayAlias(); }

  bool isNoAlias() const { return !isMayAlias(); }

  bool isSendable() const { return valueState.isSendable(); }

  bool isNonSendable() const { return !isSendable(); }

  bool isActorDerived() const { return valueState.isActorDerived(); }

  TrackableValueID getID() const {
    return TrackableValueID(valueState.getID());
  }

  /// Return the representative value of this equivalence class of values.
  SILValue getRepresentative() const { return representativeValue; }

  void print(llvm::raw_ostream &os) const {
    os << "TrackableValue. State: ";
    valueState.print(os);
    os << "\n    Rep Value: " << *getRepresentative();
  }

  SWIFT_DEBUG_DUMP { print(llvm::dbgs()); }
};

class PartitionOpTranslator;

struct PartitionOpBuilder {
  /// Parent translator that contains state.
  PartitionOpTranslator *translater;

  /// Used to statefully track the instruction currently being translated, for
  /// insertion into generated PartitionOps.
  SILInstruction *currentInst = nullptr;

  /// List of partition ops mapped to the current instruction. Used when
  /// generating partition ops.
  SmallVector<PartitionOp, 8> currentInstPartitionOps;

  void reset(SILInstruction *inst) {
    currentInst = inst;
    currentInstPartitionOps.clear();
  }

  TrackableValueID lookupValueID(SILValue value);
  bool valueHasID(SILValue value, bool dumpIfHasNoID = false);

  void addAssignFresh(SILValue value) {
    currentInstPartitionOps.emplace_back(
        PartitionOp::AssignFresh(lookupValueID(value), currentInst));
  }

  void addAssign(SILValue tgt, SILValue src) {
    assert(valueHasID(src, /*dumpIfHasNoID=*/true) &&
           "source value of assignment should already have been encountered");

    if (lookupValueID(tgt) == lookupValueID(src))
      return;

    currentInstPartitionOps.emplace_back(PartitionOp::Assign(
        lookupValueID(tgt), lookupValueID(src), currentInst));
  }

  void addTransfer(SILValue value, Expr *sourceExpr = nullptr) {
    assert(valueHasID(value) &&
           "transfered value should already have been encountered");

    currentInstPartitionOps.emplace_back(
        PartitionOp::Transfer(lookupValueID(value), currentInst, sourceExpr));
  }

  void addMerge(SILValue fst, SILValue snd) {
    assert(valueHasID(fst, /*dumpIfHasNoID=*/true) &&
           valueHasID(snd, /*dumpIfHasNoID=*/true) &&
           "merged values should already have been encountered");

    if (lookupValueID(fst) == lookupValueID(snd))
      return;

    currentInstPartitionOps.emplace_back(PartitionOp::Merge(
        lookupValueID(fst), lookupValueID(snd), currentInst));
  }

  void addRequire(SILValue value) {
    assert(valueHasID(value, /*dumpIfHasNoID=*/true) &&
           "required value should already have been encountered");
    currentInstPartitionOps.emplace_back(
        PartitionOp::Require(lookupValueID(value), currentInst));
  }

  SWIFT_DEBUG_DUMP { print(llvm::dbgs()); }

  void print(llvm::raw_ostream &os) const;
};

// PartitionOpTranslator is responsible for performing the translation from
// SILInstructions to PartitionOps. Not all SILInstructions have an effect on
// the region partition, and some have multiple effects - such as an application
// pairwise merging its arguments - so the core functions like
// translateSILBasicBlock map SILInstructions to std::vectors of PartitionOps.
// No more than a single instance of PartitionOpTranslator should be used for
// each SILFunction, as SILValues are assigned unique IDs through the nodeIDMap.
// Some special correspondences between SIL values are also tracked statefully
// by instances of this class, such as the "projection" relationship:
// instructions like begin_borrow and begin_access create effectively temporary
// values used for alternative access to base "projected" values. These are
// tracked to implement "write-through" semantics for assignments to projections
// when they're addresses.
//
// TODO: when translating basic blocks, optimizations might be possible
//       that reduce lists of PartitionOps to smaller, equivalent lists
class PartitionOpTranslator {
  friend PartitionOpBuilder;

  SILFunction *function;
  ProtocolDecl *sendableProtocol;

  /// A map from the representative of an equivalence class of values to their
  /// TrackableValueState. The state contains both the unique value id for the
  /// equivalence class of values as well as whether we determined if they are
  /// uniquely identified and sendable.
  ///
  /// nodeIDMap stores unique IDs for all SILNodes corresponding to
  /// non-Sendable values. Implicit conversion from SILValue used pervasively.
  /// ensure getUnderlyingTrackedValue is called on SILValues before entering
  /// into this map
  llvm::DenseMap<SILValue, TrackableValueState> equivalenceClassValuesToState;
  llvm::DenseMap<unsigned, SILValue> stateIndexToEquivalenceClass;

  /// A list of values that can never be transferred.
  ///
  /// This includes all function arguments as well as ref_element_addr from
  /// actors.
  std::vector<TrackableValueID> neverTransferredValueIDs;

  /// A cache of argument IDs.
  std::optional<Partition> functionArgPartition;

  /// A builder struct that we use to convert individual instructions into lists
  /// of PartitionOps.
  PartitionOpBuilder builder;

  std::optional<TrackableValue> tryToTrackValue(SILValue value) const {
    auto state = getTrackableValue(value);
    if (state.isNonSendable())
      return state;
    return {};
  }

  /// If \p isAddressCapturedByPartialApply is set to true, then this value is
  /// an address that is captured by a partial_apply and we want to treat it as
  /// may alias.
  TrackableValue
  getTrackableValue(SILValue value,
                    bool isAddressCapturedByPartialApply = false) const {
    value = getUnderlyingTrackedValue(value);

    auto *self = const_cast<PartitionOpTranslator *>(this);
    auto iter = self->equivalenceClassValuesToState.try_emplace(
        value, TrackableValueState(equivalenceClassValuesToState.size()));

    // If we did not insert, just return the already stored value.
    if (!iter.second) {
      return {iter.first->first, iter.first->second};
    }

    REGIONBASEDISOLATION_VERBOSE_LOG(llvm::dbgs() << "Initializing %%"
                                                  << iter.first->second.getID()
                                                  << ": " << *value);
    self->stateIndexToEquivalenceClass[iter.first->second.getID()] = value;

    // Otherwise, we need to compute our flags. Begin by seeing if we have a
    // value that we can prove is not aliased.
    if (value->getType().isAddress()) {
      if (auto accessStorage = AccessStorage::compute(value))
        if (accessStorage.isUniquelyIdentified() &&
            !isAddressCapturedByPartialApply)
          iter.first->getSecond().removeFlag(TrackableValueFlag::isMayAlias);
    }

    // Then see if we have a sendable value. By default we assume values are not
    // sendable.
    if (auto *defInst = value.getDefiningInstruction()) {
      // Though these values are technically non-Sendable, we can safely and
      // consistently treat them as Sendable.
      if (isa<ClassMethodInst, FunctionRefInst>(defInst)) {
        iter.first->getSecond().addFlag(TrackableValueFlag::isSendable);
        return {iter.first->first, iter.first->second};
      }
    }

    // Otherwise refer to the oracle.
    if (!isNonSendableType(value->getType()))
      iter.first->getSecond().addFlag(TrackableValueFlag::isSendable);

    // Check if our base is a ref_element_addr from an actor. In such a case,
    // mark this value as actor derived.
    if (isa<LoadInst, LoadBorrowInst>(iter.first->first)) {
      auto *svi = cast<SingleValueInstruction>(iter.first->first);
      auto storage = AccessStorageWithBase::compute(svi->getOperand(0));
      if (storage.storage && isa<RefElementAddrInst>(storage.base)) {
        if (storage.storage.getRoot()->getType().isActor()) {
          iter.first->getSecond().addFlag(TrackableValueFlag::isActorDerived);
        }
      }
    }

    // If our access storage is from a class, then see if we have an actor. In
    // such a case, we need to add this id to the neverTransferred set.

    return {iter.first->first, iter.first->second};
  }

  bool markValueAsActorDerived(SILValue value) {
    value = getUnderlyingTrackedValue(value);
    auto iter = equivalenceClassValuesToState.find(value);
    if (iter == equivalenceClassValuesToState.end())
      return false;
    iter->getSecond().addFlag(TrackableValueFlag::isActorDerived);
    return true;
  }

  void initCapturedUniquelyIdentifiedValues() {
    LLVM_DEBUG(llvm::dbgs()
               << ">>> Begin Captured Uniquely Identified addresses for "
               << function->getName() << ":\n");

    for (auto &block : *function) {
      for (auto &inst : block) {
        if (auto *pai = dyn_cast<PartialApplyInst>(&inst)) {
          // If we find an address or a box of a non-Sendable type that is
          // passed to a partial_apply, mark the value's representative as being
          // uniquely identified and captured.
          for (SILValue val : inst.getOperandValues()) {
            if (val->getType().isAddress() &&
                isNonSendableType(val->getType())) {
              auto trackVal = getTrackableValue(val, true);
              (void)trackVal;
              LLVM_DEBUG(trackVal.print(llvm::dbgs()));
              continue;
            }

            if (auto *pbi = dyn_cast<ProjectBoxInst>(val)) {
              if (isNonSendableType(
                      pbi->getType().getSILBoxFieldType(function))) {
                auto trackVal = getTrackableValue(val, true);
                (void)trackVal;
                continue;
              }
            }
          }
        }
      }
    }
  }

public:
  // create a new PartitionOpTranslator, all that's needed is the underlying
  // SIL function
  PartitionOpTranslator(SILFunction *function)
      : function(function),
        sendableProtocol(
            function->getASTContext().getProtocol(KnownProtocolKind::Sendable)),
        builder() {
    assert(sendableProtocol && "PartitionOpTranslators should only be created "
                               "in contexts in which the availability of the "
                               "Sendable protocol has already been checked.");
    builder.translater = this;
    initCapturedUniquelyIdentifiedValues();
  }

  std::optional<TrackableValue> getValueForId(TrackableValueID id) const {
    auto iter = stateIndexToEquivalenceClass.find(id);
    if (iter == stateIndexToEquivalenceClass.end())
      return {};
    auto iter2 = equivalenceClassValuesToState.find(iter->second);
    if (iter2 == equivalenceClassValuesToState.end())
      return {};
    return {{iter2->first, iter2->second}};
  }

private:
  bool valueHasID(SILValue value, bool dumpIfHasNoID = false) {
    assert(getTrackableValue(value).isNonSendable() &&
           "Can only accept non-Sendable values");
    bool hasID = equivalenceClassValuesToState.count(value);
    if (!hasID && dumpIfHasNoID) {
      llvm::errs() << "FAILURE: valueHasID of ";
      value->print(llvm::errs());
      llvm::report_fatal_error("standard compiler error");
    }
    return hasID;
  }

  /// Create or lookup the internally assigned unique ID of a SILValue.
  TrackableValueID lookupValueID(SILValue value) {
    auto state = getTrackableValue(value);
    assert(state.isNonSendable() &&
           "only non-Sendable values should be entered in the map");
    return state.getID();
  }

  // check the passed type for sendability, special casing the type used for
  // raw pointers to ensure it is treated as non-Sendable and strict checking
  // is applied to it
  bool isNonSendableType(SILType type) const {
    switch (type.getASTType()->getKind()) {
    case TypeKind::BuiltinNativeObject:
    case TypeKind::BuiltinRawPointer:
      // These are very unsafe... definitely not Sendable.
      return true;
    default:
      // Consider caching this if it's a performance bottleneck.
      return type.conformsToProtocol(function, sendableProtocol)
          .hasMissingConformance(function->getParentModule());
    }
  }

  // ===========================================================================

public:
  // Create a partition that places all arguments from this function,
  // including self if available, into the same region, ensuring those
  // arguments get IDs in doing so. This Partition will be used as the
  // entry point for the full partition analysis.
  Partition getEntryPartition() const {
    if (!functionArgPartition) {
      LLVM_DEBUG(llvm::dbgs() << "Initializing Function Args:\n");
      auto *self = const_cast<PartitionOpTranslator *>(this);
      auto functionArguments = function->getArguments();
      if (functionArguments.empty()) {
        LLVM_DEBUG(llvm::dbgs() << "    None.\n");
        self->functionArgPartition = Partition::singleRegion({});
      } else {
        llvm::SmallVector<Element, 8> nonSendableIndices;
        for (SILArgument *arg : functionArguments) {
          if (auto state = tryToTrackValue(arg)) {
            // If we have an arg that is an actor, we allow for it to be
            // consumed... value ids derived from it though cannot be consumed.
            LLVM_DEBUG(llvm::dbgs() << "    %%" << state->getID());
            self->neverTransferredValueIDs.push_back(state->getID());
            nonSendableIndices.push_back(state->getID());
            LLVM_DEBUG(llvm::dbgs() << *arg);
          }
        }

        // All non actor values are in the same partition.
        self->functionArgPartition =
            Partition::singleRegion(nonSendableIndices);
      }
    }

    return *functionArgPartition;
  }

  // Get the vector of IDs that cannot be legally transferred at any point in
  // this function. Since we place all args and self in a single region right
  // now, it is only necessary to choose a single representative of the set.
  ArrayRef<TrackableValueID> getNeverTransferredValues() const {
    return llvm::makeArrayRef(neverTransferredValueIDs);
  }

  void sortUniqueNeverTransferredValues() {
    // TODO: Make a FrozenSetVector.
    sortUnique(neverTransferredValueIDs);
  }

  // get the results of an apply instruction. This is the single result value
  // for most apply instructions, but for try apply it is the two arguments
  // to each succ block
  void getApplyResults(const SILInstruction *inst,
                       SmallVectorImpl<SILValue> &foundResults) {
    if (isa<ApplyInst, BeginApplyInst, BuiltinInst, PartialApplyInst>(inst)) {
      copy(inst->getResults(), std::back_inserter(foundResults));
      return;
    }

    if (auto tryApplyInst = dyn_cast<TryApplyInst>(inst)) {
      foundResults.emplace_back(tryApplyInst->getNormalBB()->getArgument(0));
      foundResults.emplace_back(tryApplyInst->getErrorBB()->getArgument(0));
      return;
    }

    llvm::report_fatal_error("all apply instructions should be covered");
  }

  // ===========================================================================
  // The following section of functions wrap the more primitive Assign, Require,
  // Merge, etc functions that generate PartitionOps with more logic common to
  // the translations from source-level SILInstructions.

  // require all non-sendable sources, merge their regions, and assign the
  // resulting region to all non-sendable targets, or assign non-sendable
  // targets to a fresh region if there are no non-sendable sources
  template <typename TargetRange, typename SourceRange>
  void translateSILMultiAssign(const TargetRange &resultValues,
                               const SourceRange &sourceValues,
                               bool propagagesActorSelf = false) {
    REGIONBASEDISOLATION_VERBOSE_LOG(llvm::dbgs()
                                     << "Performing Multi Assign!\n");
    SmallVector<SILValue, 8> assignOperands;
    SmallVector<SILValue, 8> assignResults;

    for (SILValue src : sourceValues) {
      if (auto value = tryToTrackValue(src)) {
        REGIONBASEDISOLATION_VERBOSE_LOG(llvm::dbgs() << "Operand: " << *src);
        assignOperands.push_back(value->getRepresentative());
      }
    }

    for (SILValue result : resultValues) {
      if (auto value = tryToTrackValue(result)) {
        REGIONBASEDISOLATION_VERBOSE_LOG(llvm::dbgs() << "Result: " << *result);
        assignResults.push_back(value->getRepresentative());
        // TODO: Can we pass back a reference to value perhaps?
        if (propagagesActorSelf) {
          markValueAsActorDerived(result);
        }
      }
    }

    // require all srcs
    for (auto src : assignOperands)
      builder.addRequire(src);

    // merge all srcs
    for (unsigned i = 1; i < assignOperands.size(); i++) {
      builder.addMerge(assignOperands[i - 1], assignOperands[i]);
    }

    // If we do not have any non sendable results
    if (assignResults.empty())
      return;

    auto assignResultsRef = llvm::makeArrayRef(assignResults);
    SILValue front = assignResultsRef.front();
    assignResultsRef = assignResultsRef.drop_front();

    if (assignOperands.empty()) {
      // If no non-sendable srcs, non-sendable tgts get a fresh region.
      builder.addAssignFresh(front);
    } else {
      builder.addAssign(front, assignOperands.front());
    }

    // Assign all targets to the target region.
    while (assignResultsRef.size()) {
      SILValue next = assignResultsRef.front();
      assignResultsRef = assignResultsRef.drop_front();

      builder.addAssign(next, front);
    }
  }

  void translateSILApply(SILInstruction *applyInst) {
    // if this apply does not cross isolation domains, it has normal,
    // non-transferring multi-assignment semantics
    if (!SILApplyCrossesIsolation(applyInst)) {
      // TODO: How do we handle partial_apply here.
      bool hasActorSelf = false;
      if (auto fas = FullApplySite::isa(applyInst)) {
        if (fas.hasSelfArgument()) {
          if (auto self = fas.getSelfArgument()) {
            hasActorSelf = self->getType().isActor();
          }
        }
      } else if (auto *pai = dyn_cast<PartialApplyInst>(applyInst)) {
        for (auto arg : pai->getOperandValues()) {
          if (auto value = tryToTrackValue(arg)) {
            if (value->isActorDerived()) {
              hasActorSelf = true;
            }
          } else {
            // NOTE: One may think that only sendable things can enter
            // here... but we treat things like function_ref/class_method which
            // are non-Sendable as sendable for our purposes.
            if (arg->getType().isActor()) {
              hasActorSelf = true;
            }
          }
        }
      }

      SmallVector<SILValue, 8> applyResults;
      getApplyResults(applyInst, applyResults);
      return translateSILMultiAssign(
          applyResults, applyInst->getOperandValues(), hasActorSelf);
    }

    if (auto cast = dyn_cast<ApplyInst>(applyInst))
      return translateIsolationCrossingSILApply(cast);
    if (auto cast = dyn_cast<BeginApplyInst>(applyInst))
      return translateIsolationCrossingSILApply(cast);
    if (auto cast = dyn_cast<TryApplyInst>(applyInst))
      return translateIsolationCrossingSILApply(cast);

    llvm_unreachable("Only ApplyInst, BeginApplyInst, and TryApplyInst should cross isolation domains");
  }

  // handles the semantics for SIL applies that cross isolation
  // in particular, all arguments are transferred.
  void translateIsolationCrossingSILApply(ApplySite applySite) {
    ApplyExpr *sourceApply = applySite.getLoc().getAsASTNode<ApplyExpr>();
    assert(sourceApply && "only ApplyExpr's should cross isolation domains");

    // require all operands
    for (auto op : applySite->getOperandValues())
      if (auto value = tryToTrackValue(op))
        builder.addRequire(value->getRepresentative());

    auto getSourceArg = [&](unsigned i) {
      if (i < sourceApply->getArgs()->size())
        return sourceApply->getArgs()->getExpr(i);
      return (Expr *)nullptr;
    };

    auto getSourceSelf = [&]() {
      if (auto callExpr = dyn_cast<CallExpr>(sourceApply))
        if (auto calledExpr = dyn_cast<DotSyntaxCallExpr>(callExpr->getDirectCallee()))
          return calledExpr->getBase();
      return (Expr *)nullptr;
    };

    auto handleSILOperands = [&](OperandValueArrayRef ops) {
      int argNum = 0;
      for (auto arg : ops) {
        if (auto value = tryToTrackValue(arg))
          builder.addTransfer(value->getRepresentative(), getSourceArg(argNum));
        argNum++;
      }
    };

    auto handleSILSelf = [&](SILValue self) {
      if (auto value = tryToTrackValue(self))
        builder.addTransfer(value->getRepresentative(), getSourceSelf());
    };

    if (applySite.hasSelfArgument()) {
      handleSILOperands(applySite.getArgumentsWithoutSelf());
      handleSILSelf(applySite.getSelfArgument());
    } else {
      handleSILOperands(applySite.getArguments());
    }

    // non-sendable results can't be returned from cross-isolation calls without
    // a diagnostic emitted elsewhere. Here, give them a fresh value for better
    // diagnostics hereafter
    SmallVector<SILValue, 8> applyResults;
    getApplyResults(*applySite, applyResults);
    for (auto result : applyResults)
      if (auto value = tryToTrackValue(result))
        builder.addAssignFresh(value->getRepresentative());
  }

  void translateSILAssign(SILValue tgt, SILValue src) {
    return translateSILMultiAssign(TinyPtrVector<SILValue>(tgt),
                                   TinyPtrVector<SILValue>(src));
  }

  // If the passed SILValue is NonSendable, then create a fresh region for it,
  // otherwise do nothing.
  void translateSILAssignFresh(SILValue val) {
    return translateSILMultiAssign(TinyPtrVector<SILValue>(val),
                                   TinyPtrVector<SILValue>());
  }

  void translateSILMerge(SILValue fst, SILValue snd) {
    auto nonSendableFst = tryToTrackValue(fst);
    auto nonSendableSnd = tryToTrackValue(snd);
    if (!nonSendableFst || !nonSendableSnd)
      return;
    builder.addMerge(nonSendableFst->getRepresentative(),
                     nonSendableSnd->getRepresentative());
  }

  // if the tgt is known to be unaliased (computed thropugh a combination
  // of AccessStorage's inUniquelyIdenfitied check and a custom search for
  // captures by applications), then these can be treated as assignments
  // of tgt to src. If the tgt could be aliased, then we must instead treat
  // them as merges, to ensure any aliases of tgt are also updated.
  void translateSILStore(SILValue tgt, SILValue src) {
    if (auto nonSendableTgt = tryToTrackValue(tgt)) {
      // stores to unaliased storage can be treated as assignments, not merges
      if (nonSendableTgt.value().isNoAlias())
        return translateSILAssign(tgt, src);

      // stores to possibly aliased storage must be treated as merges
      return translateSILMerge(tgt, src);
    }

    // stores to storage of non-Sendable type can be ignored
  }

  void translateSILRequire(SILValue val) {
    if (auto nonSendableVal = tryToTrackValue(val))
      return builder.addRequire(nonSendableVal->getRepresentative());
  }

  // an enum select is just a multi assign
  void translateSILSelectEnum(SelectEnumOperation selectEnumInst) {
    SmallVector<SILValue, 8> enumOperands;
    for (unsigned i = 0; i < selectEnumInst.getNumCases(); i ++)
      enumOperands.push_back(selectEnumInst.getCase(i).second);
    if (selectEnumInst.hasDefault())
      enumOperands.push_back(selectEnumInst.getDefaultResult());
    return translateSILMultiAssign(
        TinyPtrVector<SILValue>(selectEnumInst->getResult(0)), enumOperands);
  }

  void translateSILSwitchEnum(SwitchEnumInst *switchEnumInst) {
    TermArgSources argSources;

    // accumulate each switch case that branches to a basic block with an arg
    for (unsigned i = 0; i < switchEnumInst->getNumCases(); i++) {
      SILBasicBlock *dest = switchEnumInst->getCase(i).second;
      if (dest->getNumArguments() > 0) {
        assert(dest->getNumArguments() == 1
               && "expected at most one bb arg in dest of enum switch");
        argSources.addValues({switchEnumInst->getOperand()}, dest);
      }
    }

    translateSILPhi(argSources);
  }

  // translate a SIL instruction corresponding to possible branches with args
  // to one or more basic blocks. This is the SIL equivalent of SSA Phi nodes.
  // each element of `branches` corresponds to the arguments passed to a bb,
  // and a pointer to the bb being branches to itself.
  // this is handled as assigning to each possible arg being branched to the
  // merge of all values that could be passed to it from this basic block.
  void translateSILPhi(TermArgSources &argSources) {
    argSources.argSources.setFrozen();
    for (auto pair : argSources.argSources.getRange()) {
      translateSILMultiAssign(TinyPtrVector<SILValue>(pair.first), pair.second);
    }
  }

  // ===========================================================================
  // Some SILInstructions contribute to the partition of non-Sendable values
  // being analyzed. translateSILInstruction translate a SILInstruction
  // to its effect on the non-Sendable partition, if it has one.
  //
  // The current pattern of
  void translateSILInstruction(SILInstruction *inst) {
    builder.reset(inst);
    SWIFT_DEFER { LLVM_DEBUG(builder.print(llvm::dbgs())); };

    switch (inst->getKind()) {
    // The following instructions are treated as assigning their result to a
    // fresh region.
    case SILInstructionKind::AllocBoxInst:
    case SILInstructionKind::AllocPackInst:
    case SILInstructionKind::AllocRefDynamicInst:
    case SILInstructionKind::AllocRefInst:
    case SILInstructionKind::AllocStackInst:
    case SILInstructionKind::KeyPathInst:
    case SILInstructionKind::FunctionRefInst:
    case SILInstructionKind::DynamicFunctionRefInst:
    case SILInstructionKind::PreviousDynamicFunctionRefInst:
    case SILInstructionKind::GlobalAddrInst:
    case SILInstructionKind::BaseAddrForOffsetInst:
    case SILInstructionKind::GlobalValueInst:
    case SILInstructionKind::IntegerLiteralInst:
    case SILInstructionKind::FloatLiteralInst:
    case SILInstructionKind::StringLiteralInst:
    case SILInstructionKind::HasSymbolInst:
    case SILInstructionKind::ObjCProtocolInst:
    case SILInstructionKind::WitnessMethodInst:
      return translateSILAssignFresh(inst->getResult(0));

    case SILInstructionKind::SelectEnumAddrInst:
    case SILInstructionKind::SelectEnumInst:
      return translateSILSelectEnum(inst);

    // The following instructions are treated as assignments of their result
    // (tgt) to their operand (src). This could yield a variety of behaviors
    // depending on the sendability of both src and tgt.
    // TODO: we could reduce the size of PartitionOp sequences generated by
    //       treating some of these as lookthroughs in getUnderlyingTrackedValue
    //       instead of as assignments
    case SILInstructionKind::AddressToPointerInst:
    case SILInstructionKind::CopyValueInst:
    case SILInstructionKind::ConvertEscapeToNoEscapeInst:
    case SILInstructionKind::ConvertFunctionInst:
    case SILInstructionKind::CopyBlockInst:
    case SILInstructionKind::CopyBlockWithoutEscapingInst:
    case SILInstructionKind::IndexAddrInst:
    case SILInstructionKind::InitBlockStorageHeaderInst:
    case SILInstructionKind::InitEnumDataAddrInst:
    case SILInstructionKind::InitExistentialAddrInst:
    case SILInstructionKind::InitExistentialRefInst:
    case SILInstructionKind::LoadInst:
    case SILInstructionKind::LoadBorrowInst:
    case SILInstructionKind::LoadWeakInst:
    case SILInstructionKind::OpenExistentialAddrInst:
    case SILInstructionKind::OpenExistentialBoxInst:
    case SILInstructionKind::OpenExistentialRefInst:
    case SILInstructionKind::PointerToAddressInst:
    case SILInstructionKind::ProjectBlockStorageInst:
    case SILInstructionKind::RefElementAddrInst:
    case SILInstructionKind::ProjectBoxInst:
    case SILInstructionKind::RefToUnmanagedInst:
    case SILInstructionKind::StrongCopyUnownedValueInst:
    case SILInstructionKind::StructElementAddrInst:
    case SILInstructionKind::StructExtractInst:
    case SILInstructionKind::TailAddrInst:
    case SILInstructionKind::ThickToObjCMetatypeInst:
    case SILInstructionKind::ThinToThickFunctionInst:
    case SILInstructionKind::TupleElementAddrInst:
    case SILInstructionKind::UncheckedAddrCastInst:
    case SILInstructionKind::UncheckedEnumDataInst:
    case SILInstructionKind::UncheckedOwnershipConversionInst:
    case SILInstructionKind::UncheckedRefCastInst:
    case SILInstructionKind::UncheckedTakeEnumDataAddrInst:
    case SILInstructionKind::UnmanagedToRefInst:
    case SILInstructionKind::UpcastInst:

    // Dynamic dispatch:
    case SILInstructionKind::ClassMethodInst:
    case SILInstructionKind::ObjCMethodInst:
    case SILInstructionKind::SuperMethodInst:
    case SILInstructionKind::ObjCSuperMethodInst:
      return translateSILAssign(inst->getResult(0), inst->getOperand(0));
    case SILInstructionKind::BeginBorrowInst: {
      return translateSILAssign(inst->getResult(0), inst->getOperand(0));
    }

    case SILInstructionKind::EnumInst: {
      auto *ei = cast<EnumInst>(inst);
      if (ei->getNumOperands() == 0)
        return translateSILAssignFresh(ei);
      return translateSILAssign(ei, ei->getOperand());
    }

    // These are treated as stores - meaning that they could write values into
    // memory. The beahvior of this depends on whether the tgt addr is aliased,
    // but conservative behavior is to treat these as merges of the regions of
    // the src value and tgt addr
    case SILInstructionKind::CopyAddrInst:
    case SILInstructionKind::ExplicitCopyAddrInst:
    case SILInstructionKind::StoreInst:
    case SILInstructionKind::StoreBorrowInst:
    case SILInstructionKind::StoreWeakInst:
      return translateSILStore(inst->getOperand(1), inst->getOperand(0));

    case SILInstructionKind::ApplyInst:
    case SILInstructionKind::BeginApplyInst:
    case SILInstructionKind::BuiltinInst:
    case SILInstructionKind::PartialApplyInst:
    case SILInstructionKind::TryApplyInst:
      return translateSILApply(inst);

    case SILInstructionKind::DestructureTupleInst: {
      // handle tuple destruction
      auto *destructTupleInst = cast<DestructureTupleInst>(inst);
      return translateSILMultiAssign(
          destructTupleInst->getResults(),
          TinyPtrVector<SILValue>(destructTupleInst->getOperand()));
    }

    // handle instructions that aggregate their operands into a single structure
    // - treated as a multi assign
    case SILInstructionKind::ObjectInst:
    case SILInstructionKind::StructInst:
    case SILInstructionKind::TupleInst:
      return translateSILMultiAssign(
          TinyPtrVector<SILValue>(inst->getResult(0)),
          inst->getOperandValues());

    // Handle returns and throws - require the operand to be non-transferred
    case SILInstructionKind::ReturnInst:
    case SILInstructionKind::ThrowInst:
      return translateSILRequire(inst->getOperand(0));

    // handle branching terminators. in particular, need to handle phi-node-like
    // argument passing
    case SILInstructionKind::BranchInst: {
      auto *branchInst = cast<BranchInst>(inst);
      assert(branchInst->getNumArgs() == branchInst->getDestBB()->getNumArguments());
      TermArgSources argSources;
      argSources.addValues(branchInst->getArgs(), branchInst->getDestBB());
      return translateSILPhi(argSources);
    }

    case SILInstructionKind::CondBranchInst: {
      auto *condBranchInst = cast<CondBranchInst>(inst);
      assert(condBranchInst->getNumTrueArgs() ==
             condBranchInst->getTrueBB()->getNumArguments());
      assert(condBranchInst->getNumFalseArgs() ==
             condBranchInst->getFalseBB()->getNumArguments());
      TermArgSources argSources;
      argSources.addValues(condBranchInst->getTrueArgs(),
                           condBranchInst->getTrueBB());
      argSources.addValues(condBranchInst->getFalseArgs(),
                           condBranchInst->getFalseBB());
      return translateSILPhi(argSources);
    }

    case SILInstructionKind::SwitchEnumInst:
      return translateSILSwitchEnum(cast<SwitchEnumInst>(inst));

    case SILInstructionKind::DynamicMethodBranchInst: {
      auto *dmBranchInst = cast<DynamicMethodBranchInst>(inst);
      assert(dmBranchInst->getHasMethodBB()->getNumArguments() <= 1);
      TermArgSources argSources;
      argSources.addValues({dmBranchInst->getOperand()},
                           dmBranchInst->getHasMethodBB());
      return translateSILPhi(argSources);
    }

    case SILInstructionKind::CheckedCastBranchInst: {
      auto *ccBranchInst = cast<CheckedCastBranchInst>(inst);
      assert(ccBranchInst->getSuccessBB()->getNumArguments() <= 1);
      TermArgSources argSources;
      argSources.addValues({ccBranchInst->getOperand()},
                           ccBranchInst->getSuccessBB());
      return translateSILPhi(argSources);
    }

    case SILInstructionKind::CheckedCastAddrBranchInst: {
      auto *ccAddrBranchInst = cast<CheckedCastAddrBranchInst>(inst);
      assert(ccAddrBranchInst->getSuccessBB()->getNumArguments() <= 1);

      // checked_cast_addr_br does not have any arguments in its resulting
      // block. We should just use a multi-assign on its operands.
      //
      // TODO: We should be smarter and treat the success/fail branches
      // differently depending on what the result of checked_cast_addr_br
      // is. For now just keep the current behavior. It is more conservative,
      // but still correct.
      return translateSILMultiAssign(ArrayRef<SILValue>(),
                                     ccAddrBranchInst->getOperandValues());
    }

    // these instructions are ignored because they cannot affect the partition
    // state - they do not manipulate what region non-sendable values lie in
    case SILInstructionKind::AllocGlobalInst:
    case SILInstructionKind::DeallocBoxInst:
    case SILInstructionKind::DeallocStackInst:
    case SILInstructionKind::DebugValueInst:
    case SILInstructionKind::DestroyAddrInst:
    case SILInstructionKind::DestroyValueInst:
    case SILInstructionKind::EndAccessInst:
    case SILInstructionKind::EndBorrowInst:
    case SILInstructionKind::EndLifetimeInst:
    case SILInstructionKind::HopToExecutorInst:
    case SILInstructionKind::InjectEnumAddrInst:
    case SILInstructionKind::IsEscapingClosureInst: // ignored because result is
                                                    // always in
    case SILInstructionKind::MarkDependenceInst:
    case SILInstructionKind::MetatypeInst:
    case SILInstructionKind::EndApplyInst:
    case SILInstructionKind::AbortApplyInst:

    // ignored terminators
    case SILInstructionKind::CondFailInst:
    case SILInstructionKind::SwitchEnumAddrInst: // ignored as long as
                                                 // destinations can take no arg
    case SILInstructionKind::SwitchValueInst: // ignored as long as destinations
                                              // can take no args
    case SILInstructionKind::UnreachableInst:
    case SILInstructionKind::UnwindInst:
    case SILInstructionKind::YieldInst: // TODO: yield should be handled
      return;

    // We ignore begin_access since we look through them. We look through them
    // since we want to treat the use of the begin_access as the semantic giving
    // instruction. Otherwise, if we have a store after a consume we will emit
    // an error on the begin_access rather than allowing for the store to
    // overwrite the original value. This would then cause an error.
    //
    // Example:
    //
    //   %0 = alloc_stack
    //   store %1 to %0
    //   apply %transfer(%0)
    //   %2 = begin_access [modify] [static] %0
    //   store %2 to %0
    //
    // If we treated a begin_access as an assign, we would require %0 to not be
    // transferred at %2 even though we are about to overwrite it.
    case SILInstructionKind::BeginAccessInst:
      return;

    default:
      break;
    }

    LLVM_DEBUG(llvm::dbgs() << "warning: ";
               llvm::dbgs() << "unhandled instruction kind "
                            << getSILInstructionName(inst->getKind()) << "\n";);

    return;
  }

  // translateSILBasicBlock reduces a SIL basic block to the vector of
  // transformations to the non-Sendable partition that it induces.
  // it accomplished this by sequentially calling translateSILInstruction
  std::vector<PartitionOp> translateSILBasicBlock(SILBasicBlock *basicBlock) {
    LLVM_DEBUG(llvm::dbgs() << SEP_STR << "Compiling basic block for function "
                            << basicBlock->getFunction()->getName() << ": ";
               basicBlock->dumpID(); llvm::dbgs() << SEP_STR;
               basicBlock->print(llvm::dbgs());
               llvm::dbgs() << SEP_STR << "Results:\n";);
    //translate each SIL instruction to a PartitionOp, if necessary
    std::vector<PartitionOp> partitionOps;
    for (auto &instruction : *basicBlock) {
      LLVM_DEBUG(llvm::dbgs() << "Visiting: " << instruction);
      translateSILInstruction(&instruction);
      copy(builder.currentInstPartitionOps, std::back_inserter(partitionOps));
    }

    return partitionOps;
  }
};

TrackableValueID PartitionOpBuilder::lookupValueID(SILValue value) {
  return translater->lookupValueID(value);
}

bool PartitionOpBuilder::valueHasID(SILValue value, bool dumpIfHasNoID) {
  return translater->valueHasID(value, dumpIfHasNoID);
}

void PartitionOpBuilder::print(llvm::raw_ostream &os) const {
#ifndef NDEBUG
  // If we do not have anything to dump, just return.
  if (currentInstPartitionOps.empty())
    return;

  // First line.
  llvm::dbgs() << " ┌─┬─╼";
  currentInst->print(llvm::dbgs());

  // Second line.
  llvm::dbgs() << " │ └─╼  ";
  currentInst->getLoc().getSourceLoc().printLineAndColumn(
      llvm::dbgs(), currentInst->getFunction()->getASTContext().SourceMgr);

  auto ops = llvm::makeArrayRef(currentInstPartitionOps);

  // First op on its own line.
  llvm::dbgs() << "\n ├─────╼ ";
  ops.front().print(llvm::dbgs());

  // Rest of ops each on their own line.
  for (const PartitionOp &op : ops.drop_front()) {
    llvm::dbgs() << " │    └╼ ";
    op.print(llvm::dbgs());
  }

  // Now print out a translation from region to equivalence class value.
  llvm::dbgs() << " └─────╼ Used Values\n";
  llvm::SmallVector<unsigned, 8> opsToPrint;
  SWIFT_DEFER { opsToPrint.clear(); };
  for (const PartitionOp &op : ops) {
    // Now dump our the root value we map.
    for (unsigned opArg : op.getOpArgs()) {
      // If we didn't insert, skip this. We only emit this once.
      opsToPrint.push_back(opArg);
    }
  }
  sortUnique(opsToPrint);
  for (unsigned opArg : opsToPrint) {
    llvm::dbgs() << "          └╼ ";
    SILValue value = translater->stateIndexToEquivalenceClass[opArg];
    auto iter = translater->equivalenceClassValuesToState.find(value);
    assert(iter != translater->equivalenceClassValuesToState.end());
    llvm::dbgs() << "State: %%" << opArg << ". ";
    iter->getSecond().print(llvm::dbgs());
    llvm::dbgs() << "\n             Value: " << value;
  }
#endif
}

// Instances of BlockPartitionState record all relevant state about a
// SILBasicBlock for the region-based Sendable checking fixpoint analysis.
// In particular, it records flags such as whether the block has been
// reached by the analysis, whether the prior round indicated that this block
// needs to be updated; it records aux data such as the underlying basic block
// and associated PartitionOpTranslator; and most importantly of all it includes
// region partitions at entry and exit to this block - these are the stateful
// component of the fixpoint analysis.
class BlockPartitionState {
  friend class PartitionAnalysis;

  bool needsUpdate = false;
  bool reached = false;

  Partition entryPartition;
  Partition exitPartition;

  SILBasicBlock *basicBlock;
  PartitionOpTranslator &translator;

  bool blockPartitionOpsPopulated = false;
  std::vector<PartitionOp> blockPartitionOps = {};

  BlockPartitionState(SILBasicBlock *basicBlock,
                      PartitionOpTranslator &translator)
      : basicBlock(basicBlock), translator(translator) {}

  void ensureBlockPartitionOpsPopulated() {
    if (blockPartitionOpsPopulated) return;
    blockPartitionOpsPopulated = true;
    blockPartitionOps = translator.translateSILBasicBlock(basicBlock);
  }

  // recomputes the exit partition from the entry partition,
  // and returns whether this changed the exit partition.
  // Note that this method ignored errors that arise.
  bool recomputeExitFromEntry() {
    ensureBlockPartitionOpsPopulated();

    Partition workingPartition = entryPartition;
    for (auto partitionOp : blockPartitionOps) {
      // by calling apply without providing a `handleFailure` closure,
      // errors will be suppressed
      workingPartition.apply(partitionOp);
    }
    bool exitUpdated = !Partition::equals(exitPartition, workingPartition);
    exitPartition = workingPartition;
    return exitUpdated;
  }

  // apply each PartitionOP in this block to the entry partition,
  // but this time pass in a handleFailure closure that can be used
  // to diagnose any failures
  void diagnoseFailures(
      llvm::function_ref<void(const PartitionOp &, TrackableValueID)>
          handleFailure,
      llvm::function_ref<void(const PartitionOp &, TrackableValueID)>
          handleTransferNonTransferable) {
    Partition workingPartition = entryPartition;
    for (auto &partitionOp : blockPartitionOps) {
      workingPartition.apply(
          partitionOp, handleFailure, translator.getNeverTransferredValues(),
          handleTransferNonTransferable, [&](Element element) -> bool {
            auto iter = translator.getValueForId(element);
            if (!iter)
              return false;
            return iter->isActorDerived();
          });
    }
  }

public:
  // run the passed action on each partitionOp in this block. Action should
  // return true iff iteration should continue
  void forEachPartitionOp(llvm::function_ref<bool (const PartitionOp&)> action) const {
    for (const PartitionOp &partitionOp : blockPartitionOps)
      if (!action(partitionOp)) break;
  }

  const Partition& getEntryPartition() const {
    return entryPartition;
  }

  const Partition& getExitPartition() const {
    return exitPartition;
  }

  SWIFT_DEBUG_DUMP { print(llvm::dbgs()); }

  void print(llvm::raw_ostream &os) const {
    LLVM_DEBUG(
        os << SEP_STR << "BlockPartitionState[reached=" << reached
           << ", needsUpdate=" << needsUpdate << "]\nid: ";
        basicBlock->printID(os); os << "entry partition: ";
        entryPartition.print(os); os << "exit partition: ";
        exitPartition.print(os);
        os << "instructions:\n┌──────────╼\n"; for (PartitionOp op
                                                    : blockPartitionOps) {
          os << "│ ";
          op.print(os);
        } os << "└──────────╼\nSuccs:\n";
        for (auto succ
             : basicBlock->getSuccessorBlocks()) {
          os << "→";
          succ->printID(os);
        } os
        << "Preds:\n";
        for (auto pred
             : basicBlock->getPredecessorBlocks()) {
          os << "←";
          pred->printID(os);
        } os
        << SEP_STR;);
  }
};

enum class LocalTransferredReasonKind {
  LocalTransferInst,
  LocalNonTransferInst,
  NonLocal
};

// Why was a value transferred, without looking across blocks?
// kind == LocalTransferInst: a transfer instruction in this block
// kind == LocalNonTransferInst: an instruction besides a transfer instruction
//                              in this block
// kind == NonLocal: an instruction outside this block
struct LocalTransferredReason {
  LocalTransferredReasonKind kind;
  std::optional<PartitionOp> localInst;

  static LocalTransferredReason TransferInst(PartitionOp localInst) {
    assert(localInst.getKind() == PartitionOpKind::Transfer);
    return LocalTransferredReason(LocalTransferredReasonKind::LocalTransferInst,
                                  localInst);
  }

  static LocalTransferredReason NonTransferInst() {
    return LocalTransferredReason(
        LocalTransferredReasonKind::LocalNonTransferInst);
  }

  static LocalTransferredReason NonLocal() {
    return LocalTransferredReason(LocalTransferredReasonKind::NonLocal);
  }

  // 0-ary constructor only used in maps, where it's immediately overridden
  LocalTransferredReason() : kind(LocalTransferredReasonKind::NonLocal) {}

private:
  LocalTransferredReason(LocalTransferredReasonKind kind,
                         std::optional<PartitionOp> localInst = {})
      : kind(kind), localInst(localInst) {}
};

// This class captures all available information about why a value's region was
// transferred. In particular, it contains a map `transferOps` whose keys are
// "distances" and whose values are Transfer PartitionOps that cause the target
// region to be transferred. Distances are (roughly) the number of times
// two different predecessor blocks had to have their exit partitions joined
// together to actually cause the target region to be transferred. If a Transfer
// op only causes a target access to be invalid because of merging/joining
// that spans many different blocks worth of control flow, it is less likely
// to be informative, so distance is used as a heuristic to choose which
// access sites to display in diagnostics given a racy consumption.
class TransferredReason {
  std::map<unsigned, std::vector<PartitionOp>> transferOps;

  friend class TransferRequireAccumulator;

  bool containsOp(const PartitionOp& op) {
    for (auto [_, vec] : transferOps)
      for (auto vecOp : vec)
        if (op == vecOp)
          return true;
    return false;
  }

public:
  // a TransferredReason is valid if it contains at least one transfer
  // instruction
  bool isValid() {
    for (auto [_, vec] : transferOps)
      if (!vec.empty())
        return true;
    return false;
  }

  TransferredReason() {}

  TransferredReason(LocalTransferredReason localReason) {
    assert(localReason.kind == LocalTransferredReasonKind::LocalTransferInst);
    transferOps[0] = {localReason.localInst.value()};
  }

  void addTransferOp(PartitionOp transferOp, unsigned distance) {
    assert(transferOp.getKind() == PartitionOpKind::Transfer);
    // duplicates should not arise
    if (!containsOp(transferOp))
      transferOps[distance].push_back(transferOp);
  }

  // merge in another transferredReason, adding the specified distane to all its
  // ops
  void addOtherReasonAtDistance(const TransferredReason &otherReason,
                                unsigned distance) {
    for (auto &[otherDistance, otherTransferOpsAtDistance] :
         otherReason.transferOps)
      for (auto otherTransferOp : otherTransferOpsAtDistance)
        addTransferOp(otherTransferOp, distance + otherDistance);
  }
};

// This class is the "inverse" of a TransferredReason: instead of associating
// accessing PartitionOps with their consumption sites, it associates
// consumption site Transfer PartitionOps with the corresponding accesses.
// It is built up by repeatedly calling accumulateTransferredReason on
// TransferredReasons, which "inverts" the contents of that reason and adds it
// to this class's tracking. Instead of a two-level map, we store a set that
// join together distances and access partitionOps so that we can use the
// ordering by lowest diagnostics for prioritized output
class TransferRequireAccumulator {
  struct PartitionOpAtDistance {
    PartitionOp partitionOp;
    unsigned distance;

    PartitionOpAtDistance(PartitionOp partitionOp, unsigned distance)
        : partitionOp(partitionOp), distance(distance) {}

    bool operator<(const PartitionOpAtDistance& other) const {
      if (distance != other.distance)
        return distance < other.distance;
      return partitionOp < other.partitionOp;
    }
  };

  // map consumptions to sets of requirements for that consumption, ordered so
  // that requirements at a smaller distance from the consumption come first
  std::map<PartitionOp, std::set<PartitionOpAtDistance>>
      requirementsForTransfers;

  SILFunction *fn;

public:
  TransferRequireAccumulator(SILFunction *fn) : fn(fn) {}

  void accumulateTransferredReason(PartitionOp requireOp,
                                   const TransferredReason &transferredReason) {
    for (auto [distance, transferOps] : transferredReason.transferOps)
      for (auto transferOp : transferOps)
        requirementsForTransfers[transferOp].insert({requireOp, distance});
  }

  void emitErrorsForTransferRequire(
      unsigned numRequiresPerTransfer = UINT_MAX) const {
    for (auto [transferOp, requireOps] : requirementsForTransfers) {
      unsigned numProcessed = std::min(
          {(unsigned)requireOps.size(), (unsigned)numRequiresPerTransfer});

      // First process our transfer ops.
      unsigned numDisplayed = numProcessed;
      unsigned numHidden = requireOps.size() - numProcessed;
      if (!tryDiagnoseAsCallSite(transferOp, numDisplayed, numHidden)) {
        assert(false && "no consumptions besides callsites implemented yet");

        // default to more generic diagnostic
        auto expr = getExprForPartitionOp(transferOp);
        auto diag = fn->getASTContext().Diags.diagnose(
            expr->getLoc(), diag::transfer_yields_race, numDisplayed,
            numDisplayed != 1, numHidden > 0, numHidden);
        if (auto sourceExpr = transferOp.getSourceExpr())
          diag.highlight(sourceExpr->getSourceRange());
        return;
      }

      unsigned numRequiresToProcess = numRequiresPerTransfer;
      for (auto [requireOp, _] : requireOps) {
        // ensures at most numRequiresPerTransfer requires are processed per
        // transfer
        if (numRequiresToProcess-- == 0)
          break;
        auto expr = getExprForPartitionOp(requireOp);
        fn->getASTContext()
            .Diags.diagnose(expr->getLoc(), diag::possible_racy_access_site)
            .highlight(expr->getSourceRange());
      }
    }
  }

  SWIFT_DEBUG_DUMP { print(llvm::dbgs()); }

  void print(llvm::raw_ostream &os) const {
    for (auto [transferOp, requireOps] : requirementsForTransfers) {
      os << " ┌──╼ TRANSFER: ";
      transferOp.print(os);

      for (auto &[requireOp, _] : requireOps) {
        os << " ├╼ REQUIRE: ";
        requireOp.print(os);
      }
    }
  }

private:
  /// Try to interpret this transferOp as a source-level callsite (ApplyExpr),
  /// and report a diagnostic including actor isolation crossing information
  /// returns true iff one was succesfully formed and emitted.
  bool tryDiagnoseAsCallSite(const PartitionOp &transferOp,
                             unsigned numDisplayed, unsigned numHidden) const {
    SILInstruction *sourceInst =
        transferOp.getSourceInst(/*assertNonNull=*/true);
    ApplyExpr *apply = sourceInst->getLoc().getAsASTNode<ApplyExpr>();
    if (!apply)
      // consumption does not correspond to an apply expression
      return false;
    auto isolationCrossing = apply->getIsolationCrossing();
    if (!isolationCrossing) {
      assert(false && "ApplyExprs should be transferring only if"
                      " they are isolation crossing");
      return false;
    }
    auto argExpr = transferOp.getSourceExpr();
    if (!argExpr)
      assert(false &&
             "sourceExpr should be populated for ApplyExpr consumptions");

    sourceInst->getFunction()
        ->getASTContext()
        .Diags
        .diagnose(argExpr->getLoc(), diag::call_site_transfer_yields_race,
                  argExpr->findOriginalType(),
                  isolationCrossing.value().getCallerIsolation(),
                  isolationCrossing.value().getCalleeIsolation(), numDisplayed,
                  numDisplayed != 1, numHidden > 0, numHidden)
        .highlight(argExpr->getSourceRange());
    return true;
  }
};

// A RaceTracer is used to accumulate the facts that the main phase of
// PartitionAnalysis generates - that certain values were required at certain
// points but were in transferred regions and thus should yield diagnostics -
// and traces those facts to the Transfer operations that could have been
// responsible.
class RaceTracer {
  const BasicBlockData<BlockPartitionState>& blockStates;

  std::map<std::pair<SILBasicBlock *, TrackableValueID>, TransferredReason>
      transferredAtEntryReasons;

  // caches the reasons why transferredVals were transferred at the exit to
  // basic blocks
  std::map<std::pair<SILBasicBlock *, TrackableValueID>, LocalTransferredReason>
      transferredAtExitReasons;

  TransferRequireAccumulator accumulator;

  TransferredReason findTransferredAtOpReason(TrackableValueID transferredVal,
                                              PartitionOp op) {
    TransferredReason transferredReason;
    findAndAddTransferredReasons(op.getSourceInst(true)->getParent(),
                                 transferredVal, transferredReason, 0, op);
    return transferredReason;
  }

  void findAndAddTransferredReasons(SILBasicBlock *SILBlock,
                                    TrackableValueID transferredVal,
                                    TransferredReason &transferredReason,
                                    unsigned distance,
                                    std::optional<PartitionOp> targetOp = {}) {
    LocalTransferredReason localReason =
        findLocalTransferredReason(SILBlock, transferredVal, targetOp);
    switch (localReason.kind) {
    case LocalTransferredReasonKind::LocalTransferInst:
      // there is a local transfer in the pred block
      transferredReason.addTransferOp(localReason.localInst.value(), distance);
      break;
    case LocalTransferredReasonKind::LocalNonTransferInst:
      // ignore this case, that instruction will initiate its own search
      // for a transfer op
      break;
    case LocalTransferredReasonKind::NonLocal:
      transferredReason.addOtherReasonAtDistance(
          // recursive call
          findTransferredAtEntryReason(SILBlock, transferredVal), distance);
    }
  }

  // find the reason why a value was transferred at entry to a block
  const TransferredReason &
  findTransferredAtEntryReason(SILBasicBlock *SILBlock,
                               TrackableValueID transferredVal) {
    const BlockPartitionState &block = blockStates[SILBlock];
    assert(block.getEntryPartition().isTransferred(transferredVal));

    // check the cache
    if (transferredAtEntryReasons.count({SILBlock, transferredVal}))
      return transferredAtEntryReasons.at({SILBlock, transferredVal});

    // enter a dummy value in the cache to prevent circular call dependencies
    transferredAtEntryReasons[{SILBlock, transferredVal}] = TransferredReason();

    auto entryTracks = [&](TrackableValueID val) {
      return block.getEntryPartition().isTracked(val);
    };

    // this gets populated with all the tracked values at entry to this block
    // that are transferred at the exit to some predecessor block, associated
    // with the blocks that transfer them
    std::map<TrackableValueID, std::vector<SILBasicBlock *>>
        transferredInSomePred;
    for (SILBasicBlock *pred : SILBlock->getPredecessorBlocks())
      for (TrackableValueID transferredVal :
           blockStates[pred].getExitPartition().getTransferredVals())
        if (entryTracks(transferredVal))
          transferredInSomePred[transferredVal].push_back(pred);

    // this gets populated with all the multi-edges between values tracked
    // at entry to this block that will be merged because of common regionality
    // in the exit partition of some predecessor. It is not transitively closed
    // because we want to count how many steps transitive merges require
    std::map<TrackableValueID, std::set<TrackableValueID>> singleStepJoins;
    for (SILBasicBlock *pred : SILBlock->getPredecessorBlocks())
      for (std::vector<TrackableValueID> region :
           blockStates[pred].getExitPartition().getNonTransferredRegions()) {
        for (TrackableValueID fst : region) for (TrackableValueID snd : region)
            if (fst != snd && entryTracks(fst) && entryTracks(snd))
              singleStepJoins[fst].insert(snd);
      }

    // this gets populated with the distance, in terms of single step joins,
    // from the target transferredVal to other values that will get merged with
    // it because of the join at entry to this basic block
    std::map<TrackableValueID, unsigned> distancesFromTarget;

    // perform BFS
    // an entry of `{val, dist}` in the `processValues` deque indicates that
    // `val` is known to be merged with `transferredVal` (the target of this
    // find) at a distance of `dist` single-step joins
    std::deque<std::pair<TrackableValueID, unsigned>> processValues;
    processValues.push_back({transferredVal, 0});
    while (!processValues.empty()) {
      auto [currentTarget, currentDistance] = processValues.front();
      processValues.pop_front();
      distancesFromTarget[currentTarget] = currentDistance;
      for (TrackableValueID nextTarget : singleStepJoins[currentTarget])
        if (!distancesFromTarget.count(nextTarget))
          processValues.push_back({nextTarget, currentDistance + 1});
    }

    TransferredReason transferredReason;

    for (auto [predVal, distanceFromTarget] : distancesFromTarget) {
      for (SILBasicBlock *predBlock : transferredInSomePred[predVal]) {
        // one reason that our target transferredVal is transferred is that
        // predTransferredVal was transferred at exit of predBlock, and
        // distanceFromTarget merges had to be performed to make that
        // be a reason. Use this to build a TransferredReason for
        // transferredVal.
        findAndAddTransferredReasons(predBlock, predVal, transferredReason,
                                     distanceFromTarget);
      }
    }

    transferredAtEntryReasons[{SILBlock, transferredVal}] =
        std::move(transferredReason);

    return transferredAtEntryReasons[{SILBlock, transferredVal}];
  }

  // assuming that transferredVal is transferred at the point of targetOp within
  // SILBlock (or block exit if targetOp = {}), find the reason why it was
  // transferred, possibly local or nonlocal. Return the reason.
  LocalTransferredReason
  findLocalTransferredReason(SILBasicBlock *SILBlock,
                             TrackableValueID transferredVal,
                             std::optional<PartitionOp> targetOp = {}) {
    // if this is a query for consumption reason at block exit, check the cache
    if (!targetOp && transferredAtExitReasons.count({SILBlock, transferredVal}))
      return transferredAtExitReasons.at({SILBlock, transferredVal});

    const BlockPartitionState &block = blockStates[SILBlock];

    // if targetOp is null, we're checking why the value is transferred at exit,
    // so assert that it's actually transferred at exit
    assert(targetOp || block.getExitPartition().isTransferred(transferredVal));

    std::optional<LocalTransferredReason> transferredReason;

    Partition workingPartition = block.getEntryPartition();

    // we're looking for a local reason, so if the value is transferred at
    // entry, revive it for the sake of this search
    if (workingPartition.isTransferred(transferredVal))
      workingPartition.apply(PartitionOp::AssignFresh(transferredVal));

    int i = 0;
    block.forEachPartitionOp([&](const PartitionOp& partitionOp) {
      if (targetOp == partitionOp)
        return false; //break
      workingPartition.apply(partitionOp);
      if (workingPartition.isTransferred(transferredVal) &&
          !transferredReason) {
        // this partitionOp transfers the target value
        if (partitionOp.getKind() == PartitionOpKind::Transfer)
          transferredReason = LocalTransferredReason::TransferInst(partitionOp);
        else
          // a merge or assignment invalidated this, but that will be a separate
          // failure to diagnose, so we don't worry about it here
          transferredReason = LocalTransferredReason::NonTransferInst();
      }
      if (!workingPartition.isTransferred(transferredVal) && transferredReason)
        // value is no longer transferred - e.g. reassigned or assigned fresh
        transferredReason = llvm::None;

      // continue walking block
      i++;
      return true;
    });

    // if we failed to find a local transfer reason, but the value was
    // transferred at entry to the block, then the reason is "NonLocal"
    if (!transferredReason &&
        block.getEntryPartition().isTransferred(transferredVal))
      transferredReason = LocalTransferredReason::NonLocal();

    // if transferredReason is none, then transferredVal was not actually
    // transferred
    assert(transferredReason || dumpBlockSearch(SILBlock, transferredVal) &&
                                    " no consumption was found");

    // if this is a query for consumption reason at block exit, update the cache
    if (!targetOp)
      return transferredAtExitReasons[std::pair{SILBlock, transferredVal}] =
                 transferredReason.value();

    return transferredReason.value();
  }

  bool dumpBlockSearch(SILBasicBlock *SILBlock,
                       TrackableValueID transferredVal) {
    LLVM_DEBUG(unsigned i = 0;
               const BlockPartitionState &block = blockStates[SILBlock];
               Partition working = block.getEntryPartition();
               llvm::dbgs() << "┌──────────╼\n│ "; working.print(llvm::dbgs());
               block.forEachPartitionOp([&](const PartitionOp &op) {
                 llvm::dbgs() << "├[" << i++ << "] ";
                 op.print(llvm::dbgs());
                 working.apply(op);
                 llvm::dbgs() << "│ ";
                 if (working.isTransferred(transferredVal)) {
                   llvm::dbgs() << "(" << transferredVal << " TRANSFERRED) ";
                 }
                 working.print(llvm::dbgs());
                 return true;
               });
               llvm::dbgs() << "└──────────╼\n";);
    return false;
  }

public:
  RaceTracer(SILFunction *fn,
             const BasicBlockData<BlockPartitionState> &blockStates)
      : blockStates(blockStates), accumulator(fn) {}

  void traceUseOfTransferredValue(PartitionOp use,
                                  TrackableValueID transferredVal) {
    accumulator.accumulateTransferredReason(
        use, findTransferredAtOpReason(transferredVal, use));
  }

  const TransferRequireAccumulator &getAccumulator() { return accumulator; }
};

// Instances of PartitionAnalysis perform the region-based Sendable checking.
// Internally, a PartitionOpTranslator is stored to perform the translation from
// SILInstructions to PartitionOps, then a fixed point iteration is run to
// determine the set of exit and entry partitions to each point satisfying
// the flow equations.
class PartitionAnalysis {
  PartitionOpTranslator translator;

  BasicBlockData<BlockPartitionState> blockStates;

  RaceTracer raceTracer;

  SILFunction *function;

  bool solved;

  // TODO: make this configurable in a better way
  const static int NUM_REQUIREMENTS_TO_DIAGNOSE = 50;

  // The constructor initializes each block in the function by compiling it
  // to PartitionOps, then seeds the solve method by setting `needsUpdate` to
  // true for the entry block
  PartitionAnalysis(SILFunction *fn)
      : translator(fn),
        blockStates(fn,
                    [this](SILBasicBlock *block) {
                      return BlockPartitionState(block, translator);
                    }),
        raceTracer(fn, blockStates), function(fn), solved(false) {
    // initialize the entry block as needing an update, and having a partition
    // that places all its non-sendable args in a single region
    blockStates[fn->getEntryBlock()].needsUpdate = true;
    blockStates[fn->getEntryBlock()].entryPartition =
        translator.getEntryPartition();
  }

  void solve() {
    assert(!solved && "solve should only be called once");
    solved = true;

    bool anyNeedUpdate = true;
    while (anyNeedUpdate) {
      anyNeedUpdate = false;

      for (auto [block, blockState] : blockStates) {
        if (!blockState.needsUpdate) continue;

        // mark this block as no longer needing an update
        blockState.needsUpdate = false;

        // mark this block as reached by the analysis
        blockState.reached = true;

        // compute the new entry partition to this block
        Partition newEntryPartition;
        bool firstPred = true;

        // this loop computes the join of the exit partitions of all
        // predecessors of this block
        for (SILBasicBlock *predBlock : block.getPredecessorBlocks()) {
          BlockPartitionState &predState = blockStates[predBlock];
          // ignore predecessors that haven't been reached by the analysis yet
          if (!predState.reached) continue;

          if (firstPred) {
            firstPred = false;
            newEntryPartition = predState.exitPartition;
            continue;
          }

          newEntryPartition = Partition::join(
              newEntryPartition, predState.exitPartition);
        }

        // if we found predecessor blocks, then attempt to use them to
        // update the entry partition for this block, and abort this block's
        // update if the entry partition was not updated
        if (!firstPred) {
          // if the recomputed entry partition is the same as the current one,
          // perform no update
          if (Partition::equals(newEntryPartition, blockState.entryPartition))
            continue;

          // otherwise update the entry partition
          blockState.entryPartition = newEntryPartition;
        }

        // recompute this block's exit partition from its (updated) entry
        // partition, and if this changed the exit partition notify all
        // successor blocks that they need to update as well
        if (blockState.recomputeExitFromEntry()) {
          for (SILBasicBlock *succBlock : block.getSuccessorBlocks()) {
            anyNeedUpdate = true;
            blockStates[succBlock].needsUpdate = true;
          }
        }
      }
    }

    // Now that we have finished processing, sort/unique our non transferred
    // array.
    translator.sortUniqueNeverTransferredValues();
  }

  // track the AST exprs that have already had diagnostics emitted about
  llvm::DenseSet<Expr *> emittedExprs;

  // check if a diagnostic has already been emitted about expr, only
  // returns true false for each expr
  bool hasBeenEmitted(Expr *expr) {
    if (auto castExpr = dyn_cast<ImplicitConversionExpr>(expr))
      return hasBeenEmitted(castExpr->getSubExpr());

    if (emittedExprs.contains(expr)) return true;
    emittedExprs.insert(expr);
    return false;
  }

  // once the fixpoint has been solved for, run one more pass over each basic
  // block, reporting any failures due to requiring transferred regions in the
  // fixpoint state
  void diagnose() {
    assert(solved && "diagnose should not be called before solve");

    LLVM_DEBUG(
        llvm::dbgs() << "Emitting diagnostics for function "
                     << function->getName() << "\n");
    RaceTracer tracer(function, blockStates);

    for (auto [_, blockState] : blockStates) {
      // populate the raceTracer with all requires of transferred valued found
      // throughout the CFG
      blockState.diagnoseFailures(
          /*handleFailure=*/
          [&](const PartitionOp &partitionOp, TrackableValueID transferredVal) {
            auto expr = getExprForPartitionOp(partitionOp);

            // ensure that multiple consumptions at the same AST node are only
            // entered once into the race tracer
            if (hasBeenEmitted(expr))
              return;

            LLVM_DEBUG(llvm::dbgs()
                       << "Emitting Use After Transfer Error!\n"
                       << "ID:  %%" << transferredVal << "\n"
                       << "Rep: "
                       << *translator.getValueForId(transferredVal)
                               ->getRepresentative());

            raceTracer.traceUseOfTransferredValue(partitionOp, transferredVal);
          },

          /*handleTransferNonConsumable=*/
          [&](const PartitionOp &partitionOp, TrackableValueID transferredVal) {
            LLVM_DEBUG(llvm::dbgs()
                       << "Emitting ConsumeNonConsume Error!\n"
                       << "ID:  %%" << transferredVal << "\n"
                       << "Rep: "
                       << *translator.getValueForId(transferredVal)
                               ->getRepresentative());
            auto expr = getExprForPartitionOp(partitionOp);
            function->getASTContext().Diags.diagnose(
                expr->getLoc(), diag::arg_region_transferred);
          });
    }

    LLVM_DEBUG(llvm::dbgs() << "Accumulator Complete:\n";
               raceTracer.getAccumulator().print(llvm::dbgs()););

    // Ask the raceTracer to report diagnostics at the consumption sites for all
    // the racy requirement sites entered into it above.
    raceTracer.getAccumulator().emitErrorsForTransferRequire(
        NUM_REQUIREMENTS_TO_DIAGNOSE);
  }

  bool tryDiagnoseAsCallSite(const PartitionOp &transferOp,
                             unsigned numDisplayed, unsigned numHidden) {
    SILInstruction *sourceInst =
        transferOp.getSourceInst(/*assertNonNull=*/true);
    ApplyExpr *apply = sourceInst->getLoc().getAsASTNode<ApplyExpr>();
    if (!apply)
      // consumption does not correspond to an apply expression
      return false;
    auto isolationCrossing = apply->getIsolationCrossing();
    if (!isolationCrossing) {
      assert(false && "ApplyExprs should be transferring only if"
                      " they are isolation crossing");
      return false;
    }
    auto argExpr = transferOp.getSourceExpr();
    if (!argExpr)
      assert(false && "sourceExpr should be populated for ApplyExpr consumptions");

    function->getASTContext()
        .Diags
        .diagnose(argExpr->getLoc(), diag::call_site_transfer_yields_race,
                  argExpr->findOriginalType(),
                  isolationCrossing.value().getCallerIsolation(),
                  isolationCrossing.value().getCalleeIsolation(), numDisplayed,
                  numDisplayed != 1, numHidden > 0, numHidden)
        .highlight(argExpr->getSourceRange());
    return true;
  }

public:
  SWIFT_DEBUG_DUMP { print(llvm::dbgs()); }

  void print(llvm::raw_ostream &os) const {
    os << "\nPartitionAnalysis[fname=" << function->getName() << "]\n";

    for (auto [_, blockState] : blockStates) {
      blockState.print(os);
    }
  }

  static void performForFunction(SILFunction *function) {
    auto analysis = PartitionAnalysis(function);
    analysis.solve();
    LLVM_DEBUG(llvm::dbgs() << "SOLVED: "; analysis.print(llvm::dbgs()););
    analysis.diagnose();
  }
};

} // namespace

//===----------------------------------------------------------------------===//
//                         MARK: Top Level Entrypoint
//===----------------------------------------------------------------------===//

namespace {

// this class is the entry point to the region-based Sendable analysis,
// after certain checks are performed to ensure the analysis can be completed
// a PartitionAnalysis object is created and used to run the analysis.
class SendNonSendable : public SILFunctionTransform {
  // find any ApplyExprs in this function, and check if any of them make an
  // unsatisfied isolation jump, emitting appropriate diagnostics if so
  void run() override {
    SILFunction *function = getFunction();

    if (!function->getASTContext().LangOpts.hasFeature(
            Feature::SendNonSendable))
      return;

    LLVM_DEBUG(llvm::dbgs()
               << "===> PROCESSING: " << function->getName() << '\n');

    // If this function does not correspond to a syntactic declContext and it
    // doesn't have a parent module, don't check it since we cannot check if a
    // type is sendable.
    if (!function->getDeclContext() && !function->getParentModule()) {
      LLVM_DEBUG(llvm::dbgs() << "No Decl Context! Skipping!\n");
      return;
    }

    // The sendable protocol should /always/ be available if SendNonSendable is
    // enabled. If not, there is a major bug in the compiler and we should fail
    // loudly.
    if (!function->getASTContext().getProtocol(KnownProtocolKind::Sendable))
      llvm::report_fatal_error("Sendable protocol not available!");

    PartitionAnalysis::performForFunction(function);
  }
};

} // end anonymous namespace

/// This pass is known to depend on the following passes having run before it:
/// none so far.
SILTransform *swift::createSendNonSendable() {
  return new SendNonSendable();
}
