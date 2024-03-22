//===--- PartitionUtils.h -------------------------------------------------===//
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

#ifndef SWIFT_SILOPTIMIZER_UTILS_PARTITIONUTILS_H
#define SWIFT_SILOPTIMIZER_UTILS_PARTITIONUTILS_H

#include "swift/Basic/Defer.h"
#include "swift/Basic/FrozenMultiMap.h"
#include "swift/Basic/ImmutablePointerSet.h"
#include "swift/Basic/LLVM.h"
#include "swift/SIL/SILInstruction.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Support/Debug.h"
#include <algorithm>
#include <variant>

#define DEBUG_TYPE "transfer-non-sendable"

namespace swift {

namespace PartitionPrimitives {

#ifndef NDEBUG
extern bool REGIONBASEDISOLATION_ENABLE_VERBOSE_LOGGING;
#define REGIONBASEDISOLATION_VERBOSE_LOG(...)                                  \
  do {                                                                         \
    if (PartitionPrimitives::REGIONBASEDISOLATION_ENABLE_VERBOSE_LOGGING) {    \
      LLVM_DEBUG(__VA_ARGS__);                                                 \
    }                                                                          \
  } while (0);
#else
#define REGIONBASEDISOLATION_VERBOSE_LOG(...)
#endif

struct Element {
  unsigned num;

  explicit Element(int num) : num(num) {}

  bool operator==(const Element &other) const { return num == other.num; }
  bool operator<(const Element &other) const { return num < other.num; }

  operator unsigned() const { return num; }
};

struct Region {
  unsigned num;

  explicit Region(unsigned num) : num(num) {}

  bool operator==(const Region &other) const { return num == other.num; }
  bool operator<(const Region &other) const { return num < other.num; }

  operator unsigned() const { return num; }
};

} // namespace PartitionPrimitives

} // namespace swift

namespace llvm {

template <>
struct DenseMapInfo<swift::PartitionPrimitives::Region> {
  using Region = swift::PartitionPrimitives::Region;

  static Region getEmptyKey() {
    return Region(DenseMapInfo<unsigned>::getEmptyKey());
  }
  static Region getTombstoneKey() {
    return Region(DenseMapInfo<unsigned>::getTombstoneKey());
  }

  static unsigned getHashValue(Region region) {
    return DenseMapInfo<unsigned>::getHashValue(region);
  }
  static bool isEqual(Region LHS, Region RHS) { return LHS == RHS; }
};

} // namespace llvm

namespace swift {

class SILIsolationInfo {
public:
  /// The lattice is:
  ///
  /// Unknown -> Disconnected -> TransferringParameter -> Task -> Actor.
  ///
  /// Unknown means no information. We error when merging on it.
  enum Kind {
    Unknown,
    Disconnected,
    Task,
    Actor,
  };

private:
  Kind kind;
  // clang-format off
  std::variant<
    // Used for actor isolated when we have ActorIsolation info from the AST.
    std::optional<ActorIsolation>,
    // Used for actor isolation when we infer the actor at the SIL level.
    NominalTypeDecl *,
    // The task isolated parameter when we find a task isolated value.
    SILValue
  > data;
  // clang-format on

  SILIsolationInfo(Kind kind, std::optional<ActorIsolation> actorIsolation)
      : kind(kind), data(actorIsolation) {}
  SILIsolationInfo(Kind kind, NominalTypeDecl *decl) : kind(kind), data(decl) {}

  SILIsolationInfo(Kind kind, SILValue value) : kind(kind), data(value) {}

public:
  SILIsolationInfo() : kind(Kind::Unknown), data() {}

  operator bool() const { return kind != Kind::Unknown; }

  operator Kind() const { return kind; }

  Kind getKind() const { return kind; }

  bool isDisconnected() const { return kind == Kind::Disconnected; }
  bool isActorIsolated() const { return kind == Kind::Actor; }
  bool isTaskIsolated() const { return kind == Kind::Task; }

  void print(llvm::raw_ostream &os) const {
    switch (Kind(*this)) {
    case Unknown:
      os << "unknown";
      return;
    case Disconnected:
      os << "disconnected";
      return;
    case Actor:
      os << "actor";
      return;
    case Task:
      os << "task";
      return;
    }
  }

  void printForDiagnostics(llvm::raw_ostream &os) const;

  SWIFT_DEBUG_DUMP {
    print(llvm::dbgs());
    llvm::dbgs() << '\n';
  }

  std::optional<ActorIsolation> getActorIsolation() const {
    assert(kind == Actor);
    assert(std::holds_alternative<std::optional<ActorIsolation>>(data) &&
           "Doesn't have an actor isolation?!");
    return std::get<std::optional<ActorIsolation>>(data);
  }

  NominalTypeDecl *getActorInstance() const {
    assert(kind == Actor);
    assert(std::holds_alternative<NominalTypeDecl *>(data) &&
           "Doesn't have an actor instance?!");
    return std::get<NominalTypeDecl *>(data);
  }

  SILValue getTaskIsolatedValue() const {
    assert(kind == Task);
    assert(std::holds_alternative<SILValue>(data) &&
           "Doesn't have a task isolated value");
    return std::get<SILValue>(data);
  }

  bool hasActorIsolation() const {
    return kind == Actor &&
           std::holds_alternative<std::optional<ActorIsolation>>(data);
  }

  bool hasActorInstance() const {
    return kind == Actor && std::holds_alternative<NominalTypeDecl *>(data);
  }

  bool hasTaskIsolatedValue() const {
    return kind == Task && std::holds_alternative<SILValue>(data);
  }

  /// If we actually have an actor decl, return that. Otherwise, see if we have
  /// an actor isolation if we can find one in there. Returns nullptr if we
  /// fail.
  NominalTypeDecl *tryInferActorDecl() const {
    if (hasActorIsolation()) {
      auto actorIsolation = getActorIsolation();
      if (auto *actor = actorIsolation->getActorOrNullPtr()) {
        return actor;
      }
      return nullptr;
    }

    if (hasActorInstance()) {
      auto actorDecl = getActorInstance();
      return actorDecl;
    }

    return nullptr;
  }

  [[nodiscard]] SILIsolationInfo merge(SILIsolationInfo other) const {
    // If we are greater than the other kind, then we are further along the
    // lattice. We ignore the change.
    if (unsigned(other.kind) < unsigned(kind))
      return *this;

    // TODO: Make this failing mean that we emit an unknown SIL error instead of
    // asserting.
    assert((!other.isActorIsolated() || !isActorIsolated() || *this == other) &&
           "Actor can only be merged with the same actor");

    // Otherwise, take the other value.
    return other;
  }

  SILIsolationInfo withActorIsolated(ActorIsolation isolation) {
    return SILIsolationInfo::getActorIsolated(isolation);
  }

  static SILIsolationInfo getDisconnected() { return {Kind::Disconnected, {}}; }

  static SILIsolationInfo getActorIsolated(ActorIsolation actorIsolation) {
    return {Kind::Actor, actorIsolation};
  }

  /// Sometimes we may have something that is actor isolated or that comes from
  /// a type. First try getActorIsolation and otherwise, just use the type.
  static SILIsolationInfo getActorIsolated(NominalTypeDecl *nomDecl) {
    auto actorIsolation = swift::getActorIsolation(nomDecl);
    if (actorIsolation.isActorIsolated())
      return getActorIsolated(actorIsolation);
    if (nomDecl->isActor())
      return {Kind::Actor, nomDecl};
    return {};
  }

  static SILIsolationInfo getTaskIsolated(SILValue value) {
    return {Kind::Task, value};
  }

  /// Attempt to infer the isolation region info for \p inst.
  static SILIsolationInfo get(SILInstruction *inst);

  /// Attempt to infer the isolation region info for \p arg.
  static SILIsolationInfo get(SILFunctionArgument *arg);

  bool operator==(const SILIsolationInfo &other) const {
    if (getKind() != other.getKind())
      return false;

    switch (getKind()) {
    case Unknown:
    case Disconnected:
      return true;
    case Task:
      return getTaskIsolatedValue() == other.getTaskIsolatedValue();
    case Actor:
      // First try to use actor isolation if we have them.
      if (hasActorIsolation() && other.hasActorIsolation()) {
        auto lhsIsolation = getActorIsolation();
        auto rhsIsolation = other.getActorIsolation();
        if (lhsIsolation && rhsIsolation)
          return *lhsIsolation == *rhsIsolation;
      }

      // Otherwise, try to use the inferred actor decl.
      auto *lhsDecl = tryInferActorDecl();
      auto *rhsDecl = tryInferActorDecl();
      if (lhsDecl && rhsDecl)
        return lhsDecl == rhsDecl;

      // Otherwise, false, they are not equal.
      return false;
    }
  }

  void Profile(llvm::FoldingSetNodeID &id) const {
    id.AddInteger(getKind());
    switch (getKind()) {
    case Unknown:
    case Disconnected:
      return;
    case Task:
      id.AddPointer(getTaskIsolatedValue());
      return;
    case Actor:
      // We profile in integer cases here so that we can always distinguish in
      // between the various cases and the non-case. Just being paranoid.
      if (hasActorIsolation()) {
        if (auto isolation = getActorIsolation()) {
          id.AddInteger(1);
          return isolation->Profile(id);
        }
      }

      if (hasActorInstance()) {
        id.AddInteger(2);
        return id.AddPointer(getActorInstance());
      }

      id.AddInteger(3);
      break;
    }
  }
};

} // namespace swift

namespace swift {

struct TransferringOperand {
  using ValueType = llvm::PointerIntPair<Operand *, 1>;
  ValueType value;

  TransferringOperand() : value() {}
  TransferringOperand(Operand *op, bool isClosureCaptured)
      : value(op, isClosureCaptured) {}
  explicit TransferringOperand(Operand *op) : value(op, false) {}
  TransferringOperand(ValueType newValue) : value(newValue) {}

  operator bool() const { return bool(value.getPointer()); }

  Operand *getOperand() const { return value.getPointer(); }

  bool isClosureCaptured() const { return value.getInt(); }

  SILInstruction *getUser() const { return getOperand()->getUser(); }

  bool operator<(const TransferringOperand &other) const {
    return value < other.value;
  }

  bool operator>=(const TransferringOperand &other) const {
    return !(value < other.value);
  }

  bool operator>(const TransferringOperand &other) const {
    return value > other.value;
  }

  bool operator<=(const TransferringOperand &other) const {
    return !(value > other.value);
  }

  bool operator==(const TransferringOperand &other) const {
    return value == other.value;
  }

  void print(llvm::raw_ostream &os) const {
    os << "Op Num: " << getOperand()->getOperandNumber() << ". "
       << "Capture: " << (isClosureCaptured() ? "yes. " : "no.  ")
       << "User: " << *getUser();
  }

  static void Profile(llvm::FoldingSetNodeID &id, Operand *op,
                      bool isClosureCaptured) {
    id.AddPointer(op);
    id.AddBoolean(isClosureCaptured);
  }

  void Profile(llvm::FoldingSetNodeID &id) const {
    Profile(id, getOperand(), isClosureCaptured());
  }

  SWIFT_DEBUG_DUMP { print(llvm::dbgs()); }
};

} // namespace swift

namespace swift {

/// PartitionOpKind represents the different kinds of PartitionOps that
/// SILInstructions can be translated to
enum class PartitionOpKind : uint8_t {
  /// Assign one value to the region of another, takes two args, second arg
  /// must already be tracked with a non-transferred region
  Assign,

  /// Assign one value to a fresh region, takes one arg.
  AssignFresh,

  /// Merge the regions of two values, takes two args, both must be from
  /// non-transferred regions.
  Merge,

  /// Transfer the region of a value if not already transferred, takes one arg.
  Transfer,

  /// Due to an async let or something like that a value that was transferred is
  /// no longer transferred.
  UndoTransfer,

  /// Require the region of a value to be non-transferred, takes one arg.
  Require,
};

/// PartitionOp represents a primitive operation that can be performed on
/// Partitions. This is part of the TransferNonSendable SIL pass workflow:
/// first SILBasicBlocks are compiled to vectors of PartitionOps, then a fixed
/// point partition is found over the CFG.
class PartitionOp {
  using Element = PartitionPrimitives::Element;

private:
  PartitionOpKind opKind;
  llvm::SmallVector<Element, 2> opArgs;

  /// Record the SILInstruction that this PartitionOp was generated from, if
  /// generated during compilation from a SILBasicBlock
  PointerUnion<SILInstruction *, Operand *> source;

  // TODO: can the following declarations be merged?
  PartitionOp(PartitionOpKind opKind, Element arg1,
              SILInstruction *sourceInst = nullptr)
      : opKind(opKind), opArgs({arg1}), source(sourceInst) {
    assert(((opKind != PartitionOpKind::Transfer &&
             opKind != PartitionOpKind::UndoTransfer) ||
            sourceInst) &&
           "Transfer needs a sourceInst");
  }

  PartitionOp(PartitionOpKind opKind, Element arg1, Operand *sourceOperand)
      : opKind(opKind), opArgs({arg1}), source(sourceOperand) {
    assert(((opKind != PartitionOpKind::Transfer &&
             opKind != PartitionOpKind::UndoTransfer) ||
            bool(sourceOperand)) &&
           "Transfer needs a sourceInst");
  }

  PartitionOp(PartitionOpKind opKind, Element arg1, Element arg2,
              SILInstruction *sourceInst = nullptr)
      : opKind(opKind), opArgs({arg1, arg2}), source(sourceInst) {
    assert(((opKind != PartitionOpKind::Transfer &&
             opKind != PartitionOpKind::UndoTransfer) ||
            sourceInst) &&
           "Transfer needs a sourceInst");
  }

  friend class Partition;

public:
  static PartitionOp Assign(Element tgt, Element src,
                            SILInstruction *sourceInst = nullptr) {
    return PartitionOp(PartitionOpKind::Assign, tgt, src, sourceInst);
  }

  static PartitionOp AssignFresh(Element tgt,
                                 SILInstruction *sourceInst = nullptr) {
    return PartitionOp(PartitionOpKind::AssignFresh, tgt, sourceInst);
  }

  static PartitionOp Transfer(Element tgt, Operand *transferringOp) {
    return PartitionOp(PartitionOpKind::Transfer, tgt, transferringOp);
  }

  static PartitionOp UndoTransfer(Element tgt,
                                  SILInstruction *untransferringInst) {
    return PartitionOp(PartitionOpKind::UndoTransfer, tgt, untransferringInst);
  }

  static PartitionOp Merge(Element tgt1, Element tgt2,
                           SILInstruction *sourceInst = nullptr) {
    return PartitionOp(PartitionOpKind::Merge, tgt1, tgt2, sourceInst);
  }

  static PartitionOp Require(Element tgt,
                             SILInstruction *sourceInst = nullptr) {
    return PartitionOp(PartitionOpKind::Require, tgt, sourceInst);
  }

  bool operator==(const PartitionOp &other) const {
    return opKind == other.opKind && opArgs == other.opArgs &&
           source == other.source;
  };

  bool operator<(const PartitionOp &other) const {
    if (opKind != other.opKind)
      return opKind < other.opKind;
    if (opArgs != other.opArgs)
      return opArgs < other.opArgs;
    return source < other.source;
  }

  PartitionOpKind getKind() const { return opKind; }

  ArrayRef<Element> getOpArgs() const { return opArgs; }

  SILInstruction *getSourceInst() const {
    if (source.is<Operand *>())
      return source.get<Operand *>()->getUser();
    return source.get<SILInstruction *>();
  }

  Operand *getSourceOp() const { return source.get<Operand *>(); }

  SILLocation getSourceLoc() const { return getSourceInst()->getLoc(); }

  SWIFT_DEBUG_DUMP { print(llvm::dbgs()); }

  void print(llvm::raw_ostream &os, bool extraSpace = false) const {
    switch (opKind) {
    case PartitionOpKind::Assign: {
      constexpr static char extraSpaceLiteral[10] = "      ";
      os << "assign ";
      if (extraSpace)
        os << extraSpaceLiteral;
      os << "%%" << opArgs[0] << " = %%" << opArgs[1];
      break;
    }
    case PartitionOpKind::AssignFresh:
      os << "assign_fresh %%" << opArgs[0];
      break;
    case PartitionOpKind::Transfer: {
      constexpr static char extraSpaceLiteral[10] = "    ";
      os << "transfer ";
      if (extraSpace)
        os << extraSpaceLiteral;
      os << "%%" << opArgs[0];
      break;
    }
    case PartitionOpKind::UndoTransfer: {
      constexpr static char extraSpaceLiteral[10] = "    ";
      os << "undo_transfer ";
      if (extraSpace)
        os << extraSpaceLiteral;
      os << "%%" << opArgs[0];
      break;
    }
    case PartitionOpKind::Merge: {
      constexpr static char extraSpaceLiteral[10] = "       ";
      os << "merge ";
      if (extraSpace)
        os << extraSpaceLiteral;
      os << "%%" << opArgs[0] << " with %%" << opArgs[1];
      break;
    }
    case PartitionOpKind::Require: {
      constexpr static char extraSpaceLiteral[10] = "     ";
      os << "require ";
      if (extraSpace)
        os << extraSpaceLiteral;
      os << "%%" << opArgs[0];
      break;
    }
    }
    os << ": " << *getSourceInst();
  }
};

/// A map from Element -> Region that represents the current partition set.
///
///
class Partition {
public:
  /// A class defined in PartitionUtils unittest used to grab state from
  /// Partition without exposing it to other users.
  struct PartitionTester;

  using Element = PartitionPrimitives::Element;
  using Region = PartitionPrimitives::Region;
  using TransferringOperandSet = ImmutablePointerSet<TransferringOperand *>;
  using TransferringOperandSetFactory =
      ImmutablePointerSetFactory<TransferringOperand *>;

private:
  /// A map from a region number to a instruction that consumes it.
  ///
  /// All we care is that we ever track a single SILInstruction for a region
  /// since we are fine with emitting a single error per value and letting the
  /// user recompile. If this is an ask for in the future, we can use a true
  /// multi map here. The implication of this is that when we are performing
  /// dataflow we use a union operation to combine CFG elements and just take
  /// the first instruction that we see.
  llvm::SmallDenseMap<Region, TransferringOperandSet *, 2>
      regionToTransferredOpMap;

  /// Label each index with a non-negative (unsigned) label if it is associated
  /// with a valid region.
  std::map<Element, Region> elementToRegionMap;

  /// Track a label that is guaranteed to be strictly larger than all in use,
  /// and therefore safe for use as a fresh label.
  Region fresh_label = Region(0);

  /// In a canonical partition, all regions are labelled with the smallest index
  /// of any member. Certain operations like join and equals rely on
  /// canonicality so when it's invalidated this boolean tracks that, and it
  /// must be reestablished by a call to canonicalize().
  bool canonical;

public:
  Partition() : elementToRegionMap({}), canonical(true) {}

  /// 1-arg constructor used when canonicality will be immediately invalidated,
  /// so set to false to begin with
  Partition(bool canonical) : elementToRegionMap({}), canonical(canonical) {}

  static Partition singleRegion(ArrayRef<Element> indices) {
    Partition p;
    if (!indices.empty()) {
      Region min_index =
          Region(*std::min_element(indices.begin(), indices.end()));
      p.fresh_label = Region(min_index + 1);
      for (Element index : indices) {
        p.elementToRegionMap.insert_or_assign(index, min_index);
      }
    }

    assert(p.is_canonical_correct());
    return p;
  }

  static Partition separateRegions(ArrayRef<Element> indices) {
    Partition p;
    if (indices.empty())
      return p;

    auto maxIndex = Element(0);
    for (Element index : indices) {
      p.elementToRegionMap.insert_or_assign(index, Region(index));
      maxIndex = Element(std::max(maxIndex, index));
    }
    p.fresh_label = Region(maxIndex + 1);
    assert(p.is_canonical_correct());
    return p;
  }

  /// Test two partititons for equality by first putting them in canonical form
  /// then comparing for exact equality.
  ///
  /// Runs in linear time.
  static bool equals(Partition &fst, Partition &snd) {
    fst.canonicalize();
    snd.canonicalize();

    return fst.elementToRegionMap == snd.elementToRegionMap;
  }

  bool isTrackingElement(Element val) const {
    return elementToRegionMap.count(val);
  }

  /// Mark val as transferred.
  void markTransferred(Element val,
                       TransferringOperandSet *transferredOperandSet) {
    // First see if our val is tracked. If it is not tracked, insert it and mark
    // its new region as transferred.
    if (!isTrackingElement(val)) {
      elementToRegionMap.insert_or_assign(val, fresh_label);
      regionToTransferredOpMap.insert({fresh_label, transferredOperandSet});
      fresh_label = Region(fresh_label + 1);
      canonical = false;
      return;
    }

    // Otherwise, we already have this value in the map. Try to insert it.
    auto iter1 = elementToRegionMap.find(val);
    assert(iter1 != elementToRegionMap.end());
    auto iter2 = regionToTransferredOpMap.try_emplace(iter1->second,
                                                      transferredOperandSet);

    // If we did insert, just return. We were not tracking any state.
    if (iter2.second)
      return;

    // Otherwise, we need to merge the sets.
    iter2.first->getSecond() =
        iter2.first->second->merge(transferredOperandSet);
  }

  /// If val was marked as transferred, unmark it as transfer. Returns true if
  /// we found that \p val was transferred. We return false otherwise.
  bool undoTransfer(Element val) {
    // First see if our val is tracked. If it is not tracked, insert it.
    if (!isTrackingElement(val)) {
      elementToRegionMap.insert_or_assign(val, fresh_label);
      fresh_label = Region(fresh_label + 1);
      canonical = false;
      return true;
    }

    // Otherwise, we already have this value in the map. Remove it from the
    // transferred map.
    auto iter1 = elementToRegionMap.find(val);
    assert(iter1 != elementToRegionMap.end());
    return regionToTransferredOpMap.erase(iter1->second);
  }

  void trackNewElement(Element newElt) {
    SWIFT_DEFER { validateRegionToTransferredOpMapRegions(); };

    // First try to emplace newElt with fresh_label.
    auto iter = elementToRegionMap.try_emplace(newElt, fresh_label);

    // If we did insert, then we know that the value is completely new. We can
    // just update the fresh_label, set canonical to false, and return.
    if (iter.second) {
      // Increment the fresh label so it remains fresh.
      fresh_label = Region(fresh_label + 1);
      canonical = false;
      return;
    }

    // Otherwise, we have a bit more work that we need to perform:
    //
    // 1. We of course need to update iter to point at fresh_label.
    //
    // 2. We need to see if this value was the last element in its current
    // region. If so, then we need to remove the region from the transferred op
    // map.
    //
    // This is important to ensure that every region in the transferredOpMap is
    // also in elementToRegionMap.
    auto oldRegion = iter.first->second;
    iter.first->second = fresh_label;

    if (llvm::none_of(elementToRegionMap,
                      [&](std::pair<Element, Region> value) {
                        return value.second == oldRegion;
                      })) {
      regionToTransferredOpMap.erase(oldRegion);
    }

    // Increment the fresh label so it remains fresh.
    fresh_label = Region(fresh_label + 1);
    canonical = false;
  }

  /// Assigns \p oldElt to the region associated with \p newElt.
  void assignElement(Element oldElt, Element newElt) {
    SWIFT_DEFER { validateRegionToTransferredOpMapRegions(); };

    // First try to emplace oldElt with the newRegion.
    auto newRegion = elementToRegionMap.at(newElt);
    auto iter = elementToRegionMap.try_emplace(oldElt, newRegion);

    // If we did an insert, then we know that the value is new and we can just
    // set canonical to false and return.
    if (iter.second) {
      canonical = false;
      return;
    }

    // Otherwise, we did an assign. In such a case, we need to see if oldElt was
    // the last element in oldRegion. If so, we need to erase the oldRegion from
    // regionToTransferredOpMap.
    auto oldRegion = iter.first->second;
    iter.first->second = newRegion;

    if (llvm::none_of(elementToRegionMap,
                      [&](std::pair<Element, Region> value) {
                        return value.second == oldRegion;
                      })) {
      regionToTransferredOpMap.erase(oldRegion);
    }

    canonical = false;
  }

  bool areElementsInSameRegion(Element firstElt, Element secondElt) const {
    return elementToRegionMap.at(firstElt) == elementToRegionMap.at(secondElt);
  }

  Region getRegion(Element elt) const { return elementToRegionMap.at(elt); }

  using iterator = std::map<Element, Region>::iterator;
  iterator begin() { return elementToRegionMap.begin(); }
  iterator end() { return elementToRegionMap.end(); }
  llvm::iterator_range<iterator> range() { return {begin(), end()}; }

  /// Construct the partition corresponding to the union of the two passed
  /// partitions.
  ///
  /// Runs in quadratic time.
  static Partition join(const Partition &fst, const Partition &snd) {
    // First copy and canonicalize our inputs.
    Partition fstReduced = fst;
    Partition sndReduced = snd;

    fstReduced.canonicalize();
    sndReduced.canonicalize();

    // For each (sndEltNumber, sndRegionNumber) in snd_reduced...
    for (auto pair : sndReduced.elementToRegionMap) {
      auto sndEltNumber = pair.first;
      auto sndRegionNumber = pair.second;

      // Check if fstReduced has sndEltNumber within it...
      if (fstReduced.elementToRegionMap.count(sndEltNumber)) {
        // If we do, we just merge sndEltNumber into fstRegion.
        auto mergedRegion =
            fstReduced.merge(sndEltNumber, Element(sndRegionNumber));

        // Then if sndRegionNumber is transferred in sndReduced, make sure
        // mergedRegion is transferred in fstReduced.
        auto sndIter =
            sndReduced.regionToTransferredOpMap.find(sndRegionNumber);
        if (sndIter != sndReduced.regionToTransferredOpMap.end()) {
          auto fstIter = fstReduced.regionToTransferredOpMap.try_emplace(
              mergedRegion, sndIter->second);
          if (!fstIter.second) {
            fstIter.first->getSecond() =
                fstIter.first->getSecond()->merge(sndIter->second);
          }
        }
        continue;
      }

      // Then check if the representative element number for this element in snd
      // is in fst. In that case, we know that we visited it before we visited
      // this elt number (since we are processing in order) so what ever is
      // mapped to that number in snd must be the correct number for this
      // element as well since this number is guaranteed to be greater than our
      // representative and the number mapped to our representative in fst must
      // be <= our representative.
      //
      // In this case, we do not need to propagate transfer into fstRegion since
      // we would have handled that already when we visited our earlier
      // representative element number.
      {
        auto iter =
            fstReduced.elementToRegionMap.find(Element(sndRegionNumber));
        if (iter != fstReduced.elementToRegionMap.end()) {
          fstReduced.elementToRegionMap.insert({sndEltNumber, iter->second});
          // We want fresh_label to always be one element larger than our
          // maximum element.
          if (fstReduced.fresh_label <= Region(sndEltNumber))
            fstReduced.fresh_label = Region(sndEltNumber + 1);
          continue;
        }
      }

      // Otherwise, we have an element that is not in fst and its representative
      // is not in fst. This means that we must be our representative in snd
      // since we should have visited our representative earlier if we were not
      // due to our traversal being in order. Thus just add this to fst_reduced.
      assert(sndEltNumber == Element(sndRegionNumber));
      fstReduced.elementToRegionMap.insert({sndEltNumber, sndRegionNumber});
      auto sndIter = sndReduced.regionToTransferredOpMap.find(sndRegionNumber);
      if (sndIter != sndReduced.regionToTransferredOpMap.end()) {
        auto fstIter = fstReduced.regionToTransferredOpMap.try_emplace(
            sndRegionNumber, sndIter->second);
        if (!fstIter.second)
          fstIter.first->getSecond() =
              fstIter.first->second->merge(sndIter->second);
      }
      if (fstReduced.fresh_label <= sndRegionNumber)
        fstReduced.fresh_label = Region(sndEltNumber + 1);
    }

    assert(fstReduced.is_canonical_correct());

    // fst_reduced is now the join
    return fstReduced;
  }

  /// Return a vector of the transferred values in this partition.
  std::vector<Element> getTransferredVals() const {
    // For effeciency, this could return an iterator not a vector.
    std::vector<Element> transferredVals;
    for (auto [i, _] : elementToRegionMap)
      if (isTransferred(i))
        transferredVals.push_back(i);
    return transferredVals;
  }

  /// Return a vector of the non-transferred regions in this partition, each
  /// represented as a vector of values.
  std::vector<std::vector<Element>> getNonTransferredRegions() const {
    // For effeciency, this could return an iterator not a vector.
    std::map<Region, std::vector<Element>> buckets;

    for (auto [i, label] : elementToRegionMap)
      buckets[label].push_back(i);

    std::vector<std::vector<Element>> doubleVec;

    for (auto [_, bucket] : buckets)
      doubleVec.push_back(bucket);

    return doubleVec;
  }

  void dump_labels() const LLVM_ATTRIBUTE_USED {
    llvm::dbgs() << "Partition";
    if (canonical)
      llvm::dbgs() << "(canonical)";
    llvm::dbgs() << "(fresh=" << fresh_label << "){";
    for (const auto &[i, label] : elementToRegionMap)
      llvm::dbgs() << "[" << i << ": " << label << "] ";
    llvm::dbgs() << "}\n";
  }

  SWIFT_DEBUG_DUMP { print(llvm::dbgs()); }

  void print(llvm::raw_ostream &os) const {
    SmallFrozenMultiMap<Region, Element, 8> multimap;

    for (auto [eltNo, regionNo] : elementToRegionMap)
      multimap.insert(regionNo, eltNo);

    multimap.setFrozen();

    os << "[";
    for (auto [regionNo, elementNumbers] : multimap.getRange()) {
      auto iter = regionToTransferredOpMap.find(regionNo);
      bool isTransferred = iter != regionToTransferredOpMap.end();
      bool isClosureCaptured = false;
      if (isTransferred) {
        isClosureCaptured = llvm::any_of(
            iter->getSecond()->range(), [](const TransferringOperand *operand) {
              return operand->isClosureCaptured();
            });
      }

      if (isTransferred) {
        os << '{';
        if (isClosureCaptured)
          os << '*';
      } else {
        os << '(';
      }

      int j = 0;
      for (Element i : elementNumbers) {
        os << (j++ ? " " : "") << i;
      }
      if (isTransferred) {
        if (isClosureCaptured)
          os << '*';
        os << '}';
      } else {
        os << ')';
      }
    }
    os << "]\n";
  }

  LLVM_ATTRIBUTE_USED void dumpVerbose() const { printVerbose(llvm::dbgs()); }

  void printVerbose(llvm::raw_ostream &os) const {
    SmallFrozenMultiMap<Region, Element, 8> multimap;

    for (auto [eltNo, regionNo] : elementToRegionMap)
      multimap.insert(regionNo, eltNo);

    multimap.setFrozen();

    for (auto [regionNo, elementNumbers] : multimap.getRange()) {
      auto iter = regionToTransferredOpMap.find(regionNo);
      bool isTransferred = iter != regionToTransferredOpMap.end();
      bool isClosureCaptured = false;
      if (isTransferred) {
        isClosureCaptured = llvm::any_of(
            iter->getSecond()->range(), [](const TransferringOperand *operand) {
              return operand->isClosureCaptured();
            });
      }

      os << "Region: " << regionNo << ". ";
      if (isTransferred) {
        os << '{';
        if (isClosureCaptured)
          os << '*';
      } else {
        os << '(';
      }

      int j = 0;
      for (Element i : elementNumbers) {
        os << (j++ ? " " : "") << i;
      }
      if (isTransferred) {
        if (isClosureCaptured)
          os << '*';
        os << '}';
      } else {
        os << ')';
      }
      os << "\n";
      os << "TransferInsts:\n";
      if (isTransferred) {
        for (auto op : iter->getSecond()->data()) {
          os << "    ";
          op->print(os);
        }
      } else {
        os << "None.\n";
      }
    }
  }

  bool isTransferred(Element val) const {
    auto iter = elementToRegionMap.find(val);
    if (iter == elementToRegionMap.end())
      return false;
    return regionToTransferredOpMap.count(iter->second);
  }

  /// Return the instruction that transferred \p val's region or nullptr
  /// otherwise.
  TransferringOperandSet *getTransferred(Element val) const {
    auto iter = elementToRegionMap.find(val);
    if (iter == elementToRegionMap.end())
      return nullptr;
    auto iter2 = regionToTransferredOpMap.find(iter->second);
    if (iter2 == regionToTransferredOpMap.end())
      return nullptr;
    auto *set = iter2->second;
    assert(!set->empty());
    return set;
  }

  /// Validate that all regions in the regionToTransferredOpMap exist in the
  /// elementToRegionMap.
  ///
  /// Asserts when NDEBUG is set. Does nothing otherwise.
  void validateRegionToTransferredOpMapRegions() const {
#ifndef NDEBUG
    llvm::SmallSet<Region, 8> regions;
    for (auto [eltNo, regionNo] : elementToRegionMap) {
      regions.insert(regionNo);
    }
    for (auto [regionNo, opSet] : regionToTransferredOpMap) {
      assert(regions.contains(regionNo) && "Region doesn't exist?!");
    }
#endif
  }

  /// Used only in assertions, check that Partitions promised to be canonical
  /// are actually canonical
  bool is_canonical_correct() {
#ifdef NDEBUG
    return true;
#else
    if (!canonical)
      return true; // vacuously correct

    auto fail = [&](Element i, int type) {
      llvm::errs() << "FAIL(i=" << i << "; type=" << type << "): ";
      print(llvm::errs());
      return false;
    };

    for (auto &[eltNo, regionNo] : elementToRegionMap) {
      // Labels should not exceed fresh_label.
      if (regionNo >= fresh_label)
        return fail(eltNo, 0);

      // The label of a region should be at most as large as each index in it.
      if ((unsigned)regionNo > eltNo)
        return fail(eltNo, 1);

      // Each region label should also be an element of the partition.
      if (!elementToRegionMap.count(Element(regionNo)))
        return fail(eltNo, 2);

      // Each element that is also a region label should be mapped to itself.
      if (elementToRegionMap.at(Element(regionNo)) != regionNo)
        return fail(eltNo, 3);
    }

    // Before we do anything, validate region to transferred op map.
    validateRegionToTransferredOpMapRegions();

    return true;
#endif
  }

  /// Merge the regions of two indices while maintaining canonicality. Returns
  /// the final region used.
  ///
  /// This runs in linear time.
  Region merge(Element fst, Element snd) {
    assert(elementToRegionMap.count(fst) && elementToRegionMap.count(snd));

    auto fstRegion = elementToRegionMap.at(fst);
    auto sndRegion = elementToRegionMap.at(snd);

    if (fstRegion == sndRegion)
      return fstRegion;

    // Maintain canonicality by renaming the greater-numbered region to the
    // smaller region.
    std::optional<Region> result;
    if (fstRegion < sndRegion) {
      result = fstRegion;

      // Rename snd to use first region.
      horizontalUpdate(elementToRegionMap, snd, fstRegion);
      auto iter = regionToTransferredOpMap.find(sndRegion);
      if (iter != regionToTransferredOpMap.end()) {
        auto operand = iter->second;
        regionToTransferredOpMap.erase(iter);
        regionToTransferredOpMap.try_emplace(fstRegion, operand);
      }
    } else {
      result = sndRegion;

      horizontalUpdate(elementToRegionMap, fst, sndRegion);
      auto iter = regionToTransferredOpMap.find(fstRegion);
      if (iter != regionToTransferredOpMap.end()) {
        auto operand = iter->second;
        regionToTransferredOpMap.erase(iter);
        regionToTransferredOpMap.try_emplace(sndRegion, operand);
      }
    }

    assert(is_canonical_correct());
    assert(elementToRegionMap.at(fst) == elementToRegionMap.at(snd));
    return *result;
  }

private:
  /// For each region label that occurs, find the first index at which it occurs
  /// and relabel all instances of it to that index.  This excludes the -1 label
  /// for transferred regions.
  ///
  /// This runs in linear time.
  void canonicalize() {
    if (canonical)
      return;
    canonical = true;

    validateRegionToTransferredOpMapRegions();
    std::map<Region, Region> oldRegionToRelabeledMap;

    // We rely on in-order traversal of labels to ensure that we always take the
    // lowest eltNumber.
    for (auto &[eltNo, regionNo] : elementToRegionMap) {
      if (!oldRegionToRelabeledMap.count(regionNo)) {
        // if this is the first time encountering this region label,
        // then this region label should be relabelled to this index,
        // so enter that into the map
        oldRegionToRelabeledMap.insert_or_assign(regionNo, Region(eltNo));
      }

      // Update this label with either its own index, or a prior index that
      // shared a region with it.
      regionNo = oldRegionToRelabeledMap.at(regionNo);

      // The maximum index iterated over will be used here to appropriately
      // set fresh_label.
      fresh_label = Region(eltNo + 1);
    }

    // Then relabel our regionToTransferredInst map if we need to by swapping
    // out the old map and updating.
    //
    // TODO: If we just used an array for this, we could just rewrite and
    // re-sort and not have to deal with potential allocations.
    decltype(regionToTransferredOpMap) oldMap =
        std::move(regionToTransferredOpMap);
    for (auto &[oldReg, op] : oldMap) {
      auto iter = oldRegionToRelabeledMap.find(oldReg);
      assert(iter != oldRegionToRelabeledMap.end());
      regionToTransferredOpMap[iter->second] = op;
    }

    assert(is_canonical_correct());
  }

  /// For the passed `map`, ensure that `key` maps to `val`. If `key` already
  /// mapped to a different value, ensure that all other keys mapped to that
  /// value also now map to `val`. This is a relatively expensive (linear time)
  /// operation that's unfortunately used pervasively throughout PartitionOp
  /// application. If this is a performance bottleneck, let's consider
  /// optimizing it to a true union-find or other tree-based data structure.
  static void horizontalUpdate(std::map<Element, Region> &map, Element key,
                               Region val) {
    if (!map.count(key)) {
      map.insert({key, val});
      return;
    }

    Region oldVal = map.at(key);
    if (val == oldVal)
      return;

    for (auto [otherKey, otherVal] : map)
      if (otherVal == oldVal)
        map.insert_or_assign(otherKey, val);
  }
};

/// A data structure that applies a series of PartitionOps to a single Partition
/// that it modifies.
///
/// Callers use CRTP to modify its behavior. Please see the definition below of
/// a "blank" subclass PartitionOpEvaluatorBaseImpl for a description of the
/// methods needing to be implemented by other CRTP subclasses.
template <typename Impl>
struct PartitionOpEvaluator {
private:
  Impl &asImpl() { return *reinterpret_cast<Impl *>(this); }
  const Impl &asImpl() const { return *reinterpret_cast<const Impl *>(this); }

public:
  using Element = PartitionPrimitives::Element;
  using Region = PartitionPrimitives::Region;
  using TransferringOperandSetFactory =
      Partition::TransferringOperandSetFactory;

protected:
  TransferringOperandSetFactory &ptrSetFactory;

  Partition &p;

public:
  PartitionOpEvaluator(Partition &p,
                       TransferringOperandSetFactory &ptrSetFactory)
      : ptrSetFactory(ptrSetFactory), p(p) {}

  /// Call shouldEmitVerboseLogging on our CRTP subclass.
  bool shouldEmitVerboseLogging() const {
    return asImpl().shouldEmitVerboseLogging();
  }

  /// Call handleLocalUseAfterTransfer on our CRTP subclass.
  void handleLocalUseAfterTransfer(const PartitionOp &op, Element elt,
                                   TransferringOperand *transferringOp) const {
    return asImpl().handleLocalUseAfterTransfer(op, elt, transferringOp);
  }

  /// Call handleTransferNonTransferrable on our CRTP subclass.
  void
  handleTransferNonTransferrable(const PartitionOp &op, Element elt,
                                 SILIsolationInfo isolationRegionInfo) const {
    return asImpl().handleTransferNonTransferrable(op, elt,
                                                   isolationRegionInfo);
  }
  /// Just call our CRTP subclass.
  void
  handleTransferNonTransferrable(const PartitionOp &op, Element elt,
                                 Element otherElement,
                                 SILIsolationInfo isolationRegionInfo) const {
    return asImpl().handleTransferNonTransferrable(op, elt, otherElement,
                                                   isolationRegionInfo);
  }

  /// Call isActorDerived on our CRTP subclass.
  bool isActorDerived(Element elt) const {
    return asImpl().isActorDerived(elt);
  }

  SILIsolationInfo getIsolationRegionInfo(Element elt) const {
    return asImpl().getIsolationRegionInfo(elt);
  }

  /// Compute the isolation region info for all elements in \p region.
  ///
  /// The bool result is if it is captured by a closure element. That only is
  /// computed if \p sourceOp is non-null.
  std::pair<SILIsolationInfo, bool>
  getIsolationRegionInfo(Region region, Operand *sourceOp) const {
    bool isClosureCapturedElt = false;
    SILIsolationInfo isolationRegionInfo;

    for (const auto &pair : p.range()) {
      if (pair.second == region) {
        isolationRegionInfo =
            isolationRegionInfo.merge(getIsolationRegionInfo(pair.first));
        if (sourceOp)
          isClosureCapturedElt |= isClosureCaptured(pair.first, sourceOp);
      }
    }

    return {isolationRegionInfo, isClosureCapturedElt};
  }

  /// Overload of \p getIsolationRegionInfo without an Operand.
  SILIsolationInfo getIsolationRegionInfo(Region region) const {
    return getIsolationRegionInfo(region, nullptr).first;
  }

  bool isTaskIsolatedDerived(Element elt) const {
    return asImpl().isTaskIsolatedDerived(elt);
  }

  /// Call isClosureCaptured on our CRTP subclass.
  bool isClosureCaptured(Element elt, Operand *op) const {
    return asImpl().isClosureCaptured(elt, op);
  }

  /// Apply \p op to the partition op.
  void apply(const PartitionOp &op) const {
    if (shouldEmitVerboseLogging()) {
      REGIONBASEDISOLATION_VERBOSE_LOG(llvm::dbgs() << "Applying: ";
                                       op.print(llvm::dbgs()));
      REGIONBASEDISOLATION_VERBOSE_LOG(llvm::dbgs() << "    Before: ";
                                       p.print(llvm::dbgs()));
    }
    SWIFT_DEFER {
      if (shouldEmitVerboseLogging()) {
        REGIONBASEDISOLATION_VERBOSE_LOG(llvm::dbgs() << "    After:  ";
                                         p.print(llvm::dbgs()));
      }
      assert(p.is_canonical_correct());
    };

    switch (op.getKind()) {
    case PartitionOpKind::Assign:
      assert(op.getOpArgs().size() == 2 &&
             "Assign PartitionOp should be passed 2 arguments");
      assert(p.isTrackingElement(op.getOpArgs()[1]) &&
             "Assign PartitionOp's source argument should be already tracked");
      // If we are using a region that was transferred as our assignment source
      // value... emit an error.
      if (auto *transferredOperandSet = p.getTransferred(op.getOpArgs()[1])) {
        for (auto transferredOperand : transferredOperandSet->data()) {
          handleLocalUseAfterTransfer(op, op.getOpArgs()[1],
                                      transferredOperand);
        }
      }
      p.assignElement(op.getOpArgs()[0], op.getOpArgs()[1]);
      return;
    case PartitionOpKind::AssignFresh:
      assert(op.getOpArgs().size() == 1 &&
             "AssignFresh PartitionOp should be passed 1 argument");

      p.trackNewElement(op.getOpArgs()[0]);
      return;
    case PartitionOpKind::Transfer: {
      // NOTE: We purposely do not check here if a transferred value is already
      // transferred. Callers are expected to put a require for that
      // purpose. This ensures that if we pass the same argument multiple times
      // to the same transferring function as weakly transferred arguments, we
      // do not get an error.
      assert(op.getOpArgs().size() == 1 &&
             "Transfer PartitionOp should be passed 1 argument");
      assert(p.isTrackingElement(op.getOpArgs()[0]) &&
             "Transfer PartitionOp's argument should already be tracked");

      // Otherwise, we need to merge our isolation region info with the
      // isolation region info of everything else in our region. This is the
      // dynamic isolation region info found by the dataflow.
      Element transferredElement = op.getOpArgs()[0];
      Region transferredRegion = p.getRegion(transferredElement);
      bool isClosureCapturedElt = false;
      SILIsolationInfo transferredRegionIsolation;
      std::tie(transferredRegionIsolation, isClosureCapturedElt) =
          getIsolationRegionInfo(transferredRegion, op.getSourceOp());

      // If we merged anything, we need to handle a transfer
      // non-transferrable. We pass in the dynamic isolation region info of our
      // region.
      if (bool(transferredRegionIsolation) &&
          !transferredRegionIsolation.isDisconnected()) {
        return handleTransferNonTransferrable(op, op.getOpArgs()[0],
                                              transferredRegionIsolation);
      }

      // Mark op.getOpArgs()[0] as transferred.
      auto *ptrSet =
          ptrSetFactory.emplace(op.getSourceOp(), isClosureCapturedElt);
      p.markTransferred(op.getOpArgs()[0], ptrSet);
      return;
    }
    case PartitionOpKind::UndoTransfer: {
      assert(op.getOpArgs().size() == 1 &&
             "UndoTransfer PartitionOp should be passed 1 argument");
      assert(p.isTrackingElement(op.getOpArgs()[0]) &&
             "UndoTransfer PartitionOp's argument should already be tracked");

      // Mark op.getOpArgs()[0] as not transferred.
      p.undoTransfer(op.getOpArgs()[0]);
      return;
    }
    case PartitionOpKind::Merge:
      assert(op.getOpArgs().size() == 2 &&
             "Merge PartitionOp should be passed 2 arguments");
      assert(p.isTrackingElement(op.getOpArgs()[0]) &&
             p.isTrackingElement(op.getOpArgs()[1]) &&
             "Merge PartitionOp's arguments should already be tracked");

      // if attempting to merge a transferred region, handle the failure
      if (auto *transferredOperandSet = p.getTransferred(op.getOpArgs()[0])) {
        for (auto transferredOperand : transferredOperandSet->data()) {
          handleLocalUseAfterTransfer(op, op.getOpArgs()[0],
                                      transferredOperand);
        }
      }
      if (auto *transferredOperandSet = p.getTransferred(op.getOpArgs()[1])) {
        for (auto transferredOperand : transferredOperandSet->data()) {
          handleLocalUseAfterTransfer(op, op.getOpArgs()[1],
                                      transferredOperand);
        }
      }

      p.merge(op.getOpArgs()[0], op.getOpArgs()[1]);
      return;
    case PartitionOpKind::Require:
      assert(op.getOpArgs().size() == 1 &&
             "Require PartitionOp should be passed 1 argument");
      assert(p.isTrackingElement(op.getOpArgs()[0]) &&
             "Require PartitionOp's argument should already be tracked");
      if (auto *transferredOperandSet = p.getTransferred(op.getOpArgs()[0])) {
        for (auto transferredOperand : transferredOperandSet->data()) {
          handleLocalUseAfterTransfer(op, op.getOpArgs()[0],
                                      transferredOperand);
        }
      }
      return;
    }

    llvm_unreachable("Covered switch isn't covered?!");
  }

  void apply(std::initializer_list<PartitionOp> ops) {
    for (auto &o : ops)
      apply(o);
  }
};

/// A base implementation that can be used to default initialize CRTP
/// subclasses. Only used to implement base functionality for subclass
/// CRTPs. For true basic evaluation, use PartitionOpEvaluatorBasic below.
template <typename Subclass>
struct PartitionOpEvaluatorBaseImpl : PartitionOpEvaluator<Subclass> {
  using Element = PartitionPrimitives::Element;
  using Region = PartitionPrimitives::Region;
  using TransferringOperandSetFactory =
      Partition::TransferringOperandSetFactory;
  using Super = PartitionOpEvaluator<Subclass>;

  PartitionOpEvaluatorBaseImpl(Partition &workingPartition,
                               TransferringOperandSetFactory &ptrSetFactory)
      : Super(workingPartition, ptrSetFactory) {}

  /// Should we emit extra verbose logging statements when evaluating
  /// PartitionOps.
  bool shouldEmitVerboseLogging() const { return true; }

  /// A function called if we discover a transferred value was used after it
  /// was transferred.
  ///
  /// The arguments passed to the closure are:
  ///
  /// 1. The PartitionOp that required the element to be alive.
  ///
  /// 2. The element in the PartitionOp that was asked to be alive.
  ///
  /// 3. The operand of the instruction that originally transferred the
  /// region. Can be used to get the immediate value transferred or the
  /// transferring instruction.
  void handleLocalUseAfterTransfer(const PartitionOp &op, Element elt,
                                   TransferringOperand *transferringOp) const {}

  /// This is called if we detect a never transferred element that was passed to
  /// a transfer instruction.
  void handleTransferNonTransferrable(const PartitionOp &op, Element elt,
                                      SILIsolationInfo regionInfo) const {}

  void
  handleTransferNonTransferrable(const PartitionOp &op, Element elt,
                                 Element otherElement,
                                 SILIsolationInfo isolationRegionInfo) const {}

  /// This is used to determine if an element is actor derived. If we determine
  /// that a region containing such an element is transferred, we emit an error
  /// since actor regions cannot be transferred.
  bool isActorDerived(Element elt) const { return false; }

  /// This is used to determine if an element is in the same region as a task
  /// isolated value.
  bool isTaskIsolatedDerived(Element elt) const { return false; }

  /// Returns the information about \p elt's isolation that we ascertained from
  /// SIL and the AST.
  SILIsolationInfo getIsolationRegionInfo(Element elt) const {
    return SILIsolationInfo();
  }

  /// Check if the representative value of \p elt is closure captured at \p
  /// op.
  ///
  /// NOTE: We actually just use the user of \p op in our callbacks. The reason
  /// why we do not just pass in that SILInstruction is that then we would need
  /// to access the instruction in the evaluator which creates a problem when
  /// since the operand we pass in is a dummy operand.
  bool isClosureCaptured(Element elt, Operand *op) const { return false; }
};

/// A subclass of PartitionOpEvaluatorBaseImpl that doesn't have any special
/// behavior.
struct PartitionOpEvaluatorBasic final
    : PartitionOpEvaluatorBaseImpl<PartitionOpEvaluatorBasic> {
  PartitionOpEvaluatorBasic(Partition &workingPartition,
                            TransferringOperandSetFactory &ptrSetFactory)
      : PartitionOpEvaluatorBaseImpl(workingPartition, ptrSetFactory) {}
};

} // namespace swift

#endif // SWIFT_PARTITIONUTILS_H
