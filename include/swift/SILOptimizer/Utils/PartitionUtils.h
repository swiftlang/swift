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
#include "swift/SIL/SILFunction.h"
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

  void print(llvm::raw_ostream &os) const;

  SWIFT_DEBUG_DUMP {
    print(llvm::dbgs());
    llvm::dbgs() << '\n';
  }

  void printForDiagnostics(llvm::raw_ostream &os) const;

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
  NominalTypeDecl *tryInferActorDecl() const;

  [[nodiscard]] SILIsolationInfo merge(SILIsolationInfo other) const;

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
    return SILIsolationInfo();
  }

  static SILIsolationInfo getGlobalActorIsolated(Type globalActorType) {
    return getActorIsolated(ActorIsolation::forGlobalActor(globalActorType));
  }

  static SILIsolationInfo getTaskIsolated(SILValue value) {
    return {Kind::Task, value};
  }

  /// Attempt to infer the isolation region info for \p inst.
  static SILIsolationInfo get(SILInstruction *inst);

  /// Attempt to infer the isolation region info for \p arg.
  static SILIsolationInfo get(SILFunctionArgument *arg);

  bool operator==(const SILIsolationInfo &other) const;

  void Profile(llvm::FoldingSetNodeID &id) const;
};

} // namespace swift

namespace swift {

class TransferringOperand {
  using ValueType = llvm::PointerIntPair<Operand *, 1>;
  ValueType value;

  /// The dynamic isolation info of the region of value when we transferred.
  SILIsolationInfo isolationInfo;

  TransferringOperand(ValueType newValue, SILIsolationInfo isolationRegionInfo)
      : value(newValue), isolationInfo(isolationRegionInfo) {
    assert(isolationInfo && "Should never see unknown isolation info");
  }

public:
  TransferringOperand(Operand *op, bool isClosureCaptured,
                      SILIsolationInfo isolationRegionInfo)
      : TransferringOperand({op, isClosureCaptured}, isolationRegionInfo) {}
  explicit TransferringOperand(Operand *op,
                               SILIsolationInfo isolationRegionInfo)
      : TransferringOperand({op, false}, isolationRegionInfo) {}

  operator bool() const { return bool(value.getPointer()); }

  Operand *getOperand() const { return value.getPointer(); }

  SILValue get() const { return getOperand()->get(); }

  bool isClosureCaptured() const { return value.getInt(); }

  SILInstruction *getUser() const { return getOperand()->getUser(); }

  SILIsolationInfo getIsolationInfo() const { return isolationInfo; }

  unsigned getOperandNumber() const { return getOperand()->getOperandNumber(); }

  void print(llvm::raw_ostream &os) const {
    os << "Op Num: " << getOperand()->getOperandNumber() << ". "
       << "Capture: " << (isClosureCaptured() ? "yes. " : "no.  ")
       << "IsolationInfo: ";
    isolationInfo.print(os);
    os << "\nUser: " << *getUser();
  }

  static void Profile(llvm::FoldingSetNodeID &id, Operand *op,
                      bool isClosureCaptured,
                      SILIsolationInfo isolationRegionInfo) {
    id.AddPointer(op);
    id.AddBoolean(isClosureCaptured);
    isolationRegionInfo.Profile(id);
  }

  void Profile(llvm::FoldingSetNodeID &id) const {
    Profile(id, getOperand(), isClosureCaptured(), isolationInfo);
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

  void print(llvm::raw_ostream &os, bool extraSpace = false) const;

  SWIFT_DEBUG_DUMP { print(llvm::dbgs()); }
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

  /// Return a new Partition that has a single region containing the elements of
  /// \p indices.
  static Partition singleRegion(ArrayRef<Element> indices);

  /// Return a new Partition that has each element of \p indices in their own
  /// region.
  static Partition separateRegions(ArrayRef<Element> indices);

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
                       TransferringOperandSet *transferredOperandSet);

  /// If val was marked as transferred, unmark it as transfer. Returns true if
  /// we found that \p val was transferred. We return false otherwise.
  bool undoTransfer(Element val);

  /// If \p newElt is not being tracked, create a new region for \p newElt. If
  /// \p newElt is already being tracked, remove it from its old region as well.
  void trackNewElement(Element newElt);

  /// Assigns \p oldElt to the region associated with \p newElt.
  void assignElement(Element oldElt, Element newElt);

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
  static Partition join(const Partition &fst, const Partition &snd);

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

  void print(llvm::raw_ostream &os) const;

  SWIFT_DEBUG_DUMPER(dumpVerbose()) { printVerbose(llvm::dbgs()); }

  void printVerbose(llvm::raw_ostream &os) const;

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
  bool is_canonical_correct() const;

  /// Merge the regions of two indices while maintaining canonicality. Returns
  /// the final region used.
  ///
  /// This runs in linear time.
  Region merge(Element fst, Element snd);

private:
  /// For each region label that occurs, find the first index at which it occurs
  /// and relabel all instances of it to that index.  This excludes the -1 label
  /// for transferred regions.
  ///
  /// This runs in linear time.
  void canonicalize();

  /// For the passed `map`, ensure that `key` maps to `val`. If `key` already
  /// mapped to a different value, ensure that all other keys mapped to that
  /// value also now map to `val`. This is a relatively expensive (linear time)
  /// operation that's unfortunately used pervasively throughout PartitionOp
  /// application. If this is a performance bottleneck, let's consider
  /// optimizing it to a true union-find or other tree-based data structure.
  static void horizontalUpdate(std::map<Element, Region> &map, Element key,
                               Region val);
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
          handleLocalUseAfterTransferHelper(op, op.getOpArgs()[1],
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

      // Before we do anything, see if our dynamic isolation kind is the same as
      // the isolation info for our partition op. If they match, this is not a
      // real transfer operation.
      //
      // DISCUSSION: We couldn't not emit this earlier since we needed the
      // dynamic isolation info of our value.
      if (transferredRegionIsolation.isActorIsolated()) {
        if (auto calleeIsolationInfo =
                SILIsolationInfo::get(op.getSourceInst())) {
          if (transferredRegionIsolation == calleeIsolationInfo) {
            return;
          }
        }
      }

      // If we merged anything, we need to handle a transfer
      // non-transferrable. We pass in the dynamic isolation region info of our
      // region.
      if (bool(transferredRegionIsolation) &&
          !transferredRegionIsolation.isDisconnected()) {
        return handleTransferNonTransferrable(op, op.getOpArgs()[0],
                                              transferredRegionIsolation);
      }

      // Mark op.getOpArgs()[0] as transferred.
      auto *ptrSet = ptrSetFactory.emplace(
          op.getSourceOp(), isClosureCapturedElt, transferredRegionIsolation);
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
          handleLocalUseAfterTransferHelper(op, op.getOpArgs()[0],
                                            transferredOperand);
        }
      }
      if (auto *transferredOperandSet = p.getTransferred(op.getOpArgs()[1])) {
        for (auto transferredOperand : transferredOperandSet->data()) {
          handleLocalUseAfterTransferHelper(op, op.getOpArgs()[1],
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
          handleLocalUseAfterTransferHelper(op, op.getOpArgs()[0],
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

  /// Provides a way for subclasses to disable the error squelching
  /// functionality.
  ///
  /// Used by the unittests.
  bool shouldTryToSquelchErrors() const {
    return asImpl().shouldTryToSquelchErrors();
  }

private:
  // Private helper that squelches the error if our transfer instruction and our
  // use have the same isolation.
  void
  handleLocalUseAfterTransferHelper(const PartitionOp &op, Element elt,
                                    TransferringOperand *transferringOp) const {
    if (shouldTryToSquelchErrors()) {
      if (auto isolationInfo = SILIsolationInfo::get(op.getSourceInst())) {
        if (isolationInfo.isActorIsolated() &&
            isolationInfo == SILIsolationInfo::get(transferringOp->getUser()))
          return;
      }

      // If our instruction does not have any isolation info associated with it,
      // it must be nonisolated. See if our function has a matching isolation to
      // our transferring operand. If so, we can squelch this.
      if (auto functionIsolation =
              transferringOp->getUser()->getFunction()->getActorIsolation()) {
        if (functionIsolation.isActorIsolated() &&
            SILIsolationInfo::getActorIsolated(functionIsolation) ==
                SILIsolationInfo::get(transferringOp->getUser()))
          return;
      }
    }

    // Ok, we actually need to emit a call to the callback.
    return handleLocalUseAfterTransfer(op, elt, transferringOp);
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

  /// By default squelch errors.
  bool shouldTryToSquelchErrors() const { return true; }
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
