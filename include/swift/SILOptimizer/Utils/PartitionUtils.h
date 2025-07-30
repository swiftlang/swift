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
#include "swift/SILOptimizer/Utils/InstOptUtils.h"
#include "swift/SILOptimizer/Utils/RegionIsolation.h"
#include "swift/SILOptimizer/Utils/SILIsolationInfo.h"

#include "llvm/ADT/MapVector.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Support/Debug.h"

#include <algorithm>
#include <variant>

#define DEBUG_TYPE "send-non-sendable"

namespace swift {

namespace PartitionPrimitives {

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

class Partition;
class SendingOperandToStateMap;
class RegionAnalysisValueMap;

/// The representative value of the equivalence class that makes up a tracked
/// value.
///
/// We use a wrapper struct here so that we can inject "fake" actor-isolated
/// values into the regions of values that become merged into an actor by
/// calling a function without a non-sendable result.
class RepresentativeValue {
  friend llvm::DenseMapInfo<RepresentativeValue>;

  using InnerType = PointerUnion<SILValue, SILInstruction *>;

  /// If this is set to a SILValue then it is the actual represented value. If
  /// it is set to a SILInstruction, then this is a "fake" representative value
  /// used to inject actor isolation. The instruction stored is the
  /// instruction that introduced the actor isolation.
  InnerType value;

public:
  RepresentativeValue() : value() {}
  RepresentativeValue(SILValue value) : value(value) {}
  RepresentativeValue(SILInstruction *actorRegionInst)
      : value(actorRegionInst) {}

  operator bool() const { return bool(value); }

  void print(llvm::raw_ostream &os) const {
    if (auto *inst = value.dyn_cast<SILInstruction *>()) {
      os << "ActorRegionIntroducingInst: " << *inst;
      return;
    }

    os << *cast<SILValue>(value);
  }

  SILValue getValue() const { return cast<SILValue>(value); }
  SILValue maybeGetValue() const { return value.dyn_cast<SILValue>(); }
  bool hasRegionIntroducingInst() const { return isa<SILInstruction *>(value); }
  SILInstruction *getActorRegionIntroducingInst() const {
    return cast<SILInstruction *>(value);
  }

  bool operator==(SILValue other) const {
    if (hasRegionIntroducingInst())
      return false;
    return getValue() == other;
  }

  SWIFT_DEBUG_DUMP { print(llvm::dbgs()); }

private:
  RepresentativeValue(InnerType value) : value(value) {}
};

/// A persistent data structure that is used to "rewind" partition history so
/// that we can discover when values become part of the same region.
///
/// NOTE: This does not track whether or not values are sent. This is
/// because from the perspective of determining when two values become part of
/// the same region, that information is not important. To unroll history, a
/// Partition must have no sends to use this. NOTE: There is a method that
/// takes a Partition and produces a new Partition that does not have any
/// sends.
class IsolationHistory {
public:
  class Factory;

private:
  using Element = PartitionPrimitives::Element;
  using Region = PartitionPrimitives::Region;
  class Node;

  // TODO: This shouldn't need to be a friend.
  friend class Partition;
  friend SendingOperandToStateMap;

  /// First node in the immutable linked list.
  Node *head = nullptr;
  Factory *factory = nullptr;

  IsolationHistory(Factory *factory) : head(nullptr), factory(factory) {}

public:
  IsolationHistory(const IsolationHistory &otherIsolation)
      : head(otherIsolation.head), factory(otherIsolation.factory) {}

  IsolationHistory &operator=(const IsolationHistory &otherIsolation) {
    assert(factory == otherIsolation.factory);
    head = otherIsolation.head;
    return *this;
  }

  Node *getHead() const { return head; }

  /// Push a node that signals the end of a new sequence of history nodes that
  /// should execute together. Must be explicitly ended by a push sequence
  /// end. Is non-rentrant, so one cannot have multiple sequence starts.
  ///
  /// \p loc the SILLocation that identifies the instruction that the "package"
  /// of history nodes that this sequence boundary ends is associated with.
  Node *pushHistorySequenceBoundary(SILLocation loc);

  /// Push onto the history list that \p value should be added into its own
  /// independent region.
  Node *pushNewElementRegion(Element element);

  /// Push onto the history that \p value should be removed from a region and
  /// that element is the last element in that region (so the region is empty
  /// afterwards).
  void pushRemoveLastElementFromRegion(Element element);

  /// Push onto the history that \p element should be removed from a region that
  /// contains \p otherElementInOldRegion.
  void pushRemoveElementFromRegion(Element otherElementInOldRegion,
                                   Element element);

  /// \p elementToMergeInto is the element whose region we merge \p otherRegions
  /// into.
  void pushMergeElementRegions(Element elementToMergeInto,
                               ArrayRef<Element> otherRegions);

  /// Assign \p elementToMerge's region to \p elementToMergeInto's region.
  void pushAssignElementRegions(Element elementToMergeInto,
                                Element elementToMerge);

  /// Push that \p other should be merged into this region.
  void pushCFGHistoryJoin(Node *otherNode);

  /// Push the top node of \p history as a CFG history join.
  void pushCFGHistoryJoin(IsolationHistory history) {
    return pushCFGHistoryJoin(history.getHead());
  }

  Node *pop();
};

class IsolationHistory::Node final
    : private llvm::TrailingObjects<IsolationHistory::Node, Element> {
  friend IsolationHistory;
  friend TrailingObjects;

public:
  enum Kind {
    /// Add a new element to its own region. The region will only consist of
    /// element.
    AddNewRegionForElement,

    /// Remove an element from a region which it is the only element of.
    RemoveLastElementFromRegion,

    /// Remove an element from a region which still has elements remaining.
    ///
    /// This is different from RemoveLastElementFromRegion since we store the
    /// other element.
    RemoveElementFromRegion,

    /// Given two elements, data and otherData, merge otherData into data's
    /// region.
    MergeElementRegions,

    /// At a CFG merge point, we merged two histories. We need to visit it
    /// recursively.
    CFGHistoryJoin,

    /// Signals that a sequence boundary has been found in the history and if we
    /// are processing a sequence, should stop processing.
    ///
    /// Clients may want to ensure that a set of history elements are pushed or
    /// popped together since the effects happen at the same time.
    /// HistorySequenceStart
    /// signifies that.
    SequenceBoundary,
  };

private:
  Kind kind;
  Node *parent;

  /// Child node. Never set on construction.
  Node *child = nullptr;

  /// Contains:
  ///
  /// 1. Node * if we have a CFGHistoryJoin.
  /// 2. A SILLocation if we have a SequenceBoundary.
  /// 3. An element otherwise.
  std::variant<Element, Node *, SILLocation> subject;

  /// Number of additional element arguments stored in the tail allocated array.
  unsigned numAdditionalElements;

  /// Access the tail allocated buffer of additional element arguments.
  MutableArrayRef<Element> getAdditionalElementArgs() {
    return {getTrailingObjects<Element>(), numAdditionalElements};
  }

  Node(Kind kind, Node *parent)
      : kind(kind), parent(parent), subject(nullptr) {}
  Node(Kind kind, Node *parent, SILLocation loc)
      : kind(kind), parent(parent), subject(loc) {}
  Node(Kind kind, Node *parent, Element value)
      : kind(kind), parent(parent), subject(value), numAdditionalElements(0) {}
  Node(Kind kind, Node *parent, Element primaryElement,
       std::initializer_list<Element> restOfTheElements)
      : kind(kind), parent(parent), subject(primaryElement),
        numAdditionalElements(restOfTheElements.size()) {
    unsigned writeIndex = 0;
    for (Element restElt : restOfTheElements) {
      if (primaryElement == restElt) {
        continue;
      }

      getAdditionalElementArgs()[writeIndex] = restElt;
      ++writeIndex;
    }

    // Set writeIndex to n - 1.
    numAdditionalElements = writeIndex;
  }

  Node(Kind kind, Node *parent, Element lhsValue, ArrayRef<Element> rhsValue)
      : kind(kind), parent(parent), subject(lhsValue),
        numAdditionalElements(rhsValue.size()) {
    std::uninitialized_copy(rhsValue.begin(), rhsValue.end(),
                            getAdditionalElementArgs().data());
  }

  Node(Kind kind, Node *parent, Node *node)
      : kind(kind), parent(parent), subject(node), numAdditionalElements(0) {}

public:
  Kind getKind() const { return kind; }

  Node *getParent() const { return parent; }

  Node *getChild() const { return child; }
  void setChild(Node *newChild) { child = newChild; }

  Element getFirstArgAsElement() const {
    assert(kind != CFGHistoryJoin);
    assert(std::holds_alternative<Element>(subject));
    return std::get<Element>(subject);
  }

  Node *getFirstArgAsNode() const {
    assert(kind == CFGHistoryJoin);
    assert(std::holds_alternative<Node *>(subject));
    return std::get<Node *>(subject);
  }

  ArrayRef<Element> getAdditionalElementArgs() const {
    assert(kind == MergeElementRegions || kind == RemoveElementFromRegion);
    return const_cast<Node *>(this)->getAdditionalElementArgs();
  }

  bool isHistorySequenceBoundary() const {
    return getKind() == SequenceBoundary;
  }

  /// If this node is a history sequence join, return its node. Otherwise,
  /// return nullptr.
  Node *getHistorySequenceJoin() const {
    if (kind != CFGHistoryJoin)
      return nullptr;
    return getFirstArgAsNode();
  }

  std::optional<SILLocation> getHistoryBoundaryLoc() const {
    if (kind != SequenceBoundary)
      return {};
    return std::get<SILLocation>(subject);
  }
};

class IsolationHistory::Factory {
  friend IsolationHistory;
  using Node = IsolationHistory::Node;

  llvm::BumpPtrAllocator &allocator;

public:
  Factory(llvm::BumpPtrAllocator &allocator) : allocator(allocator) {}

  Factory(IsolationHistory::Factory &&other) = delete;
  Factory &operator=(IsolationHistory::Factory &&other) = delete;
  Factory(const IsolationHistory::Factory &other) = delete;
  Factory &operator=(const IsolationHistory::Factory &other) = delete;

  /// Returns a new isolation history without any history.
  IsolationHistory get() { return IsolationHistory(this); }
};

/// A struct that represents a specific "sending" operand of an ApplySite.
struct SendingOperandState {
  /// The dynamic isolation info of the region of value when we sent.
  ///
  /// This will contain the isolated value if we found one.
  SILDynamicMergedIsolationInfo isolationInfo;

  /// The dynamic isolation history at this point.
  IsolationHistory isolationHistory;

  /// Set to true if the element associated with the operand's value is closure
  /// captured by the user. In such a case, if our element is a sendable var of
  /// a non-Sendable type, we cannot access it since we could race against an
  /// assignment to the var in a closure.
  bool isClosureCaptured;

  SendingOperandState(IsolationHistory history)
      : isolationInfo(), isolationHistory(history), isClosureCaptured(false) {}
};

class SendingOperandToStateMap {
  llvm::SmallDenseMap<Operand *, SendingOperandState> internalMap;
  IsolationHistory::Factory &isolationHistoryFactory;

public:
  SendingOperandToStateMap(IsolationHistory::Factory &isolationHistoryFactory)
      : isolationHistoryFactory(isolationHistoryFactory) {}
  SendingOperandState &get(Operand *op) const {
    auto *self = const_cast<SendingOperandToStateMap *>(this);
    auto history = IsolationHistory(&isolationHistoryFactory);
    return self->internalMap.try_emplace(op, SendingOperandState(history))
        .first->getSecond();
  }
};

} // namespace swift

namespace swift {

/// PartitionOpKind represents the different kinds of PartitionOps that
/// SILInstructions can be translated to
enum class PartitionOpKind : uint8_t {
  /// Assign one value to the region of another, takes two args, second arg
  /// must already be tracked with a non-sent region
  Assign,

  /// Assign one value to a fresh region, takes one arg.
  ///
  /// NOTE: This just produces a new value that is tracked by the dataflow. The
  /// isolation characteristics of the value are actually decided by
  /// tryToTrackValue and SILIsolationInfo::get().
  AssignFresh,

  /// Assign one value to a fresh region and then assign it to another value
  /// that was also just assign fresh. Takes two parameters. The first is the
  /// element to assign fresh and the second is the element to assign to.
  ///
  /// Used in combination with AssignFresh to initialize a chain of
  /// values. Different from Assign since Assign allows for errors to
  /// occur. This does not allow for any errors to be emitted since we are just
  /// initializing a chain of values.
  AssignFreshAssign,

  /// Merge the regions of two values, takes two args, both must be from
  /// non-sent regions.
  Merge,

  /// Send the region of a value if not already sent, takes one arg.
  Send,

  /// Due to an async let or something like that a value that was sent is
  /// no longer sent.
  UndoSend,

  /// Require the region of a value to be non-sent, takes one arg.
  Require,

  /// Emit an error saying that the given instruction was not understood for
  /// some reason and that a bug should be filed. It expects some sort of
  /// Element number since in most cases we need to define a value for later
  /// potential uses of the value (e.x.: an alloc_stack that we emit an unknown
  /// pattern error will have later uses that will use the value... without
  /// defining the value, the dataflow will assert).
  ///
  /// This is used if we need to reject the program and do not want to assert.
  UnknownPatternError,

  /// Require that a 'inout sending' parameter's region is not sent and
  /// disconnected at a specific function exiting term inst.
  ///
  /// This ensures that if users send away an inout sending parameter, the
  /// parameter is reinitialized with a disconnected value.
  ///
  /// Takes one parameter, the inout parameter that we need to check.
  InOutSendingAtFunctionExit,

  /// This is the result of an isolation crossing apply site. We need to emit a
  /// special error since we never allow this.
  ///
  /// DISCUSSION: This is actually just a form of "send". Sadly, we can not use
  /// "send" directly since "send" expects a SILOperand and these are values. So
  /// to work around the API issue, we have to use a different, specific entry.
  NonSendableIsolationCrossingResult,
};

/// PartitionOp represents a primitive operation that can be performed on
/// Partitions. This is part of the SendNonSendable SIL pass workflow:
/// first SILBasicBlocks are compiled to vectors of PartitionOps, then a fixed
/// point partition is found over the CFG.
class PartitionOp {
  using Element = PartitionPrimitives::Element;

public:
  enum class Flag : uint8_t {
    None,

    /// This is a require of a non-Sendable base that we have a Sendable use
    /// from. If the region was sent but at the sent point did not have any
    /// element of the region that was captured by reference in a closure, we
    /// can ignore the use.
    RequireOfMutableBaseOfSendableValue,
  };
  using Options = OptionSet<Flag>;

private:
  /// Record the SILInstruction that this PartitionOp was generated from, if
  /// generated during compilation from a SILBasicBlock
  PointerUnion<SILInstruction *, Operand *> source;

  std::optional<Element> opArg1;
  std::optional<Element> opArg2;

  PartitionOpKind opKind;

  Options options;

  // TODO: can the following declarations be merged?
  PartitionOp(PartitionOpKind opKind, Element arg1,
              SILInstruction *sourceInst = nullptr, Options options = {})
      : source(sourceInst), opArg1(arg1), opKind(opKind), options(options) {
    assert(((opKind != PartitionOpKind::Send &&
             opKind != PartitionOpKind::UndoSend) ||
            sourceInst) &&
           "Send needs a sourceInst");
  }

  PartitionOp(PartitionOpKind opKind, Element arg1, Operand *sourceOperand,
              Options options = {})
      : source(sourceOperand), opArg1(arg1), opKind(opKind), options(options) {
    assert(((opKind != PartitionOpKind::Send &&
             opKind != PartitionOpKind::UndoSend) ||
            bool(sourceOperand)) &&
           "Send needs a sourceInst");
  }

  PartitionOp(PartitionOpKind opKind, Element arg1, Element arg2,
              SILInstruction *sourceInst = nullptr, Options options = {})
      : source(sourceInst), opArg1(arg1), opArg2(arg2), opKind(opKind),
        options(options) {
    assert(((opKind != PartitionOpKind::Send &&
             opKind != PartitionOpKind::UndoSend) ||
            sourceInst) &&
           "Send needs a sourceInst");
  }

  PartitionOp(PartitionOpKind opKind, Element arg1, Element arg2,
              Operand *sourceOp = nullptr, Options options = {})
      : source(sourceOp), opArg1(arg1), opArg2(arg2), opKind(opKind),
        options(options) {
    assert((opKind == PartitionOpKind::Assign ||
            opKind == PartitionOpKind::Merge) &&
           "Only supported for assign and merge");
  }

  PartitionOp(PartitionOpKind opKind, SILInstruction *sourceInst,
              Options options = {})
      : source(sourceInst), opKind(opKind), options(options) {}

  friend class Partition;

public:
  static PartitionOp Assign(Element destElt, Element srcElt,
                            Operand *srcOperand = nullptr) {
    return PartitionOp(PartitionOpKind::Assign, destElt, srcElt, srcOperand);
  }

  static PartitionOp AssignFresh(Element elt,
                                 SILInstruction *sourceInst = nullptr) {
    return PartitionOp(PartitionOpKind::AssignFresh, elt, sourceInst);
  }

  /// Assign fresh \p elt and then assign it to \p srcElt.
  ///
  /// Used as part of emitting a sequence of AssignFresh that join the same
  /// element.
  static PartitionOp AssignFreshAssign(Element elt, Element srcElt,
                                       SILInstruction *sourceInst = nullptr) {
    return PartitionOp(PartitionOpKind::AssignFreshAssign, elt, srcElt,
                       sourceInst);
  }

  static PartitionOp Send(Element tgt, Operand *sendingOp) {
    return PartitionOp(PartitionOpKind::Send, tgt, sendingOp);
  }

  static PartitionOp UndoSend(Element tgt, SILInstruction *unsendingInst) {
    return PartitionOp(PartitionOpKind::UndoSend, tgt, unsendingInst);
  }

  static PartitionOp Merge(Element destElement, Element srcElement,
                           Operand *sourceOperand = nullptr) {
    return PartitionOp(PartitionOpKind::Merge, destElement, srcElement,
                       sourceOperand);
  }

  static PartitionOp Require(Element tgt, SILInstruction *sourceInst = nullptr,
                             Options options = {}) {
    return PartitionOp(PartitionOpKind::Require, tgt, sourceInst, options);
  }

  static PartitionOp UnknownPatternError(Element elt,
                                         SILInstruction *sourceInst) {
    return PartitionOp(PartitionOpKind::UnknownPatternError, elt, sourceInst);
  }

  static PartitionOp InOutSendingAtFunctionExit(Element elt,
                                                SILInstruction *sourceInst) {
    return PartitionOp(PartitionOpKind::InOutSendingAtFunctionExit, elt,
                       sourceInst);
  }

  static PartitionOp
  NonSendableIsolationCrossingResult(Element elt, SILInstruction *sourceInst) {
    return PartitionOp(PartitionOpKind::NonSendableIsolationCrossingResult, elt,
                       sourceInst);
  }

  bool operator==(const PartitionOp &other) const {
    return opKind == other.opKind && opArg1 == other.opArg1 &&
           opArg2 == other.opArg2 && source == other.source;
  };

  bool operator<(const PartitionOp &other) const {
    if (opKind != other.opKind)
      return opKind < other.opKind;

    if (opArg1 != other.opArg1) {
      // null < non-null always.
      if (!opArg1.has_value() && other.opArg1.has_value())
        return true;
      // non-null >= null always.
      if (opArg1.has_value() && !other.opArg1.has_value())
        return false;
      return *opArg1 < other.opArg1.has_value();
    }

    if (opArg2 != other.opArg2) {
      // null < non-null always.
      if (!opArg2.has_value() && other.opArg2.has_value())
        return true;
      // non-null >= null always.
      if (opArg2.has_value() && !other.opArg2.has_value())
        return false;
      return *opArg2 < other.opArg2.has_value();
    }

    return source < other.source;
  }

  PartitionOpKind getKind() const { return opKind; }

  Element getOpArg1() const { return opArg1.value(); }
  Element getOpArg2() const { return opArg2.value(); }

  Options getOptions() const { return options; }

  void getOpArgs(SmallVectorImpl<Element> &args) const {
    if (opArg1.has_value())
      args.push_back(*opArg1);
    if (opArg2.has_value())
      args.push_back(*opArg2);
  }

  SILInstruction *getSourceInst() const {
    if (isa<Operand *>(source))
      return cast<Operand *>(source)->getUser();
    return cast<SILInstruction *>(source);
  }

  bool hasSourceInst() const { return isa<SILInstruction *>(source); }

  Operand *getSourceOp() const { return cast<Operand *>(source); }

  SILLocation getSourceLoc() const { return getSourceInst()->getLoc(); }

  void print(llvm::raw_ostream &os, bool extraSpace = false) const;

  SWIFT_DEBUG_DUMP { print(llvm::dbgs()); }
};

/// A map from Element -> Region that represents the current partition set.
class Partition {
public:
  /// A class defined in PartitionUtils unittest used to grab state from
  /// Partition without exposing it to other users.
  struct PartitionTester;

  using Element = PartitionPrimitives::Element;
  using Region = PartitionPrimitives::Region;
  using SendingOperandSet = ImmutablePointerSet<Operand *>;
  using SendingOperandSetFactory = ImmutablePointerSetFactory<Operand *>;
  using IsolationHistoryNode = IsolationHistory::Node;

private:
  /// A map from a region number to a instruction that sends it.
  ///
  /// All we care is that we ever track a single SILInstruction for a region
  /// since we are fine with emitting a single error per value and letting the
  /// user recompile. If this is an ask for in the future, we can use a true
  /// multi map here. The implication of this is that when we are performing
  /// dataflow we use a union operation to combine CFG elements and just take
  /// the first instruction that we see.
  llvm::SmallMapVector<Region, SendingOperandSet *, 2> regionToSendingOpMap;

  /// Label each index with a non-negative (unsigned) label if it is associated
  /// with a valid region.
  std::map<Element, Region> elementToRegionMap;

  /// Track a label that is guaranteed to be strictly larger than all in use,
  /// and therefore safe for use as a fresh label.
  Region freshLabel = Region(0);

  /// An immutable data structure that we use to push/pop isolation history.
  IsolationHistory history;

  /// In a canonical partition, all regions are labelled with the smallest index
  /// of any member. Certain operations like join and equals rely on
  /// canonicality so when it's invalidated this boolean tracks that, and it
  /// must be reestablished by a call to canonicalize().
  bool canonical;

public:
  Partition(IsolationHistory history)
      : elementToRegionMap({}), history(history), canonical(true) {}

  /// 1-arg constructor used when canonicality will be immediately invalidated,
  /// so set to false to begin with
  Partition(IsolationHistory history, bool canonical)
      : elementToRegionMap({}), history(history), canonical(canonical) {}

  /// Return a new Partition that has a single region containing the elements of
  /// \p indices.
  static Partition singleRegion(SILLocation loc, ArrayRef<Element> indices,
                                IsolationHistory inputHistory);

  /// Return a new Partition that has each element of \p indices in their own
  /// region.
  static Partition separateRegions(SILLocation loc, ArrayRef<Element> indices,
                                   IsolationHistory inputHistory);

  /// Test two partititons for equality by first putting them in canonical form
  /// then comparing for exact equality.
  ///
  /// Runs in linear time.
  static bool equals(Partition &fst, Partition &snd) {
    fst.canonicalize();
    snd.canonicalize();

    return fst.elementToRegionMap == snd.elementToRegionMap &&
           fst.regionToSendingOpMap.size() == snd.regionToSendingOpMap.size() &&
           llvm::all_of(
               fst.regionToSendingOpMap,
               [&snd](const std::pair<Region, SendingOperandSet *> &p) {
                 auto sndIter = snd.regionToSendingOpMap.find(p.first);
                 return sndIter != snd.regionToSendingOpMap.end() &&
                        sndIter->second == p.second;
               });
  }

  bool isTrackingElement(Element val) const {
    return elementToRegionMap.count(val);
  }

  /// Mark val as having been sent.
  void markSent(Element val, SendingOperandSet *sendingOperandSet);

  /// If val was marked as sent, unmark it as sent. Returns true if we found
  /// that \p val was sent. We return false otherwise.
  bool undoSend(Element val);

  /// If \p newElt is not being tracked, create a new region for \p newElt. If
  /// \p newElt is already being tracked, remove it from its old region as well.
  ///
  /// \arg updateHistory internal parameter used to determine if we should
  /// update the history. External users shouldn't use this
  void trackNewElement(Element newElt, bool updateHistory = true);

  /// Assigns \p oldElt to the region associated with \p newElt.
  void assignElement(Element oldElt, Element newElt, bool updateHistory = true);

  bool areElementsInSameRegion(Element firstElt, Element secondElt) const {
    return elementToRegionMap.at(firstElt) == elementToRegionMap.at(secondElt);
  }

  Region getRegion(Element elt) const { return elementToRegionMap.at(elt); }

  using iterator = std::map<Element, Region>::iterator;
  iterator begin() { return elementToRegionMap.begin(); }
  iterator end() { return elementToRegionMap.end(); }
  llvm::iterator_range<iterator> range() { return {begin(), end()}; }

  void clearSendingOperandState() { regionToSendingOpMap.clear(); }

  Partition removingSendingOperandState() const {
    Partition p = *this;
    p.clearSendingOperandState();
    return p;
  }

  /// Rewind one PartitionOp worth of history from the partition.
  ///
  /// If we rewind through a join, the joined isolation history before merging
  /// is inserted into \p foundJoinedHistories which should be processed
  /// afterwards if the current linear history does not find what one is looking
  /// for.
  ///
  /// NOTE: This can only be used if one has cleared the sent state using
  /// Partition::clearSendingOperandState or constructed a new Partiton using
  /// Partition::removingSendingOperandState(). This is because history
  /// rewinding doesn't use send information so just to be careful around
  /// potential invariants being broken, we just require the elimination of the
  /// send information.
  ///
  /// \returns true if there is more history that can be popped.
  bool popHistory(SmallVectorImpl<IsolationHistory> &foundJoinedHistories);

  /// Returns true if this value has any isolation history stored.
  bool hasHistory() const { return bool(history.getHead()); }

  /// Returns the number of nodes of stored history.
  ///
  /// NOTE: Do not use this in real code... only intended to be used in testing
  /// code.
  unsigned historySize() const {
    unsigned count = 0;
    auto *head = history.getHead();
    if (!head)
      return count;
    ++count;

    while ((head = head->getParent()))
      ++count;

    return count;
  }

  /// Return a copy of our isolation history.
  IsolationHistory getIsolationHistory() const { return history; }

  /// Construct the partition corresponding to the union of the two passed
  /// partitions.
  ///
  /// NOTE: snd is passed in as mutable since we may canonicalize snd. We will
  /// not perform any further mutations to snd.
  ///
  /// Runs in quadratic time.
  static Partition join(const Partition &fst, Partition &snd);

  /// Return a vector of the sent values in this partition.
  std::vector<Element> getSentValues() const {
    // For effeciency, this could return an iterator not a vector.
    std::vector<Element> sentVals;
    for (auto [i, _] : elementToRegionMap)
      if (isSent(i))
        sentVals.push_back(i);
    return sentVals;
  }

  /// Return a vector of the non-sent regions in this partition, each
  /// represented as a vector of values.
  std::vector<std::vector<Element>> getNonSentRegions() const {
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
    llvm::dbgs() << "(fresh=" << freshLabel << "){";
    for (const auto &[i, label] : elementToRegionMap)
      llvm::dbgs() << "[" << i << ": " << label << "] ";
    llvm::dbgs() << "}\n";
  }

  SWIFT_DEBUG_DUMP { print(llvm::dbgs()); }

  void print(llvm::raw_ostream &os) const;

  SWIFT_DEBUG_DUMPER(dumpVerbose()) { printVerbose(llvm::dbgs()); }

  void printVerbose(llvm::raw_ostream &os) const;

  SWIFT_DEBUG_DUMPER(dumpHistory()) { printHistory(llvm::dbgs()); }
  void printHistory(llvm::raw_ostream &os) const;

  /// See docs on \p history.pushHistorySequenceBoundary().
  IsolationHistoryNode *pushHistorySequenceBoundary(SILLocation loc) {
    return history.pushHistorySequenceBoundary(loc);
  }

  bool isSent(Element val) const {
    auto iter = elementToRegionMap.find(val);
    if (iter == elementToRegionMap.end())
      return false;
    return regionToSendingOpMap.count(iter->second);
  }

  /// Return the instruction that sent \p val's region or nullptr
  /// otherwise.
  SendingOperandSet *getSentOperandSet(Element val) const {
    auto iter = elementToRegionMap.find(val);
    if (iter == elementToRegionMap.end())
      return nullptr;
    auto iter2 = regionToSendingOpMap.find(iter->second);
    if (iter2 == regionToSendingOpMap.end())
      return nullptr;
    auto *set = iter2->second;
    assert(!set->empty());
    return set;
  }

  /// Validate that all regions in the regionToSentOpMap exist in the
  /// elementToRegionMap.
  ///
  /// Asserts when NDEBUG is set. Does nothing otherwise.
  void validateRegionToSendingOpMapRegions() const {
#ifndef NDEBUG
    llvm::SmallSet<Region, 8> regions;
    for (auto [eltNo, regionNo] : elementToRegionMap) {
      regions.insert(regionNo);
    }
    for (auto [regionNo, opSet] : regionToSendingOpMap) {
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
  Region merge(Element fst, Element snd, bool updateHistory = true);

private:
  /// Pop one history node. Multiple history nodes can make up one PartitionOp
  /// worth of history, so this is called by popHistory.
  ///
  /// Returns true if we succesfully popped a single history node.
  bool popHistoryOnce(SmallVectorImpl<IsolationHistory> &foundJoinHistoryNodes);

  /// A canonical region is defined to have its region number as equal to the
  /// minimum element number of all of its assigned element numbers. This
  /// routine goes through the element -> region map and transforms the
  /// partition state to restore this property.
  ///
  /// This runs in linear time.
  void canonicalize();

  /// Walk the elementToRegionMap updating all elements in the region of \p
  /// targetElement will be changed to now point at \p newRegion.
  void horizontalUpdate(Element targetElement, Region newRegion,
                        SmallVectorImpl<Element> &mergedElements);

  /// Push onto the history list that \p element should be added into its own
  /// independent region.
  IsolationHistoryNode *pushNewElementRegion(Element element) {
    return history.pushNewElementRegion(element);
  }

  /// Push onto the history that \p element should be removed from the region it
  /// belongs to and that \p element is the last element in that region.
  void pushRemoveLastElementFromRegion(Element element) {
    history.pushRemoveLastElementFromRegion(element);
  }

  /// Push onto the history that \p elementToRemove should be removed from the
  /// region which \p elementFromOldRegion belongs to.
  void pushRemoveElementFromRegion(Element elementFromOldRegion,
                                   Element elementToRemove) {
    history.pushRemoveElementFromRegion(elementFromOldRegion, elementToRemove);
  }

  /// Push that \p other should be merged into this region.
  void pushCFGHistoryJoin(IsolationHistory otherHistory) {
    if (auto *head = otherHistory.head)
      history.pushCFGHistoryJoin(head);
  }

  /// NOTE: Assumes that \p elementToMergeInto and \p otherRegions are disjoint.
  void pushMergeElementRegions(Element elementToMergeInto,
                               ArrayRef<Element> otherRegions) {
    history.pushMergeElementRegions(elementToMergeInto, otherRegions);
  }

  /// Remove a single element without touching the region to sending inst
  /// multimap. Assumes that the element is never the last element in a region.
  ///
  /// Just a helper routine.
  void removeElement(Element e) {
    // We added an element to its own region... so we should remove it and it
    // should be the last element in the region.
    bool result = elementToRegionMap.erase(e);
    canonical = false;
    assert(result && "Failed to erase?!");
  }
};

/// Swift style enum we use to decouple and reduce boilerplate in between the
/// diagnostic and non-diagnostic part of the infrastructure.
class PartitionOpError {
public:
  using Element = PartitionPrimitives::Element;
  using Region = PartitionPrimitives::Region;

  enum Kind {
#define PARTITION_OP_ERROR(NAME) NAME,
#include "PartitionOpError.def"
  };

  struct UnknownCodePatternError {
    const PartitionOp *op;

    UnknownCodePatternError(const PartitionOp &op) : op(&op) {}

    void print(llvm::raw_ostream &os, RegionAnalysisValueMap &valueMap) const;

    SWIFT_DEBUG_DUMPER(dump(RegionAnalysisValueMap &valueMap)) {
      print(llvm::dbgs(), valueMap);
    }
  };

  struct LocalUseAfterSendError {
    const PartitionOp *op;
    Element sentElement;
    Operand *sendingOp;

    LocalUseAfterSendError(const PartitionOp &op, Element elt,
                           Operand *sendingOp)
        : op(&op), sentElement(elt), sendingOp(sendingOp) {}

    void print(llvm::raw_ostream &os, RegionAnalysisValueMap &valueMap) const;

    SWIFT_DEBUG_DUMPER(dump(RegionAnalysisValueMap &valueMap)) {
      print(llvm::dbgs(), valueMap);
    }
  };

  struct SentNeverSendableError {
    const PartitionOp *op;
    Element sentElement;
    SILDynamicMergedIsolationInfo isolationRegionInfo;

    SentNeverSendableError(const PartitionOp &op, Element sentElement,
                           SILDynamicMergedIsolationInfo isolationRegionInfo)
        : op(&op), sentElement(sentElement),
          isolationRegionInfo(isolationRegionInfo) {}

    void print(llvm::raw_ostream &os, RegionAnalysisValueMap &valueMap) const;

    SWIFT_DEBUG_DUMPER(dump(RegionAnalysisValueMap &valueMap)) {
      print(llvm::dbgs(), valueMap);
    }
  };

  struct AssignNeverSendableIntoSendingResultError {
    const PartitionOp *op;
    Element destElement;
    SILFunctionArgument *destValue;
    Element srcElement;
    SILValue srcValue;
    SILDynamicMergedIsolationInfo srcIsolationRegionInfo;

    AssignNeverSendableIntoSendingResultError(
        const PartitionOp &op, Element destElement,
        SILFunctionArgument *destValue, Element srcElement, SILValue srcValue,
        SILDynamicMergedIsolationInfo srcIsolationRegionInfo)
        : op(&op), destElement(destElement), destValue(destValue),
          srcElement(srcElement), srcValue(srcValue),
          srcIsolationRegionInfo(srcIsolationRegionInfo) {}

    void print(llvm::raw_ostream &os, RegionAnalysisValueMap &valueMap) const;

    SWIFT_DEBUG_DUMPER(dump(RegionAnalysisValueMap &valueMap)) {
      print(llvm::dbgs(), valueMap);
    }
  };

  struct InOutSendingNotInitializedAtExitError {
    const PartitionOp *op;
    Element sentElement;
    Operand *sendingOp;

    InOutSendingNotInitializedAtExitError(const PartitionOp &op, Element elt,
                                          Operand *sendingOp)
        : op(&op), sentElement(elt), sendingOp(sendingOp) {}

    void print(llvm::raw_ostream &os, RegionAnalysisValueMap &valueMap) const;

    SWIFT_DEBUG_DUMPER(dump(RegionAnalysisValueMap &valueMap)) {
      print(llvm::dbgs(), valueMap);
    }
  };

  struct InOutSendingNotDisconnectedAtExitError {
    const PartitionOp *op;
    Element inoutSendingElement;
    SILDynamicMergedIsolationInfo isolationInfo;

    InOutSendingNotDisconnectedAtExitError(
        const PartitionOp &op, Element elt,
        SILDynamicMergedIsolationInfo isolation)
        : op(&op), inoutSendingElement(elt), isolationInfo(isolation) {}

    void print(llvm::raw_ostream &os, RegionAnalysisValueMap &valueMap) const;

    SWIFT_DEBUG_DUMPER(dump(RegionAnalysisValueMap &valueMap)) {
      print(llvm::dbgs(), valueMap);
    }
  };

  struct NonSendableIsolationCrossingResultError {
    const PartitionOp *op;

    Element returnValueElement;

    NonSendableIsolationCrossingResultError(const PartitionOp &op,
                                            Element returnValue)
        : op(&op), returnValueElement(returnValue) {}

    void print(llvm::raw_ostream &os, RegionAnalysisValueMap &valueMap) const;

    SWIFT_DEBUG_DUMPER(dump(RegionAnalysisValueMap &valueMap)) {
      print(llvm::dbgs(), valueMap);
    }
  };

#define PARTITION_OP_ERROR(NAME)                                               \
  static_assert(std::is_copy_constructible_v<NAME##Error>,                     \
                #NAME " must be copy constructable");
#include "PartitionOpError.def"
#define PARTITION_OP_ERROR(NAME)                                               \
  static_assert(std::is_copy_assignable_v<NAME##Error>,                        \
                #NAME " must be copy assignable");
#include "PartitionOpError.def"

private:
  Kind kind;
  std::variant<
#define PARTITION_OP_ERROR(NAME) NAME##Error,
#include "PartitionOpError.def"
      bool // sentinel value to avoid syntax issues.
      >
      data;

public:
#define PARTITION_OP_ERROR(NAME)                                               \
  PartitionOpError(NAME##Error error) : kind(Kind::NAME), data(error) {}
#include "PartitionOpError.def"

  PartitionOpError(const PartitionOpError &error)
      : kind(error.kind), data(error.data) {
    switch (getKind()) {
#define PARTITION_OP_ERROR(NAME)                                               \
  case NAME:                                                                   \
    assert(std::holds_alternative<NAME##Error>(data) &&                        \
           "Data has value that does not match kind?!");                       \
    break;
#include "PartitionOpError.def"
    }
  }

  PartitionOpError &operator=(const PartitionOpError &error) {
    kind = error.kind;
    data = error.data;

    switch (getKind()) {
#define PARTITION_OP_ERROR(NAME)                                               \
  case NAME:                                                                   \
    assert(std::holds_alternative<NAME##Error>(data) &&                        \
           "Data has value that does not match kind?!");                       \
    break;
#include "PartitionOpError.def"
    }
    return *this;
  }

  Kind getKind() const { return kind; }

  void print(llvm::raw_ostream &os, RegionAnalysisValueMap &valueMap) const {
    switch (getKind()) {
#define PARTITION_OP_ERROR(NAME)                                               \
  case NAME:                                                                   \
    return get##NAME##Error().print(os, valueMap);
#include "PartitionOpError.def"
    }
    llvm_unreachable("Covered switch isn't covered?!");
  }

  SWIFT_DEBUG_DUMPER(dump(RegionAnalysisValueMap &valueMap)) {
    return print(llvm::dbgs(), valueMap);
  }

#define PARTITION_OP_ERROR(NAME)                                               \
  NAME##Error get##NAME##Error() const {                                       \
    assert(getKind() == Kind::NAME);                                           \
    return std::get<NAME##Error>(data);                                        \
  }
#include "PartitionOpError.def"
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
  using SendingOperandSetFactory = Partition::SendingOperandSetFactory;

#define PARTITION_OP_ERROR(NAME)                                               \
  using NAME##Error = PartitionOpError::NAME##Error;
#include "PartitionOpError.def"

protected:
  SendingOperandSetFactory &ptrSetFactory;
  SendingOperandToStateMap &operandToStateMap;

  Partition &p;

public:
  PartitionOpEvaluator(Partition &p, SendingOperandSetFactory &ptrSetFactory,
                       SendingOperandToStateMap &operandToStateMap)
      : ptrSetFactory(ptrSetFactory), operandToStateMap(operandToStateMap),
        p(p) {}

  /// Call shouldEmitVerboseLogging on our CRTP subclass.
  bool shouldEmitVerboseLogging() const {
    return asImpl().shouldEmitVerboseLogging();
  }

  void handleError(PartitionOpError error) { asImpl().handleError(error); }

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
  std::optional<std::pair<SILDynamicMergedIsolationInfo, bool>>
  getIsolationRegionInfo(Region region, Operand *sourceOp) const {
    bool isClosureCapturedElt = false;
    std::optional<SILDynamicMergedIsolationInfo> isolationRegionInfo =
        SILDynamicMergedIsolationInfo();

    for (const auto &pair : p.range()) {
      if (pair.second == region) {
        isolationRegionInfo =
            isolationRegionInfo->merge(getIsolationRegionInfo(pair.first));
        if (!isolationRegionInfo)
          return {};
        if (sourceOp) {
          isClosureCapturedElt |= isClosureCaptured(pair.first, sourceOp);
        }
      }
    }

    return {{isolationRegionInfo.value(), isClosureCapturedElt}};
  }

  /// Overload of \p getIsolationRegionInfo without an Operand.
  SILDynamicMergedIsolationInfo getIsolationRegionInfo(Region region) const {
    if (auto opt = getIsolationRegionInfo(region, nullptr))
      return opt->first;
    return SILDynamicMergedIsolationInfo();
  }

  bool isTaskIsolatedDerived(Element elt) const {
    return asImpl().isTaskIsolatedDerived(elt);
  }

  /// Call isClosureCaptured on our CRTP subclass.
  bool isClosureCaptured(Element elt, Operand *op) const {
    return asImpl().isClosureCaptured(elt, op);
  }

  /// Some evaluators pass in mock instructions that one cannot call getLoc()
  /// upon. So to allow for this, provide a routine that our impl can override
  /// if they need to.
  static SILLocation getLoc(SILInstruction *inst) { return Impl::getLoc(inst); }

  /// Some evaluators pass in mock operands that one cannot call getLoc()
  /// upon. So to allow for this, provide a routine that our impl can override
  /// if they need to.
  static SILLocation getLoc(Operand *op) { return Impl::getLoc(op); }

  /// Some evaluators pass in mock operands that one cannot call getUser()
  /// upon. So to allow for this, provide a routine that our impl can override
  /// if they need to.
  static SILIsolationInfo getIsolationInfo(const PartitionOp &partitionOp) {
    return Impl::getIsolationInfo(partitionOp);
  }

  /// Some evaluators do not support accessing fields on their SILInstruction
  /// since they just pass in "mocked" SILInstruction. We allow for them to just
  /// return false for this case to prevent dereference issues.
  static bool
  doesParentFunctionHaveSendingResult(const PartitionOp &partitionOp) {
    return Impl::doesFunctionHaveSendingResult(partitionOp);
  }

  std::optional<Element> getElement(SILValue value) const {
    return asImpl().getElement(value);
  }

  SILValue getRepresentative(SILValue value) const {
    return asImpl().getRepresentative(value);
  }

  RepresentativeValue getRepresentativeValue(Element element) const {
    return asImpl().getRepresentativeValue(element);
  }

  /// Apply \p op to the partition op.
  void apply(const PartitionOp &op) {
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

    // Set the boundary so that as we push, this shows when to stop processing
    // for this PartitionOp.
    SILLocation loc = op.hasSourceInst() ? getLoc(op.getSourceInst())
                                         : getLoc(op.getSourceOp());
    p.pushHistorySequenceBoundary(loc);

    switch (op.getKind()) {
    case PartitionOpKind::Assign: {
      assert(p.isTrackingElement(op.getOpArg2()) &&
             "Assign PartitionOp's source argument should be already tracked");

      // See if we are assigning an a non-disconnected value into a 'out
      // sending' parameter. In such a case, we emit a diagnostic.
      if (doesParentFunctionHaveSendingResult(op)) {
        if (auto instance = getRepresentativeValue(op.getOpArg1())) {
          if (auto value = instance.maybeGetValue()) {
            if (auto *fArg = dyn_cast<SILFunctionArgument>(value)) {
              if (fArg->getArgumentConvention().isIndirectOutParameter()) {
                auto staticRegionIsolation =
                    getIsolationRegionInfo(op.getOpArg2());
                Region srcRegion = p.getRegion(op.getOpArg2());
                auto dynamicRegionIsolation = getIsolationRegionInfo(srcRegion);

                // We can unconditionally getValue here since we can never
                // assign an actor introducing inst.
                auto rep = getRepresentativeValue(op.getOpArg2()).getValue();
                if (!dynamicRegionIsolation.isDisconnected() &&
                    !staticRegionIsolation.isUnsafeNonIsolated()) {
                  handleError(AssignNeverSendableIntoSendingResultError(
                      op, op.getOpArg1(), fArg, op.getOpArg2(), rep,
                      dynamicRegionIsolation));
                }
              }
            }
          }
        }
      }

      p.assignElement(op.getOpArg1(), op.getOpArg2());
      return;
    }
    case PartitionOpKind::AssignFresh: {
      p.trackNewElement(op.getOpArg1());
      return;
    }
    case PartitionOpKind::Send: {
      // NOTE: We purposely do not check here if the value has already been
      // sent. Callers are expected to put a require for that purpose. This
      // ensures that if we pass the same argument multiple times to the same
      // sending function as weakly sent arguments, we do not get an
      // error.
      assert(p.isTrackingElement(op.getOpArg1()) &&
             "Send PartitionOp's argument should already be tracked");

      // Before we do any further work, see if we have a nonisolated(unsafe)
      // element. In such a case, this is also not a real send point.
      Element sentElement = op.getOpArg1();
      if (getIsolationRegionInfo(sentElement).isUnsafeNonIsolated()) {
        return;
      }

      // Otherwise, we need to merge our isolation region info with the
      // isolation region info of everything else in our region. This is the
      // dynamic isolation region info found by the dataflow.
      Region sentRegion = p.getRegion(sentElement);
      bool regionHasClosureCapturedElt = false;
      SILDynamicMergedIsolationInfo sentRegionIsolation;

      // TODO: Today we only return the first element in our region that has
      // some form of isolation. This causes us to in the case of sending
      // partial_applies to only emit a diagnostic for the first element in the
      // capture list of the partial_apply. If we returned a list of potential
      // errors... we could emit the error for each capture individually.
      auto pairOpt = getIsolationRegionInfo(sentRegion, op.getSourceOp());
      if (!pairOpt) {
        return handleError(UnknownCodePatternError(op));
      }
      std::tie(sentRegionIsolation, regionHasClosureCapturedElt) = *pairOpt;

      auto calleeIsolationInfo = getIsolationInfo(op);

      // If our callee and region are both actor isolated and part of the same
      // isolation domain, do not treat this as a send.
      if (calleeIsolationInfo.isActorIsolated() &&
          sentRegionIsolation.hasSameIsolation(calleeIsolationInfo))
        return;

      // At this point, check if our sent value is not disconnected. If so, emit
      // a sent never sendable helper.
      if (sentRegionIsolation && !sentRegionIsolation.isDisconnected()) {
        return handleSendNeverSentHelper(op, op.getOpArg1(), sentRegionIsolation);
      }

      // Next see if we are disconnected and have the same isolation. In such a
      // case, if we are not marked explicitly as sending, we do not send
      // since the disconnected value is allowed to be resued after we
      // return. If we are passed as a sending parameter, we cannot do this.
      if (auto *sourceInst = Impl::getSourceInst(op)) {
        if (auto fas = FullApplySite::isa(sourceInst);
            (!fas || !fas.isSending(*op.getSourceOp())) &&
            sentRegionIsolation.isDisconnected() && calleeIsolationInfo &&
            sentRegionIsolation.hasSameIsolation(calleeIsolationInfo))
          return;
      }

      // Mark op.getOpArg1() as sent.
      SendingOperandState &state = operandToStateMap.get(op.getSourceOp());
      state.isClosureCaptured |= regionHasClosureCapturedElt;
      if (auto newInfo = state.isolationInfo.merge(sentRegionIsolation)) {
        state.isolationInfo = *newInfo;
      } else {
        handleError(UnknownCodePatternError(op));
      }
      assert(state.isolationInfo && "Cannot have unknown");
      state.isolationHistory.pushCFGHistoryJoin(p.getIsolationHistory());
      auto *ptrSet = ptrSetFactory.get(op.getSourceOp());
      p.markSent(op.getOpArg1(), ptrSet);
      return;
    }
    case PartitionOpKind::UndoSend: {
      assert(p.isTrackingElement(op.getOpArg1()) &&
             "UndoSend PartitionOp's argument should already be tracked");

      // Mark op.getOpArg1() as not sent.
      p.undoSend(op.getOpArg1());
      return;
    }
    case PartitionOpKind::Merge: {
      assert(p.isTrackingElement(op.getOpArg1()) &&
             p.isTrackingElement(op.getOpArg2()) &&
             "Merge PartitionOp's arguments should already be tracked");

      // See if we are assigning an a non-disconnected value into a 'out
      // sending' parameter. In such a case, we emit a diagnostic.
      if (doesParentFunctionHaveSendingResult(op)) {
        if (auto instance = getRepresentativeValue(op.getOpArg1())) {
          if (auto value = instance.maybeGetValue()) {
            if (auto *fArg = dyn_cast<SILFunctionArgument>(value)) {
              if (fArg->getArgumentConvention().isIndirectOutParameter()) {
                auto staticRegionIsolation =
                    getIsolationRegionInfo(op.getOpArg2());
                Region srcRegion = p.getRegion(op.getOpArg2());
                auto dynamicRegionIsolation = getIsolationRegionInfo(srcRegion);
                // We can unconditionally getValue here since we can never
                // assign an actor introducing inst.
                auto rep = getRepresentativeValue(op.getOpArg2()).getValue();
                if (!dynamicRegionIsolation.isDisconnected() &&
                    !staticRegionIsolation.isUnsafeNonIsolated()) {
                  handleError(AssignNeverSendableIntoSendingResultError(
                      op, op.getOpArg1(), fArg, op.getOpArg2(), rep,
                      dynamicRegionIsolation));
                }
              }
            }
          }
        }
      }

      p.merge(op.getOpArg1(), op.getOpArg2());
      return;
    }
    case PartitionOpKind::Require:
      assert(p.isTrackingElement(op.getOpArg1()) &&
             "Require PartitionOp's argument should already be tracked");
      if (auto *sentOperandSet = p.getSentOperandSet(op.getOpArg1())) {
        for (auto sentOperand : sentOperandSet->data()) {
          handleLocalUseAfterSendHelper(op, op.getOpArg1(), sentOperand);
        }
      }
      return;
    case PartitionOpKind::InOutSendingAtFunctionExit: {
      assert(p.isTrackingElement(op.getOpArg1()) &&
             "Require PartitionOp's argument should already be tracked");

      // First check if the region of our 'inout sending' element has been
      // sent. In that case, we emit a special use after free error.
      if (auto *sentOperandSet = p.getSentOperandSet(op.getOpArg1())) {
        for (auto sentOperand : sentOperandSet->data()) {
          handleError(InOutSendingNotInitializedAtExitError(op, op.getOpArg1(),
                                                            sentOperand));
        }
        return;
      }

      // If we were not sent, check if our region is actor isolated. If so,
      // error since we need a disconnected value in the inout parameter.
      Region inoutSendingRegion = p.getRegion(op.getOpArg1());
      auto dynamicRegionIsolation = getIsolationRegionInfo(inoutSendingRegion);

      // If we failed to merge emit an unknown pattern error so we fail.
      if (!dynamicRegionIsolation) {
        handleError(UnknownCodePatternError(op));
        return;
      }

      // Otherwise, emit the error if the dynamic region isolation is not
      // disconnected.
      if (!dynamicRegionIsolation.isDisconnected()) {
        handleError(InOutSendingNotDisconnectedAtExitError(
            op, op.getOpArg1(), dynamicRegionIsolation));
      }
      return;
    }
    case PartitionOpKind::UnknownPatternError:
      // Begin tracking the specified element in case we have a later use.
      p.trackNewElement(op.getOpArg1());

      // Then emit an unknown code pattern error.
      return handleError(UnknownCodePatternError(op));
    case PartitionOpKind::NonSendableIsolationCrossingResult: {
      // Grab the dynamic dataflow isolation information for our element's
      // region.
      Region region = p.getRegion(op.getOpArg1());

      // Then emit the error.
      return handleError(
          NonSendableIsolationCrossingResultError(op, op.getOpArg1()));
    }
    case PartitionOpKind::AssignFreshAssign:
      assert(p.isTrackingElement(op.getOpArg2()) &&
             "Source argument should be already tracked");
      p.trackNewElement(op.getOpArg1());
      p.assignElement(op.getOpArg1(), op.getOpArg2());
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
  /// To work around not having isolation in interface types, the type checker
  /// inserts casts and other AST nodes that are used to enrich the AST with
  /// isolation information. This results in Sendable functions being
  /// wrapped/converted/etc in ways that hide the Sendability. This helper looks
  /// through these conversions/wrappers/thunks to see if the original
  /// underlying function is Sendable.
  ///
  /// The two ways this can happen is that we either get an actual function_ref
  /// that is Sendable or we get a convert function with a Sendable operand.
  bool isHiddenSendableFunctionType(SILValue equivalenceClassRep) const {
    SILValue valueToTest = equivalenceClassRep;
    while (true) {
      if (auto *pai = dyn_cast<PartialApplyInst>(valueToTest)) {
        if (auto *calleeFunction = pai->getCalleeFunction()) {
          if (pai->getNumArguments() >= 1 &&
              pai->getArgument(0)->getType().isFunction() &&
              calleeFunction->isThunk()) {
            valueToTest = pai->getArgument(0);
            continue;
          }

          if (calleeFunction->getLoweredFunctionType()->isSendable())
            return true;
        }
      }

      if (auto *i = dyn_cast<ThinToThickFunctionInst>(valueToTest)) {
        valueToTest = i->getOperand();
        continue;
      }
      if (auto *i = dyn_cast<ConvertEscapeToNoEscapeInst>(valueToTest)) {
        valueToTest = i->getOperand();
        continue;
      }
      break;
    }

    if (auto *fn = dyn_cast<FunctionRefInst>(valueToTest))
      return fn->getReferencedFunction()->getLoweredFunctionType()->isSendable();

    auto *cvi = dyn_cast<ConvertFunctionInst>(valueToTest);
    if (!cvi)
      return false;

    return cvi->getOperand()->getType().isSendable(
        equivalenceClassRep->getFunction());
  }

  // Private helper that squelches the error if our send instruction and our use
  // have the same isolation.
  void handleLocalUseAfterSendHelper(const PartitionOp &op, Element elt,
                                     Operand *sentOp) {
    if (shouldTryToSquelchErrors()) {
      if (SILValue equivalenceClassRep = getRepresentative(sentOp->get())) {

        // If we have a temporary that is initialized with an unsafe nonisolated
        // value... squelch the error like if we were that value.
        //
        // TODO: This goes away with opaque values.
        if (auto *asi = dyn_cast<AllocStackInst>(equivalenceClassRep)) {
          if (SILValue value = getInitOfTemporaryAllocStack(asi)) {
            if (auto elt = getElement(value)) {
              SILIsolationInfo eltIsolationInfo = getIsolationRegionInfo(*elt);
              if (eltIsolationInfo.isUnsafeNonIsolated()) {
                return;
              }
            }
          }
        }

        // See if we have a convert function from a `@Sendable` type. In this
        // case, we want to squelch the error.
        if (isHiddenSendableFunctionType(equivalenceClassRep))
          return;
      }

      // If our instruction does not have any isolation info associated with it,
      // it must be nonisolated. See if our function has a matching isolation to
      // our sent operand. If so, we can squelch this.
      if (auto functionIsolation =
              sentOp->getUser()->getFunction()->getActorIsolation()) {
        if (functionIsolation->isActorIsolated() &&
            SILIsolationInfo::get(sentOp->getUser())
                .hasSameIsolation(*functionIsolation))
          return;
      }
    }

    // Ok, we actually need to emit a call to the callback.
    return handleError(LocalUseAfterSendError(op, elt, sentOp));
  }

  // Private helper that squelches the error if our send instruction and our
  // use have the same isolation.
  void handleSendNeverSentHelper(
      const PartitionOp &op, Element elt,
      SILDynamicMergedIsolationInfo dynamicMergedIsolationInfo) {
    if (shouldTryToSquelchErrors()) {
      if (SILValue equivalenceClassRep =
              getRepresentative(op.getSourceOp()->get())) {
        // If we have a temporary that is initialized with an unsafe nonisolated
        // value... squelch the error like if we were that value.
        //
        // TODO: This goes away with opaque values.
        if (auto *asi = dyn_cast<AllocStackInst>(equivalenceClassRep)) {
          if (SILValue value = getInitOfTemporaryAllocStack(asi)) {
            if (auto elt = getElement(value)) {
              SILIsolationInfo eltIsolationInfo = getIsolationRegionInfo(*elt);
              if (eltIsolationInfo.isUnsafeNonIsolated()) {
                return;
              }
            }
          }
        }

        // See if we have a convert function from a `@Sendable` type. In this
        // case, we want to squelch the error.
        if (isHiddenSendableFunctionType(equivalenceClassRep))
          return;
      }
    }

    // Ok, we actually need to emit a call to the callback.
    return handleError(
        SentNeverSendableError(op, elt, dynamicMergedIsolationInfo));
  }
};

/// A base implementation that can be used to default initialize CRTP
/// subclasses. Only used to implement base functionality for subclass
/// CRTPs. For true basic evaluation, use PartitionOpEvaluatorBasic below.
template <typename Subclass>
struct PartitionOpEvaluatorBaseImpl : PartitionOpEvaluator<Subclass> {
  using Element = PartitionPrimitives::Element;
  using Region = PartitionPrimitives::Region;
  using SendingOperandSetFactory = Partition::SendingOperandSetFactory;
  using Super = PartitionOpEvaluator<Subclass>;

  PartitionOpEvaluatorBaseImpl(Partition &workingPartition,
                               SendingOperandSetFactory &ptrSetFactory,
                               SendingOperandToStateMap &operandToStateMap)
      : Super(workingPartition, ptrSetFactory, operandToStateMap) {}

  /// Should we emit extra verbose logging statements when evaluating
  /// PartitionOps.
  bool shouldEmitVerboseLogging() const { return true; }

  /// This is used to determine if an element is actor derived. If we determine
  /// that a region containing such an element is sent, we emit an error since
  /// actor regions cannot be sent.
  bool isActorDerived(Element elt) const { return false; }

  /// This is used to determine if an element is in the same region as a task
  /// isolated value.
  bool isTaskIsolatedDerived(Element elt) const { return false; }

  /// By default, do nothing upon error.
  void handleError(PartitionOpError error) {}

  /// Returns the information about \p elt's isolation that we ascertained from
  /// SIL and the AST.
  SILIsolationInfo getIsolationRegionInfo(Element elt) const {
    return SILIsolationInfo();
  }

  /// If we are able to, return the element associated with \p value. If
  /// unsupported, returns none.
  std::optional<Element> getElement(SILValue value) const { return {}; }

  /// If supported, returns the representative in \p value's equivalence
  /// class. Returns an empty SILValue if this is unsupported or if it does not
  /// have one.
  SILValue getRepresentative(SILValue value) const { return SILValue(); }

  RepresentativeValue getRepresentativeValue(Element element) const {
    return RepresentativeValue();
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

  static SILLocation getLoc(SILInstruction *inst) { return inst->getLoc(); }
  static SILLocation getLoc(Operand *op) { return op->getUser()->getLoc(); }
  static SILInstruction *getSourceInst(const PartitionOp &partitionOp) {
    return partitionOp.getSourceInst();
  }
  static SILIsolationInfo getIsolationInfo(const PartitionOp &partitionOp) {
    return SILIsolationInfo::get(partitionOp.getSourceInst());
  }
  static bool doesFunctionHaveSendingResult(const PartitionOp &partitionOp) {
    return partitionOp.getSourceInst()
        ->getFunction()
        ->getLoweredFunctionType()
        ->hasSendingResult();
  }
};

/// A subclass of PartitionOpEvaluatorBaseImpl that doesn't have any special
/// behavior.
struct PartitionOpEvaluatorBasic final
    : PartitionOpEvaluatorBaseImpl<PartitionOpEvaluatorBasic> {
  PartitionOpEvaluatorBasic(Partition &workingPartition,
                            SendingOperandSetFactory &ptrSetFactory,
                            SendingOperandToStateMap &operandToStateMap)
      : PartitionOpEvaluatorBaseImpl(workingPartition, ptrSetFactory,
                                     operandToStateMap) {}
};

} // namespace swift

namespace llvm {

inline llvm::raw_ostream &operator<<(llvm::raw_ostream &os,
                                     const swift::RepresentativeValue &value) {
  value.print(os);
  return os;
}

template <>
struct DenseMapInfo<swift::RepresentativeValue> {
  using RepresentativeValue = swift::RepresentativeValue;
  using InnerType = RepresentativeValue::InnerType;
  using InnerDenseMapInfo = DenseMapInfo<InnerType>;

  static RepresentativeValue getEmptyKey() {
    return RepresentativeValue(InnerDenseMapInfo::getEmptyKey());
  }
  static RepresentativeValue getTombstoneKey() {
    return RepresentativeValue(InnerDenseMapInfo::getTombstoneKey());
  }

  static unsigned getHashValue(RepresentativeValue value) {
    return InnerDenseMapInfo::getHashValue(value.value);
  }

  static bool isEqual(RepresentativeValue LHS, RepresentativeValue RHS) {
    return InnerDenseMapInfo::isEqual(LHS.value, RHS.value);
  }
};

} // namespace llvm

#endif // SWIFT_PARTITIONUTILS_H
