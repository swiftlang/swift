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

  SWIFT_DEBUG_DUMP { print(llvm::dbgs()); }
};

} // namespace swift

namespace llvm {

template <>
struct PointerLikeTypeTraits<swift::TransferringOperand> {
  using TransferringOperand = swift::TransferringOperand;

  static inline void *getAsVoidPointer(TransferringOperand ptr) {
    return PointerLikeTypeTraits<
        TransferringOperand::ValueType>::getAsVoidPointer(ptr.value);
  }
  static inline TransferringOperand getFromVoidPointer(void *ptr) {
    return {PointerLikeTypeTraits<
        TransferringOperand::ValueType>::getFromVoidPointer(ptr)};
  }

  static constexpr int NumLowBitsAvailable = PointerLikeTypeTraits<
      TransferringOperand::ValueType>::NumLowBitsAvailable;
};

template <>
struct DenseMapInfo<swift::TransferringOperand> {
  using TransferringOperand = swift::TransferringOperand;
  using ParentInfo = DenseMapInfo<TransferringOperand::ValueType>;

  static TransferringOperand getEmptyKey() {
    return TransferringOperand(ParentInfo::getEmptyKey());
  }
  static TransferringOperand getTombstoneKey() {
    return TransferringOperand(ParentInfo::getTombstoneKey());
  }

  static unsigned getHashValue(TransferringOperand operand) {
    return ParentInfo::getHashValue(operand.value);
  }
  static bool isEqual(TransferringOperand LHS, TransferringOperand RHS) {
    return ParentInfo::isEqual(LHS.value, RHS.value);
  }
};

} // namespace llvm

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

struct PartitionOpEvaluator;

/// A map from Element -> Region that represents the current partition set.
///
///
class Partition {
public:
  /// A class defined in PartitionUtils unittest used to grab state from
  /// Partition without exposing it to other users.
  struct PartitionTester;
  friend PartitionOpEvaluator;

  using Element = PartitionPrimitives::Element;
  using Region = PartitionPrimitives::Region;
  using TransferringOperandSet = ImmutablePointerSet<TransferringOperand>;
  using TransferringOperandSetFactory =
      ImmutablePointerSetFactory<TransferringOperand>;

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

  bool isTracked(Element val) const { return elementToRegionMap.count(val); }

  /// Mark val as transferred.
  void markTransferred(Element val,
                       TransferringOperandSet *transferredOperandSet) {
    // First see if our val is tracked. If it is not tracked, insert it and mark
    // its new region as transferred.
    if (!isTracked(val)) {
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
    if (!isTracked(val)) {
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

  void addElement(Element newElt) {
    // Map index newElt to a fresh label.
    elementToRegionMap.insert_or_assign(newElt, fresh_label);

    // Increment the fresh label so it remains fresh.
    fresh_label = Region(fresh_label + 1);
    canonical = false;
  }

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
          if (fstReduced.fresh_label < Region(sndEltNumber))
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
      if (fstReduced.fresh_label < sndRegionNumber)
        fstReduced.fresh_label = Region(sndEltNumber + 1);
    }

    LLVM_DEBUG(llvm::dbgs() << "JOIN PEFORMED: \nFST: ";
               fst.print(llvm::dbgs()); llvm::dbgs() << "SND: ";
               snd.print(llvm::dbgs()); llvm::dbgs() << "RESULT: ";
               fstReduced.print(llvm::dbgs()););

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
            iter->getSecond()->range(), [](const TransferringOperand &operand) {
              return operand.isClosureCaptured();
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
            iter->getSecond()->range(), [](const TransferringOperand &operand) {
              return operand.isClosureCaptured();
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
          op.print(os);
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

private:
  /// Used only in assertions, check that Partitions promised to be canonical
  /// are actually canonical
  bool is_canonical_correct() {
    if (!canonical)
      return true; // vacuously correct

    auto fail = [&](Element i, int type) {
      llvm::errs() << "FAIL(i=" << i << "; type=" << type << "): ";
      print(llvm::errs());
      return false;
    };

    llvm::SmallDenseSet<Region, 8> seenRegion;
    for (auto &[eltNo, regionNo] : elementToRegionMap) {
      // See if all of our regionToTransferMap keys are regions in labels.
      if (regionToTransferredOpMap.count(regionNo))
        seenRegion.insert(regionNo);

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

    if (seenRegion.size() != regionToTransferredOpMap.size()) {
      llvm::report_fatal_error(
          "FAIL! regionToTransferMap has a region that isn't being tracked?!");
    }

    return true;
  }

  /// For each region label that occurs, find the first index at which it occurs
  /// and relabel all instances of it to that index.  This excludes the -1 label
  /// for transferred regions.
  ///
  /// This runs in linear time.
  void canonicalize() {
    if (canonical)
      return;
    canonical = true;

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
/// Apply the passed PartitionOp to this partition, performing its action.  A
/// `handleFailure` closure can optionally be passed in that will be called if
/// a transferred region is required. The closure is given the PartitionOp
/// that failed, and the index of the SIL value that was required but
/// transferred. Additionally, a list of "nontransferrable" indices can be
/// passed in along with a handleTransferNonTransferrable closure. In the
/// event that a region containing one of the nontransferrable indices is
/// transferred, the closure will be called with the offending transfer.
struct PartitionOpEvaluator {
  using Element = PartitionPrimitives::Element;
  using Region = PartitionPrimitives::Region;
  using TransferringOperandSetFactory =
      Partition::TransferringOperandSetFactory;

  TransferringOperandSetFactory &ptrSetFactory;

  Partition &p;

  /// If this PartitionOp evaluator should emit log statements.
  bool emitLog = true;

  /// If set to a non-null function, then this callback will be called if we
  /// discover a transferred value was used after it was transferred.
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
  std::function<void(const PartitionOp &, Element, TransferringOperand)>
      failureCallback = nullptr;

  /// A list of elements that cannot be transferred. Whenever we transfer, we
  /// check this list to see if we are transferring the element and then call
  /// transferNonTransferrableCallback. This should consist only of function
  /// arguments.
  ArrayRef<Element> nonTransferrableElements = {};

  /// If set to a non-null function_ref, this is called if we detect a never
  /// transferred element that was passed to a transfer instruction.
  std::function<void(const PartitionOp &, Element)>
      transferredNonTransferrableCallback = nullptr;

  /// If set to a non-null function_ref, then this is used to determine if an
  /// element is actor derived. If we determine that a region containing such an
  /// element is transferred, we emit an error since actor regions cannot be
  /// transferred.
  std::function<bool(Element)> isActorDerivedCallback = nullptr;

  /// Check if the representative value of \p elt is closure captured at \p
  /// op.
  ///
  /// NOTE: We actually just use the user of \p op in our callbacks. The reason
  /// why we do not just pass in that SILInstruction is that then we would need
  /// to access the instruction in the evaluator which creates a problem when
  /// since the operand we pass in is a dummy operand.
  std::function<bool(Element elt, Operand *op)> isClosureCapturedCallback =
      nullptr;

  PartitionOpEvaluator(Partition &p,
                       TransferringOperandSetFactory &ptrSetFactory)
      : ptrSetFactory(ptrSetFactory), p(p) {}

  /// A wrapper around the failure callback that checks if it is nullptr.
  void handleFailure(const PartitionOp &op, Element elt,
                     TransferringOperand transferringOp) const {
    if (!failureCallback)
      return;
    failureCallback(op, elt, transferringOp);
  }

  /// A wrapper around transferNonTransferrableCallback that only calls it if it
  /// is not null.
  void handleTransferNonTransferrable(const PartitionOp &op,
                                      Element elt) const {
    if (!transferredNonTransferrableCallback)
      return;
    transferredNonTransferrableCallback(op, elt);
  }

  /// A wrapper around isActorDerivedCallback that returns false if
  /// isActorDerivedCallback is nullptr and otherwise returns
  /// isActorDerivedCallback().
  bool isActorDerived(Element elt) const {
    return bool(isActorDerivedCallback) && isActorDerivedCallback(elt);
  }

  /// A wraper around isClosureCapturedCallback that returns false if
  /// isClosureCapturedCallback is nullptr and otherwise returns
  /// isClosureCapturedCallback.
  bool isClosureCaptured(Element elt, Operand *op) const {
    return bool(isClosureCapturedCallback) &&
           isClosureCapturedCallback(elt, op);
  }

  /// Apply \p op to the partition op.
  void apply(const PartitionOp &op) const {
    if (emitLog) {
      REGIONBASEDISOLATION_VERBOSE_LOG(llvm::dbgs() << "Applying: ";
                                       op.print(llvm::dbgs()));
      REGIONBASEDISOLATION_VERBOSE_LOG(llvm::dbgs() << "    Before: ";
                                       p.print(llvm::dbgs()));
    }
    SWIFT_DEFER {
      if (emitLog) {
        REGIONBASEDISOLATION_VERBOSE_LOG(llvm::dbgs() << "    After:  ";
                                         p.print(llvm::dbgs()));
      }
      assert(p.is_canonical_correct());
    };

    switch (op.getKind()) {
    case PartitionOpKind::Assign:
      assert(op.getOpArgs().size() == 2 &&
             "Assign PartitionOp should be passed 2 arguments");
      assert(p.elementToRegionMap.count(op.getOpArgs()[1]) &&
             "Assign PartitionOp's source argument should be already tracked");
      // If we are using a region that was transferred as our assignment source
      // value... emit an error.
      if (auto *transferredOperandSet = p.getTransferred(op.getOpArgs()[1])) {
        for (auto transferredOperand : transferredOperandSet->data()) {
          handleFailure(op, op.getOpArgs()[1], transferredOperand);
        }
      }

      p.elementToRegionMap.insert_or_assign(
          op.getOpArgs()[0], p.elementToRegionMap.at(op.getOpArgs()[1]));

      // assignment could have invalidated canonicality of either the old region
      // of op.getOpArgs()[0] or the region of op.getOpArgs()[1], or both
      p.canonical = false;
      return;
    case PartitionOpKind::AssignFresh:
      assert(op.getOpArgs().size() == 1 &&
             "AssignFresh PartitionOp should be passed 1 argument");

      p.addElement(op.getOpArgs()[0]);
      return;
    case PartitionOpKind::Transfer: {
      assert(op.getOpArgs().size() == 1 &&
             "Transfer PartitionOp should be passed 1 argument");
      assert(p.elementToRegionMap.count(op.getOpArgs()[0]) &&
             "Transfer PartitionOp's argument should already be tracked");

      // check if any nontransferrables are transferred here, and handle the
      // failure if so
      for (Element nonTransferrable : nonTransferrableElements) {
        assert(
            p.elementToRegionMap.count(nonTransferrable) &&
            "nontransferrables should be function args and self, and therefore"
            "always present in the label map because of initialization at "
            "entry");
        if (!p.isTransferred(nonTransferrable) &&
            p.elementToRegionMap.at(nonTransferrable) ==
                p.elementToRegionMap.at(op.getOpArgs()[0])) {
          return handleTransferNonTransferrable(op, nonTransferrable);
        }
      }

      // If this value is actor derived or if any elements in its region are
      // actor derived, we need to treat as nontransferrable.
      if (isActorDerived(op.getOpArgs()[0]))
        return handleTransferNonTransferrable(op, op.getOpArgs()[0]);

      // While we are checking for actor derived, also check if our value or any
      // value in our region is closure captured and propagate that bit in our
      // transferred inst.
      bool isClosureCapturedElt =
          isClosureCaptured(op.getOpArgs()[0], op.getSourceOp());

      Region elementRegion = p.elementToRegionMap.at(op.getOpArgs()[0]);
      for (const auto &pair : p.elementToRegionMap) {
        if (pair.second == elementRegion && isActorDerived(pair.first))
          return handleTransferNonTransferrable(op, op.getOpArgs()[0]);
        isClosureCapturedElt |= isClosureCaptured(pair.first, op.getSourceOp());
      }

      // Mark op.getOpArgs()[0] as transferred.
      p.markTransferred(
          op.getOpArgs()[0],
          ptrSetFactory.get({op.getSourceOp(), isClosureCapturedElt}));
      return;
    }
    case PartitionOpKind::UndoTransfer: {
      assert(op.getOpArgs().size() == 1 &&
             "UndoTransfer PartitionOp should be passed 1 argument");
      assert(p.elementToRegionMap.count(op.getOpArgs()[0]) &&
             "UndoTransfer PartitionOp's argument should already be tracked");

      // Mark op.getOpArgs()[0] as not transferred.
      p.undoTransfer(op.getOpArgs()[0]);
      return;
    }
    case PartitionOpKind::Merge:
      assert(op.getOpArgs().size() == 2 &&
             "Merge PartitionOp should be passed 2 arguments");
      assert(p.elementToRegionMap.count(op.getOpArgs()[0]) &&
             p.elementToRegionMap.count(op.getOpArgs()[1]) &&
             "Merge PartitionOp's arguments should already be tracked");

      // if attempting to merge a transferred region, handle the failure
      if (auto *transferredOperandSet = p.getTransferred(op.getOpArgs()[0])) {
        for (auto transferredOperand : transferredOperandSet->data()) {
          handleFailure(op, op.getOpArgs()[0], transferredOperand);
        }
      }
      if (auto *transferredOperandSet = p.getTransferred(op.getOpArgs()[1])) {
        for (auto transferredOperand : transferredOperandSet->data()) {
          handleFailure(op, op.getOpArgs()[1], transferredOperand);
        }
      }

      p.merge(op.getOpArgs()[0], op.getOpArgs()[1]);
      return;
    case PartitionOpKind::Require:
      assert(op.getOpArgs().size() == 1 &&
             "Require PartitionOp should be passed 1 argument");
      assert(p.elementToRegionMap.count(op.getOpArgs()[0]) &&
             "Require PartitionOp's argument should already be tracked");
      if (auto *transferredOperandSet = p.getTransferred(op.getOpArgs()[0])) {
        for (auto transferredOperand : transferredOperandSet->data()) {
          handleFailure(op, op.getOpArgs()[0], transferredOperand);
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

} // namespace swift

#endif // SWIFT_PARTITIONUTILS_H
