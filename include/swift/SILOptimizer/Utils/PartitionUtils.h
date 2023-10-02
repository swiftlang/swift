#ifndef SWIFT_PARTITIONUTILS_H
#define SWIFT_PARTITIONUTILS_H

#include "swift/Basic/Defer.h"
#include "swift/Basic/LLVM.h"
#include "swift/SIL/SILInstruction.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Support/Debug.h"
#include <algorithm>

#define DEBUG_TYPE "send-non-sendable"

namespace swift {

namespace PartitionPrimitives {

#ifndef NDEBUG
extern bool REGIONBASEDISOLATION_ENABLE_VERBOSE_LOGGING;
#define REGIONBASEDISOLATION_VERBOSE_LOG(...)                                  \
  do {                                                                         \
    if (REGIONBASEDISOLATION_ENABLE_VERBOSE_LOGGING) {                         \
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
  signed num;

  explicit Region(int num) : num(num) {
    assert(num >= -1 && "-1 is the only valid negative Region label");
  }

  bool operator==(const Region &other) const { return num == other.num; }
  bool operator<(const Region &other) const { return num < other.num; }

  operator signed() const { return num; }

  bool isTransferred() const { return num < 0; }

  static Region transferred() { return Region(-1); }
};
}

using namespace PartitionPrimitives;

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

  /// Consume the region of a value if not already transferred, takes one arg.
  Transfer,

  /// Require the region of a value to be non-transferred, takes one arg.
  Require,
};

// PartitionOp represents a primitive operation that can be performed on
// Partitions. This is part of the SendNonSendable SIL pass workflow:
// first SILBasicBlocks are compiled to vectors of PartitionOps, then a fixed
// point partition is found over the CFG.
class PartitionOp {
private:
  PartitionOpKind OpKind;
  llvm::SmallVector<Element, 2> OpArgs;

  // Record the SILInstruction that this PartitionOp was generated from, if
  // generated during compilation from a SILBasicBlock
  SILInstruction *sourceInst;

  // Record an AST expression corresponding to this PartitionOp, currently
  // populated only for Consume expressions to indicate the value being
  // transferred
  Expr *sourceExpr;

  // TODO: can the following declarations be merged?
  PartitionOp(PartitionOpKind OpKind, Element arg1,
              SILInstruction *sourceInst = nullptr,
              Expr* sourceExpr = nullptr)
      : OpKind(OpKind), OpArgs({arg1}),
        sourceInst(sourceInst), sourceExpr(sourceExpr) {}

  PartitionOp(PartitionOpKind OpKind, Element arg1, Element arg2,
              SILInstruction *sourceInst = nullptr,
              Expr* sourceExpr = nullptr)
      : OpKind(OpKind), OpArgs({arg1, arg2}),
        sourceInst(sourceInst), sourceExpr(sourceExpr) {}

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

  static PartitionOp Transfer(Element tgt, SILInstruction *sourceInst = nullptr,
                              Expr *sourceExpr = nullptr) {
    return PartitionOp(PartitionOpKind::Transfer, tgt, sourceInst, sourceExpr);
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
      return OpKind == other.OpKind
             && OpArgs == other.OpArgs
                && sourceInst == other.sourceInst;
  };
  // implemented for insertion into std::map
  bool operator<(const PartitionOp &other) const {
    if (OpKind != other.OpKind)
      return OpKind < other.OpKind;
    if (OpArgs != other.OpArgs)
      return OpArgs < other.OpArgs;
    return sourceInst < other.sourceInst;
  }

  PartitionOpKind getKind() const { return OpKind; }

  ArrayRef<Element> getOpArgs() const { return OpArgs; }

  SILInstruction *getSourceInst(bool assertNonNull = false) const {
    assert(!assertNonNull ||
           sourceInst && "PartitionOps should be assigned SILInstruction"
                         " sources when used for the core analysis");
    return sourceInst;
  }

  Expr *getSourceExpr() const {
    return sourceExpr;
  }

  SWIFT_DEBUG_DUMP { print(llvm::dbgs()); }

  void print(llvm::raw_ostream &os) const {
    switch (OpKind) {
    case PartitionOpKind::Assign:
      os << "assign %%" << OpArgs[0] << " = %%" << OpArgs[1] << "\n";
      break;
    case PartitionOpKind::AssignFresh:
      os << "assign_fresh %%" << OpArgs[0] << "\n";
      break;
    case PartitionOpKind::Transfer:
      os << "transfer %%" << OpArgs[0] << "\n";
      break;
    case PartitionOpKind::Merge:
      os << "merge %%" << OpArgs[0] << " with %%" << OpArgs[1] << "\n";
      break;
    case PartitionOpKind::Require:
      os << "require %%" << OpArgs[0] << "\n";
      break;
    }
  }
};

// For the passed `map`, ensure that `key` maps to `val`. If `key` already
// mapped to a different value, ensure that all other keys mapped to that
// value also now map to `val`. This is a relatively expensive (linear time)
// operation that's unfortunately used pervasively throughout PartitionOp
// application. If this is a performance bottleneck, let's consider optimizing
// it to a true union-find or other tree-based data structure.
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

class Partition {
private:
  // Label each index with a non-negative (unsigned) label if it is associated
  // with a valid region, and with -1 if it is associated with a transferred
  // region in-order traversal relied upon.
  std::map<Element, Region> labels;

  // Track a label that is guaranteed to be strictly larger than all in use,
  // and therefore safe for use as a fresh label.
  Region fresh_label = Region(0);

  // In a canonical partition, all regions are labelled with the smallest index
  // of any member. Certain operations like join and equals rely on canonicality
  // so when it's invalidated this boolean tracks that, and it must be
  // reestablished by a call to canonicalize().
  bool canonical;

  // Used only in assertions, check that Partitions promised to be canonical
  // are actually canonical
  bool is_canonical_correct() {
    if (!canonical)
      return true; // vacuously correct

    auto fail = [&](Element i, int type) {
      llvm::dbgs() << "FAIL(i=" << i << "; type=" << type << "): ";
      print(llvm::dbgs());
      return false;
    };

    for (auto &[i, label] : labels) {
      // correctness vacuous at transferred indices
      if (label.isTransferred())
        continue;

      // this label should not exceed fresh_label
      if (label >= fresh_label)
        return fail(i, 0);

      // the label of a region should be at most as large as each index in it
      if ((unsigned)label > i)
        return fail(i, 1);

      // each region label should also be an element of the partition
      if (!labels.count(Element(label)))
        return fail(i, 2);

      // each element that is also a region label should be mapped to itself
      if (labels.at(Element(label)) != label)
        return fail(i, 3);
    }

    return true;
  }

  // linear time - For each region label that occurs, find the first index
  // at which it occurs and relabel all instances of it to that index.
  // This excludes the -1 label for transferred regions.
  void canonicalize() {
    if (canonical)
      return;
    canonical = true;

    std::map<Region, Region> relabel;

    // relies on in-order traversal of labels
    for (auto &[i, label] : labels) {
      // leave -1 (transferred region) as is
      if (label.isTransferred())
        continue;

      if (!relabel.count(label)) {
        // if this is the first time encountering this region label,
        // then this region label should be relabelled to this index,
        // so enter that into the map
        relabel.insert_or_assign(label, Region(i));
      }

      // update this label with either its own index, or a prior index that
      // shared a region with it
      label = relabel.at(label);

      // the maximum index iterated over will be used here to appropriately
      // set fresh_label
      fresh_label = Region(i + 1);
    }

    assert(is_canonical_correct());
  }

  // linear time - merge the regions of two indices, maintaining canonicality
  void merge(Element fst, Element snd) {
    assert(labels.count(fst) && labels.count(snd));
    if (labels.at(fst) == labels.at(snd))
      return;

    // maintain canonicality by renaming the greater-numbered region
    if (labels.at(fst) < labels.at(snd))
      horizontalUpdate(labels, snd, labels.at(fst));
    else
      horizontalUpdate(labels, fst, labels.at(snd));

    assert(is_canonical_correct());
    assert(labels.at(fst) == labels.at(snd));
  }

public:
  Partition() : labels({}), canonical(true) {}

  // 1-arg constructor used when canonicality will be immediately invalidated,
  // so set to false to begin with
  Partition(bool canonical) : labels({}), canonical(canonical) {}

  static Partition singleRegion(ArrayRef<Element> indices) {
    Partition p;
    if (!indices.empty()) {
      Region min_index =
          Region(*std::min_element(indices.begin(), indices.end()));
      p.fresh_label = Region(min_index + 1);
      for (Element index : indices) {
        p.labels.insert_or_assign(index, min_index);
      }
    }

    assert(p.is_canonical_correct());
    return p;
  }

  // linear time - Test two partititons for equality by first putting them
  // in canonical form then comparing for exact equality.
  static bool equals(Partition &fst, Partition &snd) {
    fst.canonicalize();
    snd.canonicalize();

    return fst.labels == snd.labels;
  }

  bool isTracked(Element val) const { return labels.count(val); }

  bool isTransferred(Element val) const {
    return isTracked(val) && labels.at(val).isTransferred();
  }

  // quadratic time - Construct the partition corresponding to the join of the
  // two passed partitions; the join labels each index labelled by both operands
  // and two indices are in the same region of the join iff they are in the same
  // region in either operand.
  static Partition join(Partition &fst, Partition &snd) {
    // ensure copies are made
    Partition fst_reduced = false;
    Partition snd_reduced = false;

    // make canonical copies of fst and snd, reduced to their intersected domain
    for (auto [i, _] : fst.labels)
      if (snd.labels.count(i)) {
        fst_reduced.labels.insert_or_assign(i, fst.labels.at(i));
        snd_reduced.labels.insert_or_assign(i, snd.labels.at(i));
      }
    fst_reduced.canonicalize();
    snd_reduced.canonicalize();

    // merging each index in fst with its label in snd ensures that all pairs
    // of indices that are in the same region in snd are also in the same region
    // in fst - the desired property
    for (const auto [i, snd_label] : snd_reduced.labels) {
      if (snd_label.isTransferred())
        // if snd says that the region has been transferred, mark it transferred
        // in fst
        horizontalUpdate(fst_reduced.labels, i, Region::transferred());
      else
        fst_reduced.merge(i, Element(snd_label));
    }

    LLVM_DEBUG(llvm::dbgs() << "JOIN PEFORMED: \nFST: ";
               fst.print(llvm::dbgs()); llvm::dbgs() << "SND: ";
               snd.print(llvm::dbgs()); llvm::dbgs() << "RESULT: ";
               fst_reduced.print(llvm::dbgs()););

    assert(fst_reduced.is_canonical_correct());

    // fst_reduced is now the join
    return fst_reduced;
  }

  // Apply the passed PartitionOp to this partition, performing its action.
  // A `handleFailure` closure can optionally be passed in that will be called
  // if a transferred region is required. The closure is given the PartitionOp
  // that failed, and the index of the SIL value that was required but
  // transferred. Additionally, a list of "nonconsumable" indices can be passed
  // in along with a handleConsumeNonConsumable closure. In the event that a
  // region containing one of the nonconsumable indices is transferred, the
  // closure will be called with the offending Consume.
  void apply(
      PartitionOp op,
      llvm::function_ref<void(const PartitionOp &, Element)> handleFailure =
          [](const PartitionOp &, Element) {},
      ArrayRef<Element> nonconsumables = {},
      llvm::function_ref<void(const PartitionOp &, Element)>
          handleConsumeNonConsumable = [](const PartitionOp &, Element) {},
      llvm::function_ref<bool(Element)> isActorDerived = nullptr) {

    REGIONBASEDISOLATION_VERBOSE_LOG(llvm::dbgs() << "Applying: ";
                                     op.print(llvm::dbgs()));
    REGIONBASEDISOLATION_VERBOSE_LOG(llvm::dbgs() << "    Before: ";
                                     print(llvm::dbgs()));
    SWIFT_DEFER {
      REGIONBASEDISOLATION_VERBOSE_LOG(llvm::dbgs() << "    After:  ";
                                       print(llvm::dbgs()));
    };
    switch (op.OpKind) {
    case PartitionOpKind::Assign:
      assert(op.OpArgs.size() == 2 &&
             "Assign PartitionOp should be passed 2 arguments");
      assert(labels.count(op.OpArgs[1]) &&
             "Assign PartitionOp's source argument should be already tracked");
      // if assigning to a missing region, handle the failure
      if (isTransferred(op.OpArgs[1]))
        handleFailure(op, op.OpArgs[1]);

      labels.insert_or_assign(op.OpArgs[0], labels.at(op.OpArgs[1]));

      // assignment could have invalidated canonicality of either the old region
      // of op.OpArgs[0] or the region of op.OpArgs[1], or both
      canonical = false;
      break;
    case PartitionOpKind::AssignFresh:
      assert(op.OpArgs.size() == 1 &&
             "AssignFresh PartitionOp should be passed 1 argument");

      // map index op.OpArgs[0] to a fresh label
      labels.insert_or_assign(op.OpArgs[0], fresh_label);

      // increment the fresh label so it remains fresh
      fresh_label = Region(fresh_label + 1);
      canonical = false;
      break;
    case PartitionOpKind::Transfer: {
      assert(op.OpArgs.size() == 1 &&
             "Consume PartitionOp should be passed 1 argument");
      assert(labels.count(op.OpArgs[0]) &&
             "Consume PartitionOp's argument should already be tracked");

      // check if any nonconsumables are transferred here, and handle the
      // failure if so
      for (Element nonconsumable : nonconsumables) {
        assert(labels.count(nonconsumable) &&
               "nonconsumables should be function args and self, and therefore"
               "always present in the label map because of initialization at "
               "entry");
        if (!isTransferred(nonconsumable) &&
            labels.at(nonconsumable) == labels.at(op.OpArgs[0])) {
          handleConsumeNonConsumable(op, nonconsumable);
          break;
        }
      }

      // If this value is actor derived or if any elements in its region are
      // actor derived, we need to treat as non-consumable.
      if (isActorDerived && isActorDerived(op.OpArgs[0]))
        return handleConsumeNonConsumable(op, op.OpArgs[0]);
      Region elementRegion = labels.at(op.OpArgs[0]);
      if (llvm::any_of(labels,
                       [&](const std::pair<Element, Region> &pair) -> bool {
                         if (pair.second != elementRegion)
                           return false;
                         return isActorDerived && isActorDerived(pair.first);
                       }))
        return handleConsumeNonConsumable(op, op.OpArgs[0]);

      // Ensure if the region is transferred...
      if (!isTransferred(op.OpArgs[0]))
        // that all elements associated with the region are marked as
        // transferred.
        horizontalUpdate(labels, op.OpArgs[0], Region::transferred());
      break;
    }
    case PartitionOpKind::Merge:
      assert(op.OpArgs.size() == 2 &&
             "Merge PartitionOp should be passed 2 arguments");
      assert(labels.count(op.OpArgs[0]) && labels.count(op.OpArgs[1]) &&
             "Merge PartitionOp's arguments should already be tracked");

      // if attempting to merge a transferred region, handle the failure
      if (isTransferred(op.OpArgs[0]))
        handleFailure(op, op.OpArgs[0]);
      if (isTransferred(op.OpArgs[1]))
        handleFailure(op, op.OpArgs[1]);

      merge(op.OpArgs[0], op.OpArgs[1]);
      break;
    case PartitionOpKind::Require:
      assert(op.OpArgs.size() == 1 &&
             "Require PartitionOp should be passed 1 argument");
      assert(labels.count(op.OpArgs[0]) &&
             "Require PartitionOp's argument should already be tracked");
      if (isTransferred(op.OpArgs[0]))
        handleFailure(op, op.OpArgs[0]);
    }

    assert(is_canonical_correct());
  }

  // return a vector of the transferred values in this partition
  std::vector<Element> getTransferredVals() const {
    // for effeciency, this could return an iterator not a vector
    std::vector<Element> transferredVals;
    for (auto [i, _] : labels)
      if (isTransferred(i))
        transferredVals.push_back(i);
    return transferredVals;
  }

  // return a vector of the non-transferred regions in this partition, each
  // represented as a vector of values
  std::vector<std::vector<Element>> getNonTransferredRegions() const {
    // for effeciency, this could return an iterator not a vector
    std::map<Region, std::vector<Element>> buckets;

    for (auto [i, label] : labels)
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
    for (const auto &[i, label] : labels)
      llvm::dbgs() << "[" << i << ": " << label << "] ";
    llvm::dbgs() << "}\n";
  }

  SWIFT_DEBUG_DUMP { print(llvm::dbgs()); }

  void print(llvm::raw_ostream &os) const {
    std::map<Region, std::vector<Element>> buckets;

    for (auto [i, label] : labels)
      buckets[label].push_back(i);

    os << "[";
    for (auto [label, indices] : buckets) {
      os << (label.isTransferred() ? "{" : "(");
      int j = 0;
      for (Element i : indices) {
        os << (j++ ? " " : "") << i;
      }
      os << (label.isTransferred() ? "}" : ")");
    }
    os << "]\n";
  }
};

} // namespace swift

#endif // SWIFT_PARTITIONUTILS_H
