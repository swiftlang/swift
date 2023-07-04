#ifndef SWIFT_PARTITIONUTILS_H
#define SWIFT_PARTITIONUTILS_H

#include "swift/Basic/LLVM.h"
#include "swift/SIL/SILInstruction.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Support/Debug.h"
#include <algorithm>

namespace swift {

enum class PartitionOpKind : uint8_t {
  // Assign one value to the region of another, takes two args, second arg
  // must already be tracked with a non-consumed region
  Assign,
  // Assign one value to a fresh region, takes one arg.
  AssignFresh,
  // Consume the region of a value, takes one arg
  Consume,
  Merge,
  Require
};

// PartitionOp represents a primitive operation that can be performed on
// Partitions. This is part of the SendNonSendable SIL pass workflow:
// first SILBasicBlocks are compiled to vectors of PartitionOps, then a fixed
// point partition is found over the CFG.
class PartitionOp {
private:
  PartitionOpKind OpKind;
  llvm::SmallVector<unsigned, 2> OpArgs;

  // record the SILInstruction that this PartitionOp was generated from, if
  // generated during compilation from a SILBasicBlock
  SILInstruction *sourceInst;

  // TODO: can the following declarations be merged?
  PartitionOp(PartitionOpKind OpKind, unsigned arg1,
              SILInstruction *sourceInst = nullptr)
      : OpKind(OpKind), OpArgs({arg1}), sourceInst(sourceInst) {}

  PartitionOp(PartitionOpKind OpKind, unsigned arg1, unsigned arg2,
              SILInstruction *sourceInst = nullptr)
      : OpKind(OpKind), OpArgs({arg1, arg2}), sourceInst(sourceInst) {}

  friend class Partition;

public:
  static PartitionOp Assign(unsigned tgt, unsigned src,
                            SILInstruction *sourceInst = nullptr) {
    return PartitionOp(PartitionOpKind::Assign, tgt, src, sourceInst);
  }

  static PartitionOp AssignFresh(unsigned tgt,
                                 SILInstruction *sourceInst = nullptr) {
    return PartitionOp(PartitionOpKind::AssignFresh, tgt, sourceInst);
  }

  static PartitionOp Consume(unsigned tgt,
                             SILInstruction *sourceInst = nullptr) {
    return PartitionOp(PartitionOpKind::Consume, tgt, sourceInst);
  }

  static PartitionOp Merge(unsigned tgt1, unsigned tgt2,
                           SILInstruction *sourceInst = nullptr) {
    return PartitionOp(PartitionOpKind::Merge, tgt1, tgt2, sourceInst);
  }

  static PartitionOp Require(unsigned tgt,
                             SILInstruction *sourceInst = nullptr) {
    return PartitionOp(PartitionOpKind::Require, tgt, sourceInst);
  }

  SILInstruction *getSourceInst() const {
    return sourceInst;
  }

  void dump() const {
    switch (OpKind) {
    case PartitionOpKind::Assign:
      llvm::dbgs() << "assign %" << OpArgs[0] << " = %" << OpArgs[1] << "\n";
      break;
    case PartitionOpKind::AssignFresh:
      llvm::dbgs() << "assign_fresh %" << OpArgs[0] << "\n";
      break;
    case PartitionOpKind::Consume:
      llvm::dbgs() << "consume %" << OpArgs[0] << "\n";
      break;
    case PartitionOpKind::Merge:
      llvm::dbgs() << "merge %" << OpArgs[0] << " with %" << OpArgs[1] << "\n";
      break;
    case PartitionOpKind::Require:
      llvm::dbgs() << "require %" << OpArgs[0] << "\n";
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
static void horizontalUpdate(std::map<unsigned, signed> &map, unsigned key,
                             signed val) {
  if (!map.count(key)) {
    map[key] = val;
    return;
  }

  signed oldVal = map[key];

  for (auto [otherKey, otherVal] : map)
    if (otherVal == oldVal)
      map[otherKey] = val;
}

class Partition {
private:
  // label each index with a non-negative (unsigned) label if it is associated
  // with a valid region, and with -1 if it is associated with a consumed region
  // in-order traversal relied upon
  std::map<unsigned, signed> labels;

  // track a label that is guaranteed to be fresh
  unsigned fresh_label = 0;

  // in a canonical partition, all regions are labelled with the smallest index
  // of any member. Certain operations like join and equals rely on canonicality
  // so when it's invalidated this boolean tracks that, and it must be
  // reestablished by a call to canonicalize()
  bool canonical;

  // linear time - For each region label that occurs, find the first index
  // at which it occurs and relabel all instances of it to that index.
  // This excludes the -1 label for consumed regions.
  void canonicalize() {
    if (canonical)
      return;
    canonical = true;

    std::map<signed, unsigned> relabel;

    // relies on in-order traversal of labels
    for (auto &[i, label] : labels) {
      // leave -1 (consumed region) as is
      if (label < 0)
        continue;

      if (!relabel.count(label)) {
        // if this is the first time encountering this region label,
        // then this region label should be relabelled to this index,
        // so enter that into the map
        relabel[label] = i;
      }

      // update this label with either its own index, or a prior index that
      // shared a region with it
      label = relabel[label];

      // the maximum index iterated over will be used here to appropriately
      // set fresh_label
      fresh_label = i + 1;
    }
  }

public:
  Partition() : labels({}), canonical(true) {}

  static Partition singleRegion(std::vector<unsigned> indices) {
    Partition p;
    if (!indices.empty()) {
      unsigned min_index = *std::min_element(indices.begin(), indices.end());
      p.fresh_label = min_index + 1;
      for (unsigned index : indices) {
        p.labels[index] = min_index;
      }
    }
    return p;
  }

  void dump() const {
    llvm::dbgs() << "Partition";
    if (canonical)
      llvm::dbgs() << "(canonical)";
    llvm::dbgs() << "{";
    for (const auto &[i, label] : labels)
      llvm::dbgs() << "[" << i << ": " << label << "] ";
    llvm::dbgs() << "}\n";
  }

  // linear time - Test two partititons for equality by first putting them
  // in canonical form then comparing for exact equality.
  static bool equals(Partition &fst, Partition &snd) {
    fst.canonicalize();
    snd.canonicalize();

    return fst.labels == snd.labels;
  }

  // quadratic time - Construct the partition corresponding to the join of the
  // two passed partitions; the join labels each index labelled by both operands
  // and two indices are in the same region of the join iff they are in the same
  // region in either operand.
  static Partition join(Partition &fst, Partition &snd) {
    fst.canonicalize();
    snd.canonicalize();

    std::map<unsigned, signed> relabel_fst;
    std::map<unsigned, signed> relabel_snd;
    auto lookup_fst = [&](unsigned i) {
      // signed to unsigned conversion... ?
      return relabel_fst.count(fst.labels[i]) ? relabel_fst[fst.labels[i]]
                                              : fst.labels[i];
    };

    auto lookup_snd = [&](unsigned i) {
      // signed to unsigned conversion... safe?
      return relabel_snd.count(snd.labels[i]) ? relabel_snd[snd.labels[i]]
                                              : snd.labels[i];
    };

    for (const auto &[i, _] : fst.labels) {
      // only consider indices present in both fst and snd
      if (!snd.labels.count(i))
        continue;

      signed label_joined = std::min(lookup_fst(i), lookup_snd(i));

      horizontalUpdate(relabel_fst, fst.labels[i], label_joined);
      horizontalUpdate(relabel_snd, snd.labels[i], label_joined);
    }

    Partition joined;
    joined.canonical = true;
    for (const auto &[i, _] : fst.labels) {
      if (!snd.labels.count(i))
        continue;
      signed label_i = lookup_fst(i);
      joined.labels[i] = label_i;
      joined.fresh_label = std::max(joined.fresh_label, (unsigned) label_i + 1);
    }

    return joined;
  }

  // Apply the passed PartitionOp to this partition, performing its action.
  // A `handleFailure` closure can optionally be passed in that will be called
  // if a consumed region is required. The closure is given the PartitionOp that
  // failed, and the index of the SIL value that was required but consumed.
  void apply(
      PartitionOp op, llvm::function_ref<void(const PartitionOp&, unsigned)> handleFailure =
                          [](const PartitionOp&, unsigned) {}) {
    switch (op.OpKind) {
    case PartitionOpKind::Assign:
      assert(op.OpArgs.size() == 2 &&
             "Assign PartitionOp should be passed 2 arguments");
      assert(labels.count(op.OpArgs[1]) &&
             "Assign PartitionOp's source argument should be already tracked");
      // if assigning to a missing region, handle the failure
      if (labels[op.OpArgs[1]] < 0)
        handleFailure(op, op.OpArgs[1]);

      labels[op.OpArgs[0]] = labels[op.OpArgs[1]];

      // assignment could have invalidated canonicality of either the old region
      // of op.OpArgs[0] or the region of op.OpArgs[1], or both
      canonical = false;
      break;
    case PartitionOpKind::AssignFresh:
      assert(op.OpArgs.size() == 1 &&
             "AssignFresh PartitionOp should be passed 1 argument");
      assert(!labels.count(op.OpArgs[0]) &&
             "AssignFresh PartitionOp's argument should NOT already be tracked");

      // map index op.OpArgs[0] to a fresh label
      labels[op.OpArgs[0]] = fresh_label++;
      canonical = false;
      break;
    case PartitionOpKind::Consume:
      assert(op.OpArgs.size() == 1 &&
             "Consume PartitionOp should be passed 1 argument");
      assert(labels.count(op.OpArgs[0]) &&
             "Consume PartitionOp's argument should already be tracked");

      // if attempting to consume a consumed region, handle the failure
      if (labels[op.OpArgs[0]] < 0)
        handleFailure(op, op.OpArgs[0]);

      // mark region as consumed
      horizontalUpdate(labels, op.OpArgs[0], -1);
      break;
    case PartitionOpKind::Merge:
      assert(op.OpArgs.size() == 2 &&
             "Merge PartitionOp should be passed 2 arguments");
      assert(labels.count(op.OpArgs[0]) && labels.count(op.OpArgs[1]) &&
             "Merge PartitionOp's arguments should already be tracked");
      // if attempting to merge a consumed region, handle the failure
      if (labels[op.OpArgs[0]] < 0)
          handleFailure(op, op.OpArgs[0]);
      if (labels[op.OpArgs[1]] < 0)
          handleFailure(op, op.OpArgs[1]);

      if (labels[op.OpArgs[0]] == labels[op.OpArgs[1]])
        break;

      // maintain canonicality by renaming the greater-numbered region
      if (labels[op.OpArgs[0]] < labels[op.OpArgs[1]])
        horizontalUpdate(labels, op.OpArgs[1], labels[op.OpArgs[0]]);
      else
        horizontalUpdate(labels, op.OpArgs[0], labels[op.OpArgs[1]]);
      break;
    case PartitionOpKind::Require:
      assert(op.OpArgs.size() == 1 &&
             "Require PartitionOp should be passed 1 argument");
      assert(labels.count(op.OpArgs[0]) &&
             "Require PartitionOp's argument should already be tracked");
      if (labels[op.OpArgs[0]] < 0)
        handleFailure(op, op.OpArgs[0]);
    }
  }

  void dump() {
    std::map<signed, std::vector<unsigned>> buckets;

    for (auto [i, label] : labels) {
      buckets[label].push_back(i);
    }

    llvm::dbgs() << "[";
    for (auto [label, indices] : buckets) {
      llvm::dbgs() << (label < 0 ? "{" : "(");
      for (unsigned i : indices) {
        llvm::dbgs() << i << " ";
      }
      llvm::dbgs() << (label < 0 ? "}" : ")");
    }
    llvm::dbgs() << "]\n";
  }
};
}

#endif
