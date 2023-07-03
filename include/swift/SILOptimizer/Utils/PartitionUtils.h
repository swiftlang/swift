#ifndef SWIFT_PARTITIONUTILS_H
#define SWIFT_PARTITIONUTILS_H

#include "llvm/Support/Debug.h"
#include "llvm/ADT/SmallVector.h"

namespace swift {

enum class PartitionOpKind : uint8_t {
  Assign,
  AssignFresh,
  Consume,
  Merge,
  Require
};

class PartitionOp {
private:
  PartitionOpKind OpKind;
  llvm::SmallVector<unsigned, 2> OpArgs;

  // TODO: can the following two declarations be merged?
  PartitionOp(PartitionOpKind OpKind, unsigned arg1)
      : OpKind(OpKind), OpArgs({arg1}) {}

  PartitionOp(PartitionOpKind OpKind, unsigned arg1, unsigned arg2)
      : OpKind(OpKind), OpArgs({arg1, arg2}) {}

  friend class Partition;

public:
  static PartitionOp Assign(unsigned tgt, unsigned src) {
    return PartitionOp(PartitionOpKind::Assign, tgt, src);
  }

  static PartitionOp AssignFresh(unsigned tgt) {
    return PartitionOp(PartitionOpKind::AssignFresh, tgt);
  }

  static PartitionOp Consume(unsigned tgt) {
    return PartitionOp(PartitionOpKind::Consume, tgt);
  }

  static PartitionOp Merge(unsigned tgt1, unsigned tgt2) {
    return PartitionOp(PartitionOpKind::Merge, tgt1, tgt2);
  }

  static PartitionOp Require(unsigned tgt) {
    return PartitionOp(PartitionOpKind::Require, tgt);
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
  std::map<unsigned, signed> labels = {};

  bool canonical = true;

  // linear time - For each region label that occurs, find the first index
  // at which it occurs and relabel all instances of it to that index.
  // This excludes the -1 label for missing region.
  void canonicalize() {
    if (canonical)
      return;
    canonical = true;

    std::map<signed, unsigned> relabel;

    for (auto &[i, label] : labels) {
      // leave -1 (missing region) as is
      if (label < 0)
        continue;

      if (!relabel.count(label)) {
        // if this is the first time encountering this region label,
        // then this region label should be relabelled to this index,
        // so enter that into the map
        relabel[label] = i;
      }

      label = relabel[label];
    }
  }

public:
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
      joined.labels[i] = lookup_fst(i);
    }

    return joined;
  }

  // It's possible for all PartitionOps' to maintain canonicality,
  // but it comes at the cost of making Assign operations worst-case
  // linear time instead of constant. This is likely not worth it,
  // so it's disabled by default, but leaving this flag here in case
  // it becomes useful as a performance optimization.
  static const bool ALWAYS_CANONICAL = false;

  void apply(
      PartitionOp op, std::function<void(const PartitionOp &)> handleFailure =
                          [](const PartitionOp &_) {}) {
    switch (op.OpKind) {
    case PartitionOpKind::Assign:
      assert(op.OpArgs.size() == 2 &&
             "Assign PartitionOp should be passed 2 arguments");
      assert(labels.count(op.OpArgs[0]) && labels.count(op.OpArgs[1]) &&
             "Assign PartitionOp's arguments should be already tracked");
      // if assigning to a missing region, handle the failure
      if (labels[op.OpArgs[1]] < 0)
        handleFailure(op);

      labels[op.OpArgs[0]] = labels[op.OpArgs[1]];

      if (ALWAYS_CANONICAL) {
        // if seeking to maintain canonicality, then do so
        if (op.OpArgs[0] < labels[op.OpArgs[0]])
          horizontalUpdate(labels, op.OpArgs[0], op.OpArgs[0]);
        break;
      }

      // assignment could have invalidated canonicality
      canonical = false;
      break;
    case PartitionOpKind::AssignFresh:
      assert(op.OpArgs.size() == 1 &&
             "AssignFresh PartitionOp should be passed 1 argument");
      assert(!labels.count(op.OpArgs[0]) &&
             "AssignFresh PartitionOp's argument should NOT already be tracked");

      // fresh region generated by mapping the passed index to itself
      labels[op.OpArgs[0]] = op.OpArgs[0];
      break;
    case PartitionOpKind::Consume:
      assert(op.OpArgs.size() == 1 &&
             "Consume PartitionOp should be passed 1 argument");
      assert(labels.count(op.OpArgs[0]) &&
             "Consume PartitionOp's argument should already be tracked");

      // if attempting to consume a missing region, handle the failure
      if (labels[op.OpArgs[0]] < 0)
        handleFailure(op);

      // mark region as missing
      horizontalUpdate(labels, op.OpArgs[0], -1);
      break;
    case PartitionOpKind::Merge:
      assert(op.OpArgs.size() == 2 &&
             "Merge PartitionOp should be passed 2 arguments");
      assert(labels.count(op.OpArgs[0]) && labels.count(op.OpArgs[1]) &&
             "Merge PartitionOp's arguments should already be tracked");
      // if attempting to merge a missing region, handle the failure
      if (labels[op.OpArgs[0]] < 0 || labels[op.OpArgs[1]] < 0)
        handleFailure(op);

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
        handleFailure(op);
    }
  }
};
}

#endif
