//===--- NodeBits.h ---------------------------------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines utilities for SILNode bit fields and sets.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_NODEBITS_H
#define SWIFT_SIL_NODEBITS_H

#include "swift/SIL/SILBitfield.h"

namespace swift {

/// Utility to add a custom bitfield to a SILInstruction or SILValue.
///
/// This can be used by transforms to store temporary flags or tiny values per
/// instruction or value.
/// The bits are stored in a small bit field (8 bits) within each node (\see
/// SILNode::Bits::customBits) which is very efficient: no memory allocation
/// is needed, no hash set or map is needed for lookup and there is no
/// initialization cost.
///
/// Invariants:
/// * NodeBitfield instances must be allocated and deallocated
///   following a strict stack discipline, because bit-positions in the node's
///   bit field are "allocated" and "freed" with a stack-allocation
///   algorithm. This means, it's fine to use a NodeBitfield as (or in)
///   local variables, e.g. in transformations. But it's not possible to store
///   a NodeBitfield in an Analysis.
/// * The total number of bits which are alive at the same time must not exceed
///   8 (the size of SILNode::Bits::customBits).
class NodeBitfield : public SILBitfield<NodeBitfield, SILNode> {
  template <class, class> friend class SILBitfield;

  NodeBitfield *insertInto(SILFunction *function) {
    NodeBitfield *oldParent = function->newestAliveNodeBitfield;
    function->newestAliveNodeBitfield = this;
    return oldParent;
  }

public:
  NodeBitfield(SILFunction *function, int size) :
    SILBitfield(function, size, insertInto(function)) {}

  ~NodeBitfield() {
    assert(function->newestAliveNodeBitfield == this &&
           "BasicBlockBitfield destructed too early");
    function->newestAliveNodeBitfield = parent;
  }
};

/// A set of SILNodes.
///
/// For details see NodeBitfield.
class NodeSet {
  NodeBitfield bit;
  
public:
  NodeSet(SILFunction *function) : bit(function, 1) {}

  SILFunction *getFunction() const { return bit.getFunction(); }

  bool contains(SILNode *node) const { return (bool)bit.get(node); }

  /// Returns true if \p node was not contained in the set before inserting.
  bool insert(SILNode *node) {
    bool wasContained = contains(node);
    if (!wasContained) {
      bit.set(node, 1);
    }
    return !wasContained;
  }

  void erase(SILNode *node) {
    bit.set(node, 0);
  }
};


/// A set of SILInstructions.
///
/// For details see NodeBitfield.
class InstructionSet {
  NodeSet nodeSet;
  
public:
  using Element = SILInstruction *;

  InstructionSet(SILFunction *function) : nodeSet(function) {}

  SILFunction *getFunction() const { return nodeSet.getFunction(); }

  bool contains(SILInstruction *inst) const { return nodeSet.contains(inst->asSILNode()); }

  /// Returns true if \p inst was not contained in the set before inserting.
  bool insert(SILInstruction *inst) { return nodeSet.insert(inst->asSILNode()); }

  void erase(SILInstruction *inst) { nodeSet.erase(inst->asSILNode()); }
};

using InstructionSetWithSize = KnownSizeSet<InstructionSet>;

/// A set of SILValues.
///
/// For details see NodeBitfield.
class ValueSet {
  NodeSet nodeSet;
  
public:
  using Element = SILValue;

  ValueSet(SILFunction *function) : nodeSet(function) {}

  SILFunction *getFunction() const { return nodeSet.getFunction(); }

  bool contains(SILValue value) const { return nodeSet.contains(value); }

  /// Returns true if \p value was not contained in the set before inserting.
  bool insert(SILValue value) { return nodeSet.insert(value); }

  void erase(SILValue value) { nodeSet.erase(value); }
};

using ValueSetWithSize = KnownSizeSet<ValueSet>;

} // namespace swift

#endif
