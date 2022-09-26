//===--- NodeDatastructures.h -----------------------------------*- C++ -*-===//
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
// This file defines efficient data structures for working with Nodes.
//
// TODO: Add an InstructionWorklist similar to BasicBlockWorklist.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_NODEDATASTRUCTURES_H
#define SWIFT_SIL_NODEDATASTRUCTURES_H

#include "swift/SIL/NodeBits.h"
#include "swift/SIL/StackList.h"

namespace swift {

/// An implementation of `llvm::SetVector<SILNode *,
///                                       StackList<SILNode *>,
///                                       NodeSet>`.
///
/// Unfortunately it's not possible to use `llvm::SetVector` directly because
/// the ValueSet and StackList constructors needs a `SILFunction` argument.
///
/// Note: This class does not provide a `remove` method intentionally, because
/// it would have a O(n) complexity.
class NodeSetVector {
  StackList<SILNode *> vector;
  NodeSet set;

public:
  using iterator = typename StackList<SILNode *>::iterator;

  NodeSetVector(SILFunction *function) : vector(function), set(function) {}

  iterator begin() const { return vector.begin(); }
  iterator end() const { return vector.end(); }

  llvm::iterator_range<iterator> getRange() const {
    return llvm::make_range(begin(), end());
  }

  bool empty() const { return vector.empty(); }

  bool contains(SILNode *node) const { return set.contains(node); }

  /// Returns true if \p value was not contained in the set before inserting.
  bool insert(SILNode *node) {
    if (set.insert(node)) {
      vector.push_back(node);
      return true;
    }
    return false;
  }
};

/// An implementation of `llvm::SetVector<SILValue,
///                                       StackList<SILValue>,
///                                       ValueSet>`.
///
/// Unfortunately it's not possible to use `llvm::SetVector` directly because
/// the ValueSet and StackList constructors needs a `SILFunction` argument.
///
/// Note: This class does not provide a `remove` method intentionally, because
/// it would have a O(n) complexity.
class ValueSetVector {
  StackList<SILValue> vector;
  ValueSet set;

public:
  using iterator = typename StackList<SILValue>::iterator;

  ValueSetVector(SILFunction *function) : vector(function), set(function) {}

  iterator begin() const { return vector.begin(); }
  iterator end() const { return vector.end(); }

  llvm::iterator_range<iterator> getRange() const {
    return llvm::make_range(begin(), end());
  }

  bool empty() const { return vector.empty(); }

  bool contains(SILValue value) const { return set.contains(value); }

  /// Returns true if \p value was not contained in the set before inserting.
  bool insert(SILValue value) {
    if (set.insert(value)) {
      vector.push_back(value);
      return true;
    }
    return false;
  }
};

} // namespace swift

#endif
