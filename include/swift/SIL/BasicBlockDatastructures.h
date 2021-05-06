//===--- BasicBlockDatastructures.h -----------------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines efficient data structures for working with BasicBlocks.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_BASICBLOCKDATASTRUCTURES_H
#define SWIFT_SIL_BASICBLOCKDATASTRUCTURES_H

#include "swift/SIL/StackList.h"
#include "swift/SIL/BasicBlockBits.h"

namespace swift {

/// An implementation of `llvm::SetVector<SILBasicBlock *,
///                                       StackList<SILBasicBlock *>,
///                                       BasicBlockSet>`.
///
/// Unfortunately it's not possible to use `llvm::SetVector` directly because
/// the BasicBlockSet and StackList constructors needs a `SILFunction` argument.
///
/// Note: This class does not provide a `remove` method intentinally, because
/// it would have a O(n) complexity.
class BasicBlockSetVector {
  StackList<SILBasicBlock *> vector;
  BasicBlockSet set;
  
public:
  using iterator = typename StackList<SILBasicBlock *>::iterator;

  BasicBlockSetVector(SILFunction *function) : vector(function), set(function) {}

  iterator begin() const { return vector.begin(); }
  iterator end() const { return vector.end(); }

  bool empty() const { return vector.empty(); }

  bool contains(SILBasicBlock *block) const { return set.contains(block); }

  /// Returns true if \p block was not contained in the set before inserting.
  bool insert(SILBasicBlock *block) {
    if (set.insert(block)) {
      vector.push_back(block);
      return true;
    }
    return false;
  }
};

/// A utility for processing basic blocks in a worklist.
///
/// It is basically a combination of a block vector and a block set. It can be
/// used for typical worklist-processing algorithms.
class BasicBlockWorklist {
  StackList<SILBasicBlock *> worklist;
  BasicBlockSet visited;
  
public:
  /// Construct an empty worklist.
  BasicBlockWorklist(SILFunction *function)
    : worklist(function), visited(function) {}

  /// Initialize the worklist with \p initialBlock.
  BasicBlockWorklist(SILBasicBlock *initialBlock)
      : BasicBlockWorklist(initialBlock->getParent()) {
    push(initialBlock);
  }

  /// Pops the last added element from the worklist or returns null, if the
  /// worklist is empty.
  SILBasicBlock *pop() {
    if (worklist.empty())
      return nullptr;
    return worklist.pop_back_val();
  }

  /// Pushes \p block onto the worklist if \p block has never been push before.
  bool pushIfNotVisited(SILBasicBlock *block) {
    if (visited.insert(block)) {
      worklist.push_back(block);
      return true;
    }
    return false;
  }

  /// Like `pushIfNotVisited`, but requires that \p block has never been on the
  /// worklist before.
  void push(SILBasicBlock *block) {
    assert(!visited.contains(block));
    visited.insert(block);
    worklist.push_back(block);
  }

  /// Like `pop`, but marks the returned block as "unvisited". This means, that
  /// the block can be pushed onto the worklist again.
  SILBasicBlock *popAndForget() {
    if (worklist.empty())
      return nullptr;
    SILBasicBlock *block = worklist.pop_back_val();
    visited.erase(block);
    return block;
  }

  /// Returns true if \p block was visited, i.e. has been added to the worklist.
  bool isVisited(SILBasicBlock *block) const { return visited.contains(block); }
};

} // namespace swift

#endif
