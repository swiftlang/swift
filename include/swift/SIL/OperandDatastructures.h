//===--- OperanDatastructures.h -------------------------------------------===//
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
//
// This file defines efficient data structures for working with Operands.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_OPERANDDATASTRUCTURES_H
#define SWIFT_SIL_OPERANDDATASTRUCTURES_H

#include "swift/SIL/OperandBits.h"
#include "swift/SIL/StackList.h"

namespace swift {

/// An implementation of `llvm::SetVector<Operand *,
///                                       StackList<Operand *>,
///                                       OperandSet>`.
///
/// Unfortunately it's not possible to use `llvm::SetVector` directly because
/// the OperandSet and StackList constructors needs a `SILFunction`
/// argument.
///
/// Note: This class does not provide a `remove` method intentionally, because
/// it would have a O(n) complexity.
class OperandSetVector {
  StackList<Operand *> vector;
  OperandSet set;

public:
  using iterator = typename StackList<Operand *>::iterator;

  OperandSetVector(SILFunction *function) : vector(function), set(function) {}

  iterator begin() const { return vector.begin(); }
  iterator end() const { return vector.end(); }

  llvm::iterator_range<iterator> getRange() const {
    return llvm::make_range(begin(), end());
  }

  bool empty() const { return vector.empty(); }

  bool contains(Operand *instruction) const {
    return set.contains(instruction);
  }

  /// Returns true if \p instruction was not contained in the set before
  /// inserting.
  bool insert(Operand *instruction) {
    if (set.insert(instruction)) {
      vector.push_back(instruction);
      return true;
    }
    return false;
  }
};

/// A utility for processing instructions in a worklist.
///
/// It is basically a combination of an instruction vector and an instruction
/// set. It can be used for typical worklist-processing algorithms.
class OperandWorklist {
  StackList<Operand *> worklist;
  OperandSet visited;

public:
  /// Construct an empty worklist.
  OperandWorklist(SILFunction *function)
      : worklist(function), visited(function) {}

  /// Initialize the worklist with \p initialOperand.
  OperandWorklist(Operand *initialOperand)
      : OperandWorklist(initialOperand->getUser()->getFunction()) {
    push(initialOperand);
  }

  /// Pops the last added element from the worklist or returns null, if the
  /// worklist is empty.
  Operand *pop() {
    if (worklist.empty())
      return nullptr;
    return worklist.pop_back_val();
  }

  /// Pushes \p operand onto the worklist if \p operand has never been
  /// push before.
  bool pushIfNotVisited(Operand *operand) {
    if (visited.insert(operand)) {
      worklist.push_back(operand);
      return true;
    }
    return false;
  }

  /// Pushes the operands of all uses of \p instruction onto the worklist if the
  /// operands have never been pushed before. Returns \p true if we inserted
  /// /any/ values.
  ///
  /// This is a bulk convenience API.
  bool pushResultOperandsIfNotVisited(SILInstruction *inst) {
    bool insertedOperand = false;
    for (auto result : inst->getResults()) {
      for (auto *use : result->getUses()) {
        insertedOperand |= pushIfNotVisited(use);
      }
    }
    return insertedOperand;
  }

  /// Like `pushIfNotVisited`, but requires that \p operand has never been
  /// on the worklist before.
  void push(Operand *operand) {
    assert(!visited.contains(operand));
    visited.insert(operand);
    worklist.push_back(operand);
  }

  /// Like `pop`, but marks the returned operand as "unvisited". This means,
  /// that the operand can be pushed onto the worklist again.
  Operand *popAndForget() {
    if (worklist.empty())
      return nullptr;
    Operand *operand = worklist.pop_back_val();
    visited.erase(operand);
    return operand;
  }

  /// Returns true if \p operand was visited, i.e. has been added to the
  /// worklist.
  bool isVisited(Operand *operand) const { return visited.contains(operand); }
};

} // namespace swift

#endif
