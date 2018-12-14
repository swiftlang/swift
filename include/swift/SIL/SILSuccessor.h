//===--- SILSuccessor.h - Terminator Instruction Successor ------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_SILSUCCESSOR_H
#define SWIFT_SIL_SILSUCCESSOR_H

#include "swift/Basic/ProfileCounter.h"
#include <cassert>
#include <cstddef>
#include <iterator>

namespace swift {

class SILBasicBlock;
class TermInst;

/// An edge in the control flow graph.
///
/// A SILSuccessor is stored in the terminator instruction of the tail block of
/// the CFG edge. Internally it has a back reference to the terminator that
/// contains it (ContainingInst). It also contains the SuccessorBlock that is
/// the "head" of the CFG edge. This makes it very simple to iterate over the
/// successors of a specific block.
///
/// SILSuccessor also enables given a "head" edge the ability to iterate over
/// predecessors. This is done by using an ilist that is embedded into
/// SILSuccessors.
class SILSuccessor {
  /// The terminator instruction that contains this SILSuccessor.
  TermInst *ContainingInst = nullptr;
  
  /// If non-null, this is the BasicBlock that the terminator branches to.
  SILBasicBlock *SuccessorBlock = nullptr;

  /// If hasValue, this is the profiled execution count of the edge
  ProfileCounter Count;

  /// A pointer to the SILSuccessor that represents the previous SILSuccessor in the
  /// predecessor list for SuccessorBlock.
  ///
  /// Must be nullptr if SuccessorBlock is.
  SILSuccessor **Prev = nullptr;

  /// A pointer to the SILSuccessor that represents the next SILSuccessor in the
  /// predecessor list for SuccessorBlock.
  ///
  /// Must be nullptr if SuccessorBlock is.
  SILSuccessor *Next = nullptr;

public:
  SILSuccessor(ProfileCounter Count = ProfileCounter()) : Count(Count) {}

  SILSuccessor(TermInst *CI, ProfileCounter Count = ProfileCounter())
      : ContainingInst(CI), Count(Count) {}

  SILSuccessor(TermInst *CI, SILBasicBlock *Succ,
               ProfileCounter Count = ProfileCounter())
      : ContainingInst(CI), Count(Count) {
    *this = Succ;
  }
  
  ~SILSuccessor() {
    *this = nullptr;
  }

  void operator=(SILBasicBlock *BB);
  
  operator SILBasicBlock*() const { return SuccessorBlock; }
  SILBasicBlock *getBB() const { return SuccessorBlock; }

  ProfileCounter getCount() const { return Count; }

  // Do not copy or move these.
  SILSuccessor(const SILSuccessor &) = delete;
  SILSuccessor(SILSuccessor &&) = delete;

  /// This is an iterator for walking the predecessor list of a SILBasicBlock.
  class pred_iterator {
    SILSuccessor *Cur;

    // Cache the basic block to avoid repeated pointer chasing.
    SILBasicBlock *Block;

    void cacheBasicBlock();

  public:
    using difference_type = std::ptrdiff_t;
    using value_type = SILBasicBlock *;
    using pointer = SILBasicBlock **;
    using reference = SILBasicBlock *&;
    using iterator_category = std::forward_iterator_tag;

    pred_iterator(SILSuccessor *Cur = nullptr) : Cur(Cur), Block(nullptr) {
      cacheBasicBlock();
    }

    bool operator==(pred_iterator I2) const { return Cur == I2.Cur; }
    bool operator!=(pred_iterator I2) const { return Cur != I2.Cur; }

    pred_iterator &operator++() {
      assert(Cur != nullptr);
      Cur = Cur->Next;
      cacheBasicBlock();
      return *this;
    }

    pred_iterator operator+(unsigned distance) const {
      auto copy = *this;
      if (distance == 0)
        return copy;
      do {
        copy.Cur = Cur->Next;
      } while (--distance > 0);
      copy.cacheBasicBlock();
      return copy;
    }

    SILSuccessor *getSuccessorRef() const { return Cur; }
    SILBasicBlock *operator*() const { return Block; }
  };
};

} // end swift namespace

#endif
