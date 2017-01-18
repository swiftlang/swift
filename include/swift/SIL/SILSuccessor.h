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

#include <cassert>
#include <cstddef>
#include <iterator>

namespace swift {
class SILBasicBlock;
class TermInst;

/// SILSuccessor - This represents a reference to a SILBasicBlock in a
/// terminator instruction, forming a list of TermInst references to
/// BasicBlocks.  This forms the predecessor list, ensuring that it is always
/// kept up to date.
class SILSuccessor {
  friend class SILSuccessorIterator;
  /// ContainingInst - This is the Terminator instruction that contains this
  /// successor.
  TermInst *ContainingInst = nullptr;
  
  /// SuccessorBlock - If non-null, this is the BasicBlock that the terminator
  /// branches to.
  SILBasicBlock *SuccessorBlock = nullptr;
  
  /// This is the prev and next terminator reference to SuccessorBlock, or
  /// null if SuccessorBlock is null.
  SILSuccessor **Prev = nullptr, *Next = nullptr;
public:
  SILSuccessor() {}

  SILSuccessor(TermInst *CI)
    : ContainingInst(CI) {
  }

  SILSuccessor(TermInst *CI, SILBasicBlock *Succ)
    : ContainingInst(CI) {
    *this = Succ;
  }
  
  ~SILSuccessor() {
    *this = nullptr;
  }

  void operator=(SILBasicBlock *BB);
  
  operator SILBasicBlock*() const { return SuccessorBlock; }
  SILBasicBlock *getBB() const { return SuccessorBlock; }
  
  // Do not copy or move these.
  SILSuccessor(const SILSuccessor &) = delete;
  SILSuccessor(SILSuccessor &&) = delete;
};
  
/// SILSuccessorIterator - This is an iterator for walking the successor list of
/// a SILBasicBlock.
class SILSuccessorIterator {
  SILSuccessor *Cur;
public:
  using difference_type = std::ptrdiff_t;
  using value_type = SILBasicBlock *;
  using pointer = SILBasicBlock **;
  using reference = SILBasicBlock *&;
  using iterator_category = std::forward_iterator_tag;
  
  SILSuccessorIterator(SILSuccessor *Cur = 0) : Cur(Cur) {}
  
  bool operator==(SILSuccessorIterator I2) const { return Cur == I2.Cur; }
  bool operator!=(SILSuccessorIterator I2) const { return Cur != I2.Cur; }

  SILSuccessorIterator &operator++() {
    assert(Cur && "Trying to advance past end");
    Cur = Cur->Next;
    return *this;
  }

  SILSuccessorIterator operator++(int) {
    SILSuccessorIterator copy = *this;
    ++copy;
    return copy;
  }

  SILSuccessor *getSuccessorRef() const { return Cur; }
  SILBasicBlock *operator*();
};
  

} // end swift namespace

#endif
