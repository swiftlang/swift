//===--- SILSuccessor.h - Terminator Instruction Successor -------*- C++ -*-==//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_SILSuccessor_H
#define SWIFT_SIL_SILSuccessor_H

#include <cassert>

namespace swift {
class BasicBlock;
class TermInst;

/// SILSuccessor - This represents a reference to a BasicBlock in a terminator
/// instruction, forming a list of TermInst references to BasicBlocks.  This
/// forms the predecessor list, ensuring that it is always kept up to date.
class SILSuccessor {
  friend class SILSuccessorIterator;
  /// ContainingInst - This is the Terminator instruction that contains this
  /// successor.
  TermInst *ContainingInst;
  
  /// SuccessorBlock - If non-null, this is the BasicBlock that the terminator
  /// branches to.
  BasicBlock *SuccessorBlock;
  
  /// This is the prev and next terminator reference to SuccessorBlock, or
  /// null if SuccessorBlock is null.
  SILSuccessor **Prev, *Next;
public:
  SILSuccessor() {}

  SILSuccessor(TermInst *CI)
    : ContainingInst(CI), SuccessorBlock(nullptr) {
  }

  SILSuccessor(TermInst *CI, BasicBlock *Succ)
    : ContainingInst(CI), SuccessorBlock(nullptr) {
    *this = Succ;
  }
  
  ~SILSuccessor() {
    *this = nullptr;
  }

  /// init - This should only be used when the SILSuccessor is default ctor'd.
  void init(TermInst *TI) { ContainingInst = TI; SuccessorBlock = nullptr; }

  void operator=(BasicBlock *BB);
  
  operator BasicBlock*() const { return SuccessorBlock; }
};
  
/// SILSuccessorIterator - This is an iterator for walking the successor list of
/// a BasicBlock.
class SILSuccessorIterator {
  SILSuccessor *Cur;
public:
  SILSuccessorIterator(SILSuccessor *Cur = 0) : Cur(Cur) {}
  
  bool operator==(SILSuccessorIterator I2) const { return Cur == I2.Cur; }
  bool operator!=(SILSuccessorIterator I2) const { return Cur != I2.Cur; }

  void operator++() {
    assert(Cur && "Trying to advance past end");
    Cur = Cur->Next;
  }
  
  BasicBlock *operator*();
};
  

} // end swift namespace

#endif
