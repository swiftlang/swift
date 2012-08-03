//===--- Instruction.h - Instructions for high-level CFGs --------*- C++ -*-==//
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
//
// This file defines the high-level Instruction class used for Swift CFGs.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_INSTRUCTION_H
#define SWIFT_INSTRUCTION_H

#include "llvm/ADT/ArrayRef.h"

namespace llvm {
  class raw_ostream;
}

namespace swift {

class BasicBlock;

/// This is the root class for all instructions that can be used as the contents
/// of a Swift BasicBlock.  They cannot be used as terminators for BasicBlocks;
/// for those we have TermInst.
class Instruction {
  Instruction() : prevInst(0), nextInst(0), basicBlock(0) {}
public:
  virtual ~Instruction();

  /// The previous instruction in the containing basic block.
  Instruction *prevInst;

  /// The next instruction in the containing basic block.
  Instruction *nextInst;

  /// A backreference to the containing basic block.
  BasicBlock *basicBlock;

  /// Check that Instruction invariants are preserved.
  virtual bool validate() const = 0;

  /// Pretty-print the Instruction.
  void dump() const;

  /// Pretty-print the Instruction to the designated stream.
  // FIXME: if no subclasses contain virtual methods, we can devirtualize
  // this class and save a VPTR.
  virtual void print(llvm::raw_ostream &OS) const = 0;
};

/// This class defines a "terminating instruction" for a BasicBlock.
/// These instructions are different from regular Instruction objects, as
/// they are exclusively used for representing terminators in BasicBlock,
/// while Instruction objects must appear within the BasicBlock body.
class TermInst {
  TermInst() {}
public:
  virtual ~TermInst();

  // FIXME: Implement.
  llvm::ArrayRef<BasicBlock *> branchTargets() { return nullptr; }

  /// Pretty-print the TermInst.
  void dump() const;

  /// Pretty-print the TermInst to the designated stream.
  // FIXME: if no subclasses contain virtual methods, we can devirtualize
  // this class and save a VPTR.
  virtual void print(llvm::raw_ostream &OS) const = 0;
};

} // end swift namespace

#endif
