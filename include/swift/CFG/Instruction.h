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
#include "llvm/ADT/ilist_node.h"
#include "llvm/ADT/ilist.h"

namespace llvm {
  class raw_ostream;
}

namespace swift {

class BasicBlock;

/// This is the root class for all instructions that can be used as the contents
/// of a Swift BasicBlock.  They cannot be used as terminators for BasicBlocks;
/// for those we have TermInst.
class Instruction : public llvm::ilist_node<Instruction> {
public:
  enum Kind {
    Invalid,
    UncondBranch,
    TERM_INST_BEGIN = UncondBranch,
    TERM_INST_END = TERM_INST_BEGIN
  };

  /// The kind of the Instruction.
  const Kind kind;

  /// A backreference to the containing basic block.
  BasicBlock *basicBlock;

private:
  friend struct llvm::ilist_sentinel_traits<Instruction>;
  Instruction() : kind(Invalid) {}
  void operator=(const Instruction &) = delete;

protected:
  Instruction(BasicBlock *B, Kind K) : kind(K), basicBlock(B) {}

public:
  virtual ~Instruction();

  /// Check that Instruction invariants are preserved.
  virtual void validate() const;

  /// Pretty-print the Instruction.
  void dump() const;

  /// Pretty-print the Instruction to the designated stream.
  // FIXME: if no subclasses contain virtual methods, we can devirtualize
  // this class and save a VPTR.
  virtual void print(llvm::raw_ostream &OS) const;

  static bool classof(const Instruction *I) { return true; }
};

/// This class defines a "terminating instruction" for a BasicBlock.
class TermInst : public Instruction {
public:
  TermInst(BasicBlock *B, Kind K) : Instruction(B, K) {}
  virtual ~TermInst();

  // FIXME: Implement.
  virtual llvm::ArrayRef<BasicBlock *> successors() = 0;

  /// Pretty-print the TermInst.
  void dump() const;

  static bool classof(const Instruction *I) {
    return I->kind >= TERM_INST_BEGIN && I->kind <= TERM_INST_END;
  }

};

class UncondBranchInst : public TermInst {
  unsigned *Args;
  unsigned NumArgs;
public:
  /// The jump target for the branch.
  BasicBlock &targetBlock;

  typedef llvm::ArrayRef<unsigned> ArgsTy;

  /// The temporary arguments to the target blocks.
  ArgsTy blockArgs() { return ArgsTy(Args, NumArgs); }
  const ArgsTy blockArgs() const { return ArgsTy(Args, NumArgs); }

  UncondBranchInst(BasicBlock &SrcBlk,
                   BasicBlock &DstBlk,
                   llvm::ArrayRef<unsigned> Args);

  virtual ~UncondBranchInst();

  /// Pretty-print the TermInst to the designated stream.
  void print(llvm::raw_ostream &OS) const;

  /// Check that UncondBranchInst invariants are preserved.
  void validate() const;
};
} // end swift namespace

#endif
