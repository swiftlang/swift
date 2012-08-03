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
  /// Check that Instruction invariants are preserved.
  void validate() const;

  /// Pretty-print the Instruction.
  void dump() const;

  /// Pretty-print the Instruction to the designated stream.
  void print(llvm::raw_ostream &OS) const;

  static bool classof(const Instruction *I) { return true; }
};

/// This class defines a "terminating instruction" for a BasicBlock.
class TermInst : public Instruction {
public:
  TermInst(BasicBlock *B, Kind K) : Instruction(B, K) {}

  typedef llvm::ArrayRef<BasicBlock *> Successors;

  /// The successor basic blocks of this terminator.
  Successors successors();

  /// The successor basic blocks of this terminator.
  const Successors successors() const {
    return const_cast<TermInst*>(this)->successors();
  }

  static bool classof(const Instruction *I) {
    return I->kind >= TERM_INST_BEGIN && I->kind <= TERM_INST_END;
  }
};

class UncondBranchInst : public TermInst {
public:
  typedef llvm::ArrayRef<unsigned> ArgsTy;

protected:
  unsigned *Args;
  unsigned NumArgs;
  BasicBlock *TargetBlock;

public:
  /// Construct an UncondBranchInst that will become the terminator
  /// for the specified BasicBlock.
  UncondBranchInst(BasicBlock *BB) :
    TermInst(BB, UncondBranch), Args(nullptr), NumArgs(0),
    TargetBlock(nullptr) {}

  /// The jump target for the branch.
  BasicBlock &targetBlock() { return *TargetBlock; }

  /// The jump target for the branch.
  const BasicBlock &targetBlock() const { return *TargetBlock; }

  /// The temporary arguments to the target blocks.
  ArgsTy blockArgs() { return ArgsTy(Args, NumArgs); }
  const ArgsTy blockArgs() const { return ArgsTy(Args, NumArgs); }

  /// Set the target block (with the matching arguments) for this branch.
  void setTarget(BasicBlock *Target, const ArgsTy BlockArgs);

  static bool classof(const Instruction *I) {
    return I->kind == UncondBranch;
  }

private:
  void unregisterTarget();
};
} // end swift namespace

#endif
