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

#include "swift/Basic/LLVM.h"
#include "swift/CFG/CFGBase.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/ilist_node.h"
#include "llvm/ADT/ilist.h"

namespace llvm {
  class raw_ostream;
}

namespace swift {

class BasicBlock;
class CallExpr;
class DeclRefExpr;
class IntegerLiteralExpr;
class ThisApplyExpr;
class TypeOfExpr;

/// This is the root class for all instructions that can be used as the contents
/// of a Swift BasicBlock.  They cannot be used as terminators for BasicBlocks;
/// for those we have TermInst.
class Instruction : public llvm::ilist_node<Instruction>,
                    public CFGAllocated<Instruction> {
public:
  enum Kind {
    Invalid,
    Call,
    DeclRef,
    ThisApply,
    TypeOf,
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
  Instruction() : kind(Invalid), basicBlock(0) {}
  void operator=(const Instruction &) = delete;
  void operator delete(void *Ptr, size_t)  = delete;

  /// Helper method for validating non-terminator Instructions.
  void validateNonTerm() const;

protected:
  Instruction(BasicBlock *B, Kind K);

public:
  /// Check that Instruction invariants are preserved.
  void validate() const;

  /// Pretty-print the Instruction.
  void dump() const;

  /// Pretty-print the Instruction to the designated stream.
  void print(llvm::raw_ostream &OS, CFGPrintContext &PC) const;

  static bool classof(const Instruction *I) { return true; }
};

class CallInst : public Instruction {
private:
  /// Construct a CallInst from a given call expression and the provided
  /// arguments.
  CallInst(CallExpr *expr,
           BasicBlock *B,
           CFGValue function,
           ArrayRef<CFGValue> args);

  CallInst() = delete;
  CFGValue *getArgsStorage() { return reinterpret_cast<CFGValue*>(this + 1); }
  unsigned NumArgs;

public:
  static CallInst *create(CallExpr *expr,
                          BasicBlock *B,
                          CFGValue function,
                          ArrayRef<CFGValue> args);

  /// The backing expression for the call.
  CallExpr *expr;

  /// The instruction representing the called function.
  CFGValue function;

  /// The arguments referenced by this CallInst.
  MutableArrayRef<CFGValue> arguments() {
    return MutableArrayRef<CFGValue>(getArgsStorage(), NumArgs);
  }

  /// The arguments referenced by this CallInst.
  ArrayRef<CFGValue> arguments() const {
    return const_cast<CallInst*>(this)->arguments();
  }

  static bool classof(const Instruction *I) { return I->kind == Call; }
};

class DeclRefInst : public Instruction {
  DeclRefInst() = delete;
public:
  /// The backing DeclRefExpr in the AST.
  DeclRefExpr *expr;

  DeclRefInst(DeclRefExpr *expr, BasicBlock *B)
    : Instruction(B, DeclRef),
      expr(expr) {}

  static bool classof(const Instruction *I) { return I->kind == DeclRef; }
};

class ThisApplyInst : public Instruction {
  ThisApplyInst() = delete;
public:
  /// The backing ThisApplyExpr in the AST.
  ThisApplyExpr *expr;

  /// The instruction representing the called function.
  CFGValue function;

  /// The instruction representing the argument expression.
  CFGValue argument;

  ThisApplyInst(ThisApplyExpr *expr,
                CFGValue function,
                CFGValue argument,
                BasicBlock *B)
    : Instruction(B, ThisApply),
      expr(expr),
      function(function),
      argument(argument) {}

  static bool classof(const Instruction *I) { return I->kind == ThisApply; }
};

class TypeOfInst : public Instruction {
  TypeOfInst() = delete;
public:
  /// The backing TypeOfExpr in the AST.
  TypeOfExpr *expr;

  TypeOfInst(TypeOfExpr *expr, BasicBlock *B)
    : Instruction(B, TypeOf), expr(expr) {}

  static bool classof(const Instruction *I) { return I->kind == TypeOf; }
};

//===----------------------------------------------------------------------===//
// Instructions representing terminators.
//===----------------------------------------------------------------------===//

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

//===----------------------------------------------------------------------===//
// ilist_traits for Instruction
//===----------------------------------------------------------------------===//

namespace llvm {

template <>
struct ilist_traits<::swift::Instruction> :
  public ilist_default_traits<::swift::Instruction>
{
  typedef ::swift::Instruction Instruction;

private:
  mutable ilist_half_node<Instruction> Sentinel;

public:
  Instruction *createSentinel() const {
    return static_cast<Instruction*>(&Sentinel);
  }
  void destroySentinel(Instruction *) const {}

  Instruction *provideInitialHead() const { return createSentinel(); }
  Instruction *ensureHead(Instruction*) const { return createSentinel(); }
  static void noteHead(Instruction*, Instruction*) {}
  static void deleteNode(Instruction *V) {}

private:
  void createNode(const Instruction &);
};

} // end llvm namespace

#endif
