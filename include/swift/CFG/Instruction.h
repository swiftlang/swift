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

#ifndef SWIFT_CFG_INSTRUCTION_H
#define SWIFT_CFG_INSTRUCTION_H

#include "swift/Basic/LLVM.h"
#include "swift/CFG/CFGBase.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/ilist_node.h"
#include "llvm/ADT/ilist.h"

namespace swift {

class CFG;
class BasicBlock;
class CallExpr;
class DeclRefExpr;
class IntegerLiteralExpr;
class LoadExpr;
class ReturnStmt;
class Stmt;
class ThisApplyExpr;
class TupleExpr;
class TypeOfExpr;

enum class InstKind {
#define INST(Id, Parent) Id,
#define INST_RANGE(Id, FirstId, LastId) \
  First_##Id##Inst = FirstId, Last_##Id##Inst = LastId,
#include "swift/CFG/CFGNodes.def"
};

/// This is the root class for all instructions that can be used as the contents
/// of a Swift BasicBlock.
class Instruction :
public llvm::ilist_node<Instruction>, public CFGAllocated<Instruction> {
  friend struct llvm::ilist_traits<Instruction>;
  /// The kind of the Instruction.
  const InstKind Kind;

  /// A backreference to the containing basic block.  This is maintained by
  // ilist_traits<Instruction>.
  BasicBlock *ParentBB;

  friend struct llvm::ilist_sentinel_traits<Instruction>;
  Instruction() = delete;
  void operator=(const Instruction &) = delete;
  void operator delete(void *Ptr, size_t)  = delete;

protected:
  // FIXME: Remove ctor.
  Instruction(BasicBlock *B, InstKind Kind);

  Instruction(InstKind Kind) : Kind(Kind), ParentBB(0) {}

public:

  InstKind getKind() const { return Kind; }
  const BasicBlock *getParent() const { return ParentBB; }
  BasicBlock *getParent() { return ParentBB; }

  /// Pretty-print the Instruction.
  void dump() const;
  void print(raw_ostream &OS) const;

  static bool classof(const Instruction *I) { return true; }
};

/// Represents a call to a function.
class CallInst : public Instruction {
private:
  /// Construct a CallInst from a given call expression and the provided
  /// arguments.
  CallInst(CallExpr *expr, CFGValue function, ArrayRef<CFGValue> args);

  CFGValue *getArgsStorage() { return reinterpret_cast<CFGValue*>(this + 1); }
  unsigned NumArgs;

public:
  static CallInst *create(CallExpr *expr, CFGValue function,
                          ArrayRef<CFGValue> args, CFG &C);

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

  static bool classof(const Instruction *I) {
    return I->getKind() == InstKind::Call;
  }
};

/// Represents a reference to a declaration, essentially evaluating to
/// its lvalue.
class DeclRefInst : public Instruction {
public:
  /// The backing DeclRefExpr in the AST.
  DeclRefExpr *expr;

  /// Construct a DeclRefInst.
  ///
  /// \param DR A backpointer to the original DeclRefExpr.
  ///
  /// \param B The basic block that will contain the instruction.
  ///
  DeclRefInst(DeclRefExpr *DR) : Instruction(InstKind::DeclRef), expr(DR) {}

  static bool classof(const Instruction *I) {
    return I->getKind() == InstKind::DeclRef;
  }
};

/// Encapsulates an integer constant, as defined originally by an
/// an IntegerLiteralExpr.
class IntegerLiteralInst : public Instruction {
public:
  // The backing IntegerLiteralExpr in the AST.
  IntegerLiteralExpr *literal;

  /// Constructs an IntegerLiteralInst.
  ///
  /// \param IE A backpointer to the original IntegerLiteralExpr.
  ///
  /// \param B The basic block that will contain the instruction.
  ///
  IntegerLiteralInst(IntegerLiteralExpr *IE)
    : Instruction(InstKind::IntegerLiteral), literal(IE) {
  }

  static bool classof(const Instruction *I) {
    return I->getKind() == InstKind::IntegerLiteral;
  }
};

/// Represents a load from a memory location.
class LoadInst : public Instruction {
public:
  /// The backing LoadExpr in the AST.
  LoadExpr *expr;

  /// The lvalue (memory address) to use for the load.
  CFGValue lvalue;

  /// Constructs a LoadInst.
  ///
  /// \param expr The backing LoadExpr in the AST.
  ///
  /// \param lvalue The CFGValue representing the lvalue (address) to
  ///        use for the load.
  ///
  /// \param The basic block that will contain the instruction.
  ///
  LoadInst(LoadExpr *expr, CFGValue lvalue) :
    Instruction(InstKind::Load), expr(expr), lvalue(lvalue) {}

  static bool classof(const Instruction *I) {
    return I->getKind() == InstKind::Load;
  }
};

/// Represents an abstract application that provides the 'this' pointer for
/// a curried method.
class ThisApplyInst : public Instruction {
public:
  /// The backing ThisApplyExpr in the AST.
  ThisApplyExpr *expr;

  /// The instruction representing the called function.
  CFGValue function;

  /// The instruction representing the argument expression.
  CFGValue argument;

  /// Construct a ThisApplyInst.
  ///
  /// \param expr A backpointer to the original ThisApplyExpr.
  ///
  /// \param B The basic block that will contain the instruction.
  ///
  ThisApplyInst(ThisApplyExpr *expr, CFGValue function, CFGValue argument)
    : Instruction(InstKind::ThisApply), expr(expr), function(function),
      argument(argument) {
  }

  static bool classof(const Instruction *I) {
    return I->getKind() == InstKind::ThisApply;
  }
};

/// Represents a constructed tuple.
class TupleInst : public Instruction {

  CFGValue *getElementsStorage() {
    return reinterpret_cast<CFGValue*>(this + 1);
  }
  unsigned NumArgs;

  /// Private constructor.  Because of the storage requirements of
  /// TupleInst, object creation goes through 'create()'.
  TupleInst(TupleExpr *Expr, ArrayRef<CFGValue> Elements);

public:
  /// The backing TupleExpr in the AST.
  TupleExpr *expr;

  /// The elements referenced by this TupleInst.
  MutableArrayRef<CFGValue> elements() {
    return MutableArrayRef<CFGValue>(getElementsStorage(), NumArgs);
  }

  /// The elements referenced by this TupleInst.
  ArrayRef<CFGValue> elements() const {
    return const_cast<TupleInst*>(this)->elements();
  }

  /// Construct a TupleInst.
  static TupleInst *create(TupleExpr *Expr, ArrayRef<CFGValue> Elements,CFG &C);

  static bool classof(const Instruction *I) {
    return I->getKind() == InstKind::Tuple;
  }
};

/// Represents the production of an instance of a given metatype.
class TypeOfInst : public Instruction {
public:
  /// The backing TypeOfExpr in the AST.
  TypeOfExpr *Expr;

  /// Constructs a TypeOfInst.
  ///
  /// \param expr A backpointer to the original TypeOfExpr.
  ///
  /// \param B The basic block that will contain the instruction.
  ///
  TypeOfInst(TypeOfExpr *Expr) : Instruction(InstKind::TypeOf), Expr(Expr) {}

  static bool classof(const Instruction *I) {
    return I->getKind() == InstKind::TypeOf;
  }
};

//===----------------------------------------------------------------------===//
// Instructions representing terminators.
//===----------------------------------------------------------------------===//

/// This class defines a "terminating instruction" for a BasicBlock.
class TermInst : public Instruction {
public:
  // FIXME: Remove ctor.
  TermInst(BasicBlock *B, InstKind K) : Instruction(B, K) {}
  
  TermInst(InstKind K) : Instruction(K) {}

  typedef llvm::ArrayRef<BasicBlock *> Successors;

  /// The successor basic blocks of this terminator.
  Successors successors();

  /// The successor basic blocks of this terminator.
  const Successors successors() const {
    return const_cast<TermInst*>(this)->successors();
  }

  static bool classof(const Instruction *I) {
    return I->getKind() >= InstKind::First_TermInst &&
           I->getKind() <= InstKind::Last_TermInst;
  }
};

class ReturnInst : public TermInst {
public:
  /// The backing ReturnStmt (if any) in the AST.  If this was an
  /// implicit return, this value will be null.
  ReturnStmt *returnStmt;

  /// The value to be returned (if any).  This can be null if it
  /// is an implicit return.
  CFGValue returnValue;

  // FIXME: Remove Ctor.
  ReturnInst(ReturnStmt *returnStmt, CFGValue returnValue, BasicBlock *B)
  : TermInst(B, InstKind::Return), returnStmt(returnStmt),
  returnValue(returnValue) {}

  
  /// Constructs a ReturnInst representing an \b explicit return.
  ///
  /// \param returnStmt The backing return statement in the AST.
  ///
  /// \param returnValue The value to be returned.
  ///
  ReturnInst(ReturnStmt *returnStmt, CFGValue returnValue)
    : TermInst(InstKind::Return), returnStmt(returnStmt),
      returnValue(returnValue) {}

  /// Constructs a ReturnInst representing an \b implicit return.
  ///
  /// \param The basic block that will contain the instruction.
  ///
  ReturnInst(BasicBlock *B) : ReturnInst(0, (Instruction*)0, B) {}

  static bool classof(const Instruction *I) {
    return I->getKind() == InstKind::Return;
  }
};

class CondBranchInst : public TermInst {
  BasicBlock *Branches[2];
  // FIXME: Use ArrayRef?
  struct BlockArgs { unsigned numArgs; CFGValue *args; } Args[2];
public:
  /// The branching statement in the AST.
  Stmt *branchStmt;

  /// The condition value used for the branch.
  CFGValue condition;

  struct BranchInfo {
    /// The destination basic block for a branch.
    BasicBlock *block;

    /// The argument values for the target basic block.
    MutableArrayRef<CFGValue> arguments;

    BranchInfo(BasicBlock *B, MutableArrayRef<CFGValue> A)
      : block(B), arguments(A) {}
  };

  /// Return the branch target and arguments for a given branch.
  BranchInfo getBranchInfo(unsigned branch) {
    assert(branch < 2);
    return BranchInfo(Branches[branch], getArguments(branch));
  }

  /// Return the block arguments for the specified branch.
  MutableArrayRef<CFGValue> getArguments(unsigned branch) {
    assert(branch < 2);
    return MutableArrayRef<CFGValue>(Args[branch].args, Args[branch].numArgs);
  }

  /// Return the block arguments for the specified branch.
  ArrayRef<CFGValue> getArguments(unsigned branch) const {
    assert(branch < 2);
    return ArrayRef<CFGValue>(Args[branch].args, Args[branch].numArgs);
  }

  /// The branch targets.
  MutableArrayRef<BasicBlock*> branches() {
    return MutableArrayRef<BasicBlock*>(Branches, 2);
  }

  /// The branch targets.
  ArrayRef<BasicBlock*> branches() const {
    return ArrayRef<BasicBlock*>(Branches, 2);
  }

  // FIXME: Remove ctor.
  CondBranchInst(Stmt *BranchStmt,
                 CFGValue condition,
                 BasicBlock *Target1,
                 BasicBlock *Target2,
                 BasicBlock *B);

  CondBranchInst(Stmt *BranchStmt, CFGValue Cond,
                 BasicBlock *Target1, BasicBlock *Target2);

  static bool classof(const Instruction *I) {
    return I->getKind() == InstKind::CondBranch;
  }
};

class UncondBranchInst : public TermInst {
  llvm::ArrayRef<CFGValue> Arguments;
  BasicBlock *TargetBlock;
public:
  typedef ArrayRef<CFGValue> ArgsTy;

  /// Construct an UncondBranchInst that will become the terminator
  /// for the specified BasicBlock.
  
  // FIXME: Remove ctor.
  UncondBranchInst(int X, BasicBlock *BB) :
    TermInst(BB, InstKind::UncondBranch), TargetBlock(nullptr) {}

  UncondBranchInst(BasicBlock *TargetBlock, ArgsTy BlockArgs, CFG &C)
    : TermInst(InstKind::UncondBranch), TargetBlock(nullptr) {
    setTarget(TargetBlock, BlockArgs, C);
  }
  
  /// The jump target for the branch.
  BasicBlock *targetBlock() const { return TargetBlock; }

  /// The temporary arguments to the target blocks.
  ArgsTy blockArgs() { return Arguments; }
  const ArgsTy blockArgs() const { return Arguments; }

  /// Set the target block (with the matching arguments) for this branch.
  void setTarget(BasicBlock *Target, ArgsTy BlockArgs, CFG &C);

  static bool classof(const Instruction *I) {
    return I->getKind() == InstKind::UncondBranch;
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
  public ilist_default_traits<::swift::Instruction> {
  typedef ::swift::Instruction Instruction;

private:
  mutable ilist_half_node<Instruction> Sentinel;

  swift::BasicBlock *getContainingBlock();

public:
  Instruction *createSentinel() const {
    return static_cast<Instruction*>(&Sentinel);
  }
  void destroySentinel(Instruction *) const {}

  Instruction *provideInitialHead() const { return createSentinel(); }
  Instruction *ensureHead(Instruction*) const { return createSentinel(); }
  static void noteHead(Instruction*, Instruction*) {}
  static void deleteNode(Instruction *V) {}

  void addNodeToList(Instruction *I);
  void removeNodeFromList(Instruction *I);
  void transferNodesFromList(ilist_traits<Instruction> &L2,
                             ilist_iterator<Instruction> first,
                             ilist_iterator<Instruction> last);

private:
  void createNode(const Instruction &);
};

} // end llvm namespace

#endif
