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
#include "swift/CFG/CFGSuccessor.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/ilist_node.h"
#include "llvm/ADT/ilist.h"

namespace swift {

class ValueDecl;
class Type;
class CFG;
class BasicBlock;
class ApplyExpr;
class DeclRefExpr;
class IntegerLiteralExpr;
class LoadExpr;
class ReturnStmt;
class Stmt;
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
  void operator delete(void *Ptr, size_t) = delete;

protected:
  Instruction(InstKind Kind) : Kind(Kind), ParentBB(0) {}

public:

  InstKind getKind() const { return Kind; }
  const BasicBlock *getParent() const { return ParentBB; }
  BasicBlock *getParent() { return ParentBB; }

  /// removeFromParent - This method unlinks 'this' from the containing basic
  /// block, but does not delete it.
  ///
  void removeFromParent();
  
  /// eraseFromParent - This method unlinks 'this' from the containing basic
  /// block and deletes it.
  ///
  void eraseFromParent();
  
  /// Pretty-print the Instruction.
  void dump() const;
  void print(raw_ostream &OS) const;

  static bool classof(const Instruction *I) { return true; }
};

/// ApplyInst - Represents application of an argument to a function.
class ApplyInst : public Instruction {
  /// The backing expression for the call.
  ApplyExpr *Expr;
  
  /// The instruction representing the called function.
  CFGValue Callee;

  unsigned NumArgs;
  CFGValue *getArgsStorage() { return reinterpret_cast<CFGValue*>(this + 1); }
  
  /// Construct an ApplyInst from a given call expression and the provided
  /// arguments.
  ApplyInst(ApplyExpr *Expr, CFGValue Callee, ArrayRef<CFGValue> Args);

public:
  static ApplyInst *create(ApplyExpr *Expr, CFGValue Callee,
                          ArrayRef<CFGValue> Args, CFG &C);

  
  CFGValue getCallee() { return Callee; }
  
  /// The arguments passed to this ApplyInst.
  MutableArrayRef<CFGValue> getArguments() {
    return MutableArrayRef<CFGValue>(getArgsStorage(), NumArgs);
  }

  /// The arguments passed to this ApplyInst.
  ArrayRef<CFGValue> getArguments() const {
    return const_cast<ApplyInst*>(this)->getArguments();
  }

  static bool classof(const Instruction *I) {
    return I->getKind() == InstKind::Apply;
  }
};

/// Represents a reference to a declaration, essentially evaluating to
/// its lvalue.
class DeclRefInst : public Instruction {
  /// The backing DeclRefExpr in the AST.
  DeclRefExpr *Expr;
public:

  /// Construct a DeclRefInst.
  ///
  /// \param Expr A backpointer to the original DeclRefExpr.
  ///
  DeclRefInst(DeclRefExpr *Expr) : Instruction(InstKind::DeclRef), Expr(Expr) {}

  /// getDecl - Return the underlying declaration.
  ValueDecl *getDecl() const;

  static bool classof(const Instruction *I) {
    return I->getKind() == InstKind::DeclRef;
  }
};

/// Encapsulates an integer constant, as defined originally by an
/// an IntegerLiteralExpr.
class IntegerLiteralInst : public Instruction {
  // The backing IntegerLiteralExpr in the AST.
  IntegerLiteralExpr *Expr;
public:

  /// Constructs an IntegerLiteralInst.
  ///
  /// \param Expr A backpointer to the original IntegerLiteralExpr.
  ///
  IntegerLiteralInst(IntegerLiteralExpr *Expr)
    : Instruction(InstKind::IntegerLiteral), Expr(Expr) {
  }

  /// getValue - Return the APInt for the underlying integer literal.
  APInt getValue() const;

  static bool classof(const Instruction *I) {
    return I->getKind() == InstKind::IntegerLiteral;
  }
};

/// Represents a load from a memory location.
class LoadInst : public Instruction {
  /// The backing LoadExpr in the AST.
  LoadExpr *Expr;

  /// The LValue (memory address) to use for the load.
  CFGValue LValue;
public:
  /// Constructs a LoadInst.
  ///
  /// \param Expr The backing LoadExpr in the AST.
  ///
  /// \param LValue The CFGValue representing the lvalue (address) to
  ///        use for the load.
  ///
  LoadInst(LoadExpr *Expr, CFGValue LValue) :
    Instruction(InstKind::Load), Expr(Expr), LValue(LValue) {}


  CFGValue getLValue() const { return LValue; }

  static bool classof(const Instruction *I) {
    return I->getKind() == InstKind::Load;
  }
};

/// Represents a constructed tuple.
class TupleInst : public Instruction {
  /// The backing TupleExpr in the AST.
  TupleExpr *Expr;

  CFGValue *getElementsStorage() {
    return reinterpret_cast<CFGValue*>(this + 1);
  }
  unsigned NumArgs;

  /// Private constructor.  Because of the storage requirements of
  /// TupleInst, object creation goes through 'create()'.
  TupleInst(TupleExpr *Expr, ArrayRef<CFGValue> Elements);

public:
  /// The elements referenced by this TupleInst.
  MutableArrayRef<CFGValue> getElements() {
    return MutableArrayRef<CFGValue>(getElementsStorage(), NumArgs);
  }

  /// The elements referenced by this TupleInst.
  ArrayRef<CFGValue> getElements() const {
    return const_cast<TupleInst*>(this)->getElements();
  }

  /// Construct a TupleInst.
  static TupleInst *create(TupleExpr *Expr, ArrayRef<CFGValue> Elements,CFG &C);

  static bool classof(const Instruction *I) {
    return I->getKind() == InstKind::Tuple;
  }
};

/// TypeOfInst - Represents the production of an instance of a given metatype.
class TypeOfInst : public Instruction {
  /// The backing TypeOfExpr in the AST.
  TypeOfExpr *Expr;
public:

  /// Constructs a TypeOfInst.
  ///
  /// \param Expr A backpointer to the original TypeOfExpr.
  ///
  TypeOfInst(TypeOfExpr *Expr) : Instruction(InstKind::TypeOf), Expr(Expr) {}

  /// getMetaType - Return the type of the metatype that this instruction
  /// returns.
  Type getMetaType() const;

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
  TermInst(InstKind K) : Instruction(K) {}

  typedef llvm::ArrayRef<CFGSuccessor> SuccessorListTy;

  /// The successor basic blocks of this terminator.
  SuccessorListTy getSuccessors();

  /// The successor basic blocks of this terminator.
  const SuccessorListTy getSuccessors() const {
    return const_cast<TermInst*>(this)->getSuccessors();
  }

  static bool classof(const Instruction *I) {
    return I->getKind() >= InstKind::First_TermInst &&
           I->getKind() <= InstKind::Last_TermInst;
  }
};

class ReturnInst : public TermInst {
  /// The backing ReturnStmt (if any) in the AST.  If this was an
  /// implicit return, this value will be null.
  ReturnStmt *Stmt;

  /// The value to be returned (if any).  This can be null if it
  /// is an implicit return.
  CFGValue ReturnValue;
  
public:
  /// Constructs a ReturnInst representing an \b explicit return.
  ///
  /// \param returnStmt The backing return statement in the AST.
  ///
  /// \param returnValue The value to be returned.
  ///
  ReturnInst(ReturnStmt *Stmt, CFGValue ReturnValue)
    : TermInst(InstKind::Return), Stmt(Stmt), ReturnValue(ReturnValue) {
  }

  CFGValue getReturnValue() const { return ReturnValue; }

  SuccessorListTy getSuccessors() {
    // No Successors.
    return SuccessorListTy();
  }

  
  static bool classof(const Instruction *I) {
    return I->getKind() == InstKind::Return;
  }
};

/// BranchInst - An unconditional branch.
class BranchInst : public TermInst {
  llvm::ArrayRef<CFGValue> Arguments;
  CFGSuccessor DestBB;
public:
  typedef ArrayRef<CFGValue> ArgsTy;
  
  /// Construct an BranchInst that will branches to the specified block.
  BranchInst(BasicBlock *DestBB)
  : TermInst(InstKind::Branch), DestBB(this, DestBB) {
  }
  
  /// The jump target for the branch.
  BasicBlock *getDestBB() const { return DestBB; }

#if 0
  /// The temporary arguments to the target blocks.
  ArgsTy blockArgs() { return Arguments; }
  const ArgsTy blockArgs() const { return Arguments; }
#endif
  
  SuccessorListTy getSuccessors() {
    return DestBB;
  }
  
  static bool classof(const Instruction *I) {
    return I->getKind() == InstKind::Branch;
  }
};

class CondBranchInst : public TermInst {
  /// The branching statement in the AST.
  Stmt *TheStmt;

  /// The condition value used for the branch.
  CFGValue Condition;

  CFGSuccessor DestBBs[2];
  // FIXME: Use ArrayRef?
  struct BlockArgs { unsigned numArgs; CFGValue *args; } Args[2];
public:

  CondBranchInst(Stmt *TheStmt, CFGValue Condition,
                 BasicBlock *TrueBB, BasicBlock *FalseBB);

  CFGValue getCondition() const { return Condition; }

  SuccessorListTy getSuccessors() {
    return DestBBs;
  }
  
  BasicBlock *getTrueBB() { return DestBBs[0]; }
  const BasicBlock *getTrueBB() const { return DestBBs[0]; }
  BasicBlock *getFalseBB() { return DestBBs[1]; }
  const BasicBlock *getFalseBB() const { return DestBBs[1]; }
  
  void setTrueBB(BasicBlock *BB) { DestBBs[0] = BB; }
  void setFalseBB(BasicBlock *BB) { DestBBs[1] = BB; }
  
  static bool classof(const Instruction *I) {
    return I->getKind() == InstKind::CondBranch;
  }
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
