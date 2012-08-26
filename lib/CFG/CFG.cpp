//===--- CFG.cpp - Defines the CFG data structure --------------------------==//
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

#include "swift/CFG/CFG.h"
#include "swift/CFG/CFGBuilder.h"
#include "swift/AST/AST.h"
#include "swift/AST/ASTVisitor.h"
#include "llvm/ADT/OwningPtr.h"
using namespace swift;

CFG::~CFG() {}

//===----------------------------------------------------------------------===//
// Control Flow Condition Management
//===----------------------------------------------------------------------===//

namespace {
/// A condition is the result of evaluating a boolean expression as
/// control flow.
class Condition {
  // The blocks responsible for executing the true and false conditions.  A
  // block is non-null if that branch is possible, but it's only an independent
  // block if both branches are possible.
  BasicBlock *TrueBB;
  BasicBlock *FalseBB;
  
  /// ContBB - The continuation block if both branches are possible.
  BasicBlock *ContBB;
  
public:
  Condition(BasicBlock *TrueBB, BasicBlock *FalseBB, BasicBlock *ContBB)
    : TrueBB(TrueBB), FalseBB(FalseBB), ContBB(ContBB) {}
  
  bool hasTrue() const { return TrueBB; }
  bool hasFalse() const { return FalseBB; }
  
  /// Begin the emission of the true block.  This should only be
  /// called if hasTrue() returns true.
  void enterTrue();
  
  /// End the emission of the true block.  This must be called after
  /// enterTrue but before anything else on this Condition.
  void exitTrue();
  
  /// Begin the emission of the false block.  This should only be
  /// called if hasFalse() returns true.
  void enterFalse();
  
  /// End the emission of the true block.  This must be called after
  /// exitFalse but before anything else on this Condition.
  void exitFalse();
  
  /// Complete this conditional execution.  This should be called
  /// only after all other calls on this Condition have been made.
  void complete();
};
}

//===----------------------------------------------------------------------===//
// CFG construction
//===----------------------------------------------------------------------===//

// FIXME: Why do we need this?
static Expr *ignoreParens(Expr *Ex) {
  while (ParenExpr *P = dyn_cast<ParenExpr>(Ex))
    Ex = P->getSubExpr();
  return Ex;
}

static bool hasTerminator(BasicBlock *BB) {
  return !BB->empty() && isa<TermInst>(BB->getInsts().back());
}

namespace {
class Builder : public ASTVisitor<Builder, CFGValue> {
  /// The CFG being constructed.
  CFG &C;
  
  /// B - The CFGBuilder used to construct the CFG.  It is what maintains the
  /// notion of the current block being emitted into.
  CFGBuilder B;

public:
  Builder(CFG &C) : C(C), B(new (C) BasicBlock(&C), C) {
  }

  ~Builder() {}

  void finishUp() {
    // If we have an unterminated block, just emit a dummy return for the
    // default return.
    if (B.getInsertionBB() != nullptr) {
      // FIXME: Should use empty tuple for "void" return.
      B.createReturn(0, CFGValue());
      B.clearInsertionPoint();
    }
  }

  //===--------------------------------------------------------------------===//
  // Statements.
  //===--------------------------------------------------------------------===//

  /// Construct the CFG components for the given BraceStmt.
  void visitBraceStmt(BraceStmt *S);

  /// SemiStmts are ignored for CFG construction.
  void visitSemiStmt(SemiStmt *S) {}

  void visitAssignStmt(AssignStmt *S) {
    assert(false && "Not yet implemented");
  }

  void visitReturnStmt(ReturnStmt *S) {
    // FIXME: Should use empty tuple for "void" return.
    CFGValue ArgV = S->hasResult() ? visit(S->getResult()) : (Instruction*) 0;
    B.createReturn(S, ArgV);
    B.clearInsertionPoint();
  }

  void visitIfStmt(IfStmt *S);

  void visitWhileStmt(WhileStmt *S);

  void visitDoWhileStmt(DoWhileStmt *S);

  void visitForStmt(ForStmt *S) {
    assert(false && "Not yet implemented");
  }

  void visitForEachStmt(ForEachStmt *S) {
    assert(false && "Not yet implemented");
  }

  void visitBreakStmt(BreakStmt *S);

  void visitContinueStmt(ContinueStmt *S);

  //===--------------------------------------------------------------------===//
  // Expressions.
  //===--------------------------------------------------------------------===//

  CFGValue visitExpr(Expr *E) {
    E->dump();
    llvm_unreachable("Not yet implemented");
  }

  CFGValue visitCallExpr(CallExpr *E);
  CFGValue visitDeclRefExpr(DeclRefExpr *E);
  CFGValue visitIntegerLiteralExpr(IntegerLiteralExpr *E);
  CFGValue visitLoadExpr(LoadExpr *E);
  CFGValue visitParenExpr(ParenExpr *E);
  CFGValue visitThisApplyExpr(ThisApplyExpr *E);
  CFGValue visitTupleExpr(TupleExpr *E);
  CFGValue visitTypeOfExpr(TypeOfExpr *E);
  
};
} // end anonymous namespace

CFG *CFG::constructCFG(Stmt *S) {
  llvm::OwningPtr<CFG> C(new CFG());
  Builder builder(*C);
  builder.visit(S);
  builder.finishUp();
    
  C->verify();
  return C.take();
}

void Builder::visitBraceStmt(BraceStmt *S) {
  // BraceStmts do not need to be explicitly represented in the CFG.
  // We should consider whether or not the scopes they introduce are
  // represented in the CFG.
  for (const BraceStmt::ExprStmtOrDecl &ESD : S->getElements()) {
    if (Stmt *S = ESD.dyn_cast<Stmt*>())
      visit(S);
    else if (Expr *E = ESD.dyn_cast<Expr*>())
      visit(E);
    else
      assert(0 && "FIXME: Handle Decls");
  }
}

//===--------------------------------------------------------------------===//
// Control-flow.
//===--------------------------------------------------------------------===//

void Builder::visitBreakStmt(BreakStmt *S) {
  
}

void Builder::visitContinueStmt(ContinueStmt *S) {
}

void Builder::visitDoWhileStmt(DoWhileStmt *S) {
}

void Builder::visitIfStmt(IfStmt *S) {
  // ** FIXME ** Handle the condition.  We need to handle more of the
  // statements first.

  // The condition should be the last value evaluated just before the
  // terminator.
  //  CFGValue CondV = visit(S->getCond());
  CFGValue CondV = (Instruction*) 0;

  (void)CondV;
}

void Builder::visitWhileStmt(WhileStmt *S) {

}

//===--------------------------------------------------------------------===//
// Expressions.
//===--------------------------------------------------------------------===//

CFGValue Builder::visitCallExpr(CallExpr *E) {
  Expr *Arg = ignoreParens(E->getArg());
  Expr *Fn = E->getFn();
  CFGValue FnV = visit(Fn);
  llvm::SmallVector<CFGValue, 10> ArgsV;

  // Special case Arg being a TupleExpr, to inline the arguments and
  // not create another instruction.
  if (TupleExpr *TU = dyn_cast<TupleExpr>(Arg)) {
    for (auto arg : TU->getElements())
      ArgsV.push_back(visit(arg));
  } else {
    ArgsV.push_back(visit(Arg));
  }

  return B.createCall(E, FnV, ArgsV);
}

CFGValue Builder::visitDeclRefExpr(DeclRefExpr *E) {
  return B.createDeclRef(E);
}

CFGValue Builder::visitIntegerLiteralExpr(IntegerLiteralExpr *E) {
  return B.createIntegerLiteral(E);
}

CFGValue Builder::visitLoadExpr(LoadExpr *E) {
  CFGValue SubV = visit(E->getSubExpr());
  return B.createLoad(E, SubV);
}

CFGValue Builder::visitThisApplyExpr(ThisApplyExpr *E) {
  CFGValue FnV = visit(E->getFn());
  CFGValue ArgV = visit(E->getArg());
  return B.createThisApply(E, FnV, ArgV);
}

CFGValue Builder::visitParenExpr(ParenExpr *E) {
  return visit(E->getSubExpr());
}

CFGValue Builder::visitTupleExpr(TupleExpr *E) {
  llvm::SmallVector<CFGValue, 10> ArgsV;
  for (auto &I : E->getElements())
    ArgsV.push_back(visit(I));
  return B.createTuple(E, ArgsV);
}

CFGValue Builder::visitTypeOfExpr(TypeOfExpr *E) {
  return B.createTypeOf(E);
}
