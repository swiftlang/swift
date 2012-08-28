//===--- CFGLowering.cpp - Implements Lowering of ASTs -> CFGs -------------==//
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

#include "CFGLowering.h"
#include "Condition.h"
#include "swift/AST/AST.h"
using namespace swift;
using namespace Lowering;

/// emitBlock - Each basic block is individually new'd, then them emitted with
/// this function.  Since each block is implicitly added to the CFG's list of
/// blocks when created, the construction order is not particularly useful.
///
/// Instead, we want blocks to end up in the order that they are *emitted*.  The
/// cheapest way to ensure this is to just move each block to the end of the
/// block list when emitted: as later blocks are emitted, they'll be moved after
/// this, giving us a block list order that matches emission order when the
/// function is done.
///
/// This function also sets the insertion point of the builder to be the newly
/// emitted block.
static void emitBlock(CFGBuilder &B, BasicBlock *BB) {
  CFG *C = BB->getParent();
  // If this is a fall through into BB, emit the fall through branch.
  if (B.hasValidInsertionPoint())
    B.createBranch(BB);
  
  // Start inserting into that block.
  B.setInsertionPoint(BB);
  
  // Move block to the end of the list.
  if (&C->getBlocks().back() != BB)
    C->getBlocks().splice(C->end(), C->getBlocks(), BB);
}

/// emitOrDeleteBlock - If there are branches to the specified basic block,
/// emit it per emitBlock.  If there aren't, then just delete the block - it
/// turns out to have not been needed.
static void emitOrDeleteBlock(CFGBuilder &B, BasicBlock *BB) {
  if (BB->pred_empty()) {
    // If the block is unused, we don't need it; just delete it.
    BB->eraseFromParent();
  } else {
    // Otherwise, continue emitting code in BB.
    emitBlock(B, BB);
  }
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

/// emitCondition - Emit a boolean expression as a control-flow condition.
///
/// \param TheStmt - The statement being lowered, for source information on the
///        branch.
/// \param E - The expression to be evaluated as a condition.
/// \param hasFalseCode - true if the false branch doesn't just lead
///        to the fallthrough.
/// \param invertValue - true if this routine should invert the value before
///        testing true/false.
Condition CFGLowering::emitCondition(Stmt *TheStmt, Expr *E,
                                     bool hasFalseCode, bool invertValue) {
  assert(B.hasValidInsertionPoint() &&
         "emitting condition at unreachable point");
  
  // Sema forces conditions to have Builtin.i1 type, which guarantees this.
  CFGValue V;
  {
    //    FullExpr Scope(*this);
    //    V = emitAsPrimitiveScalar(E);
    V = (Instruction*)0;
  }
  //  assert(V->getType()->isIntegerTy(1));
  
  // Check for a constant condition.
#if 0 // FIXME: Implement optimization
  if (llvm::ConstantInt *C = dyn_cast<llvm::ConstantInt>(V)) {
    //
  }
#endif
  
  BasicBlock *ContBB = new BasicBlock(&C, "condition.cont");
  BasicBlock *TrueBB = new BasicBlock(&C, "if.true");
  
  BasicBlock *FalseBB, *FalseDestBB;
  if (hasFalseCode) {
    FalseBB = FalseDestBB = new BasicBlock(&C, "if.false");
  } else {
    FalseBB = nullptr;
    FalseDestBB = ContBB;
  }
  
  if (invertValue)
    B.createCondBranch(TheStmt, V, FalseDestBB, TrueBB);
  else
    B.createCondBranch(TheStmt, V, TrueBB, FalseDestBB);
  B.clearInsertionPoint();
  
  return Condition(TrueBB, FalseBB, ContBB);
}

/// emitBranch - Emit a branch to the given jump destination, threading out
/// through any cleanups we might need to run.  Leaves the insertion point in
/// the current block.
void CFGLowering::emitBranch(JumpDest Dest) {
  assert(B.hasValidInsertionPoint() && "Inserting branch in invalid spot");
  assert(Cleanups.empty() && "FIXME: Implement cleanup support");
  B.createBranch(Dest.getBlock());
  B.clearInsertionPoint();
}


//===--------------------------------------------------------------------===//
// Statements
//===--------------------------------------------------------------------===//

void CFGLowering::visitBraceStmt(BraceStmt *S) {
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

void CFGLowering::visitBreakStmt(BreakStmt *S) {
  emitBranch(BreakDestStack.back());
  B.clearInsertionPoint();
}

void CFGLowering::visitContinueStmt(ContinueStmt *S) {
  emitBranch(ContinueDestStack.back());
  B.clearInsertionPoint();
}

void CFGLowering::visitIfStmt(IfStmt *S) {
  Condition Cond = emitCondition(S, S->getCond(), S->getElseStmt() != nullptr);
  
  if (Cond.hasTrue()) {
    Cond.enterTrue(B);
    visit(S->getThenStmt());
    Cond.exitTrue(B);
  }
  
  if (Cond.hasFalse()) {
    assert(S->getElseStmt());
    Cond.enterFalse(B);
    visit(S->getElseStmt());
    Cond.exitFalse(B);
  }
  
  Cond.complete(B);
}

void CFGLowering::visitWhileStmt(WhileStmt *S) {
  // Create a new basic block and jump into it.
  BasicBlock *LoopBB = new (C) BasicBlock(&C, "while");
  B.createBranch(LoopBB);
  B.clearInsertionPoint();
  
  emitBlock(B, LoopBB);
  
  // Set the destinations for 'break' and 'continue'
  BasicBlock *EndBB = new (C) BasicBlock(&C, "while.end");
  BreakDestStack.emplace_back(EndBB, getCleanupsDepth());
  ContinueDestStack.emplace_back(LoopBB, getCleanupsDepth());
  
  // Evaluate the condition with the false edge leading directly
  // to the continuation block.
  Condition Cond = emitCondition(S, S->getCond(), /*hasFalseCode*/ false);
  
  // If there's a true edge, emit the body in it.
  if (Cond.hasTrue()) {
    Cond.enterTrue(B);
    visit(S->getBody());
    if (B.hasValidInsertionPoint()) {
      B.createBranch(LoopBB);
      B.clearInsertionPoint();
    }
    Cond.exitTrue(B);
  }
  
  // Complete the conditional execution.
  Cond.complete(B);
  
  emitOrDeleteBlock(B, EndBB);
  BreakDestStack.pop_back();
  ContinueDestStack.pop_back();
}

void CFGLowering::visitDoWhileStmt(DoWhileStmt *S) {
  // Create a new basic block and jump into it.
  BasicBlock *LoopBB = new (C) BasicBlock(&C, "dowhile");
  B.createBranch(LoopBB);
  B.clearInsertionPoint();
  
  emitBlock(B, LoopBB);
  
  // Set the destinations for 'break' and 'continue'
  BasicBlock *EndBB = new (C) BasicBlock(&C, "dowhile.end");
  BreakDestStack.emplace_back(EndBB, getCleanupsDepth());
  ContinueDestStack.emplace_back(LoopBB, getCleanupsDepth());
  
  // Emit the body, which is always evaluated the first time around.
  visit(S->getBody());
  
  if (B.hasValidInsertionPoint()) {
    // Evaluate the condition with the false edge leading directly
    // to the continuation block.
    Condition Cond = emitCondition(S, S->getCond(), /*hasFalseCode*/ false);
    
    Cond.enterTrue(B);
    if (B.hasValidInsertionPoint()) {
      B.createBranch(LoopBB);
      B.clearInsertionPoint();
    }
    Cond.exitTrue(B);
    // Complete the conditional execution.
    Cond.complete(B);
  }
  
  emitOrDeleteBlock(B, EndBB);
  BreakDestStack.pop_back();
  ContinueDestStack.pop_back();
}



//===--------------------------------------------------------------------===//
// Expressions
//===--------------------------------------------------------------------===//

CFGValue CFGLowering::visitCallExpr(CallExpr *E) {
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

CFGValue CFGLowering::visitDeclRefExpr(DeclRefExpr *E) {
  return B.createDeclRef(E);
}

CFGValue CFGLowering::visitIntegerLiteralExpr(IntegerLiteralExpr *E) {
  return B.createIntegerLiteral(E);
}

CFGValue CFGLowering::visitLoadExpr(LoadExpr *E) {
  CFGValue SubV = visit(E->getSubExpr());
  return B.createLoad(E, SubV);
}

CFGValue CFGLowering::visitThisApplyExpr(ThisApplyExpr *E) {
  CFGValue FnV = visit(E->getFn());
  CFGValue ArgV = visit(E->getArg());
  return B.createThisApply(E, FnV, ArgV);
}

CFGValue CFGLowering::visitParenExpr(ParenExpr *E) {
  return visit(E->getSubExpr());
}

CFGValue CFGLowering::visitTupleExpr(TupleExpr *E) {
  llvm::SmallVector<CFGValue, 10> ArgsV;
  for (auto &I : E->getElements())
    ArgsV.push_back(visit(I));
  return B.createTuple(E, ArgsV);
}

CFGValue CFGLowering::visitTypeOfExpr(TypeOfExpr *E) {
  return B.createTypeOf(E);
}

//===--------------------------------------------------------------------===//
// CFG Class implementation
//===--------------------------------------------------------------------===//

CFG *CFG::constructCFG(Stmt *S) {
  CFG *C = new CFG();
  CFGLowering(*C).visit(S);
  
  C->verify();
  return C;
}
