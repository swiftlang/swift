//===--- CFGGen.cpp - Implements Lowering of ASTs -> CFGs -----------------===//
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

#include "CFGGen.h"
#include "Condition.h"
#include "swift/AST/AST.h"
using namespace swift;
using namespace Lowering;

/// emitOrDeleteBlock - If there are branches to the specified basic block,
/// emit it per emitBlock.  If there aren't, then just delete the block - it
/// turns out to have not been needed.
static void emitOrDeleteBlock(CFGBuilder &B, BasicBlock *BB) {
  if (BB->pred_empty()) {
    // If the block is unused, we don't need it; just delete it.
    BB->eraseFromParent();
  } else {
    // Otherwise, continue emitting code in BB.
    B.emitBlock(BB);
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
Condition CFGGen::emitCondition(Stmt *TheStmt, Expr *E,
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
  
  return Condition(TrueBB, FalseBB, ContBB);
}

/// emitBranch - Emit a branch to the given jump destination, threading out
/// through any cleanups we might need to run.  Leaves the insertion point in
/// the current block.
void CFGGen::emitBranch(JumpDest Dest) {
  assert(B.hasValidInsertionPoint() && "Inserting branch in invalid spot");
  assert(Cleanups.empty() && "FIXME: Implement cleanup support");
  B.createBranch(Dest.getBlock());
}


//===--------------------------------------------------------------------===//
// Statements
//===--------------------------------------------------------------------===//

void CFGGen::visitBraceStmt(BraceStmt *S) {
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

void CFGGen::visitBreakStmt(BreakStmt *S) {
  emitBranch(BreakDestStack.back());
}

void CFGGen::visitContinueStmt(ContinueStmt *S) {
  emitBranch(ContinueDestStack.back());
}

void CFGGen::visitIfStmt(IfStmt *S) {
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

void CFGGen::visitWhileStmt(WhileStmt *S) {
  // Create a new basic block and jump into it.
  BasicBlock *LoopBB = new (C) BasicBlock(&C, "while");
  B.emitBlock(LoopBB);
  
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
    if (B.hasValidInsertionPoint())
      B.createBranch(LoopBB);
    Cond.exitTrue(B);
  }
  
  // Complete the conditional execution.
  Cond.complete(B);
  
  emitOrDeleteBlock(B, EndBB);
  BreakDestStack.pop_back();
  ContinueDestStack.pop_back();
}

void CFGGen::visitDoWhileStmt(DoWhileStmt *S) {
  // Create a new basic block and jump into it.
  BasicBlock *LoopBB = new (C) BasicBlock(&C, "dowhile");
  B.emitBlock(LoopBB);
  
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
    if (B.hasValidInsertionPoint())
      B.createBranch(LoopBB);

    Cond.exitTrue(B);
    // Complete the conditional execution.
    Cond.complete(B);
  }
  
  emitOrDeleteBlock(B, EndBB);
  BreakDestStack.pop_back();
  ContinueDestStack.pop_back();
}

void CFGGen::visitForStmt(ForStmt *S) {
  // Enter a new scope.
  // FIXME
  //Scope ForScope(*this);
  
  // Emit any local 'var' variables declared in the initializer.
  // FIXME:
  //for (auto D : S->getInitializerVarDecls())
  //  emitLocal(D);
  
  if (Expr *E = S->getInitializer().dyn_cast<Expr*>()) {
    // FIXME.
    //FullExpr Scope(*this);
    visit(E);
  } else if (Stmt *AS = S->getInitializer().dyn_cast<AssignStmt*>()) {
    visit(AS);
  }
  
  // If we ever reach an unreachable point, stop emitting statements.
  // This will need revision if we ever add goto.
  if (!B.hasValidInsertionPoint()) return;
  
  // Create a new basic block and jump into it.
  BasicBlock *LoopBB = new (C) BasicBlock(&C, "for.condition");
  B.emitBlock(LoopBB);
  
  // Set the destinations for 'break' and 'continue'
  BasicBlock *IncBB = new (C) BasicBlock(&C, "for.inc");
  BasicBlock *EndBB = new (C) BasicBlock(&C, "for.end");
  BreakDestStack.emplace_back(EndBB, getCleanupsDepth());
  ContinueDestStack.emplace_back(IncBB, getCleanupsDepth());
  
  // Evaluate the condition with the false edge leading directly
  // to the continuation block.
  Condition Cond = S->getCond().isNonNull() ?
    emitCondition(S, S->getCond().get(), /*hasFalseCode*/ false) :
    Condition(LoopBB, 0, 0);  // Infinite loop.
  
  // If there's a true edge, emit the body in it.
  if (Cond.hasTrue()) {
    Cond.enterTrue(B);
    visit(S->getBody());
    
    emitOrDeleteBlock(B, IncBB);
    
    if (B.hasValidInsertionPoint() && !S->getIncrement().isNull()) {
      if (Expr *E = S->getIncrement().dyn_cast<Expr*>()) {
        // FIXME:
        //FullExpr scope(*this);
        visit(E);
      } else if (Stmt *AS = S->getIncrement().dyn_cast<AssignStmt*>()) {
        visit(AS);
      }
    }
    
    if (B.hasValidInsertionPoint())
      B.createBranch(LoopBB);
    Cond.exitTrue(B);
  }
  
  // Complete the conditional execution.
  Cond.complete(B);
  
  emitOrDeleteBlock(B, EndBB);
  BreakDestStack.pop_back();
  ContinueDestStack.pop_back();
}

void CFGGen::visitForEachStmt(ForEachStmt *S) {
  // Emit the 'range' variable that we'll be using for iteration.
  // FIXME: Scope, PBD.
  //Scope OuterForScope(*this);
  //emitPatternBindingDecl(S->getRange());
  
  // If we ever reach an unreachable point, stop emitting statements.
  // This will need revision if we ever add goto.
  if (!B.hasValidInsertionPoint()) return;
  
  // Create a new basic block and jump into it.
  BasicBlock *LoopBB = new (C) BasicBlock(&C, "foreach.cond");
  B.emitBlock(LoopBB);
  
  // Set the destinations for 'break' and 'continue'
  BasicBlock *EndBB = new (C) BasicBlock(&C, "foreach.end");
  BreakDestStack.emplace_back(EndBB, getCleanupsDepth());
  ContinueDestStack.emplace_back(LoopBB, getCleanupsDepth());
  
  Condition Cond = emitCondition(S, S->getRangeEmpty(), /*hasFalseCode=*/false,
                                 /*invertValue=*/true);
  if (Cond.hasTrue()) {
    Cond.enterTrue(B);
    
    // Emit the loop body.
    // The declared variable(s) for the current element are destroyed
    // at the end of each loop iteration.
    {
      //Scope InnerForScope(*this);
      //emitPatternBindingDecl(S->getElementInit());
      visit(S->getBody());
    }
    
    // Loop back to the header.
    if (B.hasValidInsertionPoint())
      B.createBranch(LoopBB);
    Cond.exitTrue(B);
  }
  
  // Complete the conditional execution.
  Cond.complete(B);
  
  emitOrDeleteBlock(B, EndBB);
  BreakDestStack.pop_back();
  ContinueDestStack.pop_back();
}



//===--------------------------------------------------------------------===//
// Expressions
//===--------------------------------------------------------------------===//

CFGValue CFGGen::visitApplyExpr(ApplyExpr *E) {
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
  
  return B.createApply(E, FnV, ArgsV);
}

CFGValue CFGGen::visitDeclRefExpr(DeclRefExpr *E) {
  return B.createDeclRef(E);
}

CFGValue CFGGen::visitIntegerLiteralExpr(IntegerLiteralExpr *E) {
  return B.createIntegerLiteral(E);
}

CFGValue CFGGen::visitLoadExpr(LoadExpr *E) {
  CFGValue SubV = visit(E->getSubExpr());
  return B.createLoad(E, SubV);
}

CFGValue CFGGen::visitThisApplyExpr(ThisApplyExpr *E) {
  CFGValue FnV = visit(E->getFn());
  CFGValue ArgV = visit(E->getArg());
  return B.createThisApply(E, FnV, ArgV);
}

CFGValue CFGGen::visitParenExpr(ParenExpr *E) {
  return visit(E->getSubExpr());
}

CFGValue CFGGen::visitTupleExpr(TupleExpr *E) {
  llvm::SmallVector<CFGValue, 10> ArgsV;
  for (auto &I : E->getElements())
    ArgsV.push_back(visit(I));
  return B.createTuple(E, ArgsV);
}

CFGValue CFGGen::visitTypeOfExpr(TypeOfExpr *E) {
  return B.createTypeOf(E);
}

//===--------------------------------------------------------------------===//
// CFG Class implementation
//===--------------------------------------------------------------------===//

CFG *CFG::constructCFG(Stmt *S) {
  CFG *C = new CFG();
  CFGGen(*C).visit(S);
  
  C->verify();
  return C;
}
