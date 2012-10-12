//===--- CFGGenStmt.cpp - Implements Lowering of ASTs -> CFGs for Stmts ---===//
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
#include "Scope.h"
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
  Value *V = nullptr;
  {
    FullExpr Scope(Cleanups);
    V = visit(E);
  }
  assert(V->getType()->castTo<BuiltinIntegerType>()->getBitWidth() == 1);
  
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



void CFGGen::visitBraceStmt(BraceStmt *S) {
  // Enter a new scope.
  Scope BraceScope(Cleanups);
  
  for (auto &ESD : S->getElements()) {
    assert(B.hasValidInsertionPoint());
    
    if (Stmt *S = ESD.dyn_cast<Stmt*>()) {
      visit(S);
      
      // If we ever reach an unreachable point, stop emitting statements.
      // This will need revision if we ever add goto.
      if (!B.hasValidInsertionPoint()) return;
    } else if (Expr *E = ESD.dyn_cast<Expr*>()) {
      FullExpr scope(Cleanups);
      visit(E);
    } else
      visit(ESD.get<Decl*>());
  }
}


/// emitAssignStmtRecursive - Used to destructure (potentially) recursive
/// assignments into tuple expressions down to their scalar stores.
static void emitAssignStmtRecursive(AssignStmt *S, Value *V, Expr *Dest,
                                    CFGGen &Gen) {
  // If the destination is a tuple, destructure.
  if (TupleExpr *TE = dyn_cast<TupleExpr>(Dest)) {
    for (Expr *Elem : TE->getElements()) {
      (void)Elem;
      assert(0 && "FIXME: unimplemented");
    }
    //emitAssignStmtRecursive(IGF, value, elem);
    return;
  }
  
  // Otherwise, emit the scalar assignment.
  Value *DstV = Gen.visit(Dest);
  Gen.B.createStore(S, V, DstV);
}


void CFGGen::visitAssignStmt(AssignStmt *S) {
  Value *SrcV = visit(S->getSrc());
  
  // Handle tuple destinations by destructuring them if present.
  return emitAssignStmtRecursive(S, SrcV, S->getDest(), *this);
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
  Scope ForScope(Cleanups);
  
  // Emit any local 'var' variables declared in the initializer.
  for (auto D : S->getInitializerVarDecls())
    visit(D);
  
  if (Expr *E = S->getInitializer().dyn_cast<Expr*>()) {
    FullExpr Scope(Cleanups);
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
        FullExpr Scope(Cleanups);
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
  Scope OuterForScope(Cleanups);
  visitPatternBindingDecl(S->getRange());
  
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
      Scope InnerForScope(Cleanups);
      visitPatternBindingDecl(S->getElementInit());
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

void CFGGen::visitBreakStmt(BreakStmt *S) {
  Cleanups.emitBranchAndCleanups(BreakDestStack.back());
}

void CFGGen::visitContinueStmt(ContinueStmt *S) {
  Cleanups.emitBranchAndCleanups(ContinueDestStack.back());
}
