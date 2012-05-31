//===--- GenStmt.cpp - IR Generation for Statements -----------------------===//
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
//  This file implements IR generation for Swift statements.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/Expr.h"
#include "swift/AST/Stmt.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/Support/ErrorHandling.h"
#include "Condition.h"
#include "Scope.h"
#include "GenType.h"
#include "IRGenFunction.h"
#include "JumpDest.h"
#include "LValue.h"

using namespace swift;
using namespace irgen;

void IRGenFunction::emitStmt(Stmt *S) {
  switch (S->getKind()) {
  case StmtKind::Error: assert(0 && "Invalid programs shouldn't get here");
  case StmtKind::Semi:
    // Nothing to do.
    return;

  case StmtKind::Assign:
    return emitAssignStmt(cast<AssignStmt>(S));

  case StmtKind::Brace:
    return emitBraceStmt(cast<BraceStmt>(S));

  case StmtKind::Return:
    return emitReturnStmt(cast<ReturnStmt>(S));

  case StmtKind::If:
    return emitIfStmt(cast<IfStmt>(S));

  case StmtKind::While:
    return emitWhileStmt(cast<WhileStmt>(S));
      
  case StmtKind::For:
    return emitForStmt(cast<ForStmt>(S));
      
  case StmtKind::ForEach:
    return emitForEachStmt(cast<ForEachStmt>(S));

  case StmtKind::Break:
    return emitBreakStmt(cast<BreakStmt>(S));

  case StmtKind::Continue:
    return emitContinueStmt(cast<ContinueStmt>(S));
  }
  llvm_unreachable("bad statement kind!");
}

void IRGenFunction::emitBraceStmt(BraceStmt *BS) {
  // Enter a new scope.
  Scope BraceScope(*this);

  for (auto Elt : BS->getElements()) {
    assert(Builder.hasValidIP());

    if (Expr *E = Elt.dyn_cast<Expr*>()) {
      FullExpr scope(*this);
      emitIgnored(E);
    } else if (Stmt *S = Elt.dyn_cast<Stmt*>()) {
      emitStmt(S);

      // If we ever reach an unreachable point, stop emitting statements.
      // This will need revision if we ever add goto.
      if (!Builder.hasValidIP()) return;
    } else {
      emitLocal(Elt.get<Decl*>());
    }
  }
}

/// Emit an assignment statement.
void IRGenFunction::emitAssignStmt(AssignStmt *S) {
  const TypeInfo &type = getFragileTypeInfo(S->getSrc()->getType());
  LValue LV = emitLValue(S->getDest());
  emitAssign(S->getSrc(), LV, type);
}

void IRGenFunction::emitIfStmt(IfStmt *S) {
  Condition cond = emitCondition(S->getCond(), S->getElseStmt() != nullptr);
  if (cond.hasTrue()) {
    cond.enterTrue(*this);
    emitStmt(S->getThenStmt());
    cond.exitTrue(*this);
  }

  if (cond.hasFalse()) {
    assert(S->getElseStmt());
    cond.enterFalse(*this);
    emitStmt(S->getElseStmt());
    cond.exitFalse(*this);
  }

  cond.complete(*this);
}

void IRGenFunction::emitReturnStmt(ReturnStmt *S) {
  // The expression is evaluated in a full-expression context.
  FullExpr fullExpr(*this);

  // If this function takes no return value, ignore the result of the
  // expression.
  if (!ReturnSlot.isValid()) {
    if (S->hasResult())
      emitIgnored(S->getResult());
  } else {
    const TypeInfo &resultType = getFragileTypeInfo(S->getResult()->getType());
    emitInit(S->getResult(), ReturnSlot, resultType);
  }

  // Leave the full-expression.
  fullExpr.pop();

  // In either case, branch to the return block.
  emitBranch(JumpDest(ReturnBB, Cleanups.stable_end()));
  Builder.ClearInsertionPoint();
}

static void emitOrDeleteBlock(IRGenFunction &IGF, llvm::BasicBlock *BB) {
  if (BB->use_empty()) {
    // If the block is unused, we don't need it; just delete it.
    delete BB;
  } else {
    // Otherwise, continue emitting code in BB.
    if (IGF.Builder.hasValidIP())
      IGF.Builder.CreateBr(BB);
    IGF.Builder.emitBlockAnywhere(BB);
  }
}

void IRGenFunction::emitWhileStmt(WhileStmt *S) {
  // Create a new basic block and jump into it.
  llvm::BasicBlock *loopBB = createBasicBlock("while");
  Builder.CreateBr(loopBB);
  Builder.emitBlock(loopBB);

  // Set the destinations for 'break' and 'continue'
  llvm::BasicBlock *endBB = createBasicBlock("while.end");
  BreakDestStack.emplace_back(endBB, getCleanupsDepth());
  ContinueDestStack.emplace_back(loopBB, getCleanupsDepth());

  // Evaluate the condition with the false edge leading directly
  // to the continuation block.
  Condition cond = emitCondition(S->getCond(), /*hasFalseCode*/ false);

  // If there's a true edge, emit the body in it.
  if (cond.hasTrue()) {
    cond.enterTrue(*this);
    emitStmt(S->getBody());
    if (Builder.hasValidIP()) {
      Builder.CreateBr(loopBB);
      Builder.ClearInsertionPoint();
    }
    cond.exitTrue(*this);
  }

  // Complete the conditional execution.
  cond.complete(*this);

  emitOrDeleteBlock(*this, endBB);
  BreakDestStack.pop_back();
  ContinueDestStack.pop_back();
}

void IRGenFunction::emitForStmt(ForStmt *S) {
  // Enter a new scope.
  Scope ForScope(*this);

  // Emit any local 'var' variables declared in the initializer.
  for (auto D : S->getInitializerVarDecls())
    emitLocal(D);
  
  if (Expr *E = S->getInitializer().dyn_cast<Expr*>()) {
    FullExpr scope(*this);
    emitIgnored(E);
  } else if (Stmt *AS = S->getInitializer().dyn_cast<AssignStmt*>()) {
    emitStmt(AS);
  }
  
  // If we ever reach an unreachable point, stop emitting statements.
  // This will need revision if we ever add goto.
  if (!Builder.hasValidIP()) return;

  // Create a new basic block and jump into it.
  llvm::BasicBlock *loopBB = createBasicBlock("for.condition");
  Builder.CreateBr(loopBB);
  Builder.emitBlock(loopBB);

  // Set the destinations for 'break' and 'continue'
  llvm::BasicBlock *incBB = createBasicBlock("for.inc");
  llvm::BasicBlock *endBB = createBasicBlock("for.end");
  BreakDestStack.emplace_back(endBB, getCleanupsDepth());
  ContinueDestStack.emplace_back(incBB, getCleanupsDepth());

  // Evaluate the condition with the false edge leading directly
  // to the continuation block.
  Condition cond = S->getCond().isNonNull() ?
    emitCondition(S->getCond().get(), /*hasFalseCode*/ false) :
    Condition(loopBB, 0, 0);  // Infinite loop.
  
  // If there's a true edge, emit the body in it.
  if (cond.hasTrue()) {
    cond.enterTrue(*this);
    emitStmt(S->getBody());

    emitOrDeleteBlock(*this, incBB);

    if (Builder.hasValidIP() && !S->getIncrement().isNull()) {
      if (Expr *E = S->getIncrement().dyn_cast<Expr*>()) {
        FullExpr scope(*this);
        emitIgnored(E);
      } else if (Stmt *AS = S->getIncrement().dyn_cast<AssignStmt*>()) {
        emitStmt(AS);
      }
    }
      
    if (Builder.hasValidIP()) {
      Builder.CreateBr(loopBB);
      Builder.ClearInsertionPoint();
    }
    cond.exitTrue(*this);
  }
  
  // Complete the conditional execution.
  cond.complete(*this);

  emitOrDeleteBlock(*this, endBB);
  BreakDestStack.pop_back();
  ContinueDestStack.pop_back();
}

void IRGenFunction::emitForEachStmt(ForEachStmt *S) {
  // Emit the 'range' variable that we'll be using for iteration.
  Scope OuterForScope(*this);
  emitPatternBindingDecl(S->getRange());
  
  // If we ever reach an unreachable point, stop emitting statements.
  // This will need revision if we ever add goto.
  if (!Builder.hasValidIP()) return;

  // Create a new basic block and jump into it.
  llvm::BasicBlock *loopBB = createBasicBlock("foreach.cond");
  Builder.CreateBr(loopBB);
  Builder.emitBlock(loopBB);

  // Set the destinations for 'break' and 'continue'
  llvm::BasicBlock *endBB = createBasicBlock("foreach.end");
  BreakDestStack.emplace_back(endBB, getCleanupsDepth());
  ContinueDestStack.emplace_back(loopBB, getCleanupsDepth());

  Condition Cond = emitCondition(S->getRangeEmpty(), /*hasFalseCode=*/false,
                                 /*invertValue=*/true);
  if (Cond.hasTrue()) {
    Cond.enterTrue(*this);

    // Emit the loop body.
    // The declared variable(s) for the current element are destroyed
    // at the end of each loop iteration.
    {
      Scope InnerForScope(*this);
      emitPatternBindingDecl(S->getElementInit());
      emitStmt(S->getBody());
    }
    
    // Loop back to the header.
    if (Builder.hasValidIP()) {
      Builder.CreateBr(loopBB);
      Builder.ClearInsertionPoint();
    }
    Cond.exitTrue(*this);
  }
  
  // Complete the conditional execution.
  Cond.complete(*this);

  emitOrDeleteBlock(*this, endBB);
  BreakDestStack.pop_back();
  ContinueDestStack.pop_back();
}

void IRGenFunction::emitBreakStmt(BreakStmt *S) {
  emitBranch(BreakDestStack.back());
  Builder.ClearInsertionPoint();
}

void IRGenFunction::emitContinueStmt(ContinueStmt *S) {
  emitBranch(ContinueDestStack.back());
  Builder.ClearInsertionPoint();
}
