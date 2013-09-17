//===--- SILGenStmt.cpp - Implements Lowering of ASTs -> SIL for Stmts ----===//
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

#include "SILGen.h"
#include "Scope.h"
#include "Condition.h"
#include "Initialization.h"
#include "LValue.h"
#include "RValue.h"
#include "swift/AST/AST.h"
#include "swift/SIL/SILArgument.h"
#include "swift/AST/Diagnostics.h"

using namespace swift;
using namespace Lowering;

//===--------------------------------------------------------------------===//
// SILGenFunction visit*Stmt implementation
//===--------------------------------------------------------------------===//

/// emitOrDeleteBlock - If there are branches to the specified basic block,
/// emit it per emitBlock.  If there aren't, then just delete the block - it
/// turns out to have not been needed.
static void emitOrDeleteBlock(SILBuilder &B, SILBasicBlock *BB,
                              SILLocation BranchLoc) {
  if (BB->pred_empty()) {
    // If the block is unused, we don't need it; just delete it.
    BB->eraseFromParent();
  } else {
    // Otherwise, continue emitting code in BB.
    B.emitBlock(BB, BranchLoc);
  }
}

static SILValue emitConditionValue(SILGenFunction &gen, Expr *E) {
  // Sema forces conditions to have Builtin.i1 type, which guarantees this.
  SILValue V;
  {
    FullExpr Scope(gen.Cleanups, CleanupLocation(E));
    V = gen.emitRValue(E).forwardAsSingleValue(gen, E);
  }
  assert(V.getType().castTo<BuiltinIntegerType>()->getBitWidth() == 1);

  return V;
}

Condition SILGenFunction::emitCondition(Expr *E,
                                        bool hasFalseCode, bool invertValue,
                                        ArrayRef<SILType> contArgs) {
  assert(B.hasValidInsertionPoint() &&
         "emitting condition at unreachable point");
  
  SILValue V = emitConditionValue(*this, E);
  
  SILBasicBlock *ContBB = createBasicBlock();
  SILBasicBlock *TrueBB = createBasicBlock();

  for (SILType argTy : contArgs) {
    new (F.getModule()) SILArgument(argTy, ContBB);
  }
  
  SILBasicBlock *FalseBB, *FalseDestBB;
  if (hasFalseCode) {
    FalseBB = FalseDestBB = createBasicBlock();
  } else {
    FalseBB = nullptr;
    FalseDestBB = ContBB;
  }
  RegularLocation Loc(E);
  if (invertValue)
    B.createCondBranch(Loc, V, FalseDestBB, TrueBB);
  else
    B.createCondBranch(Loc, V, TrueBB, FalseDestBB);
  
  return Condition(TrueBB, FalseBB, ContBB, Loc);
}

void SILGenFunction::visitBraceStmt(BraceStmt *S) {
  // Enter a new scope.
  LexicalScope BraceScope(Cleanups, *this, CleanupLocation(S));

  for (auto &ESD : S->getElements()) {
    assert(B.hasValidInsertionPoint());
    
    if (Stmt *S = ESD.dyn_cast<Stmt*>()) {
      visit(S);
      
      // If we ever reach an unreachable point, stop emitting statements.
      // This will need revision if we ever add goto.
      if (!B.hasValidInsertionPoint()) return;
    } else if (Expr *E = ESD.dyn_cast<Expr*>()) {
      FullExpr scope(Cleanups, CleanupLocation(E));
      emitRValue(E);
    } else
      visit(ESD.get<Decl*>());
  }
}

namespace {

/// IndirectReturnInitialization - represents initializing an indirect return
/// value.
class IndirectReturnInitialization : public SingleInitializationBase {
  SILValue address;
public:
  IndirectReturnInitialization(SILValue address)
    : address(address) {}
  
  SILValue getAddressOrNull() override { return address; }
};

} // end anonymous namespace

void SILGenFunction::emitReturnExpr(SILLocation branchLoc,
                                    Expr *ret) {
  SILValue result;
  if (IndirectReturnAddress) {
    // Indirect return of an address-only value.
    FullExpr scope(Cleanups, CleanupLocation(ret));
    InitializationPtr returnInit(
                       new IndirectReturnInitialization(IndirectReturnAddress));
    emitExprInto(ret, returnInit.get());
  } else {
    // SILValue return.
    FullExpr scope(Cleanups, CleanupLocation(ret));
    RValue resultRValue = emitRValue(ret);
    if (!resultRValue.getType()->isVoid()) {
      result = std::move(resultRValue).forwardAsSingleValue(*this, ret);
      result = emitGeneralizedValue(ret, result);
    }
  }
  Cleanups.emitBranchAndCleanups(ReturnDest, branchLoc,
                                 result ? result : ArrayRef<SILValue>{});
}

void SILGenFunction::visitReturnStmt(ReturnStmt *S) {
  CurrentSILLoc = S;
  ReturnLocation Loc(S);

  SILValue ArgV;
  if (!S->hasResult())
    // Void return.
    Cleanups.emitBranchAndCleanups(ReturnDest, Loc);
  else
    emitReturnExpr(Loc, S->getResult());
}

void SILGenFunction::visitIfStmt(IfStmt *S) {
  Condition Cond = emitCondition(S->getCond(), S->getElseStmt() != nullptr);
  
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

void SILGenFunction::visitWhileStmt(WhileStmt *S) {
  // Create a new basic block and jump into it.
  SILBasicBlock *LoopBB = createBasicBlock();
  B.emitBlock(LoopBB, S);
  
  // Set the destinations for 'break' and 'continue'
  SILBasicBlock *EndBB = createBasicBlock();
  BreakDestStack.emplace_back(EndBB, getCleanupsDepth(),
                              CleanupLocation(S->getBody()));
  ContinueDestStack.emplace_back(LoopBB, getCleanupsDepth(),
                                 CleanupLocation(S->getBody()));
  
  // Evaluate the condition with the false edge leading directly
  // to the continuation block.
  Condition Cond = emitCondition(S->getCond(), /*hasFalseCode*/ false);
  
  // If there's a true edge, emit the body in it.
  if (Cond.hasTrue()) {
    Cond.enterTrue(B);
    visit(S->getBody());
    if (B.hasValidInsertionPoint()) {
      // Accosiate the loop body's closing brace with this branch.
      RegularLocation L(S->getBody());
      L.pointToEnd();
      B.createBranch(L, LoopBB);
    }
    Cond.exitTrue(B);
  }
  
  // Complete the conditional execution.
  Cond.complete(B);
  
  emitOrDeleteBlock(B, EndBB, S);
  BreakDestStack.pop_back();
  ContinueDestStack.pop_back();
}

void SILGenFunction::visitDoWhileStmt(DoWhileStmt *S) {
  // Create a new basic block and jump into it.
  SILBasicBlock *LoopBB = createBasicBlock();
  B.emitBlock(LoopBB, S);
  
  // Set the destinations for 'break' and 'continue'
  SILBasicBlock *EndBB = createBasicBlock();
  SILBasicBlock *CondBB = createBasicBlock();
  BreakDestStack.emplace_back(EndBB, getCleanupsDepth(),
                              CleanupLocation(S->getBody()));
  ContinueDestStack.emplace_back(CondBB, getCleanupsDepth(),
                                 CleanupLocation(S->getBody()));
  
  // Emit the body, which is always evaluated the first time around.
  visit(S->getBody());

  // Let's not differ from C99 6.8.5.2: "The evaluation of the controlling
  // expression takes place after each execution of the loop body."
  emitOrDeleteBlock(B, CondBB, S);

  if (B.hasValidInsertionPoint()) {
    // Evaluate the condition with the false edge leading directly
    // to the continuation block.
    Condition Cond = emitCondition(S->getCond(), /*hasFalseCode*/ false);
    
    Cond.enterTrue(B);
    if (B.hasValidInsertionPoint()) {
      B.createBranch(S->getCond(), LoopBB);
    }
    
    Cond.exitTrue(B);
    // Complete the conditional execution.
    Cond.complete(B);
  }
  
  emitOrDeleteBlock(B, EndBB, S);
  BreakDestStack.pop_back();
  ContinueDestStack.pop_back();
}

void SILGenFunction::visitForStmt(ForStmt *S) {
  // Enter a new scope.
  Scope ForScope(Cleanups, CleanupLocation(S));
  
  // Emit any local 'var' variables declared in the initializer.
  for (auto D : S->getInitializerVarDecls())
    visit(D);
  
  if (auto *Initializer = S->getInitializer().getPtrOrNull()) {
    FullExpr Scope(Cleanups, CleanupLocation(Initializer));
    emitRValue(Initializer);
  }
  
  // If we ever reach an unreachable point, stop emitting statements.
  // This will need revision if we ever add goto.
  if (!B.hasValidInsertionPoint()) return;
  
  // Create a new basic block and jump into it.
  SILBasicBlock *LoopBB = createBasicBlock();
  B.emitBlock(LoopBB, S);
  
  // Set the destinations for 'break' and 'continue'
  SILBasicBlock *IncBB = createBasicBlock();
  SILBasicBlock *EndBB = createBasicBlock();
  BreakDestStack.emplace_back(EndBB, getCleanupsDepth(),
                              CleanupLocation(S->getBody()));
  ContinueDestStack.emplace_back(IncBB, getCleanupsDepth(),
                                 CleanupLocation(S->getBody()));
  
  // Evaluate the condition with the false edge leading directly
  // to the continuation block.
  Condition Cond = S->getCond().isNonNull() ?
    emitCondition(S->getCond().get(), /*hasFalseCode*/ false) :
    Condition(LoopBB, 0, 0, S); // Infinite loop.
  
  // If there's a true edge, emit the body in it.
  if (Cond.hasTrue()) {
    Cond.enterTrue(B);
    visit(S->getBody());
    
    emitOrDeleteBlock(B, IncBB, S);
    
    if (B.hasValidInsertionPoint() && S->getIncrement().isNonNull()) {
      FullExpr Scope(Cleanups, CleanupLocation(S->getIncrement().get()));
      emitRValue(S->getIncrement().get());
    }
    
    if (B.hasValidInsertionPoint()) {
      // Accosiate the loop body's closing brace with this branch.
      RegularLocation L(S->getBody());
      L.pointToEnd();
      B.createBranch(L, LoopBB);
    }
    Cond.exitTrue(B);
  }
  
  // Complete the conditional execution.
  Cond.complete(B);
  
  emitOrDeleteBlock(B, EndBB, S);
  BreakDestStack.pop_back();
  ContinueDestStack.pop_back();
}

void SILGenFunction::visitForEachStmt(ForEachStmt *S) {
  // Emit the 'range' variable that we'll be using for iteration.
  Scope OuterForScope(Cleanups, CleanupLocation(S));
  visitPatternBindingDecl(S->getRange());
  
  // If we ever reach an unreachable point, stop emitting statements.
  // This will need revision if we ever add goto.
  if (!B.hasValidInsertionPoint()) return;
  
  // Create a new basic block and jump into it.
  SILBasicBlock *LoopBB = createBasicBlock();
  B.emitBlock(LoopBB, S);
  
  // Set the destinations for 'break' and 'continue'
  SILBasicBlock *EndBB = createBasicBlock();
  BreakDestStack.emplace_back(EndBB,
                              getCleanupsDepth(),
                              CleanupLocation(S->getBody()));
  ContinueDestStack.emplace_back(LoopBB, getCleanupsDepth(),
                                 CleanupLocation(S->getBody()));
  
  Condition Cond = emitCondition(S->getRangeEmpty(), /*hasFalseCode=*/false,
                                 /*invertValue=*/true);
  if (Cond.hasTrue()) {
    Cond.enterTrue(B);
    
    // Emit the loop body.
    // The declared variable(s) for the current element are destroyed
    // at the end of each loop iteration.
    {
      Scope InnerForScope(Cleanups, CleanupLocation(S->getBody()));
      visitPatternBindingDecl(S->getElementInit());
      visit(S->getBody());
    }
    
    // Loop back to the header.
    if (B.hasValidInsertionPoint()) {
      // Accosiate the loop body's closing brace with this branch.
      RegularLocation L(S->getBody());
      L.pointToEnd();
      B.createBranch(L, LoopBB);
    }
    Cond.exitTrue(B);
  }
  
  // Complete the conditional execution.
  Cond.complete(B);
  
  emitOrDeleteBlock(B, EndBB, S);
  BreakDestStack.pop_back();
  ContinueDestStack.pop_back();
}

void SILGenFunction::visitBreakStmt(BreakStmt *S) {
  CurrentSILLoc = S;
  Cleanups.emitBranchAndCleanups(BreakDestStack.back(), S);
}

void SILGenFunction::visitContinueStmt(ContinueStmt *S) {
  CurrentSILLoc = S;
  Cleanups.emitBranchAndCleanups(ContinueDestStack.back(), S);
}

void SILGenFunction::visitSwitchStmt(SwitchStmt *S) {
  // Implemented in SILGenPattern.cpp.
  emitSwitchStmt(S);
}

void SILGenFunction::visitCaseStmt(CaseStmt *S) {
  llvm_unreachable("cases should be lowered as part of switch stmt");
}

void SILGenFunction::visitFallthroughStmt(FallthroughStmt *S) {
  // Implemented in SILGenPattern.cpp.
  emitSwitchFallthrough(S);
}

//===--------------------------------------------------------------------===//
// SILGenModule visitTopLevelCodeDecl implementation
//===--------------------------------------------------------------------===//

void SILGenModule::visitTopLevelCodeDecl(TopLevelCodeDecl *td) {
  // Emit top-level statements and expressions into the toplevel function until
  // we hit an unreachable point.
  assert(TopLevelSGF && "top-level code in a non-main module!");
  if (!TopLevelSGF->B.hasValidInsertionPoint())
    return;
  
  TopLevelSGF->visit(td->getBody());
}
