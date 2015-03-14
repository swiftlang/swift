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
#include "swift/AST/DiagnosticsSIL.h"

using namespace swift;
using namespace Lowering;

template<typename...T, typename...U>
static void diagnose(ASTContext &Context, SourceLoc loc, Diag<T...> diag,
                     U &&...args) {
  Context.Diags.diagnose(loc,
                         diag, std::forward<U>(args)...);
}

//===----------------------------------------------------------------------===//
// SILGenFunction visit*Stmt implementation
//===----------------------------------------------------------------------===//

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

Condition SILGenFunction::emitCondition(Expr *E,
                                        bool hasFalseCode, bool invertValue,
                                        ArrayRef<SILType> contArgs) {
  assert(B.hasValidInsertionPoint() &&
         "emitting condition at unreachable point");

  // Sema forces conditions to have Builtin.i1 type, which guarantees this.
  SILValue V;
  {
    FullExpr Scope(Cleanups, CleanupLocation(E));
    V = emitRValue(E).forwardAsSingleValue(*this, E);
  }
  assert(V.getType().castTo<BuiltinIntegerType>()->isFixedWidth(1));

  return emitCondition(V, E, hasFalseCode, invertValue, contArgs);
}



Condition SILGenFunction::emitCondition(SILValue V, SILLocation Loc,
                                        bool hasFalseCode, bool invertValue,
                                        ArrayRef<SILType> contArgs) {
  assert(B.hasValidInsertionPoint() &&
         "emitting condition at unreachable point");

  SILBasicBlock *ContBB = createBasicBlock();
  SILBasicBlock *TrueBB = createBasicBlock();

  for (SILType argTy : contArgs) {
    new (F.getModule()) SILArgument(ContBB, argTy);
  }
  
  SILBasicBlock *FalseBB, *FalseDestBB;
  if (hasFalseCode) {
    FalseBB = FalseDestBB = createBasicBlock();
  } else {
    FalseBB = nullptr;
    FalseDestBB = ContBB;
  }
  if (invertValue)
    B.createCondBranch(Loc, V, FalseDestBB, TrueBB);
  else
    B.createCondBranch(Loc, V, TrueBB, FalseDestBB);
  
  return Condition(TrueBB, FalseBB, ContBB, Loc);
}

void SILGenFunction::visitBraceStmt(BraceStmt *S) {
  // Enter a new scope.
  LexicalScope BraceScope(Cleanups, *this, CleanupLocation(S));
  const unsigned ReturnStmtType   = 0;
  const unsigned ContinueStmtType = 1;
  const unsigned UnknownStmtType  = 2;
  unsigned StmtType = UnknownStmtType;
  
  for (auto &ESD : S->getElements()) {
    
    if (auto S = ESD.dyn_cast<Stmt*>()) {
      if (isa<IfConfigStmt>(S)) {
        continue;
      }
    }
    
    // If we ever reach an unreachable point, stop emitting statements and issue
    // an unreachable code diagnostic. This will need revision if we ever add
    // goto.
    if (!B.hasValidInsertionPoint()) {
      if (StmtType != UnknownStmtType) {
        diagnose(getASTContext(), ESD.getStartLoc(),
                 diag::unreachable_code_after_stmt, StmtType);
      } else {
        diagnose(getASTContext(), ESD.getStartLoc(), diag::unreachable_code);
      }
      return;
    }

    // Process children.
    if (Stmt *S = ESD.dyn_cast<Stmt*>()) {
      visit(S);
      if (isa<ContinueStmt>(S))
        StmtType = ContinueStmtType;
      if (isa<ReturnStmt>(S))
        StmtType = ReturnStmtType;
      
    } else if (Expr *E = ESD.dyn_cast<Expr*>()) {
      emitIgnoredExpr(E);
    } else {
      visit(ESD.get<Decl*>());
    }
  }
}

namespace {

/// IndirectReturnInitialization - represents initializing an indirect return
/// value.
class IndirectReturnInitialization : public SingleBufferInitialization {
  SILValue address;
public:
  IndirectReturnInitialization(SILValue address)
    : address(address) {}
  
  SILValue getAddressOrNull() const override { return address; }
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
    }
  }
  Cleanups.emitBranchAndCleanups(ReturnDest, branchLoc,
                                 result ? result : ArrayRef<SILValue>{});
}

void SILGenFunction::visitReturnStmt(ReturnStmt *S) {
  CurrentSILLoc = S;
  SILLocation Loc = S->isImplicit() ?
                      (SILLocation)ImplicitReturnLocation(S) :
                      (SILLocation)ReturnLocation(S);

  SILValue ArgV;
  if (!S->hasResult())
    // Void return.
    Cleanups.emitBranchAndCleanups(ReturnDest, Loc);
  else
    emitReturnExpr(Loc, S->getResult());
}

void SILGenFunction::visitIfStmt(IfStmt *S) {
  Scope condBufferScope(Cleanups, S);
  
  // Create a continuation block.  We need it if there is a labeled break out
  // of the if statement or if there is an if/then/else.
  auto *ContBB = createBasicBlock();

  // Set the destinations for any 'break' and 'continue' statements inside the
  // body.  Note that "continue" is not valid out of a labeled 'if'.
  BreakContinueDestStack.push_back(std::make_tuple(
    S,
    JumpDest(ContBB, getCleanupsDepth(), CleanupLocation(S->getThenStmt())),
    JumpDest(CleanupLocation(S))
  ));

  // Set up the block for the false case.  If there is an 'else' block, we make
  // a new one, otherwise it is our continue block.
  SILBasicBlock *CondFalseBB;
  if (!S->getElseStmt())
    CondFalseBB = ContBB;
  else
    CondFalseBB = createBasicBlock();

  // Emit the condition, along with the "then" part of the if properly guarded
  // by the condition and a jump to ContBB.  If the condition fails, jump to
  // the CondFalseBB.
  emitStmtConditionWithBody(S, ContBB, CondFalseBB);

  // If there is 'else' logic, then emit it.
  if (S->getElseStmt()) {
    B.emitBlock(CondFalseBB);
    visit(S->getElseStmt());
    if (B.hasValidInsertionPoint()) {
      RegularLocation L(S->getElseStmt());
      L.pointToEnd();
      B.createBranch(L, ContBB);
    }
  }

  // If the continuation block was used, emit it now, otherwise remove it.
  if (ContBB->pred_empty())
    ContBB->eraseFromParent();
  else {
    RegularLocation L(S->getThenStmt());
    L.pointToEnd();
    B.emitBlock(ContBB, L);
  }
  BreakContinueDestStack.pop_back();
}

void SILGenFunction::visitIfConfigStmt(IfConfigStmt *S) {
  // Active members are attached to the enclosing declaration, so there's no
  // need to walk anything within.
}

void SILGenFunction::visitWhileStmt(WhileStmt *S) {
  Scope condBufferScope(Cleanups, S);
  
  // Create a new basic block and jump into it.
  SILBasicBlock *LoopBB = createBasicBlock();
  B.emitBlock(LoopBB, S);
  
  // Create a break target (at this level in the cleanup stack) in case it is
  // needed.
  auto *BreakBB = createBasicBlock();

  // Set the destinations for any 'break' and 'continue' statements inside the
  // body.
  BreakContinueDestStack.push_back(std::make_tuple(
    S,
    JumpDest(BreakBB, getCleanupsDepth(), CleanupLocation(S->getBody())),
    JumpDest(LoopBB , getCleanupsDepth(), CleanupLocation(S->getBody()))
  ));
  
  // Evaluate the condition, the body, and a branch back to LoopBB when the
  // condition is true.  On failure, jump to BreakBB.
  emitStmtConditionWithBody(S, LoopBB, BreakBB);

  BreakContinueDestStack.pop_back();

  // Handle break block.  If it was used, we link it up with the cleanup chain,
  // otherwise we just remove it.
  if (BreakBB->pred_empty()) {
    BreakBB->eraseFromParent();
  } else {
    B.emitBlock(BreakBB);
  }
}

void SILGenFunction::visitDoStmt(DoStmt *S) {
  // We don't need to do anything fancy if we don't have a label.
  // Otherwise, assume we might break or continue.
  bool hasLabel = (bool) S->getLabelInfo();

  SILBasicBlock *endBB = nullptr;
  if (hasLabel) {
    // Create a new basic block and jump into it.
    SILBasicBlock *loopBB = createBasicBlock();
    B.emitBlock(loopBB, S);

    // Set the destinations for 'break' and 'continue'.
    endBB = createBasicBlock();
    BreakContinueDestStack.push_back(std::make_tuple(
      S,
      JumpDest(endBB , getCleanupsDepth(), CleanupLocation(S->getBody())),
      JumpDest(loopBB, getCleanupsDepth(), CleanupLocation(S->getBody()))
    ));
  }

  // Emit the body.
  visit(S->getBody());

  if (hasLabel) {
    BreakContinueDestStack.pop_back();
  }
  
  emitOrDeleteBlock(B, endBB, S);
}

void SILGenFunction::visitDoWhileStmt(DoWhileStmt *S) {
  // Create a new basic block and jump into it.
  SILBasicBlock *LoopBB = createBasicBlock();
  B.emitBlock(LoopBB, S);
  
  // Set the destinations for 'break' and 'continue'
  SILBasicBlock *EndBB = createBasicBlock();
  SILBasicBlock *CondBB = createBasicBlock();
  BreakContinueDestStack.push_back(std::make_tuple(
    S,
    JumpDest(EndBB , getCleanupsDepth(), CleanupLocation(S->getBody())),
    JumpDest(CondBB, getCleanupsDepth(), CleanupLocation(S->getBody()))
  ));

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
    emitProfilerIncrement(S->getBody());
    if (B.hasValidInsertionPoint()) {
      B.createBranch(S->getCond(), LoopBB);
    }
    
    Cond.exitTrue(B);
    // Complete the conditional execution.
    Cond.complete(B);
  }
  
  emitOrDeleteBlock(B, EndBB, S);
  BreakContinueDestStack.pop_back();
}

void SILGenFunction::visitForStmt(ForStmt *S) {
  // Enter a new scope.
  Scope ForScope(Cleanups, CleanupLocation(S));
  
  // Emit any local 'var' variables declared in the initializer.
  for (auto D : S->getInitializerVarDecls())
    visit(D);
  
  if (auto *Initializer = S->getInitializer().getPtrOrNull()) {
    emitIgnoredExpr(Initializer);
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
  BreakContinueDestStack.push_back(std::make_tuple(
      S,
      JumpDest(EndBB, getCleanupsDepth(), CleanupLocation(S->getBody())),
      JumpDest(IncBB, getCleanupsDepth(), CleanupLocation(S->getBody()))));

  // Evaluate the condition with the false edge leading directly
  // to the continuation block.
  Condition Cond = S->getCond().isNonNull() ?
    emitCondition(S->getCond().get(), /*hasFalseCode*/ false) :
    Condition(LoopBB, 0, 0, S); // Infinite loop.
  
  // If there's a true edge, emit the body in it.
  if (Cond.hasTrue()) {
    Cond.enterTrue(B);
    emitProfilerIncrement(S->getBody());
    visit(S->getBody());
    
    emitOrDeleteBlock(B, IncBB, S);
    
    if (B.hasValidInsertionPoint() && S->getIncrement().isNonNull()) {
      FullExpr Scope(Cleanups, CleanupLocation(S->getIncrement().get()));
      // Ignore the result of the increment expression.
      emitIgnoredExpr(S->getIncrement().get());
    }
    
    if (B.hasValidInsertionPoint()) {
      // Associate the loop body's closing brace with this branch.
      RegularLocation L(S->getBody());
      L.pointToEnd();
      B.createBranch(L, LoopBB);
    }
    Cond.exitTrue(B);
  }
  
  // Complete the conditional execution.
  Cond.complete(B);
  
  emitOrDeleteBlock(B, EndBB, S);
  BreakContinueDestStack.pop_back();
}

namespace {
  
/// NextForEachValueInitialization - initialization for the 'next' value buffer
/// used during for each loop codegen.

class NextForEachValueInitialization : public SingleBufferInitialization {
  SILValue address;
public:
  NextForEachValueInitialization(SILValue address)
    : address(address) {}
  
  SILValue getAddressOrNull() const override { return address; }
};

} // end anonymous namespace


void SILGenFunction::visitForEachStmt(ForEachStmt *S) {
  // Emit the 'generator' variable that we'll be using for iteration.
  Scope OuterForScope(Cleanups, CleanupLocation(S));
  visitPatternBindingDecl(S->getGenerator());
  
  // If we ever reach an unreachable point, stop emitting statements.
  // This will need revision if we ever add goto.
  if (!B.hasValidInsertionPoint()) return;
  
  // Create a stack allocation to hold values out of the generator.
  // This will be initialized on every entry into the loop header and consumed
  // by the loop body. On loop exit, the terminating value will be in the
  // buffer.
  auto optTy = S->getGeneratorNext()->getType()->getCanonicalType();
  auto valTy = optTy.getAnyOptionalObjectType();
  auto &optTL = getTypeLowering(optTy);
  SILValue nextBuf = emitTemporaryAllocation(S, optTL.getLoweredType());
  
  // Create a new basic block and jump into it.
  SILBasicBlock *LoopBB = createBasicBlock();
  B.emitBlock(LoopBB, S);
  
  // Set the destinations for 'break' and 'continue'.
  SILBasicBlock *EndBB = createBasicBlock();
  BreakContinueDestStack.push_back(std::make_tuple(
      S,
      JumpDest(EndBB, getCleanupsDepth(), CleanupLocation(S->getBody())),
      JumpDest(LoopBB, getCleanupsDepth(), CleanupLocation(S->getBody()))));

  // Advance the generator.  Use a scope to ensure that any temporary stack
  // allocations in the subexpression are immediately released.
  {
    Scope InnerForScope(Cleanups, CleanupLocation(S->getGeneratorNext()));
    InitializationPtr nextInit(new NextForEachValueInitialization(nextBuf));
    emitExprInto(S->getGeneratorNext(), nextInit.get());
    nextInit->finishInitialization(*this);
  }
  
  // Continue if the value is present.
  Condition Cond = emitCondition(
         emitDoesOptionalHaveValue(S, nextBuf), S,
         /*hasFalseCode=*/false, /*invertValue=*/false);

  if (Cond.hasTrue()) {
    Cond.enterTrue(B);
    emitProfilerIncrement(S->getBody());
    
    // Emit the loop body.
    // The declared variable(s) for the current element are destroyed
    // at the end of each loop iteration.
    {
      Scope InnerForScope(Cleanups, CleanupLocation(S->getBody()));
      InitializationPtr initLoopVars
        = emitPatternBindingInitialization(S->getPattern());
      auto managedNext = emitManagedBufferWithCleanup(nextBuf);
      ManagedValue val = emitUncheckedGetOptionalValueFrom(S,
                               managedNext,
                               optTL,
                               SGFContext(initLoopVars.get()));
      if (!val.isInContext())
        RValue(*this, S, valTy, val).forwardInto(*this, initLoopVars.get(), S);
      visit(S->getBody());
    }
    
    // Loop back to the header.
    if (B.hasValidInsertionPoint()) {
      // Associate the loop body's closing brace with this branch.
      RegularLocation L(S->getBody());
      L.pointToEnd();
      B.createBranch(L, LoopBB);
    }
    Cond.exitTrue(B);
  }
  
  // Complete the conditional execution.
  Cond.complete(B);
  
  emitOrDeleteBlock(B, EndBB, S);
  BreakContinueDestStack.pop_back();
  
  // We do not need to destroy the value in the 'nextBuf' slot here, because
  // either the 'for' loop finished naturally and the buffer contains '.None',
  // or we exited by 'break' and the value in the buffer was consumed.
}

void SILGenFunction::visitBreakStmt(BreakStmt *S) {
  assert(S->getTarget() && "Sema didn't fill in break target?");
  emitBreakOutOf(S, S->getTarget());
}

void SILGenFunction::emitBreakOutOf(SILLocation loc, Stmt *target) {
  CurrentSILLoc = loc;
  
  // Find the target JumpDest based on the target that sema filled into the
  // stmt.
  for (auto elt : BreakContinueDestStack) {
    if (target == std::get<0>(elt)) {
      Cleanups.emitBranchAndCleanups(std::get<1>(elt), loc);
      return;
    }
  }
  llvm_unreachable("Break has available target block.");
}

void SILGenFunction::visitContinueStmt(ContinueStmt *S) {
  assert(S->getTarget() && "Sema didn't fill in continue target?");

  CurrentSILLoc = S;
  
  // Find the target JumpDest based on the target that sema filled into the
  // stmt.
  for (auto elt : BreakContinueDestStack) {
    if (S->getTarget() == std::get<0>(elt)) {
      Cleanups.emitBranchAndCleanups(std::get<2>(elt), S);
      return;
    }
  }
  llvm_unreachable("Break has available target block.");
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

void SILGenFunction::visitFailStmt(FailStmt *S) {
  assert(FailDest.isValid() && FailSelfDecl && "too big to fail");
  // Clean up 'self', which may be constant or variable depending on whether
  // the initializer delegates.
  auto &selfLoc = VarLocs[FailSelfDecl];
  if (selfLoc.box.isValid()) {
    // Release the box containing 'self'.
    B.createStrongRelease(S, selfLoc.box);
  } else {
    assert(!selfLoc.value.getType().isAddress() &&
           "Pointer shouldn't be an address");
    // Release the 'self' value.
    B.createStrongRelease(S, selfLoc.value);
  }
  
  // Jump to the failure block.
  Cleanups.emitBranchAndCleanups(FailDest, S);
}

void SILGenModule::visitIfConfigDecl(IfConfigDecl *ICD) {
  // Nothing to do for these kinds of decls - anything active has been added
  // to the enclosing declaration.
}

//===--------------------------------------------------------------------===//
// SILGenModule visitTopLevelCodeDecl implementation
//===--------------------------------------------------------------------===//

void SILGenModule::visitTopLevelCodeDecl(TopLevelCodeDecl *td) {
  // Emit top-level statements and expressions into the toplevel function until
  // we hit an unreachable point.
  assert(TopLevelSGF && "top-level code in a non-main source file!");
// TODO:
  if (!TopLevelSGF->B.hasValidInsertionPoint())
    return;
  
  TopLevelSGF->visit(td->getBody());
}
