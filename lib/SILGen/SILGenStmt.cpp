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
#include "llvm/Support/SaveAndRestore.h"

using namespace swift;
using namespace Lowering;

template<typename...T, typename...U>
static void diagnose(ASTContext &Context, SourceLoc loc, Diag<T...> diag,
                     U &&...args) {
  Context.Diags.diagnose(loc,
                         diag, std::forward<U>(args)...);
}

SILBasicBlock *SILGenFunction::createBasicBlock(SILBasicBlock *afterBB) {
  // Honor an explicit placement if given.
  if (afterBB) {
    return new (F.getModule()) SILBasicBlock(&F, afterBB);

  // If we don't have a requested placement, but we do have a current
  // insertion point, insert there.
  } else if (B.hasValidInsertionPoint()) {
    return new (F.getModule()) SILBasicBlock(&F, B.getInsertionBB());

  // Otherwise, insert at the end of the current section.
  } else {
    return createBasicBlock(CurFunctionSection);
  }
}

SILBasicBlock *SILGenFunction::createBasicBlock(FunctionSection section) {
  switch (section) {
  case FunctionSection::Ordinary: {
    // The end of the ordinary section is just the end of the function
    // unless postmatter blocks exist.
    SILBasicBlock *afterBB =
      (StartOfPostmatter ? StartOfPostmatter->getPrevNode() : nullptr);
    return new (F.getModule()) SILBasicBlock(&F, afterBB);
  }

  case FunctionSection::Postmatter: {
    // The end of the postmatter section is always the end of the function.
    // Register the new block as the start of the postmatter if needed.
    SILBasicBlock *newBB = new (F.getModule()) SILBasicBlock(&F, nullptr);
    if (!StartOfPostmatter) StartOfPostmatter = newBB;
    return newBB;
  }

  }
  llvm_unreachable("bad function section");
}

void SILGenFunction::eraseBasicBlock(SILBasicBlock *block) {
  assert(block->pred_empty() && "erasing block with predecessors");
  assert(block->empty() && "erasing block with content");
  if (block == StartOfPostmatter) {
    StartOfPostmatter = block->getNextNode();
  }
  block->eraseFromParent();
}

//===----------------------------------------------------------------------===//
// SILGenFunction emitStmt implementation
//===----------------------------------------------------------------------===//

namespace {
  class StmtEmitter : public Lowering::ASTVisitor<StmtEmitter> {
    SILGenFunction &SGF;
  public:
    StmtEmitter(SILGenFunction &sgf) : SGF(sgf) {}
#define STMT(ID, BASE) void visit##ID##Stmt(ID##Stmt *S);
#include "swift/AST/StmtNodes.def"

    ASTContext &getASTContext() { return SGF.getASTContext(); }

    SILBasicBlock *createBasicBlock() { return SGF.createBasicBlock(); }

    template <class... Args>
    JumpDest createJumpDest(Stmt *cleanupLoc, Args... args) {
      return JumpDest(SGF.createBasicBlock(args...),
                      SGF.getCleanupsDepth(),
                      CleanupLocation(cleanupLoc));
    }
  };
}

void SILGenFunction::emitStmt(Stmt *S) {
  StmtEmitter(*this).visit(S);
}

/// emitOrDeleteBlock - If there are branches to the specified basic block,
/// emit it per emitBlock.  If there aren't, then just delete the block - it
/// turns out to have not been needed.
static void emitOrDeleteBlock(SILGenFunction &SGF, SILBasicBlock *BB,
                              SILLocation BranchLoc) {
  // If we ever add a single-use optimization here (to just continue
  // the predecessor instead of branching to a separate block), we'll
  // need to update visitDoCatchStmt so that code like:
  //   try { throw x } catch _ { }
  // doesn't leave us emitting the rest of the function in the
  // postmatter section.

  if (BB->pred_empty()) {
    // If the block is unused, we don't need it; just delete it.
    SGF.eraseBasicBlock(BB);
  } else {
    // Otherwise, continue emitting code in BB.
    SGF.B.emitBlock(BB, BranchLoc);
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

  SILBasicBlock *TrueBB = createBasicBlock();

  if (invertValue)
    B.createCondBranch(Loc, V, FalseDestBB, TrueBB);
  else
    B.createCondBranch(Loc, V, TrueBB, FalseDestBB);
  
  return Condition(TrueBB, FalseBB, ContBB, Loc);
}

void StmtEmitter::visitBraceStmt(BraceStmt *S) {
  // Enter a new scope.
  LexicalScope BraceScope(SGF.Cleanups, SGF, CleanupLocation(S));
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
    if (!SGF.B.hasValidInsertionPoint()) {
      if (StmtType != UnknownStmtType) {
        diagnose(getASTContext(), ESD.getStartLoc(),
                 diag::unreachable_code_after_stmt, StmtType);
      } else {
        diagnose(getASTContext(), ESD.getStartLoc(),
                 diag::unreachable_code);
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
      SGF.emitIgnoredExpr(E);
    } else {
      SGF.visit(ESD.get<Decl*>());
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

void StmtEmitter::visitReturnStmt(ReturnStmt *S) {
  SGF.CurrentSILLoc = S;
  SILLocation Loc = S->isImplicit() ?
                      (SILLocation)ImplicitReturnLocation(S) :
                      (SILLocation)ReturnLocation(S);

  SILValue ArgV;
  if (!S->hasResult())
    // Void return.
    SGF.Cleanups.emitBranchAndCleanups(SGF.ReturnDest, Loc);
  else
    SGF.emitReturnExpr(Loc, S->getResult());
}

void StmtEmitter::visitIfStmt(IfStmt *S) {
  Scope condBufferScope(SGF.Cleanups, S);
  
  // Create a continuation block.  We need it if there is a labeled break out
  // of the if statement or if there is an if/then/else.
  JumpDest contDest = createJumpDest(S->getThenStmt());
  auto contBB = contDest.getBlock();

  // Set the destinations for any 'break' and 'continue' statements inside the
  // body.  Note that "continue" is not valid out of a labeled 'if'.
  SGF.BreakContinueDestStack.push_back(
                               { S, contDest, JumpDest(CleanupLocation(S)) });

  // Set up the block for the false case.  If there is an 'else' block, we make
  // a new one, otherwise it is our continue block.
  SILBasicBlock *condFalseBB;
  if (!S->getElseStmt())
    condFalseBB = contBB;
  else
    condFalseBB = createBasicBlock();

  // Emit the condition, along with the "then" part of the if properly guarded
  // by the condition and a jump to ContBB.  If the condition fails, jump to
  // the CondFalseBB.
  SGF.emitStmtConditionWithBody(S, contBB, condFalseBB);

  // If there is 'else' logic, then emit it.
  if (S->getElseStmt()) {
    SGF.B.emitBlock(condFalseBB);
    visit(S->getElseStmt());
    if (SGF.B.hasValidInsertionPoint()) {
      RegularLocation L(S->getElseStmt());
      L.pointToEnd();
      SGF.B.createBranch(L, contBB);
    }
  }

  // If the continuation block was used, emit it now, otherwise remove it.
  if (contBB->pred_empty()) {
    SGF.eraseBasicBlock(contBB);
  } else {
    RegularLocation L(S->getThenStmt());
    L.pointToEnd();
    SGF.B.emitBlock(contBB, L);
  }
  SGF.BreakContinueDestStack.pop_back();
}

void StmtEmitter::visitIfConfigStmt(IfConfigStmt *S) {
  // Active members are attached to the enclosing declaration, so there's no
  // need to walk anything within.
}

void StmtEmitter::visitWhileStmt(WhileStmt *S) {
  Scope condBufferScope(SGF.Cleanups, S);
  
  // Create a new basic block and jump into it.
  JumpDest loopDest = createJumpDest(S->getBody());
  SILBasicBlock *loopBB = loopDest.getBlock();
  SGF.B.emitBlock(loopBB, S);
  
  // Create a break target (at this level in the cleanup stack) in case it is
  // needed.
  JumpDest breakDest = createJumpDest(S->getBody());
  SILBasicBlock *breakBB = breakDest.getBlock();

  // Set the destinations for any 'break' and 'continue' statements inside the
  // body.
  SGF.BreakContinueDestStack.push_back({S, breakDest, loopDest});
  
  // Evaluate the condition, the body, and a branch back to LoopBB when the
  // condition is true.  On failure, jump to BreakBB.
  SGF.emitStmtConditionWithBody(S, loopBB, breakBB);

  SGF.BreakContinueDestStack.pop_back();

  // Handle break block.  If it was used, we link it up with the cleanup chain,
  // otherwise we just remove it.
  if (breakBB->pred_empty()) {
    SGF.eraseBasicBlock(breakBB);
  } else {
    SGF.B.emitBlock(breakBB);
  }
}

void StmtEmitter::visitDoStmt(DoStmt *S) {
  // We don't need to do anything fancy if we don't have a label.
  // Otherwise, assume we might break or continue.
  bool hasLabel = (bool) S->getLabelInfo();

  JumpDest endDest = JumpDest::invalid();
  if (hasLabel) {
    // Create the end dest first so that the loop dest comes in-between.
    endDest = createJumpDest(S->getBody());

    // Create a new basic block and jump into it.
    JumpDest loopDest = createJumpDest(S->getBody());
    SGF.B.emitBlock(loopDest.getBlock(), S);

    // Set the destinations for 'break' and 'continue'.
    SGF.BreakContinueDestStack.push_back({S, endDest, loopDest});
  }

  // Emit the body.
  visit(S->getBody());

  if (hasLabel) {
    SGF.BreakContinueDestStack.pop_back();
    emitOrDeleteBlock(SGF, endDest.getBlock(), S);
  }
}

void StmtEmitter::visitDoCatchStmt(DoCatchStmt *S) {
  Type formalExnType =
    S->getCatches().front()->getErrorPattern()->getType();
  auto &exnTL = SGF.getTypeLowering(formalExnType);

  // Create the throw destination at the end of the function.
  JumpDest throwDest = createJumpDest(S->getBody(),
                                      FunctionSection::Postmatter);
  SILArgument *exnArg =
    throwDest.getBlock()->createBBArg(exnTL.getLoweredType());

  // We always need an continuation block because we might fall out of
  // a catch block.  But we don't need a loop block unless the 'do'
  // statement is labeled.
  JumpDest endDest = createJumpDest(S->getBody());

  // We don't need to do anything too fancy about emission if we don't
  // have a label.  Otherwise, assume we might break or continue.
  bool hasLabel = (bool) S->getLabelInfo();
  if (hasLabel) {
    // Create a new basic block and jump into it.
    JumpDest loopDest = createJumpDest(S->getBody());
    SGF.B.emitBlock(loopDest.getBlock(), S);

    // Set the destinations for 'break' and 'continue'.
    SGF.BreakContinueDestStack.push_back({S, endDest, loopDest});
  }

  // Emit the body.
  {
    // Push the new throw destination.
    llvm::SaveAndRestore<JumpDest> savedThrowDest(SGF.ThrowDest, throwDest);

    visit(S->getBody());
  }

  // Emit the catch clauses.
  {
    // Move the insertion point to the throw destination.
    SavedInsertionPoint savedIP(SGF, throwDest.getBlock(),
                                FunctionSection::Postmatter);

    // The exception cleanup should be getting forwarded around
    // correctly anyway, but push a scope to ensure it gets popped.
    Scope exnScope(SGF.Cleanups, CleanupLocation(S));

    // Take ownership of the exception.
    ManagedValue exn = SGF.emitManagedRValueWithCleanup(exnArg, exnTL);

    // Emit all the catch clauses, branching to the end destination if
    // we fall out of one.
    SGF.emitCatchDispatch(S, exn, S->getCatches(), endDest);
  }

  if (hasLabel) {
    SGF.BreakContinueDestStack.pop_back();
  }

  // Handle falling out of the do-block.
  //
  // It's important for good code layout that the insertion point be
  // left in the original function section after this.  So if
  // emitOrDeleteBlock ever learns to just continue in the
  // predecessor, we'll need to suppress that here.
  emitOrDeleteBlock(SGF, endDest.getBlock(), S);
}

void StmtEmitter::visitCatchStmt(CatchStmt *S) {
  llvm_unreachable("catch statement outside of context?");
}

void StmtEmitter::visitDoWhileStmt(DoWhileStmt *S) {
  // Create a new basic block and jump into it.
  SILBasicBlock *loopBB = createBasicBlock();
  SGF.B.emitBlock(loopBB, S);
  
  // Set the destinations for 'break' and 'continue'
  JumpDest endDest = createJumpDest(S->getBody());
  JumpDest condDest = createJumpDest(S->getBody());
  SGF.BreakContinueDestStack.push_back({ S, endDest, condDest });

  // Emit the body, which is always evaluated the first time around.
  visit(S->getBody());

  // Let's not differ from C99 6.8.5.2: "The evaluation of the controlling
  // expression takes place after each execution of the loop body."
  emitOrDeleteBlock(SGF, condDest.getBlock(), S);

  if (SGF.B.hasValidInsertionPoint()) {
    // Evaluate the condition with the false edge leading directly
    // to the continuation block.
    Condition Cond = SGF.emitCondition(S->getCond(), /*hasFalseCode*/ false);
    
    Cond.enterTrue(SGF);
    SGF.emitProfilerIncrement(S->getBody());
    if (SGF.B.hasValidInsertionPoint()) {
      SGF.B.createBranch(S->getCond(), loopBB);
    }
    
    Cond.exitTrue(SGF);
    // Complete the conditional execution.
    Cond.complete(SGF);
  }
  
  emitOrDeleteBlock(SGF, endDest.getBlock(), S);
  SGF.BreakContinueDestStack.pop_back();
}

void StmtEmitter::visitForStmt(ForStmt *S) {
  // Enter a new scope.
  Scope ForScope(SGF.Cleanups, CleanupLocation(S));
  
  // Emit any local 'var' variables declared in the initializer.
  for (auto D : S->getInitializerVarDecls()) {
    SGF.visit(D);
  }
  
  if (auto *Initializer = S->getInitializer().getPtrOrNull()) {
    SGF.emitIgnoredExpr(Initializer);
  }
  
  // If we ever reach an unreachable point, stop emitting statements.
  // This will need revision if we ever add goto.
  if (!SGF.B.hasValidInsertionPoint()) return;
  
  // Create a new basic block and jump into it.
  SILBasicBlock *loopBB = createBasicBlock();
  SGF.B.emitBlock(loopBB, S);

  JumpDest endDest = createJumpDest(S->getBody());
  
  // Evaluate the condition with the false edge leading directly
  // to the continuation block.
  Condition Cond = S->getCond().isNonNull() ?
    SGF.emitCondition(S->getCond().get(), /*hasFalseCode*/ false) :
    Condition(loopBB, 0, 0, S); // Infinite loop.
  
  // If there's a true edge, emit the body in it.
  if (Cond.hasTrue()) {
    Cond.enterTrue(SGF);
    SGF.emitProfilerIncrement(S->getBody());

    // Set the destinations for 'break' and 'continue'.
    JumpDest incDest = createJumpDest(S->getBody());
    SGF.BreakContinueDestStack.push_back({S, endDest, incDest});

    visit(S->getBody());

    SGF.BreakContinueDestStack.pop_back();
    
    emitOrDeleteBlock(SGF, incDest.getBlock(), S);
    
    if (SGF.B.hasValidInsertionPoint() && S->getIncrement().isNonNull()) {
      FullExpr Scope(SGF.Cleanups, CleanupLocation(S->getIncrement().get()));
      // Ignore the result of the increment expression.
      SGF.emitIgnoredExpr(S->getIncrement().get());
    }
    
    if (SGF.B.hasValidInsertionPoint()) {
      // Associate the loop body's closing brace with this branch.
      RegularLocation L(S->getBody());
      L.pointToEnd();
      SGF.B.createBranch(L, loopBB);
    }
    Cond.exitTrue(SGF);
  }
  
  // Complete the conditional execution.
  Cond.complete(SGF);
  
  emitOrDeleteBlock(SGF, endDest.getBlock(), S);
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


void StmtEmitter::visitForEachStmt(ForEachStmt *S) {
  // Emit the 'generator' variable that we'll be using for iteration.
  Scope OuterForScope(SGF.Cleanups, CleanupLocation(S));
  SGF.visitPatternBindingDecl(S->getGenerator());
  
  // If we ever reach an unreachable point, stop emitting statements.
  // This will need revision if we ever add goto.
  if (!SGF.B.hasValidInsertionPoint()) return;
  
  // Create a stack allocation to hold values out of the generator.
  // This will be initialized on every entry into the loop header and consumed
  // by the loop body. On loop exit, the terminating value will be in the
  // buffer.
  auto optTy = S->getGeneratorNext()->getType()->getCanonicalType();
  auto valTy = optTy.getAnyOptionalObjectType();
  auto &optTL = SGF.getTypeLowering(optTy);
  SILValue nextBuf = SGF.emitTemporaryAllocation(S, optTL.getLoweredType());
  
  // Create a new basic block and jump into it.
  JumpDest loopDest = createJumpDest(S->getBody());
  SGF.B.emitBlock(loopDest.getBlock(), S);
  
  // Set the destinations for 'break' and 'continue'.
  JumpDest endDest = createJumpDest(S->getBody());
  SGF.BreakContinueDestStack.push_back({ S, endDest, loopDest });

  // Advance the generator.  Use a scope to ensure that any temporary stack
  // allocations in the subexpression are immediately released.
  {
    Scope InnerForScope(SGF.Cleanups, CleanupLocation(S->getGeneratorNext()));
    InitializationPtr nextInit(new NextForEachValueInitialization(nextBuf));
    SGF.emitExprInto(S->getGeneratorNext(), nextInit.get());
    nextInit->finishInitialization(SGF);
  }
  
  // Continue if the value is present.
  Condition Cond = SGF.emitCondition(
         SGF.emitDoesOptionalHaveValue(S, nextBuf), S,
         /*hasFalseCode=*/false, /*invertValue=*/false);

  if (Cond.hasTrue()) {
    Cond.enterTrue(SGF);
    SGF.emitProfilerIncrement(S->getBody());
    
    // Emit the loop body.
    // The declared variable(s) for the current element are destroyed
    // at the end of each loop iteration.
    {
      Scope InnerForScope(SGF.Cleanups, CleanupLocation(S->getBody()));
      InitializationPtr initLoopVars
        = SGF.emitPatternBindingInitialization(S->getPattern());
      auto managedNext = SGF.emitManagedBufferWithCleanup(nextBuf);
      ManagedValue val = SGF.emitUncheckedGetOptionalValueFrom(S,
                               managedNext,
                               optTL,
                               SGFContext(initLoopVars.get()));
      if (!val.isInContext())
        RValue(SGF, S, valTy, val).forwardInto(SGF, initLoopVars.get(), S);
      visit(S->getBody());
    }
    
    // Loop back to the header.
    if (SGF.B.hasValidInsertionPoint()) {
      // Associate the loop body's closing brace with this branch.
      RegularLocation L(S->getBody());
      L.pointToEnd();
      SGF.B.createBranch(L, loopDest.getBlock());
    }
    Cond.exitTrue(SGF);
  }
  
  // Complete the conditional execution.
  Cond.complete(SGF);
  
  emitOrDeleteBlock(SGF, endDest.getBlock(), S);
  SGF.BreakContinueDestStack.pop_back();
  
  // We do not need to destroy the value in the 'nextBuf' slot here, because
  // either the 'for' loop finished naturally and the buffer contains '.None',
  // or we exited by 'break' and the value in the buffer was consumed.
}

void StmtEmitter::visitBreakStmt(BreakStmt *S) {
  assert(S->getTarget() && "Sema didn't fill in break target?");
  SGF.emitBreakOutOf(S, S->getTarget());
}

void SILGenFunction::emitBreakOutOf(SILLocation loc, Stmt *target) {
  CurrentSILLoc = loc;
  
  // Find the target JumpDest based on the target that sema filled into the
  // stmt.
  for (auto &elt : BreakContinueDestStack) {
    if (target == elt.Target) {
      Cleanups.emitBranchAndCleanups(elt.BreakDest, loc);
      return;
    }
  }
  llvm_unreachable("Break has available target block.");
}

void StmtEmitter::visitContinueStmt(ContinueStmt *S) {
  assert(S->getTarget() && "Sema didn't fill in continue target?");

  SGF.CurrentSILLoc = S;
  
  // Find the target JumpDest based on the target that sema filled into the
  // stmt.
  for (auto &elt : SGF.BreakContinueDestStack) {
    if (S->getTarget() == elt.Target) {
      SGF.Cleanups.emitBranchAndCleanups(elt.ContinueDest, S);
      return;
    }
  }
  llvm_unreachable("Break has available target block.");
}

void StmtEmitter::visitSwitchStmt(SwitchStmt *S) {
  // Implemented in SILGenPattern.cpp.
  SGF.emitSwitchStmt(S);
}

void StmtEmitter::visitCaseStmt(CaseStmt *S) {
  llvm_unreachable("cases should be lowered as part of switch stmt");
}

void StmtEmitter::visitFallthroughStmt(FallthroughStmt *S) {
  // Implemented in SILGenPattern.cpp.
  SGF.emitSwitchFallthrough(S);
}

void StmtEmitter::visitFailStmt(FailStmt *S) {
  assert(SGF.FailDest.isValid() && SGF.FailSelfDecl && "too big to fail");
  // Clean up 'self', which may be constant or variable depending on whether
  // the initializer delegates.
  auto &selfLoc = SGF.VarLocs[SGF.FailSelfDecl];
  if (selfLoc.box.isValid()) {
    // Release the box containing 'self'.
    SGF.B.createStrongRelease(S, selfLoc.box);
  } else {
    assert(!selfLoc.value.getType().isAddress() &&
           "Pointer shouldn't be an address");
    // Release the 'self' value.
    SGF.B.createStrongRelease(S, selfLoc.value);
  }
  
  // Jump to the failure block.
  SGF.Cleanups.emitBranchAndCleanups(SGF.FailDest, S);
}

void SILGenFunction::emitThrow(SILLocation loc, ManagedValue exnMV) {
  assert(ThrowDest.isValid() &&
         "calling emitThrow with invalid throw destination!");

  // Claim the exception value.  If we need to handle throwing
  // cleanups, the correct thing to do here is to recreate the
  // exception's cleanup when emitting each cleanup we branch through.
  // But for now we aren't bothering.
  SILValue exn = exnMV.forward(*this);

  // Branch to the cleanup destination.
  Cleanups.emitBranchAndCleanups(ThrowDest, loc, exn);
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
  
  TopLevelSGF->emitStmt(td->getBody());
}
