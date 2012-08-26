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
  typedef llvm::SmallVector<BasicBlock *, 4> BlocksVector;

  BlocksVector BasePendingMerges;
  llvm::SmallVector<BlocksVector *, 4> PendingMergesStack;
  llvm::SmallVector<BlocksVector *, 4> BreakStack;
  llvm::SmallVector<BlocksVector *, 4> ContinueStack;

  /// The current basic block being constructed.
  BasicBlock *Block;

  /// The CFG being constructed.
  CFG &C;

public:
  Builder(CFG &C) : Block(0), C(C) {
    PendingMergesStack.push_back(&BasePendingMerges);
  }

  ~Builder() {}

  // FIXME: Move to ivar once currentBlock goes away.
  CFGBuilder getBuilder() {
    return CFGBuilder(currentBlock(), C);
  }

  BlocksVector &pendingMerges() {
    assert(!PendingMergesStack.empty());
    return *PendingMergesStack.back();
  }

  void flushPending(BasicBlock *TargetBlock) {
    for (auto PredBlock : pendingMerges()) {
      // If the block has no terminator, we need to add an unconditional
      // jump to the block we are creating.
      if (!hasTerminator(PredBlock)) {
        UncondBranchInst *UB = new (C) UncondBranchInst(42, PredBlock);
        UB->setTarget(TargetBlock, ArrayRef<CFGValue>(), C);
        continue;
      }
      TermInst &Term = *PredBlock->getTerminator();
      if (UncondBranchInst *UBI = dyn_cast<UncondBranchInst>(&Term)) {
        assert(UBI->getDestBB() == nullptr);
        UBI->setTarget(TargetBlock, ArrayRef<CFGValue>(), C);
        continue;
      }

      // If the block already has a CondBranch terminator, then it means
      // we are fixing up one of the branch targets because it wasn't
      // available when the instruction was created.
      CondBranchInst &CBI = cast<CondBranchInst>(Term);
      assert(CBI.getTrueBB());
      assert(!CBI.getFalseBB());
      CBI.setFalseBB(TargetBlock);
    }
    pendingMerges().clear();
  }

  /// The current basic block being constructed.
  BasicBlock *currentBlock() {
    if (!Block) {
      Block = new (C) BasicBlock(&C);

      // Flush out all pending merges.  These are basic blocks waiting
      // for a successor.
      flushPending(Block);
    }
    return Block;
  }

  void addCurrentBlockToPending() {
    if (Block && !hasTerminator(Block))
      pendingMerges().push_back(Block);
    Block = 0;
  }
  
  /// Reset the currently active basic block by creating a new one.
  BasicBlock *createFreshBlock() {
    Block = new (C) BasicBlock(&C);
    return Block;
  }

  void finishUp() {
    assert(PendingMergesStack.size() == 1);
    if (!pendingMerges().empty()) {
      assert(Block == 0);
      new (C) ReturnInst(currentBlock());
      return;
    }
    // Check if the last block has a Return.
    if (!Block)
      return;

    if (Block->empty() || !isa<ReturnInst>(Block->getInsts().back()))
      new (C) ReturnInst(Block);
  }

  void popBreakStack(BlocksVector &BlocksThatBreak) {
    assert(BreakStack.back() == &BlocksThatBreak);
    for (auto BreakBlock : BlocksThatBreak) {
      pendingMerges().push_back(BreakBlock);
    }
    BreakStack.pop_back();
  }

  void popContinueStack(BlocksVector &BlocksThatContinue,
                        BasicBlock *TargetBlock) {
    assert(ContinueStack.back() == &BlocksThatContinue);
    for (auto ContinueBlock : BlocksThatContinue) {
      auto UB = cast<UncondBranchInst>(ContinueBlock->getTerminator());
      UB->setTarget(TargetBlock, ArrayRef<CFGValue>(), C);
    }
    ContinueStack.pop_back();
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
    CFGValue ArgV = S->hasResult() ? visit(S->getResult()) : (Instruction*) 0;
    (void) new (C) ReturnInst(S, ArgV, currentBlock());
    // Treat the current block as "complete" with no successors.
    Block = 0;
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
  assert(!BreakStack.empty());
  BasicBlock *BreakBlock = currentBlock();
  BreakStack.back()->push_back(BreakBlock);

  // FIXME: we need to be able to include the BreakStmt in the jump.
  new (C) UncondBranchInst(42, BreakBlock);
  Block = 0;
}

void Builder::visitContinueStmt(ContinueStmt *S) {
  assert(!ContinueStack.empty());
  BasicBlock *ContinueBlock = currentBlock();
  ContinueStack.back()->push_back(ContinueBlock);

  // FIXME: we need to be able to include the ContinueStmt in the jump.
  new (C) UncondBranchInst(42, ContinueBlock);
  Block = 0;
}

void Builder::visitDoWhileStmt(DoWhileStmt *S) {
  // Set up a vector to record blocks that 'break'.
  BlocksVector BlocksThatBreak;
  BreakStack.push_back(&BlocksThatBreak);

  // Set up a vector to record blocks that 'continue'.
  BlocksVector BlocksThatContinue;
  ContinueStack.push_back(&BlocksThatContinue);

  // Create a new basic block for the body.
  addCurrentBlockToPending();
  BasicBlock *BodyBlock = currentBlock();

  // Push a new context to record pending blocks.  These will
  // get linked up the condition block.
  BlocksVector PendingWithinLoop;
  PendingMergesStack.push_back(&PendingWithinLoop);

  // Now visit the loop body.
  visit(S->getBody());
  addCurrentBlockToPending();

  // Create the condition block.
  BasicBlock *ConditionBlock = currentBlock();
  CFGValue CondV = (Instruction*) 0;
  //  visit(S->getCond());

  assert(ConditionBlock == Block);
  Block = 0;

  // Pop the pending merges.
  assert(PendingMergesStack.back() == &PendingWithinLoop);
  flushPending(ConditionBlock);
  PendingMergesStack.pop_back();

  // Pop the 'break' context.
  popBreakStack(BlocksThatBreak);

  // Pop the 'continue' context.
  popContinueStack(BlocksThatContinue, ConditionBlock);

  // Finally, hook up the block with the condition to the target blocks.
  CFGValue Branch = new (C) CondBranchInst(S, CondV,
                                           BodyBlock,
                                           0, /* will be fixed up later */
                                           ConditionBlock);
  (void) Branch;

  pendingMerges().push_back(ConditionBlock);
}

void Builder::visitIfStmt(IfStmt *S) {
  // ** FIXME ** Handle the condition.  We need to handle more of the
  // statements first.

  // The condition should be the last value evaluated just before the
  // terminator.
  //  CFGValue CondV = visit(S->getCond());
  CFGValue CondV = (Instruction*) 0;

  // Save the current block.  We will use it to construct the
  // CondBranchInst.
  BasicBlock *IfTermBlock = currentBlock();

  // Reset the state for the current block.
  Block = 0;

  // Create a new basic block for the first target.
  BasicBlock *Target1 = createFreshBlock();
  visit(S->getThenStmt());
  addCurrentBlockToPending();

  // Handle an (optional) 'else'.  If no 'else' is found, the false branch
  // will be fixed up later.
  BasicBlock *Target2 = nullptr;
  if (Stmt *Else = S->getElseStmt()) {
    // Create a new basic block for the second target.  The first target's
    // blocks will get added the "pending" list.
    Target2 = createFreshBlock();
    visit(Else);
    addCurrentBlockToPending();
  }
  else {
    // If we have no 'else', we need to fix up the branch later.
    pendingMerges().push_back(IfTermBlock);
  }

  // Finally, hook up the block with the condition to the target blocks.
  CFGValue Branch = new (C) CondBranchInst(S, CondV,
                                           Target1,
                                           Target2 /* may be null*/,
                                           IfTermBlock);
  (void) Branch;
}

void Builder::visitWhileStmt(WhileStmt *S) {
  // The condition needs to be in its own basic block so that
  // it can be the loop-back target.  We thus finish up the currently
  // active block.  It will get linked to the new block once we
  // create it.
  addCurrentBlockToPending();

  // Process the condition.  This will link up the previous block
  // with the condition block.
  BasicBlock *ConditionBlock = currentBlock();
  CFGValue CondV = (Instruction*) 0;
  //  visit(S->getCond());

  assert(ConditionBlock == Block);
  Block = 0;

  // Set up a vector to record blocks that 'break'.
  BlocksVector BlocksThatBreak;
  BreakStack.push_back(&BlocksThatBreak);

  // Set up a vector to record blocks that 'continue'.
  BlocksVector BlocksThatContinue;
  ContinueStack.push_back(&BlocksThatContinue);

  // Push a new context to record pending blocks.  These will
  // get linked up the condition block.
  BlocksVector PendingWithinLoop;
  PendingMergesStack.push_back(&PendingWithinLoop);

  // Create a new basic block for the body.
  BasicBlock *BodyBlock = createFreshBlock();
  visit(S->getBody());
  addCurrentBlockToPending();

  // Pop the pending merges.
  assert(PendingMergesStack.back() == &PendingWithinLoop);
  flushPending(ConditionBlock);
  PendingMergesStack.pop_back();

  // Pop the 'break' context.
  popBreakStack(BlocksThatBreak);

  // Pop the 'continue' context.
  popContinueStack(BlocksThatContinue, ConditionBlock);

  // Finally, hook up the block with the condition to the target blocks.
  CFGValue Branch = new (C) CondBranchInst(S, CondV,
                                           BodyBlock,
                                           0, /* will be fixed up later */
                                           ConditionBlock);
  (void) Branch;

  pendingMerges().push_back(ConditionBlock);
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

  return getBuilder().createCall(E, FnV, ArgsV);
}

CFGValue Builder::visitDeclRefExpr(DeclRefExpr *E) {
  return getBuilder().createDeclRef(E);
}

CFGValue Builder::visitIntegerLiteralExpr(IntegerLiteralExpr *E) {
  return getBuilder().createIntegerLiteral(E);
}

CFGValue Builder::visitLoadExpr(LoadExpr *E) {
  CFGValue SubV = visit(E->getSubExpr());
  return getBuilder().createLoad(E, SubV);
}

CFGValue Builder::visitThisApplyExpr(ThisApplyExpr *E) {
  CFGValue FnV = visit(E->getFn());
  CFGValue ArgV = visit(E->getArg());
  return getBuilder().createThisApply(E, FnV, ArgV);
}

CFGValue Builder::visitParenExpr(ParenExpr *E) {
  return visit(E->getSubExpr());
}

CFGValue Builder::visitTupleExpr(TupleExpr *E) {
  llvm::SmallVector<CFGValue, 10> ArgsV;
  for (auto &I : E->getElements())
    ArgsV.push_back(visit(I));
  return getBuilder().createTuple(E, ArgsV);
}

CFGValue Builder::visitTypeOfExpr(TypeOfExpr *E) {
  return getBuilder().createTypeOf(E);
}
