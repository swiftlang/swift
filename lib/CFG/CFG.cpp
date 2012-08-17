//===--- CFG.cpp - Defines the CFG data structure ----------------*- C++ -*-==//
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

#include "swift/AST/AST.h"
#include "swift/AST/ASTVisitor.h"
#include "swift/CFG/CFG.h"
#include "llvm/ADT/OwningPtr.h"

using namespace swift;

CFG::CFG() {}

CFG::~CFG() {
  // FIXME: if all parts of BasicBlock are BumpPtrAllocated, this shouldn't
  // eventually be needed.
  for(BasicBlock &B : blocks) { B.~BasicBlock(); }
}

//===----------------------------------------------------------------------===//
// CFG construction.
//===----------------------------------------------------------------------===//

static Expr *ignoreParens(Expr *Ex) {
  while (ParenExpr *P = dyn_cast<ParenExpr>(Ex)) { Ex = P->getSubExpr(); }
  return Ex;
}

namespace {
class CFGBuilder : public ASTVisitor<CFGBuilder, CFGValue> {
  typedef std::vector<BasicBlock *> BlocksVector;
  BlocksVector PendingMerges;

  /// Mapping from expressions to instructions.
  llvm::DenseMap<Expr *, Instruction *> ExprToInst;

  /// The current basic block being constructed.
  BasicBlock *Block;

  /// The CFG being constructed.
  CFG &C;

public:
  CFGBuilder(CFG &C) : Block(0), C(C), badCFG(false) {}
  ~CFGBuilder() {}

  /// A flag indicating whether or not there were problems
  /// constructing the CFG.
  bool badCFG;

  /// The current basic block being constructed.
  BasicBlock *currentBlock() {
    if (!Block) {
      Block = new (C) BasicBlock(&C);

      // Flush out all pending merges.  These are basic blocks waiting
      // for a successor.
      for (auto PredBlock : PendingMerges) {
        UncondBranchInst *UB = new (C) UncondBranchInst(PredBlock);
        UB->setTarget(Block, ArrayRef<CFGValue>());
      }
      PendingMerges.clear();
    }
    return Block;
  }

  void clearCurrentBlock() {
    Block = 0;
  }
  
  /// Reset the currently active basic block by creating a new one.
  BasicBlock *createFreshBlock() {
    if (Block && !Block->hasTerminator()) {
      PendingMerges.push_back(Block);
    }
    Block = new (C) BasicBlock(&C);
    return Block;
  }

  CFGValue addInst(Expr *Ex, Instruction *I) {
    ExprToInst[Ex] = I;
    return I;
  }

  Instruction *getInst(Expr *Ex) {
    auto I = ExprToInst.find(Ex);
    assert(I != ExprToInst.end());
    return I->second;
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
    visit(S->getResult());
    //assert(false && "Not yet implemented");
  }

  void visitIfStmt(IfStmt *S);

  void visitWhileStmt(WhileStmt *S) {
    assert(false && "Not yet implemented");
  }

  void visitDoWhileStmt(DoWhileStmt *S) {
    assert(false && "Not yet implemented");
  }

  void visitForStmt(ForStmt *S) {
    assert(false && "Not yet implemented");
  }

  void visitForEachStmt(ForEachStmt *S) {
    badCFG = true;
    return;
  }

  void visitBreakStmt(BreakStmt *S) {
    assert(false && "Not yet implemented");
  }

  void visitContinueStmt(ContinueStmt *S) {
    assert(false && "Not yet implemented");
  }

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
  // FIXME: implement CFG construction.
  llvm::OwningPtr<CFG> C(new CFG());
  CFGBuilder builder(*C);
  builder.visit(S);
  return builder.badCFG ? nullptr : C.take();
}

void CFGBuilder::visitBraceStmt(BraceStmt *S) {
  // BraceStmts do not need to be explicitly represented in the CFG.
  // We should consider whether or not the scopes they introduce are
  // represented in the CFG.
  for (const BraceStmt::ExprStmtOrDecl &ESD : S->elements()) {
    assert(!ESD.is<Decl*>() && "FIXME: Handle Decls");
    if (Stmt *S = ESD.dyn_cast<Stmt*>())
      visit(S);
    if (Expr *E = ESD.dyn_cast<Expr*>())
      visit(E);
  }
}

//===--------------------------------------------------------------------===//
// Control-flow.
//===--------------------------------------------------------------------===//

void CFGBuilder::visitIfStmt(IfStmt *S) {
  // ** FIXME ** Handle the condition.  We need to handle more of the
  // statements first.

  // The condition should be the last value evaluated just before the
  // terminator.
  //  CFGValue CondV = visit(S->getCond());

  // Save the current block.  We will use it to construct the
  // CondBranchInst.
  BasicBlock *IfTermBlock = currentBlock();

  // Reset the state for the current block.
  clearCurrentBlock();

  // Create a new basic block for the first target.
  BasicBlock *Target1 = createFreshBlock();
  visit(S->getThenStmt());

  // ** FIXME ** Handle case where there is no 'else'.  Requires
  // fixing up the second target later.

  // Create a new basic block for the second target.  The first target's
  // blocks will get added the "pending" list.
  BasicBlock *Target2 = createFreshBlock();
  visit(S->getElseStmt());

  // Finally, hook up the block with the condition to the target blocks.
  CFGValue Branch = new (C) CondBranchInst(S, (Instruction*) 0, Target1,
                                           Target2, IfTermBlock);
  (void) Branch;
  Target1->addPred(IfTermBlock);
  Target2->addPred(IfTermBlock);
}

//===--------------------------------------------------------------------===//
// Expressions.
//===--------------------------------------------------------------------===//

CFGValue CFGBuilder::visitCallExpr(CallExpr *E) {
  llvm::SmallVector<CFGValue, 10> Args;
  Expr *Arg = ignoreParens(E->getArg());
  Expr *Fn = E->getFn();
  CFGValue FnV = visit(Fn);
  CFGValue ArgV = visit(Arg);

  // FIXME: should we even bother constructing the TupleExpr?  Should
  // arguments be marshaled with a singled TupleExpr, or as separate arguments?
  TupleInst *TU = dyn_cast_or_null<TupleInst>(ArgV.dyn_cast<Instruction*>());
  if (TU) {
    return addInst(E, CallInst::create(E, currentBlock(), FnV, TU->elements()));
  }

  return addInst(E, CallInst::create(E, currentBlock(), getInst(Fn),
                                     ArrayRef<CFGValue>(&ArgV, 1)));
}

CFGValue CFGBuilder::visitDeclRefExpr(DeclRefExpr *E) {
  return addInst(E, new (C) DeclRefInst(E, currentBlock()));
}

CFGValue CFGBuilder::visitThisApplyExpr(ThisApplyExpr *E) {
  CFGValue FnV = visit(E->getFn());
  CFGValue ArgV = visit(E->getArg());
  return addInst(E, new (C) ThisApplyInst(E, FnV, ArgV, currentBlock()));
}

CFGValue CFGBuilder::visitIntegerLiteralExpr(IntegerLiteralExpr *E) {
  return addInst(E, new (C) IntegerLiteralInst(E, currentBlock()));
}

CFGValue CFGBuilder::visitLoadExpr(LoadExpr *E) {
  CFGValue SubV = visit(E->getSubExpr());
  return addInst(E, new (C) LoadInst(E, SubV, currentBlock()));
}

CFGValue CFGBuilder::visitParenExpr(ParenExpr *E) {
  return visit(E->getSubExpr());
}

CFGValue CFGBuilder::visitTupleExpr(TupleExpr *E) {
  llvm::SmallVector<CFGValue, 10> ArgsV;
  for (auto &I : E->getElements()) {
    ArgsV.push_back(visit(I));
  }
  return addInst(E, TupleInst::create(E, ArgsV, currentBlock()));
}

CFGValue CFGBuilder::visitTypeOfExpr(TypeOfExpr *E) {
  return addInst(E, new (C) TypeOfInst(E, currentBlock()));
}
