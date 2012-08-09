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
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/OwningPtr.h"
#include "llvm/Support/raw_ostream.h"

using namespace swift;

CFG::CFG() {}

CFG::~CFG() {
  // FIXME: if all parts of BasicBlock are BumpPtrAllocated, this shouldn't
  // eventually be needed.
  for(BasicBlock &B : blocks) { B.~BasicBlock(); }
}

//===----------------------------------------------------------------------===//
// CFG pretty-printing.
//===----------------------------------------------------------------------===//

namespace {
class DumpVisitor : public ASTVisitor<DumpVisitor> {
public:
  DumpVisitor(llvm::raw_ostream &OS) : OS(OS) {}

  raw_ostream &OS;

  void visitFuncDecl(FuncDecl *FD) {
    FuncExpr *FE = FD->getBody();
    llvm::OwningPtr<CFG> C(CFG::constructCFG(FE->getBody()));

    if (!C)
      return;

    OS << "(func_decl " << FD->getName() << '\n';
    C->print(OS);
    OS << ")\n";
  }
};
}

void CFG::dump(TranslationUnit *TU) {
  for (Decl *D : TU->Decls) { DumpVisitor(llvm::errs()).visit(D); }
}

/// Pretty-print the basic block.
void CFG::dump() const { print(llvm::errs()); }

/// Pretty-print the basi block with the designated stream.
void CFG::print(llvm::raw_ostream &OS) const {
  for (const BasicBlock &B : blocks) {
    B.print(OS);
  }
}

//===----------------------------------------------------------------------===//
// CFG construction.
//===----------------------------------------------------------------------===//

static Expr *ignoreParens(Expr *Ex) {
  while (ParenExpr *P = dyn_cast<ParenExpr>(Ex)) { Ex = P->getSubExpr(); }
  return Ex;
}

namespace {
class CFGBuilder : public ASTVisitor<CFGBuilder> {
  /// Mapping from expressions to instructions.
  llvm::DenseMap<Expr *, Instruction *> ExprToInst;

  /// The current basic block being constructed.
  BasicBlock *Block;

  /// The CFG being constructed.
  CFG &C;

public:
  CFGBuilder(CFG &C) : Block(0), C(C), badCFG(false) {}

  /// A flag indicating whether or not there were problems
  /// constructing the CFG.
  bool badCFG;

  /// The current basic block being constructed.
  BasicBlock *currentBlock() {
    if (!Block)
      Block = new (C) BasicBlock(&C);
    return Block;
  }

  void addInst(Expr *Ex, Instruction *I) {
    ExprToInst[Ex] = I;
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

  void visitIfStmt(IfStmt *S) {
    assert(false && "Not yet implemented");
  }

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

  void visitExpr(Expr *E) {
    assert(false && "Not yet implemented");
  }

  void visitCallExpr(CallExpr *E);
  void visitDeclRefExpr(DeclRefExpr *E);
  void visitIntegerLiteralExpr(IntegerLiteralExpr *E);
  void visitParenExpr(ParenExpr *E);
  void visitThisApplyExpr(ThisApplyExpr *E);
  void visitTupleExpr(TupleExpr *E);
  void visitTypeOfExpr(TypeOfExpr *E);
  
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
      return visit(S);
    if (Expr *E = ESD.dyn_cast<Expr*>())
      return visit(E);
  }
}

void CFGBuilder::visitCallExpr(CallExpr *E) {
  llvm::SmallVector<Instruction*, 10> Args;
  Expr *Arg = ignoreParens(E->getArg());
  Expr *Fn = E->getFn();
  visit(Fn);

  if (TupleExpr *TU = dyn_cast<TupleExpr>(Arg)) {
      for (auto arg : TU->getElements()) {
        visit(arg);
        Args.push_back(getInst(arg));
      }
    addInst(E, CallInst::create(E, currentBlock(), getInst(Fn), Args));
  }
  else {
    visit(Arg);
    Instruction *ArgI = getInst(Arg);
    addInst(E, CallInst::create(E, currentBlock(), getInst(Fn),
                                ArrayRef<Instruction*>(&ArgI, 1)));
  }
}

void CFGBuilder::visitDeclRefExpr(DeclRefExpr *E) {
  addInst(E, new (C) DeclRefInst(E, currentBlock()));
}

void CFGBuilder::visitThisApplyExpr(ThisApplyExpr *E) {
  visit(E->getFn());
  visit(E->getArg());
  addInst(E, new (C) ThisApplyInst(E,
                                   getInst(E->getFn()),
                                   getInst(E->getArg()),
                                   currentBlock()));
}

void CFGBuilder::visitIntegerLiteralExpr(IntegerLiteralExpr *E) {
  addInst(E, new (C) IntegerLiteralInst(E, currentBlock()));
}

void CFGBuilder::visitParenExpr(ParenExpr *E) {
  visit(E->getSubExpr());
}

void CFGBuilder::visitTupleExpr(TupleExpr *E) {
  for (auto &I : E->getElements()) visit(I);
}

void CFGBuilder::visitTypeOfExpr(TypeOfExpr *E) {
  addInst(E, new (C) TypeOfInst(E, currentBlock()));
}
