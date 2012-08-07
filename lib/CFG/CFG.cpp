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
    OS << "(func_decl " << FD->getName() << '\n';
    FuncExpr *FE = FD->getBody();
    llvm::OwningPtr<CFG> C(CFG::constructCFG(FE->getBody()));
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

namespace {
class CFGBuilder : public ASTVisitor<CFGBuilder> {
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
  BasicBlock *block() {
    if (!Block)
      Block = new (C) BasicBlock(&C);
    return Block;
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
    assert(false && "Not yet implemented");
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
    assert(false && "Not yet implemented");
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

