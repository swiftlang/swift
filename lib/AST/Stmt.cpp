//===--- Stmt.cpp - Swift Language Statement ASTs -------------------------===//
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
//  This file implements the Stmt class and subclasses.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/Stmt.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/ASTVisitor.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/Support/raw_ostream.h"
using namespace swift;

//===----------------------------------------------------------------------===//
// Stmt methods.
//===----------------------------------------------------------------------===//

// Only allow allocation of Stmts using the allocator in ASTContext.
void *Stmt::operator new(size_t Bytes, ASTContext &C,
                         unsigned Alignment) throw() {
  return C.Allocate(Bytes, Alignment);
}

/// getLocStart - Return the location of the start of the expression.
/// FIXME: Need to extend this to do full source ranges like Clang.
SourceLoc Stmt::getStartLoc() const {
  switch (Kind) {
  case StmtKind::Semi:
    return cast<SemiStmt>(this)->getStartLoc();
  case StmtKind::Assign:
    return cast<AssignStmt>(this)->getStartLoc();
  case StmtKind::Brace:
    return cast<BraceStmt>(this)->getStartLoc();
  case StmtKind::Return:
    return cast<ReturnStmt>(this)->getStartLoc();
  case StmtKind::If:
    return cast<IfStmt>(this)->getStartLoc();
  case StmtKind::While:
    return cast<WhileStmt>(this)->getStartLoc();
  }
  
  llvm_unreachable("Not reachable, all cases handled");
}

SourceLoc AssignStmt::getStartLoc() const {
  return getDest()->getStartLoc();
}

BraceStmt::BraceStmt(SourceLoc lbloc, ArrayRef<ExprStmtOrDecl> elts,
                     SourceLoc rbloc)
  : Stmt(StmtKind::Brace), NumElements(elts.size()), LBLoc(lbloc), RBLoc(rbloc){
  memcpy(getElementsStorage(), elts.data(),
         elts.size() * sizeof(ExprStmtOrDecl));
}

BraceStmt *BraceStmt::create(ASTContext &ctx, SourceLoc lbloc,
                             ArrayRef<ExprStmtOrDecl> elts, SourceLoc rbloc) {
  void *Buffer = ctx.Allocate(sizeof(BraceStmt)
                                + elts.size() * sizeof(ExprStmtOrDecl),
                              Stmt::Alignment);
  return ::new(Buffer) BraceStmt(lbloc, elts, rbloc);
}


//===----------------------------------------------------------------------===//
// Printing for Stmt and all subclasses.
//===----------------------------------------------------------------------===//

namespace {
/// PrintStmt - Visitor implementation of Expr::print.
class PrintStmt : public StmtVisitor<PrintStmt> {
public:
  raw_ostream &OS;
  unsigned Indent;
  
  PrintStmt(raw_ostream &os, unsigned indent) : OS(os), Indent(indent) {
  }
  
  void printRec(Stmt *S) {
    Indent += 2;
    if (S)
      visit(S);
    else
      OS.indent(Indent) << "(**NULL STATEMENT**)";
    Indent -= 2;
  }
  
  void printRec(Decl *D) { D->print(OS, Indent+2); }
  void printRec(Expr *E) { E->print(OS, Indent+2); }
  
  void visitSemiStmt(SemiStmt *S) {
    OS.indent(Indent) << "(semi_stmt)";
  }

  void visitAssignStmt(AssignStmt *S) {
    OS.indent(Indent) << "(assign_stmt\n";
    printRec(S->getDest());
    OS << '\n';
    printRec(S->getSrc());
    OS << ')';
  }

  void visitBraceStmt(BraceStmt *S) {
    OS.indent(Indent) << "(brace_stmt";
    for (auto Elt : S->getElements()) {
      OS << '\n';
      if (Expr *SubExpr = Elt.dyn_cast<Expr*>())
        printRec(SubExpr);
      else if (Stmt *SubStmt = Elt.dyn_cast<Stmt*>())
        printRec(SubStmt);
      else
        printRec(Elt.get<Decl*>());
    }
    OS << ')';
  }
  
  void visitReturnStmt(ReturnStmt *S) {
    OS.indent(Indent) << "(return_stmt\n";
    printRec(S->getResult());
    OS << ')';
  }
  
  void visitIfStmt(IfStmt *S) {
    OS.indent(Indent) << "(if_stmt\n";
    printRec(S->getCond());
    OS << '\n';
    printRec(S->getThenStmt());
    if (S->getElseStmt()) {
      OS << '\n';
      printRec(S->getElseStmt());
    }
    OS << ')';
  }
  void visitWhileStmt(WhileStmt *S) {
    OS.indent(Indent) << "(while_stmt\n";
    printRec(S->getCond());
    OS << '\n';
    printRec(S->getBody());
    OS << ')';
  }
};

} // end anonymous namespace.

void Stmt::dump() const {
  print(llvm::errs());
  llvm::errs() << '\n';
}

void Stmt::print(raw_ostream &OS, unsigned Indent) const {
  PrintStmt(OS, Indent).visit(const_cast<Stmt*>(this));
}
