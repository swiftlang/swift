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
                         unsigned Alignment) {
  return C.Allocate(Bytes, Alignment);
}

// Helper functions to verify statically whether the getSourceRange()
// function has been overridden.
typedef const char (&TwoChars)[2];

template<typename Class> 
inline char checkSourceRangeType(SourceRange (Class::*)() const);

inline TwoChars checkSourceRangeType(SourceRange (Stmt::*)() const);

SourceRange Stmt::getSourceRange() const {
  switch (Kind) {
#define STMT(ID, PARENT) \
case StmtKind::ID: \
static_assert(sizeof(checkSourceRangeType(&ID##Stmt::getSourceRange)) == 1, \
              #ID "Stmt is missing getSourceRange()"); \
return cast<ID##Stmt>(this)->getSourceRange();
#include "swift/AST/StmtNodes.def"
  }
  
  llvm_unreachable("statement type not handled!");
}

bool Stmt::isImplicit() const {
  if (auto brace = dyn_cast<BraceStmt>(this)) {
    for (auto element : brace->getElements()) {
      if (auto expr = element.dyn_cast<Expr *>()) {
        if (!expr->isImplicit())
          return false;
        continue;
      }

      if (auto stmt = element.dyn_cast<Stmt *>()) {
        if (!stmt->isImplicit())
          return false;
        continue;
      }

      if (!element.get<Decl *>()->isImplicit())
        return false;
    }
    if (brace->getLBraceLoc().isInvalid() &&
        brace->getRBraceLoc().isInvalid())
      return true;
  }

  return false;
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
                              alignof(BraceStmt));
  return ::new(Buffer) BraceStmt(lbloc, elts, rbloc);
}

SourceRange ReturnStmt::getSourceRange() const {
  SourceLoc Start = ReturnLoc;
  SourceLoc End = ReturnLoc;
  if (Result)
    End = Result->getEndLoc();
  if (Start.isInvalid() && Result)
    Start = Result->getStartLoc();
  
  return SourceRange(Start, End);
}

SourceRange IfStmt::getSourceRange() const {
  SourceLoc End;
  if (Else)
    End = Else->getEndLoc();
  else
    End = Then->getEndLoc();
  return SourceRange(IfLoc, End);
}

SourceRange WhileStmt::getSourceRange() const {
  return SourceRange(WhileLoc, Body->getEndLoc());
}

SourceRange DoWhileStmt::getSourceRange() const {
  return SourceRange(DoLoc, Cond->getEndLoc());
}

Pattern *ForEachStmt::getPattern() const {
  if (Pattern *P = Pat.dyn_cast<Pattern *>())
    return P;
  return Pat.get<PatternBindingDecl *>()->getPattern();
}

CaseStmt *CaseStmt::create(SourceLoc CaseOrDefaultLoc,
                           ArrayRef<Expr*> ValueExprs,
                           SourceLoc ColonLoc,
                           BraceStmt *Body,
                           ASTContext &C) {
  void *p = C.Allocate(sizeof(CaseStmt) + ValueExprs.size() * sizeof(Expr*),
                       alignof(CaseStmt));
  CaseStmt *theCase = ::new (p) CaseStmt(CaseOrDefaultLoc,
                                         ValueExprs.size(),
                                         ColonLoc, Body);
  memcpy(theCase->getValueExprBuffer(),
         ValueExprs.data(), ValueExprs.size() * sizeof(Expr*));
  return theCase;
}

SwitchStmt *SwitchStmt::create(SourceLoc SwitchLoc,
                               Expr *SubjectExpr,
                               VarDecl *SubjectDecl,
                               SourceLoc LBraceLoc,
                               ArrayRef<CaseStmt *> Cases,
                               SourceLoc RBraceLoc,
                               ASTContext &C) {
  void *p = C.Allocate(sizeof(SwitchStmt) + Cases.size() * sizeof(SwitchStmt*),
                       alignof(SwitchStmt));
  SwitchStmt *theSwitch = ::new (p) SwitchStmt(SwitchLoc,
                                               SubjectExpr,
                                               SubjectDecl,
                                               LBraceLoc,
                                               Cases.size(),
                                               RBraceLoc);
  memcpy(theSwitch->getCaseBuffer(),
         Cases.data(), Cases.size() * sizeof(CaseStmt*));
  return theSwitch;
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
  
  void printRec(Decl *D) { D->dump(Indent+2); }
  void printRec(Expr *E) { E->print(OS, Indent+2); }
  void printRec(Pattern *P) { P->print(OS); }

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
    OS.indent(Indent) << "(return_stmt";
    if (S->hasResult()) {
      OS << '\n';
      printRec(S->getResult());
    }
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

  void visitDoWhileStmt(DoWhileStmt *S) {
    OS.indent(Indent) << "(do_while_stmt\n";
    printRec(S->getBody());
    OS << '\n';
    printRec(S->getCond());
    OS << ')';
  }
  void visitForStmt(ForStmt *S) {
    OS.indent(Indent) << "(for_stmt\n";
    if (!S->getInitializerVarDecls().empty()) {
      for (auto D : S->getInitializerVarDecls()) {
        printRec(D);
        OS << '\n';
      }
    } else if (S->getInitializer()) {
      printRec(S->getInitializer());
      OS << '\n';
    } else {
      OS.indent(Indent+2) << "<null initializer>\n";
    }

    if (S->getCond().isNull())
      OS.indent(Indent+2) << "<null condition>";
    else
      printRec(S->getCond().get());
    OS << '\n';

    if (S->getIncrement()) {
      printRec(S->getIncrement());
    } else {
      OS.indent(Indent+2) << "<null increment>";
    }
    OS << '\n';
    printRec(S->getBody());
    OS << ')';
  }
  void visitForEachStmt(ForEachStmt *S) {
    OS.indent(Indent) << "(for_each_stmt\n";
    printRec(S->getPattern());
    OS << '\n';
    printRec(S->getContainer());
    OS << '\n';
    printRec(S->getBody());
    OS << ')';
  }
  void visitBreakStmt(BreakStmt *S) {
    OS.indent(Indent) << "(break_stmt)";
  }
  void visitContinueStmt(ContinueStmt *S) {
    OS.indent(Indent) << "(continue_stmt)";
  }
  void visitFallthroughStmt(FallthroughStmt *S) {
    OS.indent(Indent) << "(fallthrough_stmt)";
  }
  void visitSwitchStmt(SwitchStmt *S) {
    OS.indent(Indent) << "(switch_stmt\n";
    printRec(S->getSubjectExpr());
    for (CaseStmt *C : S->getCases()) {
      OS << '\n';
      printRec(C);
    }
    OS << ')';
  }
  void visitCaseStmt(CaseStmt *S) {
    OS.indent(Indent) << "(case_stmt";
    for (Expr *valueExpr : S->getValueExprs()) {
      OS << '\n';
      printRec(valueExpr);
    }
    if (Expr *condExpr = S->getConditionExpr()) {
      OS << "\n";
      OS.indent(Indent+2);
      OS << "condition=";
      condExpr->print(OS, Indent+2);
    }
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
