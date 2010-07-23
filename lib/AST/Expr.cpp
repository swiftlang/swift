//===--- Expr.cpp - Swift Language Expression ASTs ------------------------===//
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
//  This file implements the Expr class and subclasses.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/Expr.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Type.h"
#include "swift/AST/ASTContext.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/ErrorHandling.h"
using namespace swift;
using llvm::cast;

// Only allow allocation of Stmts using the allocator in ASTContext.
void *Expr::operator new(size_t Bytes, ASTContext &C,
                         unsigned Alignment) throw() {
  return C.Allocate(Bytes, Alignment);
}

/// getLocStart - Return the location of the start of the expression.
/// FIXME: Need to extend this to do full source ranges like Clang.
llvm::SMLoc Expr::getLocStart() const {
  switch (Kind) {
  case IntegerLiteralKind: return cast<IntegerLiteral>(this)->Loc;
  case DeclRefExprKind:    return cast<DeclRefExpr>(this)->Loc;
  case ParenExprKind:      return cast<ParenExpr>(this)->LParenLoc;
  case BraceExprKind:      return cast<BraceExpr>(this)->LBLoc;
  case BinaryAddExprKind:
  case BinarySubExprKind:
  case BinaryMulExprKind:
  case BinaryDivExprKind: return cast<BinaryExpr>(this)->getLocStart();
  }
  
  llvm_unreachable("expression type not handled!");
}


void Expr::dump() const {
  print(llvm::errs());
  llvm::errs() << '\n';
}

void Expr::print(llvm::raw_ostream &OS, unsigned Indent) const {
  switch (Kind) {
  case IntegerLiteralKind: return cast<IntegerLiteral>(this)->print(OS, Indent);
  case DeclRefExprKind:    return cast<DeclRefExpr>(this)->print(OS, Indent);
  case ParenExprKind:      return cast<ParenExpr>(this)->print(OS, Indent);
  case BraceExprKind:      return cast<BraceExpr>(this)->print(OS, Indent);
  case BinaryAddExprKind:
  case BinarySubExprKind:
  case BinaryMulExprKind:
  case BinaryDivExprKind: return cast<BinaryExpr>(this)->print(OS, Indent);
  }
}

void IntegerLiteral::print(llvm::raw_ostream &OS, unsigned Indent) const {
  OS.indent(Indent) << "(integer_literal type='";
  Ty->print(OS);
  OS << "' value=" << Val << ')';
}

void DeclRefExpr::print(llvm::raw_ostream &OS, unsigned Indent) const {
  OS.indent(Indent) << "(declrefexpr type='";
  Ty->print(OS);
  OS << "' decl=" << D->Name << ')';
}

void ParenExpr::print(llvm::raw_ostream &OS, unsigned Indent) const {
  OS.indent(Indent) << "(paren_expr type='";
  Ty->print(OS);
  OS << "'\n";
  SubExpr->print(OS, Indent+1);
  OS << ')';
}

void BraceExpr::print(llvm::raw_ostream &OS, unsigned Indent) const {
  OS.indent(Indent) << "(brace_expr type='";
  Ty->print(OS);
  OS << "'";
  
  for (unsigned i = 0, e = NumElements; i != e; ++i) {
    OS << '\n';
    if (Expr *E = Elements[i].dyn_cast<Expr*>())
      E->print(OS, Indent+1);
    else
      Elements[i].get<VarDecl*>()->print(OS, Indent+1);
  }
  
  OS << ')';
}


void BinaryExpr::print(llvm::raw_ostream &OS, unsigned Indent) const {
  OS.indent(Indent) << "(binary_expr type='";
  Ty->print(OS);
  OS << "'\n";
  LHS->print(OS, Indent+1);
  OS << '\n';
  RHS->print(OS, Indent+1);
  OS << ')';
}
