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

//===----------------------------------------------------------------------===//
// Expr methods.
//===----------------------------------------------------------------------===//

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
  case TupleExprKind:      return cast<TupleExpr>(this)->LParenLoc;
  case ApplyExprKind:      return cast<ApplyExpr>(this)->Fn->getLocStart();
  case SequenceExprKind:
    return cast<SequenceExpr>(this)->Elements[0]->getLocStart();
  case BraceExprKind:      return cast<BraceExpr>(this)->LBLoc;
  case ClosureExprKind:    return cast<ClosureExpr>(this)->Input->getLocStart();
  case BinaryExprKind:     return cast<BinaryExpr>(this)->LHS->getLocStart();
  }
  
  llvm_unreachable("expression type not handled!");
}

//===----------------------------------------------------------------------===//
// Support methods for Exprs.
//===----------------------------------------------------------------------===//

/// getNumArgs - Return the number of arguments that this closure expr takes.
/// This is the length of the ArgList.
unsigned ClosureExpr::getNumArgs() const {
  // FIXME: This should desugar the type if needed!
  Type *Input = cast<FunctionType>(Ty)->Input;
  
  if (TupleType *TT = llvm::dyn_cast<TupleType>(Input))
    return TT->NumFields;
  return 1;  
}


//===----------------------------------------------------------------------===//
// Printing for Expr and all subclasses.
//===----------------------------------------------------------------------===//

void Expr::dump() const {
  print(llvm::errs());
  llvm::errs() << '\n';
}

void Expr::print(llvm::raw_ostream &OS, unsigned Indent) const {
  switch (Kind) {
  case IntegerLiteralKind: return cast<IntegerLiteral>(this)->print(OS, Indent);
  case DeclRefExprKind:    return cast<DeclRefExpr>(this)->print(OS, Indent);
  case TupleExprKind:      return cast<TupleExpr>(this)->print(OS, Indent);
  case ApplyExprKind:      return cast<ApplyExpr>(this)->print(OS, Indent);
  case SequenceExprKind:   return cast<SequenceExpr>(this)->print(OS, Indent);
  case BraceExprKind:      return cast<BraceExpr>(this)->print(OS, Indent);
  case ClosureExprKind:    return cast<ClosureExpr>(this)->print(OS, Indent);
  case BinaryExprKind:     return cast<BinaryExpr>(this)->print(OS, Indent);
  }
}

void IntegerLiteral::print(llvm::raw_ostream &OS, unsigned Indent) const {
  OS.indent(Indent) << "(integer_literal type='";
  Ty->print(OS);
  OS << "' value=" << Val << ')';
}

void DeclRefExpr::print(llvm::raw_ostream &OS, unsigned Indent) const {
  OS.indent(Indent) << "(declref_expr type='";
  Ty->print(OS);
  OS << "' decl=" << D->Name << ')';
}

void TupleExpr::print(llvm::raw_ostream &OS, unsigned Indent) const {
  OS.indent(Indent) << "(tuple_expr type='";
  Ty->print(OS);
  OS << "'";
  if (NumSubExprs != 0) {
    for (unsigned i = 0, e = NumSubExprs; i != e; ++i) {
      OS << '\n';
      SubExprs[i]->print(OS, Indent+1);
    }
  }
  OS << ')';
}

void ApplyExpr::print(llvm::raw_ostream &OS, unsigned Indent) const {
  OS.indent(Indent) << "(apply_expr type='";
  Ty->print(OS);
  OS << "'\n";
  Fn->print(OS, Indent+1);
  OS << '\n';
  Arg->print(OS, Indent+1);
  OS << ')';
}

void SequenceExpr::print(llvm::raw_ostream &OS, unsigned Indent) const {
  OS.indent(Indent) << "(sequence_expr type='";
  Ty->print(OS);
  OS << "'";
  
  for (unsigned i = 0, e = NumElements; i != e; ++i)
    Elements[i]->print(OS << '\n', Indent+1);
  
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
      Elements[i].get<NamedDecl*>()->print(OS, Indent+1);
  }
  
  OS << ')';
}

void ClosureExpr::print(llvm::raw_ostream &OS, unsigned Indent) const {
  OS.indent(Indent) << "(closure_expr type='";
  Ty->print(OS);
  OS << "'\n";
  
  if (ArgList) {
    for (unsigned i = 0, e = getNumArgs(); i != e; ++i)
      if (ArgList[i].isNonNull()) {
        ArgList[i].get()->print(OS, Indent+1);
        OS << '\n';
      }
  }
  
  Input->print(OS, Indent+1);
  OS << ')';
}

void BinaryExpr::print(llvm::raw_ostream &OS, unsigned Indent) const {
  OS.indent(Indent) << "(binary_expr '";
  OS << Fn->Name << "' type='";
  Ty->print(OS);
  OS << "'\n";
  LHS->print(OS, Indent+1);
  OS << '\n';
  RHS->print(OS, Indent+1);
  OS << ')';
}
