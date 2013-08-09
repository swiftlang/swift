//===--- PrettyStackTrace.cpp - Swift-specific PrettyStackTraceEntries ----===//
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
//  This file implements several Swift-specific implementations of
//  PrettyStackTraceEntry.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/AST.h"
#include "swift/AST/PrettyStackTrace.h"
#include "swift/AST/TypeVisitor.h"
#include "swift/Basic/Optional.h"
#include "swift/Basic/SourceManager.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/MemoryBuffer.h"

using namespace swift;

static Optional<DecomposedLoc> decompose(SourceManager &SM, SourceLoc Loc) {
  if (!Loc.isValid())
    return Nothing;

  return SM.decompose(Loc);
}

static void printDecomposedLoc(llvm::raw_ostream &out,
                               const DecomposedLoc &loc) {
  out << loc.Buffer->getBufferIdentifier()
      << ":" << loc.Line << ':' << loc.Column;
}

static void printDecomposedLoc(llvm::raw_ostream &out,
                               const Optional<DecomposedLoc> &loc) {
  if (!loc) {
    out << "<<invalid location>>";
    return;
  }

  return printDecomposedLoc(out, loc.getValue());
}

void swift::printSourceLoc(llvm::raw_ostream &out, SourceLoc loc,
                           ASTContext &Context) {
  printDecomposedLoc(out, decompose(Context.SourceMgr, loc));
}

static void printSourceRange(llvm::raw_ostream &out, SourceRange range,
                             ASTContext &Context) {
  Optional<DecomposedLoc> start = decompose(Context.SourceMgr, range.Start);
  Optional<DecomposedLoc> end = decompose(Context.SourceMgr, range.End);

  // Use a unified message if both locations are invalid.
  if (!start && !end) {
    out << "<<invalid source range>>";
    return;
  }

  // Print the start location as normal.
  printDecomposedLoc(out, start);
  out << '-';

  // Only print the non-matching information from the second location.
  if (!start || !end || start.getValue().Buffer != end.getValue().Buffer) {
    printDecomposedLoc(out, end);
    return;
  }

  if (start.getValue().Line != end.getValue().Line) {
    out << end.getValue().Line << ':';
  }

  out << end.getValue().Column;
}

void PrettyStackTraceDecl::print(llvm::raw_ostream &out) const {
  out << "While " << Action << ' ';
  if (!TheDecl) {
    out << "NULL declaration!\n";
    return;
  }
  printDeclDescription(out, TheDecl, TheDecl->getASTContext());
}

void swift::printDeclDescription(llvm::raw_ostream &out, Decl *D,
                                 ASTContext &Context) {
  if (ValueDecl *named = dyn_cast<ValueDecl>(D)) {
    if (named->getName().get())
      out << '\'' << named->getName() << '\'';
    else
      out << "'anonname=" << (const void*)named << '\'';
  } else {
    out << "declaration";
  }
  out << " at ";
  printSourceLoc(out, D->getStartLoc(), Context);
  out << '\n';
}

void PrettyStackTraceExpr::print(llvm::raw_ostream &out) const {
  out << "While " << Action << ' ';
  if (!TheExpr) {
    out << "NULL expression!\n";
    return;
  }
  printExprDescription(out, TheExpr, Context);
}

void swift::printExprDescription(llvm::raw_ostream &out, Expr *E,
                                 ASTContext &Context) {
  if (isa<FuncExpr>(E)) {
    out << "function";
  } else {
    out << "expression";
  }
  out << " at ";
  printSourceRange(out, E->getSourceRange(), Context);
  out << '\n';
}

void PrettyStackTraceStmt::print(llvm::raw_ostream &out) const {
  out << "While " << Action << ' ';
  if (!TheStmt) {
    out << "NULL statement!\n";
    return;
  }
  printStmtDescription(out, TheStmt, Context);
}

void swift::printStmtDescription(llvm::raw_ostream &out, Stmt *S,
                                 ASTContext &Context) {
  out << "statement at ";
  printSourceRange(out, S->getSourceRange(), Context);
  out << '\n';
}

void PrettyStackTracePattern::print(llvm::raw_ostream &out) const {
  out << "While " << Action << ' ';
  if (!ThePattern) {
    out << "NULL pattern!\n";
    return;
  }
  printPatternDescription(out, ThePattern, Context);
}

void swift::printPatternDescription(llvm::raw_ostream &out, Pattern *P,
                                    ASTContext &Context) {
  out << "pattern at ";
  printSourceRange(out, P->getSourceRange(), Context);
  out << '\n';
}

namespace {
  /// Map a Type to an interesting declaration whose source range we
  /// should print.
  struct InterestingDeclForType
      : TypeVisitor<InterestingDeclForType, Decl*> {
    Decl *visitType(TypeBase *type) {
      return nullptr;
    }
    Decl *visitUnboundGenericType(UnboundGenericType *type) {
      return type->getDecl();
    }
    Decl *visitBoundGenericType(BoundGenericType *type) {
      return type->getDecl();
    }
    Decl *visitNominalType(NominalType *type) {
      return type->getDecl();
    }
    Decl *visitNameAliasType(NameAliasType *type) {
      return type->getDecl();
    }
  };
}

void PrettyStackTraceType::print(llvm::raw_ostream &out) const {
  out << "While " << Action << ' ';
  if (TheType.isNull()) {
    out << "NULL type!\n";
    return;
  }
  printTypeDescription(out, TheType, Context);
}

void swift::printTypeDescription(llvm::raw_ostream &out, Type type,
                                 ASTContext &Context) {
  out << "type '" << type << '\'';
  if (Decl *decl = InterestingDeclForType().visit(type)) {
    out << " (declared at ";
    printSourceRange(out, decl->getSourceRange(), Context);
    out << ')';
  }
  out << '\n';
}

void PrettyStackTraceLocation::print(llvm::raw_ostream &out) const {
  out << "While " << Action << " starting at ";
  printSourceLoc(out, Loc, Context);
  out << '\n';
}
