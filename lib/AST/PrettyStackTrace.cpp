//===--- PrettyStackTrace.cpp - Swift-specific PrettyStackTraceEntries ----===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
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
#include "swift/Basic/SourceManager.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/MemoryBuffer.h"

using namespace swift;

void PrettyStackTraceDecl::print(llvm::raw_ostream &out) const {
  out << "While " << Action << ' ';
  if (!TheDecl) {
    out << "NULL declaration!\n";
    return;
  }
  printDeclDescription(out, TheDecl, TheDecl->getASTContext());
}

void swift::printDeclDescription(llvm::raw_ostream &out, const Decl *D,
                                 ASTContext &Context) {
  SourceLoc loc = D->getStartLoc();
  bool hasPrintedName = false;
  if (auto *named = dyn_cast<ValueDecl>(D)) {
    if (named->hasName()) {
      out << '\'' << named->getBaseName() << '\'';
      hasPrintedName = true;
    } else if (auto *fn = dyn_cast<FuncDecl>(named)) {
      if (auto *ASD = fn->getAccessorStorageDecl()) {
        if (ASD->hasName()) {
          switch (fn->getAccessorKind()) {
          case AccessorKind::NotAccessor:
            llvm_unreachable("Isn't an accessor?");
          case AccessorKind::IsGetter:
            out << "getter";
            break;
          case AccessorKind::IsSetter:
            out << "setter";
            break;
          case AccessorKind::IsWillSet:
            out << "willset";
            break;
          case AccessorKind::IsDidSet:
            out << "didset";
            break;
          case AccessorKind::IsMaterializeForSet:
            out << "materializeForSet";
            break;
          case AccessorKind::IsAddressor:
            out << "addressor";
            break;
          case AccessorKind::IsMutableAddressor:
            out << "mutableAddressor";
            break;
          }
          
          out << " for " << ASD->getFullName();
          hasPrintedName = true;
          loc = ASD->getStartLoc();
        }
      }
    }
  } else if (auto *extension = dyn_cast<ExtensionDecl>(D)) {
    Type extendedTy = extension->getExtendedType();
    if (extendedTy) {
      out << "extension of " << extendedTy;
      hasPrintedName = true;
    }
  }

  if (!hasPrintedName)
    out << "declaration " << (const void *)D;

  if (loc.isValid()) {
    out << " at ";
    loc.print(out, Context.SourceMgr);
  } else {
    out << " in module '" << D->getModuleContext()->getName() << '\'';
  }
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
  out << "expression at ";
  E->getSourceRange().print(out, Context.SourceMgr);
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
  S->getSourceRange().print(out, Context.SourceMgr);
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
  P->getSourceRange().print(out, Context.SourceMgr);
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
} // end anonymous namespace

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
    if (decl->getSourceRange().isValid()) {
      out << " (declared at ";
      decl->getSourceRange().print(out, Context.SourceMgr);
      out << ')';
    }
  }
  out << '\n';
}

void PrettyStackTraceTypeRepr::print(llvm::raw_ostream &out) const {
  out << "While " << Action << " type ";
  TheType->print(out);
  if (TheType && TheType->getSourceRange().isValid()) {
    out << " at ";
    TheType->getSourceRange().print(out, Context.SourceMgr);
  }
  out << '\n';
}

void swift::printSourceLocDescription(llvm::raw_ostream &out,
                                      SourceLoc loc, ASTContext &ctx) {
  loc.print(out, ctx.SourceMgr);
  out << '\n';
}

void PrettyStackTraceLocation::print(llvm::raw_ostream &out) const {
  out << "While " << Action << " starting at ";
  printSourceLocDescription(out, Loc, Context);
}
