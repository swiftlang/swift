//===--- ClangSynthesizedDecls.h - Clang AST synthesis helpers --*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
#ifndef SWIFT_CLANG_SYNTHESIZED_DECLS_H
#define SWIFT_CLANG_SYNTHESIZED_DECLS_H

#include "clang/AST/ASTContext.h"
#include "clang/AST/Decl.h"
#include "clang/AST/Expr.h"
#include "clang/AST/Stmt.h"

namespace swift {
namespace importer {

inline clang::ParmVarDecl *
createClangParmVarDecl(clang::ASTContext &ctx, clang::DeclContext *dc,
                       clang::IdentifierInfo *nameId, clang::QualType type) {
  auto *param = clang::ParmVarDecl::Create(
      ctx, dc, clang::SourceLocation(), clang::SourceLocation(), nameId, type,
      ctx.getTrivialTypeSourceInfo(type), clang::SC_None, nullptr);
  param->setImplicit();
  return param;
}

inline clang::DeclRefExpr *
createClangDeclRefExpr(clang::ASTContext &ctx, clang::ValueDecl *decl,
                       clang::QualType type,
                       clang::ExprValueKind vk = clang::VK_PRValue) {
  return new (ctx) clang::DeclRefExpr(ctx, decl, false, type, vk,
                                      clang::SourceLocation());
}

inline clang::ReturnStmt *createClangReturnStmt(clang::ASTContext &ctx,
                                                clang::Expr *expr) {
  return clang::ReturnStmt::Create(ctx, clang::SourceLocation(), expr,
                                   nullptr);
}

inline clang::FunctionDecl *
createClangFunctionDecl(clang::ASTContext &ctx, clang::DeclContext *dc,
                        clang::DeclarationName name,
                        clang::QualType funcType) {
  auto *decl = clang::FunctionDecl::Create(
      ctx, dc, clang::SourceLocation(), clang::SourceLocation(), name,
      funcType, ctx.getTrivialTypeSourceInfo(funcType), clang::SC_Static);
  decl->setImplicit();
  decl->setImplicitlyInline();
  return decl;
}

} // namespace importer
} // namespace swift

#endif // SWIFT_CLANG_SYNTHESIZED_DECLS_H
