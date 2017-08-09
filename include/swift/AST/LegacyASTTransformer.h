//===--- LegacyASTTransformer.h - Swift lib/AST -> lib/Syntax ---*- C++ -*-===//
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
// This file defines the interfaces for transforming a lib/AST tree into a
// lib/Syntax tree with full source fidelity. This is meant to be temporary
// infrastructure only. The goal of this library is to produce a syntax tree
// which is more amenable to structured editing. Once lib/Syntax is fully
// integrated into the rest of the pipeline, this will no longer be necessary.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_LEGACYASTTRANSFORMER_H
#define SWIFT_AST_LEGACYASTTRANSFORMER_H

#include "swift/AST/ASTNode.h"
#include "swift/AST/ASTVisitor.h"
#include "swift/AST/SyntaxASTMap.h"
#include "swift/Syntax/References.h"
#include "swift/Syntax/Syntax.h"
#include "swift/Syntax/SyntaxNodes.h"
#include "swift/Syntax/TokenSyntax.h"

namespace swift {
namespace syntax {

using TokenPositionList = std::vector<std::pair<RC<RawTokenSyntax>,
                                                AbsolutePosition>>;

/// Transforms a swift/AST into a swift/Syntax/RC<RawSyntax>.
///
/// The default behavior for this transformer, if it doesn't know how to
/// convert the AST node, is to return an `UnknownRC<RawSyntax>` containing the
/// tokens in the source range.
class LegacyASTTransformer : public ASTVisitor<LegacyASTTransformer,
    RC<SyntaxData>, // Expr return type
    RC<SyntaxData>, // Stmt return type
    RC<SyntaxData>, // Decl return type
    RC<SyntaxData>, // Pattern return type
    RC<SyntaxData>, // TypeRepr return type
    RC<SyntaxData>> // Attribute return type
  {
  SyntaxASTMap &ASTMap;
  SourceManager &SourceMgr;
  const unsigned BufferID;
  const TokenPositionList &Tokens;

  std::pair<AttributeListSyntax, ModifierListSyntax>
  getAttributesFromDecl(Decl *D);

  AttributeSyntax getAttribute(DeclAttribute *attr);

  DeclModifierSyntax getModifier(DeclAttribute *attr);

  /// Convert the provided Syntax node to a StmtSyntax, either by wrapping it in
  /// an `ExpressionStmt` or a `DeclarationStmt`.
  StmtSyntax getStmtSyntax(Syntax Node);

  /// Transform a legacy TypeRepr to a full-fidelity `TypeSyntax`.
  ///
  /// If a TypeRepr's kind isn't covered by the transform, an `UnknownSyntax`
  /// will be returned containing all of the `TokenSyntax`es that comprise the
  /// node.
  ///
  /// If the node isn't expressible in a `TypeSyntax`, then `None` is returned.
  Optional<TypeSyntax> transform(TypeRepr *T);

  /// Transform a legacy Expr to a full-fidelity `ExprSyntax`.
  ///
  /// If an ASTNode's kind isn't covered by the transform, an `UnknownSyntax`
  /// will be returned containing all of the `TokenSyntax`es that comprise the
  /// node.
  ///
  /// If the node isn't expressible in an `ExprSyntax`, then `None` is returned.
  Optional<ExprSyntax> transform(Expr *E);

  /// Transform a legacy Decl to a full-fidelity `DeclSyntax`.
  ///
  /// If an ASTNode's kind isn't covered by the transform, an `UnknownSyntax`
  /// will be returned containing all of the `TokenSyntax`es that comprise the
  /// node.
  ///
  /// If the node isn't expressible in a `DeclSyntax`, then `None` is returned.
  Optional<DeclSyntax> transform(Decl *D);

  /// Transform a legacy Stmt to a full-fidelity `StmtSyntax`.
  ///
  /// If an ASTNode's kind isn't covered by the transform, an `UnknownSyntax`
  /// will be returned containing all of the `TokenSyntax`es that comprise the
  /// node.
  ///
  /// If the node isn't expressible in a `StmtSyntax`, then `None` is returned.
  Optional<StmtSyntax> transform(Stmt *S);

  /// Do a binary search for a token at the given `Offset`.
  TokenSyntax findToken(SourceLoc Loc,
                        Optional<tok> ExpectedKind = None,
                        OwnedString ExpectedText = OwnedString());
public:
  LegacyASTTransformer(SyntaxASTMap &ASTMap,
                       SourceManager &SourceMgr,
                       const unsigned BufferID,
                       const TokenPositionList &Tokens)
      : ASTMap(ASTMap), SourceMgr(SourceMgr),
        BufferID(BufferID), Tokens(Tokens) {}

  /// If the Decl has attributes, provide the start SourceLoc for them;
  /// otherwise, just ask the Decl for its usual start SourceLoc.
  SourceLoc getStartLocForDecl(const Decl *D) const;
  SourceLoc getEndLocForDecl(const Decl *D) const;
  SourceLoc getEndLocForStmt(const Stmt *S) const;
  SourceLoc getEndLocForExpr(const Expr *E) const;
  RC<SyntaxData> getUnknownSyntax(SourceRange SR, SyntaxKind Kind);
  RC<SyntaxData> getUnknownType(TypeRepr *T);
  RC<SyntaxData> getUnknownDecl(Decl *D);
  RC<SyntaxData> getUnknownStmt(Stmt *S);
  RC<SyntaxData> getUnknownExpr(Expr *E);
  RC<SyntaxData> visitMembers(DeclRange Members);

  /// Transform a legacy AST node to a full-fidelity `Syntax`.
  ///
  /// If an ASTNode's kind isn't covered by the transform, an `UnknownSyntax`
  /// will be returned containing all of the `TokenSyntax`es that comprise the
  /// node.
  ///
  /// If the node isn't expressible in a `Syntax`, then `None` is returned.
  Optional<Syntax> transform(ASTNode Node);

#define DECL(CLASS, PARENT) RC<SyntaxData> \
  visit##CLASS##Decl(CLASS##Decl *, \
    const SyntaxData *Parent = nullptr, \
                     const CursorIndex IndexInParent = 0);
#include "swift/AST/DeclNodes.def"

#define STMT(CLASS, PARENT) RC<SyntaxData> \
  visit##CLASS##Stmt(CLASS##Stmt *, \
    const SyntaxData *Parent = nullptr, \
                     const CursorIndex IndexInParent = 0);
#include "swift/AST/StmtNodes.def"

#define EXPR(CLASS, PARENT) RC<SyntaxData> \
  visit##CLASS##Expr(CLASS##Expr *, \
                     const SyntaxData *Parent = nullptr, \
                     const CursorIndex IndexInParent = 0);
#include "swift/AST/ExprNodes.def"

#define TYPEREPR(CLASS, PARENT) RC<SyntaxData> \
  visit##CLASS##TypeRepr(CLASS##TypeRepr *, \
                         const SyntaxData *Parent = nullptr, \
                         const CursorIndex IndexInParent = 0);
#include "swift/AST/TypeReprNodes.def"
};

/// Transform a legacy AST node to a full-fidelity `Syntax`.
///
/// If an ASTNode's kind isn't covered by the transform, an `UnknownSyntax`
/// will be returned containing all of the `TokenSyntax`es that comprise the
/// node.
///
/// If the node isn't expressible in a `Syntax`, then `None` is returned.
Optional<Syntax>
transformAST(ASTNode Node,
             SyntaxASTMap &Sema,
             SourceManager &SourceMgr,
             const unsigned BufferID,
             const TokenPositionList &Tokens);

} // end namespace syntax
} // end namespace swift

#endif // SWIFT_AST_LEGACYASTTRANSFORMER_H
