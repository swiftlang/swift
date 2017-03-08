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

#ifndef SWIFT_SYNTAX_LEGACYASTTRANSFORMER_H
#define SWIFT_SYNTAX_LEGACYASTTRANSFORMER_H

#include "swift/AST/ASTNode.h"
#include "swift/AST/ASTVisitor.h"
#include "swift/Sema/Semantics.h"
#include "swift/Syntax/References.h"
#include "swift/Syntax/Syntax.h"
#include "swift/Syntax/TokenSyntax.h"

namespace swift {
namespace syntax {

using TokenPositionList = std::vector<std::pair<RC<TokenSyntax>,
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
  sema::Semantics &Sema;
  SourceManager &SourceMgr;
  const unsigned BufferID;
  const TokenPositionList &Tokens;
public:
  LegacyASTTransformer(sema::Semantics &Sema,
                       SourceManager &SourceMgr,
                       const unsigned BufferID,
                       const TokenPositionList &Tokens)
      : Sema(Sema), SourceMgr(SourceMgr), BufferID(BufferID), Tokens(Tokens) {}

  /// If the Decl has attributes, provide the start SourceLoc for them;
  /// otherwise, just ask the Decl for its usual start SourceLoc.
  SourceLoc getStartLocForDecl(const Decl *D) const;
  SourceLoc getEndLocForDecl(const Decl *D) const;
  SourceLoc getEndLocForStmt(const Stmt *S) const;
  SourceLoc getEndLocForExpr(const Expr *E) const;
  RC<SyntaxData> getUnknownSyntax(SourceRange SR);
  RC<SyntaxData> getAttributesFromDecl(Decl *D);
  RC<SyntaxData> getUnknownDecl(Decl *D);
  RC<SyntaxData> getUnknownStmt(Stmt *S);
  RC<SyntaxData> getUnknownExpr(Expr *E);
  RC<SyntaxData> visitMembers(DeclRange Members);

//  RC<RawSyntax> visitDecl(Decl *D);
//  RC<RawSyntax> visitExpr(Expr *E);
//  RC<RawSyntax> visitStmt(Stmt *S);
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
};

/// Transform a legacy AST node to a full-fidelity `RC<RawSyntax>`.
///
/// If an ASTNode's kind isn't covered by the transform, a `RC<RawSyntax>` for
/// a SyntaxKind::Unknown will be returned containing all of the TokenSyntaxs that
/// comprise the node.
///
/// If the node isn't expressible in a `RC<RawSyntax>`, then `None` is returned.
Optional<Syntax>
transformAST(ASTNode Node,
             sema::Semantics &Sema,
             SourceManager &SourceMgr,
             const unsigned BufferID,
             const TokenPositionList &Tokens);

/// Do a binary search for a token at the given `Offset`.
RC<TokenSyntax> findTokenSyntax(tok ExpectedKind,
                                OwnedString ExpectedText,
                                SourceManager &SourceMgr,
                                SourceLoc Loc,
                                unsigned BufferID,
                                const TokenPositionList &Tokens);

//ArrayRef<RC<TokenSyntax>>
//syntax::tokensInRange(SourceRange Range, const TokenPositionList &Tokens);

} // end namespace syntax
} // end namespace swift

#endif // SWIFT_SYNTAX_LEGACYASTTRANSFORMER_H
