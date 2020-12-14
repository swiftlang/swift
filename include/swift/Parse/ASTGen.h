//===--- ASTGen.h ---------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_PARSE_ASTGEN_H
#define SWIFT_PARSE_ASTGEN_H

#include "swift/AST/ASTContext.h"
#include "swift/AST/Expr.h"
#include "swift/Syntax/SyntaxNodes.h"
#include "llvm/ADT/DenseMap.h"

namespace swift {
class ComponentIdentTypeRepr;
class TupleTypeRepr;

/// Generates AST nodes from Syntax nodes.
class ASTGen {
  ASTContext &Context;
public:
  explicit ASTGen(ASTContext &Context) : Context(Context) {}

  //===--------------------------------------------------------------------===//
  // MARK: - Expressions
public:
  Expr *generate(const syntax::BooleanLiteralExprSyntax &Expr, const SourceLoc &Loc);
  Expr *generate(const syntax::FloatLiteralExprSyntax &Expr, const SourceLoc &Loc);
  Expr *generate(const syntax::IntegerLiteralExprSyntax &Expr, const SourceLoc &Loc);
  Expr *generate(const syntax::NilLiteralExprSyntax &Expr, const SourceLoc &Loc);
  Expr *generate(const syntax::PoundColumnExprSyntax &Expr, const SourceLoc &Loc);
  Expr *generate(const syntax::PoundDsohandleExprSyntax &Expr, const SourceLoc &Loc);
  Expr *generate(const syntax::PoundFileExprSyntax &Expr, const SourceLoc &Loc);
  Expr *generate(const syntax::PoundFileIDExprSyntax &Expr, const SourceLoc &Loc);
  Expr *generate(const syntax::PoundFilePathExprSyntax &Expr, const SourceLoc &Loc);
  Expr *generate(const syntax::PoundLineExprSyntax &Expr, const SourceLoc &Loc);
  Expr *generate(const syntax::PoundFunctionExprSyntax &Expr, const SourceLoc &Loc);
  Expr *generate(const syntax::UnknownExprSyntax &Expr, const SourceLoc &Loc);

  //===--------------------------------------------------------------------===//
  // MARK: - Types.
public:
  TypeRepr *generate(const syntax::SimpleTypeIdentifierSyntax &Type,
                     const SourceLoc Loc);

  //===--------------------------------------------------------------------===//
  // MARK: Other
public:
  /// Copy a numeric literal value into AST-owned memory, stripping underscores
  /// so the semantic part of the value can be parsed by APInt/APFloat parsers.
  static StringRef copyAndStripUnderscores(StringRef Orig, ASTContext &Context);

private:
  StringRef copyAndStripUnderscores(StringRef Orig);

  /// Advance \p Loc to the first token of the \p Node.
  /// \p Loc must be the leading trivia of the first token in the tree in which
  /// \p Node resides.
  static SourceLoc advanceLocBegin(const SourceLoc &Loc,
                                   const syntax::Syntax &Node);

  /// Advance \p Loc to the last non-missing token of the \p Node or, if it
  /// doesn't contain any, the last non-missing token preceding it in the tree.
  /// \p Loc must be the leading trivia of the first token in the tree in which
  /// \p Node resides
  static SourceLoc advanceLocEnd(const SourceLoc &Loc,
                                 const syntax::Syntax &Node);

  Expr *
  generateMagicIdentifierLiteralExpr(const syntax::TokenSyntax &PoundToken,
                                     const SourceLoc &Loc);

  /// Map magic literal tokens such as #file to their MagicIdentifierLiteralExpr
  /// kind.
  MagicIdentifierLiteralExpr::Kind getMagicIdentifierLiteralKind(tok Kind);
};
} // namespace swift

#endif // SWIFT_PARSE_ASTGEN_H
