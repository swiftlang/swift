//===--- SyntaxTransformer.h ----------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_PARSE_SYNTAXTRANSFORMER_H
#define SWIFT_PARSE_SYNTAXTRANSFORMER_H

#include "swift/AST/ASTContext.h"
#include "swift/AST/Expr.h"
#include "swift/Syntax/SyntaxNodes.h"
#include "llvm/ADT/DenseMap.h"

namespace swift {
/// Transforms Syntax nodes into AST nodes.
class SyntaxTransformer {
  ASTContext &Context;
  // A stack of source locations of syntax constructs. Allows us to get the
  // SourceLoc necessary to create AST nodes for nodes in not-yet-complete
  // Syntax tree. The topmost item should always correspond to the token/node
  // that has been parsed/transformed most recently.
  // todo [gsoc]: remove when possible
  llvm::SmallVector<SourceLoc, 16> LocStack;

public:
  explicit SyntaxTransformer(ASTContext &Context) : Context(Context) {}

  IntegerLiteralExpr *transform(syntax::IntegerLiteralExprSyntax &Expr);
  FloatLiteralExpr *transform(syntax::FloatLiteralExprSyntax &Expr);
  NilLiteralExpr *transform(syntax::NilLiteralExprSyntax &Expr);
  BooleanLiteralExpr *transform(syntax::BooleanLiteralExprSyntax &Expr);
  MagicIdentifierLiteralExpr *transform(syntax::PoundFileExprSyntax &Expr);
  MagicIdentifierLiteralExpr *transform(syntax::PoundLineExprSyntax &Expr);
  MagicIdentifierLiteralExpr *transform(syntax::PoundColumnExprSyntax &Expr);
  MagicIdentifierLiteralExpr *transform(syntax::PoundFunctionExprSyntax &Expr);
  MagicIdentifierLiteralExpr *transform(syntax::PoundDsohandleExprSyntax &Expr);
  Expr *transform(syntax::UnknownExprSyntax &Expr);

  /// Stores source location necessary for AST creation.
  void pushLoc(SourceLoc Loc);

  /// Copy a numeric literal value into AST-owned memory, stripping underscores
  /// so the semantic part of the value can be parsed by APInt/APFloat parsers.
  static StringRef copyAndStripUnderscores(StringRef Orig, ASTContext &Context);

private:
  StringRef copyAndStripUnderscores(StringRef Orig);

  SourceLoc topLoc();

  MagicIdentifierLiteralExpr *
  transformMagicIdentifierLiteralExpr(const syntax::TokenSyntax &PoundToken);

  /// Map magic literal tokens such as #file to their MagicIdentifierLiteralExpr
  /// kind.
  static MagicIdentifierLiteralExpr::Kind
  getMagicIdentifierLiteralKind(tok Kind);
};
} // namespace swift

#endif // SWIFT_PARSE_SYNTAXTRANSFORMER_H
