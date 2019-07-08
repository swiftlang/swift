//===--- ASTGen.h ---------------------------------------------------------===//
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

#ifndef SWIFT_PARSE_ASTGEN_H
#define SWIFT_PARSE_ASTGEN_H

#include "swift/AST/ASTContext.h"
#include "swift/AST/Expr.h"
#include "swift/Syntax/SyntaxNodes.h"
#include "llvm/ADT/DenseMap.h"

namespace swift {
/// Generates AST nodes from Syntax nodes.
class ASTGen {
  ASTContext &Context;
  // A stack of source locations of syntax constructs. Allows us to get the
  // SourceLoc necessary to create AST nodes for nodes in not-yet-complete
  // Syntax tree. The topmost item should always correspond to the token/node
  // that has been parsed/transformed most recently.
  // todo [gsoc]: remove when possible
  llvm::SmallVector<SourceLoc, 16> LocStack;

public:
  explicit ASTGen(ASTContext &Context) : Context(Context) {}

  IntegerLiteralExpr *generate(syntax::IntegerLiteralExprSyntax &Expr);
  FloatLiteralExpr *generate(syntax::FloatLiteralExprSyntax &Expr);
  NilLiteralExpr *generate(syntax::NilLiteralExprSyntax &Expr);
  BooleanLiteralExpr *generate(syntax::BooleanLiteralExprSyntax &Expr);
  MagicIdentifierLiteralExpr *generate(syntax::PoundFileExprSyntax &Expr);
  MagicIdentifierLiteralExpr *generate(syntax::PoundLineExprSyntax &Expr);
  MagicIdentifierLiteralExpr *generate(syntax::PoundColumnExprSyntax &Expr);
  MagicIdentifierLiteralExpr *generate(syntax::PoundFunctionExprSyntax &Expr);
  MagicIdentifierLiteralExpr *generate(syntax::PoundDsohandleExprSyntax &Expr);
  Expr *generate(syntax::UnknownExprSyntax &Expr);

  /// Stores source location necessary for AST creation.
  void pushLoc(SourceLoc Loc);

  /// Copy a numeric literal value into AST-owned memory, stripping underscores
  /// so the semantic part of the value can be parsed by APInt/APFloat parsers.
  static StringRef copyAndStripUnderscores(StringRef Orig, ASTContext &Context);

private:
  StringRef copyAndStripUnderscores(StringRef Orig);

  SourceLoc topLoc();

  MagicIdentifierLiteralExpr *
  generateMagicIdentifierLiteralExpr(const syntax::TokenSyntax &PoundToken);

  /// Map magic literal tokens such as #file to their MagicIdentifierLiteralExpr
  /// kind.
  static MagicIdentifierLiteralExpr::Kind
  getMagicIdentifierLiteralKind(tok Kind);
};
} // namespace swift

#endif // SWIFT_PARSE_ASTGEN_H
