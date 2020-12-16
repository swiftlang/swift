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

using namespace swift::syntax;

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
  Expr *generate(const BooleanLiteralExprSyntax &Expr, const SourceLoc &Loc);
  Expr *generate(const FloatLiteralExprSyntax &Expr, const SourceLoc &Loc);
  Expr *generate(const IntegerLiteralExprSyntax &Expr, const SourceLoc &Loc);
  Expr *generate(const NilLiteralExprSyntax &Expr, const SourceLoc &Loc);
  Expr *generate(const PoundColumnExprSyntax &Expr, const SourceLoc &Loc);
  Expr *generate(const PoundDsohandleExprSyntax &Expr, const SourceLoc &Loc);
  Expr *generate(const PoundFileExprSyntax &Expr, const SourceLoc &Loc);
  Expr *generate(const PoundFileIDExprSyntax &Expr, const SourceLoc &Loc);
  Expr *generate(const PoundFilePathExprSyntax &Expr, const SourceLoc &Loc);
  Expr *generate(const PoundLineExprSyntax &Expr, const SourceLoc &Loc);
  Expr *generate(const PoundFunctionExprSyntax &Expr, const SourceLoc &Loc);
  Expr *generate(const UnknownExprSyntax &Expr, const SourceLoc &Loc);

private:
  /// Map magic literal tokens such as #file to their MagicIdentifierLiteralExpr
  /// kind.
  MagicIdentifierLiteralExpr::Kind getMagicIdentifierLiteralKind(tok Kind);

  Expr *
  generateMagicIdentifierLiteralExpr(const TokenSyntax &PoundToken,
                                     const SourceLoc &Loc);

  //===--------------------------------------------------------------------===//
  // MARK: - Other
public:
  /// Copy a numeric literal value into AST-owned memory, stripping underscores
  /// so the semantic part of the value can be parsed by APInt/APFloat parsers.
  static StringRef copyAndStripUnderscores(StringRef Orig, ASTContext &Context);

private:
  StringRef copyAndStripUnderscores(StringRef Orig);

  static SourceLoc advanceLocBegin(const SourceLoc &Loc, const Syntax &Node);
  static SourceLoc advanceLocEnd(const SourceLoc &Loc, const TokenSyntax &Token);
  static SourceLoc advanceLocAfter(const SourceLoc &Loc, const Syntax &Node);
};
} // namespace swift

#endif // SWIFT_PARSE_ASTGEN_H
