//===--- SyntaxParserResult.h - Syntax Parser Result Wrapper ---*- C++ -*-===//
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

#include "llvm/ADT/Optional.h"
#include "swift/Parse/ParserResult.h"

namespace swift {

template <typename Syntax, typename AST> class SyntaxParserResult {
  llvm::Optional<Syntax> SyntaxNode;
  ParserResult<AST> ASTResult;

public:
  SyntaxParserResult(std::nullptr_t = nullptr)
      : SyntaxNode(None), ASTResult(nullptr) {}
  SyntaxParserResult(llvm::Optional<Syntax> SyntaxNode, AST *ASTNode)
      : SyntaxNode(SyntaxNode), ASTResult(ASTNode) {}

  bool isNull() const { return ASTResult.isNull(); }
  bool isNonNull() const { return ASTResult.isNonNull(); }
  bool isParseError() const { return ASTResult.isParseError(); }
  bool hasCodeCompletion() const { return ASTResult.hasCodeCompletion(); }

  AST *getAST() const { return ASTResult.get(); }

  bool hasSyntax() const {
    return SyntaxNode.hasValue();
  }

  Syntax getSyntax() const {
    assert(SyntaxNode.hasValue() && "getSyntax from None value");
    return *SyntaxNode;
  }
};

/// Create a successful parser result.
template <typename Syntax, typename AST>
static inline SyntaxParserResult<Syntax, AST>
makeSyntaxResult(llvm::Optional<Syntax> SyntaxNode, AST *ASTNode) {
  return SyntaxParserResult<Syntax, AST>(SyntaxNode, ASTNode);
}

} // namespace swift
