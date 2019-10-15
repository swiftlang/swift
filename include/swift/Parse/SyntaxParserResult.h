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

#ifndef SWIFT_PARSE_SYNTAXPARSERRESULT_H
#define SWIFT_PARSE_SYNTAXPARSERRESULT_H

#include "llvm/ADT/Optional.h"
#include "swift/Parse/ParserResult.h"

namespace swift {

template <typename Syntax, typename AST> class SyntaxParserResult {
  llvm::Optional<Syntax> SyntaxNode;
  ParserResult<AST> ASTResult;

  template <typename T, typename U> friend class SyntaxParserResult;

public:
  SyntaxParserResult(std::nullptr_t = nullptr)
      : SyntaxNode(None), ASTResult(nullptr) {}
  SyntaxParserResult(ParserStatus Status)
      : SyntaxNode(None), ASTResult(Status) {}
  SyntaxParserResult(llvm::Optional<Syntax> &&SyntaxNode, AST *ASTNode)
      : SyntaxNode(std::move(SyntaxNode)), ASTResult(ASTNode) {}
  SyntaxParserResult(ParserStatus Status, llvm::Optional<Syntax> &&SyntaxNode,
                     AST *ASTNode)
      : SyntaxNode(std::move(SyntaxNode)), ASTResult(makeParserResult(Status, ASTNode)) {}

  /// Convert from a different but compatible parser result.
  template <typename U, typename Enabler = typename std::enable_if<
                            std::is_base_of<AST, U>::value>::type>
  SyntaxParserResult(SyntaxParserResult<Syntax, U> &&Other)
      : SyntaxNode(std::move(Other.SyntaxNode)), ASTResult(Other.ASTResult) {}


  bool isNull() const { return ASTResult.isNull(); }
  bool isNonNull() const { return ASTResult.isNonNull(); }
  bool isParseError() const { return ASTResult.isParseError(); }
  bool hasCodeCompletion() const { return ASTResult.hasCodeCompletion(); }

  void setIsParseError() { return ASTResult.setIsParserError(); }
  void setHasCodeCompletion() { return ASTResult.setHasCodeCompletion(); }

  const ParserResult<AST> &getASTResult() { return ASTResult; }

  AST *getAST() const { return ASTResult.get(); }

  bool hasSyntax() const {
    return SyntaxNode.hasValue();
  }

  Syntax getSyntax() {
    assert(SyntaxNode.hasValue() && "getSyntax from None value");
    return std::move(*SyntaxNode);
  }

  SyntaxParserResult<Syntax, AST> &
  operator=(SyntaxParserResult<Syntax, AST> R){
    std::swap(*this, R);
    return *this;
  };
};

/// Create a successful parser result.
template <typename Syntax, typename AST>
static inline SyntaxParserResult<Syntax, AST>
makeSyntaxResult(llvm::Optional<Syntax> &&SyntaxNode, AST *ASTNode) {
  return SyntaxParserResult<Syntax, AST>(std::move(SyntaxNode), ASTNode);
}

/// Create a result with the specified status.
template <typename Syntax, typename AST>
static inline SyntaxParserResult<Syntax, AST>
makeSyntaxResult(ParserStatus Status, llvm::Optional<Syntax> &&SyntaxNode,
                 AST *ASTNode) {
  return SyntaxParserResult<Syntax, AST>(Status, std::move(SyntaxNode), ASTNode);
}

/// Create a result (null or non-null) with error and code completion bits set.
template <typename Syntax, typename AST>
static inline SyntaxParserResult<Syntax, AST>
makeSyntaxCodeCompletionResult(AST *Result = nullptr) {
  SyntaxParserResult<Syntax, AST> SR;
  if (Result)
    SR = SyntaxParserResult<Syntax, AST>(None, Result);
  SR.setHasCodeCompletion();
  return SR;
}

} // namespace swift

#endif
