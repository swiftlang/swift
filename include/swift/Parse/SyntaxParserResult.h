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

enum class ResultDataKind : uint8_t {
  Success,
  Error,
  CodeCompletion,
};

template <typename ParsedSyntaxNode> class ParsedSyntaxResult {
public:
  template <typename OtherParsedSyntaxNode>
  friend class ParsedSyntaxResult;

private:
  // todo [gsoc]: use some kind of a proper sum type
  llvm::Optional<ParsedSyntaxNode> SuccessNode;
  llvm::Optional<llvm::SmallVector<ParsedSyntax, 0>> ErrorNodes;
  llvm::Optional<llvm::SmallVector<ParsedSyntax, 0>> CodeCompletionNodes;

  ResultDataKind DK;

public:
  explicit ParsedSyntaxResult(ParsedSyntaxNode Node)
      : SuccessNode(Node), DK(ResultDataKind::Success) {}

  ParsedSyntaxResult(ArrayRef<ParsedSyntax> Nodes,
                     ResultDataKind Kind)
      : DK(Kind) {
    switch (DK) {
    case ResultDataKind::Error:
      ErrorNodes.emplace(Nodes.begin(), Nodes.end());
      break;
    case ResultDataKind::CodeCompletion:
      CodeCompletionNodes.emplace(Nodes.begin(), Nodes.end());
      break;
    default:
      llvm_unreachable("success cannot contain multiple nodes");
    }
  }

  ParsedSyntaxResult(const ParsedSyntaxResult &Other) {
    DK = Other.DK;

    switch (DK) {
    case ResultDataKind::Success:
      SuccessNode = Other.SuccessNode;
      break;
    case ResultDataKind::Error:
      ErrorNodes = Other.ErrorNodes;
      break;
    case ResultDataKind::CodeCompletion:
      CodeCompletionNodes = Other.CodeCompletionNodes;
      break;
    }
  }

  template <typename OtherParsedSyntaxNode,
            typename Enable = typename std::enable_if<std::is_base_of<
                ParsedSyntaxNode, OtherParsedSyntaxNode>::value>::type>
  ParsedSyntaxResult(ParsedSyntaxResult<OtherParsedSyntaxNode> Other) {
    DK = Other.DK;

    switch (DK) {
    case ResultDataKind::Success:
      SuccessNode = *Other.SuccessNode;
      break;
    case ResultDataKind::Error:
      ErrorNodes = *Other.ErrorNodes;
      break;
    case ResultDataKind::CodeCompletion:
      CodeCompletionNodes = *Other.CodeCompletionNodes;
      break;
    }
  }

  bool isSuccess() const {
    return DK == ResultDataKind::Success;
  }

  bool isError() const {
    return DK == ResultDataKind::Error;
  }

  bool isCodeCompletion() const {
    return DK == ResultDataKind::CodeCompletion;
  }

  ParsedSyntaxNode getResult() const {
    assert(isSuccess() && "unsuccessful parse doesn't have any result");
    return *SuccessNode;
  }

  ArrayRef<ParsedSyntax> getUnknownNodes() const {
    assert(!isSuccess() && "successful parse doesn't contain unknown nodes");
    switch (DK) {
    case ResultDataKind::Error:
      return *ErrorNodes;
    case ResultDataKind::CodeCompletion:
      return *CodeCompletionNodes;
    default:
      llvm_unreachable("cannot get here");
    }
  }
  
  ParserStatus getStatus() const {
    ParserStatus S;
    if (isError())
      S.setIsParseError();
    if (isCodeCompletion())
      S.setHasCodeCompletion();
    return S;
  }
};

template <typename ParsedSyntaxNode>
static ParsedSyntaxResult<ParsedSyntaxNode>
makeParsedSuccess(ParsedSyntaxNode Node) {
  return ParsedSyntaxResult<ParsedSyntaxNode>(Node);
}

template <typename ParsedSyntaxNode>
static ParsedSyntaxResult<ParsedSyntaxNode>
makeParsedError(ArrayRef<ParsedSyntax> Nodes) {
  return ParsedSyntaxResult<ParsedSyntaxNode>(Nodes, ResultDataKind::Error);
}

template <typename ParsedSyntaxNode>
static ParsedSyntaxResult<ParsedSyntaxNode> makeParsedErrorEmpty() {
  return ParsedSyntaxResult<ParsedSyntaxNode>({}, ResultDataKind::Error);
}

template <typename ParsedSyntaxNode>
static ParsedSyntaxResult<ParsedSyntaxNode>
makeParsedCodeCompletion(ArrayRef<ParsedSyntax> Nodes) {
  return ParsedSyntaxResult<ParsedSyntaxNode>(Nodes,
                                              ResultDataKind::CodeCompletion);
}

template <typename ParsedSyntaxNode>
static ParsedSyntaxResult<ParsedSyntaxNode>
makeParsedResult(ArrayRef<ParsedSyntax> Nodes,
                 ParserStatus Status) {
  return Status.hasCodeCompletion()
             ? makeParsedCodeCompletion<ParsedSyntaxNode>(Nodes)
             : makeParsedError<ParsedSyntaxNode>(Nodes);
}

template <typename Syntax, typename AST> class SyntaxParserResult {
  llvm::Optional<Syntax> SyntaxNode;
  ParserResult<AST> ASTResult;

  template <typename T, typename U> friend class SyntaxParserResult;

public:
  SyntaxParserResult(std::nullptr_t = nullptr)
      : SyntaxNode(None), ASTResult(nullptr) {}
  SyntaxParserResult(ParserStatus Status)
      : SyntaxNode(None), ASTResult(Status) {}
  SyntaxParserResult(llvm::Optional<Syntax> SyntaxNode, AST *ASTNode)
      : SyntaxNode(SyntaxNode), ASTResult(ASTNode) {}
  SyntaxParserResult(ParserStatus Status, llvm::Optional<Syntax> SyntaxNode,
                     AST *ASTNode)
      : SyntaxNode(SyntaxNode), ASTResult(makeParserResult(Status, ASTNode)) {}

  /// Convert from a different but compatible parser result.
  template <typename U, typename Enabler = typename std::enable_if<
                            std::is_base_of<AST, U>::value>::type>
  SyntaxParserResult(SyntaxParserResult<Syntax, U> Other)
      : SyntaxNode(Other.SyntaxNode), ASTResult(Other.ASTResult) {}


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

  Syntax getSyntax() const {
    assert(SyntaxNode.hasValue() && "getSyntax from None value");
    return *SyntaxNode;
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
makeSyntaxResult(llvm::Optional<Syntax> SyntaxNode, AST *ASTNode) {
  return SyntaxParserResult<Syntax, AST>(SyntaxNode, ASTNode);
}

/// Create a result with the specified status.
template <typename Syntax, typename AST>
static inline SyntaxParserResult<Syntax, AST>
makeSyntaxResult(ParserStatus Status, llvm::Optional<Syntax> SyntaxNode,
                 AST *ASTNode) {
  return SyntaxParserResult<Syntax, AST>(Status, SyntaxNode, ASTNode);
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
