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
  ParsedRawSyntaxNode Raw;
  ParserStatus Status;

public:
  explicit ParsedSyntaxResult() : Raw(), Status() { setIsError(); }

  ParsedSyntaxResult(ParserStatus Status) : Raw(), Status(Status) {
    assert(Status.isError());
  }

  explicit ParsedSyntaxResult(ParsedRawSyntaxNode Raw)
      : Raw(Raw), Status() {}

  explicit ParsedSyntaxResult(ParsedSyntaxNode Node)
      : ParsedSyntaxResult(Node.getRaw()) {}

  template <typename OtherParsedSyntaxNode,
            typename Enable = typename std::enable_if<std::is_base_of<
                ParsedSyntaxNode, OtherParsedSyntaxNode>::value>::type>
  ParsedSyntaxResult(ParsedSyntaxResult<OtherParsedSyntaxNode> other) {
    Raw = other.Raw;
    Status = other.Status;
  }

  bool isSuccess() const {
    return Status.isSuccess();
  }

  bool isError() const {
    return Status.isError();
  }
  void setIsError() {
    Status.setIsParseError();
  }

  bool hasCodeCompletion() const {
    return Status.hasCodeCompletion();
  }
  void setHasCodeCompletion() {
    Status.setHasCodeCompletion();
  }

  ParsedSyntaxNode get() const {
    assert(!isNull());
    return ParsedSyntaxNode(Raw);
  }
  Optional<ParsedSyntaxNode> getOrNull() const {
    if (isNull())
      return None;
    return get();
  }

  bool isNull() const {
    return Raw.isNull();
  }

  ParserStatus getStatus() const {
    return Status;
  }
};

template <typename ParsedSyntaxNode>
static ParsedSyntaxResult<ParsedSyntaxNode>
makeParsedResult(ParsedSyntaxNode node) {
  return ParsedSyntaxResult<ParsedSyntaxNode>(node);
}

template <typename ParsedSyntaxNode>
static ParsedSyntaxResult<ParsedSyntaxNode>
makeParsedError(ParsedSyntaxNode node) {
  auto result = ParsedSyntaxResult<ParsedSyntaxNode>(node);
  result.setIsError();
  return result;
}

template <typename ParsedSyntaxNode>
static ParsedSyntaxResult<ParsedSyntaxNode> makeParsedError() {
  return ParsedSyntaxResult<ParsedSyntaxNode>();
}

template <typename ParsedSyntaxNode>
static ParsedSyntaxResult<ParsedSyntaxNode>
makeParsedCodeCompletion(ParsedSyntaxNode node) {
  auto result = ParsedSyntaxResult<ParsedSyntaxNode>(node);
  result.setHasCodeCompletion();
  return result;
}

template <typename ParsedSyntaxNode>
static ParsedSyntaxResult<ParsedSyntaxNode>
makeParsedResult(ParsedSyntaxNode node, ParserStatus Status) {
  auto result = ParsedSyntaxResult<ParsedSyntaxNode>(node);
  if (Status.hasCodeCompletion())
    result.setHasCodeCompletion();
  else if (Status.isError())
    result.setIsError();
  return result;
}

} // namespace swift

#endif
