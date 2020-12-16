//===--- ParsedSyntaxResult.h -----------------------------------*- C++ -*-===//
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

#ifndef SWIFT_PARSE_SYNTAXPARSERRESULT_H
#define SWIFT_PARSE_SYNTAXPARSERRESULT_H

#include "swift/Parse/ParsedRawSyntaxNode.h"
#include "swift/Parse/ParserResult.h"
#include "llvm/ADT/Optional.h"

namespace swift {

enum class ResultDataKind : uint8_t {
  Success,
  Error,
  CodeCompletion,
};

template <typename ParsedSyntaxNode> class ParsedSyntaxResult {
public:
  template <typename OtherParsedSyntaxNode> friend class ParsedSyntaxResult;

private:
  ParsedRawSyntaxNode Raw;
  ParserStatus Status;

public:
  explicit ParsedSyntaxResult() : Raw(), Status() { setIsError(); }

  ParsedSyntaxResult(ParserStatus Status) : Raw(), Status(Status) {
    assert(Status.isError());
  }

  ParsedSyntaxResult(ParsedSyntaxNode &&Node, ParserStatus Status) : Raw(Node.takeRaw()), Status(Status) {}

  explicit ParsedSyntaxResult(ParsedRawSyntaxNode &&Raw)
      : Raw(std::move(Raw)), Status() {}

  explicit ParsedSyntaxResult(ParsedSyntaxNode &&Node)
      : ParsedSyntaxResult(Node.takeRaw()) {}

  template <typename OtherParsedSyntaxNode,
            typename Enable = typename std::enable_if<std::is_base_of<
                ParsedSyntaxNode, OtherParsedSyntaxNode>::value>::type>
  ParsedSyntaxResult(ParsedSyntaxResult<OtherParsedSyntaxNode> &&other) {
    Raw = std::move(other.Raw);
    Status = std::move(other.Status);
  }

  bool isSuccess() const { return Status.isSuccess(); }

  bool isError() const { return Status.isError(); }
  void setIsError() { Status.setIsParseError(); }

  bool hasCodeCompletion() const { return Status.hasCodeCompletion(); }
  void setHasCodeCompletionAndIsError() {
    Status.setHasCodeCompletionAndIsError();
  }

  ParsedSyntaxNode get() {
    assert(!isNull());
    return ParsedSyntaxNode(std::move(Raw));
  }

  template <typename NewSyntaxNode> Optional<NewSyntaxNode> getAs() {
    assert(!isNull());
    if (NewSyntaxNode::kindof(Raw.getKind()))
      return NewSyntaxNode(std::move(Raw));
    return None;
  }

  Optional<ParsedSyntaxNode> getOrNull() {
    if (isNull())
      return None;
    return get();
  }

  bool isNull() const { return Raw.isNull(); }

  ParserStatus getStatus() const { return Status; }
};

template <typename ParsedSyntaxNode>
static ParsedSyntaxResult<ParsedSyntaxNode>
makeParsedResult(ParsedSyntaxNode node) {
  return ParsedSyntaxResult<ParsedSyntaxNode>(std::move(node));
}

template <typename ParsedSyntaxNode>
static ParsedSyntaxResult<ParsedSyntaxNode>
makeParsedError(ParsedSyntaxNode node) {
  auto result = ParsedSyntaxResult<ParsedSyntaxNode>(std::move(node));
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
  auto result = ParsedSyntaxResult<ParsedSyntaxNode>(std::move(node));
  result.setHasCodeCompletionAndIsError();
  return result;
}

template <typename ParsedSyntaxNode>
static ParsedSyntaxResult<ParsedSyntaxNode>
makeParsedResult(ParsedSyntaxNode node, ParserStatus Status) {
  return ParsedSyntaxResult<ParsedSyntaxNode>(std::move(node), Status);
}

template <typename ParsedSyntaxNode>
static ParsedSyntaxResult<ParsedSyntaxNode>
makeParsedResult(Optional<ParsedSyntaxNode> Node, ParserStatus Status) {
  if (Node) {
    return makeParsedResult(std::move(*Node), Status);
  } else {
    return Status;
  }
}

} // namespace swift

#endif
