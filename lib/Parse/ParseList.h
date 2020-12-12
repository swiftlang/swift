//===--- ParseList.h ------------------------------------------------------===//
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

#ifndef SWIFT_PARSE_PARSELIST_H
#define SWIFT_PARSE_PARSELIST_H

#include "swift/Parse/Parser.h"
#include "swift/Parse/ParserResult.h"

namespace swift {

/// Parse a comma-separated list of something.
template <typename ParsedNode>
inline ParserStatus Parser::parseListSyntax(
    SmallVectorImpl<ParsedNode> &Elements, bool AllowEmpty,
    bool AllowSepAfterLast, llvm::function_ref<bool()> IsAtCloseTok,
    llvm::function_ref<ParserStatus(typename ParsedNode::Builder &)> Callback) {
  // FIXME: Handling of string interpolation EOF once it's needed
  ParserStatus status;

  auto isAtTerminator = [&]() -> bool {
    return Tok.isAny(tok::eof, tok::pound_endif, tok::pound_else,
                     tok::pound_elseif) ||
           (Tok.isAtStartOfLine() &&
            (Tok.is(tok::r_brace) || isStartOfSwiftDecl() || isStartOfStmt()));
  };

  bool isTerminated = AllowEmpty && IsAtCloseTok();

  while (!isTerminated) {
    typename ParsedNode::Builder elemBuilder(*SyntaxContext);

    // Parse the element.
    status |= Callback(elemBuilder);

    if (status.isError()) {
      // Recover by skipping to ',' or close bracket.
      while (!Tok.is(tok::comma) && !IsAtCloseTok() && !isAtTerminator()) {
        ignoreSingle(/*Collect=*/nullptr);
      }
    }

    // Parse ','.
    auto hasComma = Tok.is(tok::comma);
    if (hasComma) {
      elemBuilder.useTrailingComma(consumeTokenSyntax(tok::comma));
    }

    // Store the element to the list.
    Elements.emplace_back(elemBuilder.build());

    // Check to see if there's close bracket.
    isTerminated = IsAtCloseTok();

    // If we found EOF or such without seeing ',' or close bracket, terminate.
    if (isAtTerminator() && !isTerminated && !hasComma) {
      checkForInputIncomplete();
      isTerminated = true;
    }

    if (isTerminated) {
      // Complain about the trailing comma at the last element.
      if (hasComma && !AllowSepAfterLast) {
        diagnose(Tok, diag::unexpected_separator, ",")
            .fixItRemove(SourceRange(PreviousLoc));
      }
    } else {
      // Complain about the missing ','.
      if (!hasComma) {
        diagnose(Tok, diag::expected_separator, ",")
            .fixItInsertAfter(PreviousLoc, ",");
        status.setIsParseError();
      }
    }
  }
  return status;
}

} // namespace swift
#endif
