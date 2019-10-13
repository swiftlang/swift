//===--- ParseList.h ------------------------------------------------------===//
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

#ifndef SWIFT_PARSE_PARSELIST_H
#define SWIFT_PARSE_PARSELIST_H

#include "swift/Parse/Parser.h"
#include "swift/Parse/ParserResult.h"

namespace swift {

/// Parse a comma-separated list of something.
template <typename ParsedNode>
inline ParserStatus Parser::parseListSyntax(
    SmallVectorImpl<ParsedNode> &elements, bool AllowEmpty,
    bool AllowSepAfterLast, llvm::function_ref<bool()> isAtCloseTok,
    llvm::function_ref<ParserStatus(typename ParsedNode::Builder &)> callback) {
  ParserStatus status;

  auto isAtTerminator = [&]() -> bool {
    return Tok.isAny(tok::eof, tok::pound_endif, tok::pound_else,
                     tok::pound_elseif) ||
           (Tok.isAtStartOfLine() &&
            (Tok.is(tok::r_brace) || isStartOfDecl() || isStartOfStmt()));
  };

  bool isTerminated = AllowEmpty && isAtCloseTok();

  while (!isTerminated) {
    typename ParsedNode::Builder elemBuilder(*SyntaxContext);

    // Parse the element.
    status |= callback(elemBuilder);

    if (status.isError()) {
      // Recover by skipping to ',' or close bracket.
      while (!Tok.is(tok::comma) && !isAtCloseTok() && !isAtTerminator())
        ignoreSingle();
    }

    // Parse ','.
    auto hasComma = Tok.is(tok::comma);
    if (hasComma)
      elemBuilder.useTrailingComma(consumeTokenSyntax(tok::comma));

    // Store the element to the list.
    elements.emplace_back(elemBuilder.build());

    // Check to see if there's close bracket.
    isTerminated = isAtCloseTok();

    // If we found EOF or such without seeing ',' or close bracket, terminate.
    if (!isTerminated && !hasComma) {
      if (isAtTerminator()) {
        checkForInputIncomplete();
        isTerminated = true;
      }
    }

    if (isTerminated) {
      // Complain about the trailing comma at the last element.
      if (hasComma && !AllowSepAfterLast)
        diagnose(Tok, diag::unexpected_separator, ",")
            .fixItRemove(SourceRange(PreviousLoc));
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
