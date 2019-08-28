//===--- ParsedRawSyntaxRecorder.h - Raw Syntax Parsing Recorder ----------===//
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
//
// This file defines the ParsedRawSyntaxRecorder, which is the interface the
// parser is using to pass parsed syntactic elements to a SyntaxParseActions
// receiver and get a ParsedRawSyntaxNode object back.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_PARSE_PARSEDRAWSYNTAXRECORDER_H
#define SWIFT_PARSE_PARSEDRAWSYNTAXRECORDER_H

#include "swift/Basic/LLVM.h"
#include <memory>

namespace swift {

class CharSourceRange;
class ParsedRawSyntaxNode;
struct ParsedTrivia;
class ParsedTriviaPiece;
class SyntaxParseActions;
class SourceLoc;
class Token;
enum class tok;

namespace syntax {
  enum class SyntaxKind;
}

class ParsedRawSyntaxRecorder {
  std::shared_ptr<SyntaxParseActions> SPActions;

public:
  explicit ParsedRawSyntaxRecorder(std::shared_ptr<SyntaxParseActions> spActions)
    : SPActions(std::move(spActions)) {}

  ParsedRawSyntaxNode recordToken(const Token &tok,
                                  const ParsedTrivia &leadingTrivia,
                                  const ParsedTrivia &trailingTrivia);

  ParsedRawSyntaxNode recordToken(tok tokenKind, CharSourceRange tokenRange,
                                  ArrayRef<ParsedTriviaPiece> leadingTrivia,
                                  ArrayRef<ParsedTriviaPiece> trailingTrivia);

  /// Record a missing token. \p loc can be invalid or an approximate location
  /// of where the token would be if not missing.
  ParsedRawSyntaxNode recordMissingToken(tok tokenKind, SourceLoc loc);

  /// The provided \p elements are an exact layout appropriate for the syntax
  /// \p kind. Missing optional elements are represented with a null
  /// ParsedRawSyntaxNode object.
  ParsedRawSyntaxNode recordRawSyntax(syntax::SyntaxKind kind,
                                      MutableArrayRef<ParsedRawSyntaxNode> elements);

  /// Record a raw syntax collecton without eny elements. \p loc can be invalid
  /// or an approximate location of where an element of the collection would be
  /// if not missing.
  ParsedRawSyntaxNode recordEmptyRawSyntaxCollection(syntax::SyntaxKind kind,
                                                     SourceLoc loc);

  void discardRecordedNode(ParsedRawSyntaxNode &node);

  /// Used for incremental re-parsing.
  ParsedRawSyntaxNode lookupNode(size_t lexerOffset, SourceLoc loc,
                                 syntax::SyntaxKind kind);
};

} // end namespace swift

#endif // SWIFT_PARSE_PARSEDRAWSYNTAXRECORDER_H
