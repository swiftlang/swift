//===--- ParsedRawSyntaxNode.h - Parsed Raw Syntax Node ---------*- C++ -*-===//
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

#ifndef SWIFT_PARSE_PARSEDRAWSYNTAXNODE_H
#define SWIFT_PARSE_PARSEDRAWSYNTAXNODE_H

#include "swift/Basic/Debug.h"
#include "swift/Basic/SourceLoc.h"
#include "swift/Parse/ParsedTrivia.h"
#include "swift/Parse/Token.h"
#include "swift/Syntax/SyntaxKind.h"
#include <vector>

namespace swift {

typedef void *OpaqueSyntaxNode;
class SyntaxParsingContext;

/// Represents a raw syntax node formed by the parser.
///
/// It can be either 'recorded', in which case it encapsulates an
/// \c OpaqueSyntaxNode that was returned from a \c SyntaxParseActions
/// invocation, or 'deferred' which captures the data for a
/// \c SyntaxParseActions invocation to occur later.
///
/// An \c OpaqueSyntaxNode can represent both the result of 'recording' a token
/// as well as 'recording' a syntax layout, so there's only one
/// \c RecordedSyntaxNode structure that can represent both.
///
/// The 'deferred' form is used for when the parser is backtracking and when
/// there are instances that it's not clear what will be the final syntax node
/// in the current parsing context.
class ParsedRawSyntaxNode {
  enum class DataKind: uint8_t {
    Null,
    Recorded,
    DeferredLayout,
    DeferredToken,
  };

  struct RecordedSyntaxNode {
    OpaqueSyntaxNode OpaqueNode;
    CharSourceRange Range;
  };
  struct DeferredLayoutNode {
    MutableArrayRef<ParsedRawSyntaxNode> Children;
    CharSourceRange Range;
  };
  struct DeferredTokenNode {
    const ParsedTriviaPiece *TriviaPieces;
    SourceLoc TokLoc;
    unsigned TokLength;
    uint16_t NumLeadingTrivia;
    uint16_t NumTrailingTrivia;
  };

  union {
    RecordedSyntaxNode RecordedData;
    DeferredLayoutNode DeferredLayout;
    DeferredTokenNode DeferredToken;
  };
  uint16_t SynKind;
  uint16_t TokKind;
  DataKind DK;
  /// Primary used for capturing a deferred missing token.
  bool IsMissing = false;

  ParsedRawSyntaxNode(syntax::SyntaxKind k, CharSourceRange r,
                      MutableArrayRef<ParsedRawSyntaxNode> deferredNodes)
    : DeferredLayout({deferredNodes, r}),
      SynKind(uint16_t(k)), TokKind(uint16_t(tok::unknown)),
      DK(DataKind::DeferredLayout) {
    assert(getKind() == k && "Syntax kind with too large value!");
  }

  ParsedRawSyntaxNode(tok tokKind, SourceLoc tokLoc, unsigned tokLength,
                      const ParsedTriviaPiece *triviaPieces,
                      unsigned numLeadingTrivia,
                      unsigned numTrailingTrivia)
    : DeferredToken{triviaPieces,
                    tokLoc, tokLength,
                    uint16_t(numLeadingTrivia),
                    uint16_t(numTrailingTrivia)},
      SynKind(uint16_t(syntax::SyntaxKind::Token)),
      TokKind(uint16_t(tokKind)),
      DK(DataKind::DeferredToken) {
    assert(getTokenKind() == tokKind && "Token kind is too large value!");
    assert(DeferredToken.NumLeadingTrivia == numLeadingTrivia &&
           "numLeadingTrivia is too large value!");
    assert(DeferredToken.NumTrailingTrivia == numTrailingTrivia &&
           "numLeadingTrivia is too large value!");
  }
  ParsedRawSyntaxNode(ParsedRawSyntaxNode &other) = delete;
  ParsedRawSyntaxNode &operator=(ParsedRawSyntaxNode &other) = delete;

public:
  ParsedRawSyntaxNode()
    : RecordedData{},
      SynKind(uint16_t(syntax::SyntaxKind::Unknown)),
      TokKind(uint16_t(tok::unknown)),
      DK(DataKind::Null) {
  }

  ParsedRawSyntaxNode(syntax::SyntaxKind k, tok tokKind,
                      CharSourceRange r, OpaqueSyntaxNode n,
                      bool IsMissing = false)
    : RecordedData{n, r},
      SynKind(uint16_t(k)), TokKind(uint16_t(tokKind)),
      DK(DataKind::Recorded),
      IsMissing(IsMissing) {
    assert(getKind() == k && "Syntax kind with too large value!");
    assert(getTokenKind() == tokKind && "Token kind with too large value!");
  }

#ifndef NDEBUG
  bool ensureDataIsNotRecorded() {
    if (DK != DataKind::Recorded)
      return true;
    llvm::dbgs() << "Leaking node: ";
    dump(llvm::dbgs());
    llvm::dbgs() << "\n";
    return false;
  }
#endif

  ParsedRawSyntaxNode &operator=(ParsedRawSyntaxNode &&other) {
    assert(ensureDataIsNotRecorded() &&
           "recorded data is being destroyed by assignment");
    switch (other.DK) {
    case DataKind::Null:
      break;
    case DataKind::Recorded:
      RecordedData = std::move(other.RecordedData);
      break;
    case DataKind::DeferredLayout:
      DeferredLayout = std::move(other.DeferredLayout);
      break;
    case DataKind::DeferredToken:
      DeferredToken = std::move(other.DeferredToken);
      break;
    }
    SynKind = std::move(other.SynKind);
    TokKind = std::move(other.TokKind);
    DK = std::move(other.DK);
    IsMissing = std::move(other.IsMissing);
    other.reset();
    return *this;
  }
  ParsedRawSyntaxNode(ParsedRawSyntaxNode &&other) : ParsedRawSyntaxNode() {
    *this = std::move(other);
  }
  ~ParsedRawSyntaxNode() {
    assert(ensureDataIsNotRecorded() && "recorded data is being destructed");
  }

  syntax::SyntaxKind getKind() const { return syntax::SyntaxKind(SynKind); }
  tok getTokenKind() const { return tok(TokKind); }

  bool isToken() const {
    return getKind() == syntax::SyntaxKind::Token;
  }
  bool isToken(tok tokKind) const {
    return getTokenKind() == tokKind;
  }

  bool isNull() const {
    return DK == DataKind::Null;
  }

  bool isRecorded() const { return DK == DataKind::Recorded; }
  bool isDeferredLayout() const { return DK == DataKind::DeferredLayout; }
  bool isDeferredToken() const { return DK == DataKind::DeferredToken; }

  /// Primary used for a deferred missing token.
  bool isMissing() const { return IsMissing; }

  void reset() {
    RecordedData = {};
    SynKind = uint16_t(syntax::SyntaxKind::Unknown);
    TokKind = uint16_t(tok::unknown);
    DK = DataKind::Null;
    IsMissing = false;
  }

  ParsedRawSyntaxNode unsafeCopy() const {
    ParsedRawSyntaxNode copy;
    switch (DK) {
    case DataKind::DeferredLayout:
      copy.DeferredLayout = DeferredLayout;
      break;
    case DataKind::DeferredToken:
      copy.DeferredToken = DeferredToken;
      break;
    case DataKind::Recorded:
      copy.RecordedData = RecordedData;
      break;
    case DataKind::Null:
      break;
    }
    copy.SynKind = SynKind;
    copy.TokKind = TokKind;
    copy.DK = DK;
    copy.IsMissing = IsMissing;
    return copy;
  }

  CharSourceRange getDeferredRange() const {
    switch (DK) {
    case DataKind::DeferredLayout:
      return getDeferredLayoutRange();
    case DataKind::DeferredToken:
      return getDeferredTokenRangeWithTrivia();
    default:
      llvm_unreachable("node not deferred");
    }
  }

  // Recorded Data ===========================================================//

  CharSourceRange getRecordedRange() const {
    assert(isRecorded());
    return RecordedData.Range;
  }
  const OpaqueSyntaxNode &getOpaqueNode() const {
    assert(isRecorded());
    return RecordedData.OpaqueNode;
  }
  OpaqueSyntaxNode takeOpaqueNode() {
    assert(isRecorded());
    auto opaque = RecordedData.OpaqueNode;
    reset();
    return opaque;
  }

  // Deferred Layout Data ====================================================//

  CharSourceRange getDeferredLayoutRange() const {
    assert(DK == DataKind::DeferredLayout);
    return DeferredLayout.Range;
  }
  ArrayRef<ParsedRawSyntaxNode> getDeferredChildren() const {
    assert(DK == DataKind::DeferredLayout);
    return DeferredLayout.Children;
  }

  MutableArrayRef<ParsedRawSyntaxNode> getDeferredChildren() {
    assert(DK == DataKind::DeferredLayout);
    return DeferredLayout.Children;
  }

  ParsedRawSyntaxNode copyDeferred() const {
    ParsedRawSyntaxNode copy;
    switch (DK) {
    case DataKind::DeferredLayout:
      copy.DeferredLayout = DeferredLayout;
      break;
    case DataKind::DeferredToken:
      copy.DeferredToken = DeferredToken;
      break;
    default:
      llvm_unreachable("node not deferred");
    }
    copy.SynKind = SynKind;
    copy.TokKind = TokKind;
    copy.DK = DK;
    copy.IsMissing = IsMissing;
    return copy;
  }

  // Deferred Token Data =====================================================//

  CharSourceRange getDeferredTokenRangeWithTrivia() const {
    assert(DK == DataKind::DeferredToken);
    auto leadTriviaPieces = getDeferredLeadingTriviaPieces();
    auto trailTriviaPieces = getDeferredTrailingTriviaPieces();

    auto leadTriviaLen = ParsedTriviaPiece::getTotalLength(leadTriviaPieces);
    auto trailTriviaLen = ParsedTriviaPiece::getTotalLength(trailTriviaPieces);

    SourceLoc begin = DeferredToken.TokLoc.getAdvancedLoc(-leadTriviaLen);
    unsigned len = leadTriviaLen + DeferredToken.TokLength + trailTriviaLen;

    return CharSourceRange{begin, len};
  }
  CharSourceRange getDeferredTokenRange() const {
    assert(DK == DataKind::DeferredToken);
    return CharSourceRange{DeferredToken.TokLoc, DeferredToken.TokLength};
  }
  ArrayRef<ParsedTriviaPiece> getDeferredLeadingTriviaPieces() const {
    assert(DK == DataKind::DeferredToken);
    return ArrayRef<ParsedTriviaPiece>(DeferredToken.TriviaPieces,
                                       DeferredToken.NumLeadingTrivia);
  }
  ArrayRef<ParsedTriviaPiece> getDeferredTrailingTriviaPieces() const {
    assert(DK == DataKind::DeferredToken);
    return ArrayRef<ParsedTriviaPiece>(
      DeferredToken.TriviaPieces + DeferredToken.NumLeadingTrivia,
      DeferredToken.NumTrailingTrivia);
  }

  //==========================================================================//

  /// Form a deferred syntax layout node.
  static ParsedRawSyntaxNode makeDeferred(syntax::SyntaxKind k,
                        MutableArrayRef<ParsedRawSyntaxNode> deferredNodes,
                                          SyntaxParsingContext &ctx);

  /// Form a deferred token node.
  static ParsedRawSyntaxNode makeDeferred(Token tok,
                                          const ParsedTrivia &leadingTrivia,
                                          const ParsedTrivia &trailingTrivia,
                                          SyntaxParsingContext &ctx);

  /// Form a deferred missing token node.
  static ParsedRawSyntaxNode makeDeferredMissing(tok tokKind, SourceLoc loc) {
    auto raw = ParsedRawSyntaxNode(tokKind, loc, 0, nullptr, 0, 0);
    raw.IsMissing = true;
    return raw;
  }

  /// Dump this piece of syntax recursively for debugging or testing.
  SWIFT_DEBUG_DUMP;

  /// Dump this piece of syntax recursively.
  void dump(raw_ostream &OS, unsigned Indent = 0) const;

  static ParsedRawSyntaxNode null() {
    return ParsedRawSyntaxNode{};
  }
};

} // end namespace swift

#endif
