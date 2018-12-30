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

#include "swift/Basic/SourceLoc.h"
#include "swift/Parse/ParsedTrivia.h"
#include "swift/Parse/Token.h"
#include "swift/Syntax/SyntaxKind.h"
#include <vector>

namespace swift {

typedef void *OpaqueSyntaxNode;

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
    std::vector<ParsedRawSyntaxNode> Children;
  };
  struct DeferredTokenNode {
    Token Tok;
    ParsedTrivia LeadingTrivia;
    ParsedTrivia TrailingTrivia;
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

  ParsedRawSyntaxNode(syntax::SyntaxKind k,
                      ArrayRef<ParsedRawSyntaxNode> deferredNodes)
    : DeferredLayout{deferredNodes},
      SynKind(uint16_t(k)), TokKind(uint16_t(tok::unknown)),
      DK(DataKind::DeferredLayout) {
    assert(getKind() == k && "Syntax kind with too large value!");
  }

  ParsedRawSyntaxNode(Token tok,
                      ParsedTrivia leadingTrivia,
                      ParsedTrivia trailingTrivia)
    : DeferredToken{std::move(tok),
                    std::move(leadingTrivia),
                    std::move(trailingTrivia)},
      SynKind(uint16_t(syntax::SyntaxKind::Token)),
      TokKind(uint16_t(tok.getKind())),
      DK(DataKind::DeferredToken) {
    assert(getTokenKind() == tok.getKind() && "Token kind is too large value!");
  }

public:
  ParsedRawSyntaxNode()
    : RecordedData{},
      SynKind(uint16_t(syntax::SyntaxKind::Unknown)),
      TokKind(uint16_t(tok::unknown)),
      DK(DataKind::Null) {
  }

  ParsedRawSyntaxNode(syntax::SyntaxKind k, tok tokKind,
                      CharSourceRange r, OpaqueSyntaxNode n)
    : RecordedData{n, r},
      SynKind(uint16_t(k)), TokKind(uint16_t(tokKind)),
      DK(DataKind::Recorded) {
    assert(getKind() == k && "Syntax kind with too large value!");
    assert(getTokenKind() == tokKind && "Token kind with too large value!");
  }

  ParsedRawSyntaxNode(const ParsedRawSyntaxNode &other) {
    switch (other.DK) {
    case DataKind::Null:
      break;
    case DataKind::Recorded:
      new(&this->RecordedData)RecordedSyntaxNode(other.RecordedData);
      break;
    case DataKind::DeferredLayout:
      new(&this->DeferredLayout)DeferredLayoutNode(other.DeferredLayout);
      break;
    case DataKind::DeferredToken:
      new(&this->DeferredToken)DeferredTokenNode(other.DeferredToken);
      break;
    }
    this->SynKind = other.SynKind;
    this->TokKind = other.TokKind;
    this->DK = other.DK;
  }

  ParsedRawSyntaxNode(ParsedRawSyntaxNode &&other) {
    switch (other.DK) {
    case DataKind::Null:
      break;
    case DataKind::Recorded:
      new(&this->RecordedData)RecordedSyntaxNode(
          std::move(other.RecordedData));
      break;
    case DataKind::DeferredLayout:
      new(&this->DeferredLayout)DeferredLayoutNode(
          std::move(other.DeferredLayout));
      break;
    case DataKind::DeferredToken:
      new(&this->DeferredToken)DeferredTokenNode(
          std::move(other.DeferredToken));
      break;
    }
    this->SynKind = other.SynKind;
    this->TokKind = other.TokKind;
    this->DK = other.DK;
  }

  ~ParsedRawSyntaxNode() {
    releaseMemory();
  }

  ParsedRawSyntaxNode &operator=(const ParsedRawSyntaxNode &other) {
    releaseMemory();
    new (this)ParsedRawSyntaxNode(other);
    return *this;
  }

  ParsedRawSyntaxNode &operator=(ParsedRawSyntaxNode &&other) {
    releaseMemory();
    new (this)ParsedRawSyntaxNode(std::move(other));
    return *this;
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

  // Recorded Data ===========================================================//

  CharSourceRange getRange() const {
    assert(isRecorded());
    return RecordedData.Range;
  }
  OpaqueSyntaxNode getOpaqueNode() const {
    assert(isRecorded());
    return RecordedData.OpaqueNode;
  }

  // Deferred Layout Data ====================================================//

  ArrayRef<ParsedRawSyntaxNode> getDeferredChildren() const {
    assert(DK == DataKind::DeferredLayout);
    return DeferredLayout.Children;
  }
  void addDeferredChild(ParsedRawSyntaxNode subnode) {
    assert(DK == DataKind::DeferredLayout);
    DeferredLayout.Children.push_back(std::move(subnode));
  }

  // Deferred Token Data =====================================================//

  const Token &getToken() const {
    assert(DK == DataKind::DeferredToken);
    return DeferredToken.Tok;
  }
  const ParsedTrivia &getLeadingTrivia() const {
    assert(DK == DataKind::DeferredToken);
    return DeferredToken.LeadingTrivia;
  }
  const ParsedTrivia &getTrailingTrivia() const {
    assert(DK == DataKind::DeferredToken);
    return DeferredToken.TrailingTrivia;
  }

  //==========================================================================//

  /// Form a deferred syntax layout node.
  static ParsedRawSyntaxNode makeDeferred(syntax::SyntaxKind k,
                        ArrayRef<ParsedRawSyntaxNode> deferredNodes) {
    return ParsedRawSyntaxNode{k, deferredNodes};
  }

  /// Form a deferred token node.
  static ParsedRawSyntaxNode makeDeferred(Token tok,
                                          ParsedTrivia leadingTrivia,
                                          ParsedTrivia trailingTrivia) {
    return ParsedRawSyntaxNode{std::move(tok), std::move(leadingTrivia),
        std::move(trailingTrivia)};
  }

  /// Form a deferred missing token node.
  static ParsedRawSyntaxNode makeDeferredMissing(tok tokKind, SourceLoc loc);

  /// Dump this piece of syntax recursively for debugging or testing.
  LLVM_ATTRIBUTE_DEPRECATED(
    void dump() const LLVM_ATTRIBUTE_USED,
    "only for use within the debugger");

  /// Dump this piece of syntax recursively.
  void dump(raw_ostream &OS, unsigned Indent = 0) const;

  static ParsedRawSyntaxNode null() {
    return ParsedRawSyntaxNode{};
  }

private:
  void releaseMemory() {
    switch (DK) {
    case DataKind::Null:
      break;
    case DataKind::Recorded:
      RecordedData.~RecordedSyntaxNode(); break;
    case DataKind::DeferredLayout:
      DeferredLayout.~DeferredLayoutNode(); break;
    case DataKind::DeferredToken:
      DeferredToken.~DeferredTokenNode(); break;
    }
  }
};

} // end namespace swift

#endif
