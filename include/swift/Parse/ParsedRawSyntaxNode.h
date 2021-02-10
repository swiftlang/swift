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
#include "llvm/Support/Debug.h"

namespace swift {

/// A opaque syntax node. It has the requirement that nullptr must always
/// represent a non-existent node.
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
public:
  enum class DataKind : uint8_t {
    Null,
    Recorded,
    DeferredLayout,
    DeferredToken,
    /// The node has been destroyed because it has been moved somewhere else.
    Destroyed,
  };

private:
  friend class ParsedRawSyntaxRecorder;

  /// The opaque data created by \c SyntaxParseActions. Depending on \c DK, this
  /// can be data required to record a deferred node or the deferred node
  /// itself. The \c SyntaxParseActions that created this node need to be
  /// consulted to interpret the data.
  OpaqueSyntaxNode Data;

  /// The range of the entire node, including trivia.
  CharSourceRange Range;

  uint16_t SynKind;
  uint16_t TokKind;
  DataKind DK;
  /// Primary used for capturing a deferred missing token.
  bool IsMissing = false;

  /// Create a deferred layout node.
  ParsedRawSyntaxNode(syntax::SyntaxKind k, CharSourceRange Range,
                      OpaqueSyntaxNode Data, DataKind DK)
      : Data(Data), Range(Range), SynKind(uint16_t(k)),
        TokKind(uint16_t(tok::NUM_TOKENS)), DK(DK) {
    assert(DK == DataKind::DeferredLayout);
    assert(getKind() == k && "Syntax kind with too large value!");
  }

  /// Create a deferred token node.
  ParsedRawSyntaxNode(tok tokKind, SourceLoc tokLoc, CharSourceRange Range,
                      bool IsMissing, OpaqueSyntaxNode Data, DataKind DK)
      : Data(Data), Range(Range), SynKind(uint16_t(syntax::SyntaxKind::Token)),
        TokKind(uint16_t(tokKind)), DK(DK), IsMissing(IsMissing) {
    assert(DK == DataKind::DeferredToken);
    assert(getTokenKind() == tokKind && "Token kind is too large value!");
  }
  ParsedRawSyntaxNode(const ParsedRawSyntaxNode &other) = delete;
  ParsedRawSyntaxNode &operator=(const ParsedRawSyntaxNode &other) = delete;

public:
  ParsedRawSyntaxNode()
      : Data(nullptr), Range(), SynKind(uint16_t(syntax::SyntaxKind::Unknown)),
        TokKind(uint16_t(tok::unknown)), DK(DataKind::Null) {}

  /// Create an arbitrary syntax node. If \p is \c Token, \p tokKind must be set
  /// otherwise \p tokKind must be \c NUM_TOKENS.
  ParsedRawSyntaxNode(syntax::SyntaxKind k, tok tokKind, CharSourceRange r,
                      bool IsMissing, OpaqueSyntaxNode n, DataKind DK)
      : Data(n), Range(r), SynKind(uint16_t(k)), TokKind(uint16_t(tokKind)),
        DK(DK), IsMissing(IsMissing) {
    assert(getKind() == k && "Syntax kind with too large value!");
    assert(getTokenKind() == tokKind && "Token kind with too large value!");
    if (k == syntax::SyntaxKind::Token) {
      assert(tokKind != tok::NUM_TOKENS &&
             "If node is a token, it must have a token kind");
    } else {
      assert(tokKind == tok::NUM_TOKENS &&
             "If node is not a token, it cannot have a token kind");
    }
  }

#ifndef NDEBUG
  bool ensureDataIsNotRecorded() {
    if (!isRecorded()) {
      return true;
    }
    llvm::dbgs() << "Leaking node: ";
    dump(llvm::dbgs());
    llvm::dbgs() << "\n";
    return false;
  }
#endif

  ParsedRawSyntaxNode &operator=(ParsedRawSyntaxNode &&other) {
    assert(ensureDataIsNotRecorded() &&
           "recorded data is being destroyed by assignment");
    Data = std::move(other.Data);
    Range = std::move(other.Range);
    SynKind = std::move(other.SynKind);
    TokKind = std::move(other.TokKind);
    DK = std::move(other.DK);
    IsMissing = std::move(other.IsMissing);
    other.reset();
    return *this;
  }
  ParsedRawSyntaxNode(ParsedRawSyntaxNode &&other)
      : Data(std::move(other.Data)), Range(std::move(other.Range)),
        SynKind(std::move(other.SynKind)), TokKind(std::move(other.TokKind)),
        DK(std::move(other.DK)), IsMissing(std::move(other.IsMissing)) {
    other.reset();
  }

  ~ParsedRawSyntaxNode() {
    assert(ensureDataIsNotRecorded() && "recorded data is being destructed");
  }

  syntax::SyntaxKind getKind() const { return syntax::SyntaxKind(SynKind); }
  tok getTokenKind() const { return tok(TokKind); }

  /// Retrieve the opaque data of this node. This does not transfer ownership
  /// of the data. If the data is being consumed, use \p takeData to reset this
  /// node and supress any warnings about recorded nodes being destructed.
  OpaqueSyntaxNode getData() const { return Data; }

  /// Return the data of this node and reset it.
  OpaqueSyntaxNode takeData() {
    OpaqueSyntaxNode Data = this->Data;
    reset();
    return Data;
  }

  /// If this node is a deferred layout node, return the child at index \p
  /// ChildIndex.
  /// Note that this may be an expensive operation since the \c
  /// SyntaxParseAction, which created the node (implicitly passed via the
  /// \p SyntaxContext) needs to be consulted to retrieve the child.
  ParsedRawSyntaxNode
  getDeferredChild(size_t ChildIndex,
                   const SyntaxParsingContext *SyntaxContext) const;

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

  void reset() { DK = DataKind::Destroyed; }

  ParsedRawSyntaxNode unsafeCopy() const {
    ParsedRawSyntaxNode copy;
    copy.Data = Data;
    copy.Range = Range;
    copy.SynKind = SynKind;
    copy.TokKind = TokKind;
    copy.DK = DK;
    copy.IsMissing = IsMissing;
    return copy;
  }

  CharSourceRange getRange() const { return Range; }

  //==========================================================================//

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
