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
#include "swift/Parse/SyntaxParseActions.h"
#include "swift/Parse/Token.h"
#include "swift/Syntax/SyntaxKind.h"
#include "llvm/Support/Debug.h"

namespace swift {

typedef const void *OpaqueSyntaxNode;
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
  friend class ParsedRawSyntaxRecorder;
  using DataKind = RecordedOrDeferredNode::Kind;

  /// The opaque data of this node. Needs to be interpreted by the \c
  /// SyntaxParseActions, which created it.
  RecordedOrDeferredNode Data;

  /// The range of this node, including trivia.
  CharSourceRange Range;
  uint16_t SynKind;
  uint16_t TokKind;
  /// Primary used for capturing a deferred missing token.
  bool IsMissing = false;

  ParsedRawSyntaxNode(const ParsedRawSyntaxNode &other) = delete;
  ParsedRawSyntaxNode &operator=(const ParsedRawSyntaxNode &other) = delete;

public:
  // MARK: - Constructors

  ParsedRawSyntaxNode()
      : Data(nullptr, DataKind::Null), Range(),
        SynKind(uint16_t(syntax::SyntaxKind::Unknown)),
        TokKind(uint16_t(tok::unknown)) {}

  ParsedRawSyntaxNode(OpaqueSyntaxNode Opaque, CharSourceRange Range,
                      syntax::SyntaxKind SynKind, tok TokKind, DataKind DK,
                      bool IsMissing)
      : Data(Opaque, DK), Range(Range), SynKind(uint16_t(SynKind)),
        TokKind(uint16_t(TokKind)), IsMissing(IsMissing) {
    assert(getKind() == SynKind && "Syntax kind with too large value!");
    assert(getTokenKind() == TokKind && "Token kind with too large value!");
  }

  ParsedRawSyntaxNode &operator=(ParsedRawSyntaxNode &&other) {
    assert(ensureDataIsNotRecorded() &&
           "recorded data is being destroyed by assignment");
    Data = std::move(other.Data);
    Range = std::move(other.Range);
    SynKind = std::move(other.SynKind);
    TokKind = std::move(other.TokKind);
    IsMissing = std::move(other.IsMissing);
    other.reset();
    return *this;
  }

  ParsedRawSyntaxNode(ParsedRawSyntaxNode &&other) : ParsedRawSyntaxNode() {
    *this = std::move(other);
  }

  static ParsedRawSyntaxNode null() {
    return ParsedRawSyntaxNode{};
  }

  ~ParsedRawSyntaxNode() {
    assert(ensureDataIsNotRecorded() && "recorded data is being destructed");
  }

  // MARK: - Retrieving node kind

  /// Returns the type of this node (recorded, deferred layout, deferred token,
  /// null).
  DataKind getDataKind() const { return Data.getKind(); }

  bool isNull() const { return getDataKind() == DataKind::Null; }
  bool isRecorded() const { return getDataKind() == DataKind::Recorded; }
  bool isDeferredLayout() const {
    return getDataKind() == DataKind::DeferredLayout;
  }
  bool isDeferredToken() const {
    return getDataKind() == DataKind::DeferredToken;
  }

  // MARK: - Retrieving opaque data

  /// Returns the opaque data of this node, assuming that it is deferred. This
  /// must be interpreted by the \c SyntaxParseAction, which likely also needs
  /// the node type (layout or token) to interpret the data.
  /// The data opaque data returned by this function *must not* be used to
  /// record the node, only to insepect it.
  OpaqueSyntaxNode getUnsafeDeferredOpaqueData() const {
    assert(isDeferredLayout() || isDeferredToken());
    return Data.getOpaque();
  }

  /// Return the opaque data of this node and reset it.
  OpaqueSyntaxNode takeData() {
    OpaqueSyntaxNode Data = this->Data.getOpaque();
    reset();
    return Data;
  }

  RecordedOrDeferredNode takeRecordedOrDeferredNode() {
    RecordedOrDeferredNode Data = this->Data;
    reset();
    return Data;
  }

  // MARK: - Retrieving additional node info

  syntax::SyntaxKind getKind() const { return syntax::SyntaxKind(SynKind); }
  tok getTokenKind() const { return tok(TokKind); }

  bool isToken() const {
    return getKind() == syntax::SyntaxKind::Token;
  }
  bool isToken(tok tokKind) const {
    return getTokenKind() == tokKind;
  }
  bool isMissing() const { return IsMissing; }

  /// Returns the range of this node including leading and trailing trivia.
  CharSourceRange getRange() const { return Range; }

  size_t
  getDeferredNumChildren(const SyntaxParsingContext *SyntaxContext) const;

  /// If this node is a deferred layout node, return the child at index \p
  /// ChildIndex.
  /// Note that this may be an expensive operation since the \c
  /// SyntaxParseAction, which created the node (implicitly passed via the
  /// \p SyntaxContext) needs to be consulted to retrieve the child.
  ParsedRawSyntaxNode
  getDeferredChild(size_t ChildIndex,
                   const SyntaxParsingContext *SyntaxContext) const;

  // MARK: - Miscellaneous

  void reset() {
    Data = RecordedOrDeferredNode(nullptr, DataKind::Null);
    SynKind = uint16_t(syntax::SyntaxKind::Unknown);
    TokKind = uint16_t(tok::unknown);
    IsMissing = false;
    Range = CharSourceRange();
  }

  ParsedRawSyntaxNode unsafeCopy() const {
    ParsedRawSyntaxNode copy;
    copy.Data = Data;
    copy.Range = Range;
    copy.SynKind = SynKind;
    copy.TokKind = TokKind;
    copy.IsMissing = IsMissing;
    return copy;
  }

  ParsedRawSyntaxNode copyDeferred() const {
    assert(isDeferredLayout() || isDeferredToken() && "node not deferred");
    ParsedRawSyntaxNode copy;
    copy.Data = Data;
    copy.Range = Range;
    copy.SynKind = SynKind;
    copy.TokKind = TokKind;
    copy.IsMissing = IsMissing;
    return copy;
  }

#ifndef NDEBUG
  bool ensureDataIsNotRecorded() {
    if (getDataKind() != DataKind::Recorded)
      return true;
    llvm::dbgs() << "Leaking node: ";
    dump(llvm::dbgs());
    llvm::dbgs() << "\n";
    return false;
  }
#endif

  // MARK: - Printing

  /// Dump this piece of syntax recursively for debugging or testing.
  SWIFT_DEBUG_DUMP;

  /// Dump this piece of syntax recursively. If \p Context is passed, this
  /// method is also able to traverse its children and dump them.
  void dump(raw_ostream &OS, const SyntaxParsingContext *Context = nullptr,
            unsigned Indent = 0) const;
};

} // end namespace swift

#endif
