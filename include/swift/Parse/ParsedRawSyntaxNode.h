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

#ifndef NDEBUG
/// Whether \c ParsedRawSyntaxNode should keep track of its range and verify
/// that the children of layout nodes have consecutive ranges.
/// Because this significantly changes the way, \c ParsedRawSyntaxNode and
/// \c ParsedRawSyntaxNodeRecorder are being compiled, this is a separate
/// constant from \c NDEBUG, so that it can be toggled independently to \c
/// NDEBUG during development.
#define PARSEDRAWSYNTAXNODE_VERIFY_RANGES 1
#endif

namespace swift {

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

#ifdef PARSEDRAWSYNTAXNODE_VERIFY_RANGES
  /// The range of this node, including trivia.
  /// Only store this as a member when it's actually needed to keep \c
  /// ParsedRawSyntaxNode as small as possible, which improves performance
  /// when it is being passed around.
  CharSourceRange Range;
#endif
  syntax::SyntaxKind SynKind;
  tok TokKind;
  /// Primary used for capturing a deferred missing token.
  bool IsMissing = false;

  ParsedRawSyntaxNode(const ParsedRawSyntaxNode &other) = delete;
  ParsedRawSyntaxNode &operator=(const ParsedRawSyntaxNode &other) = delete;

public:
  // MARK: - Constructors

  ParsedRawSyntaxNode()
      : Data(nullptr, DataKind::Null), SynKind(syntax::SyntaxKind::Unknown),
        TokKind(tok::unknown) {}

#ifdef PARSEDRAWSYNTAXNODE_VERIFY_RANGES
  ParsedRawSyntaxNode(RecordedOrDeferredNode Data, syntax::SyntaxKind SynKind,
                      tok TokKind, bool IsMissing, CharSourceRange Range)
      : Data(Data), Range(Range), SynKind(SynKind), TokKind(TokKind),
        IsMissing(IsMissing) {
    assert(getKind() == SynKind && "Syntax kind with too large value!");
    assert(getTokenKind() == TokKind && "Token kind with too large value!");
  }

  ParsedRawSyntaxNode(OpaqueSyntaxNode Opaque, syntax::SyntaxKind SynKind,
                      tok TokKind, DataKind DK, bool IsMissing,
                      CharSourceRange Range)
      : ParsedRawSyntaxNode(RecordedOrDeferredNode(Opaque, DK), SynKind,
                            TokKind, IsMissing, Range) {}
#else
  ParsedRawSyntaxNode(RecordedOrDeferredNode Data, syntax::SyntaxKind SynKind,
                      tok TokKind, bool IsMissing)
      : Data(Data), SynKind(SynKind), TokKind(TokKind), IsMissing(IsMissing) {
    assert(getKind() == SynKind && "Syntax kind with too large value!");
    assert(getTokenKind() == TokKind && "Token kind with too large value!");
  }

  ParsedRawSyntaxNode(OpaqueSyntaxNode Opaque, syntax::SyntaxKind SynKind,
                      tok TokKind, DataKind DK, bool IsMissing)
      : ParsedRawSyntaxNode(RecordedOrDeferredNode(Opaque, DK), SynKind,
                            TokKind, IsMissing) {}
#endif

  ParsedRawSyntaxNode &operator=(ParsedRawSyntaxNode &&other) {
    assert(ensureDataIsNotRecorded() &&
           "recorded data is being destroyed by assignment");
    Data = std::move(other.Data);
#ifdef PARSEDRAWSYNTAXNODE_VERIFY_RANGES
    Range = std::move(other.Range);
#endif
    SynKind = std::move(other.SynKind);
    TokKind = std::move(other.TokKind);
    IsMissing = std::move(other.IsMissing);
    other.reset();
    return *this;
  }

  ParsedRawSyntaxNode(ParsedRawSyntaxNode &&other) : ParsedRawSyntaxNode() {
    *this = std::move(other);
  }

  static ParsedRawSyntaxNode null() { return ParsedRawSyntaxNode(); }

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

  syntax::SyntaxKind getKind() const { return SynKind; }
  tok getTokenKind() const { return TokKind; }

  bool isToken() const {
    return getKind() == syntax::SyntaxKind::Token;
  }
  bool isToken(tok tokKind) const {
    return getTokenKind() == tokKind;
  }
  bool isMissing() const { return IsMissing; }

#ifdef PARSEDRAWSYNTAXNODE_VERIFY_RANGES
  /// Returns the range of this node including leading and trailing trivia.
  ///
  /// This method is only present if \c ParsedRawSyntaxNode is keeping track
  /// of its range to verify element ranges.
  CharSourceRange getRange() const { return Range; }
#endif

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
    SynKind = syntax::SyntaxKind::Unknown;
    TokKind = tok::unknown;
    IsMissing = false;
#ifdef PARSEDRAWSYNTAXNODE_VERIFY_RANGES
    Range = CharSourceRange();
#endif
  }

  ParsedRawSyntaxNode unsafeCopy() const {
    ParsedRawSyntaxNode copy;
    copy.Data = Data;
#ifdef PARSEDRAWSYNTAXNODE_VERIFY_RANGES
    copy.Range = Range;
#endif
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
