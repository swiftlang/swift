//===----------- LibSyntaxGenerator.h -------------------------------------===//
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

#ifndef SWIFT_PARSE_LIBSYNTAXGENERATOR_H
#define SWIFT_PARSE_LIBSYNTAXGENERATOR_H

#include "swift/Parse/HiddenLibSyntaxAction.h"
#include "swift/Parse/ParsedRawSyntaxNode.h"
#include "swift/Parse/ParsedRawSyntaxRecorder.h"
#include "swift/Parse/SyntaxParsingCache.h"
#include "swift/Syntax/RawSyntax.h"
#include "swift/SyntaxParse/SyntaxTreeCreator.h"

namespace swift {
// TODO: (swift-parse) remove when possible
/// Generates libSyntax nodes either by looking them up using
/// HiddenLibSyntaxAction (based on provided OpaqueSyntaxNode) or by recording
/// them with ParsedRawSyntaxRecorder.
class LibSyntaxGenerator {
  std::shared_ptr<HiddenLibSyntaxAction> Actions;
  ParsedRawSyntaxRecorder Recorder;

public:
  explicit LibSyntaxGenerator(std::shared_ptr<HiddenLibSyntaxAction> spActions)
      : Actions(std::move(spActions)), Recorder(Actions->getLibSyntaxAction()) {
  }

  /// Create a \c TokenSyntax from the raw data and record it in the
  /// \c HiddenLibSyntaxAction's \c LibSyntaxAction.
  TokenSyntax createToken(ParsedRawSyntaxNode Node) {
    assert(Node.isDeferredToken());

    auto Kind = Node.getTokenKind();
    auto Range = Node.getDeferredTokenRangeWithTrivia();
    auto LeadingTriviaPieces = Node.getDeferredLeadingTriviaPieces();
    auto TrailingTriviaPieces = Node.getDeferredTrailingTriviaPieces();

    auto Recorded = Recorder.recordToken(Kind, Range, LeadingTriviaPieces,
                                         TrailingTriviaPieces);
    auto Raw = static_cast<RawSyntax *>(Recorded.getOpaqueNode());
    return make<TokenSyntax>(Raw);
  }

  /// Create a \c SyntaxNode from the raw data and record it in the
  /// \c HiddenLibSyntaxAction's \c LibSyntaxAction.
  template <typename SyntaxNode>
  SyntaxNode createNode(ParsedRawSyntaxNode Node) {
    assert(Node.isDeferredLayout());
    auto Kind = Node.getKind();
    auto Children = Node.getDeferredChildren();

    auto Recorded = Recorder.recordRawSyntax(Kind, Children);
    RC<RawSyntax> Raw{static_cast<RawSyntax *>(Recorded.takeOpaqueNode())};
    Raw->Release(); // -1 since it's transfer of ownership.
    return make<SyntaxNode>(Raw);
  }

  /// Return the libSyntax node stored in the given opaque \c Node.
  /// Assumes that \c Node is of type \c HiddenNode.
  TokenSyntax getLibSyntaxTokenFor(OpaqueSyntaxNode Node) {
    return getLibSyntaxNodeFor<TokenSyntax>(Node);
  }

  /// Return the libSyntax node stored in the given opaque \c Node.
  /// Assumes that \c Node is of type \c HiddenNode.
  template <typename SyntaxNode>
  SyntaxNode getLibSyntaxNodeFor(OpaqueSyntaxNode Node) {
    return make<SyntaxNode>(Actions->getLibSyntaxNodeFor(Node));
  }

  OpaqueSyntaxNode finalizeNode(OpaqueSyntaxNode Node) {
    if (Actions->isReleaseOfLibSyntaxNodeNeeded()) {
      Actions->getLibSyntaxNodeFor(Node)->Release();
    }
    return Actions->getExplicitNodeFor(Node);
  }
};
} // namespace swift

#endif // SWIFT_PARSE_LIBSYNTAXGENERATOR_H
