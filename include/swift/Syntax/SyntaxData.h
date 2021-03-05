//===--- SyntaxData.h - Swift Syntax Data Interface -------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines the SyntaxData interface, the type for the instance
// data for Syntax nodes.
//
// Effectively, these provide two main things to a Syntax node - parental
// relationships and caching for its children.
//
// A SyntaxData contains at least a strong reference to the RawSyntax,
// from which most information comes, and additionally a weak reference to
// its parent and the "index" at which it occurs in its parent. These were
// originally intended to have the important public APIs for structured
// editing but now contain no significant or public API; for those, see the
// Syntax type. These are purely to contain data, hence the name.
//
// Conceptually, SyntaxData add the characteristic of specific identity in a
// piece of Swift source code. While the RawSyntax for the integer literal
// token '1' can be reused anywhere a '1' occurs and has identical formatting,
// a SyntaxData represents *a* specific '1' at a particular location in
// Swift source.
//
// These are effectively internal implementation. For all public APIs, look
// for the type without "Data" in its name. For example, a StructDeclSyntaxData
// expresses its API through the wrapping StructDeclSyntax type.
//
//===----------------------------------------------------------------------===//


#ifndef SWIFT_SYNTAX_SYNTAXDATA_H
#define SWIFT_SYNTAX_SYNTAXDATA_H

#include "swift/Basic/Debug.h"
#include "swift/Syntax/AbsoluteRawSyntax.h"
#include "swift/Syntax/AtomicCache.h"
#include "swift/Syntax/RawSyntax.h"
#include "swift/Syntax/References.h"
#include "llvm/ADT/DenseMap.h"

#include <atomic>

namespace swift {
namespace syntax {

/// The class for holding parented syntax.
///
/// This structure should not contain significant public
/// API or internal modification API.
///
/// It is essentially a wrapper around \c AbsoluteRawSyntax that also keeps
/// track of the parent.
class SyntaxData : public llvm::ThreadSafeRefCountedBase<SyntaxData> {
  const AbsoluteRawSyntax AbsoluteRaw;
  RC<const SyntaxData> Parent;

  /// If this node is the root of a Syntax tree (i.e. \c Parent is \c nullptr ),
  /// the arena in which this node's \c RawSyntax node has been allocated.
  /// This keeps this \c RawSyntax nodes referenced by this tree alive.
  RC<SyntaxArena> Arena;

  /// Create a intermediate node with a parent.
  SyntaxData(AbsoluteRawSyntax AbsoluteRaw, const RC<const SyntaxData> &Parent)
      : AbsoluteRaw(AbsoluteRaw), Parent(Parent), Arena(nullptr) {}

  /// Create a new root node
  SyntaxData(AbsoluteRawSyntax AbsoluteRaw)
      : AbsoluteRaw(AbsoluteRaw), Parent(nullptr),
        Arena(AbsoluteRaw.getRaw()->getArena()) {}

public:
  /// With a new \c RawSyntax node, create a new node from this one and
  /// recursively rebuild the parental chain up to the root.
  RC<const SyntaxData> replacingSelf(const RawSyntax *NewRaw) const;

  /// Replace a child in the raw syntax and recursively rebuild the
  /// parental chain up to the root.
  template <typename CursorType>
  RC<const SyntaxData> replacingChild(const RawSyntax *RawChild,
                                      CursorType ChildCursor) const {
    auto NewRaw = AbsoluteRaw.getRaw()->replacingChild(ChildCursor, RawChild);
    return replacingSelf(NewRaw);
  }

  /// Get the node immediately before this current node that does contain a
  /// non-missing token. Return \c nullptr if we cannot find such node.
  RC<const SyntaxData> getPreviousNode() const;

  /// Get the node immediately after this current node that does contain a
  /// non-missing token. Return \c nullptr if we cannot find such node.
  RC<const SyntaxData> getNextNode() const;

  /// Get the first non-missing token node in this tree. Return \c nullptr if
  /// this node does not contain non-missing tokens.
  RC<const SyntaxData> getFirstToken() const;

  /// Get the last non-missing token node in this tree. Return \c nullptr if
  /// this node does not contain non-missing tokens.
  RC<const SyntaxData> getLastToken() const;

  /// Make a new \c SyntaxData node for the tree's root.
  static RC<const SyntaxData> makeRoot(AbsoluteRawSyntax AbsoluteRaw) {
    return RC<const SyntaxData>(new SyntaxData(AbsoluteRaw));
  }

  const AbsoluteRawSyntax &getAbsoluteRaw() const { return AbsoluteRaw; }

  /// Returns the raw syntax node for this syntax node.
  const RawSyntax *getRaw() const { return AbsoluteRaw.getRaw(); }

  /// Returns the kind of syntax node this is.
  SyntaxKind getKind() const { return AbsoluteRaw.getRaw()->getKind(); }

  /// Return the parent syntax if there is one.
  RC<const SyntaxData> getParent() const { return Parent; }

  /// Returns true if this syntax node has a parent.
  bool hasParent() const {
    return Parent != nullptr;
  }

  /// Returns the child index of this node in its parent, if it has a parent,
  /// otherwise 0.
  AbsoluteSyntaxPosition::IndexInParentType getIndexInParent() const {
    return AbsoluteRaw.getPosition().getIndexInParent();
  }

  /// Returns the number of children this SyntaxData represents.
  size_t getNumChildren() const {
    return AbsoluteRaw.getRaw()->getLayout().size();
  }

  /// Gets the child at the index specified by the provided cursor.
  template <typename CursorType>
  RC<const SyntaxData> getChild(CursorType Cursor) const {
    return getChild(
        (AbsoluteSyntaxPosition::IndexInParentType)cursorIndex(Cursor));
  }

  /// Gets the child at the specified \p Index.
  RC<const SyntaxData>
  getChild(AbsoluteSyntaxPosition::IndexInParentType Index) const;

  /// Get the offset at which the leading trivia of this node starts.
  AbsoluteOffsetPosition getAbsolutePositionBeforeLeadingTrivia() const;

  /// Get the offset at which the content (excluding trailing trivia) of this
  /// node ends.
  AbsoluteOffsetPosition getAbsoluteEndPositionBeforeTrailingTrivia() const;

  /// Get the offset at which the content of this node (excluding leading
  /// trivia) starts.
  AbsoluteOffsetPosition getAbsolutePositionAfterLeadingTrivia() const;

  /// Get the offset at chiwh the trailing trivia of this node ends.
  AbsoluteOffsetPosition getAbsoluteEndPositionAfterTrailingTrivia() const;

  /// Returns true if the data node represents type syntax.
  bool isType() const;

  /// Returns true if the data node represents statement syntax.
  bool isStmt() const;

  /// Returns true if the data node represents declaration syntax.
  bool isDecl() const;

  /// Returns true if the data node represents expression syntax.
  bool isExpr() const;

  /// Returns true if the data node represents pattern syntax.
  bool isPattern() const;

  /// Returns true if this syntax is of some "unknown" kind.
  bool isUnknown() const;

  /// Dump a debug description of the syntax data for debugging to
  /// standard error.
  void dump(llvm::raw_ostream &OS) const;

  SWIFT_DEBUG_DUMP;
};

} // end namespace syntax
} // end namespace swift

#endif // SWIFT_SYNTAX_SYNTAXDATA_H
