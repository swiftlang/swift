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

/// A reference counted box that can contain any type.
template <typename T>
class RefCountedBox final
    : public llvm::ThreadSafeRefCountedBase<RefCountedBox<T>> {
public:
  const T Data;

  RefCountedBox(const T Data) : Data(Data) {}

  static RC<RefCountedBox<T>> make(const T &Data) {
    return RC<RefCountedBox<T>>{new RefCountedBox(Data)};
  }
};

/// The class for holding parented syntax.
///
/// This structure should not contain significant public
/// API or internal modification API.
///
/// It is essentially a wrapper around \c AbsoluteRawSyntax that also keeps
/// track of the parent.
///
/// The parent can either be stored ref-counted for memory-safety access or as
/// a plain pointer if it can be guaranteed that the parent will always outlive
/// the child.
/// If memory-safety should be guranteed, use \c SyntaxData, which gurantees
/// that the parent is always stored ref-counted.
///
/// Having \c SyntaxData be a sublcass of \c SyntaxDataRef means that we can
/// write algorithms that operate on \c SyntaxDataRef. When invoking those with
/// a \c SyntaxData node, we can efficiently demote the \c SyntaxData node to a
/// \c SyntaxDataRef.
///
/// We also uphold the following invariant: If a node's parent is ref-counted,
/// then all of its parent's parents are also ref-counted. This means that we
/// can address a subtree of a ref-counted syntax tree in a fast, but unsafe
/// unowned way, but we can never address a subtree of an unowned tree as
/// ref-counted.
class SyntaxDataRef {
  friend class SyntaxData;

  const AbsoluteRawSyntaxRef AbsoluteRaw;

  /// The parent can be stored either ref-counted or unsafe by a direct pointer.
  /// Either of those must always be \c nullptr. If both are \c nullptr, then
  /// the node does not have a parent.
  const RC<RefCountedBox<SyntaxDataRef>> RefCountedParent;
  const SyntaxDataRef *UnownedParent;

  /// Create a reference-counted \c SyntaxDataRef. \p AbsoluteRaw must be
  /// reference-counted and \p Parent must be \c nullptr or also ref-counted.
  SyntaxDataRef(AbsoluteRawSyntaxRef AbsoluteRaw,
                const RC<RefCountedBox<SyntaxDataRef>> &Parent)
      : AbsoluteRaw(AbsoluteRaw), RefCountedParent(Parent),
        UnownedParent(nullptr) {
    assert(AbsoluteRaw.isRefCounted() &&
           "If parent is ref-counted, AbsoluteRaw must also be ref-counted");
    assert((Parent == nullptr || Parent->Data.isRefCounted()) &&
           "Cannot address a subtree of an unowned tree as ref-counted");
  }

  /// Create an unowned \c SyntaxDataRef.
  /// \p AbsoluteRaw must not be reference-counted.
  SyntaxDataRef(AbsoluteRawSyntaxRef AbsoluteRaw, const SyntaxDataRef *Parent)
      : AbsoluteRaw(AbsoluteRaw), RefCountedParent(nullptr),
        UnownedParent(Parent) {
    if (Parent != nullptr) {
      assert(!AbsoluteRaw.isRefCounted() &&
             "If parent is unowned, AbsoluteRaw must also be unowned");
    }
  }

public:
  // MARK: - Retrieving underlying storage

  bool isRefCounted() const {
    bool IsRefCounted = (UnownedParent == nullptr);
    assert(IsRefCounted == getAbsoluteRawRef().isRefCounted() &&
           "Either both AbsoluteRawSyntax and the parent reference should be "
           "reference-counted or none of them");
    return IsRefCounted;
  }

  const AbsoluteRawSyntaxRef &getAbsoluteRawRef() const { return AbsoluteRaw; }

  /// Returns the raw syntax node for this syntax node.
  const RawSyntax *getRawRef() const { return getAbsoluteRawRef().getRawRef(); }

  // MARK: - Retrieving related nodes

  Optional<SyntaxDataRef> getParentRef() const {
    if (UnownedParent) {
      return *UnownedParent;
    } else if (RefCountedParent) {
      return RefCountedParent->Data;
    } else {
      return None;
    }
  }

  /// Returns true if this syntax node has a parent.
  bool hasParent() const { return getParentRef().hasValue(); }

  /// Returns the number of children this SyntaxData has.
  size_t getNumChildren() const { return getRawRef()->getLayout().size(); }

  /// Gets the child at the index specified by the provided cursor.
  template <typename CursorType>
  Optional<SyntaxDataRef> getChildRef(CursorType Cursor) const {
    return getChildRef(
        (AbsoluteSyntaxPosition::IndexInParentType)cursorIndex(Cursor));
  }

  /// Gets the child at the specified \p Index.
  Optional<SyntaxDataRef>
  getChildRef(AbsoluteSyntaxPosition::IndexInParentType Index) const;

  /// Returns the child index of this node in its parent, if it has a parent,
  /// otherwise 0.
  AbsoluteSyntaxPosition::IndexInParentType getIndexInParent() const {
    return getAbsoluteRawRef().getPosition().getIndexInParent();
  }

  // MARK: - Retrieving source locations

  /// Get the offset at which the leading trivia of this node starts.
  AbsoluteOffsetPosition getAbsolutePositionBeforeLeadingTrivia() const;

  /// Get the offset at which the content of this node (excluding leading
  /// trivia) starts.
  AbsoluteOffsetPosition getAbsolutePositionAfterLeadingTrivia() const;

  /// Get the offset at which the content (excluding trailing trivia) of this
  /// node ends.
  AbsoluteOffsetPosition getAbsoluteEndPositionBeforeTrailingTrivia() const;

  /// Get the offset at chiwh the trailing trivia of this node ends.
  AbsoluteOffsetPosition getAbsoluteEndPositionAfterTrailingTrivia() const;

  // MARK: - Getting the node's kind

  /// Returns the kind of syntax node this is.
  SyntaxKind getKind() const { return getRawRef()->getKind(); }

  /// Returns true if the data node represents type syntax.
  bool isType() const { return getRawRef()->isType(); }

  /// Returns true if the data node represents statement syntax.
  bool isStmt() const { return getRawRef()->isStmt(); }

  /// Returns true if the data node represents declaration syntax.
  bool isDecl() const { return getRawRef()->isDecl(); }

  /// Returns true if the data node represents expression syntax.
  bool isExpr() const { return getRawRef()->isExpr(); }

  /// Returns true if the data node represents pattern syntax.
  bool isPattern() const { return getRawRef()->isPattern(); }

  /// Returns true if this syntax is of some "unknown" kind.
  bool isUnknown() const { return getRawRef()->isUnknown(); }

  // MARK: - Miscellaneous

  /// Dump a debug description of the syntax data for debugging to
  /// standard error.
  void dump(llvm::raw_ostream &OS) const;

  SWIFT_DEBUG_DUMP;
};

class SyntaxData : public SyntaxDataRef {

  SyntaxData(AbsoluteRawSyntax AbsoluteRaw, const SyntaxData &Parent)
      : SyntaxDataRef(AbsoluteRaw, RefCountedBox<SyntaxDataRef>::make(Parent)) {
  }

public:
  // MARK: - Creating new SyntaxData

  /// Create a \c SyntaxData for a tree's root (i.e. a node without a parent).
  SyntaxData(AbsoluteRawSyntax AbsoluteRaw, std::nullptr_t Parent)
      : SyntaxDataRef(AbsoluteRaw, nullptr) {}

  /// Cast a \c SyntaxDataRef to a \c SyntaxData. This requires that \c Ref is
  /// known to be reference counted.
  explicit SyntaxData(const SyntaxDataRef &Ref)
      : SyntaxDataRef(Ref.AbsoluteRaw, Ref.RefCountedParent) {
    assert(Ref.isRefCounted());
  }

  // MARK: - Retrieving underlying storage

  AbsoluteRawSyntax getAbsoluteRaw() const {
    return AbsoluteRawSyntax(getAbsoluteRawRef());
  }

  /// Returns the raw syntax node for this syntax node.
  const RC<RawSyntax> &getRaw() const { return getAbsoluteRaw().getRaw(); }

  // MARK: - Retrieving related nodes

  /// Return the parent syntax if there is one.
  Optional<SyntaxData> getParent() const {
    if (auto ParentRef = getParentRef()) {
      return SyntaxData(*ParentRef);
    } else {
      return None;
    }
  }

  /// Gets the child at the index specified by the provided cursor.
  template <typename CursorType>
  Optional<SyntaxData> getChild(CursorType Cursor) const {
    return getChild(
        (AbsoluteSyntaxPosition::IndexInParentType)cursorIndex(Cursor));
  }

  /// Gets the child at the specified \p Index.
  Optional<SyntaxData>
  getChild(AbsoluteSyntaxPosition::IndexInParentType Index) const;

  /// Get the node immediately before this current node that contains a
  /// non-missing token. Return \c None if we cannot find such node.
  Optional<SyntaxData> getPreviousNode() const;

  /// Get the node immediately after this current node that contains a
  /// non-missing token. Return \c None if we cannot find such node.
  Optional<SyntaxData> getNextNode() const;

  /// Get the first non-missing token node in this tree. Return \c None if
  /// this node does not contain non-missing tokens.
  Optional<SyntaxData> getFirstToken() const;

  /// Get the last non-missing token node in this tree. Return \c None if
  /// this node does not contain non-missing tokens.
  Optional<SyntaxData> getLastToken() const;

  // MARK: - Modifying node

  /// With a new \c RawSyntax node, create a new node from this one and
  /// recursively rebuild the parental chain up to the root.
  SyntaxData replacingSelf(const RC<RawSyntax> &NewRaw) const;

  /// Replace a child in the raw syntax and recursively rebuild the
  /// parental chain up to the root.
  template <typename CursorType>
  SyntaxData replacingChild(const RC<RawSyntax> &RawChild,
                            CursorType ChildCursor) const {
    auto NewRaw = getRaw()->replacingChild(ChildCursor, RawChild);
    return replacingSelf(NewRaw);
  }
};

} // end namespace syntax
} // end namespace swift

#endif // SWIFT_SYNTAX_SYNTAXDATA_H
