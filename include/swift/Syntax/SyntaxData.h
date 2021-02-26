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
/// \c SyntaxDataRef is designed for efficiency, not memory safety.
/// It *does not* retain the \c RawSyntax that backs the syntax node, the \c
/// SyntaxArena in which the \c RawSyntax node lives nor the parent \c
/// SyntaxDataRef node. It is the user's responsibility to ensure these values
/// outlive the \c SyntaxDataRef.
///
/// Always use \c SyntaxData if performance is not a concern and memory safety
/// should be guaranteed.
class SyntaxDataRef {
  friend class SyntaxData;

protected:
  /// The \c AbsoluteRawSyntaxNode that provides the majority of this node's
  /// data. The underlying \c RawSyntax node is *not* retained by the \c
  /// SyntaxDataRef. It is the user's responsibility to ensure that the \c
  /// RawSyntax's \c SyntaxArena outlives this \c SyntaxDataRef.
  ///
  /// In \c SyntaxData, the backing \c SyntaxArena is retained via the \c Arena
  /// property, lifiting the responsibility to guarantee the \c RawSyntax node
  /// stays alive from the user.
  const AbsoluteRawSyntax AbsoluteRaw;

  /// The parent of this node. The parent is *not* retained by the \c
  /// SyntaxDataRef. It is the user's responsibility to ensure that the parent
  /// always outlives the child.
  ///
  /// If this is a \c SyntaxData object, \c Parent is guaranteed to also be a
  /// \c SyntaxData object, which is manually retained when \c this is created
  /// and released when \c this is destroyed.
  const SyntaxDataRef *Parent;

#ifndef NDEBUG
  /// In debug builds, a flag that specifies if this instance is a \c
  /// SyntaxDataRef or a \c SyntaxData object.
  bool IsRef = true;
#endif

  /// Creates an empty \c SyntaxDataRef variable to which values can later be
  /// stored.
  SyntaxDataRef() : AbsoluteRaw() {}

  /// Creates a \c SyntaxDataRef. \p AbsoluteRaw must not be null. If \p Parent
  /// is a \c nullptr, this node represents the root of a tree.
  SyntaxDataRef(const AbsoluteRawSyntax &AbsoluteRaw,
                const SyntaxDataRef *Parent)
      : AbsoluteRaw(AbsoluteRaw), Parent(Parent) {
    assert(!AbsoluteRaw.isNull() &&
           "SyntaxDataRef must always have a value. Use default constructor to "
           "create a SyntaxDataRef to be populated later.");
  }

public:
  SyntaxDataRef(const SyntaxDataRef &DataRef) = default;
  SyntaxDataRef(SyntaxDataRef &&DataRef) = default;

  // MARK: - Retrieving underlying data

  /// Whether this is a null node created by the default constructor, and will
  /// be populated later.
  bool isNull() const { return AbsoluteRaw.isNull(); }

  const AbsoluteRawSyntax &getAbsoluteRaw() const {
    assert(!isNull() && "Cannot get AbsoluteRaw of a null SyntaxDataRef");
    return AbsoluteRaw;
  }

  /// Returns the raw syntax node for this syntax node.
  const RawSyntax *getRaw() const { return getAbsoluteRaw().getRaw(); }

  // MARK: - Retrieving related nodes

  const SyntaxDataRef *getParentRef() const {
    assert(!isNull() && "Cannot get Parent of a null SyntaxDataRef");
    return Parent;
  }

  /// Returns true if this syntax node has a parent.
  bool hasParent() const { return getParentRef() != nullptr; }

  /// Returns the number of children this SyntaxData has.
  size_t getNumChildren() const { return getRaw()->getLayout().size(); }

  /// Returns the child index of this node in its parent, if it has a parent,
  /// otherwise 0.
  AbsoluteSyntaxPosition::IndexInParentType getIndexInParent() const {
    return getAbsoluteRaw().getPosition().getIndexInParent();
  }

  // MARK: - Retrieving source locations

  /// Get the offset at which the leading trivia of this node starts.
  AbsoluteOffsetPosition getAbsolutePositionBeforeLeadingTrivia() const {
    return getAbsoluteRaw().getPosition();
  }

  /// Get the offset at which the content of this node (excluding leading
  /// trivia) starts.
  AbsoluteOffsetPosition getAbsolutePositionAfterLeadingTrivia() const {
    if (auto FirstToken = getAbsoluteRaw().getFirstToken()) {
      return getAbsolutePositionBeforeLeadingTrivia().advancedBy(
          FirstToken->getRaw()->getLeadingTriviaLength());
    } else {
      return getAbsolutePositionBeforeLeadingTrivia();
    }
  }

  /// Get the offset at which the content (excluding trailing trivia) of this
  /// node ends.
  AbsoluteOffsetPosition getAbsoluteEndPositionBeforeTrailingTrivia() const {
    if (auto LastToken = getAbsoluteRaw().getLastToken()) {
      return getAbsoluteEndPositionAfterTrailingTrivia().advancedBy(
          -LastToken->getRaw()->getTrailingTriviaLength());
    } else {
      return getAbsoluteEndPositionAfterTrailingTrivia();
    }
  }

  /// Get the offset at chiwh the trailing trivia of this node ends.
  AbsoluteOffsetPosition getAbsoluteEndPositionAfterTrailingTrivia() const {
    return getAbsolutePositionBeforeLeadingTrivia().advancedBy(
        getRaw()->getTextLength());
  }

  // MARK: - Getting the node's kind

  /// Returns which kind of syntax node this is.
  SyntaxKind getKind() const { return getRaw()->getKind(); }

  /// Returns true if the data node represents type syntax.
  bool isType() const { return getRaw()->isType(); }

  /// Returns true if the data node represents statement syntax.
  bool isStmt() const { return getRaw()->isStmt(); }

  /// Returns true if the data node represents declaration syntax.
  bool isDecl() const { return getRaw()->isDecl(); }

  /// Returns true if the data node represents expression syntax.
  bool isExpr() const { return getRaw()->isExpr(); }

  /// Returns true if the data node represents pattern syntax.
  bool isPattern() const { return getRaw()->isPattern(); }

  /// Returns true if this syntax is of some "unknown" kind.
  bool isUnknown() const { return getRaw()->isUnknown(); }

  // MARK: - Miscellaneous

  /// Dump a debug description of the syntax data for debugging to
  /// standard error.
  void dump(llvm::raw_ostream &OS) const;

  SWIFT_DEBUG_DUMP;
};

class SyntaxData final : public SyntaxDataRef {
  /// If this node is the root of a Syntax tree (i.e. \c Parent is \c nullptr ),
  /// the arena in which this node's \c RawSyntax node has been allocated.
  /// This keeps this \c RawSyntax nodes referenced by this tree alive.
  /// If \c Parent has a value, this is always \c nullptr.
  RC<SyntaxArena> Arena;

  mutable std::atomic<int> RefCount{0};

  /// Create a non-root node with a \p Parent.
  SyntaxData(const AbsoluteRawSyntax &AbsoluteRaw,
             const RC<const SyntaxData> &Parent)
      : SyntaxDataRef(AbsoluteRaw, Parent.get()), Arena(nullptr) {
    assert(
        Parent != nullptr &&
        "Use SyntaxData(AbsoluteRawSyntax) or makeRoot to create a root node.");
    assert(!Parent->IsRef &&
           "Cannot create a SyntaxData as a child of a SyntaxDataRef");
    Parent->Retain();
#ifndef NDEBUG
    IsRef = false;
#endif
  }

  /// Create a new root node.
  SyntaxData(const AbsoluteRawSyntax &AbsoluteRaw)
      : SyntaxDataRef(AbsoluteRaw, nullptr),
        Arena(AbsoluteRaw.getRaw()->getArena()) {
#ifndef NDEBUG
    IsRef = false;
#endif
  }

public:
  SyntaxData(const SyntaxData &DataRef)
      : SyntaxDataRef(DataRef.AbsoluteRaw, DataRef.Parent),
        Arena(DataRef.Arena) {
    if (auto Parent = getParent()) {
      assert(!Parent->IsRef &&
             "Parent of a SyntaxData node should always be a SyntaxData node");
      Parent->Retain();
    }
  }

  ~SyntaxData() {
    assert(RefCount == 0 &&
           "Destruction occured when there are still references to this.");
    if (auto Parent = getParent()) {
      Parent->Release();
    }
  }

  /// Make a new \c SyntaxData node for the tree's root.
  static RC<const SyntaxData> makeRoot(const AbsoluteRawSyntax &AbsoluteRaw) {
    return RC<const SyntaxData>(new SyntaxData(AbsoluteRaw));
  }

  void Retain() const { RefCount.fetch_add(1, std::memory_order_relaxed); }

  void Release() const {
    int NewRefCount = RefCount.fetch_sub(1, std::memory_order_acq_rel) - 1;
    assert(NewRefCount >= 0 && "Reference count was already zero.");
    if (NewRefCount == 0) {
      delete this;
    }
  }

  // MARK: - Retrieving related nodes

  /// Return the parent syntax if there is one, otherwise return \c nullptr.
  RC<const SyntaxData> getParent() const {
    if (auto ParentRef = getParentRef()) {
      assert(!ParentRef->IsRef &&
             "Parent of a SyntaxData node should always be a SyntaxData node");
      return RC<const SyntaxData>(static_cast<const SyntaxData *>(ParentRef));
    } else {
      return nullptr;
    }
  }

  /// Gets the child at the index specified by the provided cursor if there is
  /// one, otherwise returns \c nullptr.
  template <typename CursorType>
  RC<const SyntaxData> getChild(CursorType Cursor) const {
    return getChild(
        (AbsoluteSyntaxPosition::IndexInParentType)cursorIndex(Cursor));
  }

  /// Gets the child at the specified \p Index  if there is one, otherwise
  /// returns \c nullptr.
  RC<const SyntaxData>
  getChild(AbsoluteSyntaxPosition::IndexInParentType Index) const;

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

  // MARK: - Modifying node

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
};

} // end namespace syntax
} // end namespace swift

#endif // SWIFT_SYNTAX_SYNTAXDATA_H
