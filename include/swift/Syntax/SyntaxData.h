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
/// This is only for holding a strong reference to the RawSyntax, a weak
/// reference to the parent, and, in subclasses, lazily created strong
/// references to non-terminal child nodes.
class SyntaxData final
    : public llvm::ThreadSafeRefCountedBase<SyntaxData>,
      private llvm::TrailingObjects<SyntaxData, AtomicCache<SyntaxData>> {
  friend TrailingObjects;

  using RootDataPair = std::pair<RC<SyntaxData>, RC<SyntaxData>>;

  /// The shared raw syntax representing this syntax data node.
  const RC<RawSyntax> Raw;

  /// The parent of this syntax.
  ///
  /// WARNING! Do not access this directly. Use getParent(),
  /// which enforces nullptr checking.
  const SyntaxData *Parent;

  /// The index into the parent's child layout.
  ///
  /// If there is no parent, this is 0.
  const CursorIndex IndexInParent;

  /// Cache the absolute position of this node.
  Optional<AbsolutePosition> PositionCache;

  size_t numTrailingObjects(OverloadToken<AtomicCache<SyntaxData>>) const {
    return Raw->getNumChildren();
  }

  SyntaxData(RC<RawSyntax> Raw, const SyntaxData *Parent = nullptr,
             CursorIndex IndexInParent = 0)
      : Raw(Raw), Parent(Parent), IndexInParent(IndexInParent) {
    auto *I = getTrailingObjects<AtomicCache<SyntaxData>>();
    for (auto *E = I + getNumChildren(); I != E; ++I)
      ::new (static_cast<void *>(I)) AtomicCache<SyntaxData>();
  }

  /// With a new RawSyntax node, create a new node from this one and
  /// recursively rebuild the parental chain up to the root.
  ///
  /// DO NOT expose this as public API.
  RootDataPair replaceSelf(const RC<RawSyntax> NewRaw) const {
    if (hasParent()) {
      auto NewRootAndParent = Parent->replaceChild(NewRaw, IndexInParent);
      auto NewMe = NewRootAndParent.second->getChild(IndexInParent);
      return { NewRootAndParent.first, NewMe.get() };
    } else {
      auto NewMe = make(NewRaw, nullptr, IndexInParent);
      return { NewMe, NewMe.get() };
    }
  }

  /// Create the data for a child node with the raw syntax in our layout
  /// at the provided index.
  /// DO NOT expose this as public API.
  RC<SyntaxData> realizeSyntaxNode(CursorIndex Index) const {
    if (auto &RawChild = Raw->getChild(Index))
      return SyntaxData::make(RawChild, this, Index);
    return nullptr;
  }

  /// Replace a child in the raw syntax and recursively rebuild the
  /// parental chain up to the root.
  ///
  /// DO NOT expose this as public API.
  template <typename CursorType>
  RootDataPair replaceChild(const RC<RawSyntax> RawChild,
                            CursorType ChildCursor) const {
    auto NewRaw = Raw->replaceChild(ChildCursor, RawChild);
    return replaceSelf(NewRaw);
  }

  ArrayRef<AtomicCache<SyntaxData>> getChildren() const {
    return {getTrailingObjects<AtomicCache<SyntaxData>>(), getNumChildren()};
  }

public:
  /// Disable sized deallocation for SyntaxData, because it has tail-allocated
  /// data.
  void operator delete(void *p) { ::operator delete(p); }

  /// Get the node immediately before this current node that does contain a
  /// non-missing token. Return nullptr if we cannot find such node.
  RC<SyntaxData> getPreviousNode() const;

  /// Get the node immediately after this current node that does contain a
  /// non-missing token. Return nullptr if we cannot find such node.
  RC<SyntaxData> getNextNode() const;

  /// Get the first non-missing token node in this tree. Return nullptr if this
  /// node does not contain non-missing tokens.
  RC<SyntaxData> getFirstToken() const;

  ~SyntaxData() {
    for (auto &I : getChildren())
      I.~AtomicCache<SyntaxData>();
  }

  /// Constructs a SyntaxNode by replacing `self` and recursively building
  /// the parent chain up to the root.
  template <typename SyntaxNode>
  SyntaxNode replaceSelf(const RC<RawSyntax> NewRaw) const {
    auto NewRootAndData = replaceSelf(NewRaw);
    return { NewRootAndData.first, NewRootAndData.second.get() };
  }

  /// Replace a child in the raw syntax and recursively rebuild the
  /// parental chain up to the root.
  ///
  /// DO NOT expose this as public API.
  template <typename SyntaxNode, typename CursorType>
  SyntaxNode replaceChild(const RC<RawSyntax> RawChild,
                          CursorType ChildCursor) const {
    auto NewRootAndParent = replaceChild(RawChild, ChildCursor);
    return SyntaxNode {
      NewRootAndParent.first,
      NewRootAndParent.second.get()
    };
  }


  static RC<SyntaxData> make(RC<RawSyntax> Raw,
                             const SyntaxData *Parent = nullptr,
                             CursorIndex IndexInParent = 0);

  /// Returns the raw syntax node for this syntax node.
  const RC<RawSyntax> &getRaw() const {
    return Raw;
  }

  /// Returns the kind of syntax node this is.
  SyntaxKind getKind() const {
    return Raw->getKind();
  }

  /// Return the parent syntax if there is one.
  const SyntaxData * getParent() const {
    return Parent;
  }

  /// Returns true if this syntax node has a parent.
  bool hasParent() const {
    return Parent != nullptr;
  }

  /// Returns the child index of this node in its parent, if it has a parent,
  /// otherwise 0.
  size_t getIndexInParent() const {
    return IndexInParent;
  }

  /// Returns the number of children this SyntaxData represents.
  size_t getNumChildren() const {
    return Raw->getLayout().size();
  }

  /// Gets the child at the index specified by the provided cursor,
  /// lazily creating it if necessary.
  template <typename CursorType>
  RC<SyntaxData> getChild(CursorType Cursor) const {
    return getChild((size_t)cursorIndex(Cursor));
  }

  /// Gets the child at the specified index in this data's children array.
  /// Why do we need this?
  /// - SyntaxData nodes should have pointer identity.
  /// - We only want to construct parented, realized child nodes as
  ///   SyntaxData when asked.
  ///
  /// For example, if we have a ReturnStmtSyntax, and ask for its returned
  /// expression for the first time with getExpression(), two nodes can race
  /// to create and set the cached expression.
  ///
  /// Looking at an example - say we have a SyntaxData.
  ///
  /// SyntaxData = {
  ///   RC<RawSyntax> Raw = {
  ///     RC<RawTokenSyntax> { SyntaxKind::Token, tok::return_kw, "return" },
  ///     RC<RawSyntax> { SyntaxKind::SomeExpression, ... }
  ///   }
  ///   llvm::SmallVector<AtomicCache<SyntaxData>, 10> Children {
  ///     AtomicCache<SyntaxData> { RC<SyntaxData> = nullptr; },
  ///     AtomicCache<SyntaxData> { RC<SyntaxData> = nullptr; },
  ///   }
  /// }
  ///
  /// If we wanted to safely create the 0th child, an instance of TokenSyntax,
  /// then we ask the AtomicCache in that position to realize its value and
  /// cache it. This is safe because AtomicCache only ever mutates its cache
  /// one time -- the first initialization that wins a compare_exchange_strong.
  RC<SyntaxData> getChild(size_t Index) const {
    if (!getRaw()->getChild(Index))
      return nullptr;
    return getChildren()[Index].getOrCreate([&]() {
      return realizeSyntaxNode(Index);
    });
  }

  /// Calculate the absolute position of this node, use cache if the cache
  /// is populated.
  AbsolutePosition getAbsolutePosition() const;

  /// Calculate the absolute end position of this node, use cache of the immediate
  /// next node if populated.
  AbsolutePosition getAbsoluteEndPositionAfterTrailingTrivia() const;

  /// Get the absolute position without skipping the leading trivia of this
  /// node.
  AbsolutePosition getAbsolutePositionBeforeLeadingTrivia() const;

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

// DenseMapInfo for RC<SyntaxData>, used for a Syntax Node -> lib/AST mapping.
namespace llvm {
  using SD = swift::syntax::SyntaxData;
  using RCSD = swift::RC<SD>;
  template <> struct DenseMapInfo<RCSD> {
    static inline RCSD getEmptyKey() {
      return SD::make(nullptr, nullptr, 0);
    }
    static inline RCSD getTombstoneKey() {
        return SD::make(nullptr, nullptr, 0);
    }
    static unsigned getHashValue(const RCSD Value) {
      unsigned H = 0;
      H ^= DenseMapInfo<uintptr_t>::getHashValue(reinterpret_cast<const uintptr_t>(Value->getRaw().get()));
      H ^= DenseMapInfo<uintptr_t>::getHashValue(reinterpret_cast<const uintptr_t>(Value->getParent()));
      H ^= DenseMapInfo<swift::syntax::CursorIndex>::getHashValue(Value->getIndexInParent());
      return H;
    }
    static bool isEqual(const RCSD LHS, const RCSD RHS) {
      return LHS->getRaw().get() == RHS->getRaw().get() &&
             LHS->getParent() == RHS->getParent() &&
             LHS->getIndexInParent() == RHS->getIndexInParent();
    }
  };
}

#endif // SWIFT_SYNTAX_SYNTAXDATA_H
