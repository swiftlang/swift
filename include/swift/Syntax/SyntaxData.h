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
// This file defines the SyntaxData interface, the base type for the instance
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
// a SyntaxData reprsents *a* specific '1' at a particular location in
// Swift source.
//
// These are effectively internal implementation. For all public APIs, look
// for the type without "Data" in its name. For example, a StructDeclSyntaxData
// expresses its API through the wrapping StructDeclSyntax type.
//
//===----------------------------------------------------------------------===//


#ifndef SWIFT_SYNTAX_SYNTAXDATA_H
#define SWIFT_SYNTAX_SYNTAXDATA_H

#include "swift/Syntax/RawSyntax.h"
#include "swift/Syntax/References.h"
#include "swift/Syntax/Syntax.h"
#include "llvm/ADT/DenseMap.h"

#include <atomic>

namespace swift {
namespace syntax {

class Syntax;

/// The base class for holding parented syntax.
///
/// This structure and subclasses thereof should not contain significant public
/// API or internal modification API.
///
/// This is only for holding a strong reference to the RawSyntax, a weak
/// reference to the parent, and, in subclasses, lazily created strong
/// references to non-terminal child nodes.
class SyntaxData : public llvm::ThreadSafeRefCountedBase<SyntaxData> {
  friend struct SyntaxFactory;
#define SYNTAX(Id, Parent) friend class Id##Syntax;
#include "swift/Syntax/SyntaxKinds.def"
public:
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
protected:
  SyntaxData(RC<RawSyntax> Raw, const SyntaxData *Parent = nullptr,
             CursorIndex IndexInParent = 0)
    : Raw(Raw), Parent(Parent), IndexInParent(IndexInParent) {}

  /// With a new RawSyntax node, create a new node from this one and
  /// recursively rebuild the parental chain up to the root.
  ///
  /// DO NOT expose this as public API.
  template <typename SyntaxNode>
  SyntaxNode replaceSelf(RC<RawSyntax> NewRaw) const {
    auto NewMe = SyntaxNode::DataType::make(NewRaw, nullptr, IndexInParent);
    if (hasParent()) {
      auto NewRootAndParent = getParent().getValue()
        ->replaceChild<Syntax>(NewRaw, IndexInParent);
      NewMe->Parent = NewRootAndParent.Data;
      return SyntaxNode {
        NewRootAndParent.Root,
        cast<typename SyntaxNode::DataType>(NewMe.get())
      };
    } else {
      return SyntaxNode {
        NewMe,
        cast<typename SyntaxNode::DataType>(NewMe.get())
      };
    }
  }

  /// Unsafely instantiate a child within another node.
  ///
  /// DANGER!
  ///
  /// Scary thread-safe code here. This should only be used for internally
  /// mutating cached children!
  ///
  /// Why do we need this?
  /// - ___SyntaxData nodes should have pointer identity.
  /// - We only want to construct parented, realized child nodes as
  /// ___SyntaxData when asked.
  ///
  /// For example, if we have a ReturnStmtSyntax, and ask for its returned
  /// expression for the first time with getExpression(), two nodes can race
  /// to create and set the cached expression.
  ///
  ///
  /// Looking at an example - say we have a ReturnStmtSyntaxData.
  ///
  /// ReturnStmtSyntaxData = {
  ///   RC<RawSyntax> Raw = {
  ///     RC<TokenSyntax { SyntaxKind::Token, tok::return_kw, "return" },
  ///     RC<RawSyntax> { SyntaxKind::SomeExpression, ... }
  ///   }
  ///   RC<ExprSyntaxData> CachedExpression = { 0 };
  /// }
  ///
  /// We pretend that `CachedExpression` is a std::atomic<uintptr_t> &, so that
  /// we can safely instantiate that field using the RawSyntax for the
  /// expression, i.e. getRaw()->getChild(ReturnStmtSyntax::Cursor::Expression))
  template <typename SyntaxNode>
  static
  void realizeSyntaxNode(std::atomic<uintptr_t> &Child,
                         RC<RawSyntax> RawChild,
                         const SyntaxData *Parent,
                         CursorIndex IndexInParent) {
    // We rely on the fact that an RC<___SyntaxData> is pointer-sized, which
    // means we can atomically compare-exchange the child field.
    // If we can't do that, we can't pretend it's a uintptr_t and use its
    // compare_exchange_strong.
    static_assert(
      sizeof(uintptr_t) == sizeof(RC<typename SyntaxNode::DataType>),
      "Can't safely atomically replace a child SyntaxData node "
      "for caching! This is the end of the world!");

    if (Child == 0) {
      // We expect the uncached value to wrap a nullptr. If another thread
      // beats us to caching the child, it'll be non-null, so we would
      // leave it alone.
      uintptr_t Expected = 0;

      // Make a RC<SyntaxNode::DataType> at RefCount == 1, which we'll try to
      // atomically swap in.
      RC<typename SyntaxNode::DataType> Data =
        cast<typename SyntaxNode::DataType>(
          SyntaxData::makeDataFromRaw(RawChild, Parent, IndexInParent));

      // Try to swap in raw pointer value.
      auto SuccessfullySwapped =
        Child.compare_exchange_strong(Expected,
                                      reinterpret_cast<uintptr_t>(Data.get()));

      // If we won, then leave the RefCount == 1.
      if (SuccessfullySwapped) {
        Data.resetWithoutRelease();
      }

      // Otherwise, the Data we just made is unfortunately useless.
      // Let it die on this scope exit after its terminal release.
    }
  }

  /// Replace a child in the raw syntax and recursively rebuild the
  /// parental chain up to the rooet.
  ///
  /// This is the effective private implementation of all setters in
  /// subclasses of `SyntaxData`.
  ///
  /// DO NOT expose this as public API.
  template <typename SyntaxNode, typename CursorType>
  SyntaxNode replaceChild(RC<RawSyntax> RawChild,
                          CursorType ChildCursor) const {
    auto NewRaw = Raw->replaceChild(ChildCursor, RawChild);
    return replaceSelf<SyntaxNode>(NewRaw);
  }

public:

  static RC<SyntaxData> makeDataFromRaw(RC<RawSyntax> Raw,
                                        const SyntaxData *Parent,
                                        CursorIndex IndexInParent);

  static RC<SyntaxData> make(RC<RawSyntax> Raw,
                             const SyntaxData *Parent = nullptr,
                             CursorIndex IndexInParent = 0);

  /// Returns the raw syntax node for this syntax node.
  const RC<RawSyntax> getRaw() const {
    return Raw;
  }

  /// Returns the kind of syntax node this is.
  SyntaxKind getKind() const {
    return Raw->Kind;
  }

  /// Return the parent syntax if there is one.
  llvm::Optional<const SyntaxData *> getParent() const {
    if (Parent != nullptr) {
      return Parent;
    }
    return NoParent;
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

  /// Returns true if the data node represents type syntax.
  bool isType() const;

  /// Returns true if the data node represents statement syntax.
  bool isStmt() const;

  /// Returns true if the data node represents declaration syntax.
  bool isDecl() const;

  /// Returns true if the data node represents expression syntax.
  bool isExpr() const;

  /// Returns true if this syntax is of some "unknown" kind.
  bool isUnknown() const;

  /// Dump a debug description of the syntax data for debugging to
  /// standard error.
  void dump(llvm::raw_ostream &OS) const;
};

} // end namespace syntax
} // end namespace swift

// DenseMapInfo for RC<SyntaxData>, used for a Syntax Node -> lib/AST mapping.
namespace llvm {
  using SD = swift::syntax::SyntaxData;
  using RCSD = swift::syntax::RC<SD>;
  template <> struct llvm::DenseMapInfo<RCSD> {
    static inline RCSD getEmptyKey() {
      return SD::make(nullptr, nullptr, 0);
    }
    static inline RCSD getTombstoneKey() {
        return SD::make(nullptr, nullptr, 0);
    }
    static unsigned getHashValue(const RCSD Value) {
      unsigned H = 0;
      H ^= DenseMapInfo<uintptr_t>::getHashValue(reinterpret_cast<const uintptr_t>(Value->Raw.get()));
      H ^= DenseMapInfo<uintptr_t>::getHashValue(reinterpret_cast<const uintptr_t>(Value->Parent));
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

