//===--- Syntax.h - Swift Syntax Interface ----------------------*- C++ -*-===//
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
// This file defines the Syntax type, the main public-facing classes and
// subclasses for dealing with Swift Syntax.
//
// Syntax types contain a strong reference to the root of the tree to keep
// the subtree above alive, and a weak reference to the data representing
// the syntax node (weak to prevent retain cycles). All significant public API
// are contained in Syntax and its subclasses.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SYNTAX_SYNTAX_H
#define SWIFT_SYNTAX_SYNTAX_H

#include "swift/Syntax/References.h"
#include "swift/Syntax/RawSyntax.h"
#include "swift/Syntax/Trivia.h"
#include "llvm/ADT/IntrusiveRefCntPtr.h"
#include "llvm/ADT/Optional.h"
#include "llvm/Support/raw_ostream.h"

namespace swift {
namespace sema {
  class Semantics;
}
namespace syntax {

const auto NoParent = llvm::None;

class SyntaxData;
class UnknownSyntaxData;

/// The main handle for syntax nodes - subclasses contain all public
/// structured editing APIs.
///
/// This opaque structure holds two pieces of data: a strong reference to a
/// root node and a weak reference to the node itself. The node of interest can
/// be weakly held because the data nodes contain strong references to
/// their children.
class Syntax {
  friend struct SyntaxFactory;
  friend class SyntaxData;
  friend class LegacyASTTransformer;
  friend class sema::Semantics;
  using DataType = SyntaxData;

#define SYNTAX(Id, Parent) friend class Id##Syntax;
#include "swift/Syntax/SyntaxKinds.def"

protected:
  /// A strong reference to the root node of the tree in which this piece of
  /// syntax resides.
  const RC<SyntaxData> Root;

  /// A raw pointer to the data representing this syntax node.
  ///
  /// This is mutable for being able to set cached child members, which are
  /// lazily created.
  mutable const SyntaxData *Data;

  template <typename SyntaxNode>
  typename SyntaxNode::DataType *getUnsafeData() const {
    auto Casted = cast<typename SyntaxNode::DataType>(Data);
    return const_cast<typename SyntaxNode::DataType *>(Casted);
  }

public:
  Syntax(const RC<SyntaxData> Root, const SyntaxData *Data);

  /// Get the kind of syntax.
  SyntaxKind getKind() const;

  /// Get the shared raw syntax.
  RC<RawSyntax> getRaw() const;

  /// Returns true if the syntax node is of the given type.
  template <typename T>
  bool is() const {
    return T::classof(this);
  }

  /// Get the Data for this Syntax node.
  template <typename T>
  typename T::DataType &getData() const {
    assert(is<T>() && "getData<T>() node of incompatible type!");
    return *reinterpret_cast<typename T::DataType *>(Data);
  }

  const SyntaxData *getDataPointer() const {
    return Data;
  }

  /// Cast this Syntax node to a more specific type, asserting it's of the
  /// right kind.
  template <typename T>
  T castTo() const {
    assert(is<T>() && "castTo<T>() node of incompatible type!");
    return T { Root, reinterpret_cast<const typename T::DataType *>(Data) };
  }

  /// If this Syntax node is of the right kind, cast and return it,
  /// otherwise return None.
  template <typename T>
  llvm::Optional<T> getAs() const {
    if (is<T>()) {
      return castTo<T>();
    }
    return llvm::None;
  }

  /// Return the parent of this node, if it has one.
  llvm::Optional<Syntax> getParent() const;

  /// Returns the child index of this node in its parent,
  /// if it has one, otherwise 0.
  CursorIndex getIndexInParent() const;

  /// Returns true if this syntax node represents a statement.
  bool isStmt() const;

  /// Returns true if this syntax node represents a declaration.
  bool isDecl() const;

  /// Returns true if this syntax node represents an expression.
  bool isExpr() const;

  /// Returns true if this syntax node represents a type.
  bool isType() const;

  /// Print the syntax node with full fidelity to the given output stream.
  void print(llvm::raw_ostream &OS) const;

  /// Print a debug representation of the syntax node to the given output stream
  /// and indentation level.
  void dump(llvm::raw_ostream &OS, unsigned Indent = 0) const;

  /// Print a debug representation of the syntax node to standard error.
  void dump() const;

  bool hasSameIdentityAs(const Syntax &Other) {
    return Root == Other.Root && Data == Other.Data;
  }

  // TODO: hasSameStructureAs ?
};

/// A chunk of "unknown" syntax - effectively a sequence of tokens.
class UnknownSyntax final : public Syntax {
  friend struct SyntaxFactory;

  UnknownSyntax(const RC<SyntaxData> Root, UnknownSyntaxData *Data);
  static UnknownSyntax make(RC<RawSyntax> Raw);

public:
  static bool classof(const Syntax *S) {
    return S->getKind() == SyntaxKind::Unknown;
  }
};

} // end namespace syntax
} // end namespace swift

#endif // SWIFT_SYNTAX_SYNTAX_H
