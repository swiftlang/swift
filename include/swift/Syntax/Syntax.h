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

#include "swift/Basic/Debug.h"
#include "swift/Syntax/SyntaxData.h"
#include "swift/Syntax/References.h"
#include "swift/Syntax/RawSyntax.h"
#include "swift/Syntax/Trivia.h"
#include "llvm/ADT/IntrusiveRefCntPtr.h"
#include "llvm/ADT/Optional.h"
#include "llvm/Support/raw_ostream.h"

namespace swift {

namespace syntax {

struct SyntaxVisitor;
class SourceFileSyntax;

template <typename SyntaxNode> SyntaxNode makeRoot(RC<RawSyntax> Raw) {
  auto Data = SyntaxData::make(AbsoluteRawSyntax::forRoot(Raw));
  return SyntaxNode(Data);
}

const auto NoParent = llvm::None;

/// The main handle for syntax nodes - subclasses contain all public
/// structured editing APIs.
///
/// Essentially, this is a wrapper around \c SyntaxData that provides
/// convenience methods based on the node's kind.
class Syntax {
  friend struct SyntaxFactory;

protected:
  SyntaxData Data;

public:
  explicit Syntax(const SyntaxData Data) : Data(Data) {}

  virtual ~Syntax() {}

  /// Get the kind of syntax.
  SyntaxKind getKind() const;

  /// Get the shared raw syntax.
  const RC<RawSyntax> &getRaw() const;

  /// Get an ID for this node that is stable across incremental parses
  SyntaxNodeId getId() const { return getRaw()->getId(); }

  /// Get the number of child nodes in this piece of syntax, not including
  /// tokens.
  size_t getNumChildren() const;

  /// Get the Nth child of this piece of syntax.
  llvm::Optional<Syntax> getChild(const size_t N) const;

  /// Returns true if the syntax node is of the given type.
  template <typename T>
  bool is() const {
    return T::classof(this);
  }

  /// Get the Data for this Syntax node.
  const SyntaxData &getData() const {
    return Data;
  }

  /// Cast this Syntax node to a more specific type, asserting it's of the
  /// right kind.
  template <typename T>
  T castTo() const {
    assert(is<T>() && "castTo<T>() node of incompatible type!");
    return T(Data);
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
  CursorIndex getIndexInParent() const { return getData().getIndexInParent(); }

  /// Return the number of bytes this node takes when spelled out in the source
  size_t getTextLength() const { return getRaw()->getTextLength(); }

  /// Returns true if this syntax node represents a token.
  bool isToken() const;

  /// Returns true if this syntax node represents a statement.
  bool isStmt() const;

  /// Returns true if this syntax node represents a declaration.
  bool isDecl() const;

  /// Returns true if this syntax node represents an expression.
  bool isExpr() const;

  /// Returns true if this syntax node represents a pattern.
  bool isPattern() const;

  /// Returns true if this syntax node represents a type.
  bool isType() const;

  /// Returns true if this syntax is of some "unknown" kind.
  bool isUnknown() const;

  /// Returns true if the node is "missing" in the source (i.e. it was
  /// expected (or optional) but not written.
  bool isMissing() const;

  /// Returns true if the node is "present" in the source.
  bool isPresent() const;

  /// Print the syntax node with full fidelity to the given output stream.
  void print(llvm::raw_ostream &OS, SyntaxPrintOptions Opts = SyntaxPrintOptions()) const;

  /// Print a debug representation of the syntax node to the given output stream
  /// and indentation level.
  void dump(llvm::raw_ostream &OS, unsigned Indent = 0) const;

  /// Print a debug representation of the syntax node to standard error.
  SWIFT_DEBUG_DUMP;

  bool hasSameIdentityAs(const Syntax &Other) const {
    return Data.getAbsoluteRaw().getNodeId() ==
           Other.Data.getAbsoluteRaw().getNodeId();
  }

  static bool kindof(SyntaxKind Kind) {
    return true;
  }

  static bool classof(const Syntax *S) {
    // Trivially true.
    return true;
  }

  /// Recursively visit this node.
  void accept(SyntaxVisitor &Visitor);

  /// Same as \c getAbsolutePositionAfterLeadingTrivia.
  AbsoluteOffsetPosition getAbsolutePosition() const {
    return getAbsolutePositionAfterLeadingTrivia();
  }

  /// Get the offset at which the leading trivia of this node starts.
  AbsoluteOffsetPosition getAbsolutePositionBeforeLeadingTrivia() const {
    return Data.getAbsolutePositionBeforeLeadingTrivia();
  }

  /// Get the offset at which the actual content (i.e. non-triva) of this node
  /// starts.
  AbsoluteOffsetPosition getAbsolutePositionAfterLeadingTrivia() const {
    return Data.getAbsolutePositionAfterLeadingTrivia();
  }

  /// Get the offset at which the trailing trivia of this node starts.
  AbsoluteOffsetPosition getAbsoluteEndPositionBeforeTrailingTrivia() const {
    return Data.getAbsoluteEndPositionBeforeTrailingTrivia();
  }

  /// Get the offset at which the trailing trivia of this node starts.
  AbsoluteOffsetPosition getAbsoluteEndPositionAfterTrailingTrivia() const {
    return Data.getAbsoluteEndPositionAfterTrailingTrivia();
  }

  // TODO: hasSameStructureAs ?
};

} // end namespace syntax
} // end namespace swift

#endif // SWIFT_SYNTAX_SYNTAX_H
