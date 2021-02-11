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
class TokenSyntax;

template <typename SyntaxNode> SyntaxNode makeRoot(RC<RawSyntax> Raw) {
  auto Data = SyntaxData(AbsoluteRawSyntax::forRoot(Raw), /*Parent=*/nullptr);
  return SyntaxNode(Data);
}

const auto NoParent = llvm::None;

// MARK: - SyntaxRef

/// The main handle for syntax nodes - subclasses contain all public
/// structured editing APIs.
///
/// Essentially, this is a wrapper around \c SyntaxDataRef that provides
/// convenience methods based on the node's kind. As such \c SyntaxRef can
/// either be reference-counted or unowned. See the comment on \c SyntaxDataRef
/// for more details.
class SyntaxRef {
protected:
  SyntaxDataRef Data;

public:
  explicit SyntaxRef(const SyntaxDataRef &Data) : Data(Data) {}

  virtual ~SyntaxRef() {}

  // MARK: - Get underlying data

  /// Get the Data for this Syntax node.
  const SyntaxDataRef &getDataRef() const { return Data; }

  /// Get the shared raw syntax.
  const RawSyntax *getRawRef() const { return getDataRef().getRawRef(); }

  /// Get the kind of syntax.
  SyntaxKind getKind() const { return getRawRef()->getKind(); }

  /// Get an ID for the \c RawSyntax node backing this \c Syntax which is
  /// stable across incremental parses.
  /// Note that this is different from the \c AbsoluteRawSyntax's \c NodeId,
  /// which uniquely identifies this node in the tree, but is not stable across
  /// incremental parses.
  SyntaxNodeId getId() const { return getRawRef()->getId(); }

  /// Return the number of bytes this node takes when spelled out in the source
  size_t getTextLength() const { return getRawRef()->getTextLength(); }

  // MARK: Parents/children

  /// Return the parent of this node, if it has one.
  llvm::Optional<SyntaxRef> getParentRef() const {
    if (auto ParentDataRef = getDataRef().getParentRef()) {
      return SyntaxRef(*ParentDataRef);
    } else {
      return None;
    }
  }

  /// Get the number of child nodes in this piece of syntax.
  size_t getNumChildren() const { return getDataRef().getNumChildren(); }

  /// Returns the child index of this node in its parent, if it has one,
  /// otherwise 0.
  CursorIndex getIndexInParent() const {
    return getDataRef().getIndexInParent();
  }

  /// Get the \p N -th child of this piece of syntax.
  llvm::Optional<SyntaxRef> getChildRef(const size_t N) const {
    if (auto ChildData = getDataRef().getChildRef(N)) {
      return SyntaxRef(*ChildData);
    } else {
      return None;
    }
  }

  // MARK: Position

  /// Get the offset at which the leading trivia of this node starts.
  AbsoluteOffsetPosition getAbsolutePositionBeforeLeadingTrivia() const {
    return getDataRef().getAbsolutePositionBeforeLeadingTrivia();
  }

  /// Get the offset at which the actual content (i.e. non-triva) of this node
  /// starts.
  AbsoluteOffsetPosition getAbsolutePositionAfterLeadingTrivia() const {
    return getDataRef().getAbsolutePositionAfterLeadingTrivia();
  }

  /// Get the offset at which the trailing trivia of this node starts.
  AbsoluteOffsetPosition getAbsoluteEndPositionBeforeTrailingTrivia() const {
    return getDataRef().getAbsoluteEndPositionBeforeTrailingTrivia();
  }

  /// Get the offset at which the trailing trivia of this node ends.
  AbsoluteOffsetPosition getAbsoluteEndPositionAfterTrailingTrivia() const {
    return getDataRef().getAbsoluteEndPositionAfterTrailingTrivia();
  }

  // MARK: - Get node kind

  /// Returns true if this syntax node represents a token.
  bool isToken() const { return getRawRef()->isToken(); }

  /// Returns true if this syntax node represents a statement.
  bool isStmt() const { return getRawRef()->isStmt(); }

  /// Returns true if this syntax node represents a declaration.
  bool isDecl() const { return getRawRef()->isDecl(); }

  /// Returns true if this syntax node represents an expression.
  bool isExpr() const { return getRawRef()->isExpr(); }

  /// Returns true if this syntax node represents a pattern.
  bool isPattern() const { return getRawRef()->isPattern(); }

  /// Returns true if this syntax node represents a type.
  bool isType() const { return getRawRef()->isType(); }

  /// Returns true if this syntax is of some "unknown" kind.
  bool isUnknown() const { return getRawRef()->isUnknown(); }

  /// Returns true if the node is "missing" in the source (i.e. it was
  /// expected (or optional) but not written.
  bool isMissing() const { return getRawRef()->isMissing(); }

  /// Returns true if the node is "present" in the source.
  bool isPresent() const { return getRawRef()->isPresent(); }

  // MARK: Casting

  /// Returns true if the syntax node is of the given type.
  template <typename T>
  bool is() const {
    return T::classof(this);
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
    } else {
      return None;
    }
  }

  static bool kindof(SyntaxKind Kind) {
    return true;
  }

  static bool classof(const SyntaxRef *S) {
    // Trivially true.
    return true;
  }

  // MARK: - Miscellaneous

  /// Print the syntax node with full fidelity to the given output stream.
  void print(llvm::raw_ostream &OS,
             SyntaxPrintOptions Opts = SyntaxPrintOptions()) const {
    if (auto Raw = getRawRef()) {
      Raw->print(OS, Opts);
    }
  }

  /// Print a debug representation of the syntax node to the given output stream
  /// and indentation level.
  void dump(llvm::raw_ostream &OS, unsigned Indent = 0) const {
    getRawRef()->dump(OS, Indent);
  }

  /// Print a debug representation of the syntax node to standard error.
  SWIFT_DEBUG_DUMP { getRawRef()->dump(); }

  bool hasSameIdentityAs(const SyntaxRef &Other) const {
    return getDataRef().getAbsoluteRawRef().getNodeId() ==
           Other.getDataRef().getAbsoluteRawRef().getNodeId();
  }
};

// MARK: - Syntax

/// A \c SyntaxRef that is guaranteed to always be reference-counted. See
/// comment on \c SyntaxRef.
class Syntax : public SyntaxRef {
  friend struct SyntaxFactory;

public:
  explicit Syntax(const SyntaxData &Data) : SyntaxRef(Data) {}

  virtual ~Syntax() {}

  // MARK: - Get underlying data

  /// Get the data for this Syntax node.
  SyntaxData getData() const { return SyntaxData(getDataRef()); }

  /// Get the shared raw syntax.
  RC<RawSyntax> getRaw() const { return getData().getRaw(); }

  /// Get the \p N -th child of this piece of syntax.
  llvm::Optional<Syntax> getChild(const size_t N) const;

  /// Get the first non-missing token in this node.
  Optional<TokenSyntax> getFirstToken() const;

  /// Get the last non-missing token in this node.
  Optional<TokenSyntax> getLastToken() const;

  /// Return the parent of this node, if it has one.
  llvm::Optional<Syntax> getParent() const;

  /// Get the node immediately before this current node that does contain a
  /// non-missing token. Return \c None if we cannot find such node.
  Optional<Syntax> getPreviousNode() const;

  /// Get the node immediately after this node that does contain a
  /// non-missing token. Return \c None if we cannot find such node.
  Optional<Syntax> getNextNode() const;

  // MARK: - Casting

  /// Returns true if the syntax node is of the given type.
  template <typename T>
  bool is() const {
    return T::classof(this);
  }

  /// Cast this Syntax node to a more specific type, asserting it's of the
  /// right kind.
  template <typename T>
  T castTo() const {
    assert(is<T>() && "castTo<T>() node of incompatible type!");
    return T(getData());
  }

  /// If this Syntax node is of the right kind, cast and return it,
  /// otherwise return None.
  template <typename T>
  llvm::Optional<T> getAs() const {
    if (is<T>()) {
      return castTo<T>();
    } else {
      return None;
    }
  }

  // MARK: - Miscellaneous

  /// Recursively visit this node.
  void accept(SyntaxVisitor &Visitor);
};

} // end namespace syntax
} // end namespace swift

#endif // SWIFT_SYNTAX_SYNTAX_H
