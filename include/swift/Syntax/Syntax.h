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
// subclasses for dealing with Swift Syntax. It essentially wraps
// SyntaxData(Ref) and provides convenience APIs (like retrieving children)
// based on the syntax kind.
//
// There are two versions of the Syntax type.
// SyntaxRef:
// SyntaxRef is designed around efficiency. It *does not* retain the
// SyntaxDataRef that stores its data - the user must gurantee that the
// SyntaxDataRef outlives the SyntaxRef that references it. Instead,
// SyntaxDataRef provides a *view* into the SyntaxDataRef and the view provides
// all convinience APIs. The advantage of this is that the underlying SyntaxData
// can be stack-allocated and does not need to be copied when the the SyntaxRef
// is being passsed around or when the SyntaxRef is being casted.
//
// Syntax:
// The syntax nodes are designed for memory safety. Syntax nodes always retain
// (and ref-count) heap-allocated SyntaxData nodes. While this provides maximum
// memory safety, the heap allocations and the ref-counting has a significant
// performance overhead.
//
// Note that the two access modes can also be mixed. When a syntax tree is
// accessed by Syntax (memory-safe) nodes, they can be demoted to SyntaxRef
// nodes to perform perfomance-critical tasks.
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

template <typename SyntaxNode>
SyntaxNode makeRoot(const RawSyntax *Raw) {
  auto Data = SyntaxData::makeRoot(AbsoluteRawSyntax::forRoot(Raw));
  return SyntaxNode(Data);
}

const auto NoParent = llvm::None;

/// Marker type to construct \c SyntaxRef nodes without validation. This is used
/// to create \c SyntaxRef inside \c OwnedSyntaxRef and \c
/// OptionalOwnedSyntaxRef that point to a \c SyntaxDataRef which is yet to be
/// initialised.
/// Validation will occur in these types once the \c SyntaxRef is accessed.
struct no_validation_t {};

// MARK: - OwnedSyntaxRef

/// Holds a \c SyntaxDataRef and provides a \c SyntaxRef (or one of its
/// subclasses) as an accessor to the \c SyntaxDataRef.
/// The user of this type needs to make sure that the \c OwnedSyntaxRef always
/// outlives the \c SyntaxRef provided by it, because otherwise the \c SyntaxRef
/// points to invalid memory.
/// It allows transparent access to the \c SyntaxRef through the \c -> operator.
///
/// All methods that return a \c OwnedSyntaxRef should be inlined to avoid
/// copying the \c SyntaxDataRef, which is rather expensive because the struct
/// is rather large.
///
/// A typical initialisation of a OwnedSyntaxRef looks as follows:
/// \code
/// OwnedSyntaxRef<MySyntaxRef> Result;
/// someSyntaxDataRef.getChildRef(Index, Result.getDataPtr());
/// \endcode
/// The first line creates an empty \c OwnedSyntaxRef with uninitialised memory.
/// The second line invokes a method that fills \c Data of \c OwnedSyntaxRef.
/// This way, we directly write the \c SyntaxDataRef to the correct memory
/// location and avoid copying it around.
template <typename SyntaxRefType>
class OwnedSyntaxRef {
  SyntaxDataRef Data;
  SyntaxRefType Ref;

public:
  /// Create an *uninintialized* \c OwnedSyntaxRef. Its storage needs to be
  /// initialised by writing a \c SyntaxDataRef to the pointer returned by
  /// \c getDataPtr()
  /// Implementation Note: We need to initialise \c Ref without validation,
  /// because \c Data is still uninitialised. \c Ref will be validated when
  /// accessed using \c getRef or \c -> .
  OwnedSyntaxRef() : Data(), Ref(getDataPtr(), no_validation_t()) {}

  OwnedSyntaxRef(const OwnedSyntaxRef &Other)
      : Data(Other.Data), Ref(getDataPtr(), no_validation_t()) {}
  OwnedSyntaxRef(OwnedSyntaxRef &&Other)
      : Data(std::move(Other.Data)), Ref(getDataPtr(), no_validation_t()) {}

  /// The pointer to the location at which \c this stores the \c Data.
  /// Can be used to retroactively populate the \c Data after \c OwnedSyntaxRef
  /// has been constructed with uninitialised memory.
  SyntaxDataRef *getDataPtr() { return &Data; }

  const SyntaxRefType &getRef() {
    assert(Ref.getDataRef() == getDataPtr() &&
           "Ref no longer pointing to Data?");
#ifndef NDEBUG
    // This might be the first access to Ref after Data has been modified.
    // Validate the node.
    Ref.validate();
#endif
    return Ref;
  }

  const SyntaxRefType *operator->() {
    assert(Ref.getDataRef() == getDataPtr() &&
           "Ref no longer pointing to Data?");
#ifndef NDEBUG
    // This might be the first access to Ref after Data has been modified.
    // Validate the node.
    Ref.validate();
#endif
    return &Ref;
  }
};

/// Same as \c OwnedSyntaxRef but can be null. We don't use \c
/// Optional<OwnedSyntaxRef<SyntaxRefType>>>, because then we couldn't access
/// the underlying \c SytnaxRefType via the \c -> operator (the use of \c ->
/// would access the \c OwnedSyntaxRef<SyntaxRefType> wrapped by \c Optional and
/// not the \c SyntaxRefType wrapped by \c OwnedSyntaxRef.
template <typename SyntaxRefType>
class OptionalOwnedSyntaxRef {
  Optional<SyntaxDataRef> Data;
  SyntaxRefType Ref;

public:
  OptionalOwnedSyntaxRef() : Data(), Ref(getDataPtr(), no_validation_t()) {}

  OptionalOwnedSyntaxRef(const OptionalOwnedSyntaxRef &Other)
      : Data(Other.Data), Ref(getDataPtr(), no_validation_t()) {}
  OptionalOwnedSyntaxRef(OptionalOwnedSyntaxRef &&Other)
      : Data(std::move(Other.Data)), Ref(getDataPtr(), no_validation_t()) {}

  SyntaxDataRef *getDataPtr() { return Data.getPointer(); }

  bool hasValue() const { return Data.hasValue(); }

  explicit operator bool() const { return hasValue(); }

  const SyntaxRefType &getRef() {
    assert(Ref.getDataRef() == getDataPtr() &&
           "Ref no longer pointing to Data?");
    assert(hasValue() && "Accessing a OptionalOwnedSyntaxRef without a value");
#ifndef NDEBUG
    // This might be the first access to Ref after Data has been populated.
    // Validate the node.
    Ref.validate();
#endif
    return Ref;
  }

  SyntaxRefType *operator->() {
    assert(Ref.getDataRef() == getDataPtr() &&
           "Ref no longer pointing to Data?");
    assert(hasValue() && "OptionalOwnedSyntaxRef doesn't have a value");
    return &Ref;
  }
};

// MARK: - Syntax

/// See comment on top of file.
class Syntax {
protected:
  const RC<const SyntaxData> Data;

public:
  explicit Syntax(const RC<const SyntaxData> &Data) : Data(Data) {
    assert(Data != nullptr && "Syntax must be backed by non-null Data");
  }

  /// Get the kind of syntax.
  SyntaxKind getKind() const;

  /// Get the shared raw syntax.
  const RawSyntax *getRaw() const;

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
  const RC<const SyntaxData> &getData() const { return Data; }

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
  CursorIndex getIndexInParent() const { return getData()->getIndexInParent(); }

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
    return Data->getAbsoluteRaw().getNodeId() ==
           Other.Data->getAbsoluteRaw().getNodeId();
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
    return Data->getAbsolutePositionBeforeLeadingTrivia();
  }

  /// Get the offset at which the actual content (i.e. non-triva) of this node
  /// starts.
  AbsoluteOffsetPosition getAbsolutePositionAfterLeadingTrivia() const {
    return Data->getAbsolutePositionAfterLeadingTrivia();
  }

  /// Get the offset at which the trailing trivia of this node starts.
  AbsoluteOffsetPosition getAbsoluteEndPositionBeforeTrailingTrivia() const {
    return Data->getAbsoluteEndPositionBeforeTrailingTrivia();
  }

  /// Get the offset at which the trailing trivia of this node starts.
  AbsoluteOffsetPosition getAbsoluteEndPositionAfterTrailingTrivia() const {
    return Data->getAbsoluteEndPositionAfterTrailingTrivia();
  }
};

// MARK: - SyntaxRef

/// See comment on top of file.
class SyntaxRef {
  const SyntaxDataRef * const Data;

public:
  /// Create a \c SyntaxRef and validate that the \p Data can actually represent
  /// a \c SyntaxRef. Validation in particular performs checks for derived
  /// types.
  explicit SyntaxRef(const SyntaxDataRef *Data) : Data(Data) {
    assert(Data != nullptr && "SyntaxRef must reference Data");
    this->validate();
  }
  SyntaxRef(const SyntaxDataRef *Data, no_validation_t) : Data(Data) {
    assert(Data != nullptr && "SyntaxRef must reference Data");
  }

  /// Demote a \c Syntax to a \c SyntaxRef
  SyntaxRef(const Syntax &Node) : SyntaxRef(Node.getData().get()) {}

  void validate() {}

  // MARK: - Get underlying data

  /// Get the \c SyntaxDataRef that stores the data of this \c SyntaxRef node.
  const SyntaxDataRef *getDataRef() const {
    return Data;
  }

  const AbsoluteRawSyntax &getAbsoluteRaw() const {
    return getDataRef()->getAbsoluteRaw();
  }

  /// Get the shared raw syntax.
  const RawSyntax *getRaw() const { return getDataRef()->getRaw(); }

  /// Get the kind of syntax.
  SyntaxKind getKind() const { return getRaw()->getKind(); }

  /// Return the number of bytes this node takes when spelled out in the source,
  /// including trivia.
  size_t getTextLength() const { return getRaw()->getTextLength(); }

  // MARK: Parents/children

  /// Return the parent of this node, if it has one, otherwise return \c None.
  llvm::Optional<SyntaxRef> getParentRef() const {
    if (auto ParentDataRef = getDataRef()->getParentRef()) {
      return SyntaxRef(ParentDataRef);
    } else {
      return None;
    }
  }

  /// Get the number of child nodes in this piece of syntax.
  size_t getNumChildren() const { return getDataRef()->getNumChildren(); }

  /// Returns the child index of this node in its parent, if it has one,
  /// otherwise 0.
  CursorIndex getIndexInParent() const {
    return getDataRef()->getIndexInParent();
  }

  /// Get the \p N -th child of this piece of syntax.
  OptionalOwnedSyntaxRef<SyntaxRef> getChildRef(const size_t N) const {
    OptionalOwnedSyntaxRef<SyntaxRef> Result;
    getDataRef()->getChildRef(N, Result.getDataPtr());
    return Result;
  }

  // MARK: Position

  /// Get the offset at which the leading trivia of this node starts.
  AbsoluteOffsetPosition getAbsolutePositionBeforeLeadingTrivia() const {
    return getDataRef()->getAbsolutePositionBeforeLeadingTrivia();
  }

  /// Get the offset at which the actual content (i.e. non-triva) of this node
  /// starts.
  AbsoluteOffsetPosition getAbsolutePositionAfterLeadingTrivia() const {
    return getDataRef()->getAbsolutePositionAfterLeadingTrivia();
  }

  /// Get the offset at which the trailing trivia of this node starts.
  AbsoluteOffsetPosition getAbsoluteEndPositionBeforeTrailingTrivia() const {
    return getDataRef()->getAbsoluteEndPositionBeforeTrailingTrivia();
  }

  /// Get the offset at which the trailing trivia of this node ends.
  AbsoluteOffsetPosition getAbsoluteEndPositionAfterTrailingTrivia() const {
    return getDataRef()->getAbsoluteEndPositionAfterTrailingTrivia();
  }

  // MARK: - Get node kind

  /// Returns true if this syntax node represents a token.
  bool isToken() const { return getRaw()->isToken(); }

  /// Returns true if this syntax node represents a statement.
  bool isStmt() const { return getRaw()->isStmt(); }

  /// Returns true if this syntax node represents a declaration.
  bool isDecl() const { return getRaw()->isDecl(); }

  /// Returns true if this syntax node represents an expression.
  bool isExpr() const { return getRaw()->isExpr(); }

  /// Returns true if this syntax node represents a pattern.
  bool isPattern() const { return getRaw()->isPattern(); }

  /// Returns true if this syntax node represents a type.
  bool isType() const { return getRaw()->isType(); }

  /// Returns true if this syntax is of some "unknown" kind.
  bool isUnknown() const { return getRaw()->isUnknown(); }

  /// Returns true if the node is "missing" in the source (i.e. it was
  /// expected (or optional) but not written.
  bool isMissing() const { return getRaw()->isMissing(); }

  /// Returns true if the node is "present" in the source.
  bool isPresent() const { return getRaw()->isPresent(); }

  // MARK: Casting

  /// Returns true if the syntax node is of the given type \p T.
  template <typename T>
  bool is() const {
    return T::classof(this);
  }

  /// Cast this Syntax node to a more specific type, asserting it's of the
  /// right kind \p T.
  template <typename T>
  T castTo() const {
    assert(is<T>() && "castTo<T>() node of incompatible type!");
    return T(getDataRef());
  }

  /// If this Syntax node is of the right kind \p T, cast and return it,
  /// otherwise return None.
  template <typename T>
  llvm::Optional<T> getAs() const {
    if (is<T>()) {
      return castTo<T>();
    } else {
      return None;
    }
  }

  static bool kindof(SyntaxKind Kind) { return true; }

  static bool classof(const SyntaxRef *S) {
    // Trivially true.
    return true;
  }

  // MARK: - Miscellaneous

  /// Print the syntax node with full fidelity to the given output stream.
  void print(llvm::raw_ostream &OS,
             SyntaxPrintOptions Opts = SyntaxPrintOptions()) const {
    if (auto Raw = getRaw()) {
      Raw->print(OS, Opts);
    }
  }

  /// Print a debug representation of the syntax node to the given output stream
  /// and indentation level.
  void dump(llvm::raw_ostream &OS, unsigned Indent = 0) const {
    getRaw()->dump(OS, Indent);
  }

  /// Print a debug representation of the syntax node to standard error.
  SWIFT_DEBUG_DUMP { getRaw()->dump(); }

  bool hasSameIdentityAs(const SyntaxRef &Other) const {
    return getDataRef()->getAbsoluteRaw().getNodeId() ==
           Other.getDataRef()->getAbsoluteRaw().getNodeId();
  }
};

} // end namespace syntax
} // end namespace swift

#endif // SWIFT_SYNTAX_SYNTAX_H
