//===--- Symbol.h - The generics rewrite system alphabet --------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "llvm/ADT/ArrayRef.h"

#ifndef SWIFT_RQM_SYMBOL_H
#define SWIFT_RQM_SYMBOL_H

namespace llvm {
  class raw_ostream;
}

namespace swift {

class CanType;
class ProtocolDecl;
class GenericTypeParamType;
class Identifier;
class LayoutConstraint;

namespace rewriting {

class MutableTerm;
class RewriteContext;
class Term;

/// The smallest element in the rewrite system.
///
/// enum Symbol {
///   case name(Identifier)
///   case protocol(Protocol)
///   case type([Protocol], Identifier)
///   case genericParam(index: Int, depth: Int)
///   case layout(LayoutConstraint)
///   case superclass(CanType, substitutions: [Term])
///   case concrete(CanType, substitutions: [Term])
/// }
///
/// For the concrete type symbols (`superclass` and `concrete`),
/// the type's structural components must either be concrete, or
/// generic parameters. All generic parameters must have a depth
/// of 0; the generic parameter index corresponds to an index in
/// the `substitutions` array.
///
/// For example, the superclass requirement
/// "T : MyClass<U.X, (Int) -> V.A.B>" is denoted with a symbol
/// structured as follows:
///
/// - type: MyClass<τ_0_0, (Int) -> τ_0_1>
/// - substitutions:
///   - U.X
///   - V.A.B
///
/// Out-of-line methods are documented in Symbol.cpp.
class Symbol final {
public:
  enum class Kind : uint8_t {
    //////
    ////// Special symbol kind that is both type-like and property-like:
    //////

    /// When appearing at the start of a term, denotes a nested
    /// type of a protocol 'Self' type.
    ///
    /// When appearing at the end of a term, denotes that the
    /// term's type conforms to the protocol.
    Protocol,

    //////
    ////// "Type-like" symbol kinds:
    //////

    /// An associated type [P:T] or [P&Q&...:T]. The parent term
    /// must be known to conform to P (or P, Q, ...).
    AssociatedType,

    /// A generic parameter, uniquely identified by depth and
    /// index. Can only appear at the beginning of a term, where
    /// it denotes a generic parameter of the top-level generic
    /// signature.
    GenericParam,

    /// An unbound identifier name.
    Name,

    //////
    ////// "Property-like" symbol kinds:
    //////

    /// When appearing at the end of a term, denotes that the
    /// term's type satisfies the layout constraint.
    Layout,

    /// When appearing at the end of a term, denotes that the term
    /// is a subclass of the superclass constraint.
    Superclass,

    /// When appearing at the end of a term, denotes that the term
    /// is exactly equal to the concrete type.
    ConcreteType,
  };

  static const unsigned NumKinds = 7;

  static const StringRef Kinds[];

private:
  friend class RewriteContext;

  struct Storage;

private:
  const Storage *Ptr;

  Symbol(const Storage *ptr) : Ptr(ptr) {}

public:
  Kind getKind() const;

  /// A property records something about a type term; either a protocol
  /// conformance, a layout constraint, or a superclass or concrete type
  /// constraint.
  bool isProperty() const {
    auto kind = getKind();
    return (kind == Symbol::Kind::Protocol ||
            kind == Symbol::Kind::Layout ||
            kind == Symbol::Kind::Superclass ||
            kind == Symbol::Kind::ConcreteType);
  }

  bool isSuperclassOrConcreteType() const {
    auto kind = getKind();
    return (kind == Kind::Superclass || kind == Kind::ConcreteType);
  }

  Identifier getName() const;

  const ProtocolDecl *getProtocol() const;

  ArrayRef<const ProtocolDecl *> getProtocols() const;

  GenericTypeParamType *getGenericParam() const;

  LayoutConstraint getLayoutConstraint() const;

  CanType getSuperclass() const;

  CanType getConcreteType() const;

  ArrayRef<Term> getSubstitutions() const;

  /// Returns an opaque pointer that uniquely identifies this symbol.
  const void *getOpaquePointer() const {
    return Ptr;
  }

  static Symbol fromOpaquePointer(void *ptr) {
    return Symbol((Storage *) ptr);
  }

  static Symbol forName(Identifier name,
                        RewriteContext &ctx);

  static Symbol forProtocol(const ProtocolDecl *proto,
                            RewriteContext &ctx);

  static Symbol forAssociatedType(const ProtocolDecl *proto,
                                  Identifier name,
                                  RewriteContext &ctx);

  static Symbol forAssociatedType(ArrayRef<const ProtocolDecl *> protos,
                                  Identifier name,
                                  RewriteContext &ctx);

  static Symbol forGenericParam(GenericTypeParamType *param,
                                RewriteContext &ctx);

  static Symbol forLayout(LayoutConstraint layout,
                          RewriteContext &ctx);

  static Symbol forSuperclass(CanType type,
                              ArrayRef<Term> substitutions,
                              RewriteContext &ctx);

  static Symbol forConcreteType(CanType type,
                                ArrayRef<Term> substitutions,
                                RewriteContext &ctx);

  ArrayRef<const ProtocolDecl *> getRootProtocols() const;

  int compare(Symbol other, const ProtocolGraph &protos) const;

  Symbol transformConcreteSubstitutions(
      llvm::function_ref<Term(Term)> fn,
      RewriteContext &ctx) const;

  Symbol prependPrefixToConcreteSubstitutions(
      const MutableTerm &prefix,
      RewriteContext &ctx) const;

  void dump(llvm::raw_ostream &out) const;

  friend bool operator==(Symbol lhs, Symbol rhs) {
    return lhs.Ptr == rhs.Ptr;
  }

  friend bool operator!=(Symbol lhs, Symbol rhs) {
    return !(lhs == rhs);
  }

  friend llvm::raw_ostream &operator<<(llvm::raw_ostream &out, Symbol symbol) {
    symbol.dump(out);
    return out;
  }
};

} // end namespace rewriting

} // end namespace swift

namespace llvm {
  template<> struct DenseMapInfo<swift::rewriting::Symbol> {
    static swift::rewriting::Symbol getEmptyKey() {
      return swift::rewriting::Symbol::fromOpaquePointer(
        llvm::DenseMapInfo<void *>::getEmptyKey());
    }
    static swift::rewriting::Symbol getTombstoneKey() {
      return swift::rewriting::Symbol::fromOpaquePointer(
        llvm::DenseMapInfo<void *>::getTombstoneKey());
    }
    static unsigned getHashValue(swift::rewriting::Symbol Val) {
      return DenseMapInfo<void *>::getHashValue(Val.getOpaquePointer());
    }
    static bool isEqual(swift::rewriting::Symbol LHS,
                        swift::rewriting::Symbol RHS) {
      return LHS == RHS;
    }
  };
} // end namespace llvm

#endif