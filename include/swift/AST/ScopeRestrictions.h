//===--- ScopeRestrictions.h - Scopes referenced from types -----*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Defines types to represent scopes during parsing and in the AST.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_SCOPERESTRICTIONS_H
#define SWIFT_AST_SCOPERESTRICTIONS_H

#include "swift/AST/Identifier.h"
#include "swift/Basic/Assertions.h"
#include "swift/Basic/Debug.h"
#include "swift/Basic/Located.h"
#include "swift/Basic/SourceLoc.h"
#include "llvm/ADT/FoldingSet.h"
#include "llvm/ADT/PointerIntPair.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Support/TrailingObjects.h"
#include "llvm/ADT/bit.h"
#include "llvm/Support/MathExtras.h"
#include <cstdint>
#include <type_traits>

namespace swift {

class AnyFunctionType;
class ASTContext;
class NominalTypeDecl;
class Type;

/// Wrapper for `Identifier`s which syntactically refer to scopes to reduce risk
/// of confusion / implementation bugs.
class ScopeName {
  Identifier Name;

public:
  explicit ScopeName(Identifier name) : Name(name) {}

  Identifier getIdentifier() const { return Name; }

  friend bool operator==(ScopeName lhs, ScopeName rhs) {
    return lhs.Name == rhs.Name;
  }
  friend bool operator!=(ScopeName lhs, ScopeName rhs) { return !(lhs == rhs); }
};

/// A scope as written, e.g. `&a`, `self`, or `immortal`, before resolution.
class ScopeRepr {
public:
  enum class Kind : uint8_t {
    /// A scope by name, e.g. `a`.
    ScopeName,
    /// The access of a value, e.g. `&a`.
    AccessedValue,
    /// `self`
    Self,
    /// `&self`
    AccessedSelf,
    /// `immortal`
    Immortal,
  };

private:
  /// The name (null unless the kind is `ScopeName` or `AccessedValue`) and the
  /// kind.
  llvm::PointerIntPair<Identifier, 3, Kind> Storage;
  SourceLoc Loc;

  ScopeRepr(Kind kind, Identifier name, SourceLoc loc)
      : Storage(name, kind), Loc(loc) {}

public:
  /// Create a scope that refers to a name, e.g. `a`
  static ScopeRepr forScopeName(Located<ScopeName> name) {
    return {Kind::ScopeName, name.Item.getIdentifier(), name.Loc};
  }

  /// Create a scope that describes the access of a value, e.g. `&a`
  static ScopeRepr forAccessedValue(Located<Identifier> name) {
    return {Kind::AccessedValue, name.Item, name.Loc};
  }

  static ScopeRepr forSelf(SourceLoc selfLoc, bool isAccess) {
    return {isAccess ? Kind::AccessedSelf : Kind::Self, Identifier(), selfLoc};
  }

  static ScopeRepr forImmortal(SourceLoc immortalLoc) {
    return {Kind::Immortal, Identifier(), immortalLoc};
  }

  Kind getKind() const { return Storage.getInt(); }

  ScopeName getScopeName() const {
    ASSERT(getKind() == Kind::ScopeName);
    return ScopeName(Storage.getPointer());
  }

  Identifier getAccessedValue() const {
    ASSERT(getKind() == Kind::AccessedValue);
    return Storage.getPointer();
  }

  SourceLoc getLoc() const { return Loc; }
};

/// A single scope restriction within a `@_scoped` attribute, e.g. `left: &a` in
/// `@_scoped(left: &a, right: b)`.
class ScopeRestrictionRepr {
  /// Null when not specifying a scope restriction by name, e.g. `array` in
  /// `@_scoped(array)`.
  Identifier LabelName;
  SourceLoc LabelLoc;
  ScopeRepr Scope;

public:
  ScopeRestrictionRepr(std::optional<Located<ScopeName>> label, ScopeRepr scope)
      : Scope(scope) {
    if (label) {
      LabelName = label->Item.getIdentifier();
      LabelLoc = label->Loc;
      ASSERT(!LabelName.empty());
    }
  }

  std::optional<Located<ScopeName>> getLabel() const {
    if (LabelName.empty())
      return std::nullopt;
    return Located<ScopeName>(ScopeName(LabelName), LabelLoc);
  }

  ScopeRepr getScope() const { return Scope; }
};

/// The scope parameters declared by a `@_scopeParams` attribute as written, e.g.
/// `a` and `b` in `@_scopeParams(a, b)`.
class ScopeParamListRepr final
    : private llvm::TrailingObjects<ScopeParamListRepr, Located<ScopeName>> {
  friend TrailingObjects;

  unsigned NumParams;

  explicit ScopeParamListRepr(ArrayRef<Located<ScopeName>> params);

public:
  static ScopeParamListRepr *create(const ASTContext &ctx,
                                    ArrayRef<Located<ScopeName>> params);

  ArrayRef<Located<ScopeName>> getParams() const {
    return getTrailingObjects(NumParams);
  }
};

/// The number of function types between a position in a type and an enclosing
/// binder. Only depths measured from the same position are comparable.
class ScopeDepth {
  // Packed into ScopeRef's data.
  friend class ScopeRef;

  unsigned Value;

  explicit constexpr ScopeDepth(unsigned value) : Value(value) {}

public:
  /// The binder's own parameters and result.
  static constexpr ScopeDepth zero() { return ScopeDepth(0); }

  /// One more function type between this position and the binder.
  ScopeDepth deeper() const { return ScopeDepth(Value + 1); }

  /// One fewer function type between this position and the binder.
  ScopeDepth shallower() const {
    ASSERT(Value != 0 && "no function type to leave");
    return ScopeDepth(Value - 1);
  }

  friend bool operator==(ScopeDepth lhs, ScopeDepth rhs) {
    return lhs.Value == rhs.Value;
  }
  friend bool operator!=(ScopeDepth lhs, ScopeDepth rhs) {
    return !(lhs == rhs);
  }

  /// The element at this depth from the top of \p binders, a stack of the
  /// enclosing binders indexed by de Bruijn level, or null if the binder is
  /// further out.
  template <typename Binders>
  auto lookUpIn(Binders &&binders) const -> decltype(&binders[0]) {
    if (Value >= binders.size())
      return nullptr;
    return &binders[binders.size() - 1 - Value];
  }
};

/// A reference to a scope from within a type.
///
/// Indexing is explicit `@_scopeParams`, then accesses of `&foo` parameters.
///
/// Uses de Bruijn indexing, not levels, so that it's cheap to pass function
/// types without rewriting.
class ScopeRef {
public:
  enum class Kind : uint8_t {
    /// The global scope, which outlives every other scope. Lives "sufficiently
    /// long". Escapable types have immortal scope.
    Immortal,
    /// A scope parameter of an enclosing type.
    Param,
    /// The scope for the body of a call to a given function. Represents the
    /// access scope of a parameter which isn't bound or referred to. Only known
    /// to live for the body of the call.
    Body,

    Last_Kind = Body
  };

private:
  static constexpr unsigned NumKindBits = 2;
  static constexpr unsigned NumDataBits = 32 - NumKindBits;
  static constexpr unsigned NumParamIndexBits = 15;
  static constexpr unsigned NumParamDepthBits = NumDataBits - NumParamIndexBits;
  static constexpr uint32_t ParamIndexMask =
      llvm::maskTrailingOnes<uint32_t>(NumParamIndexBits);
  static constexpr unsigned ParamDepthShift = NumParamIndexBits;
  static_assert(llvm::isUInt<NumKindBits>(unsigned(Kind::Last_Kind)),
                "unable to fit a ScopeRef::Kind in the given number of bits");

  uint32_t TheKind : NumKindBits;
  /// Interpreted according to the kind:
  /// - 32 of 0 for `Immortal`,
  /// - `NumParamDepthBits` then `NumParamIndexBits` for `Param`
  /// - `NumParamDepthBits` followed by `NumParamDepthBits` of 0 for `Body`
  uint32_t Data : NumDataBits;

  ScopeRef(Kind kind, uint32_t data) : TheKind(unsigned(kind)), Data(data) {
    ASSERT(llvm::isUInt<NumDataBits>(data) && "scope reference overflow");
  }

public:
  static ScopeRef forImmortal() { return {Kind::Immortal, 0}; }

  static ScopeRef forParam(ScopeDepth depth, unsigned index) {
    ASSERT(llvm::isUInt<NumParamDepthBits>(depth.Value) &&
           llvm::isUInt<NumParamIndexBits>(index) &&
           "scope reference overflow");
    return {Kind::Param, (depth.Value << ParamDepthShift) | index};
  }

  static ScopeRef forBody(ScopeDepth depth) {
    ASSERT(llvm::isUInt<NumParamDepthBits>(depth.Value) &&
           "scope reference overflow");
    return {Kind::Body, depth.Value << ParamDepthShift};
  }

  /// Create a Body scope at depth 0, to use as the default access scope of a
  /// parameter.
  static ScopeRef forCallBody() { return forBody(ScopeDepth::zero()); }

  Kind getKind() const { return Kind(TheKind); }

  /// The number of function types between this reference and its binder.
  ///
  /// ```
  /// func with(_ a: MyArray, _ body: (@_scoped(&a) MyRef) -> Void)
  /// ```
  ///
  /// Here `&a` has depth 1, since the type of `body` is between it and the
  /// type of `with`.
  ScopeDepth getDepth() const {
    ASSERT(getKind() != Kind::Immortal);
    return ScopeDepth(Data >> ParamDepthShift);
  }

  /// The index of the scope parameter in its binder.
  ///
  /// ```
  /// @_scopeParams(s, t)
  /// func foo(_ a: MyArray, _ b: MyArray, _ x: @_scoped(t) MyRef,
  ///          _ y: @_scoped(s) MyRef) -> @_scoped(&b) MyRef
  /// ```
  ///
  /// Here `s` and `t` have indexes 0 and 1, in the order they're declared, and
  /// `&b`, the access scope of `b`, has index 2, since the result refers to it.
  /// The accesses of `a`, `x`, and `y` are body scopes and don't have an index.
  unsigned getIndex() const {
    ASSERT(getKind() == Kind::Param);
    return Data & ParamIndexMask;
  }

  /// Reference the same scope from \p by more function types in.
  ///
  /// Immortal scopes are not shifted, since they are global.
  ScopeRef shifted(ScopeDepth by) const {
    switch (getKind()) {
    case Kind::Immortal:
      return *this;
    case Kind::Param:
      return forParam(ScopeDepth(getDepth().Value + by.Value), getIndex());
    case Kind::Body:
      return forBody(ScopeDepth(getDepth().Value + by.Value));
    }
    llvm_unreachable("unhandled kind");
  }

  /// Prints `^depth.index`, `^depth.body`, or `immortal`.
  void print(llvm::raw_ostream &os) const;
  SWIFT_DEBUG_DUMP;

  uint32_t getOpaqueValue() const { return llvm::bit_cast<uint32_t>(*this); }

  friend bool operator==(ScopeRef lhs, ScopeRef rhs) {
    return lhs.getOpaqueValue() == rhs.getOpaqueValue();
  }
  friend bool operator!=(ScopeRef lhs, ScopeRef rhs) { return !(lhs == rhs); }
};

// Not all uint32_t are inhabited for `ScopeRef` (like kind = 2+), but there
// shouldn't be any uninitialized bits! This way we can just bit_cast for
// getOpaqueValue.
static_assert(std::has_unique_object_representations_v<ScopeRef>,
              "equal ScopeRefs must have equal opaque values");

/// The scope parameters a function type expects from callers, which the scopes
/// in its parameters and result can refer to.
class ScopeSignature {
  unsigned NumParams = 0;

public:
  ScopeSignature() = default;
  explicit ScopeSignature(unsigned numParams) : NumParams(numParams) {}

  unsigned getNumParams() const { return NumParams; }

  bool empty() const { return NumParams == 0; }

  void Profile(llvm::FoldingSetNodeID &ID) const { ID.AddInteger(NumParams); }

  friend bool operator==(ScopeSignature lhs, ScopeSignature rhs) {
    return lhs.NumParams == rhs.NumParams;
  }
  friend bool operator!=(ScopeSignature lhs, ScopeSignature rhs) {
    return !(lhs == rhs);
  }
};

/// The number of scope parameters a value of the declared type is restricted
/// by.
unsigned getNumScopeParams(const NominalTypeDecl *decl);

/// Whether the type is or contains a scope-unsaturated type: a nominal type without
/// arguments for its declaration's scope parameters.
bool hasScopeUnsaturatedType(Type ty);

// FIXME: Types that differ only in scopes are reconciled after solving with a
// cast, which doesn't cover lvalues or function conversions yet.
/// \p ty without its scopes, which the constraint solver ignores: its nominal
/// types' scope arguments, and the access scopes of its function types'
/// parameters.
Type eraseScopes(Type ty);

} // end namespace swift

#endif // SWIFT_AST_SCOPERESTRICTIONS_H
