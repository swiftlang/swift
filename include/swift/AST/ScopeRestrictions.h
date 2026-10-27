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
#include "llvm/ADT/PointerIntPair.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Support/TrailingObjects.h"
#include "llvm/ADT/bit.h"
#include "llvm/Support/MathExtras.h"
#include <cstdint>
#include <type_traits>

namespace swift {

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
};

/// A reference to a scope from within a type.
///
/// DISCUSSION: Indexing is parameters (for access scopes) followed by scopes
/// within types ordered by first occurrence of that type. `consuming`
/// parameters increment the index but their "access scope" does not exist and
/// won't be referenced, since they are owned.
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

    Last_Kind = Param
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
  /// Interpreted according to the kind: zero for `Immortal`, and the depth
  /// above the index for `Param`.
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
    ASSERT(getKind() == Kind::Param);
    return ScopeDepth(Data >> ParamDepthShift);
  }

  /// The index of the scope parameter in its binder.
  ///
  /// ```
  /// func foo(_ a: MyArray, _ x: MyRef, _ y: MyRef) -> @_scoped(&a) MyRef
  /// // access: &0            &1 ~~~~~    &2 ~~~~~
  /// //  value:                    3           4
  /// ```
  ///
  /// Here `&a`, the access scope of `a`, has index 0. The scopes of `x` and `y`
  /// have indexes 3 and 4 (assuming `MyRef` only contains one scope), after the
  /// access scopes of all three parameters. MyArray does not contain any
  /// scopes, since it is `Escapable`.
  unsigned getIndex() const {
    ASSERT(getKind() == Kind::Param);
    return Data & ParamIndexMask;
  }

  /// Reference the same scope from \p by more function types in.
  ///
  /// Immortal scopes are not shifted, since they are global.
  ScopeRef shifted(ScopeDepth by) const {
    if (getKind() == Kind::Immortal)
      return *this;
    return forParam(ScopeDepth(getDepth().Value + by.Value), getIndex());
  }

  /// Prints `^depth.index`, or `immortal`.
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

/// The number of scope parameters a value of the declared type is restricted
/// by.
unsigned getNumScopeParams(const NominalTypeDecl *decl);

/// Whether the type is or contains a scope-unsaturated type: a nominal type without
/// arguments for its declaration's scope parameters.
bool hasScopeUnsaturatedType(Type ty);

// FIXME: Types that differ only in scopes are reconciled after solving with a
// cast, which doesn't cover lvalues or function conversions yet.
/// \p ty without its scope arguments, which the constraint solver ignores.
Type eraseScopes(Type ty);

} // end namespace swift

#endif // SWIFT_AST_SCOPERESTRICTIONS_H
