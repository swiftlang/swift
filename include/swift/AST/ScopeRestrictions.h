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
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/FoldingSet.h"
#include "llvm/ADT/PointerIntPair.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/bit.h"
#include "llvm/Support/TrailingObjects.h"
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
class ScopeDescriptor {
public:
  /// Does this scope descriptor refer to something by name, `self`, or
  /// `immortal`?
  enum class Subject : uint8_t { Name, Self, Immortal };

private:
  /// The name (null unless the subject is `Name`), whether this is an access
  /// (`&`), and the subject.
  llvm::PointerIntPair<llvm::PointerIntPair<Identifier, 1, bool>, 2, Subject>
      Storage;
  SourceLoc Loc;

  ScopeDescriptor(Subject subject, Identifier name, bool isAccess,
                  SourceLoc loc)
      : Storage({name, isAccess}, subject), Loc(loc) {}

public:
  /// Create a scope descriptor that refers to a name, e.g. `a`
  static ScopeDescriptor forScopeName(Located<ScopeName> name) {
    return {Subject::Name, name.Item.getIdentifier(), /*isAccess=*/false,
            name.Loc};
  }

  /// Create a scope descriptor that describes the access of a value, e.g. `&a`
  static ScopeDescriptor forAccessedValue(Located<Identifier> name) {
    return {Subject::Name, name.Item, /*isAccess=*/true, name.Loc};
  }

  static ScopeDescriptor forSelf(SourceLoc selfLoc, bool isAccess) {
    return {Subject::Self, Identifier(), isAccess, selfLoc};
  }

  static ScopeDescriptor forImmortal(SourceLoc immortalLoc) {
    return {Subject::Immortal, Identifier(), /*isAccess=*/false, immortalLoc};
  }

  Subject getSubject() const { return Storage.getInt(); }

  /// Whether this is `&a`, the scope of an access to the value `a`, rather than
  /// a scope itself.
  bool isAccess() const { return Storage.getPointer().getInt(); }

  ScopeName getScopeName() const {
    ASSERT(getSubject() == Subject::Name && !isAccess());
    return ScopeName(Storage.getPointer().getPointer());
  }

  Identifier getAccessedValue() const {
    ASSERT(getSubject() == Subject::Name && isAccess());
    return Storage.getPointer().getPointer();
  }

  SourceLoc getLoc() const { return Loc; }
};

/// A single scope restriction within a `@_scoped` attribute, e.g. `left: &a` in
/// `@_scoped(left: &a, right: b)`.
class ScopeSpecifier {
  /// Null when not specifying a scope restriction by name, e.g. `array` in
  /// `@_scoped(array)`.
  Identifier LabelName;
  SourceLoc LabelLoc;
  ScopeDescriptor Scope;

public:
  ScopeSpecifier(std::optional<Located<ScopeName>> label, ScopeDescriptor scope)
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

  ScopeDescriptor getScope() const { return Scope; }
};

/// The number of function types between a position in a type and an enclosing
/// binder. Only depths measured from the same position are comparable.
class ScopeDepth {
  // Packed into ScopeRef's bitfield.
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
  static constexpr unsigned NumDepthBits = 15;
  static constexpr unsigned NumIndexBits = 15;
  static_assert(unsigned(Kind::Last_Kind) < (1 << NumKindBits),
                "unable to fit a ScopeRef::Kind in the given number of bits");

  uint32_t TheKind : NumKindBits;
  uint32_t Depth : NumDepthBits;
  uint32_t Index : NumIndexBits;

  ScopeRef(Kind kind, ScopeDepth depth, unsigned index)
      : TheKind(unsigned(kind)), Depth(depth.Value), Index(index) {
    ASSERT(Depth == depth.Value && Index == index &&
           "scope reference overflow");
  }

public:
  static ScopeRef forImmortal() {
    return {Kind::Immortal, ScopeDepth::zero(), 0};
  }

  static ScopeRef forParam(ScopeDepth depth, unsigned index) {
    return {Kind::Param, depth, index};
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
    return ScopeDepth(Depth);
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
    return Index;
  }

  /// Reference the same scope from \p by more function types in.
  ///
  /// Immortal scopes are not shifted, since they are global.
  ScopeRef shifted(ScopeDepth by) const {
    if (getKind() == Kind::Immortal)
      return *this;
    return {getKind(), ScopeDepth(Depth + by.Value), Index};
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

/// The scope arguments of a non-escapable nominal type, one per scope parameter
/// of its declaration.
///
/// Scope arguments are part of type identity but deliberately aren't mangled
/// because scopes don't distinguish overloads.
class ScopeArgs final : public llvm::FoldingSetNode,
                        private llvm::TrailingObjects<ScopeArgs, ScopeRef> {
  friend TrailingObjects;

  unsigned NumScopes;

  explicit ScopeArgs(llvm::ArrayRef<ScopeRef> scopes);

public:
  static const ScopeArgs *get(const ASTContext &ctx,
                              llvm::ArrayRef<ScopeRef> scopes);

  llvm::ArrayRef<ScopeRef> getScopes() const {
    return getTrailingObjects(NumScopes);
  }

  void Profile(llvm::FoldingSetNodeID &ID) const { Profile(ID, getScopes()); }
  static void Profile(llvm::FoldingSetNodeID &ID,
                      llvm::ArrayRef<ScopeRef> scopes);
};

/// The number of scope parameters a value of the declared type is restricted
/// by.
unsigned getNumScopeParams(const NominalTypeDecl *decl);

/// Whether the type is or contains a scope-unsaturated type: a nominal type without
/// arguments for its declaration's scope parameters.
bool hasScopeUnsaturatedType(Type ty);

// FIXME: Scopes need to be erased / handled after the solver. SILGen and debug
// info crash on types that differ only in scopes.
/// \p ty without its scope arguments, which the constraint solver ignores.
Type eraseScopes(Type ty);

} // end namespace swift

#endif // SWIFT_AST_SCOPERESTRICTIONS_H
