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
#include "swift/Basic/Located.h"
#include "swift/Basic/SourceLoc.h"
#include "llvm/ADT/PointerIntPair.h"

namespace swift {

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

} // end namespace swift

#endif // SWIFT_AST_SCOPERESTRICTIONS_H
