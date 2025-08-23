//===--- AvailabilityConstraint.h - Swift Availability Constraints ------*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2024 - 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines the AvailabilityConstraint class.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_AVAILABILITY_CONSTRAINT_H
#define SWIFT_AST_AVAILABILITY_CONSTRAINT_H

#include "swift/AST/Attr.h"
#include "swift/AST/AvailabilityDomain.h"
#include "swift/AST/AvailabilityRange.h"
#include "swift/AST/PlatformKindUtils.h"
#include "swift/Basic/LLVM.h"
#include "swift/Basic/OptionSet.h"
#include "llvm/Support/raw_ostream.h"

namespace swift {

class ASTContext;
class AvailabilityContext;
class Decl;

/// Represents the reason a declaration is considered not available in a
/// specific `AvailabilityContext`.
class AvailabilityConstraint {
public:
  /// The reason that a declaration is not available in a context. Broadly, the
  /// declaration may either be "unintroduced" or "unavailable" depending on its
  /// `@available` attributes. A declaration that is unintroduced can become
  /// available if availability constraints are added to the context. For
  /// unavailable declarations, on the other hand, either changing the
  /// deployment target or making the context itself unavailable are necessary
  /// to satisfy the constraint.
  ///
  /// For example, take the following declaration `f()`:
  ///
  ///     @available(macOS, introduced: 11.0, obsoleted: 14.0)
  ///     func f() { ... }
  ///
  /// In contexts that may run on earlier OSes, references to `f()` yield
  /// an `Unintroduced` constraint:
  ///
  ///     @available(macOS 10.15, *)
  ///     func g() {
  ///       f() // error: 'f()' is only available in macOS 11.0 or newer
  ///     }
  ///
  ///     macOS ...            11.0                   14.0               ...
  ///     f()   |----------------[=======================)-----------------|
  ///     g()   |-----------[==============================================)
  ///                       ^
  ///                  Unintroduced
  ///
  /// On the other hand, in contexts where deployment target is high enough to
  /// make `f()` obsolete, references to it yield an `UnavailableObsolete`
  /// constraint:
  ///
  ///     // compiled with -target arm64-apple-macos14
  ///     func h() {
  ///       f() // error: 'f()' is unavailable in macOS
  ///     }
  ///
  ///     macOS ...            11.0                    14.0              ...
  ///     f()   |----------------[=======================)-----------------|
  ///     h()   |----------------------------------------[=================)
  ///                                                    ^
  ///                                           UnavailableObsolete
  ///
  /// References to declarations that are unavailable in all versions of a
  /// domain generate `UnavailableUnconditional` constraints unless the context
  /// is also unavailable under the same conditions:
  ///
  ///     @available(macOS, unavailable)
  ///     func foo() { ... }
  ///
  ///     func bar() {
  ///       foo() // error: 'foo()' is unavailable in macOS
  ///     }
  ///
  ///     @available(macOS, unavailable)
  ///     func baz() {
  ///       foo() // OK
  ///     }
  ///
  ///     @available(*, unavailable)
  ///     func qux() {
  ///       foo() // also OK
  ///     }
  ///
  /// NOTE: The order of this enum matters. Reasons are defined in descending
  /// priority order.
  enum class Reason {
    /// The declaration is unconditionally unavailable, e.g. because of
    /// `@available(macOS, unavailable)`.
    UnavailableUnconditionally,

    /// The declaration is obsolete, e.g. because of
    /// `@available(macOS, obsolete: 14.0)` in a program with a deployment
    /// target of `macOS 14` or later.
    UnavailableObsolete,

    /// The declaration is only available for later deployment configurations,
    /// e.g. because of `@available(swift 6)` in a program compiled with
    /// `-swift-version 5`.
    UnavailableUnintroduced,

    /// The declaration has not yet been introduced, e.g. because of
    /// `@available(macOS 14, *)` in a context that may run on macOS 13 or
    /// later. The constraint may be satisfied adding an `@available` attribute
    /// or an `if #available(...)` query with sufficient introduction
    /// constraints to the context.
    Unintroduced,
  };

private:
  llvm::PointerIntPair<SemanticAvailableAttr, 2, Reason> attrAndReason;

  AvailabilityConstraint(Reason reason, SemanticAvailableAttr attr)
      : attrAndReason(attr, reason) {};

public:
  static AvailabilityConstraint
  unavailableUnconditionally(SemanticAvailableAttr attr) {
    return AvailabilityConstraint(Reason::UnavailableUnconditionally, attr);
  }

  static AvailabilityConstraint
  unavailableObsolete(SemanticAvailableAttr attr) {
    return AvailabilityConstraint(Reason::UnavailableObsolete, attr);
  }

  static AvailabilityConstraint
  unavailableUnintroduced(SemanticAvailableAttr attr) {
    return AvailabilityConstraint(Reason::UnavailableUnintroduced, attr);
  }

  static AvailabilityConstraint unintroduced(SemanticAvailableAttr attr) {
    return AvailabilityConstraint(Reason::Unintroduced, attr);
  }

  Reason getReason() const { return attrAndReason.getInt(); }
  SemanticAvailableAttr getAttr() const {
    return static_cast<SemanticAvailableAttr>(attrAndReason.getPointer());
  }

  /// Returns true if the constraint cannot be satisfied using a runtime
  /// availability query (`if #available(...)`).
  bool isUnavailable() const {
    switch (getReason()) {
    case Reason::UnavailableUnconditionally:
    case Reason::UnavailableObsolete:
    case Reason::UnavailableUnintroduced:
      return true;
    case Reason::Unintroduced:
      return false;
    }
  }

  /// Returns the domain that the constraint applies to.
  AvailabilityDomain getDomain() const { return getAttr().getDomain(); }

  /// Returns the domain and range (remapped if necessary) in which the
  /// constraint must be satisfied. How the range should be interpreted depends
  /// on the reason for the constraint.
  AvailabilityDomainAndRange getDomainAndRange(const ASTContext &ctx) const;

  /// Some availability constraints are active for type-checking but cannot
  /// be translated directly into an `if #available(...)` runtime query.
  bool isActiveForRuntimeQueries(const ASTContext &ctx) const;

  void print(raw_ostream &os) const;
};

/// Represents a set of availability constraints that restrict use of a
/// declaration in a particular context. There can only be one active constraint
/// for a given `AvailabilityDomain`, but there may be multiple active
/// constraints from separate domains.
class DeclAvailabilityConstraints {
  using Storage = llvm::SmallVector<AvailabilityConstraint, 4>;
  Storage constraints;

public:
  DeclAvailabilityConstraints() {}
  DeclAvailabilityConstraints(const Storage &&constraints)
      : constraints(constraints) {}

  /// Returns the strongest availability constraint or `std::nullopt` if empty.
  std::optional<AvailabilityConstraint> getPrimaryConstraint() const;

  using const_iterator = Storage::const_iterator;
  const_iterator begin() const { return constraints.begin(); }
  const_iterator end() const { return constraints.end(); }

  void print(raw_ostream &os) const;
};

enum class AvailabilityConstraintFlag : uint8_t {
  /// By default, the availability constraints for the members of extensions
  /// include the constraints for `@available` attributes that were written on
  /// the enclosing extension, since these members can be referred to without
  /// referencing the extension. When this flag is specified, though, only the
  /// attributes directly attached to the declaration are considered.
  SkipEnclosingExtension = 1 << 0,

  /// Include constraints for all domains, regardless of whether they are active
  /// or relevant to type checking.
  IncludeAllDomains = 1 << 1,

  /// By default, non-type declarations that are universally unavailable are
  /// always diagnosed, regardless of whether the context of the reference
  /// is also universally unavailable. If this flag is set, though, those
  /// references are allowed.
  AllowUniversallyUnavailableInCompatibleContexts = 1 << 2,
};
using AvailabilityConstraintFlags = OptionSet<AvailabilityConstraintFlag>;

/// Returns the set of availability constraints that restricts use of \p decl
/// when it is referenced from the given context. In other words, it is the
/// collection of `@available` attributes with unsatisfied conditions.
DeclAvailabilityConstraints getAvailabilityConstraintsForDecl(
    const Decl *decl, const AvailabilityContext &context,
    AvailabilityConstraintFlags flags = std::nullopt);

/// Returns the availability constraints that restricts use of \p decl
/// in \p domain when it is referenced from the given context. In other words,
/// it is the unsatisfied `@available` attribute  that applies to \p domain in
/// the given context.
std::optional<AvailabilityConstraint> getAvailabilityConstraintForDeclInDomain(
    const Decl *decl, const AvailabilityContext &context,
    AvailabilityDomain domain,
    AvailabilityConstraintFlags flags = std::nullopt);
} // end namespace swift

namespace llvm {

inline llvm::raw_ostream &
operator<<(llvm::raw_ostream &os,
           const swift::AvailabilityConstraint &constraint) {
  constraint.print(os);
  return os;
}

inline llvm::raw_ostream &
operator<<(llvm::raw_ostream &os,
           const swift::DeclAvailabilityConstraints &constraints) {
  constraints.print(os);
  return os;
}

} // end namespace llvm
#endif
