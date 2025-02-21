//===--- AvailabilityConstraint.h - Swift Availability Constraints ------*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2024 Apple Inc. and the Swift project authors
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
#include "swift/AST/PlatformKind.h"
#include "swift/Basic/LLVM.h"
#include "swift/Basic/OptionSet.h"

namespace swift {

class ASTContext;
class AvailabilityContext;
class Decl;

/// Represents the reason a declaration could be considered unavailable in a
/// certain context.
class AvailabilityConstraint {
public:
  /// The reason that the availability constraint is unsatisfied.
  ///
  /// NOTE: The order of this enum matters. Reasons are defined in descending
  /// priority order.
  enum class Reason {
    /// The declaration is referenced in a context in which it is generally
    /// unavailable. For example, a reference to a declaration that is
    /// unavailable on macOS from a context that may execute on macOS has this
    /// constraint.
    UnconditionallyUnavailable,

    /// The declaration is referenced in a context in which it is considered
    /// obsolete. For example, a reference to a declaration that is obsolete in
    /// macOS 13 from a context that may execute on macOS 13 or later has this
    /// constraint.
    Obsoleted,

    /// The declaration is only available in a later version. For example,
    /// the declaration might only be introduced in the Swift 6 language mode
    /// while the module is being compiled in the Swift 5 language mode.
    IntroducedInLaterVersion,

    /// The declaration is referenced in a context that does not have an
    /// adequate minimum version constraint. For example, a reference to a
    /// declaration that is introduced in macOS 13 from a context that may
    /// execute on earlier versions of macOS has this constraint. This
    /// kind of constraint can be satisfied by tightening the minimum
    /// version of the context with `if #available(...)` or by adding or
    /// adjusting an `@available` attribute.
    IntroducedInLaterDynamicVersion,
  };

  /// Classifies constraints into different high level categories.
  enum class Kind {
    /// There are no contexts in which the declaration would be available.
    Unavailable,

    /// There are some contexts in which the declaration would be available if
    /// additional constraints were added.
    PotentiallyAvailable,
  };

private:
  llvm::PointerIntPair<SemanticAvailableAttr, 2, Reason> attrAndReason;

  AvailabilityConstraint(Reason reason, SemanticAvailableAttr attr)
      : attrAndReason(attr, reason) {};

public:
  static AvailabilityConstraint
  unconditionallyUnavailable(SemanticAvailableAttr attr) {
    return AvailabilityConstraint(Reason::UnconditionallyUnavailable, attr);
  }

  static AvailabilityConstraint obsoleted(SemanticAvailableAttr attr) {
    return AvailabilityConstraint(Reason::Obsoleted, attr);
  }

  static AvailabilityConstraint
  introducedInLaterVersion(SemanticAvailableAttr attr) {
    return AvailabilityConstraint(Reason::IntroducedInLaterVersion, attr);
  }

  static AvailabilityConstraint
  introducedInLaterDynamicVersion(SemanticAvailableAttr attr) {
    return AvailabilityConstraint(Reason::IntroducedInLaterDynamicVersion,
                                  attr);
  }

  Reason getReason() const { return attrAndReason.getInt(); }
  SemanticAvailableAttr getAttr() const {
    return static_cast<SemanticAvailableAttr>(attrAndReason.getPointer());
  }

  Kind getKind() const {
    switch (getReason()) {
    case Reason::UnconditionallyUnavailable:
    case Reason::Obsoleted:
    case Reason::IntroducedInLaterVersion:
      return Kind::Unavailable;
    case Reason::IntroducedInLaterDynamicVersion:
      return Kind::PotentiallyAvailable;
    }
  }

  /// Returns true if the constraint cannot be satisfied at runtime.
  bool isUnavailable() const { return getKind() == Kind::Unavailable; }

  /// Returns true if the constraint is unsatisfied but could be satisfied at
  /// runtime in a more constrained context.
  bool isPotentiallyAvailable() const {
    return getKind() == Kind::PotentiallyAvailable;
  }

  /// Returns the domain that the constraint applies to.
  AvailabilityDomain getDomain() const { return getAttr().getDomain(); }

  /// Returns the platform that this constraint applies to, or
  /// `PlatformKind::none` if it is not platform specific.
  PlatformKind getPlatform() const;

  /// Returns the required range for `IntroducedInNewerVersion` requirements, or
  /// `std::nullopt` otherwise.
  std::optional<AvailabilityRange>
  getRequiredNewerAvailabilityRange(const ASTContext &ctx) const;

  /// Some availability constraints are active for type-checking but cannot
  /// be translated directly into an `if #available(...)` runtime query.
  bool isActiveForRuntimeQueries(const ASTContext &ctx) const;
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
};

enum class AvailabilityConstraintFlag : uint8_t {
  /// By default, the availability constraints for the members of extensions
  /// include the constraints for `@available` attributes that were written on
  /// the enclosing extension, since these members can be referred to without
  /// referencing the extension. When this flag is specified, though, only the
  /// attributes directly attached to the declaration are considered.
  SkipEnclosingExtension = 1 << 0,
};
using AvailabilityConstraintFlags = OptionSet<AvailabilityConstraintFlag>;

/// Returns the set of availability constraints that restrict use of \p decl
/// when it is referenced from the given context. In other words, it is the
/// collection of of `@available` attributes with unsatisfied conditions.
DeclAvailabilityConstraints getAvailabilityConstraintsForDecl(
    const Decl *decl, const AvailabilityContext &context,
    AvailabilityConstraintFlags flags = std::nullopt);
} // end namespace swift

#endif
