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

#include "swift/AST/AvailabilityRange.h"
#include "swift/AST/PlatformKind.h"
#include "swift/Basic/LLVM.h"

namespace swift {

class ASTContext;
class AvailableAttr;

/// Represents the reason a declaration could be considered unavailable in a
/// certain context.
class AvailabilityConstraint {
public:
  enum class Kind {
    /// The declaration is referenced in a context in which it is generally
    /// unavailable. For example, a reference to a declaration that is
    /// unavailable on macOS from a context that may execute on macOS has this
    /// constraint.
    AlwaysUnavailable,

    /// The declaration is referenced in a context in which it is considered
    /// obsolete. For example, a reference to a declaration that is obsolete in
    /// macOS 13 from a context that may execute on macOS 13 or later has this
    /// constraint.
    Obsoleted,

    /// The declaration is only available in a different version. For example,
    /// the declaration might only be introduced in the Swift 6 language mode
    /// while the module is being compiled in the Swift 5 language mode.
    RequiresVersion,

    /// The declaration is referenced in a context that does not have an
    /// adequate minimum version constraint. For example, a reference to a
    /// declaration that is introduced in macOS 13 from a context that may
    /// execute on earlier versions of macOS has this constraint. This
    /// kind of constraint can be satisfied by tightening the minimum
    /// version of the context with `if #available(...)` or by adding or
    /// adjusting an `@available` attribute.
    IntroducedInNewerVersion,
  };

private:
  Kind kind;
  const AvailableAttr *attr;

  AvailabilityConstraint(Kind kind, const AvailableAttr *attr)
      : kind(kind), attr(attr) {};

public:
  static AvailabilityConstraint
  forAlwaysUnavailable(const AvailableAttr *attr) {
    return AvailabilityConstraint(Kind::AlwaysUnavailable, attr);
  }

  static AvailabilityConstraint forObsoleted(const AvailableAttr *attr) {
    return AvailabilityConstraint(Kind::Obsoleted, attr);
  }

  static AvailabilityConstraint forRequiresVersion(const AvailableAttr *attr) {
    return AvailabilityConstraint(Kind::RequiresVersion, attr);
  }

  static AvailabilityConstraint
  forIntroducedInNewerVersion(const AvailableAttr *attr) {
    return AvailabilityConstraint(Kind::IntroducedInNewerVersion, attr);
  }

  Kind getKind() const { return kind; }
  const AvailableAttr *getAttr() const { return attr; }

  /// Returns the platform that this constraint applies to, or
  /// `PlatformKind::none` if it is not platform specific.
  PlatformKind getPlatform() const;

  /// Returns the required range for `IntroducedInNewerVersion` requirements, or
  /// `std::nullopt` otherwise.
  std::optional<AvailabilityRange>
  getRequiredNewerAvailabilityRange(ASTContext &ctx) const;

  /// Returns true if this unmet requirement can be satisfied by introducing an
  /// `if #available(...)` condition in source.
  bool isConditionallySatisfiable() const;

  /// Some availability constraints are active for type-checking but cannot
  /// be translated directly into an `if #available(...)` runtime query.
  bool isActiveForRuntimeQueries(ASTContext &ctx) const;
};

} // end namespace swift

#endif
