//===--- AvailabilityContext.h - Swift Availability Structures --*- C++ -*-===//
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
// This file defines the AvailabilityContext data structure, which summarizes
// availability constraints for a specific scope.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_AVAILABILITY_CONTEXT_H
#define SWIFT_AST_AVAILABILITY_CONTEXT_H

#include "swift/AST/AvailabilityDomain.h"
#include "swift/AST/AvailabilityRange.h"
#include "swift/AST/PlatformKind.h"
#include "swift/Basic/Debug.h"
#include "swift/Basic/LLVM.h"
#include <optional>

namespace swift {
class ASTContext;
class AvailableAttr;
class Decl;

/// An `AvailabilityContext` summarizes the availability constraints for a
/// specific scope, such as within a declaration or at a particular source
/// location in a function body. This context is sufficient to determine whether
/// a declaration is available or not in that scope.
class AvailabilityContext {
public:
  class Storage;

private:
  class Info;

  /// A non-null pointer to uniqued storage for this availability context.
  const Storage *storage;

  AvailabilityContext(const Storage *storage) : storage(storage) {
    assert(storage);
  };

  /// Retrieves an `AvailabilityContext` with the given platform availability
  /// parameters.
  static AvailabilityContext
  get(const AvailabilityRange &platformAvailability,
      std::optional<AvailabilityDomain> unavailableDomain, bool deprecated,
      ASTContext &ctx);

public:
  /// Retrieves an `AvailabilityContext` constrained by the given platform
  /// availability range.
  static AvailabilityContext forPlatformRange(const AvailabilityRange &range,
                                              ASTContext &ctx);

  /// Retrieves the maximally available `AvailabilityContext` for the
  /// compilation. The platform availability range will be set to the minimum
  /// inlining target (which may just be the deployment target).
  static AvailabilityContext forInliningTarget(ASTContext &ctx);

  /// Retrieves an `AvailabilityContext` with the platform availability range
  /// set to the deployment target.
  static AvailabilityContext forDeploymentTarget(ASTContext &ctx);

  /// Returns the range of platform versions which may execute code in the
  /// availability context, starting at its introduction version.
  AvailabilityRange getPlatformRange() const;

  /// Returns true if this context contains any unavailable domains.
  bool isUnavailable() const;

  /// Returns true if \p domain is unavailable in this context.
  bool containsUnavailableDomain(AvailabilityDomain domain) const;

  /// Returns true if this context is deprecated on the current platform.
  bool isDeprecated() const;

  /// Constrain with another `AvailabilityContext`.
  void constrainWithContext(const AvailabilityContext &other, ASTContext &ctx);

  /// Constrain the platform availability range with `platformRange`.
  void constrainWithPlatformRange(const AvailabilityRange &platformRange,
                                  ASTContext &ctx);

  /// Constrain the context by adding \p domain to the set of unavailable
  /// domains.
  void constrainWithUnavailableDomain(AvailabilityDomain domain,
                                      ASTContext &ctx);

  /// Constrain with the availability attributes of `decl`.
  void constrainWithDecl(const Decl *decl);

  /// Constrain with the availability attributes of `decl`, intersecting the
  /// platform range of `decl` with `platformRange`.
  void
  constrainWithDeclAndPlatformRange(const Decl *decl,
                                    const AvailabilityRange &platformRange);

  /// Returns true if `other` is as available or is more available.
  bool isContainedIn(const AvailabilityContext other) const;

  friend bool operator==(const AvailabilityContext &lhs,
                         const AvailabilityContext &rhs) {
    return lhs.storage == rhs.storage;
  }

  friend bool operator!=(const AvailabilityContext &lhs,
                         const AvailabilityContext &rhs) {
    return lhs.storage != rhs.storage;
  }

  void print(llvm::raw_ostream &os) const;
  SWIFT_DEBUG_DUMP;
};

} // end namespace swift

#endif
