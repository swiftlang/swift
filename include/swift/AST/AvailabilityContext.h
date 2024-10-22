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

#include "swift/AST/Availability.h"
#include "swift/AST/PlatformKind.h"
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
  struct PlatformInfo;

  /// A non-null pointer to uniqued storage for this availability context.
  const Storage *Info;

  AvailabilityContext(const Storage *info) : Info(info) { assert(info); };

public:
  AvailabilityContext(const AvailabilityContext &other) : Info(other.Info){};

  /// Retrieves the default `AvailabilityContext`, which is maximally available.
  /// The platform availability range will be set to the deployment target (or
  /// minimum inlining target when applicable).
  static AvailabilityContext getDefault(ASTContext &ctx);

  /// Retrieves a uniqued `AvailabilityContext` with the given platform
  /// availability parameters.
  static AvailabilityContext
  get(const AvailabilityRange &platformAvailability,
      std::optional<PlatformKind> unavailablePlatform, bool deprecated,
      ASTContext &ctx);

  /// Returns the range of platform versions which may execute code in the
  /// availability context, starting at its introduction version.
  AvailabilityRange getPlatformRange() const;

  /// When the context is unavailable on the current platform this returns the
  /// broadest `PlatformKind` for which the context is unavailable. Otherwise,
  /// returns `nullopt`.
  std::optional<PlatformKind> getUnavailablePlatformKind() const;

  /// Returns true if this context is deprecated on the current platform.
  bool isDeprecated() const;

  /// Constrain the platform availability range with `platformRange`.
  void constrainWithPlatformRange(const AvailabilityRange &platformRange,
                                  ASTContext &ctx);

  /// Constrain the platform availability range with both the availability
  /// attributes of `decl` and with `platformRange`.
  void
  constrainWithDeclAndPlatformRange(Decl *decl,
                                    const AvailabilityRange &platformRange);

  /// Returns true if `other` is as available or is more available.
  bool isContainedIn(const AvailabilityContext other) const;

  friend bool operator==(const AvailabilityContext &lhs,
                         const AvailabilityContext &rhs) {
    return lhs.Info == rhs.Info;
  }

  friend bool operator!=(const AvailabilityContext &lhs,
                         const AvailabilityContext &rhs) {
    return lhs.Info != rhs.Info;
  }

  void print(llvm::raw_ostream &os) const;
  SWIFT_DEBUG_DUMP;
};

} // end namespace swift

#endif
