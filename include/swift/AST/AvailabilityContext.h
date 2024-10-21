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
#include "llvm/ADT/FoldingSet.h"
#include <optional>

namespace swift {
class ASTContext;
class AvailableAttr;
class Decl;

/// An `AvailabilityContext` summarizes the availability constraints for a
/// specific scope, such as within a declaration or at a particular source
/// location in a function body. This context is sufficient to determine whether
/// a declaration is available or not in that scope.
class AvailabilityContext : public llvm::FoldingSetNode {
  /// Summarizes platform specific availability constraints.
  struct PlatformInfo {
    /// The introduction version.
    AvailabilityRange Range;

    /// When `IsUnavailable` is true, this value stores the broadest platform
    /// kind for which the context is unavailable.
    PlatformKind UnavailablePlatform;

    /// Whether or not the context is considered unavailable on the current
    /// platform.
    unsigned IsUnavailable : 1;

    /// Whether or not the context is considered deprecated on the current
    /// platform.
    unsigned IsDeprecated : 1;

    /// Sets `Range` to `other` if `other` is more restrictive. Returns true if
    /// any property changed as a result of adding this constraint.
    bool constrainRange(const AvailabilityRange &other) {
      if (!other.isContainedIn(Range))
        return false;

      Range = other;
      return true;
    }

    /// Sets `Range` to the platform introduction range of `decl` if that range
    /// is more restrictive. Returns true if
    /// any property changed as a result of adding this constraint.
    bool constrainRange(const Decl *decl);

    /// Updates `UnavailablePlatform` and `IsUnavailable` to reflect the status
    /// of `decl` if its platform unavailability is more restrictive. Returns
    /// true if any property changed as a result of adding this constraint.
    bool constrainUnavailability(const Decl *decl);

    /// If `decl` is deprecated, sets `IsDeprecated` to true. Returns true if
    /// any property changed as a result of adding this constraint.
    bool constrainDeprecated(const Decl *decl);

    /// Returns true if `other` is as available or is more available.
    bool isContainedIn(const PlatformInfo &other) const;

    void Profile(llvm::FoldingSetNodeID &ID) const {
      Range.getRawVersionRange().Profile(ID);
      ID.AddBoolean(IsUnavailable);
      ID.AddInteger(static_cast<uint8_t>(UnavailablePlatform));
      ID.AddBoolean(IsDeprecated);
    }
  };
  PlatformInfo PlatformAvailability;

  AvailabilityContext(const PlatformInfo &platformInfo)
      : PlatformAvailability(platformInfo){};

  static const AvailabilityContext *get(const PlatformInfo &platformInfo,
                                        ASTContext &ctx);

public:
  /// Retrieves the default `AvailabilityContext`, which is maximally available.
  /// The platform availability range will be set to the deployment target (or
  /// minimum inlining target when applicable).
  static const AvailabilityContext *getDefault(ASTContext &ctx);

  /// Retrieves a uniqued `AvailabilityContext` with the given platform
  /// availability parameters.
  static const AvailabilityContext *
  get(const AvailabilityRange &platformAvailability,
      std::optional<PlatformKind> unavailablePlatform, bool deprecated,
      ASTContext &ctx) {
    PlatformInfo platformInfo{platformAvailability,
                              unavailablePlatform.has_value()
                                  ? *unavailablePlatform
                                  : PlatformKind::none,
                              unavailablePlatform.has_value(), deprecated};
    return get(platformInfo, ctx);
  }

  /// Returns the range of platform versions which may execute code in the
  /// availability context, starting at its introduction version.
  AvailabilityRange getPlatformRange() const {
    return PlatformAvailability.Range;
  }

  /// When the context is unavailable on the current platform this returns the
  /// broadest `PlatformKind` for which the context is unavailable. Otherwise,
  /// returns `nullopt`.
  std::optional<PlatformKind> getUnavailablePlatformKind() const {
    if (PlatformAvailability.IsUnavailable)
      return PlatformAvailability.UnavailablePlatform;
    return std::nullopt;
  }

  /// Returns true if this context is deprecated on the current platform.
  bool isDeprecated() const { return PlatformAvailability.IsDeprecated; }

  /// Returns the unique context that is the result of constraining the current
  /// context's platform availability range with `platformRange`.
  const AvailabilityContext *
  constrainWithPlatformRange(const AvailabilityRange &platformRange,
                             ASTContext &ctx) const;

  /// Returns the unique context that is the result of constraining the current
  /// context both with the availability attributes of `decl` and with
  /// `platformRange`.
  const AvailabilityContext *constrainWithDeclAndPlatformRange(
      Decl *decl, const AvailabilityRange &platformRange) const;

  /// Returns true if `other` is as available or is more available.
  bool isContainedIn(const AvailabilityContext *other) const;

  void print(llvm::raw_ostream &os) const;
  SWIFT_DEBUG_DUMP;

  void Profile(llvm::FoldingSetNodeID &ID) const;
};

} // end namespace swift

#endif
