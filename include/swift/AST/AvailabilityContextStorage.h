//===--- AvailabilityContextStorage.h - Swift AvailabilityContext ---------===//
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
// This file defines types used in the implementation of AvailabilityContext.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_AVAILABILITY_CONTEXT_STORAGE_H
#define SWIFT_AST_AVAILABILITY_CONTEXT_STORAGE_H

#include "swift/AST/AvailabilityContext.h"
#include "llvm/ADT/FoldingSet.h"
#include <optional>

namespace swift {

/// Summarizes platform specific availability constraints.
struct AvailabilityContext::PlatformInfo {
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

/// As an implementation detail, the values that make up an `Availability`
/// context are uniqued and stored as folding set nodes.
class AvailabilityContext::Storage final : public llvm::FoldingSetNode {
  Storage(const PlatformInfo &platformInfo) : Platform(platformInfo){};

public:
  PlatformInfo Platform;

  static const Storage *get(const PlatformInfo &platformInfo, ASTContext &ctx);

  void Profile(llvm::FoldingSetNodeID &ID) const;
};

} // end namespace swift

#endif
