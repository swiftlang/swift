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

class DeclAvailabilityConstraints;

/// Summarizes availability the constraints contained by an AvailabilityContext.
class AvailabilityContext::Info {
public:
  /// The introduction version.
  AvailabilityRange Range;

  /// The broadest unavailable domain.
  std::optional<AvailabilityDomain> UnavailableDomain;

  /// Whether or not the context is considered deprecated on the current
  /// platform.
  unsigned IsDeprecated : 1;

  /// Sets each field to the value of the corresponding field in `other` if the
  /// other is more restrictive. Returns true if any field changed as a result
  /// of adding this constraint.
  bool constrainWith(const Info &other);

  /// Constrains each field using the given constraints if they are more
  /// restrictive than the current values. Returns true if any field was
  /// updated.
  bool constrainWith(const DeclAvailabilityConstraints &constraints,
                     ASTContext &ctx);

  bool constrainUnavailability(std::optional<AvailabilityDomain> domain);

  /// Returns true if `other` is as available or is more available.
  bool isContainedIn(const Info &other) const;

  void Profile(llvm::FoldingSetNodeID &ID) const {
    Range.getRawVersionRange().Profile(ID);
    if (UnavailableDomain) {
      UnavailableDomain->Profile(ID);
    } else {
      ID.AddPointer(nullptr);
    }
    ID.AddBoolean(IsDeprecated);
  }
};

/// As an implementation detail, the values that make up an `Availability`
/// context are uniqued and stored as folding set nodes.
class AvailabilityContext::Storage final : public llvm::FoldingSetNode {
  Storage(const Info &info) : info(info){};

public:
  Info info;

  static const Storage *get(const Info &info, ASTContext &ctx);

  void Profile(llvm::FoldingSetNodeID &ID) const { info.Profile(ID); }
};

} // end namespace swift

#endif
