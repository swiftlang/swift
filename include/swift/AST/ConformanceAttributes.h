//===--- ConformanceLookup.h - Global conformance lookup --------*- C++ -*-===//
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

#ifndef SWIFT_AST_CONFORMANCEATTRIBUTES_H
#define SWIFT_AST_CONFORMANCEATTRIBUTES_H

#include "swift/Basic/SourceLoc.h"

namespace swift {

class TypeExpr;

/// Describes all of the attributes that can occur on a conformance.
struct ConformanceAttributes {
  /// The location of the "unchecked" attribute, if present.
  SourceLoc uncheckedLoc;

  /// The location of the "preconcurrency" attribute if present.
  SourceLoc preconcurrencyLoc;

  /// The location of the "unsafe" attribute if present.
  SourceLoc unsafeLoc;

  /// The location of the "nonisolated" modifier, if present.
  SourceLoc nonisolatedLoc;

  /// The location of the '@' prior to the global actor type.
  SourceLoc globalActorAtLoc;

  /// The global actor type to which this conformance is isolated.
  TypeExpr *globalActorType = nullptr;

  /// Merge other conformance attributes into this set.
  ConformanceAttributes &
  operator |=(const ConformanceAttributes &other) {
    if (other.uncheckedLoc.isValid())
      uncheckedLoc = other.uncheckedLoc;
    if (other.preconcurrencyLoc.isValid())
      preconcurrencyLoc = other.preconcurrencyLoc;
    if (other.unsafeLoc.isValid())
      unsafeLoc = other.unsafeLoc;
    if (other.nonisolatedLoc.isValid())
      nonisolatedLoc = other.nonisolatedLoc;
    if (other.globalActorType && !globalActorType) {
      globalActorAtLoc = other.globalActorAtLoc;
      globalActorType = other.globalActorType;
    }
    return *this;
  }
};

}

#endif
