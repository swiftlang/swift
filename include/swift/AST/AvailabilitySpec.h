//===--- AvailabilitySpec.h - Swift Availability Query ASTs -----*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines the availability specification AST classes.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_AVAILABILITY_SPEC_H
#define SWIFT_AST_AVAILABILITY_SPEC_H

#include "swift/AST/Identifier.h"
#include "swift/Basic/SourceLoc.h"
#include "swift/AST/PlatformKind.h"
#include "clang/Basic/VersionTuple.h"

namespace swift {
class ASTContext;

enum class VersionComparison { GreaterThanEqual };

/// \brief An availability specification that guards execution based on the
/// run-time platform and version, e.g., OSX >= 10.10.
class VersionConstraintAvailabilitySpec {
  PlatformKind Platform;
  SourceLoc PlatformLoc;

  VersionComparison Comparison;
  SourceLoc ComparisonLoc;

  clang::VersionTuple Version;
  SourceRange VersionSrcRange;

public:
  VersionConstraintAvailabilitySpec(PlatformKind Platform,
                                    SourceLoc PlatformLoc,
                                    VersionComparison Comparison,
                                    SourceLoc ComparisonLoc,
                                    clang::VersionTuple Version,
                                    SourceRange VersionSrcRange)
      : Platform(Platform), PlatformLoc(PlatformLoc), Comparison(Comparison),
        ComparisonLoc(ComparisonLoc), Version(Version),
        VersionSrcRange(VersionSrcRange) {}

  /// The required platform.
  PlatformKind getPlatform() const { return Platform; }
  SourceLoc getPlatformLoc() const { return PlatformLoc; }

  /// The comparison operator for the specified version.
  VersionComparison getComparison() const { return Comparison; }
  SourceLoc getConstraintLoc() const { return ComparisonLoc; }

  // The platform version to compare against.
  clang::VersionTuple getVersion() { return Version; }
  SourceRange getVersionSrcRange() const { return VersionSrcRange; }

  SourceRange getSourceRange() const;

  void *
  operator new(size_t Bytes, ASTContext &C,
               unsigned Alignment = alignof(VersionConstraintAvailabilitySpec));
  void *operator new(size_t Bytes) throw() = delete;
  void operator delete(void *Data) throw() = delete;
};

} // end namespace swift

#endif
