//===--- AvailabilitySpec.h - Swift Availability Query ASTs -----*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
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

enum class AvailabilitySpecKind {
    /// A platform-version constraint of the form "PlatformName X.Y.Z"
    PlatformVersionConstraint,

    /// A wildcard constraint, spelled '*', that is equivalent
    /// to CurrentPlatformName >= MinimumDeploymentTargetVersion
    OtherPlatform,

    /// A language-version constraint of the form "swift X.Y.Z"
    LanguageVersionConstraint,
};

/// The root class for specifications of API availability in availability
/// queries.
class AvailabilitySpec {
  AvailabilitySpecKind Kind;

public:
  AvailabilitySpec(AvailabilitySpecKind Kind) : Kind(Kind) {}

  AvailabilitySpecKind getKind() const { return Kind; }

  SourceRange getSourceRange() const;

  void *
  operator new(size_t Bytes, ASTContext &C,
               unsigned Alignment = alignof(AvailabilitySpec));
  void *operator new(size_t Bytes) throw() = delete;
  void operator delete(void *Data) throw() = delete;
};

/// \brief An availability specification that guards execution based on the
/// run-time platform and version, e.g., OS X >= 10.10.
class PlatformVersionConstraintAvailabilitySpec : public AvailabilitySpec {
  PlatformKind Platform;
  SourceLoc PlatformLoc;

  clang::VersionTuple Version;
  SourceRange VersionSrcRange;

public:
  PlatformVersionConstraintAvailabilitySpec(PlatformKind Platform,
                                            SourceLoc PlatformLoc,
                                            clang::VersionTuple Version,
                                            SourceRange VersionSrcRange)
    : AvailabilitySpec(AvailabilitySpecKind::PlatformVersionConstraint),
      Platform(Platform),
      PlatformLoc(PlatformLoc), Version(Version),
      VersionSrcRange(VersionSrcRange) {}

  /// The required platform.
  PlatformKind getPlatform() const { return Platform; }
  SourceLoc getPlatformLoc() const { return PlatformLoc; }
  
  // The platform version to compare against.
  clang::VersionTuple getVersion() const { return Version; }
  SourceRange getVersionSrcRange() const { return VersionSrcRange; }

  SourceRange getSourceRange() const;

  void print(raw_ostream &OS, unsigned Indent) const;
  
  static bool classof(const AvailabilitySpec *Spec) {
    return Spec->getKind() == AvailabilitySpecKind::PlatformVersionConstraint;
  }

  void *
  operator new(size_t Bytes, ASTContext &C,
               unsigned Alignment = alignof(PlatformVersionConstraintAvailabilitySpec)){
    return AvailabilitySpec::operator new(Bytes, C, Alignment);
  }
};

/// \brief An availability specification that guards execution based on the
/// compile-time language version, e.g., swift >= 3.0.1.
class LanguageVersionConstraintAvailabilitySpec : public AvailabilitySpec {
  SourceLoc SwiftLoc;

  clang::VersionTuple Version;
  SourceRange VersionSrcRange;

public:
  LanguageVersionConstraintAvailabilitySpec(SourceLoc SwiftLoc,
                                            clang::VersionTuple Version,
                                            SourceRange VersionSrcRange)
    : AvailabilitySpec(AvailabilitySpecKind::LanguageVersionConstraint),
      SwiftLoc(SwiftLoc), Version(Version),
      VersionSrcRange(VersionSrcRange) {}

  SourceLoc getSwiftLoc() const { return SwiftLoc; }

  // The platform version to compare against.
  clang::VersionTuple getVersion() const { return Version; }
  SourceRange getVersionSrcRange() const { return VersionSrcRange; }

  SourceRange getSourceRange() const;

  void print(raw_ostream &OS, unsigned Indent) const;

  static bool classof(const AvailabilitySpec *Spec) {
    return Spec->getKind() == AvailabilitySpecKind::LanguageVersionConstraint;
  }

  void *
  operator new(size_t Bytes, ASTContext &C,
               unsigned Alignment = alignof(LanguageVersionConstraintAvailabilitySpec)){
    return AvailabilitySpec::operator new(Bytes, C, Alignment);
  }
};

/// A wildcard availability specification that guards execution
/// by checking that the run-time version is greater than the minimum
/// deployment target. This specification is designed to ease porting
/// to new platforms. Because new platforms typically branch from
/// existing platforms, the wildcard allows an #available() check to do the
/// "right" thing (executing the guarded branch) on the new platform without
/// requiring a modification to every availability guard in the program. Note
/// that we still do compile-time availability checking with '*', so the
/// compiler will still catch references to potentially unavailable symbols.
class OtherPlatformAvailabilitySpec : public AvailabilitySpec {
  SourceLoc StarLoc;

public:
  OtherPlatformAvailabilitySpec(SourceLoc StarLoc)
      : AvailabilitySpec(AvailabilitySpecKind::OtherPlatform),
        StarLoc(StarLoc) {}

  SourceRange getSourceRange() const { return SourceRange(StarLoc, StarLoc); }

  void print(raw_ostream &OS, unsigned Indent) const;

  static bool classof(const AvailabilitySpec *Spec) {
    return Spec->getKind() == AvailabilitySpecKind::OtherPlatform;
  }

  void *
  operator new(size_t Bytes, ASTContext &C,
               unsigned Alignment = alignof(OtherPlatformAvailabilitySpec)) {
    return AvailabilitySpec::operator new(Bytes, C, Alignment);
  }
};

} // end namespace swift

#endif
