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

#include "swift/AST/ASTAllocated.h"
#include "swift/AST/AvailabilityDomain.h"
#include "swift/AST/Identifier.h"
#include "swift/AST/PlatformKind.h"
#include "swift/Basic/SourceLoc.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/Support/VersionTuple.h"

namespace swift {
class ASTContext;

enum class AvailabilitySpecKind {
  /// A platform-version constraint of the form "PlatformName X.Y.Z"
  PlatformVersionConstraint,

  /// A wildcard constraint, spelled '*', that is equivalent
  /// to CurrentPlatformName >= MinimumDeploymentTargetVersion
  Wildcard,

  /// A language-version constraint of the form "swift X.Y.Z"
  LanguageVersionConstraint,

  /// A PackageDescription version constraint of the form "_PackageDescription
  /// X.Y.Z"
  PackageDescriptionVersionConstraint,
};

/// The root class for specifications of API availability in availability
/// queries.
class AvailabilitySpec : public ASTAllocated<AvailabilitySpec> {
protected:
  AvailabilitySpecKind Kind;

  std::optional<AvailabilityDomain> Domain;

  /// The range of the entire spec, including the version if there is one.
  SourceRange SrcRange;

  /// The version (may be empty if there was no version specified).
  llvm::VersionTuple Version;

  /// If there is a version specified, this is its start location within the
  /// overall source range.
  SourceLoc VersionStartLoc;

  // Location of the availability macro expanded to create this spec.
  SourceLoc MacroLoc;

  AvailabilitySpec(AvailabilitySpecKind Kind,
                   std::optional<AvailabilityDomain> Domain,
                   SourceRange SrcRange, llvm::VersionTuple Version,
                   SourceLoc VersionStartLoc)
      : Kind(Kind), Domain(Domain), SrcRange(SrcRange), Version(Version),
        VersionStartLoc(VersionStartLoc) {}

public:
  /// Creates a wildcard availability specification that guards execution
  /// by checking that the run-time version is greater than the minimum
  /// deployment target. This specification is designed to ease porting
  /// to new platforms. Because new platforms typically branch from
  /// existing platforms, the wildcard allows an #available() check to do the
  /// "right" thing (executing the guarded branch) on the new platform without
  /// requiring a modification to every availability guard in the program. Note
  /// that we still do compile-time availability checking with '*', so the
  /// compiler will still catch references to potentially unavailable symbols.
  static AvailabilitySpec *createWildcard(ASTContext &ctx, SourceLoc starLoc);

  /// Creates an availability specification that guards execution based on the
  /// compile-time platform agnostic version, e.g., swift >= 3.0.1,
  /// package-description >= 4.0.
  static AvailabilitySpec *createPlatformAgnostic(ASTContext &ctx,
                                                  AvailabilitySpecKind kind,
                                                  SourceLoc nameLoc,
                                                  llvm::VersionTuple version,
                                                  SourceRange versionRange);

  /// Creates an availability specification that guards execution based on the
  /// run-time platform and version, e.g., macOS >= 10.10.
  static AvailabilitySpec *createPlatformVersioned(ASTContext &ctx,
                                                   PlatformKind platform,
                                                   SourceLoc platformLoc,
                                                   llvm::VersionTuple version,
                                                   SourceRange versionRange);

  AvailabilitySpec *clone(ASTContext &ctx) const {
    return new (ctx)
        AvailabilitySpec(Kind, Domain, SrcRange, Version, VersionStartLoc);
  }

  AvailabilitySpecKind getKind() const { return Kind; }

  bool isWildcard() { return getKind() == AvailabilitySpecKind::Wildcard; }

  SourceRange getSourceRange() const { return SrcRange; }
  SourceLoc getStartLoc() const { return SrcRange.Start; }

  std::optional<AvailabilityDomain> getDomain() const { return Domain; }

  PlatformKind getPlatform() const {
    if (auto domain = getDomain())
      return domain->getPlatformKind();
    return PlatformKind::none;
  }

  // The platform version to compare against.
  llvm::VersionTuple getVersion() const;

  // The version to be used in codegen for version comparisons at run time.
  // This is required to support beta versions of macOS Big Sur that
  // report 10.16 at run time.
  llvm::VersionTuple getRuntimeVersion() const { return Version; }

  SourceRange getVersionSrcRange() const {
    if (!VersionStartLoc)
      return SourceRange();
    return SourceRange(VersionStartLoc, SrcRange.End);
  }

  // Location of the macro expanded to create this spec.
  SourceLoc getMacroLoc() const { return MacroLoc; }
  void setMacroLoc(SourceLoc loc) { MacroLoc = loc; }
};

/// Maps of macro name and version to availability specifications.
/// Organized as two nested \c DenseMap keyed first on the macro name then
/// the macro version. This structure allows to peek at macro names before
/// parsing a version tuple.
class AvailabilityMacroMap {
public:
  typedef llvm::DenseMap<llvm::VersionTuple, SmallVector<AvailabilitySpec *, 4>>
      VersionEntry;
  llvm::StringMap<VersionEntry> Impl;

  bool hasMacroName(StringRef name) const {
    return Impl.find(name) != Impl.end();
  }

  bool hasMacroNameVersion(StringRef name, llvm::VersionTuple version) const {
    auto entry = Impl.find(name);
    if (entry == Impl.end()) {
      return false;
    }
    return entry->second.find(version) != entry->second.end();
  }

  void addEntry(StringRef name, llvm::VersionTuple version,
                ArrayRef<AvailabilitySpec *> specs) {
    assert(!hasMacroNameVersion(name, version));
    Impl[name][version].assign(specs.begin(), specs.end());
  }

  ArrayRef<AvailabilitySpec *> getEntry(StringRef name,
                                        llvm::VersionTuple version) const {
    auto versions = Impl.find(name);
    if (versions == Impl.end()) {
      return {};
    }
    auto entry = versions->second.find(version);
    if (entry == versions->second.end()) {
      return {};
    }
    return entry->second;
  }
};

} // end namespace swift

#endif
