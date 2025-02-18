//===--- AvailabilitySpec.cpp - Swift Availability Query ASTs -------------===//
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
// This file implements the availability specification AST classes.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/AvailabilitySpec.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/AvailabilityDomain.h"
#include "llvm/Support/raw_ostream.h"

using namespace swift;

AvailabilitySpec *AvailabilitySpec::createWildcard(ASTContext &ctx,
                                                   SourceLoc starLoc) {
  return new (ctx)
      AvailabilitySpec(AvailabilitySpecKind::Wildcard, std::nullopt, starLoc,
                       /*Version=*/{},
                       /*VersionStartLoc=*/{});
}

static AvailabilityDomain getDomainForSpecKind(AvailabilitySpecKind Kind) {
  switch (Kind) {
  case AvailabilitySpecKind::PlatformVersionConstraint:
  case AvailabilitySpecKind::Wildcard:
    llvm_unreachable("unexpected spec kind");
  case AvailabilitySpecKind::LanguageVersionConstraint:
    return AvailabilityDomain::forSwiftLanguage();
  case AvailabilitySpecKind::PackageDescriptionVersionConstraint:
    return AvailabilityDomain::forPackageDescription();
  }
}

AvailabilitySpec *AvailabilitySpec::createPlatformAgnostic(
    ASTContext &ctx, AvailabilitySpecKind kind, SourceLoc nameLoc,
    llvm::VersionTuple version, SourceRange versionRange) {
  return new (ctx) AvailabilitySpec(kind, getDomainForSpecKind(kind),
                                    SourceRange(nameLoc, versionRange.End),
                                    version, versionRange.Start);
}

static std::optional<AvailabilityDomain>
getDomainForPlatform(PlatformKind Platform) {
  if (Platform != PlatformKind::none)
    return AvailabilityDomain::forPlatform(Platform);
  return std::nullopt;
}

AvailabilitySpec *AvailabilitySpec::createPlatformVersioned(
    ASTContext &ctx, PlatformKind platform, SourceLoc platformLoc,
    llvm::VersionTuple version, SourceRange versionRange) {
  return new (ctx) AvailabilitySpec(
      AvailabilitySpecKind::PlatformVersionConstraint,
      getDomainForPlatform(platform),
      SourceRange(platformLoc, versionRange.End), version, versionRange.Start);
}

llvm::VersionTuple AvailabilitySpec::getVersion() const {
  // For macOS Big Sur, we canonicalize 10.16 to 11.0 for compile-time
  // checking since clang canonicalizes availability markup. However, to
  // support Beta versions of macOS Big Sur where the OS
  // reports 10.16 at run time, we need to compare against 10.16,
  //
  // This means for:
  //
  // if #available(macOS 10.16, *) { ... }
  //
  // we need to store the uncanonicalized version for codegen and canonicalize
  // it as necessary for compile-time checks.
  return canonicalizePlatformVersion(getPlatform(), Version);
}
