//===--- Bridging/AvailabilityBridging.cpp --------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/AST/ASTBridging.h"
#include "swift/AST/AvailabilitySpec.h"
#include "swift/AST/PlatformKind.h"

using namespace swift;

//===----------------------------------------------------------------------===//
// MARK: BridgedAvailabilityMacroMap
//===----------------------------------------------------------------------===//

bool BridgedAvailabilityMacroMap_hasName(BridgedAvailabilityMacroMap map,
                                         BridgedStringRef name) {
  return map.unbridged()->hasMacroName(name.unbridged());
}

bool BridgedAvailabilityMacroMap_hasNameAndVersion(
    BridgedAvailabilityMacroMap map, BridgedStringRef name,
    BridgedVersionTuple version) {
  return map.unbridged()->hasMacroNameVersion(name.unbridged(),
                                              version.unbridged());
}

BridgedArrayRef
BridgedAvailabilityMacroMap_getSpecs(BridgedAvailabilityMacroMap map,
                                     BridgedStringRef name,
                                     BridgedVersionTuple version) {
  return map.unbridged()->getEntry(name.unbridged(), version.unbridged());
}

//===----------------------------------------------------------------------===//
// MARK: PlatformKind
//===----------------------------------------------------------------------===//

BridgedPlatformKind BridgedPlatformKind_fromString(BridgedStringRef cStr) {
  auto optKind = platformFromString(cStr.unbridged());
  if (!optKind)
    return BridgedPlatformKind_None;

  switch (*optKind) {
  case PlatformKind::none:
    return BridgedPlatformKind_None;
#define AVAILABILITY_PLATFORM(X, PrettyName)                                   \
  case PlatformKind::X:                                                        \
    return BridgedPlatformKind_##X;
#include "swift/AST/PlatformKinds.def"
  }
}

static PlatformKind unbridge(BridgedPlatformKind platform) {
  switch (platform) {
  case BridgedPlatformKind_None:
    return PlatformKind::none;
#define AVAILABILITY_PLATFORM(X, PrettyName)                                   \
  case BridgedPlatformKind_##X:                                                \
    return PlatformKind::X;
#include "swift/AST/PlatformKinds.def"
  }
  llvm_unreachable("unhandled enum value");
}

//===----------------------------------------------------------------------===//
// MARK: AvailabilitySpec
//===----------------------------------------------------------------------===//

BridgedSourceRange
BridgedAvailabilitySpec_getSourceRange(BridgedAvailabilitySpec spec) {
  return spec.unbridged()->getSourceRange();
}

BridgedAvailabilityDomain
BridgedAvailabilitySpec_getDomain(BridgedAvailabilitySpec spec) {
  auto domain = spec.unbridged()->getDomain();
  if (domain)
    return *domain;
  return BridgedAvailabilityDomain();
}

BridgedVersionTuple
BridgedAvailabilitySpec_getVersion(BridgedAvailabilitySpec spec) {
  return spec.unbridged()->getVersion();
}

BridgedSourceRange
BridgedAvailabilitySpec_getVersionRange(BridgedAvailabilitySpec spec) {
  return spec.unbridged()->getVersionSrcRange();
}

static AvailabilitySpecKind unbridge(BridgedAvailabilitySpecKind kind) {
  switch (kind) {
  case BridgedAvailabilitySpecKindPlatformVersionConstraint:
    return AvailabilitySpecKind::PlatformVersionConstraint;
  case BridgedAvailabilitySpecKindOtherPlatform:
    return AvailabilitySpecKind::OtherPlatform;
  case BridgedAvailabilitySpecKindLanguageVersionConstraint:
    return AvailabilitySpecKind::LanguageVersionConstraint;
  case BridgedAvailabilitySpecKindPackageDescriptionVersionConstraint:
    return AvailabilitySpecKind::PackageDescriptionVersionConstraint;
  }
  llvm_unreachable("unhandled enum value");
}

BridgedPlatformVersionConstraintAvailabilitySpec
BridgedPlatformVersionConstraintAvailabilitySpec_createParsed(
    BridgedASTContext cContext, BridgedPlatformKind cPlatform,
    BridgedSourceLoc cPlatformLoc, BridgedVersionTuple cVersion,
    BridgedVersionTuple cRuntimeVersion, BridgedSourceRange cVersionSrcRange) {
  return new (cContext.unbridged()) PlatformVersionConstraintAvailabilitySpec(
      unbridge(cPlatform), cPlatformLoc.unbridged(), cVersion.unbridged(),
      cRuntimeVersion.unbridged(), cVersionSrcRange.unbridged());
}

BridgedPlatformAgnosticVersionConstraintAvailabilitySpec
BridgedPlatformAgnosticVersionConstraintAvailabilitySpec_createParsed(
    BridgedASTContext cContext, BridgedAvailabilitySpecKind cKind,
    BridgedSourceLoc cNameLoc, BridgedVersionTuple cVersion,
    BridgedSourceRange cVersionSrcRange) {
  return new (cContext.unbridged())
      PlatformAgnosticVersionConstraintAvailabilitySpec(
          unbridge(cKind), cNameLoc.unbridged(), cVersion.unbridged(),
          cVersionSrcRange.unbridged());
}

BridgedOtherPlatformAvailabilitySpec
BridgedOtherPlatformAvailabilitySpec_createParsed(BridgedASTContext cContext,
                                                  BridgedSourceLoc cLoc) {
  return new (cContext.unbridged())
      OtherPlatformAvailabilitySpec(cLoc.unbridged());
}

BridgedAvailabilitySpec
BridgedPlatformVersionConstraintAvailabilitySpec_asAvailabilitySpec(
    BridgedPlatformVersionConstraintAvailabilitySpec spec) {
  return static_cast<AvailabilitySpec *>(spec.unbridged());
}

BridgedAvailabilitySpec
BridgedPlatformAgnosticVersionConstraintAvailabilitySpec_asAvailabilitySpec(
    BridgedPlatformAgnosticVersionConstraintAvailabilitySpec spec) {
  return static_cast<AvailabilitySpec *>(spec.unbridged());
}

BridgedAvailabilitySpec BridgedOtherPlatformAvailabilitySpec_asAvailabilitySpec(
    BridgedOtherPlatformAvailabilitySpec spec) {
  return static_cast<AvailabilitySpec *>(spec.unbridged());
}

//===----------------------------------------------------------------------===//
// MARK: AvailabilityDomain
//===----------------------------------------------------------------------===//

BridgedAvailabilityDomain BridgedAvailabilityDomain::forUniversal() {
  return swift::AvailabilityDomain::forUniversal();
}
BridgedAvailabilityDomain
BridgedAvailabilityDomain::forPlatform(BridgedPlatformKind platformKind) {
  return swift::AvailabilityDomain::forPlatform(unbridge(platformKind));
}
BridgedAvailabilityDomain BridgedAvailabilityDomain::forSwiftLanguage() {
  return swift::AvailabilityDomain::forSwiftLanguage();
}
BridgedAvailabilityDomain BridgedAvailabilityDomain::forPackageDescription() {
  return swift::AvailabilityDomain::forPackageDescription();
}
BridgedAvailabilityDomain BridgedAvailabilityDomain::forEmbedded() {
  return swift::AvailabilityDomain::forEmbedded();
}
