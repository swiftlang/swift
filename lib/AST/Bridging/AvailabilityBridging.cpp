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

PlatformKind unbridge(BridgedPlatformKind platform) {
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

static BridgedPlatformKind bridge(PlatformKind platform) {
  switch (platform) {
  case PlatformKind::none:
    return BridgedPlatformKind_None;
#define AVAILABILITY_PLATFORM(X, PrettyName)                                   \
  case PlatformKind::X:                                                        \
    return BridgedPlatformKind_##X;
#include "swift/AST/PlatformKinds.def"
  }
  llvm_unreachable("unhandled enum value");
}

//===----------------------------------------------------------------------===//
// MARK: AvailabilitySpec
//===----------------------------------------------------------------------===//

static AvailabilitySpecKind unbridge(BridgedAvailabilitySpecKind kind) {
  switch (kind) {
  case BridgedAvailabilitySpecKindPlatformVersionConstraint:
    return AvailabilitySpecKind::PlatformVersionConstraint;
  case BridgedAvailabilitySpecKindWildcard:
    return AvailabilitySpecKind::Wildcard;
  case BridgedAvailabilitySpecKindLanguageVersionConstraint:
    return AvailabilitySpecKind::LanguageVersionConstraint;
  case BridgedAvailabilitySpecKindPackageDescriptionVersionConstraint:
    return AvailabilitySpecKind::PackageDescriptionVersionConstraint;
  }
  llvm_unreachable("unhandled enum value");
}

BridgedAvailabilitySpec
BridgedAvailabilitySpec_createWildcard(BridgedASTContext cContext,
                                       BridgedSourceLoc cLoc) {
  return AvailabilitySpec::createWildcard(cContext.unbridged(),
                                          cLoc.unbridged());
}

BridgedAvailabilitySpec BridgedAvailabilitySpec_createPlatformAgnostic(
    BridgedASTContext cContext, BridgedAvailabilitySpecKind cKind,
    BridgedSourceLoc cLoc, BridgedVersionTuple cVersion,
    BridgedSourceRange cVersionRange) {
  return AvailabilitySpec::createPlatformAgnostic(
      cContext.unbridged(), unbridge(cKind), cLoc.unbridged(),
      cVersion.unbridged(), cVersionRange.unbridged());
}

BridgedAvailabilitySpec BridgedAvailabilitySpec_createPlatformVersioned(
    BridgedASTContext cContext, BridgedPlatformKind cPlatform,
    BridgedSourceLoc cPlatformLoc, BridgedVersionTuple cVersion,
    BridgedSourceRange cVersionSrcRange) {
  return AvailabilitySpec::createPlatformVersioned(
      cContext.unbridged(), unbridge(cPlatform), cPlatformLoc.unbridged(),
      cVersion.unbridged(), cVersionSrcRange.unbridged());
}

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

BridgedPlatformKind
BridgedAvailabilitySpec_getPlatform(BridgedAvailabilitySpec spec) {
  return bridge(spec.unbridged()->getPlatform());
}

BridgedVersionTuple
BridgedAvailabilitySpec_getVersion(BridgedAvailabilitySpec spec) {
  return spec.unbridged()->getVersion();
}

BridgedSourceRange
BridgedAvailabilitySpec_getVersionRange(BridgedAvailabilitySpec spec) {
  return spec.unbridged()->getVersionSrcRange();
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
