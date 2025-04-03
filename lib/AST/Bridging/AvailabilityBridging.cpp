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

BridgedPlatformKind
BridgedPlatformKind_fromIdentifier(BridgedIdentifier cIdent) {
  return BridgedPlatformKind_fromString(cIdent.unbridged().str());
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

//===----------------------------------------------------------------------===//
// MARK: AvailabilitySpec
//===----------------------------------------------------------------------===//

BridgedAvailabilitySpec
BridgedAvailabilitySpec_createWildcard(BridgedASTContext cContext,
                                       BridgedSourceLoc cLoc) {
  return AvailabilitySpec::createWildcard(cContext.unbridged(),
                                          cLoc.unbridged());
}

BridgedAvailabilitySpec BridgedAvailabilitySpec_createForDomainIdentifier(
    BridgedASTContext cContext, BridgedIdentifier cName, BridgedSourceLoc cLoc,
    BridgedVersionTuple cVersion, BridgedSourceRange cVersionRange) {
  return AvailabilitySpec::createForDomainIdentifier(
      cContext.unbridged(), cName.unbridged(), cLoc.unbridged(),
      cVersion.unbridged(), cVersionRange.unbridged());
}

BridgedAvailabilitySpec
BridgedAvailabilitySpec_clone(BridgedAvailabilitySpec spec,
                              BridgedASTContext cContext) {
  return spec.unbridged()->clone(cContext.unbridged());
}

void BridgedAvailabilitySpec_setMacroLoc(BridgedAvailabilitySpec spec,
                                         BridgedSourceLoc cLoc) {
  spec.unbridged()->setMacroLoc(cLoc.unbridged());
}

BridgedAvailabilityDomainOrIdentifier
BridgedAvailabilitySpec_getDomainOrIdentifier(BridgedAvailabilitySpec spec) {
  return spec.unbridged()->getDomainOrIdentifier();
}

BridgedSourceRange
BridgedAvailabilitySpec_getSourceRange(BridgedAvailabilitySpec spec) {
  return spec.unbridged()->getSourceRange();
}

bool BridgedAvailabilitySpec_isWildcard(BridgedAvailabilitySpec spec) {
  return spec.unbridged()->isWildcard();
}

BridgedVersionTuple
BridgedAvailabilitySpec_getRawVersion(BridgedAvailabilitySpec spec) {
  return spec.unbridged()->getRawVersion();
}

BridgedSourceRange
BridgedAvailabilitySpec_getVersionRange(BridgedAvailabilitySpec spec) {
  return spec.unbridged()->getVersionSrcRange();
}
