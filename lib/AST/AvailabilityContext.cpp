//===--- AvailabilityContext.cpp - Swift Availability Structures ----------===//
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

#include "swift/AST/AvailabilityContext.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/AvailabilityContextStorage.h"
#include "swift/AST/AvailabilityInference.h"
#include "swift/AST/Decl.h"
#include "swift/Basic/Assertions.h"

using namespace swift;

// Defined as a macro because you can't take the reference of a bitfield.
#define CONSTRAIN_BOOL(_old, _new)                                             \
  [&]() {                                                                      \
    if (_old || !_new)                                                         \
      return false;                                                            \
    _old = true;                                                               \
    return true;                                                               \
  }()

static bool constrainRange(AvailabilityRange &existing,
                           const AvailabilityRange &other) {
  if (!other.isContainedIn(existing))
    return false;

  existing = other;
  return true;
}

bool AvailabilityContext::PlatformInfo::constrainWith(
    const PlatformInfo &other) {
  bool isConstrained = false;
  isConstrained |= constrainRange(Range, other.Range);
  if (other.IsUnavailable) {
    isConstrained |= constrainUnavailability(other.UnavailablePlatform);
    isConstrained |=
        CONSTRAIN_BOOL(IsUnavailableInEmbedded, other.IsUnavailableInEmbedded);
  }
  isConstrained |= CONSTRAIN_BOOL(IsDeprecated, other.IsDeprecated);
  isConstrained |= CONSTRAIN_BOOL(AllowsUnsafe, other.AllowsUnsafe);

  return isConstrained;
}

bool AvailabilityContext::PlatformInfo::constrainWith(const Decl *decl) {
  bool isConstrained = false;

  if (auto range = AvailabilityInference::annotatedAvailableRange(decl))
    isConstrained |= constrainRange(Range, *range);

  if (auto attr = decl->getUnavailableAttr()) {
    isConstrained |= constrainUnavailability(attr->getPlatform());
    isConstrained |=
        CONSTRAIN_BOOL(IsUnavailableInEmbedded, attr->isEmbeddedSpecific());
  }

  isConstrained |= CONSTRAIN_BOOL(IsDeprecated, decl->isDeprecated());
  isConstrained |= CONSTRAIN_BOOL(AllowsUnsafe, decl->allowsUnsafe());

  return isConstrained;
}

bool AvailabilityContext::PlatformInfo::constrainUnavailability(
    std::optional<PlatformKind> unavailablePlatform) {
  if (!unavailablePlatform)
    return false;

  if (IsUnavailable) {
    // Universal unavailability cannot be refined.
    if (UnavailablePlatform == PlatformKind::none)
      return false;

    // There's nothing to do if the platforms already match.
    if (UnavailablePlatform == *unavailablePlatform)
      return false;

    // The new platform must be more restrictive.
    if (*unavailablePlatform != PlatformKind::none &&
        inheritsAvailabilityFromPlatform(*unavailablePlatform,
                                         UnavailablePlatform))
      return false;
  }

  IsUnavailable = true;
  UnavailablePlatform = *unavailablePlatform;
  return true;
}

bool AvailabilityContext::PlatformInfo::isContainedIn(
    const PlatformInfo &other) const {
  if (!Range.isContainedIn(other.Range))
    return false;

  if (!IsUnavailable && other.IsUnavailable)
    return false;

  if (IsUnavailable && other.IsUnavailable) {
    if (UnavailablePlatform != other.UnavailablePlatform &&
        UnavailablePlatform != PlatformKind::none &&
        inheritsAvailabilityFromPlatform(UnavailablePlatform,
                                         other.UnavailablePlatform))
      return false;

    if (IsUnavailableInEmbedded && !other.IsUnavailableInEmbedded)
      return false;
  }

  if (!IsDeprecated && other.IsDeprecated)
    return false;

  return true;
}

void AvailabilityContext::Storage::Profile(llvm::FoldingSetNodeID &id) const {
  Platform.Profile(id);
}

AvailabilityContext
AvailabilityContext::forPlatformRange(const AvailabilityRange &range,
                                      ASTContext &ctx) {
  PlatformInfo platformInfo{range, PlatformKind::none,
                            /*IsUnavailable*/ false,
                            /*IsUnavailableInEmbedded*/ false,
                            /*IsDeprecated*/ false,
                            /*AllowsUnsafe*/ false};
  return AvailabilityContext(Storage::get(platformInfo, ctx));
}

AvailabilityContext AvailabilityContext::forInliningTarget(ASTContext &ctx) {
  return AvailabilityContext::forPlatformRange(
      AvailabilityRange::forInliningTarget(ctx), ctx);
}

AvailabilityContext AvailabilityContext::forDeploymentTarget(ASTContext &ctx) {
  return AvailabilityContext::forPlatformRange(
      AvailabilityRange::forDeploymentTarget(ctx), ctx);
}

AvailabilityContext
AvailabilityContext::get(const AvailabilityRange &platformAvailability,
                         std::optional<PlatformKind> unavailablePlatform,
                         bool deprecated, ASTContext &ctx) {
  PlatformInfo platformInfo{platformAvailability,
                            unavailablePlatform.has_value()
                                ? *unavailablePlatform
                                : PlatformKind::none,
                            unavailablePlatform.has_value(),
                            /*IsUnavailableInEmbedded*/ false, deprecated,
                            /*AllowsUnsafe*/ false};
  return AvailabilityContext(Storage::get(platformInfo, ctx));
}

AvailabilityRange AvailabilityContext::getPlatformRange() const {
  return Info->Platform.Range;
}

std::optional<PlatformKind>
AvailabilityContext::getUnavailablePlatformKind() const {
  if (Info->Platform.IsUnavailable)
    return Info->Platform.UnavailablePlatform;
  return std::nullopt;
}

bool AvailabilityContext::isUnavailableInEmbedded() const {
  return Info->Platform.IsUnavailableInEmbedded;
}

bool AvailabilityContext::allowsUnsafe() const {
  return Info->Platform.AllowsUnsafe;
}

bool AvailabilityContext::isDeprecated() const {
  return Info->Platform.IsDeprecated;
}

void AvailabilityContext::constrainWithContext(const AvailabilityContext &other,
                                               ASTContext &ctx) {
  PlatformInfo platformAvailability{Info->Platform};
  if (platformAvailability.constrainWith(other.Info->Platform)) {
    Info = Storage::get(platformAvailability, ctx);
  }
}

void AvailabilityContext::constrainWithDecl(const Decl *decl) {
  constrainWithDeclAndPlatformRange(decl, AvailabilityRange::alwaysAvailable());
}

void AvailabilityContext::constrainWithAllowsUnsafe(ASTContext &ctx) {
  if (allowsUnsafe())
    return;

  PlatformInfo platformInfo{Info->Platform};
  platformInfo.AllowsUnsafe = true;
  Info = Storage::get(platformInfo, ctx);
}

void AvailabilityContext::constrainWithPlatformRange(
    const AvailabilityRange &platformRange, ASTContext &ctx) {
  PlatformInfo platformAvailability{Info->Platform};
  if (!constrainRange(platformAvailability.Range, platformRange))
    return;

  Info = Storage::get(platformAvailability, ctx);
}

void AvailabilityContext::constrainWithDeclAndPlatformRange(
    const Decl *decl, const AvailabilityRange &platformRange) {
  PlatformInfo platformAvailability{Info->Platform};
  bool isConstrained = false;
  isConstrained |= platformAvailability.constrainWith(decl);
  isConstrained |= constrainRange(platformAvailability.Range, platformRange);

  if (!isConstrained)
    return;

  Info = Storage::get(platformAvailability, decl->getASTContext());
}

bool AvailabilityContext::isContainedIn(const AvailabilityContext other) const {
  if (!Info->Platform.isContainedIn(other.Info->Platform))
    return false;

  return true;
}

static std::string
stringForAvailability(const AvailabilityRange &availability) {
  if (availability.isAlwaysAvailable())
    return "all";
  if (availability.isKnownUnreachable())
    return "none";

  return availability.getVersionString();
}

void AvailabilityContext::print(llvm::raw_ostream &os) const {
  os << "version=" << stringForAvailability(getPlatformRange());

  if (auto unavailablePlatform = getUnavailablePlatformKind())
    os << " unavailable=" << platformString(*unavailablePlatform);

  if (isDeprecated())
    os << " deprecated";

  if (allowsUnsafe())
    os << " allows_unsafe";
}

void AvailabilityContext::dump() const { print(llvm::errs()); }
