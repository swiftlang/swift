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
#include "swift/AST/Decl.h"
#include "swift/Basic/Assertions.h"

using namespace swift;

bool AvailabilityContext::PlatformInfo::constrainRange(const Decl *decl) {
  if (auto range = AvailabilityInference::annotatedAvailableRange(decl))
    return constrainRange(*range);

  return false;
}

bool AvailabilityContext::PlatformInfo::constrainUnavailability(
    const Decl *decl) {
  auto &ctx = decl->getASTContext();
  auto *attr = decl->getAttrs().getUnavailable(ctx);
  if (!attr)
    return false;

  // Check whether the decl's unavailability reason is the same.
  if (IsUnavailable && UnavailablePlatform == attr->Platform)
    return false;

  // Check whether the decl's unavailability reason is more specific.
  if (attr->Platform != PlatformKind::none &&
      inheritsAvailabilityFromPlatform(attr->Platform, UnavailablePlatform))
    return false;

  IsUnavailable = true;
  UnavailablePlatform = attr->Platform;
  return true;
}

bool AvailabilityContext::PlatformInfo::constrainDeprecated(const Decl *decl) {
  auto &ctx = decl->getASTContext();
  if (IsDeprecated || !decl->getAttrs().isDeprecated(ctx))
    return false;

  IsDeprecated = true;
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
        !inheritsAvailabilityFromPlatform(UnavailablePlatform,
                                          other.UnavailablePlatform))
      return false;
  }

  if (!IsDeprecated && other.IsDeprecated)
    return false;

  return true;
}

const AvailabilityContext *AvailabilityContext::getDefault(ASTContext &ctx) {
  PlatformInfo platformInfo{AvailabilityRange::forInliningTarget(ctx),
                            PlatformKind::none,
                            /*IsUnavailable*/ false,
                            /*IsDeprecated*/ false};
  return AvailabilityContext::get(platformInfo, ctx);
}

/// Returns the unique context that is the result of constraining the current
/// context's platform availability range with `platformRange`.
const AvailabilityContext *AvailabilityContext::constrainWithPlatformRange(
    const AvailabilityRange &platformRange, ASTContext &ctx) const {
  PlatformInfo platformAvailability{PlatformAvailability};
  if (platformAvailability.constrainRange(platformRange))
    return get(platformAvailability, ctx);

  return this;
}

const AvailabilityContext *
AvailabilityContext::constrainWithDeclAndPlatformRange(
    Decl *decl, const AvailabilityRange &platformRange) const {
  PlatformInfo platformAvailability{PlatformAvailability};
  bool isConstrained = false;
  isConstrained |= platformAvailability.constrainRange(decl);
  isConstrained |= platformAvailability.constrainRange(platformRange);
  isConstrained |= platformAvailability.constrainUnavailability(decl);
  isConstrained |= platformAvailability.constrainDeprecated(decl);

  if (!isConstrained)
    return this;

  return get(platformAvailability, decl->getASTContext());
}

bool AvailabilityContext::isContainedIn(
    const AvailabilityContext *other) const {
  if (!PlatformAvailability.isContainedIn(other->PlatformAvailability))
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
}

void AvailabilityContext::dump() const { print(llvm::errs()); }

void AvailabilityContext::Profile(llvm::FoldingSetNodeID &id) const {
  PlatformAvailability.Profile(id);
}
