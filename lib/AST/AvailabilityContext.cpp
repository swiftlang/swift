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
#include "swift/AST/AvailabilityConstraint.h"
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

static bool constrainUnavailableDomain(
    std::optional<AvailabilityDomain> &domain,
    const std::optional<AvailabilityDomain> &otherDomain) {
  // If the other domain is absent or is the same domain, it's a noop.
  if (!otherDomain || domain == otherDomain)
    return false;

  // Check if the other domain is a superset and constrain to it if it is.
  if (!domain || otherDomain->contains(*domain)) {
    domain = otherDomain;
    return true;
  }

  return false;
}

bool AvailabilityContext::Info::constrainWith(const Info &other) {
  bool isConstrained = false;
  isConstrained |= constrainRange(Range, other.Range);
  if (other.UnavailableDomain)
    isConstrained |= constrainUnavailability(other.UnavailableDomain);
  isConstrained |= CONSTRAIN_BOOL(IsDeprecated, other.IsDeprecated);

  return isConstrained;
}

bool AvailabilityContext::Info::constrainWith(
    const DeclAvailabilityConstraints &constraints, ASTContext &ctx) {
  bool isConstrained = false;

  for (auto constraint : constraints) {
    auto attr = constraint.getAttr();
    auto domain = attr.getDomain();
    switch (constraint.getReason()) {
    case AvailabilityConstraint::Reason::UnconditionallyUnavailable:
    case AvailabilityConstraint::Reason::Obsoleted:
    case AvailabilityConstraint::Reason::IntroducedInLaterVersion:
      isConstrained |= constrainUnavailability(domain);
      break;
    case AvailabilityConstraint::Reason::IntroducedInLaterDynamicVersion:
      // FIXME: [availability] Support versioning for other kinds of domains.
      DEBUG_ASSERT(domain.isPlatform());
      if (domain.isPlatform())
        isConstrained |= constrainRange(Range, attr.getIntroducedRange(ctx));
      break;
    }
  }

  return isConstrained;
}

bool AvailabilityContext::Info::constrainUnavailability(
    std::optional<AvailabilityDomain> domain) {
  return constrainUnavailableDomain(UnavailableDomain, domain);
}

bool AvailabilityContext::Info::isContainedIn(const Info &other) const {
  // The available versions range be the same or smaller.
  if (!Range.isContainedIn(other.Range))
    return false;

  // The set of unavailable domains should be the same or larger.
  if (auto otherUnavailableDomain = other.UnavailableDomain) {
    if (!UnavailableDomain)
      return false;

    if (!UnavailableDomain->contains(otherUnavailableDomain.value()))
      return false;
  }

  // The set of deprecated domains should be the same or larger.
  if (!IsDeprecated && other.IsDeprecated)
    return false;

  return true;
}

AvailabilityContext
AvailabilityContext::forPlatformRange(const AvailabilityRange &range,
                                      ASTContext &ctx) {
  Info info{range, /*UnavailableDomain*/ std::nullopt,
            /*IsDeprecated*/ false};
  return AvailabilityContext(Storage::get(info, ctx));
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
                         std::optional<AvailabilityDomain> unavailableDomain,
                         bool deprecated, ASTContext &ctx) {
  Info info{platformAvailability, unavailableDomain, deprecated};
  return AvailabilityContext(Storage::get(info, ctx));
}

AvailabilityRange AvailabilityContext::getPlatformRange() const {
  return storage->info.Range;
}

bool AvailabilityContext::isUnavailable() const {
  return storage->info.UnavailableDomain.has_value();
}

bool AvailabilityContext::containsUnavailableDomain(
    AvailabilityDomain domain) const {
  if (auto unavailableDomain = storage->info.UnavailableDomain)
    return unavailableDomain->contains(domain);

  return false;
}

bool AvailabilityContext::isDeprecated() const {
  return storage->info.IsDeprecated;
}

void AvailabilityContext::constrainWithContext(const AvailabilityContext &other,
                                               ASTContext &ctx) {
  bool isConstrained = false;

  Info info{storage->info};
  isConstrained |= info.constrainWith(other.storage->info);

  if (!isConstrained)
    return;

  storage = Storage::get(info, ctx);
}

void AvailabilityContext::constrainWithPlatformRange(
    const AvailabilityRange &platformRange, ASTContext &ctx) {

  Info info{storage->info};
  if (!constrainRange(info.Range, platformRange))
    return;

  storage = Storage::get(info, ctx);
}

void AvailabilityContext::constrainWithUnavailableDomain(
    AvailabilityDomain domain, ASTContext &ctx) {
  Info info{storage->info};
  if (!info.constrainUnavailability(domain))
    return;

  storage = Storage::get(info, ctx);
}

void AvailabilityContext::constrainWithDecl(const Decl *decl) {
  constrainWithDeclAndPlatformRange(decl, AvailabilityRange::alwaysAvailable());
}

void AvailabilityContext::constrainWithDeclAndPlatformRange(
    const Decl *decl, const AvailabilityRange &platformRange) {
  bool isConstrained = false;

  Info info{storage->info};
  AvailabilityConstraintFlags flags =
      AvailabilityConstraintFlag::SkipEnclosingExtension;
  auto constraints =
      swift::getAvailabilityConstraintsForDecl(decl, *this, flags);
  isConstrained |= info.constrainWith(constraints, decl->getASTContext());
  isConstrained |= CONSTRAIN_BOOL(info.IsDeprecated, decl->isDeprecated());
  isConstrained |= constrainRange(info.Range, platformRange);

  if (!isConstrained)
    return;

  storage = Storage::get(info, decl->getASTContext());
}

bool AvailabilityContext::isContainedIn(const AvailabilityContext other) const {
  if (!storage->info.isContainedIn(other.storage->info))
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

  if (auto unavailableDomain = storage->info.UnavailableDomain)
    os << " unavailable=" << unavailableDomain->getNameForAttributePrinting();

  if (isDeprecated())
    os << " deprecated";
}

void AvailabilityContext::dump() const { print(llvm::errs()); }
