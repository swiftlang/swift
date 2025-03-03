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

bool AvailabilityContext::Info::constrainWith(const Info &other) {
  bool isConstrained = false;
  isConstrained |= constrainRange(PlatformRange, other.PlatformRange);
  isConstrained |= constrainUnavailability(other.UnavailableDomains);
  isConstrained |= CONSTRAIN_BOOL(IsDeprecated, other.IsDeprecated);

  return isConstrained;
}

bool AvailabilityContext::Info::constrainWith(
    const DeclAvailabilityConstraints &constraints, const ASTContext &ctx) {
  bool isConstrained = false;

  for (auto constraint : constraints) {
    auto attr = constraint.getAttr();
    auto domain = attr.getDomain();
    switch (constraint.getReason()) {
    case AvailabilityConstraint::Reason::UnconditionallyUnavailable:
    case AvailabilityConstraint::Reason::Obsoleted:
    case AvailabilityConstraint::Reason::UnavailableForDeployment:
      isConstrained |= constrainUnavailability(domain);
      break;
    case AvailabilityConstraint::Reason::PotentiallyUnavailable:
      // FIXME: [availability] Support versioning for other kinds of domains.
      DEBUG_ASSERT(domain.isPlatform());
      if (domain.isPlatform())
        isConstrained |=
            constrainRange(PlatformRange, attr.getIntroducedRange(ctx));
      break;
    }
  }

  return isConstrained;
}

/// Returns true if `domain` is not already contained in `unavailableDomains`.
/// Also, removes domains from `unavailableDomains` that are contained in
/// `domain`.
static bool shouldConstrainUnavailableDomains(
    AvailabilityDomain domain,
    llvm::SmallVectorImpl<AvailabilityDomain> &unavailableDomains) {
  bool didRemove = false;
  for (auto iter = unavailableDomains.rbegin(), end = unavailableDomains.rend();
       iter != end; ++iter) {
    auto const &existingDomain = *iter;

    // Check if the domain is already unavailable.
    if (existingDomain.contains(domain)) {
      ASSERT(!didRemove); // This would indicate that the context is malformed.
      return false;
    }

    // Check if the existing domain would be absorbed by the new domain.
    if (domain.contains(existingDomain)) {
      unavailableDomains.erase((iter + 1).base());
      didRemove = true;
    }
  }

  return true;
}

bool AvailabilityContext::Info::constrainUnavailability(
    const llvm::SmallVectorImpl<AvailabilityDomain> &domains) {
  llvm::SmallVector<AvailabilityDomain, 2> domainsToAdd;

  for (auto domain : domains) {
    if (shouldConstrainUnavailableDomains(domain, UnavailableDomains))
      domainsToAdd.push_back(domain);
  }

  if (domainsToAdd.size() < 1)
    return false;

  // Add the candidate domain and then re-sort.
  for (auto domain : domainsToAdd)
    UnavailableDomains.push_back(domain);

  llvm::sort(UnavailableDomains, StableAvailabilityDomainComparator());
  return true;
}

bool AvailabilityContext::Info::isContainedIn(const Info &other) const {
  // The available versions range be the same or smaller.
  if (!PlatformRange.isContainedIn(other.PlatformRange))
    return false;

  // Every unavailable domain in the other context should be contained in some
  // unavailable domain in this context.
  bool disjointUnavailability = llvm::any_of(
      other.UnavailableDomains,
      [&](const AvailabilityDomain &otherUnavailableDomain) {
        return llvm::none_of(
            UnavailableDomains,
            [&otherUnavailableDomain](const AvailabilityDomain &domain) {
              return domain.contains(otherUnavailableDomain);
            });
      });

  if (disjointUnavailability)
    return false;

  // The set of deprecated domains should be the same or larger.
  if (!IsDeprecated && other.IsDeprecated)
    return false;

  return true;
}

void AvailabilityContext::Info::Profile(llvm::FoldingSetNodeID &ID) const {
  PlatformRange.getRawVersionRange().Profile(ID);
  ID.AddInteger(UnavailableDomains.size());
  for (auto domain : UnavailableDomains) {
    domain.Profile(ID);
  }
  ID.AddBoolean(IsDeprecated);
}

bool AvailabilityContext::Info::verify(const ASTContext &ctx) const {
  // Unavailable domains must be sorted to ensure folding set node lookups yield
  // consistent results.
  if (!llvm::is_sorted(UnavailableDomains,
                       StableAvailabilityDomainComparator()))
    return false;

  return true;
}

AvailabilityContext
AvailabilityContext::forPlatformRange(const AvailabilityRange &range,
                                      const ASTContext &ctx) {
  Info info{range, /*UnavailableDomains*/ {},
            /*IsDeprecated*/ false};
  return AvailabilityContext(Storage::get(info, ctx));
}

AvailabilityContext
AvailabilityContext::forInliningTarget(const ASTContext &ctx) {
  return AvailabilityContext::forPlatformRange(
      AvailabilityRange::forInliningTarget(ctx), ctx);
}

AvailabilityContext
AvailabilityContext::forDeploymentTarget(const ASTContext &ctx) {
  return AvailabilityContext::forPlatformRange(
      AvailabilityRange::forDeploymentTarget(ctx), ctx);
}

AvailabilityRange AvailabilityContext::getPlatformRange() const {
  return storage->info.PlatformRange;
}

bool AvailabilityContext::isUnavailable() const {
  return storage->info.UnavailableDomains.size() > 0;
}

bool AvailabilityContext::containsUnavailableDomain(
    AvailabilityDomain domain) const {
  for (auto unavailableDomain : storage->info.UnavailableDomains) {
    if (unavailableDomain.contains(domain))
      return true;
  }
  return false;
}

bool AvailabilityContext::isDeprecated() const {
  return storage->info.IsDeprecated;
}

void AvailabilityContext::constrainWithContext(const AvailabilityContext &other,
                                               const ASTContext &ctx) {
  bool isConstrained = false;

  Info info{storage->info};
  isConstrained |= info.constrainWith(other.storage->info);

  if (!isConstrained)
    return;

  storage = Storage::get(info, ctx);
}

void AvailabilityContext::constrainWithPlatformRange(
    const AvailabilityRange &platformRange, const ASTContext &ctx) {

  Info info{storage->info};
  if (!constrainRange(info.PlatformRange, platformRange))
    return;

  storage = Storage::get(info, ctx);
}

void AvailabilityContext::constrainWithUnavailableDomain(
    AvailabilityDomain domain, const ASTContext &ctx) {
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
  isConstrained |= constrainRange(info.PlatformRange, platformRange);

  if (!isConstrained)
    return;

  storage = Storage::get(info, decl->getASTContext());
}

bool AvailabilityContext::isContainedIn(const AvailabilityContext other) const {
  return storage->info.isContainedIn(other.storage->info);
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

  if (storage->info.UnavailableDomains.size() > 0) {
    os << " unavailable=";
    llvm::interleave(
        storage->info.UnavailableDomains, os,
        [&](const AvailabilityDomain &domain) { domain.print(os); }, ",");
  }

  if (isDeprecated())
    os << " deprecated";
}

void AvailabilityContext::dump() const { print(llvm::errs()); }

bool AvailabilityContext::verify(const ASTContext &ctx) const {
  return storage->info.verify(ctx);
}
