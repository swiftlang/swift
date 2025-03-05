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

struct AvailabilityDomainInfoComparator {
  bool operator()(const AvailabilityContext::DomainInfo &lhs,
                  const AvailabilityContext::DomainInfo &rhs) const {
    StableAvailabilityDomainComparator domainComparator;
    return domainComparator(lhs.getDomain(), rhs.getDomain());
  }
};

static bool constrainBool(bool &existing, bool other) {
  if (existing || !other)
    return false;
  existing = other;
  return true;
}

static bool constrainRange(AvailabilityRange &existing,
                           const AvailabilityRange &other) {
  if (!other.isContainedIn(existing))
    return false;

  existing = other;
  return true;
}

/// Returns true if `domain` is not already contained in `domainInfos` as an
/// unavailable domain. Also, removes domains from `unavailableDomains` that are
/// contained in `domain`.
static bool shouldConstrainUnavailableDomains(
    AvailabilityDomain domain,
    llvm::SmallVectorImpl<AvailabilityContext::DomainInfo> &domainInfos) {
  bool didRemove = false;
  for (auto iter = domainInfos.rbegin(), end = domainInfos.rend(); iter != end;
       ++iter) {
    auto const &domainInfo = *iter;
    auto existingDomain = domainInfo.getDomain();

    if (!domainInfo.isUnavailable())
      continue;

    // Check if the domain is already unavailable.
    if (existingDomain.contains(domain)) {
      ASSERT(!didRemove); // This would indicate that the context is malformed.
      return false;
    }

    // Check if the existing domain would be absorbed by the new domain.
    if (domain.contains(existingDomain)) {
      domainInfos.erase((iter + 1).base());
      didRemove = true;
    }
  }

  return true;
}

static bool constrainDomainInfos(
    llvm::SmallVectorImpl<AvailabilityContext::DomainInfo> &domainInfos,
    llvm::ArrayRef<AvailabilityContext::DomainInfo> otherDomainInfos) {
  llvm::SmallVector<AvailabilityContext::DomainInfo, 4> domainInfosToAdd;

  for (auto otherDomainInfo : otherDomainInfos) {
    if (otherDomainInfo.isUnavailable())
      if (shouldConstrainUnavailableDomains(otherDomainInfo.getDomain(),
                                            domainInfos))
        domainInfosToAdd.push_back(otherDomainInfo);
  }

  if (domainInfosToAdd.size() < 1)
    return false;

  // Add the candidate domain and then re-sort.
  for (auto domainInfo : domainInfosToAdd)
    domainInfos.push_back(domainInfo);

  llvm::sort(domainInfos, AvailabilityDomainInfoComparator());
  return true;
}

AvailabilityContext
AvailabilityContext::forPlatformRange(const AvailabilityRange &range,
                                      const ASTContext &ctx) {
  return AvailabilityContext(
      Storage::get(range, /*isDeprecated=*/false, /*domainInfos=*/{}, ctx));
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
  return storage->platformRange;
}

bool AvailabilityContext::isUnavailable() const {
  for (auto domainInfo : storage->getDomainInfos()) {
    if (domainInfo.isUnavailable())
      return true;
  }
  return false;
}

bool AvailabilityContext::containsUnavailableDomain(
    AvailabilityDomain domain) const {
  for (auto domainInfo : storage->getDomainInfos()) {
    if (domainInfo.isUnavailable()) {
      if (domainInfo.getDomain().contains(domain))
        return true;
    }
  }
  return false;
}

bool AvailabilityContext::isDeprecated() const { return storage->isDeprecated; }

void AvailabilityContext::constrainWithContext(const AvailabilityContext &other,
                                               const ASTContext &ctx) {
  bool isConstrained = false;
  auto platformRange = storage->platformRange;
  isConstrained |= constrainRange(platformRange, other.storage->platformRange);

  bool isDeprecated = storage->isDeprecated;
  isConstrained |= constrainBool(isDeprecated, other.storage->isDeprecated);

  auto domainInfos = storage->copyDomainInfos();
  isConstrained |=
      constrainDomainInfos(domainInfos, other.storage->getDomainInfos());

  if (!isConstrained)
    return;

  storage = Storage::get(platformRange, isDeprecated, domainInfos, ctx);
}

void AvailabilityContext::constrainWithPlatformRange(
    const AvailabilityRange &otherPlatformRange, const ASTContext &ctx) {

  auto platformRange = storage->platformRange;
  if (!constrainRange(platformRange, otherPlatformRange))
    return;

  storage = Storage::get(platformRange, storage->isDeprecated,
                         storage->getDomainInfos(), ctx);
}

void AvailabilityContext::constrainWithUnavailableDomain(
    AvailabilityDomain domain, const ASTContext &ctx) {
  auto domainInfos = storage->copyDomainInfos();
  if (!constrainDomainInfos(domainInfos, {DomainInfo::unavailable(domain)}))
    return;

  storage = Storage::get(storage->platformRange, storage->isDeprecated,
                         domainInfos, ctx);
}

void AvailabilityContext::constrainWithDecl(const Decl *decl) {
  constrainWithDeclAndPlatformRange(decl, AvailabilityRange::alwaysAvailable());
}

void AvailabilityContext::constrainWithDeclAndPlatformRange(
    const Decl *decl, const AvailabilityRange &otherPlatformRange) {
  auto &ctx = decl->getASTContext();
  bool isConstrained = false;
  auto platformRange = storage->platformRange;
  isConstrained |= constrainRange(platformRange, otherPlatformRange);

  bool isDeprecated = storage->isDeprecated;
  isConstrained |= constrainBool(isDeprecated, decl->isDeprecated());

  llvm::SmallVector<DomainInfo, 4> declDomainInfos;
  AvailabilityConstraintFlags flags =
      AvailabilityConstraintFlag::SkipEnclosingExtension;
  auto constraints =
      swift::getAvailabilityConstraintsForDecl(decl, *this, flags);
  for (auto constraint : constraints) {
    auto attr = constraint.getAttr();
    auto domain = attr.getDomain();
    switch (constraint.getReason()) {
    case AvailabilityConstraint::Reason::UnconditionallyUnavailable:
    case AvailabilityConstraint::Reason::Obsoleted:
    case AvailabilityConstraint::Reason::UnavailableForDeployment:
      declDomainInfos.push_back(DomainInfo::unavailable(domain));
      break;
    case AvailabilityConstraint::Reason::PotentiallyUnavailable:
      DEBUG_ASSERT(domain.isPlatform());
      if (domain.isPlatform())
        isConstrained |=
            constrainRange(platformRange, attr.getIntroducedRange(ctx));
      // FIXME: [availability] Store other potentially unavailable domains in
      // domainInfos.
      break;
    }
  }

  auto domainInfos = storage->copyDomainInfos();
  isConstrained |= constrainDomainInfos(domainInfos, declDomainInfos);

  if (!isConstrained)
    return;

  storage = Storage::get(platformRange, isDeprecated, domainInfos,
                         decl->getASTContext());
}

bool AvailabilityContext::isContainedIn(const AvailabilityContext other) const {
  // The available versions range be the same or smaller.
  if (!storage->platformRange.isContainedIn(other.storage->platformRange))
    return false;

  // The set of deprecated domains should be the same or larger.
  if (!storage->isDeprecated && other.storage->isDeprecated)
    return false;

  // Every unavailable domain in the other context should be contained in some
  // unavailable domain in this context.
  bool disjointUnavailability = llvm::any_of(
      other.storage->getDomainInfos(), [&](const DomainInfo &otherDomainInfo) {
        return llvm::none_of(storage->getDomainInfos(),
                             [&otherDomainInfo](const DomainInfo &domainInfo) {
                               return domainInfo.getDomain().contains(
                                   otherDomainInfo.getDomain());
                             });
      });
  if (disjointUnavailability)
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

  auto domainInfos = storage->getDomainInfos();
  if (domainInfos.size() > 0) {
    os << " unavailable=";
    llvm::interleave(
        domainInfos, os,
        [&](const DomainInfo &domainInfo) { domainInfo.getDomain().print(os); },
        ",");
  }

  if (isDeprecated())
    os << " deprecated";
}

void AvailabilityContext::dump() const { print(llvm::errs()); }

bool AvailabilityContext::verify(const ASTContext &ctx) const {
  // Domain infos must be sorted to ensure folding set node lookups yield
  // consistent results.
  if (!llvm::is_sorted(storage->getDomainInfos(),
                       AvailabilityDomainInfoComparator()))
    return false;

  return true;
}

void AvailabilityContext::Storage::Profile(
    llvm::FoldingSetNodeID &ID, const AvailabilityRange &platformRange,
    bool isDeprecated,
    llvm::ArrayRef<AvailabilityContext::DomainInfo> domainInfos) {
  platformRange.getRawVersionRange().Profile(ID);
  ID.AddBoolean(isDeprecated);
  ID.AddInteger(domainInfos.size());
  for (auto domainInfo : domainInfos) {
    domainInfo.Profile(ID);
  }
}
