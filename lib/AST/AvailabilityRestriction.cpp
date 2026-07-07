//===--- AvailabilityRestriction.cpp - Swift Availability Restrictions ----===//
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

#include "swift/AST/AvailabilityRestriction.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/AvailabilityContext.h"
#include "swift/AST/Decl.h"

using namespace swift;

AvailabilityDomainAndRange
AvailabilityRestriction::getDomainAndRange(const ASTContext &ctx) const {
  switch (getReason()) {
  case Reason::UnavailableUnconditionally:
  case Reason::UnavailableObsolete:
    return getAttr().getObsoletedDomainAndRange(ctx).value();
  case Reason::UnavailableUnintroduced:
  case Reason::Unintroduced:
    return getAttr().getIntroducedDomainAndRange(ctx).value();
  case Reason::Deprecated:
    return getAttr().getDeprecatedDomainAndRange(ctx).value();
  }
}

AvailabilityDomainAndRange
AvailabilityRestriction::getFixItDomainAndRange(const ASTContext &ctx) const {
  auto attrDomain = getAttr().getDomain();
  if (attrDomain.contains(
          AvailabilityDomain::forPlatform(PlatformKind::anyAppleOS))) {
    switch (getReason()) {
    case Reason::UnavailableUnconditionally:
    case Reason::UnavailableObsolete:
      return AvailabilityDomainAndRange(
          attrDomain, AvailabilityRange(getAttr().getObsoleted().value()));
    case Reason::UnavailableUnintroduced:
    case Reason::Unintroduced:
      return AvailabilityDomainAndRange(
          attrDomain, AvailabilityRange(getAttr().getIntroduced().value()));
    case Reason::Deprecated:
      return AvailabilityDomainAndRange(
          attrDomain, AvailabilityRange(getAttr().getDeprecated().value()));
    }
  }
  return getDomainAndRange(ctx);
}

bool AvailabilityRestriction::isActiveForRuntimeQueries(
    const ASTContext &ctx) const {
  if (getAttr().getPlatform() == PlatformKind::none)
    return true;

  return swift::isPlatformActive(getAttr().getPlatform(), ctx.LangOpts,
                                 /*forTargetVariant=*/false,
                                 /*forRuntimeQuery=*/true);
}

void AvailabilityRestriction::print(llvm::raw_ostream &os) const {
  os << "AvailabilityRestriction(";
  getAttr().getDomain().print(os);
  os << ", ";

  std::optional<llvm::VersionTuple> version;
  switch (getReason()) {
  case Reason::UnavailableUnconditionally:
    os << "unavailable";
    break;
  case Reason::UnavailableObsolete:
    os << "obsoleted";
    version = getAttr().getObsoleted();
    break;
  case Reason::UnavailableUnintroduced:
  case Reason::Unintroduced:
    os << "introduced";
    version = getAttr().getIntroduced();
    break;
  case Reason::Deprecated:
    os << "deprecated";
    version = getAttr().getDeprecated();
    break;
  }

  if (version)
    os << ": " << *version;
  os << ")";
}

std::optional<AvailabilityRestriction>
DeclAvailabilityRestrictions::getPrimaryRestriction() const {
  std::optional<AvailabilityRestriction> result;

  auto isStrongerRestriction = [](const AvailabilityRestriction &lhs,
                                  const AvailabilityRestriction &rhs) {
    // Restriction reasons are defined in descending order of strength.
    if (lhs.getReason() != rhs.getReason())
      return lhs.getReason() < rhs.getReason();

    if (lhs.getDomain() != rhs.getDomain()) {
      // Restrictions in the universal domain are the strongest.
      if (rhs.getDomain().isUniversal())
        return true;

      // Otherwise, pick the restriction from the broader domain.
      if (lhs.getDomain() != rhs.getDomain())
        return rhs.getDomain().contains(lhs.getDomain());
    }

    return false;
  };

  // Pick the strongest restriction.
  for (auto const &restriction : restrictions) {
    if (!result || isStrongerRestriction(restriction, *result))
      result.emplace(restriction);
  }

  return result;
}

void DeclAvailabilityRestrictions::print(llvm::raw_ostream &os) const {
  os << "{\n";
  llvm::interleave(
      restrictions,
      [&os](const AvailabilityRestriction &restriction) {
        os << "  " << restriction;
      },
      [&os] { os << ",\n"; });
  os << "\n}";
}

static bool canIgnoreRestrictionInUnavailableContexts(
    const Decl *decl, const AvailabilityRestriction &restriction,
    const AvailabilityRestrictionFlags flags) {
  auto domain = restriction.getDomain();

  // Always reject uses of universally unavailable declarations, regardless
  // of context, since there are no possible compilation configurations in
  // which they are available. However, make an exception for types and
  // conformances, which can sometimes be awkward to avoid references to.
  if (!flags.contains(AvailabilityRestrictionFlag::
                          AllowUniversallyUnavailableInCompatibleContexts)) {
    if (!isa<TypeDecl>(decl) && !isa<ExtensionDecl>(decl)) {
      if (domain.isUniversal() || domain.isSwiftLanguageMode())
        return false;
    }
  }

  switch (restriction.getReason()) {
  case AvailabilityRestriction::Reason::UnavailableUnconditionally:
    return true;

  case AvailabilityRestriction::Reason::Unintroduced:
  case AvailabilityRestriction::Reason::UnavailableObsolete:
  case AvailabilityRestriction::Reason::UnavailableUnintroduced:
    return domain.isVersioned();

  case AvailabilityRestriction::Reason::Deprecated:
    // Filtering of deprecation restrictions is done by
    // getDeprecationRestrictionForAttr().
    return false;
  }
}

static bool
shouldIgnoreRestrictionInContext(const Decl *decl,
                                 const AvailabilityRestriction &restriction,
                                 const AvailabilityContext &context,
                                 const AvailabilityRestrictionFlags flags) {
  if (!context.isUnavailable())
    return false;

  if (!canIgnoreRestrictionInUnavailableContexts(decl, restriction, flags))
    return false;

  // If the restriction's domain is a superset of the compilation's target
  // availability domain, use the more specific target availability domain
  // instead. This allows declarations that are @available(macOS, unavailable)
  // to be used in contexts that are @available(macOSApplicationExtension,
  // unavailable), for example.
  auto &ctx = decl->getASTContext();
  auto domain = restriction.getDomain();
  auto targetDomain = ctx.getTargetAvailabilityDomain();
  if (domain.isSupersetOf(targetDomain))
    domain = targetDomain;

  return context.isUnavailableForDomain(domain);
}

static std::optional<AvailabilityRestriction>
getDeprecationRestrictionForAttr(const Decl *decl,
                                 const SemanticAvailableAttr &attr,
                                 const AvailabilityContext &context,
                                 std::optional<AvailabilityRange> availableRange,
                                 const AvailabilityRestrictionFlags flags) {
  bool includeSoftDeprecation =
      flags.contains(AvailabilityRestrictionFlag::IncludeSoftDeprecation);

  if (context.isDeprecated())
    return std::nullopt;

  if (context.isUnavailable() && isa<TypeDecl>(decl))
    return std::nullopt;

  // Don't diagnose deprecation in contexts that are unreachable for the
  // deprecated domain.
  if (availableRange && availableRange->isKnownUnreachable())
    return std::nullopt;

  if (attr.isUnconditionallyDeprecated())
    return AvailabilityRestriction::deprecated(attr);

  auto &ctx = decl->getASTContext();
  if (auto deprecatedRange = attr.getDeprecatedRange(ctx)) {
    // When IncludeSoftDeprecation is specified, decls deprecated in a future
    // deployment target are diagnosed too.
    if (includeSoftDeprecation)
      return AvailabilityRestriction::deprecated(attr);

    auto deploymentRange = attr.getDomain().getDeploymentRange(ctx);
    if (deploymentRange && deploymentRange->isContainedIn(*deprecatedRange))
      return AvailabilityRestriction::deprecated(attr);
  }

  return std::nullopt;
}

std::optional<AvailabilityRestriction> swift::getAvailabilityRestrictionForAttr(
    const SemanticAvailableAttr &attr, const Decl *decl,
    const AvailabilityContext &context, AvailabilityRestrictionFlags flags) {
  auto getRestriction = [&]() -> std::optional<AvailabilityRestriction> {
    // Is the decl unconditionally unavailable?
    if (attr.isUnconditionallyUnavailable())
      return AvailabilityRestriction::unavailableUnconditionally(attr);

    auto &ctx = decl->getASTContext();
    auto domain = attr.getDomain();
    bool domainSupportsRefinement = domain.supportsContextRefinement();

    // Compute the available range in the given context. If there is no
    // explicit range defined by the context, use the deployment range as
    // fallback.
    std::optional<AvailabilityRange> availableRange;
    if (domainSupportsRefinement)
      availableRange = context.getAvailabilityRange(domain, ctx);
    if (!availableRange)
      availableRange = domain.getDeploymentRange(ctx);

    // Is the decl obsoleted in this context?
    if (auto obsoletedRange = attr.getObsoletedRange(ctx)) {
      if (availableRange && !availableRange->isKnownUnreachable() &&
          availableRange->isContainedIn(*obsoletedRange))
        return AvailabilityRestriction::unavailableObsolete(attr);
    }

    // Is the decl not yet introduced in this context?
    if (auto introducedRange = attr.getIntroducedRange(ctx)) {
      if (!availableRange || !availableRange->isContainedIn(*introducedRange))
        return domainSupportsRefinement
                   ? AvailabilityRestriction::unintroduced(attr)
                   : AvailabilityRestriction::unavailableUnintroduced(attr);
    }

    // Is the decl deprecated in this context?
    if (auto deprecation = getDeprecationRestrictionForAttr(
            decl, attr, context, availableRange, flags))
      return deprecation;

    return std::nullopt;
  };

  auto restriction = getRestriction();
  if (restriction &&
      shouldIgnoreRestrictionInContext(decl, *restriction, context, flags))
    return std::nullopt;

  return restriction;
}

/// Returns true if unsatisfied `@available(..., unavailable)` restrictions for
/// \p domain make code unreachable at runtime
static bool
domainCanBeUnconditionallyUnavailableAtRuntime(AvailabilityDomain domain,
                                               const ASTContext &ctx) {
  switch (domain.getKind()) {
  case AvailabilityDomain::Kind::Universal:
    return true;

  case AvailabilityDomain::Kind::Platform:
    if (ctx.LangOpts.TargetVariant &&
        domain.isActive(ctx, /*forTargetVariant=*/true))
      return true;
    return domain.isActive(ctx);

  case AvailabilityDomain::Kind::SwiftLanguageMode:
  case AvailabilityDomain::Kind::StandaloneSwiftRuntime:
  case AvailabilityDomain::Kind::PackageDescription:
    return false;

  case AvailabilityDomain::Kind::Embedded:
    return ctx.LangOpts.hasFeature(Feature::Embedded);

  case AvailabilityDomain::Kind::Custom:
    switch (domain.getCustomDomain()->getKind()) {
    case CustomAvailabilityDomain::Kind::Enabled:
    case CustomAvailabilityDomain::Kind::AlwaysEnabled:
      return true;
    case CustomAvailabilityDomain::Kind::Disabled:
    case CustomAvailabilityDomain::Kind::Dynamic:
      return false;
    }
  }
}

/// Returns true if unsatisfied introduction restrictions for \p domain make
/// code unreachable at runtime.
static bool
domainIsUnavailableAtRuntimeIfUnintroduced(AvailabilityDomain domain,
                                           const ASTContext &ctx) {
  switch (domain.getKind()) {
  case AvailabilityDomain::Kind::Universal:
  case AvailabilityDomain::Kind::Platform:
  case AvailabilityDomain::Kind::SwiftLanguageMode:
  case AvailabilityDomain::Kind::StandaloneSwiftRuntime:
  case AvailabilityDomain::Kind::PackageDescription:
    return false;

  case AvailabilityDomain::Kind::Embedded:
    return !ctx.LangOpts.hasFeature(Feature::Embedded);

  case AvailabilityDomain::Kind::Custom:
    switch (domain.getCustomDomain()->getKind()) {
    case CustomAvailabilityDomain::Kind::Enabled:
    case CustomAvailabilityDomain::Kind::AlwaysEnabled:
    case CustomAvailabilityDomain::Kind::Dynamic:
      return false;
    case CustomAvailabilityDomain::Kind::Disabled:
      return true;
    }
  }
}

static bool restrictionIndicatesRuntimeUnavailability(
    const AvailabilityRestriction &restriction, const ASTContext &ctx) {
  auto domain = restriction.getDomain();
  switch (restriction.getReason()) {
  case AvailabilityRestriction::Reason::UnavailableUnconditionally:
    return domainCanBeUnconditionallyUnavailableAtRuntime(domain, ctx);
  case AvailabilityRestriction::Reason::UnavailableObsolete:
  case AvailabilityRestriction::Reason::UnavailableUnintroduced:
  case AvailabilityRestriction::Reason::Deprecated:
    return false;
  case AvailabilityRestriction::Reason::Unintroduced:
    return domainIsUnavailableAtRuntimeIfUnintroduced(domain, ctx);
  }
}

void swift::getRuntimeUnavailableDomains(
    const DeclAvailabilityRestrictions &restrictions,
    llvm::SmallVectorImpl<AvailabilityDomain> &domains, const ASTContext &ctx) {
  for (auto restriction : restrictions) {
    if (restrictionIndicatesRuntimeUnavailability(restriction, ctx))
      domains.push_back(restriction.getDomain());
  }
}
