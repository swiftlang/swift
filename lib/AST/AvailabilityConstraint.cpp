//===--- AvailabilityConstraint.cpp - Swift Availability Constraints ------===//
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

#include "swift/AST/AvailabilityConstraint.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/AvailabilityContext.h"
#include "swift/AST/Decl.h"

using namespace swift;

AvailabilityDomainAndRange
AvailabilityConstraint::getDomainAndRange(const ASTContext &ctx) const {
  switch (getReason()) {
  case Reason::UnavailableUnconditionally:
  case Reason::UnavailableObsolete:
    return getAttr().getObsoletedDomainAndRange(ctx).value();
  case Reason::UnavailableUnintroduced:
  case Reason::Unintroduced:
    return getAttr().getIntroducedDomainAndRange(ctx).value();
  }
}

bool AvailabilityConstraint::isActiveForRuntimeQueries(
    const ASTContext &ctx) const {
  if (getAttr().getPlatform() == PlatformKind::none)
    return true;

  return swift::isPlatformActive(getAttr().getPlatform(), ctx.LangOpts,
                                 /*forTargetVariant=*/false,
                                 /*forRuntimeQuery=*/true);
}

void AvailabilityConstraint::print(llvm::raw_ostream &os) const {
  os << "AvailabilityConstraint(";
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
  }

  if (version)
    os << ": " << *version;
  os << ")";
}

static bool constraintIsStronger(const AvailabilityConstraint &lhs,
                                 const AvailabilityConstraint &rhs) {
  DEBUG_ASSERT(lhs.getDomain() == rhs.getDomain());

  // If the constraints have matching domains but different reasons, the
  // constraint with the lowest reason is "strongest".
  if (lhs.getReason() != rhs.getReason())
    return lhs.getReason() < rhs.getReason();

  switch (lhs.getReason()) {
  case AvailabilityConstraint::Reason::UnavailableUnconditionally:
    // Just keep the first.
    return false;

  case AvailabilityConstraint::Reason::UnavailableObsolete:
    // Pick the larger obsoleted range.
    return lhs.getAttr().getObsoleted().value() <
           rhs.getAttr().getObsoleted().value();

  case AvailabilityConstraint::Reason::UnavailableUnintroduced:
  case AvailabilityConstraint::Reason::Unintroduced:
    // Pick the smaller introduced range.
    return lhs.getAttr().getIntroduced().value_or(llvm::VersionTuple()) >
           rhs.getAttr().getIntroduced().value_or(llvm::VersionTuple());
  }
}

void addConstraint(llvm::SmallVector<AvailabilityConstraint, 4> &constraints,
                   const AvailabilityConstraint &constraint,
                   const ASTContext &ctx) {

  auto iter = llvm::find_if(
      constraints, [&constraint](AvailabilityConstraint &existing) {
        return constraint.getDomain() == existing.getDomain();
      });

  // There's no existing constraint for the same domain so just add it.
  if (iter == constraints.end()) {
    constraints.emplace_back(constraint);
    return;
  }

  if (constraintIsStronger(constraint, *iter)) {
    constraints.erase(iter);
    constraints.emplace_back(constraint);
  }
}

std::optional<AvailabilityConstraint>
DeclAvailabilityConstraints::getPrimaryConstraint() const {
  std::optional<AvailabilityConstraint> result;

  auto isStrongerConstraint = [](const AvailabilityConstraint &lhs,
                                 const AvailabilityConstraint &rhs) {
    // Constraint reasons are defined in descending order of strength.
    if (lhs.getReason() != rhs.getReason())
      return lhs.getReason() < rhs.getReason();

    // Pick the constraint from the broader domain.
    if (lhs.getDomain() != rhs.getDomain())
      return rhs.getDomain().contains(lhs.getDomain());
    
    return false;
  };

  // Pick the strongest constraint.
  for (auto const &constraint : constraints) {
    if (!result || isStrongerConstraint(constraint, *result))
      result.emplace(constraint);
  }

  return result;
}

void DeclAvailabilityConstraints::print(llvm::raw_ostream &os) const {
  os << "{\n";
  llvm::interleave(
      constraints,
      [&os](const AvailabilityConstraint &constraint) {
        os << "  " << constraint;
      },
      [&os] { os << ",\n"; });
  os << "\n}";
}

static bool canIgnoreConstraintInUnavailableContexts(
    const Decl *decl, const AvailabilityConstraint &constraint,
    const AvailabilityConstraintFlags flags) {
  auto domain = constraint.getDomain();

  switch (constraint.getReason()) {
  case AvailabilityConstraint::Reason::UnavailableUnconditionally:
    if (flags.contains(AvailabilityConstraintFlag::
                           AllowUniversallyUnavailableInCompatibleContexts))
      return true;

    // Always reject uses of universally unavailable declarations, regardless
    // of context, since there are no possible compilation configurations in
    // which they are available. However, make an exception for types and
    // conformances, which can sometimes be awkward to avoid references to.
    if (!isa<TypeDecl>(decl) && !isa<ExtensionDecl>(decl)) {
      if (domain.isUniversal() || domain.isSwiftLanguage())
        return false;
    }
    return true;

  case AvailabilityConstraint::Reason::Unintroduced:
    switch (domain.getKind()) {
    case AvailabilityDomain::Kind::Universal:
    case AvailabilityDomain::Kind::SwiftLanguage:
    case AvailabilityDomain::Kind::PackageDescription:
    case AvailabilityDomain::Kind::Embedded:
    case AvailabilityDomain::Kind::Custom:
      return false;
    case AvailabilityDomain::Kind::Platform:
      // Platform availability only applies to the target triple that the
      // binary is being compiled for. Since the same declaration can be
      // potentially unavailable from a given context when compiling for one
      // platform, but available from that context when compiling for a
      // different platform, it is overly strict to enforce potential platform
      // unavailability constraints in contexts that are unavailable to that
      // platform.
      return true;
    }
    return constraint.getDomain().isPlatform();

  case AvailabilityConstraint::Reason::UnavailableObsolete:
  case AvailabilityConstraint::Reason::UnavailableUnintroduced:
    return false;
  }
}

static bool
shouldIgnoreConstraintInContext(const Decl *decl,
                                const AvailabilityConstraint &constraint,
                                const AvailabilityContext &context,
                                const AvailabilityConstraintFlags flags) {
  if (!context.isUnavailable())
    return false;

  if (!canIgnoreConstraintInUnavailableContexts(decl, constraint, flags))
    return false;

  return context.containsUnavailableDomain(constraint.getDomain());
}

/// Returns the `AvailabilityConstraint` that describes how \p attr restricts
/// use of \p decl in \p context or `std::nullopt` if there is no restriction.
static std::optional<AvailabilityConstraint>
getAvailabilityConstraintForAttr(const Decl *decl,
                                 const SemanticAvailableAttr &attr,
                                 const AvailabilityContext &context) {
  // Is the decl unconditionally unavailable?
  if (attr.isUnconditionallyUnavailable())
    return AvailabilityConstraint::unavailableUnconditionally(attr);

  auto &ctx = decl->getASTContext();
  auto domain = attr.getDomain();
  auto deploymentRange = domain.getDeploymentRange(ctx);
  bool domainSupportsRefinement = domain.supportsContextRefinement();
  std::optional<AvailabilityRange> availableRange =
      domainSupportsRefinement ? context.getAvailabilityRange(domain, ctx)
                               : deploymentRange;

  // Is the decl obsoleted in this context?
  if (auto obsoletedRange = attr.getObsoletedRange(ctx)) {
    if (availableRange && availableRange->isContainedIn(*obsoletedRange))
      return AvailabilityConstraint::unavailableObsolete(attr);
  }

  // Is the decl not yet introduced in this context?
  if (auto introducedRange = attr.getIntroducedRange(ctx)) {
    if (!availableRange || !availableRange->isContainedIn(*introducedRange))
      return domainSupportsRefinement
                 ? AvailabilityConstraint::unintroduced(attr)
                 : AvailabilityConstraint::unavailableUnintroduced(attr);
  }

  return std::nullopt;
}

/// Returns the most specific platform domain from the availability attributes
/// attached to \p decl or `std::nullopt` if there are none. Platform specific
/// `@available` attributes for other platforms should be ignored. For example,
/// if a declaration has attributes for both iOS and macCatalyst, only the
/// macCatalyst attributes take effect when compiling for a macCatalyst target.
static std::optional<AvailabilityDomain>
activePlatformDomainForDecl(const Decl *decl) {
  std::optional<AvailabilityDomain> activeDomain;
  for (auto attr :
       decl->getSemanticAvailableAttrs(/*includingInactive=*/false)) {
    auto domain = attr.getDomain();
    if (!domain.isPlatform())
      continue;

    if (activeDomain && domain.contains(*activeDomain))
      continue;

    activeDomain.emplace(domain);
  }

  return activeDomain;
}

static void getAvailabilityConstraintsForDecl(
    llvm::SmallVector<AvailabilityConstraint, 4> &constraints, const Decl *decl,
    const AvailabilityContext &context, AvailabilityConstraintFlags flags) {
  auto &ctx = decl->getASTContext();
  auto activePlatformDomain = activePlatformDomainForDecl(decl);
  bool includeAllDomains =
      flags.contains(AvailabilityConstraintFlag::IncludeAllDomains);

  for (auto attr : decl->getSemanticAvailableAttrs(includeAllDomains)) {
    auto domain = attr.getDomain();
    if (!includeAllDomains && domain.isPlatform() && activePlatformDomain &&
        !activePlatformDomain->contains(domain))
      continue;

    if (auto constraint = getAvailabilityConstraintForAttr(decl, attr, context))
      addConstraint(constraints, *constraint, ctx);
  }

  // After resolving constraints, remove any constraints that indicate the
  // declaration is unconditionally unavailable in a domain for which
  // the context is already unavailable.
  llvm::erase_if(constraints, [&](const AvailabilityConstraint &constraint) {
    return shouldIgnoreConstraintInContext(decl, constraint, context, flags);
  });
}

DeclAvailabilityConstraints
swift::getAvailabilityConstraintsForDecl(const Decl *decl,
                                         const AvailabilityContext &context,
                                         AvailabilityConstraintFlags flags) {
  llvm::SmallVector<AvailabilityConstraint, 4> constraints;

  // Generic parameters are always available.
  if (isa<GenericTypeParamDecl>(decl))
    return DeclAvailabilityConstraints();

  decl = decl->getAbstractSyntaxDeclForAttributes();

  getAvailabilityConstraintsForDecl(constraints, decl, context, flags);

  if (flags.contains(AvailabilityConstraintFlag::SkipEnclosingExtension))
    return constraints;

  // If decl is an extension member, query the attributes of the extension, too.
  //
  // Skip decls imported from Clang, though, as they could be associated to the
  // wrong extension and inherit unavailability incorrectly. ClangImporter
  // associates Objective-C protocol members to the first category where the
  // protocol is directly or indirectly adopted, no matter its availability
  // and the availability of other categories. rdar://problem/53956555
  if (decl->getClangNode())
    return constraints;

  auto parent = decl->parentDeclForAvailability();
  if (auto extension = dyn_cast_or_null<ExtensionDecl>(parent))
    getAvailabilityConstraintsForDecl(constraints, extension, context, flags);

  return constraints;
}

std::optional<AvailabilityConstraint>
swift::getAvailabilityConstraintForDeclInDomain(
    const Decl *decl, const AvailabilityContext &context,
    AvailabilityDomain domain, AvailabilityConstraintFlags flags) {
  auto constraints = getAvailabilityConstraintsForDecl(decl, context, flags);
  for (auto const &constraint : constraints) {
    if (constraint.getDomain().isRelated(domain))
      return constraint;
  }

  return std::nullopt;
}
