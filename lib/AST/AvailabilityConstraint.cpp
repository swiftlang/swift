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
#include "swift/AST/AvailabilityInference.h"
#include "swift/AST/Decl.h"

using namespace swift;

PlatformKind AvailabilityConstraint::getPlatform() const {
  return getAttr().getPlatform();
}

std::optional<AvailabilityRange>
AvailabilityConstraint::getRequiredNewerAvailabilityRange(
    const ASTContext &ctx) const {
  switch (getReason()) {
  case Reason::UnconditionallyUnavailable:
  case Reason::Obsoleted:
  case Reason::IntroducedInLaterVersion:
    return std::nullopt;
  case Reason::IntroducedInLaterDynamicVersion:
    return getAttr().getIntroducedRange(ctx);
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

static bool constraintIsStronger(const AvailabilityConstraint &lhs,
                                 const AvailabilityConstraint &rhs) {
  DEBUG_ASSERT(lhs.getDomain() == rhs.getDomain());

  // If the constraints have matching domains but different reasons, the
  // constraint with the lowest reason is "strongest".
  if (lhs.getReason() != rhs.getReason())
    return lhs.getReason() < rhs.getReason();

  switch (lhs.getReason()) {
  case AvailabilityConstraint::Reason::UnconditionallyUnavailable:
    // Just keep the first.
    return false;

  case AvailabilityConstraint::Reason::Obsoleted:
    // Pick the earliest obsoleted version.
    return *lhs.getAttr().getObsoleted() < *rhs.getAttr().getObsoleted();

  case AvailabilityConstraint::Reason::IntroducedInLaterVersion:
  case AvailabilityConstraint::Reason::IntroducedInLaterDynamicVersion:
    // Pick the latest introduced version.
    return *lhs.getAttr().getIntroduced() > *rhs.getAttr().getIntroduced();
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

static bool
isInsideCompatibleUnavailableDeclaration(const Decl *decl,
                                         const SemanticAvailableAttr &attr,
                                         const AvailabilityContext &context) {
  if (!context.isUnavailable())
    return false;

  if (!attr.isUnconditionallyUnavailable())
    return false;

  // Refuse calling universally unavailable functions from unavailable code,
  // but allow the use of types.
  auto domain = attr.getDomain();
  if (!isa<TypeDecl>(decl) && !isa<ExtensionDecl>(decl)) {
    if (domain.isUniversal() || domain.isSwiftLanguage())
      return false;
  }

  return context.containsUnavailableDomain(domain);
}

/// Returns the `AvailabilityConstraint` that describes how \p attr restricts
/// use of \p decl in \p context or `std::nullopt` if there is no restriction.
static std::optional<AvailabilityConstraint>
getAvailabilityConstraintForAttr(const Decl *decl,
                                 const SemanticAvailableAttr &attr,
                                 const AvailabilityContext &context) {
  if (attr.isUnconditionallyUnavailable())
    return AvailabilityConstraint::unconditionallyUnavailable(attr);

  auto &ctx = decl->getASTContext();
  auto deploymentVersion = attr.getActiveVersion(ctx);
  auto deploymentRange =
      AvailabilityRange(VersionRange::allGTE(deploymentVersion));
  std::optional<llvm::VersionTuple> obsoletedVersion = attr.getObsoleted();

  {
    StringRef obsoletedPlatform;
    llvm::VersionTuple remappedObsoletedVersion;
    if (AvailabilityInference::updateObsoletedPlatformForFallback(
            attr, ctx, obsoletedPlatform, remappedObsoletedVersion))
      obsoletedVersion = remappedObsoletedVersion;
  }

  if (obsoletedVersion && *obsoletedVersion <= deploymentVersion)
    return AvailabilityConstraint::obsoleted(attr);

  AvailabilityRange introducedRange = attr.getIntroducedRange(ctx);

  // FIXME: [availability] Expand this to cover custom versioned domains
  if (attr.isPlatformSpecific()) {
    if (!context.getPlatformRange().isContainedIn(introducedRange))
      return AvailabilityConstraint::introducedInLaterDynamicVersion(attr);
  } else if (!deploymentRange.isContainedIn(introducedRange)) {
    return AvailabilityConstraint::introducedInLaterVersion(attr);
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
    const AvailabilityContext &context) {
  auto &ctx = decl->getASTContext();
  auto activePlatformDomain = activePlatformDomainForDecl(decl);

  for (auto attr :
       decl->getSemanticAvailableAttrs(/*includingInactive=*/false)) {
    auto domain = attr.getDomain();
    if (domain.isPlatform() && activePlatformDomain &&
        !activePlatformDomain->contains(domain))
      continue;

    if (auto constraint = getAvailabilityConstraintForAttr(decl, attr, context))
      addConstraint(constraints, *constraint, ctx);
  }

  // After resolving constraints, remove any constraints that indicate the
  // declaration is unconditionally unavailable in a domain for which
  // the context is already unavailable.
  llvm::erase_if(constraints, [&](const AvailabilityConstraint &constraint) {
    return isInsideCompatibleUnavailableDeclaration(decl, constraint.getAttr(),
                                                    context);
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

  decl = abstractSyntaxDeclForAvailableAttribute(decl);

  getAvailabilityConstraintsForDecl(constraints, decl, context);

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

  auto parent = AvailabilityInference::parentDeclForInferredAvailability(decl);
  if (auto extension = dyn_cast_or_null<ExtensionDecl>(parent))
    getAvailabilityConstraintsForDecl(constraints, extension, context);

  return constraints;
}
