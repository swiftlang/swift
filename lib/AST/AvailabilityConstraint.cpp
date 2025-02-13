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
    ASTContext &ctx) const {
  switch (getReason()) {
  case Reason::UnconditionallyUnavailable:
  case Reason::Obsoleted:
  case Reason::IntroducedInLaterVersion:
    return std::nullopt;
  case Reason::IntroducedInLaterDynamicVersion:
    return getAttr().getIntroducedRange(ctx);
  }
}

bool AvailabilityConstraint::isActiveForRuntimeQueries(ASTContext &ctx) const {
  if (getAttr().getPlatform() == PlatformKind::none)
    return true;

  return swift::isPlatformActive(getAttr().getPlatform(), ctx.LangOpts,
                                 /*forTargetVariant=*/false,
                                 /*forRuntimeQuery=*/true);
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

std::optional<AvailabilityConstraint>
swift::getAvailabilityConstraintForAttr(const Decl *decl,
                                        const SemanticAvailableAttr &attr,
                                        const AvailabilityContext &context) {
  if (isInsideCompatibleUnavailableDeclaration(decl, attr, context))
    return std::nullopt;

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

static void
getAvailabilityConstraintsForDecl(DeclAvailabilityConstraints &constraints,
                                  const Decl *decl,
                                  const AvailabilityContext &context) {
  auto activePlatformDomain = activePlatformDomainForDecl(decl);

  for (auto attr :
       decl->getSemanticAvailableAttrs(/*includingInactive=*/false)) {
    auto domain = attr.getDomain();
    if (domain.isPlatform() && activePlatformDomain &&
        !activePlatformDomain->contains(domain))
      continue;

    if (auto constraint =
            swift::getAvailabilityConstraintForAttr(decl, attr, context))
      constraints.addConstraint(*constraint);
  }
}

DeclAvailabilityConstraints
swift::getAvailabilityConstraintsForDecl(const Decl *decl,
                                         const AvailabilityContext &context) {
  DeclAvailabilityConstraints constraints;

  // Generic parameters are always available.
  if (isa<GenericTypeParamDecl>(decl))
    return constraints;

  decl = abstractSyntaxDeclForAvailableAttribute(decl);

  getAvailabilityConstraintsForDecl(constraints, decl, context);

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
