//===--- Availability.cpp - Swift Availability Structures -----------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines data structures for API availability.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/ASTContext.h"
#include "swift/AST/Attr.h"
#include "swift/AST/AvailabilityConstraint.h"
#include "swift/AST/AvailabilityContext.h"
#include "swift/AST/AvailabilityDomain.h"
#include "swift/AST/AvailabilityInference.h"
#include "swift/AST/AvailabilityRange.h"
#include "swift/AST/Decl.h"
#include "swift/AST/DeclExportabilityVisitor.h"
// FIXME: [availability] Remove this when possible
#include "swift/AST/DiagnosticsParse.h"
#include "swift/AST/DiagnosticsSema.h"
#include "swift/AST/PlatformKindUtils.h"
#include "swift/AST/TypeCheckRequests.h"
#include "swift/AST/TypeWalker.h"
#include "swift/AST/Types.h"
#include "swift/Basic/Assertions.h"
#include "swift/Basic/Platform.h"
#include "swift/ClangImporter/ClangModule.h"
#include <map>

using namespace swift;

void VersionRange::Profile(llvm::FoldingSetNodeID &id) const {
  id.AddBoolean(hasLowerEndpoint());
  if (!hasLowerEndpoint()) {
    id.AddBoolean(isAll());
    return;
  }

  auto profileVersionComponent = [&id](std::optional<unsigned> component) {
    id.AddBoolean(component.has_value());
    if (component)
      id.AddInteger(*component);
  };

  auto lowerEndpoint = getLowerEndpoint();
  id.AddInteger(lowerEndpoint.getMajor());
  profileVersionComponent(lowerEndpoint.getMinor());
  profileVersionComponent(lowerEndpoint.getSubminor());
  profileVersionComponent(lowerEndpoint.getBuild());
}

AvailabilityRange
AvailabilityRange::forDeploymentTarget(const ASTContext &Ctx) {
  return AvailabilityRange(Ctx.LangOpts.getMinPlatformVersion());
}

AvailabilityRange AvailabilityRange::forInliningTarget(const ASTContext &Ctx) {
  return AvailabilityRange(Ctx.LangOpts.MinimumInliningTargetVersion);
}

AvailabilityRange AvailabilityRange::forRuntimeTarget(const ASTContext &Ctx) {
  return AvailabilityRange(Ctx.LangOpts.RuntimeVersion);
}

namespace {

/// The inferred availability required to access a group of declarations
/// on a single platform.
struct InferredAvailability {
  AvailableAttr::Kind Kind = AvailableAttr::Kind::Default;

  std::optional<llvm::VersionTuple> Introduced;
  std::optional<llvm::VersionTuple> Deprecated;
  std::optional<llvm::VersionTuple> Obsoleted;

  StringRef Message;
  StringRef Rename;
  bool IsSPI = false;
};

/// The type of a function that merges two version tuples.
typedef const llvm::VersionTuple &(*MergeFunction)(
    const llvm::VersionTuple &, const llvm::VersionTuple &);

} // end anonymous namespace

/// Apply a merge function to two optional versions, returning the result
/// in Inferred.
static bool
mergeIntoInferredVersion(const std::optional<llvm::VersionTuple> &Version,
                         std::optional<llvm::VersionTuple> &Inferred,
                         MergeFunction Merge) {
  if (Version.has_value()) {
    if (Inferred.has_value()) {
      Inferred = Merge(Inferred.value(), Version.value());
      return *Inferred == *Version;
    } else {
      Inferred = Version;
      return true;
    }
  }
  return false;
}

/// Merge an attribute's availability with an existing inferred availability
/// so that the new inferred availability is at least as available as
/// the attribute requires.
static void mergeWithInferredAvailability(SemanticAvailableAttr Attr,
                                          InferredAvailability &Inferred) {
  auto *ParsedAttr = Attr.getParsedAttr();
  Inferred.Kind = static_cast<AvailableAttr::Kind>(
      std::max(static_cast<unsigned>(Inferred.Kind),
               static_cast<unsigned>(ParsedAttr->getKind())));

  // The merge of two introduction versions is the maximum of the two versions.
  if (mergeIntoInferredVersion(Attr.getIntroduced(), Inferred.Introduced,
                               std::max)) {
    Inferred.IsSPI = Attr.isSPI();
  }

  // The merge of deprecated and obsoleted versions takes the minimum.
  mergeIntoInferredVersion(Attr.getDeprecated(), Inferred.Deprecated, std::min);
  mergeIntoInferredVersion(Attr.getObsoleted(), Inferred.Obsoleted, std::min);

  if (Inferred.Message.empty() && !Attr.getMessage().empty())
    Inferred.Message = Attr.getMessage();

  if (Inferred.Rename.empty() && !Attr.getRename().empty())
    Inferred.Rename = Attr.getRename();
}

/// Create an implicit availability attribute for the given domain
/// and with the inferred availability.
static AvailableAttr *createAvailableAttr(AvailabilityDomain Domain,
                                          const InferredAvailability &Inferred,
                                          ASTContext &Context) {
  // If there is no information that would go into the availability attribute,
  // don't create one.
  if (Inferred.Kind == AvailableAttr::Kind::Default && !Inferred.Introduced &&
      !Inferred.Deprecated && !Inferred.Obsoleted && Inferred.Message.empty() &&
      Inferred.Rename.empty())
    return nullptr;

  llvm::VersionTuple Introduced =
      Inferred.Introduced.value_or(llvm::VersionTuple());
  llvm::VersionTuple Deprecated =
      Inferred.Deprecated.value_or(llvm::VersionTuple());
  llvm::VersionTuple Obsoleted =
      Inferred.Obsoleted.value_or(llvm::VersionTuple());

  return new (Context) AvailableAttr(
      SourceLoc(), SourceRange(), Domain, SourceLoc(), Inferred.Kind,
      Inferred.Message, Inferred.Rename, Introduced, SourceRange(), Deprecated,
      SourceRange(), Obsoleted, SourceRange(), /*Implicit=*/true,
      Inferred.IsSPI);
}

void AvailabilityInference::applyInferredAvailableAttrs(
    Decl *ToDecl, ArrayRef<const Decl *> InferredFromDecls) {
  auto &Context = ToDecl->getASTContext();

  // Iterate over the declarations and infer required availability on
  // a per-domain basis.
  std::map<AvailabilityDomain, InferredAvailability,
           StableAvailabilityDomainComparator>
      Inferred;
  for (const Decl *D : InferredFromDecls) {
    llvm::SmallVector<SemanticAvailableAttr, 8> MergedAttrs;

    do {
      llvm::SmallVector<SemanticAvailableAttr, 8> PendingAttrs;

      for (auto AvAttr : D->getSemanticAvailableAttrs()) {
        // Skip an attribute from an outer declaration if it is for a platform
        // that was already handled implicitly by an attribute from an inner
        // declaration.
        if (llvm::any_of(MergedAttrs,
                         [&AvAttr](SemanticAvailableAttr MergedAttr) {
                           return inheritsAvailabilityFromPlatform(
                               AvAttr.getPlatform(), MergedAttr.getPlatform());
                         }))
          continue;

        mergeWithInferredAvailability(AvAttr, Inferred[AvAttr.getDomain()]);
        PendingAttrs.push_back(AvAttr);
      }

      MergedAttrs.append(PendingAttrs);

      // Walk up the enclosing declaration hierarchy to make sure we aren't
      // missing any inherited attributes.
      D = D->parentDeclForAvailability();
    } while (D);
  }

  DeclAttributes &Attrs = ToDecl->getAttrs();

  // Create an availability attribute for each observed platform and add
  // to ToDecl.
  for (auto &Pair : Inferred) {
    if (auto Attr = createAvailableAttr(Pair.first, Pair.second, Context))
      Attrs.add(Attr);
  }
}

/// Returns the decl that should be considered the parent decl of the given decl
/// when looking for inherited availability annotations.
const Decl *Decl::parentDeclForAvailability() const {
  if (auto *AD = dyn_cast<AccessorDecl>(this))
    return AD->getStorage();

  if (auto *ED = dyn_cast<ExtensionDecl>(this)) {
    if (auto *NTD = ED->getExtendedNominal())
      return NTD;
  }

  if (auto *PBD = dyn_cast<PatternBindingDecl>(this)) {
    if (PBD->getNumPatternEntries() < 1)
      return nullptr;

    return PBD->getAnchoringVarDecl(0);
  }

  if (auto *OTD = dyn_cast<OpaqueTypeDecl>(this))
    return OTD->getNamingDecl();

  // Clang decls may be inaccurately parented rdar://53956555
  if (hasClangNode())
    return nullptr;

  // Availability is inherited from the enclosing context.
  return getDeclContext()->getInnermostDeclarationDeclContext();
}

/// Returns true if the introduced version in \p newAttr should be used instead
/// of the introduced version in \p prevAttr when both are attached to the same
/// declaration and refer to the active platform.
static bool isBetterThan(const SemanticAvailableAttr &newAttr,
                         const std::optional<SemanticAvailableAttr> &prevAttr) {
  // If there is no prevAttr, newAttr of course wins.
  if (!prevAttr)
    return true;

  // If they belong to the same platform, the one that introduces later wins.
  if (prevAttr->getPlatform() == newAttr.getPlatform())
    return prevAttr->getIntroduced().value() < newAttr.getIntroduced().value();

  // If the new attribute's platform inherits from the old one, it wins.
  return inheritsAvailabilityFromPlatform(newAttr.getPlatform(),
                                          prevAttr->getPlatform());
}

static const clang::DarwinSDKInfo::RelatedTargetVersionMapping *
getFallbackVersionMapping(const ASTContext &Ctx,
                          clang::DarwinSDKInfo::OSEnvPair Kind) {
  auto *SDKInfo = Ctx.getDarwinSDKInfo();
  if (SDKInfo)
    return SDKInfo->getVersionMapping(Kind);

  return Ctx.getAuxiliaryDarwinPlatformRemapInfo(Kind);
}

static std::optional<clang::VersionTuple>
getRemappedIntroducedVersionForFallbackPlatform(
    const ASTContext &Ctx, const llvm::VersionTuple &Version) {
  const auto *Mapping = getFallbackVersionMapping(
      Ctx, clang::DarwinSDKInfo::OSEnvPair(
               llvm::Triple::IOS, llvm::Triple::UnknownEnvironment,
               llvm::Triple::XROS, llvm::Triple::UnknownEnvironment));
  if (!Mapping)
    return std::nullopt;
  return Mapping->mapIntroducedAvailabilityVersion(Version);
}

static std::optional<clang::VersionTuple>
getRemappedDeprecatedObsoletedVersionForFallbackPlatform(
    const ASTContext &Ctx, const llvm::VersionTuple &Version) {
  const auto *Mapping = getFallbackVersionMapping(
      Ctx, clang::DarwinSDKInfo::OSEnvPair(
               llvm::Triple::IOS, llvm::Triple::UnknownEnvironment,
               llvm::Triple::XROS, llvm::Triple::UnknownEnvironment));
  if (!Mapping)
    return std::nullopt;
  return Mapping->mapDeprecatedObsoletedAvailabilityVersion(Version);
}

bool AvailabilityInference::updateIntroducedAvailabilityDomainForFallback(
    const SemanticAvailableAttr &attr, const ASTContext &ctx,
    AvailabilityDomain &domain, llvm::VersionTuple &platformVer) {
  std::optional<llvm::VersionTuple> introducedVersion = attr.getIntroduced();
  if (!introducedVersion.has_value())
    return false;

  bool hasRemap = false;
  auto remappedDomain = attr.getDomain().getRemappedDomain(ctx, hasRemap);
  if (!hasRemap)
    return false;

  auto potentiallyRemappedIntroducedVersion =
      getRemappedIntroducedVersionForFallbackPlatform(ctx, *introducedVersion);
  if (potentiallyRemappedIntroducedVersion.has_value()) {
    domain = remappedDomain;
    platformVer = potentiallyRemappedIntroducedVersion.value();
    return true;
  }
  return false;
}

bool AvailabilityInference::updateDeprecatedAvailabilityDomainForFallback(
    const SemanticAvailableAttr &attr, const ASTContext &ctx,
    AvailabilityDomain &domain, llvm::VersionTuple &platformVer) {
  std::optional<llvm::VersionTuple> deprecatedVersion = attr.getDeprecated();
  if (!deprecatedVersion.has_value())
    return false;

  bool hasRemap = false;
  auto remappedDomain = attr.getDomain().getRemappedDomain(ctx, hasRemap);
  if (!hasRemap)
    return false;

  auto potentiallyRemappedDeprecatedVersion =
      getRemappedDeprecatedObsoletedVersionForFallbackPlatform(
          ctx, *deprecatedVersion);
  if (potentiallyRemappedDeprecatedVersion.has_value()) {
    domain = remappedDomain;
    platformVer = potentiallyRemappedDeprecatedVersion.value();
    return true;
  }
  return false;
}

bool AvailabilityInference::updateObsoletedAvailabilityDomainForFallback(
    const SemanticAvailableAttr &attr, const ASTContext &ctx,
    AvailabilityDomain &domain, llvm::VersionTuple &platformVer) {
  std::optional<llvm::VersionTuple> obsoletedVersion = attr.getObsoleted();
  if (!obsoletedVersion.has_value())
    return false;

  bool hasRemap = false;
  auto remappedDomain = attr.getDomain().getRemappedDomain(ctx, hasRemap);
  if (!hasRemap)
    return false;

  auto potentiallyRemappedObsoletedVersion =
      getRemappedDeprecatedObsoletedVersionForFallbackPlatform(
          ctx, *obsoletedVersion);
  if (potentiallyRemappedObsoletedVersion.has_value()) {
    domain = remappedDomain;
    platformVer = potentiallyRemappedObsoletedVersion.value();
    return true;
  }
  return false;
}

bool AvailabilityInference::updateBeforeAvailabilityDomainForFallback(
    const BackDeployedAttr *attr, const ASTContext &ctx,
    AvailabilityDomain &domain, llvm::VersionTuple &platformVer) {
  bool hasRemap = false;
  auto remappedDomain =
      attr->getAvailabilityDomain().getRemappedDomain(ctx, hasRemap);
  if (!hasRemap)
    return false;

  auto beforeVersion = attr->getVersion();
  auto potentiallyRemappedIntroducedVersion =
      getRemappedIntroducedVersionForFallbackPlatform(ctx, beforeVersion);
  if (potentiallyRemappedIntroducedVersion.has_value()) {
    domain = remappedDomain;
    platformVer = potentiallyRemappedIntroducedVersion.value();
    return true;
  }
  return false;
}

static std::optional<SemanticAvailableAttr>
getDeclAvailableAttrForPlatformIntroduction(const Decl *D) {
  std::optional<SemanticAvailableAttr> bestAvailAttr;

  D = D->getAbstractSyntaxDeclForAttributes();

  for (auto attr : D->getSemanticAvailableAttrs(/*includingInactive=*/false)) {
    if (!attr.isPlatformSpecific() || !attr.getIntroduced())
      continue;

    if (isBetterThan(attr, bestAvailAttr))
      bestAvailAttr.emplace(attr);
  }

  return bestAvailAttr;
}

std::optional<AvailabilityRange>
AvailabilityInference::annotatedAvailableRange(const Decl *D) {
  auto bestAvailAttr = D->getAvailableAttrForPlatformIntroduction();
  if (!bestAvailAttr)
    return std::nullopt;

  return bestAvailAttr->getIntroducedRange(D->getASTContext());
}

bool Decl::isAvailableAsSPI() const {
  if (auto attr = getAvailableAttrForPlatformIntroduction())
    return attr->isSPI();

  return false;
}

SemanticAvailableAttributes
Decl::getSemanticAvailableAttrs(bool includeInactive) const {
  // A decl in an @abi gets its availability from the decl it's attached to.
  auto abiRole = ABIRoleInfo(this);
  if (!abiRole.providesAPI() && abiRole.getCounterpart())
    return abiRole.getCounterpart()->getSemanticAvailableAttrs(includeInactive);

  return SemanticAvailableAttributes(getAttrs(), this, includeInactive);
}

std::optional<SemanticAvailableAttr>
Decl::getSemanticAvailableAttr(const AvailableAttr *attr) const {
  return evaluateOrDefault(getASTContext().evaluator,
                           SemanticAvailableAttrRequest{attr, this},
                           std::nullopt);
}

std::optional<SemanticAvailableAttr>
Decl::getActiveAvailableAttrForCurrentPlatform() const {
  std::optional<SemanticAvailableAttr> bestAttr;

  for (auto attr : getSemanticAvailableAttrs(/*includingInactive=*/false)) {
    if (!attr.isPlatformSpecific())
      continue;

    // We have an attribute that is active for the platform, but is it more
    // specific than our current best?
    if (!bestAttr || inheritsAvailabilityFromPlatform(
                         attr.getPlatform(), bestAttr->getPlatform())) {
      bestAttr.emplace(attr);
    }
  }

  return bestAttr;
}

std::optional<SemanticAvailableAttr> Decl::getDeprecatedAttr() const {
  auto &ctx = getASTContext();
  std::optional<SemanticAvailableAttr> result;
  auto bestActive = getActiveAvailableAttrForCurrentPlatform();

  for (auto attr : getSemanticAvailableAttrs(/*includingInactive=*/false)) {
    if (attr.isPlatformSpecific() && (!bestActive || attr != bestActive))
      continue;

    // Unconditional deprecated.
    if (attr.isUnconditionallyDeprecated())
      return attr;

    auto deprecatedRange = attr.getDeprecatedRange(ctx);
    if (!deprecatedRange)
      continue;

    // We treat the declaration as deprecated if it is deprecated on
    // all deployment targets.
    auto deploymentRange = attr.getDomain().getDeploymentRange(ctx);
    if (deploymentRange && deploymentRange->isContainedIn(*deprecatedRange))
      result.emplace(attr);
  }
  return result;
}

std::optional<SemanticAvailableAttr> Decl::getSoftDeprecatedAttr() const {
  auto &ctx = getASTContext();
  std::optional<SemanticAvailableAttr> result;
  auto bestActive = getActiveAvailableAttrForCurrentPlatform();

  for (auto attr : getSemanticAvailableAttrs(/*includingInactive=*/false)) {
    if (attr.isPlatformSpecific() && (!bestActive || attr != bestActive))
      continue;

    auto deprecatedRange = attr.getDeprecatedRange(ctx);
    if (!deprecatedRange)
      continue;

    // We treat the declaration as soft-deprecated if it is deprecated in a
    // future version.
    auto deploymentRange = attr.getDomain().getDeploymentRange(ctx);
    if (!deploymentRange || !deploymentRange->isContainedIn(*deprecatedRange))
      result.emplace(attr);
  }
  return result;
}

std::optional<SemanticAvailableAttr> Decl::getNoAsyncAttr() const {
  std::optional<SemanticAvailableAttr> bestAttr;

  for (auto attr : getSemanticAvailableAttrs(/*includingInactive=*/false)) {
    if (!attr.isNoAsync())
      continue;

    if (!bestAttr) {
      // If there is no best attr selected and the attr either has an active
      // platform, or doesn't have one at all, select it.
      bestAttr.emplace(attr);
    } else if (bestAttr && attr.isPlatformSpecific() &&
               bestAttr->isPlatformSpecific() &&
               inheritsAvailabilityFromPlatform(attr.getPlatform(),
                                                bestAttr->getPlatform())) {
      // if they both have a viable platform, use the better one
      bestAttr.emplace(attr);
    } else if (attr.isPlatformSpecific() && !bestAttr->isPlatformSpecific()) {
      // Use the one more specific
      bestAttr.emplace(attr);
    }
  }
  return bestAttr;
}

bool Decl::isUnavailableInCurrentSwiftVersion() const {
  llvm::VersionTuple vers = getASTContext().LangOpts.EffectiveLanguageVersion;
  for (auto attr : getSemanticAvailableAttrs(/*includingInactive=*/false)) {
    if (attr.isSwiftLanguageModeSpecific()) {
      auto introduced = attr.getIntroduced();
      if (introduced && *introduced > vers)
        return true;

      auto obsoleted = attr.getObsoleted();
      if (obsoleted && *obsoleted <= vers)
        return true;
    }
  }

  return false;
}

std::optional<SemanticAvailableAttr> Decl::getUnavailableAttr() const {
  auto context = AvailabilityContext::forDeploymentTarget(getASTContext());
  if (auto constraint = getAvailabilityConstraintsForDecl(this, context)
                            .getPrimaryConstraint()) {
    if (constraint->isUnavailable())
      return constraint->getAttr();
  }

  return std::nullopt;
}

/// Returns the mutually exclusive root platform domains that must all be
/// unavailable in order for a declaration to be unavailable at runtime.
static llvm::SmallSetVector<AvailabilityDomain, 2>
getRootTargetDomains(const ASTContext &ctx) {
  llvm::SmallSetVector<AvailabilityDomain, 2> domains;

  // Regardless of target platform, binaries built for Embedded do not require
  // compatibility.
  if (ctx.LangOpts.hasFeature(Feature::Embedded))
    return domains;

  auto targetPlatform = swift::targetPlatform(ctx.LangOpts);
  if (targetPlatform != PlatformKind::none)
    domains.insert(
        AvailabilityDomain::forPlatform(targetPlatform).getRootDomain());

  auto targetVariantPlatform = swift::targetVariantPlatform(ctx.LangOpts);
  if (targetVariantPlatform != PlatformKind::none)
    domains.insert(
        AvailabilityDomain::forPlatform(targetVariantPlatform).getRootDomain());

  return domains;
}

static bool constraintIndicatesRuntimeUnavailability(
    const AvailabilityConstraint &constraint, const ASTContext &ctx) {
  std::optional<CustomAvailabilityDomain::Kind> customDomainKind;
  if (auto customDomain = constraint.getDomain().getCustomDomain())
    customDomainKind = customDomain->getKind();

  switch (constraint.getReason()) {
  case AvailabilityConstraint::Reason::UnconditionallyUnavailable:
    if (customDomainKind)
      return customDomainKind == CustomAvailabilityDomain::Kind::Enabled;
    return true;
  case AvailabilityConstraint::Reason::Obsoleted:
  case AvailabilityConstraint::Reason::UnavailableForDeployment:
    return false;
  case AvailabilityConstraint::Reason::PotentiallyUnavailable:
    if (customDomainKind)
      return customDomainKind == CustomAvailabilityDomain::Kind::Disabled;
    return false;
  }
}

/// Returns true if a decl that is unavailable in the given domain must still be
/// emitted to preserve load time ABI compatibility.
static bool
domainRequiresABICompatibleUnavailableDecls(AvailabilityDomain domain,
                                            const ASTContext &ctx) {
  // FIXME: [availability] Restrict ABI compatible unavailable decls to modules
  // compiled with macOS, iOS, watchOS, tvOS, or visionOS target triples. For
  // other targets, unavailable code should always be stripped from binaries.
  return domain.isUniversal() || domain.isPlatform();
}

/// Computes the `DeclRuntimeAvailability` value for `decl` in isolation.
static DeclRuntimeAvailability
computeDeclRuntimeAvailability(const Decl *decl) {
  // Don't trust unavailability on declarations from Clang modules.
  if (isa<ClangModuleUnit>(decl->getDeclContext()->getModuleScopeContext()))
    return DeclRuntimeAvailability::PotentiallyAvailable;

  auto &ctx = decl->getASTContext();
  auto rootTargetDomains = getRootTargetDomains(ctx);
  auto remainingTargetDomains = rootTargetDomains;

  AvailabilityConstraintFlags flags;

  // Semantic availability was already computed separately for any enclosing
  // extension.
  flags |= AvailabilityConstraintFlag::SkipEnclosingExtension;

  // FIXME: [availability] Replace IncludeAllDomains with a RuntimeAvailability
  // flag that includes the target variant constraints and keeps all constraints
  // from active platforms.
  flags |= AvailabilityConstraintFlag::IncludeAllDomains;

  auto constraints = getAvailabilityConstraintsForDecl(
      decl, AvailabilityContext::forInliningTarget(ctx), flags);

  // First, collect the unavailable domains from the constraints.
  llvm::SmallVector<AvailabilityDomain, 8> unavailableDomains;
  for (auto constraint : constraints) {
    if (constraintIndicatesRuntimeUnavailability(constraint, ctx))
      unavailableDomains.push_back(constraint.getDomain());
  }

  // Check whether there are any available attributes that would make the
  // decl available in descendants of the unavailable domains.
  for (auto attr :
       decl->getSemanticAvailableAttrs(/*includingInactive=*/false)) {
    auto domain = attr.getDomain();
    if (llvm::is_contained(unavailableDomains, domain))
      continue;

    llvm::erase_if(unavailableDomains, [domain](auto unavailableDomain) {
      return unavailableDomain.contains(domain);
    });
  }

  // Check the remaining unavailable domains to see if the requirements for
  // runtime unreachability are met.
  auto result = DeclRuntimeAvailability::PotentiallyAvailable;
  for (auto domain : unavailableDomains) {
    // Check whether the constraint is from a relevant domain.
    bool isTargetDomain = rootTargetDomains.contains(domain);
    if (!domain.isActive(ctx) && !isTargetDomain)
      continue;

    if (!domain.isRoot())
      continue;

    // We've found an unavailable target domain. If all the target domains are
    // unavailable then the decl is unreachable at runtime.
    if (isTargetDomain) {
      remainingTargetDomains.remove(domain);
      if (remainingTargetDomains.empty())
        result = DeclRuntimeAvailability::AlwaysUnavailableABICompatible;

      continue;
    }

    // We've found a single unavailable domain that alone proves the decl is
    // unreachable at runtime. It may still be required at load time, though.
    if (domainRequiresABICompatibleUnavailableDecls(domain, ctx)) {
      result = DeclRuntimeAvailability::AlwaysUnavailableABICompatible;
      continue;
    }

    return DeclRuntimeAvailability::AlwaysUnavailable;
  }

  return result;
}

/// Determines the `DeclRuntimeAvailability` value for `decl` via
/// `DeclRuntimeAvailabilityRequest`.
static DeclRuntimeAvailability getDeclRuntimeAvailability(const Decl *decl) {
  return evaluateOrDefault(decl->getASTContext().evaluator,
                           DeclRuntimeAvailabilityRequest{decl},
                           DeclRuntimeAvailability::PotentiallyAvailable);
}

DeclRuntimeAvailability
DeclRuntimeAvailabilityRequest::evaluate(Evaluator &evaluator,
                                         const Decl *decl) const {
  auto inherited = DeclRuntimeAvailability::PotentiallyAvailable;
  if (auto *parent = decl->parentDeclForAvailability())
    inherited = getDeclRuntimeAvailability(parent);

  // If the inherited runtime availability is already maximally unavailable
  // then skip computing unavailability for this declaration.
  if (inherited == DeclRuntimeAvailability::AlwaysUnavailable)
    return DeclRuntimeAvailability::AlwaysUnavailable;

  auto availability = computeDeclRuntimeAvailability(decl);
  return std::max(inherited, availability);
}

bool Decl::isUnreachableAtRuntime() const {
  return getDeclRuntimeAvailability(this) >=
         DeclRuntimeAvailability::AlwaysUnavailableABICompatible;
}

static UnavailableDeclOptimization
getEffectiveUnavailableDeclOptimization(ASTContext &ctx) {
  if (ctx.LangOpts.UnavailableDeclOptimizationMode.has_value())
    return *ctx.LangOpts.UnavailableDeclOptimizationMode;

  return UnavailableDeclOptimization::None;
}

bool Decl::isAvailableDuringLowering() const {
  auto availability = getDeclRuntimeAvailability(this);

  if (getEffectiveUnavailableDeclOptimization(getASTContext()) !=
      UnavailableDeclOptimization::Complete)
    return availability < DeclRuntimeAvailability::AlwaysUnavailable;

  // All unreachable declarations should be skipped during lowering
  // when -unavailable-decl-optimization=complete is specified.
  return availability < DeclRuntimeAvailability::AlwaysUnavailableABICompatible;
}

bool Decl::requiresUnavailableDeclABICompatibilityStubs() const {
  // Code associated with unavailable declarations should trap at runtime if
  // -unavailable-decl-optimization=stub is specified.
  if (getEffectiveUnavailableDeclOptimization(getASTContext()) !=
      UnavailableDeclOptimization::Stub)
    return false;

  return isUnreachableAtRuntime();
}

AvailabilityRange AvailabilityInference::annotatedAvailableRangeForAttr(
    const Decl *D, const AbstractSpecializeAttr *attr, ASTContext &ctx) {
  std::optional<SemanticAvailableAttr> bestAvailAttr;

  for (auto *availAttr : attr->getAvailableAttrs()) {
    auto semanticAttr = D->getSemanticAvailableAttr(availAttr);
    if (!semanticAttr)
      continue;

    if (!semanticAttr->getIntroduced() || !semanticAttr->isActive(ctx) ||
        !semanticAttr->isPlatformSpecific()) {
      continue;
    }

    if (isBetterThan(*semanticAttr, bestAvailAttr))
      bestAvailAttr.emplace(*semanticAttr);
  }

  if (bestAvailAttr)
    return bestAvailAttr->getIntroducedRange(ctx).value_or(
        AvailabilityRange::alwaysAvailable());

  return AvailabilityRange::alwaysAvailable();
}

std::optional<SemanticAvailableAttr>
Decl::getAvailableAttrForPlatformIntroduction(bool checkExtension) const {
  if (auto attr = getDeclAvailableAttrForPlatformIntroduction(this))
    return attr;

  // Unlike other declarations, extensions can be used without referring to them
  // by name (they don't have one) in the source. For this reason, when checking
  // the available range of a declaration we also need to check to see if it is
  // immediately contained in an extension and use the extension's availability
  // if the declaration does not have an explicit @available attribute
  // itself. This check relies on the fact that we cannot have nested
  // extensions.
  if (!checkExtension)
    return std::nullopt;

  if (auto parent = parentDeclForAvailability()) {
    if (auto *ED = dyn_cast<ExtensionDecl>(parent)) {
      if (auto attr = getDeclAvailableAttrForPlatformIntroduction(ED))
        return attr;
    }
  }

  return std::nullopt;
}

AvailabilityRange AvailabilityInference::availableRange(const Decl *D) {
  if (auto attr = D->getAvailableAttrForPlatformIntroduction())
    return attr->getIntroducedRange(D->getASTContext())
        .value_or(AvailabilityRange::alwaysAvailable());

  return AvailabilityRange::alwaysAvailable();
}

std::optional<SemanticAvailableAttr>
SemanticAvailableAttrRequest::evaluate(swift::Evaluator &evaluator,
                                       const AvailableAttr *attr,
                                       const Decl *decl) const {
  if (attr->getDomainOrIdentifier().isDomain())
    return SemanticAvailableAttr(attr);

  auto &ctx = decl->getASTContext();
  auto &diags = ctx.Diags;
  auto attrLoc = attr->getLocation();
  auto domainLoc = attr->getDomainLoc();
  auto declContext = decl->getInnermostDeclContext();
  auto mutableAttr = const_cast<AvailableAttr *>(attr);
  auto domain = mutableAttr->DomainOrIdentifier.resolveInDeclContext(
      domainLoc, declContext);

  if (!domain)
    return std::nullopt;

  auto checkVersion = [&](std::optional<llvm::VersionTuple> version,
                          SourceRange sourceRange) {
    if (!version)
      return false;

    if (!VersionRange::isValidVersion(*version)) {
      diags
          .diagnose(attrLoc, diag::availability_unsupported_version_number,
                    *version)
          .highlight(sourceRange);
      return true;
    }

    // Warn if the version is not a valid one for the domain. For example, macOS
    // 17 will never exist.
    if (domain->isVersioned() && !domain->isVersionValid(*version)) {
      diags
          .diagnose(attrLoc,
                    diag::availability_invalid_version_number_for_domain,
                    *version, *domain)
          .highlight(sourceRange);
    }

    return false;
  };

  if (checkVersion(attr->getRawIntroduced(), attr->IntroducedRange))
    return std::nullopt;
  if (checkVersion(attr->getRawDeprecated(), attr->DeprecatedRange))
    return std::nullopt;
  if (checkVersion(attr->getRawObsoleted(), attr->ObsoletedRange))
    return std::nullopt;

  bool hasIntroduced = attr->getRawIntroduced().has_value();
  bool hasDeprecated = attr->getRawDeprecated().has_value();
  auto hasObsoleted = attr->getRawObsoleted().has_value();
  bool hasVersionSpec = (hasIntroduced || hasDeprecated || hasObsoleted);

  if (!domain->isVersioned() && hasVersionSpec) {
    SourceRange versionSourceRange;
    if (hasIntroduced)
      versionSourceRange = attr->IntroducedRange;
    else if (hasDeprecated)
      versionSourceRange = attr->DeprecatedRange;
    else if (hasObsoleted)
      versionSourceRange = attr->ObsoletedRange;

    diags.diagnose(attrLoc, diag::availability_unexpected_version, *domain)
        .limitBehaviorIf(domain->isUniversal(), DiagnosticBehavior::Warning)
        .highlight(versionSourceRange);
    return std::nullopt;
  }

  if (domain->isSwiftLanguage() || domain->isPackageDescription()) {
    switch (attr->getKind()) {
    case AvailableAttr::Kind::Deprecated:
      diags.diagnose(attrLoc,
                     diag::attr_availability_expected_deprecated_version, attr,
                     *domain);
      return std::nullopt;

    case AvailableAttr::Kind::Unavailable:
      diags.diagnose(attrLoc, diag::attr_availability_cannot_be_used_for_domain,
                     "unavailable", attr, *domain);
      return std::nullopt;

    case AvailableAttr::Kind::NoAsync:
      diags.diagnose(attrLoc, diag::attr_availability_cannot_be_used_for_domain,
                     "noasync", attr, *domain);
      return std::nullopt;

    case AvailableAttr::Kind::Default:
      break;
    }
  }

  if (!hasVersionSpec && domain->isVersioned()) {
    switch (attr->getKind()) {
    case AvailableAttr::Kind::Default:
      diags.diagnose(domainLoc, diag::attr_availability_expected_version_spec,
                     attr, *domain);
      return std::nullopt;
    case AvailableAttr::Kind::Deprecated:
    case AvailableAttr::Kind::Unavailable:
    case AvailableAttr::Kind::NoAsync:
      break;
    }
  }

  return SemanticAvailableAttr(attr);
}

std::optional<llvm::VersionTuple> SemanticAvailableAttr::getIntroduced() const {
  if (auto version = attr->getRawIntroduced())
    return canonicalizePlatformVersion(getPlatform(), *version);
  return std::nullopt;
}

std::optional<AvailabilityDomainAndRange>
SemanticAvailableAttr::getIntroducedDomainAndRange(
    const ASTContext &Ctx) const {
  auto *attr = getParsedAttr();
  auto domain = getDomain();

  if (!attr->getRawIntroduced().has_value()) {
    // For versioned domains, an "introduced:" version is always required to
    // indicate introduction.
    if (domain.isVersioned())
      return std::nullopt;

    // For version-less domains, an attribute that does not indicate some other
    // kind of unconditional availability constraint implicitly specifies that
    // the decl is available in all versions of the domain.
    switch (attr->getKind()) {
    case AvailableAttr::Kind::Default:
      return AvailabilityDomainAndRange(domain.getRemappedDomain(Ctx),
                                        AvailabilityRange::alwaysAvailable());
    case AvailableAttr::Kind::Deprecated:
    case AvailableAttr::Kind::Unavailable:
    case AvailableAttr::Kind::NoAsync:
      return std::nullopt;
    }
  }

  llvm::VersionTuple introducedVersion = getIntroduced().value();
  llvm::VersionTuple remappedVersion;
  if (AvailabilityInference::updateIntroducedAvailabilityDomainForFallback(
          *this, Ctx, domain, remappedVersion))
    introducedVersion = remappedVersion;

  return AvailabilityDomainAndRange(domain,
                                    AvailabilityRange{introducedVersion});
}

std::optional<llvm::VersionTuple> SemanticAvailableAttr::getDeprecated() const {
  if (auto version = attr->getRawDeprecated())
    return canonicalizePlatformVersion(getPlatform(), *version);
  return std::nullopt;
}

std::optional<AvailabilityDomainAndRange>
SemanticAvailableAttr::getDeprecatedDomainAndRange(
    const ASTContext &Ctx) const {
  auto *attr = getParsedAttr();
  AvailabilityDomain domain = getDomain();

  if (!attr->getRawDeprecated().has_value()) {
    // Regardless of the whether the domain supports versions or not, an
    // unconditional deprecation attribute indicates the decl is always
    // deprecated.
    if (isUnconditionallyDeprecated())
      return AvailabilityDomainAndRange(domain.getRemappedDomain(Ctx),
                                        AvailabilityRange::alwaysAvailable());

    return std::nullopt;
  }

  llvm::VersionTuple deprecatedVersion = getDeprecated().value();
  llvm::VersionTuple remappedVersion;
  if (AvailabilityInference::updateDeprecatedAvailabilityDomainForFallback(
          *this, Ctx, domain, remappedVersion))
    deprecatedVersion = remappedVersion;

  return AvailabilityDomainAndRange(domain,
                                    AvailabilityRange{deprecatedVersion});
}

std::optional<llvm::VersionTuple> SemanticAvailableAttr::getObsoleted() const {
  if (auto version = attr->getRawObsoleted())
    return canonicalizePlatformVersion(getPlatform(), *version);
  return std::nullopt;
}

std::optional<AvailabilityDomainAndRange>
SemanticAvailableAttr::getObsoletedDomainAndRange(const ASTContext &Ctx) const {
  auto *attr = getParsedAttr();

  // Obsoletion always requires a version.
  if (!attr->getRawObsoleted().has_value())
    return std::nullopt;

  llvm::VersionTuple obsoletedVersion = getObsoleted().value();
  AvailabilityDomain domain = getDomain();
  llvm::VersionTuple remappedVersion;
  if (AvailabilityInference::updateObsoletedAvailabilityDomainForFallback(
          *this, Ctx, domain, remappedVersion))
    obsoletedVersion = remappedVersion;

  return AvailabilityDomainAndRange(domain,
                                    AvailabilityRange{obsoletedVersion});
}

namespace {
/// Infers the availability required to access a type.
class AvailabilityInferenceTypeWalker : public TypeWalker {
public:
  AvailabilityRange AvailabilityInfo = AvailabilityRange::alwaysAvailable();

  Action walkToTypePre(Type ty) override {
    if (auto *nominalDecl = ty->getAnyNominal()) {
      AvailabilityInfo.intersectWith(
          AvailabilityInference::availableRange(nominalDecl));
    }

    return Action::Continue;
  }
};
} // end anonymous namespace

AvailabilityRange AvailabilityInference::inferForType(Type t) {
  AvailabilityInferenceTypeWalker walker;
  t.walk(walker);
  return walker.AvailabilityInfo;
}

AvailabilityRange ASTContext::getSwiftFutureAvailability() const {
  auto target = LangOpts.Target;

  auto getFutureAvailabilityRange = []() -> AvailabilityRange {
    return AvailabilityRange(llvm::VersionTuple(99, 99, 0));
  };

  if (target.isMacOSX()) {
    return getFutureAvailabilityRange();
  } else if (target.isiOS()) {
    return getFutureAvailabilityRange();
  } else if (target.isWatchOS()) {
    return getFutureAvailabilityRange();
  } else if (target.isXROS()) {
    return getFutureAvailabilityRange();
  } else {
    return AvailabilityRange::alwaysAvailable();
  }
}

AvailabilityRange ASTContext::getSwiftAvailability(unsigned major,
                                                   unsigned minor) const {
  auto target = LangOpts.Target;

  // Deal with special cases for Swift 5.3 and lower
  if (major == 5 && minor <= 3) {
    if (target.getArchName() == "arm64e")
      return AvailabilityRange::alwaysAvailable();
    if (target.isMacOSX() && target.isAArch64())
      return AvailabilityRange::alwaysAvailable();
    if (target.isiOS() && target.isAArch64()
        && (target.isSimulatorEnvironment()
            || target.isMacCatalystEnvironment()))
      return AvailabilityRange::alwaysAvailable();
    if (target.isWatchOS() && target.isArch64Bit())
      return AvailabilityRange::alwaysAvailable();
  }

  switch (major) {
#define MAJOR_VERSION(V) case V: switch (minor) {
#define END_MAJOR_VERSION(V) } break;
#define PLATFORM(P, V)                                                         \
  if (IS_PLATFORM(P))                                                          \
    return AvailabilityRange(VersionRange::allGTE(llvm::VersionTuple V));
#define IS_PLATFORM(P) PLATFORM_TEST_##P
#define FUTURE                  return getSwiftFutureAvailability();
#define PLATFORM_TEST_macOS     target.isMacOSX()
#define PLATFORM_TEST_iOS       target.isiOS()
#define PLATFORM_TEST_watchOS   target.isWatchOS()
#define PLATFORM_TEST_visionOS  target.isXROS()

#define _SECOND(A, B) B
#define SECOND(T) _SECOND T

#define RUNTIME_VERSION(V, PLATFORMS)                                          \
  case SECOND(V):                                                              \
    PLATFORMS                                                                  \
    return AvailabilityRange::alwaysAvailable();

#include "swift/AST/RuntimeVersions.def"

#undef PLATFORM_TEST_macOS
#undef PLATFORM_TEST_iOS
#undef PLATFORM_TEST_watchOS
#undef PLATFORM_TEST_visionOS
#undef _SECOND
#undef SECOND

  case 99:
    if (minor == 99)
      return getSwiftFutureAvailability();
    break;
  }

  llvm::report_fatal_error(
    Twine("Missing runtime version data for Swift ") +
    Twine(major) + Twine('.') + Twine(minor));
}

bool ASTContext::supportsVersionedAvailability() const {
  return minimumAvailableOSVersionForTriple(LangOpts.Target).has_value();
}

bool swift::isExported(const Decl *D) {
  if (auto *VD = dyn_cast<ValueDecl>(D)) {
    return isExported(VD);
  }
  if (auto *PBD = dyn_cast<PatternBindingDecl>(D)) {
    for (unsigned i = 0, e = PBD->getNumPatternEntries(); i < e; ++i) {
      if (auto *VD = PBD->getAnchoringVarDecl(i))
        return isExported(VD);
    }

    return false;
  }
  if (auto *ED = dyn_cast<ExtensionDecl>(D)) {
    return isExported(ED);
  }

  return true;
}

bool swift::isExported(const ValueDecl *VD) {
  if (VD->getAttrs().hasAttribute<ImplementationOnlyAttr>())
    return false;
  if (VD->isObjCMemberImplementation())
    return false;

  // Is this part of the module's API or ABI?
  AccessScope accessScope =
      VD->getFormalAccessScope(nullptr,
                               /*treatUsableFromInlineAsPublic*/ true);
  if (accessScope.isPublic())
    return true;

  // Is this a stored property in a @frozen struct or class?
  if (auto *property = dyn_cast<VarDecl>(VD))
    if (property->isLayoutExposedToClients())
      return true;

  return false;
}

bool swift::hasConformancesToPublicProtocols(const ExtensionDecl *ED) {
  auto nominal = ED->getExtendedNominal();
  if (!nominal)
    return false;

  // Extensions of protocols cannot introduce additional conformances.
  if (isa<ProtocolDecl>(nominal))
    return false;

  auto protocols = ED->getLocalProtocols(ConformanceLookupKind::OnlyExplicit);
  for (const ProtocolDecl *PD : protocols) {
    AccessScope scope =
        PD->getFormalAccessScope(/*useDC*/ nullptr,
                                 /*treatUsableFromInlineAsPublic*/ true);
    if (scope.isPublic())
      return true;
  }

  return false;
}

bool swift::isExported(const ExtensionDecl *ED) {
  // An extension can only be exported if it extends an exported type.
  if (auto *NTD = ED->getExtendedNominal()) {
    if (!isExported(NTD))
      return false;
  }

  // If there are any exported members then the extension is exported.
  for (const Decl *D : ED->getMembers()) {
    if (isExported(D))
      return true;
  }

  // If the extension declares a conformance to a public protocol then the
  // extension is exported.
  if (hasConformancesToPublicProtocols(ED))
    return true;

  return false;
}
