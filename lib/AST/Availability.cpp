//===--- Availability.cpp - Swift Availability Structures -----------------===//
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
// FIXME: [availability] Remove this when possible
#include "swift/AST/DiagnosticsParse.h"
#include "swift/AST/DiagnosticsSema.h"
#include "swift/AST/PlatformKind.h"
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
  return AvailabilityRange(
      VersionRange::allGTE(Ctx.LangOpts.getMinPlatformVersion()));
}

AvailabilityRange AvailabilityRange::forInliningTarget(const ASTContext &Ctx) {
  return AvailabilityRange(
      VersionRange::allGTE(Ctx.LangOpts.MinimumInliningTargetVersion));
}

AvailabilityRange AvailabilityRange::forRuntimeTarget(const ASTContext &Ctx) {
  return AvailabilityRange(VersionRange::allGTE(Ctx.LangOpts.RuntimeVersion));
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

  /// A wrapper for AvailabilityDomain that implements a stable, total ordering for
  /// domains. This is needed to ensure that the inferred attributes are added to
  /// the declaration in a consistent order, preserving interface printing output
  /// stability across compilations.
  class OrderedAvailabilityDomain {
  public:
    AvailabilityDomain domain;

    OrderedAvailabilityDomain(AvailabilityDomain domain) : domain(domain) {}

    bool operator<(const OrderedAvailabilityDomain &other) const {
      auto kind = domain.getKind();
      auto otherKind = other.domain.getKind();
      if (kind != otherKind)
        return kind < otherKind;

      switch (kind) {
      case AvailabilityDomain::Kind::Universal:
      case AvailabilityDomain::Kind::SwiftLanguage:
      case AvailabilityDomain::Kind::PackageDescription:
      case AvailabilityDomain::Kind::Embedded:
        return false;
      case AvailabilityDomain::Kind::Platform:
        return domain.getPlatformKind() < other.domain.getPlatformKind();
      case AvailabilityDomain::Kind::Custom: {
        auto mod = domain.getModule();
        auto otherMod = other.domain.getModule();
        if (mod != otherMod)
          return mod->getName() < otherMod->getName();

        return domain.getNameForAttributePrinting() <
               other.domain.getNameForAttributePrinting();
      }
      }
    }
  };

  // Iterate over the declarations and infer required availability on
  // a per-platform basis.
  std::map<OrderedAvailabilityDomain, InferredAvailability> Inferred;
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
      D = AvailabilityInference::parentDeclForInferredAvailability(D);
    } while (D);
  }

  DeclAttributes &Attrs = ToDecl->getAttrs();

  // Create an availability attribute for each observed platform and add
  // to ToDecl.
  for (auto &Pair : Inferred) {
    if (auto Attr =
            createAvailableAttr(Pair.first.domain, Pair.second, Context))
      Attrs.add(Attr);
  }
}

/// Returns the decl that should be considered the parent decl of the given decl
/// when looking for inherited availability annotations.
const Decl *
AvailabilityInference::parentDeclForInferredAvailability(const Decl *D) {
  if (auto *AD = dyn_cast<AccessorDecl>(D))
    return AD->getStorage();

  if (auto *ED = dyn_cast<ExtensionDecl>(D)) {
    if (auto *NTD = ED->getExtendedNominal())
      return NTD;
  }

  if (auto *PBD = dyn_cast<PatternBindingDecl>(D)) {
    if (PBD->getNumPatternEntries() < 1)
      return nullptr;

    return PBD->getAnchoringVarDecl(0);
  }

  if (auto *OTD = dyn_cast<OpaqueTypeDecl>(D))
    return OTD->getNamingDecl();

  // Clang decls may be inaccurately parented rdar://53956555
  if (D->hasClangNode())
    return nullptr;

  // Availability is inherited from the enclosing context.
  return D->getDeclContext()->getInnermostDeclarationDeclContext();
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

bool AvailabilityInference::updateIntroducedPlatformForFallback(
    const SemanticAvailableAttr &attr, const ASTContext &Ctx,
    llvm::StringRef &Platform, llvm::VersionTuple &PlatformVer) {
  std::optional<llvm::VersionTuple> IntroducedVersion = attr.getIntroduced();
  if (attr.getPlatform() == PlatformKind::iOS &&
      IntroducedVersion.has_value() &&
      isPlatformActive(PlatformKind::visionOS, Ctx.LangOpts)) {
    // We re-map the iOS introduced version to the corresponding visionOS version
    auto PotentiallyRemappedIntroducedVersion =
        getRemappedIntroducedVersionForFallbackPlatform(Ctx,
                                                        *IntroducedVersion);
    if (PotentiallyRemappedIntroducedVersion.has_value()) {
      Platform = swift::prettyPlatformString(PlatformKind::visionOS);
      PlatformVer = PotentiallyRemappedIntroducedVersion.value();
      return true;
    }
  }
  return false;
}

bool AvailabilityInference::updateDeprecatedPlatformForFallback(
    const SemanticAvailableAttr &attr, const ASTContext &Ctx,
    llvm::StringRef &Platform, llvm::VersionTuple &PlatformVer) {
  std::optional<llvm::VersionTuple> DeprecatedVersion = attr.getDeprecated();
  if (attr.getPlatform() == PlatformKind::iOS &&
      DeprecatedVersion.has_value() &&
      isPlatformActive(PlatformKind::visionOS, Ctx.LangOpts)) {
    // We re-map the iOS deprecated version to the corresponding visionOS version
    auto PotentiallyRemappedDeprecatedVersion =
        getRemappedDeprecatedObsoletedVersionForFallbackPlatform(
            Ctx, *DeprecatedVersion);
    if (PotentiallyRemappedDeprecatedVersion.has_value()) {
      Platform = swift::prettyPlatformString(PlatformKind::visionOS);
      PlatformVer = PotentiallyRemappedDeprecatedVersion.value();
      return true;
    }
  }
  return false;
}

bool AvailabilityInference::updateObsoletedPlatformForFallback(
    const SemanticAvailableAttr &attr, const ASTContext &Ctx,
    llvm::StringRef &Platform, llvm::VersionTuple &PlatformVer) {
  std::optional<llvm::VersionTuple> ObsoletedVersion = attr.getObsoleted();
  if (attr.getPlatform() == PlatformKind::iOS && ObsoletedVersion.has_value() &&
      isPlatformActive(PlatformKind::visionOS, Ctx.LangOpts)) {
    // We re-map the iOS obsoleted version to the corresponding visionOS version
    auto PotentiallyRemappedObsoletedVersion =
        getRemappedDeprecatedObsoletedVersionForFallbackPlatform(
            Ctx, *ObsoletedVersion);
    if (PotentiallyRemappedObsoletedVersion.has_value()) {
      Platform = swift::prettyPlatformString(PlatformKind::visionOS);
      PlatformVer = PotentiallyRemappedObsoletedVersion.value();
      return true;
    }
  }
  return false;
}

void AvailabilityInference::updatePlatformStringForFallback(
    const SemanticAvailableAttr &attr, const ASTContext &Ctx,
    llvm::StringRef &Platform) {
  if (attr.getPlatform() == PlatformKind::iOS &&
      isPlatformActive(PlatformKind::visionOS, Ctx.LangOpts)) {
    Platform = swift::prettyPlatformString(PlatformKind::visionOS);
  }
}

bool AvailabilityInference::updateBeforePlatformForFallback(
    const BackDeployedAttr *attr, const ASTContext &Ctx,
    llvm::StringRef &Platform, llvm::VersionTuple &PlatformVer) {
  auto BeforeVersion = attr->Version;
  if (attr->Platform == PlatformKind::iOS &&
      isPlatformActive(PlatformKind::visionOS, Ctx.LangOpts)) {
    // We re-map the iOS before version to the corresponding visionOS version
    auto PotentiallyRemappedIntroducedVersion =
        getRemappedIntroducedVersionForFallbackPlatform(Ctx, BeforeVersion);
    if (PotentiallyRemappedIntroducedVersion.has_value()) {
      Platform = swift::prettyPlatformString(PlatformKind::visionOS);
      PlatformVer = PotentiallyRemappedIntroducedVersion.value();
      return true;
    }
  }
  return false;
}

static std::optional<SemanticAvailableAttr>
getDeclAvailableAttrForPlatformIntroduction(const Decl *D) {
  std::optional<SemanticAvailableAttr> bestAvailAttr;

  D = abstractSyntaxDeclForAvailableAttribute(D);

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
  return AvailabilityInference::isAvailableAsSPI(this);
}

SemanticAvailableAttributes
Decl::getSemanticAvailableAttrs(bool includeInactive) const {
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

    auto deprecatedVersion = attr.getDeprecated();

    StringRef deprecatedPlatform;
    llvm::VersionTuple remappedDeprecatedVersion;
    if (AvailabilityInference::updateDeprecatedPlatformForFallback(
            attr, ctx, deprecatedPlatform, remappedDeprecatedVersion))
      deprecatedVersion = remappedDeprecatedVersion;

    if (!deprecatedVersion.has_value())
      continue;

    llvm::VersionTuple minVersion = attr.getActiveVersion(ctx);

    // We treat the declaration as deprecated if it is deprecated on
    // all deployment targets.
    if (deprecatedVersion.value() <= minVersion) {
      result.emplace(attr);
    }
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

    // FIXME: This needs to do a version remap.
    auto deprecatedVersion = attr.getDeprecated();
    if (!deprecatedVersion.has_value())
      continue;

    llvm::VersionTuple activeVersion = attr.getActiveVersion(ctx);

    if (deprecatedVersion.value() > activeVersion)
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

static llvm::SmallVector<AvailabilityDomain, 2>
availabilityDomainsForABICompatibility(const ASTContext &ctx) {
  llvm::SmallVector<AvailabilityDomain, 2> domains;

  // Regardless of target platform, binaries built for Embedded do not require
  // compatibility.
  if (ctx.LangOpts.hasFeature(Feature::Embedded))
    return domains;

  if (auto targetDomain = AvailabilityDomain::forTargetPlatform(ctx))
    domains.push_back(targetDomain->getABICompatibilityDomain());
  
  if (auto variantDomain = AvailabilityDomain::forTargetVariantPlatform(ctx))
    domains.push_back(variantDomain->getABICompatibilityDomain());

  return domains;
}

/// Returns true if \p decl is proven to be unavailable for all platforms that
/// external modules interacting with this module could target. A declaration
/// that is not proven to be unavailable in this way could be reachable at
/// runtime, even if it is unavailable to all code in this module.
static bool isUnavailableForAllABICompatiblePlatforms(const Decl *decl) {
  // Don't trust unavailability on declarations from Clang modules.
  if (isa<ClangModuleUnit>(decl->getDeclContext()->getModuleScopeContext()))
    return false;

  auto &ctx = decl->getASTContext();
  llvm::SmallVector<AvailabilityDomain, 2> compatibilityDomains =
      availabilityDomainsForABICompatibility(ctx);

  llvm::SmallSet<AvailabilityDomain, 8> unavailableDescendantDomains;
  llvm::SmallSet<AvailabilityDomain, 8> availableDescendantDomains;

  // Build up the collection of relevant available and unavailable platform
  // domains by looking at all the @available attributes. Along the way, we
  // may find an attribute that makes the declaration universally unavailable
  // in which case platform availability is irrelevant.
  for (auto attr : decl->getSemanticAvailableAttrs(/*includeInactive=*/true)) {
    auto domain = attr.getDomain();
    bool isCompabilityDomainDescendant =
        llvm::find_if(compatibilityDomains,
                      [&domain](AvailabilityDomain compatibilityDomain) {
                        return compatibilityDomain.contains(domain);
                      }) != compatibilityDomains.end();

    if (isCompabilityDomainDescendant) {
      // Record the whether the descendant domain is marked available
      // or unavailable. Unavailability overrides availability.
      if (attr.isUnconditionallyUnavailable()) {
        availableDescendantDomains.erase(domain);
        unavailableDescendantDomains.insert(domain);
      } else if (!unavailableDescendantDomains.contains(domain)) {
        availableDescendantDomains.insert(domain);
      }
    } else if (attr.isActive(ctx)) {
      // The declaration is always unavailable if an active attribute from a
      // domain outside the compatibility hierarchy indicates unavailability.
      if (attr.isUnconditionallyUnavailable())
        return true;
    }
  }

  // If there aren't any compatibility domains to check and we didn't find any
  // other active attributes that make the declaration unavailable, then it must
  // be available.
  if (compatibilityDomains.empty())
    return false;

  // Verify that the declaration has been marked unavailable in every
  // compatibility domain.
  for (auto compatibilityDomain : compatibilityDomains) {
    if (!unavailableDescendantDomains.contains(compatibilityDomain))
      return false;
  }
  
  // Verify that there aren't any explicitly available descendant domains.
  if (availableDescendantDomains.size() > 0)
    return false;

  return true;
}

SemanticDeclAvailability
SemanticDeclAvailabilityRequest::evaluate(Evaluator &evaluator,
                                          const Decl *decl) const {
  auto inherited = SemanticDeclAvailability::PotentiallyAvailable;
  if (auto *parent =
          AvailabilityInference::parentDeclForInferredAvailability(decl)) {
    inherited = evaluateOrDefault(
        evaluator, SemanticDeclAvailabilityRequest{parent}, inherited);
  }

  if (inherited == SemanticDeclAvailability::CompletelyUnavailable ||
      isUnavailableForAllABICompatiblePlatforms(decl))
    return SemanticDeclAvailability::CompletelyUnavailable;

  if (inherited == SemanticDeclAvailability::ConditionallyUnavailable ||
      decl->isUnavailable())
    return SemanticDeclAvailability::ConditionallyUnavailable;

  return SemanticDeclAvailability::PotentiallyAvailable;
}

bool Decl::isSemanticallyUnavailable() const {
  auto availability = evaluateOrDefault(
      getASTContext().evaluator, SemanticDeclAvailabilityRequest{this},
      SemanticDeclAvailability::PotentiallyAvailable);
  return availability != SemanticDeclAvailability::PotentiallyAvailable;
}

bool Decl::isUnreachableAtRuntime() const {
  auto availability = evaluateOrDefault(
      getASTContext().evaluator, SemanticDeclAvailabilityRequest{this},
      SemanticDeclAvailability::PotentiallyAvailable);
  return availability == SemanticDeclAvailability::CompletelyUnavailable;
}

static UnavailableDeclOptimization
getEffectiveUnavailableDeclOptimization(ASTContext &ctx) {
  if (ctx.LangOpts.UnavailableDeclOptimizationMode.has_value())
    return *ctx.LangOpts.UnavailableDeclOptimizationMode;

  return UnavailableDeclOptimization::None;
}

bool Decl::isAvailableDuringLowering() const {
  // Unconditionally unavailable declarations should be skipped during lowering
  // when -unavailable-decl-optimization=complete is specified.
  if (getEffectiveUnavailableDeclOptimization(getASTContext()) !=
      UnavailableDeclOptimization::Complete)
    return true;

  return !isUnreachableAtRuntime();
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
    const Decl *D, const SpecializeAttr *attr, ASTContext &ctx) {
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
    return bestAvailAttr->getIntroducedRange(ctx);

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

  if (auto parent =
          AvailabilityInference::parentDeclForInferredAvailability(this)) {
    if (auto *ED = dyn_cast<ExtensionDecl>(parent)) {
      if (auto attr = getDeclAvailableAttrForPlatformIntroduction(ED))
        return attr;
    }
  }

  return std::nullopt;
}

AvailabilityRange AvailabilityInference::availableRange(const Decl *D) {
  if (auto attr = D->getAvailableAttrForPlatformIntroduction())
    return attr->getIntroducedRange(D->getASTContext());

  return AvailabilityRange::alwaysAvailable();
}

bool AvailabilityInference::isAvailableAsSPI(const Decl *D) {
  if (auto attr = D->getAvailableAttrForPlatformIntroduction())
    return attr->isSPI();

  return false;
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
  auto attrName = attr->getAttrName();
  auto domainLoc = attr->getDomainLoc();
  auto declContext = decl->getInnermostDeclContext();
  auto mutableAttr = const_cast<AvailableAttr *>(attr);
  auto domain = mutableAttr->DomainOrIdentifier.resolveInDeclContext(
      domainLoc, declContext);

  if (!domain)
    return std::nullopt;

  auto domainName = domain->getNameForAttributePrinting();
  auto semanticAttr = SemanticAvailableAttr(attr);

  bool hasIntroduced = attr->getRawIntroduced().has_value();
  bool hasDeprecated = attr->getRawDeprecated().has_value();
  auto hasObsoleted = attr->getRawObsoleted().has_value();
  bool hasVersionSpec = (hasIntroduced || hasDeprecated || hasObsoleted);

  if (!domain->isVersioned() && hasVersionSpec) {
    SourceRange versionSourceRange;
    if (hasIntroduced)
      versionSourceRange = semanticAttr.getIntroducedSourceRange();
    else if (hasDeprecated)
      versionSourceRange = semanticAttr.getDeprecatedSourceRange();
    else if (hasObsoleted)
      versionSourceRange = semanticAttr.getObsoletedSourceRange();

    diags
        .diagnose(attrLoc, diag::attr_availability_unexpected_version, attr,
                  domainName)
        .highlight(versionSourceRange);
    return std::nullopt;
  }

  if (domain->isSwiftLanguage() || domain->isPackageDescription()) {
    switch (attr->getKind()) {
    case AvailableAttr::Kind::Deprecated:
      diags.diagnose(attrLoc,
                     diag::attr_availability_expected_deprecated_version,
                     attrName, domainName);
      return std::nullopt;

    case AvailableAttr::Kind::Unavailable:
      diags.diagnose(attrLoc, diag::attr_availability_cannot_be_used_for_domain,
                     "unavailable", attrName, domainName);
      return std::nullopt;

    case AvailableAttr::Kind::NoAsync:
      diags.diagnose(attrLoc, diag::attr_availability_cannot_be_used_for_domain,
                     "noasync", attrName, domainName);
      return std::nullopt;

    case AvailableAttr::Kind::Default:
      break;
    }

    if (!hasVersionSpec) {
      diags.diagnose(attrLoc, diag::attr_availability_expected_version_spec,
                     attrName, domainName);
      return std::nullopt;
    }
  }

  return semanticAttr;
}

std::optional<llvm::VersionTuple> SemanticAvailableAttr::getIntroduced() const {
  if (auto version = attr->getRawIntroduced())
    return canonicalizePlatformVersion(getPlatform(), *version);
  return std::nullopt;
}

AvailabilityRange
SemanticAvailableAttr::getIntroducedRange(const ASTContext &Ctx) const {
  assert(getDomain().isActive(Ctx));

  auto *attr = getParsedAttr();
  if (!attr->getRawIntroduced().has_value())
    return AvailabilityRange::alwaysAvailable();

  llvm::VersionTuple IntroducedVersion = getIntroduced().value();
  StringRef Platform;
  llvm::VersionTuple RemappedIntroducedVersion;
  if (AvailabilityInference::updateIntroducedPlatformForFallback(
          *this, Ctx, Platform, RemappedIntroducedVersion))
    IntroducedVersion = RemappedIntroducedVersion;

  return AvailabilityRange{VersionRange::allGTE(IntroducedVersion)};
}

std::optional<llvm::VersionTuple> SemanticAvailableAttr::getDeprecated() const {
  if (auto version = attr->getRawDeprecated())
    return canonicalizePlatformVersion(getPlatform(), *version);
  return std::nullopt;
}

std::optional<llvm::VersionTuple> SemanticAvailableAttr::getObsoleted() const {
  if (auto version = attr->getRawObsoleted())
    return canonicalizePlatformVersion(getPlatform(), *version);
  return std::nullopt;
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
    return AvailabilityRange(
        VersionRange::allGTE(llvm::VersionTuple(99, 99, 0)));
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

// FIXME: Rename abstractSyntaxDeclForAvailableAttribute since it's useful
// for more attributes than `@available`.
const Decl *
swift::abstractSyntaxDeclForAvailableAttribute(const Decl *ConcreteSyntaxDecl) {
  // This function needs to be kept in sync with its counterpart,
  // concreteSyntaxDeclForAvailableAttribute().

  if (auto *PBD = dyn_cast<PatternBindingDecl>(ConcreteSyntaxDecl)) {
    // Existing @available attributes in the AST are attached to VarDecls
    // rather than PatternBindingDecls, so we return the first VarDecl for
    // the pattern binding declaration.
    // This is safe, even though there may be multiple VarDecls, because
    // all parsed attribute that appear in the concrete syntax upon on the
    // PatternBindingDecl are added to all of the VarDecls for the pattern
    // binding.
    for (auto index : range(PBD->getNumPatternEntries())) {
      if (auto VD = PBD->getAnchoringVarDecl(index))
        return VD;
    }
  } else if (auto *ECD = dyn_cast<EnumCaseDecl>(ConcreteSyntaxDecl)) {
    // Similar to the PatternBindingDecl case above, we return the
    // first EnumElementDecl.
    if (auto *Elem = ECD->getFirstElement()) {
      return Elem;
    }
  }

  return ConcreteSyntaxDecl;
}
