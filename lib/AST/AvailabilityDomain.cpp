//===--- AvailabilityDomain.cpp - Swift Availability Domains --------------===//
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

#include "swift/AST/AvailabilityDomain.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/DiagnosticsSema.h"
#include "swift/AST/Module.h"
#include "swift/AST/TypeCheckRequests.h"
#include "swift/Basic/Assertions.h"
#include "swift/ClangImporter/ClangImporter.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/Decl.h"
#include "llvm/ADT/StringSwitch.h"

using namespace swift;

CustomAvailabilityDomain::Kind
getCustomDomainKind(clang::FeatureAvailKind featureAvailKind) {
  switch (featureAvailKind) {
  case clang::FeatureAvailKind::Available:
    return CustomAvailabilityDomain::Kind::Enabled;
  case clang::FeatureAvailKind::Unavailable:
    return CustomAvailabilityDomain::Kind::Disabled;
  case clang::FeatureAvailKind::Dynamic:
    return CustomAvailabilityDomain::Kind::Dynamic;
  case clang::FeatureAvailKind::AlwaysAvailable:
    return CustomAvailabilityDomain::Kind::AlwaysEnabled;
  default:
    llvm::report_fatal_error("unexpected kind");
  }
}

static const CustomAvailabilityDomain *
customDomainForClangDecl(ValueDecl *decl) {
  auto *clangDecl = decl->getClangDecl();
  auto *varDecl = dyn_cast_or_null<clang::VarDecl>(clangDecl);
  if (!varDecl)
    return nullptr;

  auto featureInfo = clangDecl->getASTContext().getFeatureAvailInfo(
      const_cast<clang::VarDecl *>(varDecl));

  // Ensure the decl actually represents an availability domain.
  if (featureInfo.first.empty())
    return nullptr;

  // Check that the domain has a supported availability kind.
  switch (featureInfo.second.Kind) {
  case clang::FeatureAvailKind::Available:
  case clang::FeatureAvailKind::Unavailable:
  case clang::FeatureAvailKind::Dynamic:
  case clang::FeatureAvailKind::AlwaysAvailable:
    break;
  default:
    return nullptr;
  }

  auto &ctx = decl->getASTContext();
  FuncDecl *predicate = nullptr;
  if (featureInfo.second.Kind == clang::FeatureAvailKind::Dynamic)
    predicate =
        ctx.getClangModuleLoader()->getAvailabilityDomainPredicate(varDecl);

  return CustomAvailabilityDomain::get(
      featureInfo.first, getCustomDomainKind(featureInfo.second.Kind),
      decl->getModuleContext(), decl, predicate, ctx);
}

std::optional<AvailabilityDomain>
AvailabilityDomainForDeclRequest::evaluate(Evaluator &evaluator,
                                           ValueDecl *decl) const {
  if (!decl)
    return std::nullopt;

  if (decl->hasClangNode()) {
    if (auto *customDomain = customDomainForClangDecl(decl))
      return AvailabilityDomain::forCustom(customDomain);
  } else {
    // FIXME: [availability] Handle Swift availability domains decls.
  }

  return std::nullopt;
}

std::optional<AvailabilityDomain>
AvailabilityDomain::forCustom(ValueDecl *decl) {
  if (!decl)
    return std::nullopt;

  return evaluateOrDefault(decl->getASTContext().evaluator,
                           AvailabilityDomainForDeclRequest{decl}, {});
}

static AvailabilityDomain getSwiftRuntimeDomain(ASTContext &ctx) {
  // If -enable-experimental-feature StandaloneSwiftAvailability is specified
  // then forcibly separate platform and Swift runtime availability.
  if (ctx.LangOpts.hasFeature(Feature::StandaloneSwiftAvailability))
    return AvailabilityDomain::forStandaloneSwiftRuntime();

  // FIXME: [runtime availability] Return the platform Swift runtime domain.

  return AvailabilityDomain::forStandaloneSwiftRuntime();
}

std::optional<AvailabilityDomain>
AvailabilityDomain::builtinDomainForString(StringRef string,
                                           const DeclContext *declContext) {
  auto &ctx = declContext->getASTContext();
  auto domain =
      llvm::StringSwitch<std::optional<AvailabilityDomain>>(string)
          .Case("*", AvailabilityDomain::forUniversal())
          .Case("swift", AvailabilityDomain::forSwiftLanguageMode())
          .Case("SwiftLanguageMode", AvailabilityDomain::forSwiftLanguageMode())
          .Case("Swift", getSwiftRuntimeDomain(ctx))
          .Case("_PackageDescription",
                AvailabilityDomain::forPackageDescription())
          .Default(std::nullopt);

  if (domain)
    return domain;

  if (auto platformKind = platformFromString(string))
    return AvailabilityDomain::forPlatform(*platformKind);

  return std::nullopt;
}

bool AvailabilityDomain::isVersioned() const {
  switch (getKind()) {
  case Kind::Universal:
  case Kind::Embedded:
    return false;
  case Kind::SwiftLanguageMode:
  case Kind::StandaloneSwiftRuntime:
  case Kind::PackageDescription:
  case Kind::Platform:
    return true;
  case Kind::Custom:
    // FIXME: [availability] Support versioned custom availability domains
    return false;
  }
}

bool AvailabilityDomain::isVersionValid(
    const llvm::VersionTuple &version) const {
  ASSERT(isVersioned());

  switch (getKind()) {
  case Kind::Universal:
  case Kind::Embedded:
    llvm_unreachable("unexpected domain kind");
  case Kind::SwiftLanguageMode:
  case Kind::PackageDescription:
    return true;
  case Kind::StandaloneSwiftRuntime:
    // Swift 5.0 is the first ABI stable Swift runtime version.
    if (version.getMajor() < 5)
      return false;
    return true;

  case Kind::Platform:
    // If the platform kind corresponds to a specific OS, LLVM is the source of
    // truth for version validity.
    if (auto osType = tripleOSTypeForPlatform(getPlatformKind()))
      return llvm::Triple::isValidVersionForOS(*osType, version);

    // Unified versioning for Apple's operating systems starts at 26.0.
    if (getPlatformKind() == PlatformKind::anyAppleOS)
      return (version.getMajor() >= 26);

    return true;

  case Kind::Custom:
    return true;
  }
}

bool AvailabilityDomain::supportsContextRefinement() const {
  switch (getKind()) {
  case Kind::Universal:
  case Kind::Embedded:
  case Kind::SwiftLanguageMode:
  case Kind::PackageDescription:
    return false;
  case Kind::StandaloneSwiftRuntime:
  case Kind::Platform:
  case Kind::Custom:
    return true;
  }
}

bool AvailabilityDomain::supportsQueries() const {
  switch (getKind()) {
  case Kind::Universal:
  case Kind::Embedded:
  case Kind::SwiftLanguageMode:
  case Kind::PackageDescription:
    return false;
  case Kind::StandaloneSwiftRuntime:
  case Kind::Platform:
  case Kind::Custom:
    return true;
  }
}

bool AvailabilityDomain::isActive(const ASTContext &ctx,
                                  bool forTargetVariant) const {
  switch (getKind()) {
  case Kind::Universal:
  case Kind::SwiftLanguageMode:
  case Kind::PackageDescription:
  case Kind::Embedded:
    return true;
  case Kind::StandaloneSwiftRuntime:
    // FIXME: [runtime availability] Active either when
    // StandaloneSwiftAvailability is enabled or the target supports a
    // standalone Swift runtime.
    return ctx.LangOpts.hasFeature(Feature::SwiftRuntimeAvailability);
  case Kind::Platform:
    return isPlatformActive(getPlatformKind(), ctx.LangOpts, forTargetVariant);
  case Kind::Custom:
    // For now, custom domains are always active but it's conceivable that in
    // the future someone might want to define a domain but leave it inactive.
    return true;
  }
}

bool AvailabilityDomain::isActivePlatform(const ASTContext &ctx,
                                          bool forTargetVariant) const {
  if (!isPlatform())
    return false;

  return isActive(ctx, forTargetVariant);
}

bool AvailabilityDomain::mustBeSpecifiedAlone() const {
  switch (getKind()) {
  case Kind::Universal:
  case Kind::SwiftLanguageMode:
  case Kind::PackageDescription:
  case Kind::Embedded:
  case Kind::Custom:
    return true;
  case Kind::StandaloneSwiftRuntime:
  case Kind::Platform:
    // Platform and Swift runtime availability specifications can appear
    // together, e.g. `@available(Swift 6, macOS 15, iOS 18, *)`.
    // If there are ever multiple disjoint groups of domains that may be
    // specified together in the future, this will need to be re-designed.
    return false;
  }
}

static std::optional<llvm::VersionTuple>
getDeploymentVersion(const AvailabilityDomain &domain, const ASTContext &ctx) {
  switch (domain.getKind()) {
  case AvailabilityDomain::Kind::Universal:
  case AvailabilityDomain::Kind::Embedded:
  case AvailabilityDomain::Kind::Custom:
    return std::nullopt;
  case AvailabilityDomain::Kind::SwiftLanguageMode:
    return ctx.LangOpts.EffectiveLanguageVersion;
  case AvailabilityDomain::Kind::PackageDescription:
    return ctx.LangOpts.PackageDescriptionVersion;
  case AvailabilityDomain::Kind::StandaloneSwiftRuntime:
    if (!ctx.LangOpts.hasFeature(Feature::SwiftRuntimeAvailability))
      return std::nullopt;
    return ctx.LangOpts.MinSwiftRuntimeVersion;
  case AvailabilityDomain::Kind::Platform:
    if (domain.isActive(ctx))
      return ctx.LangOpts.getMinPlatformVersion();
    return std::nullopt;
  }
}

std::optional<AvailabilityRange>
AvailabilityDomain::getDeploymentRange(const ASTContext &ctx) const {
  if (isVersioned()) {
    if (auto version = getDeploymentVersion(*this, ctx))
      return AvailabilityRange{*version};

    return std::nullopt;
  }

  if (auto customDomain = getCustomDomain()) {
    switch (customDomain->getKind()) {
    case CustomAvailabilityDomain::Kind::AlwaysEnabled:
      return AvailabilityRange::alwaysAvailable();
    case CustomAvailabilityDomain::Kind::Enabled:
    case CustomAvailabilityDomain::Kind::Disabled:
    case CustomAvailabilityDomain::Kind::Dynamic:
      return std::nullopt;
    }
  }
  return std::nullopt;
}

llvm::StringRef AvailabilityDomain::getNameForDiagnostics() const {
  switch (getKind()) {
  case Kind::Universal:
    return "*";
  case Kind::SwiftLanguageMode:
    // FIXME: [runtime availability] Render language mode diags differently.
    return "Swift";
  case Kind::StandaloneSwiftRuntime:
    return "Swift";
  case Kind::PackageDescription:
    return "PackageDescription";
  case Kind::Embedded:
    return "Embedded Swift";
  case Kind::Platform:
    return swift::prettyPlatformString(getPlatformKind());
  case Kind::Custom:
    return getCustomDomain()->getName().str();
  }
}

llvm::StringRef AvailabilityDomain::getNameForAttributePrinting() const {
  switch (getKind()) {
  case Kind::Universal:
    return "*";
  case Kind::SwiftLanguageMode:
    return "swift";
  case Kind::StandaloneSwiftRuntime:
    return "Swift";
  case Kind::PackageDescription:
    return "_PackageDescription";
  case Kind::Embedded:
    return "Embedded";
  case Kind::Platform:
    return swift::platformString(getPlatformKind());
  case Kind::Custom:
    return getCustomDomain()->getName().str();
  }
}

ValueDecl *AvailabilityDomain::getDecl() const {
  if (auto *customDomain = getCustomDomain())
    return customDomain->getDecl();

  return nullptr;
}

ModuleDecl *AvailabilityDomain::getModule() const {
  if (auto customDomain = getCustomDomain())
    return customDomain->getModule();

  return nullptr;
}

bool AvailabilityDomain::contains(const AvailabilityDomain &other) const {
  switch (getKind()) {
  case Kind::Universal:
    return true;
  case Kind::SwiftLanguageMode:
  case Kind::StandaloneSwiftRuntime:
  case Kind::PackageDescription:
  case Kind::Embedded:
  case Kind::Custom:
    return other == *this;
  case Kind::Platform:
    if (getPlatformKind() == other.getPlatformKind())
      return true;
    return inheritsAvailabilityFromPlatform(other.getPlatformKind(),
                                            getPlatformKind());
  }
}

bool AvailabilityDomain::isRoot() const {
  switch (getKind()) {
  case AvailabilityDomain::Kind::Universal:
  case AvailabilityDomain::Kind::Embedded:
  case AvailabilityDomain::Kind::SwiftLanguageMode:
  case AvailabilityDomain::Kind::StandaloneSwiftRuntime:
  case AvailabilityDomain::Kind::PackageDescription:
    return true;
  case AvailabilityDomain::Kind::Platform:
    return getRootDomain() == *this;
  case AvailabilityDomain::Kind::Custom:
    // For now, all custom domains are their own root.
    return true;
  }
}

AvailabilityDomain AvailabilityDomain::getRootDomain() const {
  if (!isPlatform())
    return *this;

  // iOS specifically contains a few other platforms.
  auto iOSDomain = AvailabilityDomain::forPlatform(PlatformKind::iOS);
  if (iOSDomain.contains(*this))
    return iOSDomain;

  // App Extension domains are contained by their base platform domain.
  if (auto basePlatform = basePlatformForExtensionPlatform(getPlatformKind()))
    return AvailabilityDomain::forPlatform(*basePlatform);

  return *this;
}

static std::optional<PlatformKind>
getApplePlatformKindForTarget(const llvm::Triple &target) {
  if (target.isMacOSX())
    return PlatformKind::macOS;
  if (target.isTvOS()) // Must be checked before isiOS.
    return PlatformKind::tvOS;
  if (target.isiOS())
    return PlatformKind::iOS;
  if (target.isWatchOS())
    return PlatformKind::watchOS;
  if (target.isXROS())
    return PlatformKind::visionOS;

  return std::nullopt;
}

std::optional<AvailabilityDomain>
AvailabilityDomain::getRemappedDomainOrNull(const ASTContext &ctx) const {
  if (getPlatformKind() == PlatformKind::iOS &&
      isPlatformActive(PlatformKind::visionOS, ctx.LangOpts))
    return AvailabilityDomain::forPlatform(PlatformKind::visionOS);

  if (getPlatformKind() == PlatformKind::anyAppleOS) {
    if (auto applePlatformKind =
            getApplePlatformKindForTarget(ctx.LangOpts.Target))
      return AvailabilityDomain::forPlatform(*applePlatformKind);

    return std::nullopt;
  }

  return std::nullopt;
}

static const clang::DarwinSDKInfo::RelatedTargetVersionMapping *
getFallbackVersionMapping(const ASTContext &Ctx,
                          clang::DarwinSDKInfo::OSEnvPair Kind) {
  auto *SDKInfo = Ctx.getDarwinSDKInfo();
  if (!SDKInfo)
    return nullptr;
  return SDKInfo->getVersionMapping(Kind);
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

AvailabilityDomainAndRange AvailabilityDomain::getRemappedDomainAndRange(
    const llvm::VersionTuple &version, AvailabilityVersionKind versionKind,
    const ASTContext &ctx) const {
  auto remappedDomain = getRemappedDomainOrNull(ctx);
  if (!remappedDomain)
    return {*this, AvailabilityRange{version}};

  if (getPlatformKind() == PlatformKind::anyAppleOS)
    return {*remappedDomain, AvailabilityRange{version}};

  if (getPlatformKind() == PlatformKind::iOS) {
    std::optional<clang::VersionTuple> remappedVersion;
    switch (versionKind) {
    case AvailabilityVersionKind::Introduced:
      remappedVersion =
          getRemappedIntroducedVersionForFallbackPlatform(ctx, version);
      break;
    case AvailabilityVersionKind::Deprecated:
    case AvailabilityVersionKind::Obsoleted:
      remappedVersion =
          getRemappedDeprecatedObsoletedVersionForFallbackPlatform(ctx,
                                                                   version);
      break;
    }

    if (remappedVersion)
      return {*remappedDomain, AvailabilityRange{*remappedVersion}};
  }

  return {*this, AvailabilityRange{version}};
}

bool IsCustomAvailabilityDomainPermanentlyEnabled::evaluate(
    Evaluator &evaluator, const CustomAvailabilityDomain *customDomain) const {
  switch (customDomain->getKind()) {
  case CustomAvailabilityDomain::Kind::Enabled:
  case CustomAvailabilityDomain::Kind::Disabled:
  case CustomAvailabilityDomain::Kind::Dynamic:
    return false;

  case CustomAvailabilityDomain::Kind::AlwaysEnabled:
    break;
  }

  auto *domainDecl = customDomain->getDecl();
  if (!domainDecl)
    return false;

  if (auto deprecatedAttr = domainDecl->getDeprecatedAttr()) {
    if (deprecatedAttr->getDomain().isUniversal())
      return true;
  }

  if (auto unavailableAttr = domainDecl->getUnavailableAttr()) {
    if (unavailableAttr->getDomain().isUniversal())
      return true;
  }

  return false;
}

bool AvailabilityDomain::isPermanentlyAlwaysEnabled() const {
  if (auto *customDomain = getCustomDomain()) {
    if (auto *domainDecl = customDomain->getDecl())
      return evaluateOrDefault(
          domainDecl->getASTContext().evaluator,
          IsCustomAvailabilityDomainPermanentlyEnabled{customDomain}, false);
  }
  return false;
}

void AvailabilityDomain::print(llvm::raw_ostream &os) const {
  os << getNameForAttributePrinting();
}

AvailabilityDomain AvailabilityDomain::copy(ASTContext &ctx) const {
  switch (getKind()) {
  case Kind::Universal:
  case Kind::SwiftLanguageMode:
  case Kind::StandaloneSwiftRuntime:
  case Kind::PackageDescription:
  case Kind::Embedded:
  case Kind::Platform:
    // These domain kinds aren't ASTContext dependent.
    return *this;
  case Kind::Custom:
    // To support this, the CustomAvailabilityDomain content would need to
    // be copied to the other context, allocating new storage if necessary.
    llvm::report_fatal_error("unsupported");
  }
}

bool StableAvailabilityDomainComparator::operator()(
    const AvailabilityDomain &lhs, const AvailabilityDomain &rhs) const {
  auto lhsKind = lhs.getKind();
  auto rhsKind = rhs.getKind();
  if (lhsKind != rhsKind)
    return lhsKind < rhsKind;

  switch (lhsKind) {
  case AvailabilityDomain::Kind::Universal:
  case AvailabilityDomain::Kind::SwiftLanguageMode:
  case AvailabilityDomain::Kind::StandaloneSwiftRuntime:
  case AvailabilityDomain::Kind::PackageDescription:
  case AvailabilityDomain::Kind::Embedded:
    return false;
  case AvailabilityDomain::Kind::Platform:
    return lhs.getPlatformKind() < rhs.getPlatformKind();
  case AvailabilityDomain::Kind::Custom: {
    auto lhsMod = lhs.getModule();
    auto rhsMod = rhs.getModule();
    if (lhsMod != rhsMod)
      return lhsMod->getName() < rhsMod->getName();

    return lhs.getNameForAttributePrinting() <
           rhs.getNameForAttributePrinting();
  }
  }
}

CustomAvailabilityDomain::CustomAvailabilityDomain(Identifier name, Kind kind,
                                                   ModuleDecl *mod,
                                                   ValueDecl *decl,
                                                   FuncDecl *predicateFunc)
    : name(name), mod(mod), decl(decl), predicateFunc(predicateFunc),
      kind(kind) {
  ASSERT(!name.empty());
  ASSERT(mod);
  if (predicateFunc)
    ASSERT(kind == Kind::Dynamic);
}

void CustomAvailabilityDomain::Profile(llvm::FoldingSetNodeID &ID,
                                       Identifier name, ModuleDecl *mod) {
  ID.AddPointer(name.getAsOpaquePointer());
  ID.AddPointer(mod);
}

std::optional<AvailabilityDomain>
AvailabilityDomainOrIdentifier::lookUpInDeclContext(
    SourceLoc loc, const DeclContext *declContext) const {
  DEBUG_ASSERT(isIdentifier());

  auto &ctx = declContext->getASTContext();
  auto &diags = ctx.Diags;
  std::optional<AvailabilityDomain> domain;
  auto identifier = getAsIdentifier().value();

  llvm::SmallVector<AvailabilityDomain> results;
  declContext->lookupAvailabilityDomains(identifier, results);
  if (results.size() > 0) {
    // FIXME: [availability] Diagnose ambiguity if necessary.
    domain = results.front();
  }

  bool hasCustomAvailability =
      ctx.LangOpts.hasFeature(Feature::CustomAvailability);
  bool hasSwiftRuntimeAvailability =
      ctx.LangOpts.hasFeature(Feature::SwiftRuntimeAvailability);

  if (!domain) {
    auto domainString = identifier.str();
    bool downgradeErrors =
        !hasCustomAvailability || declContext->isInSwiftinterface();
    if (auto suggestion = closestCorrectedPlatformString(domainString)) {
      diags
          .diagnose(loc, diag::availability_suggest_platform_name, identifier,
                    *suggestion)
          .limitBehaviorIf(downgradeErrors, DiagnosticBehavior::Warning)
          .fixItReplace(SourceRange(loc), *suggestion);
    } else {
      diags
          .diagnose(loc, diag::availability_unrecognized_platform_name,
                    identifier)
          .limitBehaviorIf(downgradeErrors, DiagnosticBehavior::Warning);
    }
    return std::nullopt;
  }

  // The remaining diagnostics are suppressed in .swiftinterfaces.
  if (declContext->isInSwiftinterface())
    return domain;

  // Use of custom domains requires the 'CustomAvailability' feature.
  if (domain->isCustom() && !hasCustomAvailability) {
    diags.diagnose(loc, diag::availability_domain_requires_feature, *domain,
                   "CustomAvailability");
    return std::nullopt;
  }

  if (domain->getPlatformKind() == PlatformKind::anyAppleOS &&
      !ctx.LangOpts.hasFeature(Feature::AnyAppleOSAvailability)) {
    diags.diagnose(loc, diag::availability_domain_requires_feature, *domain,
                   "AnyAppleOSAvailability");
    return std::nullopt;
  }

  // Use of the 'Swift' domain requires the 'SwiftRuntimeAvailability' feature.
  if (!hasSwiftRuntimeAvailability && domain->isStandaloneSwiftRuntime()) {
    diags.diagnose(loc, diag::availability_domain_requires_feature, *domain,
                   "SwiftRuntimeAvailability");
    return std::nullopt;
  }

  if (domain->isSwiftLanguageMode()) {
    // When the 'SwiftRuntimeAvailability' feature is enabled, the 'swift'
    // domain spelling is deprecated in favor of 'SwiftLanguageMode'.
    if (hasSwiftRuntimeAvailability && identifier.str() == "swift") {
      diags
          .diagnose(loc, diag::availability_domain_renamed, identifier,
                    "SwiftLanguageMode")
          .fixItReplace(SourceRange(loc), "SwiftLanguageMode");
    }

    // Use of the 'SwiftLanguageMode' domain spelling requires the
    // 'SwiftRuntimeAvailability' feature.
    if (!hasSwiftRuntimeAvailability &&
        identifier.str() == "SwiftLanguageMode") {
      diags.diagnose(loc, diag::availability_domain_requires_feature, *domain,
                     "SwiftRuntimeAvailability");
      return std::nullopt;
    }
  }

  return domain;
}

AvailabilityDomainOrIdentifier
AvailabilityDomainOrIdentifier::copy(ASTContext &ctx) const {
  if (auto identifier = getAsIdentifier())
    return ctx.getIdentifier(identifier->str());

  DEBUG_ASSERT(isDomain());
  return getAsDomain()->copy(ctx);
}

void AvailabilityDomainOrIdentifier::print(llvm::raw_ostream &os) const {
  if (auto identifier = getAsIdentifier())
    os << identifier->str();
  else
    getAsDomain()->print(os);
}
