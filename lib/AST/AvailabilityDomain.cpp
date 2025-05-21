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
#include "swift/Basic/Version.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/Decl.h"
#include "llvm/ADT/StringSwitch.h"

using namespace swift;

CustomAvailabilityDomain::Kind
getCustomDomainKind(clang::FeatureAvailKind featureAvailKind) {
  switch (featureAvailKind) {
  case clang::FeatureAvailKind::None:
    llvm_unreachable("unexpected kind");
  case clang::FeatureAvailKind::Available:
    return CustomAvailabilityDomain::Kind::Enabled;
  case clang::FeatureAvailKind::Unavailable:
    return CustomAvailabilityDomain::Kind::Disabled;
  case clang::FeatureAvailKind::Dynamic:
    return CustomAvailabilityDomain::Kind::Dynamic;
  }
}

static const CustomAvailabilityDomain *
customDomainForClangDecl(Decl *decl, const ASTContext &ctx) {
  auto *clangDecl = decl->getClangDecl();
  ASSERT(clangDecl);

  auto featureInfo = clangDecl->getASTContext().getFeatureAvailInfo(
      const_cast<clang::Decl *>(clangDecl));

  // Ensure the decl actually represents an availability domain.
  if (featureInfo.first.empty())
    return nullptr;

  if (featureInfo.second.Kind == clang::FeatureAvailKind::None)
    return nullptr;

  return CustomAvailabilityDomain::get(
      featureInfo.first, getCustomDomainKind(featureInfo.second.Kind),
      decl->getModuleContext(), decl, ctx);
}

std::optional<AvailabilityDomain>
AvailabilityDomain::forCustom(Decl *decl, const ASTContext &ctx) {
  if (!decl)
    return std::nullopt;

  if (decl->hasClangNode()) {
    if (auto *customDomain = customDomainForClangDecl(decl, ctx))
      return AvailabilityDomain::forCustom(customDomain);
  } else {
    // FIXME: [availability] Handle Swift availability domains decls.
  }

  return std::nullopt;
}

std::optional<AvailabilityDomain>
AvailabilityDomain::builtinDomainForString(StringRef string,
                                           const DeclContext *declContext) {
  // This parameter is used in downstream forks, do not remove.
  (void)declContext;

  auto domain = llvm::StringSwitch<std::optional<AvailabilityDomain>>(string)
                    .Case("*", AvailabilityDomain::forUniversal())
                    .Case("swift", AvailabilityDomain::forSwiftLanguage())
                    .Case("_SwiftToolchain",
                          AvailabilityDomain::forSwiftToolchain())
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
  case Kind::SwiftLanguage:
  case Kind::SwiftToolchain:
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
  case Kind::SwiftLanguage:
  case Kind::PackageDescription:
    return true;
  case Kind::Platform:
    if (auto osType = tripleOSTypeForPlatform(getPlatformKind()))
      return llvm::Triple::isValidVersionForOS(*osType, version);
    return true;
  case Kind::Custom:
    return true;
  }
}

bool AvailabilityDomain::supportsContextRefinement() const {
  switch (getKind()) {
  case Kind::Universal:
  case Kind::Embedded:
  case Kind::SwiftLanguage:
  case Kind::SwiftToolchain:
  case Kind::PackageDescription:
    return false;
  case Kind::Platform:
  case Kind::Custom:
    return true;
  }
}

bool AvailabilityDomain::supportsQueries() const {
  switch (getKind()) {
  case Kind::Universal:
  case Kind::Embedded:
  case Kind::SwiftLanguage:
  case Kind::SwiftToolchain:
  case Kind::PackageDescription:
    return false;
  case Kind::Platform:
  case Kind::Custom:
    return true;
  }
}

bool AvailabilityDomain::isActive(const ASTContext &ctx) const {
  switch (getKind()) {
  case Kind::Universal:
  case Kind::SwiftLanguage:
  case Kind::SwiftToolchain:
  case Kind::PackageDescription:
  case Kind::Embedded:
    return true;
  case Kind::Platform:
    return isPlatformActive(getPlatformKind(), ctx.LangOpts);
  case Kind::Custom:
    // For now, custom domains are always active but it's conceivable that in
    // the future someone might want to define a domain but leave it inactive.
    return true;
  }
}

bool AvailabilityDomain::isActivePlatform(const ASTContext &ctx) const {
  if (!isPlatform())
    return false;

  return isActive(ctx);
}

static std::optional<llvm::VersionTuple>
getDeploymentVersion(const AvailabilityDomain &domain, const ASTContext &ctx) {
  switch (domain.getKind()) {
  case AvailabilityDomain::Kind::Universal:
  case AvailabilityDomain::Kind::Embedded:
  case AvailabilityDomain::Kind::Custom:
    return std::nullopt;
  case AvailabilityDomain::Kind::SwiftLanguage:
    return ctx.LangOpts.EffectiveLanguageVersion;
  case AvailabilityDomain::Kind::SwiftToolchain:
    return version::Version::getCurrentLanguageVersion();
  case AvailabilityDomain::Kind::PackageDescription:
    return ctx.LangOpts.PackageDescriptionVersion;
  case AvailabilityDomain::Kind::Platform:
    if (domain.isActive(ctx))
      return ctx.LangOpts.getMinPlatformVersion();
    return std::nullopt;
  }
}

std::optional<AvailabilityRange>
AvailabilityDomain::getDeploymentRange(const ASTContext &ctx) const {
  if (auto version = getDeploymentVersion(*this, ctx))
    return AvailabilityRange{*version};
  return std::nullopt;
}

llvm::StringRef AvailabilityDomain::getNameForDiagnostics() const {
  switch (getKind()) {
  case Kind::Universal:
    return "*";
  case Kind::SwiftLanguage:
    return "Swift";
  case Kind::SwiftToolchain:
    return "Swift Toolchain";
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
  case Kind::SwiftLanguage:
    return "swift";
  case Kind::SwiftToolchain:
    return "_SwiftToolchain";
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

Decl *AvailabilityDomain::getDecl() const {
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
  case Kind::SwiftLanguage:
  case Kind::SwiftToolchain:
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
  case AvailabilityDomain::Kind::SwiftLanguage:
  case AvailabilityDomain::Kind::SwiftToolchain:
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

const AvailabilityDomain
AvailabilityDomain::getRemappedDomain(const ASTContext &ctx,
                                      bool &didRemap) const {
  if (getPlatformKind() == PlatformKind::iOS &&
      isPlatformActive(PlatformKind::visionOS, ctx.LangOpts)) {
    didRemap = true;
    return AvailabilityDomain::forPlatform(PlatformKind::visionOS);
  }

  return *this;
}

void AvailabilityDomain::print(llvm::raw_ostream &os) const {
  os << getNameForAttributePrinting();
}

AvailabilityDomain AvailabilityDomain::copy(ASTContext &ctx) const {
  switch (getKind()) {
  case Kind::Universal:
  case Kind::SwiftLanguage:
  case Kind::SwiftToolchain:
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
  case AvailabilityDomain::Kind::SwiftLanguage:
  case AvailabilityDomain::Kind::SwiftToolchain:
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
                                                   ModuleDecl *mod, Decl *decl)
    : name(name), kind(kind), mod(mod), decl(decl) {
  ASSERT(!name.empty());
  ASSERT(mod);
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

  if (domain->isCustom() && !hasCustomAvailability &&
      !declContext->isInSwiftinterface()) {
    diags.diagnose(loc, diag::attr_availability_requires_custom_availability,
                   *domain);
    return std::nullopt;
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
