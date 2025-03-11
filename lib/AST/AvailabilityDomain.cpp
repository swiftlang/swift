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
#include "llvm/ADT/StringSwitch.h"

using namespace swift;

std::optional<AvailabilityDomain>
AvailabilityDomain::forTargetPlatform(const ASTContext &ctx) {
  auto platform = swift::targetPlatform(ctx.LangOpts);
  if (platform == PlatformKind::none)
    return std::nullopt;

  return forPlatform(platform);
}

std::optional<AvailabilityDomain>
AvailabilityDomain::forTargetVariantPlatform(const ASTContext &ctx) {
  auto platform = swift::targetVariantPlatform(ctx.LangOpts);
  if (platform == PlatformKind::none)
    return std::nullopt;

  return forPlatform(platform);
}

std::optional<AvailabilityDomain>
AvailabilityDomain::builtinDomainForString(StringRef string,
                                           const DeclContext *declContext) {
  // This parameter is used in downstream forks, do not remove.
  (void)declContext;

  auto domain = llvm::StringSwitch<std::optional<AvailabilityDomain>>(string)
                    .Case("*", AvailabilityDomain::forUniversal())
                    .Case("swift", AvailabilityDomain::forSwiftLanguage())
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
  case Kind::PackageDescription:
  case Kind::Platform:
    return true;
  case Kind::Custom:
    // FIXME: [availability] Support versioned custom availability domains
    return false;
  }
}

bool AvailabilityDomain::supportsContextRefinement() const {
  switch (getKind()) {
  case Kind::Universal:
  case Kind::Embedded:
  case Kind::SwiftLanguage:
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

bool AvailabilityDomain::isActiveForTargetPlatform(
    const ASTContext &ctx) const {
  if (isPlatform()) {
    if (auto targetDomain = AvailabilityDomain::forTargetPlatform(ctx)) {
      auto compatibleDomain = targetDomain->getABICompatibilityDomain();
      return compatibleDomain.contains(*this);
    }
  }
  return false;
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

AvailabilityDomain AvailabilityDomain::getABICompatibilityDomain() const {
  if (!isPlatform())
    return *this;

  auto iOSDomain = AvailabilityDomain::forPlatform(PlatformKind::iOS);
  if (iOSDomain.contains(*this))
    return iOSDomain;

  if (auto basePlatform = basePlatformForExtensionPlatform(getPlatformKind()))
    return AvailabilityDomain::forPlatform(*basePlatform);

  return *this;
}

void AvailabilityDomain::print(llvm::raw_ostream &os) const {
  os << getNameForAttributePrinting();
}

AvailabilityDomain AvailabilityDomain::copy(ASTContext &ctx) const {
  switch (getKind()) {
  case Kind::Universal:
  case Kind::SwiftLanguage:
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

CustomAvailabilityDomain::CustomAvailabilityDomain(Identifier name,
                                                   ModuleDecl *mod, Kind kind)
    : name(name), kind(kind), mod(mod) {
  ASSERT(!name.empty());
  ASSERT(mod);
}

CustomAvailabilityDomain *
CustomAvailabilityDomain::create(const ASTContext &ctx, StringRef name,
                                 ModuleDecl *mod, Kind kind) {
  return new (ctx) CustomAvailabilityDomain(ctx.getIdentifier(name), mod, kind);
}

static std::optional<AvailabilityDomain>
getAvailabilityDomainForName(Identifier identifier,
                             const DeclContext *declContext) {
  if (auto builtinDomain = AvailabilityDomain::builtinDomainForString(
          identifier.str(), declContext))
    return builtinDomain;

  auto &ctx = declContext->getASTContext();
  if (auto customDomain =
          ctx.MainModule->getAvailabilityDomainForIdentifier(identifier))
    return customDomain;

  return std::nullopt;
}

std::optional<AvailabilityDomain>
AvailabilityDomainOrIdentifier::lookUpInDeclContext(
    SourceLoc loc, const DeclContext *declContext) const {
  DEBUG_ASSERT(isIdentifier());

  auto &ctx = declContext->getASTContext();
  auto &diags = ctx.Diags;
  std::optional<AvailabilityDomain> domain;
  auto identifier = getAsIdentifier().value();
  domain = getAvailabilityDomainForName(identifier, declContext);

  if (!domain) {
    auto domainString = identifier.str();
    if (auto suggestion = closestCorrectedPlatformString(domainString)) {
      diags
          .diagnose(loc, diag::availability_suggest_platform_name, identifier,
                    *suggestion)
          .fixItReplace(SourceRange(loc), *suggestion);
    } else {
      diags.diagnose(loc, diag::availability_unrecognized_platform_name,
                     identifier);
    }
    return std::nullopt;
  }

  if (domain->isCustom() &&
      !ctx.LangOpts.hasFeature(Feature::CustomAvailability) &&
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
