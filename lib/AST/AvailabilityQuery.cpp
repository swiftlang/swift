//===--- AvailabilityQuery.cpp - Swift Availability Queries ---------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/AST/AvailabilityQuery.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/PlatformKindUtils.h"
#include "swift/Basic/Platform.h"

using namespace swift;

AvailabilityQuery::AvailabilityQuery(
    AvailabilityDomain domain, ResultKind kind,
    const std::optional<AvailabilityRange> &primaryRange,
    const std::optional<AvailabilityRange> &variantRange)
    : domain(domain), primaryRange(primaryRange), variantRange(variantRange),
      kind(kind), unavailable(false) {
  // Check invariants.
  switch (domain.getKind()) {
  case AvailabilityDomain::Kind::SwiftLanguageMode:
  case AvailabilityDomain::Kind::PackageDescription:
  case AvailabilityDomain::Kind::Embedded:
    // These domains don't support queries at all.
    DEBUG_ASSERT(false);
    break;

  case AvailabilityDomain::Kind::Universal:
    // The universal domain can only support constant queries.
    DEBUG_ASSERT(kind != ResultKind::Dynamic);
    break;

  case AvailabilityDomain::Kind::StandaloneSwiftRuntime:
    // Dynamic Swift runtime queries take just a primary version argument.
    if (kind == ResultKind::Dynamic) {
      DEBUG_ASSERT(primaryRange);
      DEBUG_ASSERT(!variantRange);
    }
    break;

  case AvailabilityDomain::Kind::Platform:
    // Dynamic platform version queries must have either a primary version
    // argument or a variant version argument (or both).
    if (kind == ResultKind::Dynamic) {
      DEBUG_ASSERT(primaryRange || variantRange);
    }
    break;

  case AvailabilityDomain::Kind::Custom:
    // Custom availability domains do not support versioned queries at all yet.
    DEBUG_ASSERT(!primaryRange);
    DEBUG_ASSERT(!variantRange);

    // A valid custom domain object is required.
    auto customDomain = domain.getCustomDomain();
    ASSERT(customDomain);
    break;
  }
}

AvailabilityQuery AvailabilityQuery::forDomain(
    AvailabilityDomain domain,
    const std::optional<AvailabilityRange> &primaryRange,
    const std::optional<AvailabilityRange> &variantRange) {
  switch (domain.getKind()) {
  case AvailabilityDomain::Kind::Universal:
  case AvailabilityDomain::Kind::Embedded:
  case AvailabilityDomain::Kind::SwiftLanguageMode:
  case AvailabilityDomain::Kind::PackageDescription:
    // These domains don't support queries.
    llvm::report_fatal_error("unsupported domain");

  case AvailabilityDomain::Kind::StandaloneSwiftRuntime:
    return dynamic(domain, primaryRange, std::nullopt);

  case AvailabilityDomain::Kind::Platform:
    // Platform and Swift runtime checks are always dynamic. We can't perform an
    // analysis of whether the check would always succeed due to the deployment
    // target here because the answer may depend on inlining across module
    // boundaries.
    return dynamic(domain, primaryRange, variantRange);

  case AvailabilityDomain::Kind::Custom:
    auto customDomain = domain.getCustomDomain();
    ASSERT(customDomain);

    switch (customDomain->getKind()) {
    case CustomAvailabilityDomain::Kind::Enabled:
    case CustomAvailabilityDomain::Kind::AlwaysEnabled:
      return constant(domain, true);
    case CustomAvailabilityDomain::Kind::Disabled:
      return constant(domain, false);
    case CustomAvailabilityDomain::Kind::Dynamic:
      return dynamic(domain, primaryRange, variantRange);
    }
  }
}

static void unpackVersion(const llvm::VersionTuple &version,
                          llvm::SmallVectorImpl<unsigned> &arguments) {
  arguments.push_back(version.getMajor());
  arguments.push_back(version.getMinor().value_or(0));
  arguments.push_back(version.getSubminor().value_or(0));
}

static FuncDecl *
getOSVersionRangeCheck(const llvm::VersionTuple &version,
                       llvm::SmallVectorImpl<unsigned> &arguments,
                       ASTContext &ctx, bool forTargetVariant) {
  unpackVersion(version, arguments);
  return forTargetVariant ? ctx.getIsVariantOSVersionAtLeastDecl()
                          : ctx.getIsOSVersionAtLeastDecl();
}

static FuncDecl *getOSVersionOrVariantVersionRangeCheck(
    const llvm::VersionTuple &targetVersion,
    const llvm::VersionTuple &variantVersion,
    llvm::SmallVectorImpl<unsigned> &arguments, ASTContext &ctx) {
  unpackVersion(targetVersion, arguments);
  unpackVersion(variantVersion, arguments);
  return ctx.getIsOSVersionAtLeastOrVariantVersionAtLeast();
}

static FuncDecl *
getZipperedOSVersionRangeCheck(const AvailabilityQuery &query,
                               llvm::SmallVectorImpl<unsigned> &arguments,
                               ASTContext &ctx) {

  auto targetVersion = query.getPrimaryArgument();
  auto variantVersion = query.getVariantArgument();
  DEBUG_ASSERT(targetVersion || variantVersion);

  // We're building zippered, so we need to pass both macOS and iOS versions to
  // the runtime version range check. At run time that check will determine what
  // kind of process this code is loaded into. In a macOS process it will use
  // the macOS version; in an macCatalyst process it will use the iOS version.
  llvm::Triple targetTriple = ctx.LangOpts.Target;
  llvm::Triple variantTriple = *ctx.LangOpts.TargetVariant;

  // From perspective of the driver and most of the frontend, -target and
  // -target-variant are symmetric. That is, the user can pass either:
  //    -target x86_64-apple-macosx10.15 \
  //    -target-variant x86_64-apple-ios13.1-macabi
  // or:
  //    -target x86_64-apple-ios13.1-macabi \
  //    -target-variant x86_64-apple-macosx10.15
  //
  // However, the runtime availability-checking entry points need to compare
  // against an actual running OS version and so can't be symmetric. Here we
  // standardize on "target" means macOS version and "targetVariant" means iOS
  // version.
  if (tripleIsMacCatalystEnvironment(targetTriple)) {
    DEBUG_ASSERT(variantTriple.isMacOSX());
    // Normalize so that "variant" always means iOS version.
    std::swap(targetVersion, variantVersion);
    std::swap(targetTriple, variantTriple);
  }

  // The variant-only availability-checking entrypoint is not part of the
  // Swift 5.0 ABI. It is only available in macOS 10.15 and above.
  bool isVariantEntrypointAvailable = !targetTriple.isMacOSXVersionLT(10, 15);

  // If there is no check for the target but there is for the variant, then we
  // only need to emit code for the variant check.
  if (isVariantEntrypointAvailable && !targetVersion && variantVersion)
    return getOSVersionRangeCheck(*variantVersion, arguments, ctx,
                                  /*forVariant=*/true);

  // Similarly, if there is a check for the target but not for the target
  // variant then we only to emit code for the target check.
  if (targetVersion && !variantVersion)
    return getOSVersionRangeCheck(*targetVersion, arguments, ctx,
                                  /*forTargetVariant=*/false);

  if (!isVariantEntrypointAvailable || (targetVersion && variantVersion)) {

    // If the variant-only entrypoint isn't available (as is the case
    // pre-macOS 10.15) we need to use the zippered entrypoint (which is part of
    // the Swift 5.0 ABI) even when the macOS version is '*' (all). In this
    // case, use the minimum macOS deployment version from the target triple.
    // This ensures the check always passes on macOS.
    if (!isVariantEntrypointAvailable && !targetVersion) {
      DEBUG_ASSERT(targetTriple.isMacOSX());

      llvm::VersionTuple macosVersion;
      targetTriple.getMacOSXVersion(macosVersion);
      targetVersion = macosVersion;
    }

    return getOSVersionOrVariantVersionRangeCheck(
        *targetVersion, *variantVersion, arguments, ctx);
  }

  llvm_unreachable("Unhandled zippered configuration");
}

static FuncDecl *
getOSAvailabilityDeclAndArguments(const AvailabilityQuery &query,
                                  llvm::SmallVectorImpl<unsigned> &arguments,
                                  ASTContext &ctx) {
  if (ctx.LangOpts.TargetVariant)
    return getZipperedOSVersionRangeCheck(query, arguments, ctx);

  bool isMacCatalyst = tripleIsMacCatalystEnvironment(ctx.LangOpts.Target);
  return getOSVersionRangeCheck(query.getPrimaryArgument().value(), arguments,
                                ctx, isMacCatalyst);
}

FuncDecl *AvailabilityQuery::getDynamicQueryDeclAndArguments(
    llvm::SmallVectorImpl<unsigned> &arguments, ASTContext &ctx) const {
  auto domain = getDomain();
  switch (domain.getKind()) {
  case AvailabilityDomain::Kind::Universal:
  case AvailabilityDomain::Kind::SwiftLanguageMode:
  case AvailabilityDomain::Kind::PackageDescription:
  case AvailabilityDomain::Kind::Embedded:
    // These domains don't support dynamic queries.
    return nullptr;

  case AvailabilityDomain::Kind::StandaloneSwiftRuntime:
    unpackVersion(getPrimaryArgument().value(), arguments);
    return ctx.getIsSwiftRuntimeVersionAtLeast();
  case AvailabilityDomain::Kind::Platform:
    return getOSAvailabilityDeclAndArguments(*this, arguments, ctx);
  case AvailabilityDomain::Kind::Custom:
    return domain.getCustomDomain()->getPredicateFunc();
  }
}

/// Returns the version tuple that `arguments`, the three integer components of
/// a version that is passed to an OS version query function, describes.
static llvm::VersionTuple packVersion(llvm::ArrayRef<unsigned> arguments) {
  ASSERT(arguments.size() == 3);
  return llvm::VersionTuple(arguments[0], arguments[1], arguments[2]);
}

std::optional<AvailabilityQuery>
AvailabilityQuery::forOSVersionQueryCall(OSVersionQueryKind queryKind,
                                         llvm::ArrayRef<unsigned> arguments,
                                         const ASTContext &ctx) {
  std::optional<AvailabilityRange> osRange;
  std::optional<AvailabilityRange> variantOSRange;

  switch (queryKind) {
  case OSVersionQueryKind::IsOSVersionAtLeast:
    if (arguments.size() != 3)
      return std::nullopt;

    osRange = AvailabilityRange(packVersion(arguments));
    break;

  case OSVersionQueryKind::IsVariantOSVersionAtLeast:
    if (arguments.size() != 3)
      return std::nullopt;

    variantOSRange = AvailabilityRange(packVersion(arguments));
    break;

  case OSVersionQueryKind::IsOSVersionAtLeastOrVariantVersionAtLeast:
    if (arguments.size() != 6)
      return std::nullopt;

    osRange = AvailabilityRange(packVersion(arguments.slice(0, 3)));
    variantOSRange = AvailabilityRange(packVersion(arguments.slice(3, 3)));
    break;
  }

  // If necessary, swap the order of the osRange and variantOSRange arguments
  // to match the order expected by this compilation context. When encoded
  // as a function call, the non-macCatalyst version argument always comes
  // first, but if the -target specified for this compilation context is a
  // macCatalyst triple then the primary version range is expected to be the
  // macCatalyst one.
  bool targetIsMacCatalyst =
      tripleIsMacCatalystEnvironment(ctx.LangOpts.Target);
  auto primaryRange = targetIsMacCatalyst ? variantOSRange : osRange;
  auto variantRange = targetIsMacCatalyst ? osRange : variantOSRange;

  // The query belongs to the domain of the platform whose version it tests.
  // When it tests the versions of both platforms, the `-target` platform's
  // domain is the one to use.
  auto platform = primaryRange ? targetPlatform(ctx.LangOpts)
                               : targetVariantPlatform(ctx.LangOpts);
  if (!platform)
    return std::nullopt;

  return dynamic(AvailabilityDomain::forPlatform(*platform), primaryRange,
                 variantRange);
}

/// Returns the range of versions of the `-target-variant` platform that the
/// module being compiled deploys to, or `std::nullopt` if there is no
/// `-target-variant`.
static std::optional<AvailabilityRange>
deploymentRangeForTargetVariant(const ASTContext &ctx) {
  if (!ctx.LangOpts.TargetVariant)
    return std::nullopt;

  return AvailabilityRange(getVersionForTriple(*ctx.LangOpts.TargetVariant));
}

bool AvailabilityQuery::isAlwaysTrueForDeploymentTargets(
    const ASTContext &ctx) const {
  // A constant query doesn't depend on the deployment targets at all.
  if (auto constantResult = getConstantResult())
    return *constantResult;

  // An `#unavailable` query is true when the version test that it performs
  // fails. A deployment target is only a lower bound on the version of the OS
  // that runs the code, so it can never prove that a version test fails.
  if (isUnavailability())
    return false;

  switch (domain.getKind()) {
  case AvailabilityDomain::Kind::Universal:
  case AvailabilityDomain::Kind::SwiftLanguageMode:
  case AvailabilityDomain::Kind::PackageDescription:
  case AvailabilityDomain::Kind::Embedded:
    // These domains only support constant queries, which are handled above.
    return false;

  case AvailabilityDomain::Kind::StandaloneSwiftRuntime:
    // The version of the Swift runtime that the code runs against is not
    // analyzed here.
    return false;

  case AvailabilityDomain::Kind::Custom:
    // The enablement of a dynamic custom domain is decided at runtime.
    return false;

  case AvailabilityDomain::Kind::Platform:
    break;
  }

  // A version must be specified for a platform query.
  DEBUG_ASSERT(primaryRange || variantRange);

  if (primaryRange) {
    if (!AvailabilityRange::forDeploymentTarget(ctx).isContainedIn(
            *primaryRange))
      return false;
  }

  if (variantRange) {
    if (auto variantDeployment = deploymentRangeForTargetVariant(ctx)) {
      if (!variantDeployment->isContainedIn(*variantRange))
        return false;
    }
  }

  return true;
}
