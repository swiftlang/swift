//===--- AvailabilitySpec.cpp - Swift Availability Query ASTs -------------===//
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
// This file implements the availability specification AST classes.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/AvailabilitySpec.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/AvailabilityDomain.h"
#include "swift/AST/DiagnosticsParse.h"
#include "llvm/Support/raw_ostream.h"

using namespace swift;

AvailabilitySpec *AvailabilitySpec::createWildcard(ASTContext &ctx,
                                                   SourceLoc starLoc) {
  return new (ctx) AvailabilitySpec(AvailabilityDomain::forUniversal(), starLoc,
                                    /*Version=*/{},
                                    /*VersionStartLoc=*/{});
}

AvailabilitySpec *AvailabilitySpec::createForDomain(ASTContext &ctx,
                                                    AvailabilityDomain domain,
                                                    SourceLoc loc,
                                                    llvm::VersionTuple version,
                                                    SourceRange versionRange) {
  DEBUG_ASSERT(!version.empty());
  return new (ctx) AvailabilitySpec(domain, SourceRange(loc, versionRange.End),
                                    version, versionRange.Start);
}

AvailabilitySpec *AvailabilitySpec::createForDomainIdentifier(
    ASTContext &ctx, Identifier domainIdentifier, SourceLoc loc,
    llvm::VersionTuple version, SourceRange versionRange) {
  DEBUG_ASSERT(!version.empty());
  return new (ctx)
      AvailabilitySpec(domainIdentifier, SourceRange(loc, versionRange.End),
                       version, versionRange.Start);
}

AvailabilitySpec *AvailabilitySpec::clone(ASTContext &ctx) const {
  return new (ctx) AvailabilitySpec(getDomainOrIdentifier().copy(ctx), SrcRange,
                                    Version, VersionStartLoc);
}

void AvailabilitySpec::print(llvm::raw_ostream &os) const {
  getDomainOrIdentifier().print(os);

  if (!getRawVersion().empty())
    os << " " << getRawVersion().getAsString();
}

std::optional<SemanticAvailabilitySpec>
AvailabilitySpec::getSemanticAvailabilitySpec(
    const DeclContext *declContext) const {
  AvailabilitySpec *mutableThis = const_cast<AvailabilitySpec *>(this);
  auto domain = mutableThis->DomainOrIdentifier.resolveInDeclContext(
      getStartLoc(), declContext);

  if (domain)
    return SemanticAvailabilitySpec(this);
  return std::nullopt;
}

llvm::VersionTuple SemanticAvailabilitySpec::getVersion() const {
  // For macOS Big Sur, we canonicalize 10.16 to 11.0 for compile-time
  // checking since clang canonicalizes availability markup. However, to
  // support Beta versions of macOS Big Sur where the OS
  // reports 10.16 at run time, we need to compare against 10.16,
  //
  // This means for:
  //
  // if #available(macOS 10.16, *) { ... }
  //
  // we need to store the uncanonicalized version for codegen and canonicalize
  // it as necessary for compile-time checks.
  return canonicalizePlatformVersion(getDomain().getPlatformKind(),
                                     spec->getRawVersion());
}

std::optional<SemanticAvailabilitySpec>
SemanticAvailabilitySpecs::Filter::operator()(
    const AvailabilitySpec *spec) const {
  if (auto semanticSpec = spec->getSemanticAvailabilitySpec(declContext))
    return semanticSpec;
  return std::nullopt;
}
