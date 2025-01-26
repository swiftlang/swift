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
#include "llvm/ADT/StringSwitch.h"

using namespace swift;

std::optional<AvailabilityDomain>
AvailabilityDomain::builtinDomainForString(StringRef string) {
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

bool AvailabilityDomain::isActive(const ASTContext &ctx) const {
  switch (getKind()) {
  case Kind::Universal:
  case Kind::SwiftLanguage:
  case Kind::PackageDescription:
  case Kind::Embedded:
    return true;
  case Kind::Platform:
    return isPlatformActive(getPlatformKind(), ctx.LangOpts);
  }
}

llvm::StringRef AvailabilityDomain::getNameForDiagnostics() const {
  switch (getKind()) {
  case Kind::Universal:
    return "";
  case Kind::SwiftLanguage:
    return "Swift";
  case Kind::PackageDescription:
    return "PackageDescription";
  case Kind::Embedded:
    return "Embedded Swift";
  case Kind::Platform:
    return swift::prettyPlatformString(getPlatformKind());
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
  }
}

bool AvailabilityDomain::contains(const AvailabilityDomain &other) const {
  // FIXME: [availability] This currently implements something closer to a
  // total ordering instead of the more flexible partial ordering that it
  // would ideally represent. Until AvailabilityContext supports tracking
  // multiple unavailable domains simultaneously, a stricter ordering is
  // necessary to support source compatibility.
  switch (getKind()) {
  case Kind::Universal:
    return true;
  case Kind::SwiftLanguage:
    return !other.isUniversal();
  case Kind::PackageDescription:
  case Kind::Embedded:
    return !other.isUniversal() && !other.isSwiftLanguage();
  case Kind::Platform:
    if (getPlatformKind() == other.getPlatformKind())
      return true;
    return inheritsAvailabilityFromPlatform(other.getPlatformKind(),
                                            getPlatformKind());
  }
}
