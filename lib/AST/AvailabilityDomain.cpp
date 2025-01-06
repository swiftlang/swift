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

using namespace swift;

bool AvailabilityDomain::isActive(ASTContext &ctx) const {
  switch (kind) {
  case Kind::Universal:
  case Kind::SwiftLanguage:
  case Kind::PackageDescription:
    return true;
  case Kind::Platform:
    return isPlatformActive(getPlatformKind(), ctx.LangOpts);
  }
}

llvm::StringRef AvailabilityDomain::getNameForDiagnostics() const {
  switch (kind) {
  case Kind::Universal:
    return "";
  case Kind::Platform:
    return swift::prettyPlatformString(getPlatformKind());
  case Kind::SwiftLanguage:
    return "Swift";
  case Kind::PackageDescription:
    return "PackageDescription";
  }
}

llvm::StringRef AvailabilityDomain::getNameForAttributePrinting() const {
  switch (kind) {
  case Kind::Universal:
    return "*";
  case Kind::Platform:
    return swift::platformString(getPlatformKind());
  case Kind::SwiftLanguage:
    return "swift";
  case Kind::PackageDescription:
    return "_PackageDescription";
  }
}
