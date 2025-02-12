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
#include "llvm/Support/raw_ostream.h"

using namespace swift;

llvm::VersionTuple AvailabilitySpec::getVersion() const {
  switch (getKind()) {
  case AvailabilitySpecKind::PlatformVersionConstraint: {
    auto spec = cast<PlatformVersionConstraintAvailabilitySpec>(this);
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
    return canonicalizePlatformVersion(spec->getPlatform(), Version);
  }
  case AvailabilitySpecKind::LanguageVersionConstraint:
  case AvailabilitySpecKind::PackageDescriptionVersionConstraint:
  case AvailabilitySpecKind::OtherPlatform:
    return Version;
  }
}

void PlatformVersionConstraintAvailabilitySpec::print(raw_ostream &OS,
                                              unsigned Indent) const {
  OS.indent(Indent) << '(' << "platform_version_constraint_availability_spec"
                    << " platform='" << platformString(getPlatform()) << "'"
                    << " version='" << getVersion() << "'"
                    << ')';
}

llvm::VersionTuple
PlatformVersionConstraintAvailabilitySpec::getRuntimeVersion() const {
  return Version;
}

void PlatformAgnosticVersionConstraintAvailabilitySpec::print(raw_ostream &OS,
                                                      unsigned Indent) const {
  OS.indent(Indent) << '('
                    << "platform_agnostic_version_constraint_availability_spec"
                    << " kind='"
                    << (getDomain()->isSwiftLanguage() ?
                         "swift" : "package_description")
                    << "'"
                    << " version='" << getVersion() << "'"
                    << ')';
}

void OtherPlatformAvailabilitySpec::print(raw_ostream &OS, unsigned Indent) const {
  OS.indent(Indent) << '(' << "other_constraint_availability_spec"
                    << " "
                    << ')';
}
