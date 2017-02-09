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

#include "swift/AST/ASTContext.h"
#include "swift/AST/AvailabilitySpec.h"
#include "llvm/Support/raw_ostream.h"

using namespace swift;

SourceRange AvailabilitySpec::getSourceRange() const {
  switch (getKind()) {
  case AvailabilitySpecKind::PlatformVersionConstraint:
    return cast<PlatformVersionConstraintAvailabilitySpec>(this)->getSourceRange();

 case AvailabilitySpecKind::LanguageVersionConstraint:
   return cast<LanguageVersionConstraintAvailabilitySpec>(this)->getSourceRange();

  case AvailabilitySpecKind::OtherPlatform:
    return cast<OtherPlatformAvailabilitySpec>(this)->getSourceRange();
  }
  llvm_unreachable("bad AvailabilitySpecKind");
}

// Only allow allocation of AvailabilitySpecs using the
// allocator in ASTContext.
void *AvailabilitySpec::operator new(size_t Bytes, ASTContext &C,
                                     unsigned Alignment) {
  return C.Allocate(Bytes, Alignment);
}


SourceRange PlatformVersionConstraintAvailabilitySpec::getSourceRange() const {
  return SourceRange(PlatformLoc, VersionSrcRange.End);
}

void PlatformVersionConstraintAvailabilitySpec::print(raw_ostream &OS,
                                              unsigned Indent) const {
  OS.indent(Indent) << '(' << "platform_version_constraint_availability_spec"
                    << " platform='" << platformString(getPlatform()) << "'"
                    << " version='" << getVersion() << "'"
                    << ')';
}

SourceRange LanguageVersionConstraintAvailabilitySpec::getSourceRange() const {
  return SourceRange(SwiftLoc, VersionSrcRange.End);
}

void LanguageVersionConstraintAvailabilitySpec::print(raw_ostream &OS,
                                                      unsigned Indent) const {
  OS.indent(Indent) << '(' << "language_version_constraint_availability_spec"
                    << " version='" << getVersion() << "'"
                    << ')';
}

void OtherPlatformAvailabilitySpec::print(raw_ostream &OS, unsigned Indent) const {
  OS.indent(Indent) << '(' << "other_constraint_availability_spec"
                    << " "
                    << ')';
}
