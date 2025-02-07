//===--- AvailabilityConstraint.cpp - Swift Availability Constraints ------===//
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

#include "swift/AST/AvailabilityConstraint.h"
#include "swift/AST/ASTContext.h"

using namespace swift;

PlatformKind AvailabilityConstraint::getPlatform() const {
  return getAttr().getPlatform();
}

std::optional<AvailabilityRange>
AvailabilityConstraint::getRequiredNewerAvailabilityRange(
    ASTContext &ctx) const {
  switch (getKind()) {
  case Kind::AlwaysUnavailable:
  case Kind::RequiresVersion:
  case Kind::Obsoleted:
    return std::nullopt;
  case Kind::IntroducedInNewerVersion:
    return getAttr().getIntroducedRange(ctx);
  }
}

bool AvailabilityConstraint::isConditionallySatisfiable() const {
  switch (getKind()) {
  case Kind::AlwaysUnavailable:
  case Kind::RequiresVersion:
  case Kind::Obsoleted:
    return false;
  case Kind::IntroducedInNewerVersion:
    return true;
  }
}

bool AvailabilityConstraint::isActiveForRuntimeQueries(ASTContext &ctx) const {
  if (getAttr().getPlatform() == PlatformKind::none)
    return true;

  return swift::isPlatformActive(getAttr().getPlatform(), ctx.LangOpts,
                                 /*forTargetVariant=*/false,
                                 /*forRuntimeQuery=*/true);
}
