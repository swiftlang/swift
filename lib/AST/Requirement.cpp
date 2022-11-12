//===--- Requirement.cpp - Generic requirement ----------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file implements the Requirement class.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/Requirement.h"
#include "swift/AST/Decl.h"
#include "swift/AST/GenericSignature.h"
#include "swift/AST/Module.h"
#include "swift/AST/Types.h"

using namespace swift;

bool Requirement::hasError() const {
  if (getFirstType()->hasError())
    return true;

  if (getKind() != RequirementKind::Layout && getSecondType()->hasError())
    return true;

  return false;
}

bool Requirement::isCanonical() const {
  if (!getFirstType()->isCanonical())
    return false;

  switch (getKind()) {
  case RequirementKind::SameShape:
  case RequirementKind::Conformance:
  case RequirementKind::SameType:
  case RequirementKind::Superclass:
    if (!getSecondType()->isCanonical())
      return false;
    break;

  case RequirementKind::Layout:
    break;
  }

  return true;
}

/// Get the canonical form of this requirement.
Requirement Requirement::getCanonical() const {
  Type firstType = getFirstType()->getCanonicalType();

  switch (getKind()) {
  case RequirementKind::SameShape:
  case RequirementKind::Conformance:
  case RequirementKind::SameType:
  case RequirementKind::Superclass: {
    Type secondType = getSecondType()->getCanonicalType();
    return Requirement(getKind(), firstType, secondType);
  }

  case RequirementKind::Layout:
    return Requirement(getKind(), firstType, getLayoutConstraint());
  }
  llvm_unreachable("Unhandled RequirementKind in switch");
}

ProtocolDecl *Requirement::getProtocolDecl() const {
  assert(getKind() == RequirementKind::Conformance);
  return getSecondType()->castTo<ProtocolType>()->getDecl();
}

bool
Requirement::isSatisfied(ArrayRef<Requirement> &conditionalRequirements,
                         bool allowMissing) const {
  switch (getKind()) {
  case RequirementKind::Conformance: {
    auto *proto = getProtocolDecl();
    auto *module = proto->getParentModule();
    auto conformance = module->lookupConformance(
        getFirstType(), proto, allowMissing);
    if (!conformance)
      return false;

    conditionalRequirements = conformance.getConditionalRequirements();
    return true;
  }

  case RequirementKind::Layout: {
    if (auto *archetypeType = getFirstType()->getAs<ArchetypeType>()) {
      auto layout = archetypeType->getLayoutConstraint();
      return (layout && layout.merge(getLayoutConstraint()));
    }

    if (getLayoutConstraint()->isClass())
      return getFirstType()->satisfiesClassConstraint();

    // TODO: Statically check other layout constraints, once they can
    // be spelled in Swift.
    return true;
  }

  case RequirementKind::Superclass:
    return getSecondType()->isExactSuperclassOf(getFirstType());

  case RequirementKind::SameType:
    return getFirstType()->isEqual(getSecondType());

  case RequirementKind::SameShape:
    return (getFirstType()->getReducedShape() ==
            getSecondType()->getReducedShape());
  }

  llvm_unreachable("Bad requirement kind");
}

bool Requirement::canBeSatisfied() const {
  switch (getKind()) {
  case RequirementKind::SameShape:
    llvm_unreachable("Same-shape requirements not supported here");

  case RequirementKind::Conformance:
    return getFirstType()->is<ArchetypeType>();

  case RequirementKind::Layout: {
    if (auto *archetypeType = getFirstType()->getAs<ArchetypeType>()) {
      auto layout = archetypeType->getLayoutConstraint();
      return (!layout || layout.merge(getLayoutConstraint()));
    }

    return false;
  }

  case RequirementKind::Superclass:
    return (getFirstType()->isBindableTo(getSecondType()) ||
            getSecondType()->isBindableTo(getFirstType()));

  case RequirementKind::SameType:
    return (getFirstType()->isBindableTo(getSecondType()) ||
            getSecondType()->isBindableTo(getFirstType()));
  }

  llvm_unreachable("Bad requirement kind");
}

/// Determine the canonical ordering of requirements.
static unsigned getRequirementKindOrder(RequirementKind kind) {
  switch (kind) {
  case RequirementKind::SameShape: return 4;
  case RequirementKind::Conformance: return 2;
  case RequirementKind::Superclass: return 0;
  case RequirementKind::SameType: return 3;
  case RequirementKind::Layout: return 1;
  }
  llvm_unreachable("unhandled kind");
}

/// Linear order on requirements in a generic signature.
int Requirement::compare(const Requirement &other) const {
  int compareLHS =
    compareDependentTypes(getFirstType(), other.getFirstType());

  if (compareLHS != 0)
    return compareLHS;

  int compareKind = (getRequirementKindOrder(getKind()) -
                     getRequirementKindOrder(other.getKind()));

  if (compareKind != 0)
    return compareKind;

  // We should only have multiple conformance requirements.
  if (getKind() != RequirementKind::Conformance) {
    llvm::errs() << "Unordered generic requirements\n";
    llvm::errs() << "LHS: "; dump(llvm::errs()); llvm::errs() << "\n";
    llvm::errs() << "RHS: "; other.dump(llvm::errs()); llvm::errs() << "\n";
    abort();
  }

  int compareProtos =
    TypeDecl::compare(getProtocolDecl(), other.getProtocolDecl());
  assert(compareProtos != 0 && "Duplicate conformance requirements");

  return compareProtos;
}