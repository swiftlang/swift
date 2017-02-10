//===--- Substitution.cpp - Type substitutions ----------------------------===//
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
//  This file implements the Substitution class and operations on it.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/Substitution.h"

#include "swift/AST/ASTContext.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/Module.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/SubstitutionMap.h"
#include "swift/AST/Types.h"
#include "llvm/ADT/DenseMap.h"

using namespace swift;

bool Substitution::operator==(const Substitution &other) const {
  // The archetypes may be missing, but we can compare them directly
  // because archetypes are always canonical.
  return
    Replacement->isEqual(other.Replacement) &&
    Conformance.equals(other.Conformance);
}

Substitution::Substitution(Type Replacement,
                           ArrayRef<ProtocolConformanceRef> Conformance)
  : Replacement(Replacement), Conformance(Conformance)
{
  // The replacement type must be materializable.
  assert(Replacement->isMaterializable()
         && "cannot substitute with a non-materializable type");
}

Substitution Substitution::subst(ModuleDecl *module,
                                 const SubstitutionMap &subMap) const {
  return subst(module, QuerySubstitutionMap{subMap},
               LookUpConformanceInSubstitutionMap(subMap));
}

Substitution Substitution::subst(ModuleDecl *module,
                                 TypeSubstitutionFn subs,
                                 LookupConformanceFn conformances) const {
  // Substitute the replacement.
  Type substReplacement = Replacement.subst(subs, conformances, None);
  assert(substReplacement && "substitution replacement failed");

  if (substReplacement->isEqual(Replacement))
    return *this;

  if (Conformance.empty()) {
    return {substReplacement, Conformance};
  }

  bool conformancesChanged = false;
  SmallVector<ProtocolConformanceRef, 4> substConformances;
  substConformances.reserve(Conformance.size());

  for (auto c : Conformance) {
    // If we have a concrete conformance, we need to substitute the
    // conformance to apply to the new type.
    if (c.isConcrete()) {
      auto substC = c.getConcrete()->subst(module, substReplacement,
                                           subs, conformances);
      substConformances.push_back(ProtocolConformanceRef(substC));
      if (c != substConformances.back())
        conformancesChanged = true;
      continue;
    }

    // Otherwise, we may need to fill in the conformance.
    ProtocolDecl *proto = c.getAbstract();
    Optional<ProtocolConformanceRef> conformance;

    // If the original type was an archetype, check the conformance map.
    if (Replacement->is<SubstitutableType>()
        || Replacement->is<DependentMemberType>()) {
      conformance = conformances(Replacement->getCanonicalType(),
                                 substReplacement,
                                 proto->getDeclaredType());
    }

    // If that didn't find anything, we can still synthesize AnyObject
    // conformances from thin air.  FIXME: gross.
    if (!conformance &&
        proto->isSpecificProtocol(KnownProtocolKind::AnyObject)) {
      auto archetype =
        dyn_cast<ArchetypeType>(substReplacement->getCanonicalType());
      // If classDecl is not nullptr, it is a concrete class.
      auto classDecl = substReplacement->getClassOrBoundGenericClass();
      if (!classDecl && archetype->getSuperclass()) {
        // Replacement type is an archetype with a superclass constraint.
        classDecl = archetype->getSuperclass()->getClassOrBoundGenericClass();
        assert(classDecl);
      }
      if (classDecl) {
        // Create a concrete conformance based on the conforming class.
        SmallVector<ProtocolConformance *, 1> lookupResults;
        classDecl->lookupConformance(classDecl->getParentModule(), proto,
                                     lookupResults);
        conformance = ProtocolConformanceRef(lookupResults.front());
      } else if (archetype && archetype->requiresClass()) {
        // Replacement type is an archetype with a class constraint.
        // Create an abstract conformance.
        conformance = ProtocolConformanceRef(proto);
      }
    }

    assert(conformance);
    if (conformance->isConcrete())
      conformancesChanged = true;
    substConformances.push_back(*conformance);
  }
  assert(substConformances.size() == Conformance.size());

  ArrayRef<ProtocolConformanceRef> substConfs;
  if (conformancesChanged)
    substConfs = Replacement->getASTContext().AllocateCopy(substConformances);
  else
    substConfs = Conformance;

  return Substitution{substReplacement, substConfs};
}
