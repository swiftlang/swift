//===--- Substitution.cpp - Type substitutions ----------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  This file implements the Substitution class and operations on it.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/Substitution.h"

#include "swift/AST/ASTContext.h"
#include "swift/AST/Module.h"
#include "swift/AST/Types.h"
#include "llvm/ADT/DenseMap.h"

using namespace swift;

bool Substitution::operator==(const Substitution &other) const {
  // The archetypes may be missing, but we can compare them directly
  // because archetypes are always canonical.
  return Archetype == other.Archetype &&
    Replacement->getCanonicalType() == other.Replacement->getCanonicalType() &&
    Conformance.equals(other.Conformance);
}

static void
getSubstitutionMaps(GenericParamList *context,
                    ArrayRef<Substitution> subs,
                    TypeSubstitutionMap &typeMap,
                    ArchetypeConformanceMap &conformanceMap) {
  for (auto arch : context->getAllNestedArchetypes()) {
    auto sub = subs.front();
    subs = subs.slice(1);

    // Save the conformances from the substitution so that we can substitute
    // them into substitutions that map between archetypes.
    conformanceMap[arch] = sub.Conformance;

    if (arch->isPrimary())
      typeMap[arch] = sub.Replacement;
  }
  assert(subs.empty() && "did not use all substitutions?!");
}

Substitution Substitution::subst(Module *module,
                                 GenericParamList *context,
                                 ArrayRef<Substitution> subs) const {
  TypeSubstitutionMap subMap;
  ArchetypeConformanceMap conformanceMap;
  getSubstitutionMaps(context, subs,
                      subMap, conformanceMap);
  return subst(module, subs, subMap, conformanceMap);
}

Substitution Substitution::subst(Module *module,
                                 ArrayRef<Substitution> subs,
                                 TypeSubstitutionMap &subMap,
                                 ArchetypeConformanceMap &conformanceMap) const {
  // Substitute the replacement.
  Type substReplacement
    = Replacement.subst(module, subMap, /*ignoreMissing*/false, nullptr);
  assert(substReplacement && "substitution replacement failed");

  if (substReplacement->isEqual(Replacement))
    return *this;

  bool conformancesChanged = false;
  SmallVector<ProtocolConformance *, 4> substConformance;
  substConformance.reserve(Conformance.size());

  // When substituting a concrete type for an archetype, we need to fill in the
  // conformances.
  if (auto replacementArch = Replacement->getAs<ArchetypeType>()) {
    if (!substReplacement->is<ArchetypeType>()
        && !substReplacement->isAnyExistentialType()) {
      conformancesChanged = true;
      // Find the conformances mapped to the archetype.
      auto foundConformances = conformanceMap.find(replacementArch);
      assert(foundConformances != conformanceMap.end()
             && "no conformances for replaced archetype?!");

      // Get the conformances for the type that apply to the original
      // substituted archetype.
      for (auto proto : Archetype->getConformsTo()) {
        for (auto c : foundConformances->second) {
          if (c->getProtocol() == proto) {
            substConformance.push_back(c);
            goto found_conformance;
          }
          if (c->getProtocol()->inheritsFrom(proto)) {
            substConformance.push_back(c->getInheritedConformance(proto));
            goto found_conformance;
          }
        }
        assert(false && "did not find conformance for archetype requirement?!");
found_conformance:;
      }
    }
  } else {
    // If we substituted a concrete type for another, we need to substitute the
    // conformance to apply to the new type.
    for (auto c : Conformance) {
      auto substC = c->subst(module, substReplacement, subs,
                             subMap, conformanceMap);
      if (c != substC)
        conformancesChanged = true;
      substConformance.push_back(substC);
    }
  }

  ArrayRef<ProtocolConformance *> substConformanceRef;
  if (conformancesChanged)
    substConformanceRef = module->getASTContext().AllocateCopy(substConformance);
  else
    substConformanceRef = Conformance;

  assert(substReplacement->is<ArchetypeType>()
         || substConformanceRef.size() == Archetype->getConformsTo().size());

  return Substitution{Archetype, substReplacement, substConformanceRef};
}
