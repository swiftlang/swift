//===--- TypeCheckGeneric.cpp - Generics ----------------------------------===//
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
// This file implements support for generics.
//
//===----------------------------------------------------------------------===//
#include "TypeChecker.h"
using namespace swift;

SpecializeExpr *
TypeChecker::buildSpecializeExpr(Expr *Sub, Type Ty,
                                 const TypeSubstitutionMap &Substitutions,
                                 const ConformanceMap &Conformances,
                                 bool OnlyInnermostParams) {
  auto polyFn = Sub->getType()->castTo<PolymorphicFunctionType>();
  return new (Context) SpecializeExpr(Sub, Ty,
                         encodeSubstitutions(&polyFn->getGenericParams(),
                                             Substitutions,
                                             Conformances,
                                             OnlyInnermostParams));
}

ArrayRef<Substitution>
TypeChecker::encodeSubstitutions(const GenericParamList *GenericParams,
                                 const TypeSubstitutionMap &Substitutions,
                                 const ConformanceMap &Conformances,
                                 bool OnlyInnermostParams) {
  // Collect all of the archetypes.
  SmallVector<ArchetypeType *, 2> allArchetypesList;
  ArrayRef<ArchetypeType *> allArchetypes = GenericParams->getAllArchetypes();
  if (GenericParams->getOuterParameters() && !OnlyInnermostParams) {
    SmallVector<const GenericParamList *, 2> allGenericParams;
    unsigned numArchetypes = 0;
    for (; GenericParams; GenericParams = GenericParams->getOuterParameters()) {
      allGenericParams.push_back(GenericParams);
      numArchetypes += GenericParams->getAllArchetypes().size();
    }
    allArchetypesList.reserve(numArchetypes);
    for (auto gp = allGenericParams.rbegin(), gpEnd = allGenericParams.rend();
         gp != gpEnd; ++gp) {
      allArchetypesList.append((*gp)->getAllArchetypes().begin(),
                               (*gp)->getAllArchetypes().end());
    }
    allArchetypes = allArchetypesList;
  }

  SmallVector<SpecializeExpr::Substitution, 2> storedSubstitutions;
  storedSubstitutions.resize(allArchetypes.size());
  unsigned index = 0;
  for (auto archetype : allArchetypes) {
    // Figure out the key into the maps we were given.
    SubstitutableType *key = archetype;
    assert(Substitutions.count(key) && "Missing substitution information");
    assert(Conformances.count(key) && "Missing conformance information");

    // Record this substitution.
    storedSubstitutions[index].Archetype = archetype;
    storedSubstitutions[index].Replacement
      = Substitutions.find(key)->second;
    storedSubstitutions[index].Conformance
      = Context.AllocateCopy(Conformances.find(key)->second);

    ++index;
  }

  return Context.AllocateCopy(storedSubstitutions);
}

bool TypeChecker::checkSubstitutions(TypeSubstitutionMap &Substitutions,
                                     ConformanceMap &Conformance,
                                     SourceLoc ComplainLoc,
                                     TypeSubstitutionMap *RecordSubstitutions) {
  llvm::SmallPtrSet<ArchetypeType *, 8> knownArchetypes;
  SmallVector<ArchetypeType *, 8> archetypeStack;

  // Find all of the primary archetypes and enter them into the archetype
  // stack.
  for (const auto &sub : Substitutions) {
    auto archetype = sub.first->getArchetype();
    if (archetype->isPrimary() && knownArchetypes.insert(archetype))
      archetypeStack.push_back(archetype);
  }

  // Check that each of the replacements for the archetypes conform
  // to the required protocols.
  while (!archetypeStack.empty()) {
    // Grab the last archetype on the stack.
    auto archetype = archetypeStack.back();
    archetypeStack.pop_back();

    // Substitute our deductions into the archetype type to produce the
    // concrete type we need to evaluate.
    Type T = substType(archetype, Substitutions);
    if (!T)
      return true;

    // If we were asked to record the substitution, do so now.
    if (RecordSubstitutions)
      (*RecordSubstitutions)[archetype] = T;

    // If the archetype has a superclass requirement, check that now.
    if (auto superclass = archetype->getSuperclass()) {
      if (!isSubtypeOf(T, superclass))
        return true;
    }

    SmallVectorImpl<ProtocolConformance *> &Conformances
      = Conformance[archetype];
    if (Conformances.empty()) {
      for (auto Proto : archetype->getConformsTo()) {
        ProtocolConformance *Conformance = nullptr;
        if (conformsToProtocol(T, Proto, &Conformance, ComplainLoc)) {
          Conformances.push_back(Conformance);
        } else {
          // FIXME: Diagnose, if requested.
          return true;
        }
      }
    }

    validateTypeSimple(T);

    // Add any nested archetypes to the archetype stack.
    for (auto Nested : archetype->getNestedTypes()) {
      if (knownArchetypes.insert(Nested.second))
        archetypeStack.push_back(Nested.second);
    }
  }

  // FIXME: Check same-type constraints!
  
  return false; 
}

Type TypeChecker::getWitnessType(Type type, ProtocolDecl *protocol,
                                 ProtocolConformance *conformance,
                                 Identifier name,
                                 Diag<> brokenProtocolDiag) {
  // For an archetype, retrieve the nested type with the appropriate name.
  // There are no conformance tables.
  if (auto archetype = type->getAs<ArchetypeType>()) {
    return archetype->getNestedType(name);
  }

  // Find the named requirement.
  TypeAliasDecl *requirement = nullptr;
  for (auto member : protocol->getMembers()) {
    auto td = dyn_cast<TypeAliasDecl>(member);
    if (!td || td->getName().empty())
      continue;

    if (td->getName() == name) {
      requirement = td;
      break;
    }
  }

  if (!requirement) {
    diagnose(protocol->getLoc(), brokenProtocolDiag);
    return nullptr;
  }

  assert(conformance && "Missing conformance information");
  auto reqType = requirement->getDeclaredType()->castTo<SubstitutableType>();
  assert(conformance->TypeMapping.count(reqType) && "Missing conformance");
  // FIXME: substMemberTypeWithBase when we deal with generic conformance.
  return conformance->TypeMapping[reqType];
}
