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
#include "GenericTypeResolver.h"
#include "swift/AST/ArchetypeBuilder.h"

using namespace swift;

Type GenericTypeToArchetypeResolver::resolveGenericTypeParamType(
                                       GenericTypeParamType *gp) {
  auto gpDecl = gp->getDecl();
  assert(gpDecl && "Missing generic parameter declaration");

  auto archetype = gpDecl->getArchetype();
  assert(archetype && "Missing archetype for generic parameter");

  return archetype;
}


Type GenericTypeToArchetypeResolver::resolveDependentMemberType(
                                  Type baseTy,
                                  SourceRange baseRange,
                                  Identifier name,
                                  SourceLoc nameLoc) {
  llvm_unreachable("Dependent type after archetype substitution");
}

Type PartialGenericTypeToArchetypeResolver::resolveGenericTypeParamType(
                                              GenericTypeParamType *gp) {
  auto gpDecl = gp->getDecl();
  if (!gpDecl)
    return Type(gp);

  auto archetype = gpDecl->getArchetype();
  if (!archetype)
    return Type(gp);

  return archetype;
}


Type PartialGenericTypeToArchetypeResolver::resolveDependentMemberType(
                                              Type baseTy,
                                              SourceRange baseRange,
                                              Identifier name,
                                              SourceLoc nameLoc) {
  // We don't have enough information to find the associated type.
  return DependentMemberType::get(baseTy, name, TC.Context);
}

Type CompleteGenericTypeResolver::resolveGenericTypeParamType(
                                              GenericTypeParamType *gp) {
  // If there is an archetype corresponding to this generic parameter, use it.
  // FIXME: This is a hack for nested generics. It should go away eventually.
  if (auto gpDecl = gp->getDecl()) {
    if (auto archetype = gpDecl->getArchetype()) {
      return archetype;
    }
  }

  // Retrieve the potential archetype corresponding to this generic type
  // parameter.
  // FIXME: When generic parameters can map down to specific types, do so
  // here.
  auto pa = Builder.resolveType(gp);
  assert(pa && "Missing archetype for generic type parameter");

  return gp;
}


Type CompleteGenericTypeResolver::resolveDependentMemberType(
                                    Type baseTy,
                                    SourceRange baseRange,
                                    Identifier name,
                                    SourceLoc nameLoc) {
  // Resolve the base to a potential archetype.
  auto basePA = Builder.resolveType(baseTy);
  assert(basePA && "Missing potential archetype for base");
  basePA = basePA->getRepresentative();

  // Find the associated type declaration for this name.
  for (auto proto : basePA->getConformsTo()) {
    SmallVector<ValueDecl *, 2> decls;
    if (TC.TU.lookupQualified(proto->getDeclaredType(), name,
                              NL_VisitSupertypes, nullptr, decls)) {
      for (auto decl : decls) {
        // Note: once we find any associated type, we have our answer, because
        // the archetype builder is supposed to ensure that all associated
        // types with the same name are equivalent.
        auto assocType = dyn_cast<AssociatedTypeDecl>(decl);
        if (assocType) {
          return DependentMemberType::get(baseTy, assocType, TC.Context);
        }
      }
    }
  }

  // Check whether the name can be found in the superclass.
  if (auto superclassTy = basePA->getSuperclass()) {
    if (auto lookup = TC.lookupMemberType(superclassTy, name)) {
      if (lookup.isAmbiguous()) {
        TC.diagnoseAmbiguousMemberType(baseTy, baseRange, name, nameLoc,
                                       lookup);
        return ErrorType::get(TC.Context);
      }

      // FIXME: Record (via type sugar) that this was referenced via baseTy.
      return lookup.front().second;
    }
  }

  // Complain that there is no suitable type.
  TC.diagnose(nameLoc, diag::invalid_member_type, name, baseTy)
    .highlight(baseRange);
  return ErrorType::get(TC.Context);
}

/// Create a fresh archetype builder.
static ArchetypeBuilder createArchetypeBuilder(TypeChecker &TC) {
  return ArchetypeBuilder(
           TC.TU, TC.Diags,
           [&](ProtocolDecl *protocol) -> ArrayRef<ProtocolDecl *> {
             return TC.getDirectConformsTo(protocol);
           },
           [&](AbstractTypeParamDecl *assocType) -> ArrayRef<ProtocolDecl *> {
             TC.checkInheritanceClause(assocType);
             return assocType->getProtocols();
           });
}

/// Check the generic parameters in the given generic parameter list (and its
/// parent generic parameter lists) according to the given resolver.
static void checkGenericParameters(TypeChecker &tc, ArchetypeBuilder *builder,
                                   GenericParamList *genericParams,
                                   GenericTypeResolver &resolver,
                                   bool innermost = true) {
  // If there are outer generic parameters, visit them (so that the archetype
  // builder knows about them) but don't modify them in any way.
  // FIXME: Rather than crawling the generic outer parameters, it would be far
  // better to simply grab the generic parameters and requirements for the
  // outer context directly. However, we don't currently store those anywhere.
  if (auto outerGenericParams = genericParams->getOuterParameters())
    checkGenericParameters(tc, builder, outerGenericParams, resolver,
                           /*innermost=*/false);

  // Visit each of the generic parameters.
  unsigned depth = genericParams->getDepth();
  unsigned index = 0;
  for (auto param : *genericParams) {
    auto typeParam = param.getAsTypeParam();

    // Check the generic type parameter.
    if (innermost) {
      // Set the depth of this type parameter.
      typeParam->setDepth(depth);

      // Check the inheritance clause of this type parameter.
      tc.checkInheritanceClause(typeParam, &resolver);
    }

    if (builder) {
      // Add the generic parameter to the builder.
      builder->addGenericParameter(typeParam, index++);

      // Infer requirements from the inherited types.
      // FIXME: This doesn't actually do what we want for outer generic
      // parameters, because they've been resolved to archetypes too eagerly.
      for (const auto &inherited : typeParam->getInherited()) {
        builder->inferRequirements(inherited.getTypeRepr());
      }
    }
  }

  // Visit each of the requirements, adding them to the builder.
  // Add the requirements clause to the builder, validating the types in
  // the requirements clause along the way.
  for (auto &req : genericParams->getRequirements()) {
    if (req.isInvalid())
      continue;

    switch (req.getKind()) {
    case RequirementKind::Conformance: {
      // Validate the types.
      if (tc.validateType(req.getSubjectLoc(),
                          /*allowUnboundGenerics=*/false,
                          &resolver)) {
        req.setInvalid();
        continue;
      }

      if (tc.validateType(req.getConstraintLoc(),
                          /*allowUnboundGenerics=*/false,
                          &resolver)) {
        req.setInvalid();
        continue;
      }

      // FIXME: Feels too early to perform this check.
      if (!req.getConstraint()->isExistentialType() &&
          !req.getConstraint()->getClassOrBoundGenericClass()) {
        tc.diagnose(genericParams->getWhereLoc(),
                    diag::requires_conformance_nonprotocol,
                    req.getSubjectLoc(), req.getConstraintLoc());
        req.getConstraintLoc().setInvalidType(tc.Context);
        req.setInvalid();
        continue;
      }

      // DynamicLookup cannot be used in a generic constraint.
      if (auto protoTy = req.getConstraint()->getAs<ProtocolType>()) {
        if (protoTy->getDecl()->isSpecificProtocol(
                                  KnownProtocolKind::DynamicLookup)) {
          tc.diagnose(req.getConstraintLoc().getSourceRange().Start,
                      diag::dynamic_lookup_conformance);
          continue;
        }
      }
      break;
    }

    case RequirementKind::SameType:
      if (tc.validateType(req.getFirstTypeLoc(),
                          /*allowUnboundGenerics=*/false,
                          &resolver)) {
        req.setInvalid();
        continue;
      }

      if (tc.validateType(req.getSecondTypeLoc(),
                          /*allowUnboundGenerics=*/false,
                          &resolver)) {
        req.setInvalid();
        continue;
      }
      
      break;
    }
    
    if (builder && builder->addRequirement(req))
      req.setInvalid();
  }
}

/// Collect all of the generic parameter types at every level in the generic
/// parameter list.
static void collectGenericParamTypes(
              GenericParamList *genericParams,
              SmallVectorImpl<GenericTypeParamType *> &allParams) {
  if (!genericParams)
    return;

  // Collect outer generic parameters first.
  collectGenericParamTypes(genericParams->getOuterParameters(), allParams);

  // Add our parameters.
  for (auto param : *genericParams) {
    allParams.push_back(param.getAsTypeParam()->getDeclaredType()
                        ->castTo<GenericTypeParamType>());
  }
}

namespace {
  /// \brief Function object that orders potential archetypes by name.
  struct OrderPotentialArchetypeByName {
    using PotentialArchetype = ArchetypeBuilder::PotentialArchetype;

    bool operator()(std::pair<Identifier, PotentialArchetype *> X,
                    std::pair<Identifier, PotentialArchetype *> Y) const {
      return X.first.str() < Y.second->getName().str();
    }

    bool operator()(std::pair<Identifier, PotentialArchetype *> X,
                    Identifier Y) const {
      return X.first.str() < Y.str();
    }

    bool operator()(Identifier X,
                    std::pair<Identifier, PotentialArchetype *> Y) const {
      return X.str() < Y.first.str();
    }

    bool operator()(Identifier X, Identifier Y) const {
      return X.str() < Y.str();
    }
  };
}

/// Add the requirements for the given potential archetype and its nested
/// potential archetypes to the set of requirements.
static void
addRequirements(
    TranslationUnit &tu, Type type,
    ArchetypeBuilder::PotentialArchetype *pa,
    llvm::SmallPtrSet<ArchetypeBuilder::PotentialArchetype *, 16> &knownPAs,
    SmallVectorImpl<Requirement> &requirements) {
  using PotentialArchetype = ArchetypeBuilder::PotentialArchetype;

  // Add superclass requirement, if needed.
  if (auto superclass = pa->getSuperclass()) {
    // FIXME: Distinguish superclass from conformance?
    // FIXME: What if the superclass type involves a type parameter?
    requirements.push_back(Requirement(RequirementKind::Conformance,
                                       type, superclass));
  }

  // Add conformance requirements.
  for (auto proto : pa->getConformsTo()) {
    requirements.push_back(Requirement(RequirementKind::Conformance,
                                       type, proto->getDeclaredType()));
  }

  // Collect the nested types, sorted by name.
  // FIXME: Could collect these from the conformance requirements, above.
  SmallVector<std::pair<Identifier, PotentialArchetype*>, 16>
  nestedTypes(pa->getNestedTypes().begin(), pa->getNestedTypes().end());
  std::sort(nestedTypes.begin(), nestedTypes.end(),
            OrderPotentialArchetypeByName());

  // Add requirements for associated types.
  for (const auto &nested : nestedTypes) {
    auto rep = nested.second->getRepresentative();
    if (knownPAs.insert(rep)) {
      // Form the dependent type that refers to this archetype.
      auto assocType = pa->getAssociatedType(tu, nested.first);
      if (!assocType)
        continue; // FIXME: If we do this late enough, there will be no failure.

      auto nestedType = DependentMemberType::get(type, assocType,
                                                 tu.getASTContext());
      addRequirements(tu, nestedType, rep, knownPAs, requirements);
    }
  }
}

/// Collect the set of requirements placed on the given generic parameters and
/// their associated types.
static void collectRequirements(ArchetypeBuilder &builder,
                                ArrayRef<GenericTypeParamType *> params,
                                SmallVectorImpl<Requirement> &requirements) {
  // Find the "primary" potential archetypes, from which we'll collect all
  // of the requirements.
  llvm::SmallPtrSet<ArchetypeBuilder::PotentialArchetype *, 16> knownPAs;
  llvm::SmallVector<GenericTypeParamType *, 8> primary;
  for (auto param : params) {
    auto pa = builder.resolveType(param);
    assert(pa && "Missing potential archetype for generic parameter");

    // We only care about the representative.
    pa = pa->getRepresentative();

    // If the potential archetype has a parent, it isn't primary.
    if (pa->getRepresentative()->getParent())
      continue;

    if (knownPAs.insert(pa))
      primary.push_back(param);
  }

  // For each of the primary potential archetypes, add the requirements,
  // along with the requirements of its nested types.
  for (auto param : primary) {
    auto pa = builder.resolveType(param)->getRepresentative();
    addRequirements(builder.getTranslationUnit(), param, pa, knownPAs,
                    requirements);
  }
}

/// Check the signature of a generic function.
static bool checkGenericFuncSignature(TypeChecker &tc,
                                      ArchetypeBuilder *builder,
                                      FuncDecl *func,
                                      GenericTypeResolver &resolver) {
  bool badType = false;

  // Check the generic parameter list.
  checkGenericParameters(tc, builder, func->getGenericParams(), resolver);

  // Check the parameter patterns.
  for (auto pattern : func->getArgParamPatterns()) {
    // Check the pattern.
    if (tc.typeCheckPattern(pattern, func, /*allowUnboundGenerics=*/false,
                         &resolver))
      badType = true;

    // Infer requirements from the pattern.
    if (builder) {
      builder->inferRequirements(pattern);
    }
  }

  // If there is a declared result type, check that as well.
  if (!func->getBodyResultTypeLoc().isNull()) {
    // Check the result type of the function.
    if (tc.validateType(func->getBodyResultTypeLoc(),
                        /*allowUnboundGenerics=*/false,
                        &resolver)) {
      badType = true;
    }

    // Infer requirements from it.
    if (builder) {
      builder->inferRequirements(func->getBodyResultTypeLoc().getTypeRepr());
    }
  }

  return badType;
}

bool TypeChecker::validateGenericFuncSignature(FuncDecl *func) {
  // Create the archetype builder.
  ArchetypeBuilder builder = createArchetypeBuilder(*this);

  // Type check the function declaration, treating all generic type
  // parameters as dependent, unresolved.
  PartialGenericTypeToArchetypeResolver partialResolver(*this);
  if (checkGenericFuncSignature(*this, &builder, func, partialResolver)) {
    func->setType(ErrorType::get(Context));
    return true;
  }

  // The archetype builder now has all of the requirements, although there might
  // still be errors that have not yet been diagnosed. Revert the generic
  // function signature and type-check it again, completely.
  revertGenericFuncSignature(func);
  CompleteGenericTypeResolver completeResolver(*this, builder);
  if (checkGenericFuncSignature(*this, nullptr, func, completeResolver)) {
    func->setType(ErrorType::get(Context));
    return true;
  }

  // The generic function signature is complete and well-formed. Determine
  // the type of the generic function.

  // Collect the complete set of generic parameter types.
  SmallVector<GenericTypeParamType *, 4> allGenericParams;
  collectGenericParamTypes(func->getGenericParams(), allGenericParams);

  // Collect the requirements placed on the generic parameter types.
  SmallVector<Requirement, 4> requirements;
  collectRequirements(builder, allGenericParams, requirements);

  // Compute the function type.
  auto funcTy = func->getBodyResultTypeLoc().getType();
  if (!funcTy) {
    funcTy = TupleType::getEmpty(Context);
  }

  auto patterns = func->getArgParamPatterns();
  for (unsigned i = 0, e = patterns.size(); i != e; ++i) {
    Type argTy = patterns[e - i - 1]->getType();

    // Validate and consume the function type attributes.
    // FIXME: Hacked up form of validateAndConsumeFunctionTypeAttributes().
    auto info = AnyFunctionType::ExtInfo()
                  .withIsNoReturn(func->getAttrs().isNoReturn());

    if (i == e-1) {
      funcTy = GenericFunctionType::get(allGenericParams, requirements,
                                        argTy, funcTy, info, Context);
    } else {
      funcTy = FunctionType::get(argTy, funcTy, info, Context);
    }
  }

  return false;
}

SpecializeExpr *
TypeChecker::buildSpecializeExpr(Expr *Sub, Type Ty,
                                 const TypeSubstitutionMap &Substitutions,
                                 const ConformanceMap &Conformances) {
  auto polyFn = Sub->getType()->castTo<PolymorphicFunctionType>();
  return new (Context) SpecializeExpr(Sub, Ty,
                         encodeSubstitutions(&polyFn->getGenericParams(),
                                             Substitutions,
                                             Conformances,
                                             /*OnlyInnermostParams=*/false));
}

ArrayRef<Substitution>
TypeChecker::encodeSubstitutions(const GenericParamList *GenericParams,
                                 const TypeSubstitutionMap &Substitutions,
                                 const ConformanceMap &Conformances,
                                 bool OnlyInnermostParams) {
  SmallVector<Substitution, 4> Results;
  encodeSubstitutions(GenericParams, Substitutions, Conformances,
                      OnlyInnermostParams, Results);
  return Context.AllocateCopy(Results);
}

void TypeChecker::encodeSubstitutions(const GenericParamList *GenericParams,
                                      const TypeSubstitutionMap &Substitutions,
                                      const ConformanceMap &Conformances,
                                      bool OnlyInnermostParams,
                                      SmallVectorImpl<Substitution> &Results) {
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

  Results.resize(allArchetypes.size());
  unsigned index = 0;
  for (auto archetype : allArchetypes) {
    // Figure out the key into the maps we were given.
    SubstitutableType *key = archetype;
    assert(Substitutions.count(key) && "Missing substitution information");
    assert(Conformances.count(key) && "Missing conformance information");

    // Record this substitution.
    Results[index].Archetype = archetype;
    Results[index].Replacement
      = Substitutions.find(key)->second;
    Results[index].Conformance
      = Context.AllocateCopy(Conformances.find(key)->second);

    ++index;
  }
}

bool TypeChecker::checkSubstitutions(TypeSubstitutionMap &Substitutions,
                                     ConformanceMap &Conformance,
                                     SourceLoc ComplainLoc,
                                     TypeSubstitutionMap *RecordSubstitutions) {
  // FIXME: We want to migrate to a world where we don't need ComplainLoc, and
  // this routine can't fail, because the type checker checks everything in
  // advance.
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
      if (!isSubtypeOf(T, superclass)) {
        if (ComplainLoc.isValid()) {
          diagnose(ComplainLoc, diag::type_does_not_inherit, T, superclass);
          // FIXME: Show where the requirement came from?
        }
        return true;
      }
    }

    SmallVectorImpl<ProtocolConformance *> &Conformances
      = Conformance[archetype];
    if (Conformances.empty()) {
      for (auto Proto : archetype->getConformsTo()) {
        ProtocolConformance *Conformance = nullptr;
        if (conformsToProtocol(T, Proto, &Conformance, ComplainLoc)) {
          Conformances.push_back(Conformance);
        } else {
          return true;
        }
      }
    }

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
  AssociatedTypeDecl *requirement = nullptr;
  for (auto member : protocol->getMembers()) {
    auto td = dyn_cast<AssociatedTypeDecl>(member);
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
  // FIXME: substMemberTypeWithBase when we deal with generic conformance.
  return conformance->getTypeWitness(requirement).Replacement;
}
