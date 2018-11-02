//===--- TypeCheckGeneric.cpp - Generics ----------------------------------===//
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
// This file implements support for generics.
//
//===----------------------------------------------------------------------===//
#include "TypeChecker.h"
#include "GenericTypeResolver.h"
#include "TypoCorrection.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/GenericSignatureBuilder.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/Types.h"
#include "swift/Basic/Defer.h"
#include "llvm/Support/ErrorHandling.h"

using namespace swift;

///
/// GenericTypeResolver implementations
///

Type DependentGenericTypeResolver::mapTypeIntoContext(Type type) {
  return type;
}

Type DependentGenericTypeResolver::resolveDependentMemberType(
                                     Type baseTy,
                                     DeclContext *DC,
                                     SourceRange baseRange,
                                     ComponentIdentTypeRepr *ref) {
  return DependentMemberType::get(baseTy, ref->getIdentifier());
}

bool DependentGenericTypeResolver::areSameType(Type type1, Type type2) {
  if (!type1->hasTypeParameter() && !type2->hasTypeParameter())
    return type1->isEqual(type2);

  // Conservative answer: they could be the same.
  return true;
}

Type GenericTypeToArchetypeResolver::mapTypeIntoContext(Type type) {
  return GenericEnvironment::mapTypeIntoContext(GenericEnv, type);
}

Type GenericTypeToArchetypeResolver::resolveDependentMemberType(
                                  Type baseTy,
                                  DeclContext *DC,
                                  SourceRange baseRange,
                                  ComponentIdentTypeRepr *ref) {
  llvm_unreachable("Dependent type after archetype substitution");
}

bool GenericTypeToArchetypeResolver::areSameType(Type type1, Type type2) {
  return type1->isEqual(type2);
}

Type ProtocolRequirementTypeResolver::mapTypeIntoContext(Type type) {
  return type;
}

Type ProtocolRequirementTypeResolver::resolveDependentMemberType(
    Type baseTy, DeclContext *DC, SourceRange baseRange,
    ComponentIdentTypeRepr *ref) {
  return DependentMemberType::get(baseTy, ref->getIdentifier());
}

bool ProtocolRequirementTypeResolver::areSameType(Type type1, Type type2) {
  if (type1->isEqual(type2))
    return true;

  // If both refer to associated types with the same name, they'll implicitly
  // be considered equivalent.
  auto depMem1 = type1->getAs<DependentMemberType>();
  if (!depMem1) return false;

  auto depMem2 = type2->getAs<DependentMemberType>();
  if (!depMem2) return false;

  if (depMem1->getName() != depMem2->getName()) return false;

  return areSameType(depMem1->getBase(), depMem2->getBase());
}

CompleteGenericTypeResolver::CompleteGenericTypeResolver(
                                              TypeChecker &tc,
                                              GenericSignature *genericSig)
  : tc(tc), genericSig(genericSig) {
  if (genericSig) {
    builder = tc.Context.getOrCreateGenericSignatureBuilder(
      genericSig->getCanonicalSignature());
  } else {
    builder = nullptr;
  }
}

Type CompleteGenericTypeResolver::mapTypeIntoContext(Type type) {
  return type;
}

Type CompleteGenericTypeResolver::resolveDependentMemberType(
                                    Type baseTy,
                                    DeclContext *DC,
                                    SourceRange baseRange,
                                    ComponentIdentTypeRepr *ref) {
  auto baseEquivClass =
    builder->resolveEquivalenceClass(
                                baseTy,
                                ArchetypeResolutionKind::CompleteWellFormed);
  assert(baseEquivClass && "Unknown base type?");

  // Look for a nested type with the given name.
  if (auto nestedType =
          baseEquivClass->lookupNestedType(*builder, ref->getIdentifier())) {
    // Record the type we found.
    ref->setValue(nestedType, nullptr);
  } else {
    // Resolve the base to a potential archetype.
    // Perform typo correction.
    TypoCorrectionResults corrections(tc, ref->getIdentifier(),
                                      DeclNameLoc(ref->getIdLoc()));
    tc.performTypoCorrection(DC, DeclRefKind::Ordinary,
                             MetatypeType::get(baseTy),
                             NameLookupFlags::ProtocolMembers,
                             corrections, builder);

    // Check whether we have a single type result.
    auto singleType = cast_or_null<TypeDecl>(
      corrections.getUniqueCandidateMatching([](ValueDecl *result) {
        return isa<TypeDecl>(result);
      }));

    // If we don't have a single result, complain and fail.
    if (!singleType) {
      Identifier name = ref->getIdentifier();
      SourceLoc nameLoc = ref->getIdLoc();
      tc.diagnose(nameLoc, diag::invalid_member_type, name, baseTy)
        .highlight(baseRange);
      corrections.noteAllCandidates();

      return ErrorType::get(tc.Context);
    }

    // We have a single type result. Suggest it.
    tc.diagnose(ref->getIdLoc(), diag::invalid_member_type_suggest,
                baseTy, ref->getIdentifier(),
                singleType->getBaseName().getIdentifier())
      .fixItReplace(ref->getIdLoc(),
                    singleType->getBaseName().userFacingName());

    // Correct to the single type result.
    ref->overwriteIdentifier(singleType->getBaseName().getIdentifier());
    ref->setValue(singleType, nullptr);
  }

  // If the nested type has been resolved to an associated type, use it.
  if (auto assocType = dyn_cast<AssociatedTypeDecl>(ref->getBoundDecl())) {
    return DependentMemberType::get(baseTy, assocType);
  }

  // Otherwise, the nested type comes from a concrete type. Substitute the
  // base type into it.
  auto concrete = ref->getBoundDecl();
  tc.validateDeclForNameLookup(concrete);

  if (auto typeAlias = dyn_cast<TypeAliasDecl>(concrete)) {
    if (auto protocol = dyn_cast<ProtocolDecl>(typeAlias->getDeclContext())) {
      // We need to make sure the generic environment of a surrounding protocol
      // propagates to the typealias, since the former may not have existed when
      // the typealiases type was first computed.
      // FIXME: See the comment in the ProtocolDecl case of validateDecl().
      tc.validateDecl(protocol);
    }
  }
  if (!concrete->hasInterfaceType())
    return ErrorType::get(tc.Context);
  if (baseTy->isTypeParameter()) {
    if (auto proto =
          concrete->getDeclContext()
            ->getAsProtocolOrProtocolExtensionContext()) {
      // Fast path: if there are no type parameters in the concrete type, just
      // return it.
      if (!concrete->getDeclaredInterfaceType()->hasTypeParameter())
        return concrete->getDeclaredInterfaceType();

      tc.validateDecl(proto);
      auto subMap = SubstitutionMap::getProtocolSubstitutions(
                      proto, baseTy, ProtocolConformanceRef(proto));
      return concrete->getDeclaredInterfaceType().subst(subMap);
    }

    Type baseType = baseEquivClass->concreteType ? baseEquivClass->concreteType
                                                 : baseEquivClass->superclass;

    if (baseType) {
      return baseType->getTypeOfMember(DC->getParentModule(), concrete,
                                       concrete->getDeclaredInterfaceType());
    }

    llvm_unreachable("shouldn't have a concrete decl here");
  }

  return tc.substMemberTypeWithBase(DC->getParentModule(), concrete, baseTy);
}

bool CompleteGenericTypeResolver::areSameType(Type type1, Type type2) {
  return genericSig->getCanonicalTypeInContext(type1)
           == genericSig->getCanonicalTypeInContext(type2);
}

///
/// Common code for generic functions, generic types
///

/// Check the generic parameters in the given generic parameter list (and its
/// parent generic parameter lists) according to the given resolver.
void TypeChecker::checkGenericParamList(GenericSignatureBuilder *builder,
                                        GenericParamList *genericParams,
                                        GenericSignature *parentSig,
                                        GenericTypeResolver *resolver) {
  // If there is a parent context, add the generic parameters and requirements
  // from that context.
  if (builder)
    builder->addGenericSignature(parentSig);

  // If there aren't any generic parameters at this level, we're done.
  if (!genericParams)
    return;

  assert(genericParams->size() > 0 &&
         "Parsed an empty generic parameter list?");

  // Determine where and how to perform name lookup for the generic
  // parameter lists and where clause.
  TypeResolutionOptions options;
  DeclContext *lookupDC = genericParams->begin()[0]->getDeclContext();
  if (!lookupDC->isModuleScopeContext()) {
    assert((isa<GenericTypeDecl>(lookupDC) ||
            isa<ExtensionDecl>(lookupDC) ||
            isa<AbstractFunctionDecl>(lookupDC) ||
            isa<SubscriptDecl>(lookupDC)) &&
           "not a proper generic parameter context?");
    options = TypeResolutionFlags::GenericSignature;
  }    

  // First, add the generic parameters to the generic signature builder.
  // Do this before checking the inheritance clause, since it may
  // itself be dependent on one of these parameters.
  if (builder) {
    for (auto param : *genericParams)
      builder->addGenericParameter(param);
  }

  // Now, check the inheritance clauses of each parameter.
  for (auto param : *genericParams) {
    checkInheritanceClause(param, resolver);

    if (builder)
      builder->addGenericParameterRequirements(param);
  }

  // Add the requirements clause to the builder, validating the types in
  // the requirements clause along the way.
  validateRequirements(genericParams->getWhereLoc(),
                       genericParams->getRequirements(), lookupDC,
                       options, resolver, builder);
}

bool TypeChecker::validateRequirement(SourceLoc whereLoc, RequirementRepr &req,
                                      DeclContext *lookupDC,
                                      TypeResolutionOptions options,
                                      GenericTypeResolver *resolver) {
  if (req.isInvalid())
    return true;

  // Note that we are resolving within a requirement.
  options |= TypeResolutionFlags::GenericRequirement;

  // Protocol where clauses cannot add conformance and superclass constraints
  // to 'Self', because we need to be able to resolve inherited protocols and
  // protocol superclasses before computing the protocol requirement signature.
  if (options & TypeResolutionFlags::ProtocolWhereClause) {
    if (req.getKind() == RequirementReprKind::TypeConstraint ||
        req.getKind() == RequirementReprKind::LayoutConstraint) {
      if (auto *subjectTyR = req.getSubjectLoc().getTypeRepr()) {
        if (auto *componentTyR = dyn_cast<ComponentIdentTypeRepr>(subjectTyR)) {
          if (componentTyR->getIdentifier() == Context.Id_Self) {
            diagnose(req.getSubjectLoc().getLoc(),
                     diag::protocol_where_clause_self_requirement);

            req.getSubjectLoc().setType(ErrorType::get(Context));

            if (req.getKind() == RequirementReprKind::TypeConstraint)
              req.getConstraintLoc().setType(ErrorType::get(Context));

            req.setInvalid();
            return true;
          }
        }
      }
    }
  }

  switch (req.getKind()) {
  case RequirementReprKind::TypeConstraint: {
    // Validate the types.
    if (validateType(req.getSubjectLoc(), lookupDC, options, resolver)) {
      req.setInvalid();
    }

    if (validateType(req.getConstraintLoc(), lookupDC, options, resolver)) {
      req.setInvalid();
    }

    return req.isInvalid();
  }

  case RequirementReprKind::LayoutConstraint: {
    // Validate the types.
    if (validateType(req.getSubjectLoc(), lookupDC, options, resolver)) {
      req.setInvalid();
    }

    if (req.getLayoutConstraintLoc().isNull()) {
      req.setInvalid();
    }
    return req.isInvalid();
  }

  case RequirementReprKind::SameType: {
    if (validateType(req.getFirstTypeLoc(), lookupDC, options, resolver)) {
      req.setInvalid();
    }

    if (validateType(req.getSecondTypeLoc(), lookupDC, options, resolver)) {
      req.setInvalid();
    }

    return req.isInvalid();
  }
  }

  llvm_unreachable("Unhandled RequirementKind in switch.");
}

void TypeChecker::validateRequirements(
                                 SourceLoc whereLoc,
                                 MutableArrayRef<RequirementRepr> requirements,
                                 DeclContext *dc,
                                 TypeResolutionOptions options,
                                 GenericTypeResolver *resolver,
                                 GenericSignatureBuilder *builder) {
  for (auto &req : requirements) {
    if (validateRequirement(whereLoc, req, dc, options, resolver))
      continue;

    if (builder &&
        isErrorResult(builder->addRequirement(&req, dc->getParentModule())))
      req.setInvalid();
  }
}

std::string
TypeChecker::gatherGenericParamBindingsText(
                              ArrayRef<Type> types,
                              TypeArrayView<GenericTypeParamType> genericParams,
                              TypeSubstitutionFn substitutions) {
  llvm::SmallPtrSet<GenericTypeParamType *, 2> knownGenericParams;
  for (auto type : types) {
    if (type.isNull()) continue;

    type.visit([&](Type type) {
      if (auto gp = type->getAs<GenericTypeParamType>()) {
        knownGenericParams.insert(
            gp->getCanonicalType()->castTo<GenericTypeParamType>());
      }
    });
  }

  if (knownGenericParams.empty())
    return "";

  SmallString<128> result;
  for (auto gp : genericParams) {
    auto canonGP = gp->getCanonicalType()->castTo<GenericTypeParamType>();
    if (!knownGenericParams.count(canonGP))
      continue;

    if (result.empty())
      result += " [with ";
    else
      result += ", ";
    result += gp->getName().str();
    result += " = ";

    auto type = substitutions(canonGP);
    if (!type)
      return "";

    result += type.getString();
  }

  result += "]";
  return result.str().str();
}

void
TypeChecker::prepareGenericParamList(GenericParamList *gp,
                                     DeclContext *dc) {
  AccessLevel access;
  if (auto *fd = dyn_cast<FuncDecl>(dc))
    access = fd->getFormalAccess();
  else if (auto *nominal = dyn_cast<NominalTypeDecl>(dc))
    access = nominal->getFormalAccess();
  else
    access = AccessLevel::Internal;
  access = std::max(access, AccessLevel::Internal);

  unsigned depth = gp->getDepth();
  for (auto paramDecl : *gp) {
    checkDeclAttributesEarly(paramDecl);
    paramDecl->setDepth(depth);
    if (!paramDecl->hasAccess())
      paramDecl->setAccess(access);
  }
}

/// Add the generic parameter types from the given list to the vector.
static void addGenericParamTypes(GenericParamList *gpList,
                                 SmallVectorImpl<GenericTypeParamType *> &params) {
  if (!gpList) return;

  for (auto gpDecl : *gpList) {
    params.push_back(
            gpDecl->getDeclaredInterfaceType()->castTo<GenericTypeParamType>());
  }
}

static void revertDependentTypeLoc(TypeLoc &tl) {
  // If there's no type representation, there's nothing to revert.
  if (!tl.getTypeRepr())
    return;

  // Don't revert an error type; we've already complained.
  if (tl.wasValidated() && tl.isError())
    return;

  // Make sure we validate the type again.
  tl.setType(Type());
}

/// Revert the dependent types within the given generic parameter list.
void TypeChecker::revertGenericParamList(GenericParamList *genericParams) {
  // Revert the inherited clause of the generic parameter list.
  for (auto param : *genericParams) {
    for (auto &inherited : param->getInherited())
      revertDependentTypeLoc(inherited);
  }

  // Revert the requirements of the generic parameter list.
  revertGenericRequirements(genericParams->getRequirements());
}

void TypeChecker::revertGenericRequirements(
                                MutableArrayRef<RequirementRepr> requirements) {
  for (auto &req : requirements) {
    if (req.isInvalid())
      continue;

    switch (req.getKind()) {
    case RequirementReprKind::TypeConstraint:
      revertDependentTypeLoc(req.getConstraintLoc());
      LLVM_FALLTHROUGH;

    case RequirementReprKind::LayoutConstraint:
      revertDependentTypeLoc(req.getSubjectLoc());
      break;

    case RequirementReprKind::SameType:
      revertDependentTypeLoc(req.getFirstTypeLoc());
      revertDependentTypeLoc(req.getSecondTypeLoc());
      break;
    }
  }
}

///
/// Generic functions
///

/// Check the signature of a generic function.
static bool checkGenericFuncSignature(TypeChecker &tc,
                                      GenericSignatureBuilder *builder,
                                      AbstractFunctionDecl *func,
                                      GenericTypeResolver &resolver) {
  bool badType = false;

  // Check the generic parameter list.
  auto genericParams = func->getGenericParams();

  tc.checkGenericParamList(
                         builder, genericParams,
                         func->getDeclContext()->getGenericSignatureOfContext(),
                         &resolver);

  // Check the parameter patterns.
  auto params = func->getParameters();

  if (tc.typeCheckParameterList(params, func, TypeResolutionOptions(),
                                resolver))
    badType = true;

  // Infer requirements from the pattern.
  if (builder) {
    builder->inferRequirements(*func->getParentModule(), params,
                               genericParams);
  }

  // If there is a declared result type, check that as well.
  if (auto fn = dyn_cast<FuncDecl>(func)) {
    if (!fn->getBodyResultTypeLoc().isNull()) {
      // Check the result type of the function.
      TypeResolutionOptions options = TypeResolutionFlags::AllowIUO;
      if (fn->hasDynamicSelf())
        options |= TypeResolutionFlags::DynamicSelfResult;

      if (tc.validateType(fn->getBodyResultTypeLoc(), fn, options, &resolver)) {
        badType = true;
      }

      // Infer requirements from it.
      if (builder && genericParams &&
          fn->getBodyResultTypeLoc().getTypeRepr()) {
        auto source =
          GenericSignatureBuilder::FloatingRequirementSource::forInferred(
              fn->getBodyResultTypeLoc().getTypeRepr());
        builder->inferRequirements(*func->getParentModule(),
                                   fn->getBodyResultTypeLoc(),
                                   source);
      }
    }

    // If this is a materializeForSet, infer requirements from the
    // storage type instead, since it's not part of the accessor's
    // type signature.
    auto accessor = dyn_cast<AccessorDecl>(fn);
    if (accessor && accessor->isMaterializeForSet()) {
      if (builder) {
        auto *storage = accessor->getStorage();
        if (auto *subscriptDecl = dyn_cast<SubscriptDecl>(storage)) {
          auto source =
            GenericSignatureBuilder::FloatingRequirementSource::forInferred(
                subscriptDecl->getElementTypeLoc().getTypeRepr());

          TypeLoc type(nullptr, subscriptDecl->getElementInterfaceType());
          assert(type.getType());
          builder->inferRequirements(*func->getParentModule(),
                                     type, source);
        }
      }
    }
  }

  return badType;
}

static void revertGenericFuncSignature(AbstractFunctionDecl *func) {
  // Revert the result type.
  if (auto fn = dyn_cast<FuncDecl>(func))
    if (!fn->getBodyResultTypeLoc().isNull())
      revertDependentTypeLoc(fn->getBodyResultTypeLoc());

  // Revert the body parameter types.
  for (auto &param : *func->getParameters())
    revertDependentTypeLoc(param->getTypeLoc());
}

/// Determine whether the given type is \c Self, an associated type of \c Self,
/// or a concrete type.
static bool isSelfDerivedOrConcrete(Type protoSelf, Type type) {
  // Check for a concrete type.
  if (!type->hasTypeParameter())
    return true;

  if (type->isTypeParameter() &&
      type->getRootGenericParam()->isEqual(protoSelf))
    return true;

  return false;
}

// For a generic requirement in a protocol, make sure that the requirement
// set didn't add any requirements to Self or its associated types.
static bool checkProtocolSelfRequirements(GenericSignature *sig,
                                          ValueDecl *decl,
                                          TypeChecker &TC) {
  // For a generic requirement in a protocol, make sure that the requirement
  // set didn't add any requirements to Self or its associated types.
  if (auto *proto = dyn_cast<ProtocolDecl>(decl->getDeclContext())) {
    auto protoSelf = proto->getSelfInterfaceType();
    for (auto req : sig->getRequirements()) {
      // If one of the types in the requirement is dependent on a non-Self
      // type parameter, this requirement is okay.
      if (!isSelfDerivedOrConcrete(protoSelf, req.getFirstType()) ||
          !isSelfDerivedOrConcrete(protoSelf, req.getSecondType()))
        continue;

      // The conformance of 'Self' to the protocol is okay.
      if (req.getKind() == RequirementKind::Conformance &&
          req.getSecondType()->getAs<ProtocolType>()->getDecl() == proto &&
          req.getFirstType()->is<GenericTypeParamType>())
        continue;

      TC.diagnose(decl,
                  TC.Context.LangOpts.EffectiveLanguageVersion[0] >= 4
                    ? diag::requirement_restricts_self
                    : diag::requirement_restricts_self_swift3,
                  decl->getDescriptiveKind(), decl->getFullName(),
                  req.getFirstType().getString(),
                  static_cast<unsigned>(req.getKind()),
                  req.getSecondType().getString());

      if (TC.Context.LangOpts.EffectiveLanguageVersion[0] >= 4)
        return true;
    }
  }

  return false;
}

/// All generic parameters of a generic function must be referenced in the
/// declaration's type, otherwise we have no way to infer them.
static void checkReferencedGenericParams(GenericContext *dc,
                                         TypeChecker &TC) {
  auto *genericParams = dc->getGenericParams();
  auto *genericSig = dc->getGenericSignatureOfContext();
  if (!genericParams)
    return;

  auto *decl = cast<ValueDecl>(dc->getInnermostDeclarationDeclContext());

  // A helper class to collect referenced generic type parameters
  // and dependent member types.
  class ReferencedGenericTypeWalker : public TypeWalker {
    SmallPtrSet<CanType, 4> ReferencedGenericParams;

  public:
    ReferencedGenericTypeWalker() {}
    Action walkToTypePre(Type ty) override {
      // Find generic parameters or dependent member types.
      // Once such a type is found, don't recurse into its children.
      if (!ty->hasTypeParameter())
        return Action::SkipChildren;
      if (ty->isTypeParameter()) {
        ReferencedGenericParams.insert(ty->getCanonicalType());
        return Action::SkipChildren;
      }
      return Action::Continue;
    }

    SmallPtrSetImpl<CanType> &getReferencedGenericParams() {
      return ReferencedGenericParams;
    }
  };

  // Collect all generic params referenced in parameter types and
  // return type.
  ReferencedGenericTypeWalker paramsAndResultWalker;
  auto *funcTy = decl->getInterfaceType()->castTo<GenericFunctionType>();
  for (const auto &param : funcTy->getParams())
    param.getType().walk(paramsAndResultWalker);
  funcTy->getResult().walk(paramsAndResultWalker);

  // Set of generic params referenced in parameter types,
  // return type or requirements.
  auto &referencedGenericParams =
      paramsAndResultWalker.getReferencedGenericParams();

  // Check if at least one of the generic params in the requirement refers
  // to an already referenced generic parameter. If this is the case,
  // then the other type is also considered as referenced, because
  // it is used to put requirements on the first type.
  auto reqTypesVisitor = [&referencedGenericParams](Requirement req) -> bool {
    Type first;
    Type second;

    switch (req.getKind()) {
    case RequirementKind::Superclass:
    case RequirementKind::SameType:
      second = req.getSecondType();
      LLVM_FALLTHROUGH;

    case RequirementKind::Conformance:
    case RequirementKind::Layout:
      first = req.getFirstType();
      break;
    }

    // Collect generic parameter types referenced by types used in a requirement.
    ReferencedGenericTypeWalker walker;
    if (first && first->hasTypeParameter())
      first.walk(walker);
    if (second && second->hasTypeParameter())
      second.walk(walker);
    auto &genericParamsUsedByRequirementTypes =
        walker.getReferencedGenericParams();

    // If at least one of the collected generic types or a root generic
    // parameter of dependent member types is known to be referenced by
    // parameter types, return types or other types known to be "referenced",
    // then all the types used in the requirement are considered to be
    // referenced, because they are used to defined something that is known
    // to be referenced.
    bool foundNewReferencedGenericParam = false;
    if (std::any_of(genericParamsUsedByRequirementTypes.begin(),
                    genericParamsUsedByRequirementTypes.end(),
                    [&referencedGenericParams](CanType t) {
                      assert(t->isTypeParameter());
                      return referencedGenericParams.find(
                                 t->getRootGenericParam()
                                     ->getCanonicalType()) !=
                             referencedGenericParams.end();
                    })) {
      std::for_each(genericParamsUsedByRequirementTypes.begin(),
                    genericParamsUsedByRequirementTypes.end(),
                    [&referencedGenericParams,
                     &foundNewReferencedGenericParam](CanType t) {
                      // Add only generic type parameters, but ignore any
                      // dependent member types, because requirement
                      // on a dependent member type does not provide enough
                      // information to infer the base generic type
                      // parameter.
                      if (!t->is<GenericTypeParamType>())
                        return;
                      if (referencedGenericParams.insert(t).second)
                        foundNewReferencedGenericParam = true;
                    });
    }
    return foundNewReferencedGenericParam;
  };

  ArrayRef<Requirement> requirements;

  auto FindReferencedGenericParamsInRequirements =
    [&requirements, genericSig, &reqTypesVisitor] {
    requirements = genericSig->getRequirements();
    // Try to find new referenced generic parameter types in requirements until
    // we reach a fix point. We need to iterate until a fix point, because we
    // may have e.g. chains of same-type requirements like:
    // not-yet-referenced-T1 == not-yet-referenced-T2.DepType2,
    // not-yet-referenced-T2 == not-yet-referenced-T3.DepType3,
    // not-yet-referenced-T3 == referenced-T4.DepType4.
    // When we process the first of these requirements, we don't know yet that
    // T2
    // will be referenced, because T3 will be referenced,
    // because T3 == T4.DepType4.
    while (true) {
      bool foundNewReferencedGenericParam = false;
      for (auto req : requirements) {
        if (reqTypesVisitor(req))
          foundNewReferencedGenericParam = true;
      }
      if (!foundNewReferencedGenericParam)
        break;
    }
  };

  // Find the depth of the function's own generic parameters.
  unsigned fnGenericParamsDepth = genericParams->getDepth();

  // Check that every generic parameter type from the signature is
  // among referencedGenericParams.
  for (auto *genParam : genericSig->getGenericParams()) {
    auto *paramDecl = genParam->getDecl();
    if (paramDecl->getDepth() != fnGenericParamsDepth)
      continue;
    if (!referencedGenericParams.count(genParam->getCanonicalType())) {
      // Lazily search for generic params that are indirectly used in the
      // function signature. Do it only if there is a generic parameter
      // that is not known to be referenced yet.
      if (requirements.empty()) {
        FindReferencedGenericParamsInRequirements();
        // Nothing to do if this generic parameter is considered to be
        // referenced after analyzing the requirements from the generic
        // signature.
        if (referencedGenericParams.count(genParam->getCanonicalType()))
          continue;
      }
      // Produce an error that this generic parameter cannot be bound.
      TC.diagnose(paramDecl->getLoc(), diag::unreferenced_generic_parameter,
                  paramDecl->getNameStr());
      decl->setInterfaceType(ErrorType::get(TC.Context));
      decl->setInvalid();
    }
  }
}

void TypeChecker::validateGenericFuncSignature(AbstractFunctionDecl *func) {
  bool invalid = false;

  if (auto *selfDecl = func->getImplicitSelfDecl())
    invalid |= selfDecl->getInterfaceType()->hasError();

  auto *dc = func->getDeclContext();

  GenericSignature *sig;
  if (auto gp = func->getGenericParams()) {
    gp->setOuterParameters(dc->getGenericParamsOfContext());
    prepareGenericParamList(gp, func);

    // Create the generic signature builder.
    GenericSignatureBuilder builder(Context);

    // Type check the function declaration, treating all generic type
    // parameters as dependent, unresolved.
    DependentGenericTypeResolver dependentResolver;
    if (checkGenericFuncSignature(*this, &builder, func, dependentResolver))
      invalid = true;

    // The generic function signature is complete and well-formed. Determine
    // the type of the generic function.
    sig = std::move(builder).computeGenericSignature(func->getLoc());

    // The generic signature builder now has all of the requirements, although
    // there might still be errors that have not yet been diagnosed. Revert the
    // generic function signature and type-check it again, completely.
    revertGenericFuncSignature(func);
    revertGenericParamList(gp);

    // Debugging of the generic signature.
    if (Context.LangOpts.DebugGenericSignatures) {
      func->dumpRef(llvm::errs());
      llvm::errs() << "\n";
      llvm::errs() << "Generic signature: ";
      sig->print(llvm::errs());
      llvm::errs() << "\n";
      llvm::errs() << "Canonical generic signature: ";
      sig->getCanonicalSignature()->print(llvm::errs());
      llvm::errs() << "\n";
    }

    GenericEnvironment *env;
    if (auto accessor = dyn_cast<AccessorDecl>(func)) {
      env = cast<SubscriptDecl>(accessor->getStorage())->getGenericEnvironment();
      assert(env && "accessor has generics but subscript is not generic");
    } else {
      env = sig->createGenericEnvironment();
    }
    func->setGenericEnvironment(env);
  } else {
    // Inherit the signature of our environment.
    sig = dc->getGenericSignatureOfContext();
    func->setGenericEnvironment(dc->getGenericEnvironmentOfContext());
  }

  CompleteGenericTypeResolver completeResolver(*this, sig);
  if (checkGenericFuncSignature(*this, nullptr, func, completeResolver))
    invalid = true;

  if (!invalid)
    invalid = checkProtocolSelfRequirements(sig, func, *this);

  if (invalid) {
    func->setInterfaceType(ErrorType::get(Context));
    func->setInvalid();
    return;
  }

  func->computeType();

  // We get bogus errors here with generic subscript materializeForSet.
  if (!isa<AccessorDecl>(func) ||
      !cast<AccessorDecl>(func)->isMaterializeForSet())
    checkReferencedGenericParams(func, *this);

  // Make sure that there are no unresolved
  // dependent types in the generic signature.
  assert(func->getInterfaceType()->hasError() ||
         !func->getInterfaceType()->findUnresolvedDependentMemberType());
}

///
/// Generic subscripts
///
/// FIXME: A lot of this code is duplicated from the generic functions
/// path above. We could consolidate more of this.
///

/// Check the signature of a generic subscript.
static bool checkGenericSubscriptSignature(TypeChecker &tc,
                                           GenericSignatureBuilder *builder,
                                           SubscriptDecl *subscript,
                                           GenericTypeResolver &resolver) {
  bool badType = false;

  // Check the generic parameter list.
  auto genericParams = subscript->getGenericParams();

  auto *dc = subscript->getDeclContext();

  tc.checkGenericParamList(
                    builder, genericParams,
                    dc->getGenericSignatureOfContext(),
                    &resolver);

  // Check the element type.
  badType |= tc.validateType(subscript->getElementTypeLoc(), subscript,
                             TypeResolutionFlags::AllowIUO, &resolver);

  // Infer requirements from it.
  if (genericParams && builder) {
    auto source =
      GenericSignatureBuilder::FloatingRequirementSource::forInferred(
          subscript->getElementTypeLoc().getTypeRepr());

    builder->inferRequirements(*subscript->getParentModule(),
                               subscript->getElementTypeLoc(),
                               source);
  }

  // Check the indices.
  auto params = subscript->getIndices();
  TypeResolutionOptions options = TypeResolutionFlags::SubscriptParameters;

  badType |= tc.typeCheckParameterList(params, subscript,
                                       options,
                                       resolver);

  // Infer requirements from the pattern.
  if (builder) {
    builder->inferRequirements(*subscript->getParentModule(), params,
                               genericParams);
  }

  return badType;
}

static void revertGenericSubscriptSignature(SubscriptDecl *subscript) {
  // Revert the element type.
  if (!subscript->getElementTypeLoc().isNull())
    revertDependentTypeLoc(subscript->getElementTypeLoc());

  // Revert the indices.
  for (auto &param : *subscript->getIndices())
    revertDependentTypeLoc(param->getTypeLoc());
}

void
TypeChecker::validateGenericSubscriptSignature(SubscriptDecl *subscript) {
  bool invalid = false;

  auto *dc = subscript->getDeclContext();

  GenericSignature *sig;
  if (auto *gp = subscript->getGenericParams()) {
    gp->setOuterParameters(dc->getGenericParamsOfContext());
    prepareGenericParamList(gp, subscript);

    // Create the generic signature builder.
    GenericSignatureBuilder builder(Context);

    // Type check the function declaration, treating all generic type
    // parameters as dependent, unresolved.
    DependentGenericTypeResolver dependentResolver;
    if (checkGenericSubscriptSignature(*this, &builder, subscript,
                                       dependentResolver))
      invalid = true;

    // The generic subscript signature is complete and well-formed. Determine
    // the type of the generic subscript.
    sig =
      std::move(builder).computeGenericSignature(subscript->getLoc());

    // The generic signature builder now has all of the requirements, although
    // there might still be errors that have not yet been diagnosed. Revert the
    // generic function signature and type-check it again, completely.
    revertGenericSubscriptSignature(subscript);
    revertGenericParamList(gp);

    // Debugging of generic signature generation.
    if (Context.LangOpts.DebugGenericSignatures) {
      subscript->dumpRef(llvm::errs());
      llvm::errs() << "\n";
      llvm::errs() << "Generic signature: ";
      sig->print(llvm::errs());
      llvm::errs() << "\n";
      llvm::errs() << "Canonical generic signature: ";
      sig->getCanonicalSignature()->print(llvm::errs());
      llvm::errs() << "\n";
    }

    subscript->setGenericEnvironment(sig->createGenericEnvironment());
  } else {
    // Inherit the signature of our environment.
    sig = dc->getGenericSignatureOfContext();
    subscript->setGenericEnvironment(dc->getGenericEnvironmentOfContext());
  }

  CompleteGenericTypeResolver completeResolver(*this, sig);
  if (checkGenericSubscriptSignature(*this, nullptr, subscript, completeResolver))
    invalid = true;

  if (!invalid)
    invalid = checkProtocolSelfRequirements(sig, subscript, *this);

  if (invalid) {
    subscript->setInterfaceType(ErrorType::get(Context));
    subscript->setInvalid();
    // null doesn't mean error here: callers still expect the signature.
    return;
  }

  subscript->computeType();

  checkReferencedGenericParams(subscript, *this);
}

///
/// Generic types
///

/// Visit the given generic parameter lists from the outermost to the innermost,
/// calling the visitor function for each list.
static void visitOuterToInner(
                      GenericParamList *genericParams,
                      llvm::function_ref<void(GenericParamList *)> visitor) {
  if (auto outerGenericParams = genericParams->getOuterParameters())
    visitOuterToInner(outerGenericParams, visitor);

  visitor(genericParams);
}

/// Retrieve the generic parameter depth of the extended type.
static unsigned getExtendedTypeGenericDepth(ExtensionDecl *ext) {
  auto nominal = ext->getAsNominalTypeOrNominalTypeExtensionContext();
  if (!nominal) return static_cast<unsigned>(-1);

  auto sig = nominal->getGenericSignatureOfContext();
  if (!sig) return static_cast<unsigned>(-1);

  return sig->getGenericParams().back()->getDepth();
}

GenericEnvironment *TypeChecker::checkGenericEnvironment(
                      GenericParamList *genericParams,
                      DeclContext *dc,
                      GenericSignature *parentSig,
                      bool allowConcreteGenericParams,
                      ExtensionDecl *ext,
                      llvm::function_ref<void(GenericSignatureBuilder &)>
                        inferRequirements,
                      bool mustInferRequirements) {
  assert(genericParams && "Missing generic parameters?");
  bool recursivelyVisitGenericParams =
    genericParams->getOuterParameters() && !parentSig;

  GenericSignature *sig;
  if (!ext || mustInferRequirements || ext->getTrailingWhereClause() ||
      getExtendedTypeGenericDepth(ext) != genericParams->getDepth()) {
    // Collect the generic parameters.
    SmallVector<GenericTypeParamType *, 4> allGenericParams;
    if (recursivelyVisitGenericParams) {
      visitOuterToInner(genericParams,
                        [&](GenericParamList *gpList) {
        addGenericParamTypes(gpList, allGenericParams);
      });
    } else {
      if (parentSig) {
        allGenericParams.append(parentSig->getGenericParams().begin(),
                                parentSig->getGenericParams().end());
      }

      addGenericParamTypes(genericParams, allGenericParams);
    }

    // Create the generic signature builder.
    GenericSignatureBuilder builder(Context);

    // Type check the generic parameters, treating all generic type
    // parameters as dependent, unresolved.
    DependentGenericTypeResolver dependentResolver;
    if (recursivelyVisitGenericParams) {
      visitOuterToInner(genericParams,
                        [&](GenericParamList *gpList) {
        checkGenericParamList(&builder, gpList, nullptr, &dependentResolver);
      });
    } else {
      checkGenericParamList(&builder, genericParams, parentSig,
                            &dependentResolver);
    }

    /// Perform any necessary requirement inference.
    inferRequirements(builder);

    // Record the generic type parameter types and the requirements.
    sig = std::move(builder).computeGenericSignature(
                                         genericParams->getSourceRange().Start,
                                         allowConcreteGenericParams);

    // The generic signature builder now has all of the requirements, although
    // there might still be errors that have not yet been diagnosed. Revert the
    // signature and type-check it again, completely.
    if (recursivelyVisitGenericParams) {
      visitOuterToInner(genericParams,
                        [&](GenericParamList *gpList) {
        revertGenericParamList(gpList);
      });
    } else {
      revertGenericParamList(genericParams);
    }

    // Debugging of the generic signature builder and generic signature
    // generation.
    if (Context.LangOpts.DebugGenericSignatures) {
      dc->printContext(llvm::errs());
      llvm::errs() << "\n";
      llvm::errs() << "Generic signature: ";
      sig->print(llvm::errs());
      llvm::errs() << "\n";
      llvm::errs() << "Canonical generic signature: ";
      sig->getCanonicalSignature()->print(llvm::errs());
      llvm::errs() << "\n";
    }
  } else {
    // Re-use the signature of the type being extended.
    sig = ext->getAsNominalTypeOrNominalTypeExtensionContext()
            ->getGenericSignatureOfContext();
  }

  CompleteGenericTypeResolver completeResolver(*this, sig);
  if (recursivelyVisitGenericParams) {
    visitOuterToInner(genericParams,
                      [&](GenericParamList *gpList) {
      checkGenericParamList(nullptr, gpList, nullptr, &completeResolver);
    });
  } else {
    checkGenericParamList(nullptr, genericParams, parentSig,
                          &completeResolver);
  }

  // Form the generic environment.
  return sig->createGenericEnvironment();
}

void TypeChecker::validateGenericTypeSignature(GenericTypeDecl *typeDecl) {
  assert(!typeDecl->getGenericEnvironment());

  auto *gp = typeDecl->getGenericParams();
  auto *dc = typeDecl->getDeclContext();

  if (!gp) {
    auto *parentEnv = dc->getGenericEnvironmentOfContext();
    typeDecl->setGenericEnvironment(parentEnv);
    return;
  }

  gp->setOuterParameters(dc->getGenericParamsOfContext());

  prepareGenericParamList(gp, typeDecl);

  // For a protocol, compute the requirement signature first. It will be used
  // by clients of the protocol.
  if (auto proto = dyn_cast<ProtocolDecl>(typeDecl)) {
    if (!proto->isRequirementSignatureComputed())
      proto->computeRequirementSignature();
  }

  auto *env = checkGenericEnvironment(gp, dc,
                                      dc->getGenericSignatureOfContext(),
                                      /*allowConcreteGenericParams=*/false,
                                      /*ext=*/nullptr);
  typeDecl->setGenericEnvironment(env);
}

///
/// Checking bound generic type arguments
///

RequirementCheckResult TypeChecker::checkGenericArguments(
    DeclContext *dc, SourceLoc loc, SourceLoc noteLoc, Type owner,
    TypeArrayView<GenericTypeParamType> genericParams,
    ArrayRef<Requirement> requirements,
    TypeSubstitutionFn substitutions,
    LookupConformanceFn conformances,
    ConformanceCheckOptions conformanceOptions,
    GenericRequirementsCheckListener *listener,
    SubstOptions options) {
  bool valid = true;

  // We handle any conditional requirements ourselves.
  conformanceOptions |= ConformanceCheckFlags::SkipConditionalRequirements;

  struct RequirementSet {
    ArrayRef<Requirement> Requirements;
    SmallVector<ParentConditionalConformance, 4> Parents;
  };

  SmallVector<RequirementSet, 8> pendingReqs;
  pendingReqs.push_back({requirements, {}});

  auto *env = dc->getGenericEnvironmentOfContext();

  while (!pendingReqs.empty()) {
    auto current = pendingReqs.pop_back_val();

    for (const auto &rawReq : current.Requirements) {
      auto req = rawReq;
      if (current.Parents.empty()) {
        auto substed = rawReq.subst(substitutions, conformances, options);
        if (!substed) {
          // Another requirement will fail later; just continue.
          valid = false;
          continue;
        }

        req = *substed;
      }

      auto kind = req.getKind();
      Type rawFirstType = rawReq.getFirstType();
      Type firstType = req.getFirstType();
      if (firstType->hasTypeParameter()) {
        if (!env)
          continue;
        firstType = env->mapTypeIntoContext(firstType);
      }

      Type rawSecondType, secondType;
      if (kind != RequirementKind::Layout) {
        rawSecondType = rawReq.getSecondType();
        secondType = req.getSecondType();
        if (secondType->hasTypeParameter()) {
          if (!env)
            continue;
          secondType = env->mapTypeIntoContext(secondType);
        }
      }

      // Don't do further checking on error types.
      if (firstType->hasError() || (secondType && secondType->hasError())) {
        // Another requirement will fail later; just continue.
        valid = false;
        continue;
      }

      bool requirementFailure = false;
      if (listener && !listener->shouldCheck(kind, firstType, secondType))
        continue;

      Diag<Type, Type, Type> diagnostic;
      Diag<Type, Type, StringRef> diagnosticNote;

      switch (kind) {
      case RequirementKind::Conformance: {
        // Protocol conformance requirements.
        auto proto = secondType->castTo<ProtocolType>();
        // FIXME: This should track whether this should result in a private
        // or non-private dependency.
        // FIXME: Do we really need "used" at this point?
        // FIXME: Poor location information. How much better can we do here?
        // FIXME: This call should support listener to be able to properly
        //        diagnose problems with conformances.
        auto result =
            conformsToProtocol(firstType, proto->getDecl(), dc,
                               conformanceOptions, loc);

        if (result) {
          auto conformance = *result;
          // Report the conformance.
          if (listener && valid && current.Parents.empty()) {
            listener->satisfiedConformance(rawFirstType, firstType,
                                           conformance);
          }

          auto conditionalReqs = conformance.getConditionalRequirements();
          if (!conditionalReqs.empty()) {
            auto history = current.Parents;
            history.push_back({firstType, proto});
            pendingReqs.push_back({conditionalReqs, std::move(history)});
          }
          continue;
        }

        // A failure at the top level is diagnosed elsewhere.
        if (current.Parents.empty())
          return RequirementCheckResult::Failure;

        // Failure needs to emit a diagnostic.
        diagnostic = diag::type_does_not_conform_owner;
        diagnosticNote = diag::type_does_not_inherit_or_conform_requirement;
        requirementFailure = true;
        break;
      }

      case RequirementKind::Layout:
        // TODO: Statically check other layout constraints, once they can
        // be spelled in Swift.
        if (req.getLayoutConstraint()->isClass() &&
            !firstType->satisfiesClassConstraint()) {
          diagnostic = diag::type_is_not_a_class;
          diagnosticNote = diag::anyobject_requirement;
          requirementFailure = true;
        }
        break;

      case RequirementKind::Superclass:
        // Superclass requirements.
        if (!isSubclassOf(firstType, secondType, dc)) {
          diagnostic = diag::type_does_not_inherit;
          diagnosticNote = diag::type_does_not_inherit_or_conform_requirement;
          requirementFailure = true;
        }
        break;

      case RequirementKind::SameType:
        if (!firstType->isEqual(secondType)) {
          diagnostic = diag::types_not_equal;
          diagnosticNote = diag::types_not_equal_requirement;
          requirementFailure = true;
        }
        break;
      }

      if (!requirementFailure)
        continue;

      if (listener &&
          listener->diagnoseUnsatisfiedRequirement(rawReq, firstType,
                                                   secondType, current.Parents))
        return RequirementCheckResult::Failure;

      if (loc.isValid()) {
        // FIXME: Poor source-location information.
        diagnose(loc, diagnostic, owner, firstType, secondType);

        std::string genericParamBindingsText;
        if (!genericParams.empty()) {
          genericParamBindingsText =
            gatherGenericParamBindingsText(
              {rawFirstType, rawSecondType}, genericParams, substitutions);
        }
        diagnose(noteLoc, diagnosticNote, rawFirstType, rawSecondType,
                 genericParamBindingsText);

        ParentConditionalConformance::diagnoseConformanceStack(Diags, noteLoc,
                                                               current.Parents);
      }

      return RequirementCheckResult::Failure;
    }
  }

  if (valid)
    return RequirementCheckResult::Success;
  return RequirementCheckResult::SubstitutionFailure;
}
