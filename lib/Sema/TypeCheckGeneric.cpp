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
#include "TypeCheckType.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/GenericSignatureBuilder.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/TypeCheckRequests.h"
#include "swift/AST/TypeResolutionStage.h"
#include "swift/AST/Types.h"
#include "swift/Basic/Defer.h"
#include "llvm/Support/ErrorHandling.h"

using namespace swift;

///
/// Common code for generic functions, generic types
///

/// Check the generic parameters in the given generic parameter list (and its
/// parent generic parameter lists) according to the given resolver.
void checkGenericParamList(TypeChecker &tc,
                           GenericSignatureBuilder *builder,
                           GenericParamList *genericParams,
                           GenericSignature *parentSig,
                           TypeResolution resolution) {
  // If there is a parent context, add the generic parameters and requirements
  // from that context.
  if (builder)
    builder->addGenericSignature(parentSig);

  // If there aren't any generic parameters at this level, we're done.
  if (!genericParams)
    return;

  assert(genericParams->size() > 0 &&
         "Parsed an empty generic parameter list?");

  // Determine where and how to perform name lookup.
  DeclContext *lookupDC = genericParams->begin()[0]->getDeclContext();
  assert(lookupDC == resolution.getDeclContext());

  // First, add the generic parameters to the generic signature builder.
  // Do this before checking the inheritance clause, since it may
  // itself be dependent on one of these parameters.
  if (builder) {
    for (auto param : *genericParams)
      builder->addGenericParameter(param);
  }

  // Add the requirements for each of the generic parameters to the builder.
  // Now, check the inheritance clauses of each parameter.
  if (builder) {
    for (auto param : *genericParams)
      builder->addGenericParameterRequirements(param);
  }


  // Add the requirements clause to the builder.
  if (builder) {
    WhereClauseOwner owner(resolution.getDeclContext(), genericParams);
    using FloatingRequirementSource =
      GenericSignatureBuilder::FloatingRequirementSource;
    RequirementRequest::visitRequirements(owner, resolution.getStage(),
        [&](const Requirement &req, RequirementRepr *reqRepr) {
          auto source = FloatingRequirementSource::forExplicit(reqRepr);
          builder->addRequirement(req, reqRepr, source, nullptr,
                                  lookupDC->getParentModule());
          return false;
        });
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
  unsigned depth = gp->getDepth();
  for (auto paramDecl : *gp) {
    checkDeclAttributesEarly(paramDecl);
    paramDecl->setDepth(depth);
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

///
/// Generic functions
///

/// Check the signature of a generic function.
static void checkGenericFuncSignature(TypeChecker &tc,
                                      GenericSignatureBuilder *builder,
                                      AbstractFunctionDecl *func,
                                      TypeResolution resolution) {
  // Check the generic parameter list.
  auto genericParams = func->getGenericParams();

  checkGenericParamList(tc, builder, genericParams,
                        func->getDeclContext()->getGenericSignatureOfContext(),
                        resolution);

  // Check the parameter patterns.
  auto params = func->getParameters();

  tc.typeCheckParameterList(params, resolution,
                            TypeResolverContext::AbstractFunctionDecl);

  // Infer requirements from the pattern.
  if (builder) {
    builder->inferRequirements(*func->getParentModule(), params,
                               genericParams);
  }

  // If there is a declared result type, check that as well.
  if (auto fn = dyn_cast<FuncDecl>(func)) {
    if (!fn->getBodyResultTypeLoc().isNull()) {
      // Check the result type of the function.
      TypeResolutionOptions options(fn->hasDynamicSelf()
          ? TypeResolverContext::DynamicSelfResult
          : TypeResolverContext::FunctionResult);

      tc.validateType(fn->getBodyResultTypeLoc(), resolution, options);

      // Infer requirements from it.
      if (builder && genericParams &&
          fn->getBodyResultTypeLoc().getTypeRepr()) {
        auto source =
          GenericSignatureBuilder::FloatingRequirementSource::forInferred(
              fn->getBodyResultTypeLoc().getTypeRepr());
        builder->inferRequirements(*func->getParentModule(),
                                   fn->getBodyResultTypeLoc().getType(),
                                   fn->getBodyResultTypeLoc().getTypeRepr(),
                                   source);
      }
    }
  }
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
void TypeChecker::checkProtocolSelfRequirements(ValueDecl *decl) {
  // For a generic requirement in a protocol, make sure that the requirement
  // set didn't add any requirements to Self or its associated types.
  if (auto *proto = dyn_cast<ProtocolDecl>(decl->getDeclContext())) {
    auto protoSelf = proto->getSelfInterfaceType();
    auto *sig = decl->getInnermostDeclContext()->getGenericSignatureOfContext();
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

      diagnose(decl,
               Context.isSwiftVersion3()
                 ? diag::requirement_restricts_self_swift3
                 : diag::requirement_restricts_self,
               decl->getDescriptiveKind(), decl->getFullName(),
               req.getFirstType().getString(),
               static_cast<unsigned>(req.getKind()),
               req.getSecondType().getString());
    }
  }
}

/// All generic parameters of a generic function must be referenced in the
/// declaration's type, otherwise we have no way to infer them.
void TypeChecker::checkReferencedGenericParams(GenericContext *dc) {
  // Don't do this check for accessors: they're not used directly, so we
  // never need to infer their generic arguments.  This is mostly a
  // compile-time optimization, but it also avoids problems with accessors
  // like 'read' and 'modify' that would arise due to yields not being
  // part of the formal type.
  if (isa<AccessorDecl>(dc))
    return;

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
      diagnose(paramDecl->getLoc(), diag::unreferenced_generic_parameter,
               paramDecl->getNameStr());
      decl->setInterfaceType(ErrorType::get(Context));
      decl->setInvalid();
    }
  }
}

static GenericSignature *
computeGenericFuncSignature(TypeChecker &tc, AbstractFunctionDecl *func) {
  auto *dc = func->getDeclContext();

  // Check whether the function is separately generic.
  auto gp = func->getGenericParams();
  if (!gp) {
    // If not, inherit the signature of our environment.
    func->setGenericEnvironment(dc->getGenericEnvironmentOfContext());
    return dc->getGenericSignatureOfContext();
  }

  // Do some initial configuration of the generic parameter lists that's
  // required in all cases.
  gp->setOuterParameters(dc->getGenericParamsOfContext());
  tc.prepareGenericParamList(gp, func);

  // Accessors can always use the generic context of their storage
  // declarations.  This is a compile-time optimization since it lets us
  // avoid the requirements-gathering phase, but it also simplifies that
  // work for accessors which don't mention the value type in their formal
  // signatures (like the read and modify coroutines, since yield types
  // aren't tracked in the AST type yet).
  //
  // Most accessors will implicitly have been handled above because they
  // aren't separately generic; we only get here for the accessors of
  // generic subscripts.
  if (auto accessor = dyn_cast<AccessorDecl>(func)) {
    auto subscript = cast<SubscriptDecl>(accessor->getStorage());
    auto sig = subscript->getGenericSignature();
    auto env = subscript->getGenericEnvironment();
    assert(sig && env && "accessor has generics but subscript is not generic");
    func->setGenericEnvironment(env);
    return sig;
  }

  // Create the generic signature builder.
  GenericSignatureBuilder builder(tc.Context);

  // Type check the function declaration, treating all generic type
  // parameters as dependent, unresolved.
  checkGenericFuncSignature(tc, &builder, func,
                            TypeResolution::forStructural(func));

  // The generic function signature is complete and well-formed. Determine
  // the type of the generic function.
  auto sig = std::move(builder).computeGenericSignature(func->getLoc());

  // The generic signature builder now has all of the requirements, although
  // there might still be errors that have not yet been diagnosed. Revert the
  // generic function signature and type-check it again, completely.
  revertGenericFuncSignature(func);

  // Debugging of the generic signature.
  if (tc.Context.LangOpts.DebugGenericSignatures) {
    func->dumpRef(llvm::errs());
    llvm::errs() << "\n";
    llvm::errs() << "Generic signature: ";
    sig->print(llvm::errs());
    llvm::errs() << "\n";
    llvm::errs() << "Canonical generic signature: ";
    sig->getCanonicalSignature()->print(llvm::errs());
    llvm::errs() << "\n";
  }

  GenericEnvironment *env = sig->createGenericEnvironment();
  func->setGenericEnvironment(env);

  return sig;
}

void TypeChecker::validateGenericFuncSignature(AbstractFunctionDecl *func) {
  GenericSignature *sig = computeGenericFuncSignature(*this, func);

  checkGenericFuncSignature(*this, nullptr, func,
                            TypeResolution::forInterface(func, sig));

  func->computeType();

  // Make sure that there are no unresolved dependent types in the
  // generic signature.
  assert(!func->getInterfaceType()->findUnresolvedDependentMemberType());
}

///
/// Generic subscripts
///
/// FIXME: A lot of this code is duplicated from the generic functions
/// path above. We could consolidate more of this.
///

/// Check the signature of a generic subscript.
static void checkGenericSubscriptSignature(TypeChecker &tc,
                                           GenericSignatureBuilder *builder,
                                           SubscriptDecl *subscript,
                                           TypeResolution resolution) {
  // Check the generic parameter list.
  auto genericParams = subscript->getGenericParams();

  auto *dc = subscript->getDeclContext();

  checkGenericParamList(tc, builder, genericParams,
                        dc->getGenericSignatureOfContext(),
                        resolution);

  // Check the element type.
  tc.validateType(subscript->getElementTypeLoc(), resolution,
                  TypeResolverContext::FunctionResult);

  // Infer requirements from it.
  if (genericParams && builder) {
    auto source =
      GenericSignatureBuilder::FloatingRequirementSource::forInferred(
          subscript->getElementTypeLoc().getTypeRepr());

    builder->inferRequirements(*subscript->getParentModule(),
                               subscript->getElementTypeLoc().getType(),
                               subscript->getElementTypeLoc().getTypeRepr(),
                               source);
  }

  // Check the indices.
  auto params = subscript->getIndices();

  tc.typeCheckParameterList(params, resolution,
                            TypeResolverContext::SubscriptDecl);

  // Infer requirements from the pattern.
  if (builder) {
    builder->inferRequirements(*subscript->getParentModule(), params,
                               genericParams);
  }
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
  auto *dc = subscript->getDeclContext();

  GenericSignature *sig;
  if (auto *gp = subscript->getGenericParams()) {
    gp->setOuterParameters(dc->getGenericParamsOfContext());
    prepareGenericParamList(gp, subscript);

    // Create the generic signature builder.
    GenericSignatureBuilder builder(Context);

    // Type check the function declaration, treating all generic type
    // parameters as dependent, unresolved.
    checkGenericSubscriptSignature(*this, &builder, subscript,
                                   TypeResolution::forStructural(subscript));

    // The generic subscript signature is complete and well-formed. Determine
    // the type of the generic subscript.
    sig =
      std::move(builder).computeGenericSignature(subscript->getLoc());

    // The generic signature builder now has all of the requirements, although
    // there might still be errors that have not yet been diagnosed. Revert the
    // generic function signature and type-check it again, completely.
    revertGenericSubscriptSignature(subscript);

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

  checkGenericSubscriptSignature(*this, nullptr, subscript,
                                 TypeResolution::forInterface(subscript, sig));

  subscript->computeType();
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
  auto nominal = ext->getSelfNominalTypeDecl();
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
    if (recursivelyVisitGenericParams) {
      visitOuterToInner(genericParams,
                        [&](GenericParamList *gpList) {
      auto genericParamsDC = gpList->begin()[0]->getDeclContext();
      TypeResolution structuralResolution =
        TypeResolution::forStructural(genericParamsDC);
        checkGenericParamList(*this, &builder, gpList, nullptr,
                              structuralResolution);
      });
    } else {
      auto genericParamsDC = genericParams->begin()[0]->getDeclContext();
      TypeResolution structuralResolution =
        TypeResolution::forStructural(genericParamsDC);
      checkGenericParamList(*this, &builder, genericParams, parentSig,
                            structuralResolution);
    }

    /// Perform any necessary requirement inference.
    inferRequirements(builder);

    // Record the generic type parameter types and the requirements.
    sig = std::move(builder).computeGenericSignature(
                                         genericParams->getSourceRange().Start,
                                         allowConcreteGenericParams);

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
    sig = ext->getSelfNominalTypeDecl()->getGenericSignatureOfContext();
  }

  if (recursivelyVisitGenericParams) {
    visitOuterToInner(genericParams,
                      [&](GenericParamList *gpList) {
      auto paramsDC = gpList->getParams().front()->getDeclContext();
      TypeResolution interfaceResolution =
        TypeResolution::forInterface(paramsDC, sig);
      checkGenericParamList(*this, nullptr, gpList, nullptr,
                            interfaceResolution);
    });
  } else {
    auto paramsDC = genericParams->getParams().front()->getDeclContext();
    TypeResolution interfaceResolution =
      TypeResolution::forInterface(paramsDC, sig);
    checkGenericParamList(*this, nullptr, genericParams, parentSig,
                          interfaceResolution);
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
  ASTContext &ctx = dc->getASTContext();
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

      case RequirementKind::Superclass: {
        // Superclass requirements.
        // FIXME: Don't use the type checker instance here?
        TypeChecker &tc = static_cast<TypeChecker &>(*ctx.getLazyResolver());
        if (!tc.isSubclassOf(firstType, secondType, dc)) {
          diagnostic = diag::type_does_not_inherit;
          diagnosticNote = diag::type_does_not_inherit_or_conform_requirement;
          requirementFailure = true;
        }
        break;
      }

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
        ctx.Diags.diagnose(loc, diagnostic, owner, firstType, secondType);

        std::string genericParamBindingsText;
        if (!genericParams.empty()) {
          genericParamBindingsText =
            gatherGenericParamBindingsText(
              {rawFirstType, rawSecondType}, genericParams, substitutions);
        }
        ctx.Diags.diagnose(noteLoc, diagnosticNote, rawFirstType, rawSecondType,
                           genericParamBindingsText);

        ParentConditionalConformance::diagnoseConformanceStack(
            ctx.Diags, noteLoc, current.Parents);
      }

      return RequirementCheckResult::Failure;
    }
  }

  if (valid)
    return RequirementCheckResult::Success;
  return RequirementCheckResult::SubstitutionFailure;
}

llvm::Expected<Requirement>
RequirementRequest::evaluate(Evaluator &evaluator,
                             WhereClauseOwner owner,
                             unsigned index,
                             TypeResolutionStage stage) const {
  // Figure out the type resolution.
  TypeResolutionOptions options = TypeResolverContext::GenericRequirement;
  Optional<TypeResolution> resolution;
  switch (stage) {
  case TypeResolutionStage::Structural:
    resolution = TypeResolution::forStructural(owner.dc);
    break;

  case TypeResolutionStage::Interface:
    resolution = TypeResolution::forInterface(owner.dc);
    break;

  case TypeResolutionStage::Contextual:
    llvm_unreachable("No clients care about this. Use mapTypeIntoContext()");
  }

  auto resolveType = [&](TypeLoc &typeLoc) -> Type {
    Type result;
    if (auto typeRepr = typeLoc.getTypeRepr())
      result = resolution->resolveType(typeRepr, options);
    else
      result = typeLoc.getType();

    return result ? result : ErrorType::get(owner.dc->getASTContext());
  };

  auto &reqRepr = getRequirement();
  switch (reqRepr.getKind()) {
  case RequirementReprKind::TypeConstraint: {
    Type subject = resolveType(reqRepr.getSubjectLoc());
    Type constraint = resolveType(reqRepr.getConstraintLoc());
    return Requirement(constraint->getClassOrBoundGenericClass()
                         ? RequirementKind::Superclass
                         : RequirementKind::Conformance,
                       subject, constraint);
  }

  case RequirementReprKind::SameType:
    return Requirement(RequirementKind::SameType,
                       resolveType(reqRepr.getFirstTypeLoc()),
                       resolveType(reqRepr.getSecondTypeLoc()));

  case RequirementReprKind::LayoutConstraint:
    return Requirement(RequirementKind::Layout,
                       resolveType(reqRepr.getSubjectLoc()),
                       reqRepr.getLayoutConstraint());
  }
}
