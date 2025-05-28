//===--- OpenedExistentials.cpp - Utilities for existential types ---------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines common utilities for existential opening and some related
// things, such as the checks around covariant `Self` in class conformances.
//
//===----------------------------------------------------------------------===//

#include "OpenedExistentials.h"
#include "TypeChecker.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/ConformanceLookup.h"
#include "swift/AST/Decl.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/Types.h"
#include "swift/AST/TypeCheckRequests.h"
#include "swift/Basic/Assertions.h"

using namespace swift;

GenericParameterReferenceInfo &
GenericParameterReferenceInfo::operator|=(const GenericParameterReferenceInfo &other) {
  DirectRefs |= other.DirectRefs;
  DepMemberTyRefs |= other.DepMemberTyRefs;
  HasCovariantGenericParamResult |= other.HasCovariantGenericParamResult;

  return *this;
}

/// Forward declaration.
static GenericParameterReferenceInfo
findGenericParameterReferencesRec(CanGenericSignature,
                                  GenericTypeParamType *,
                                  GenericTypeParamType *,
                                  Type, TypePosition, bool);

/// Determine whether a function type with the given result type may have
/// a covariant generic parameter type result. This is true if the result type
/// is either a function type, or a generic parameter, possibly wrapped in some
/// level of optionality.
static bool canResultTypeHaveCovariantGenericParameterResult(Type resultTy) {
  if (resultTy->is<AnyFunctionType>())
    return true;

  resultTy = resultTy->lookThroughAllOptionalTypes();
  return resultTy->is<GenericTypeParamType>();
}

/// Report references to the given generic parameter within the given function
/// type using the given generic signature.
///
/// \param position The current position in terms of variance.
/// \param skipParamIndex The index of the parameter that shall be skipped.
static GenericParameterReferenceInfo findGenericParameterReferencesInFunction(
    CanGenericSignature genericSig,
    GenericTypeParamType *origParam,
    GenericTypeParamType *openedParam,
    const AnyFunctionType *fnType, TypePosition position,
    bool canBeCovariantResult, std::optional<unsigned> skipParamIndex) {
  // If there are no type parameters, we're done.
  if (!isa<GenericFunctionType>(fnType) && !fnType->hasTypeParameter())
    return GenericParameterReferenceInfo();

  auto inputInfo = GenericParameterReferenceInfo();
  const auto params = fnType->getParams();
  for (const auto paramIdx : indices(params)) {
    // If this is the parameter we were supposed to skip, do so.
    if (skipParamIndex && paramIdx == *skipParamIndex)
      continue;

    const auto &param = params[paramIdx];
    // inout types are invariant.
    if (param.isInOut()) {
      inputInfo |= ::findGenericParameterReferencesRec(
          genericSig, origParam, openedParam, param.getPlainType(),
          TypePosition::Invariant, /*canBeCovariantResult=*/false);
      continue;
    }

    // Parameters are contravariant, but if we're prior to the skipped
    // parameter treat them as invariant because we're not allowed to
    // reference the parameter at all.
    TypePosition paramPos = position.flipped();
    if (skipParamIndex && paramIdx < *skipParamIndex)
      paramPos = TypePosition::Invariant;

    inputInfo |= ::findGenericParameterReferencesRec(
        genericSig, origParam, openedParam, param.getParameterType(), paramPos,
        /*canBeCovariantResult=*/false);
  }

  canBeCovariantResult =
      // &= does not short-circuit.
      canBeCovariantResult &&
      canResultTypeHaveCovariantGenericParameterResult(fnType->getResult());

  const auto resultInfo = ::findGenericParameterReferencesRec(
      genericSig, origParam, openedParam, fnType->getResult(),
      position, canBeCovariantResult);

  return inputInfo |= resultInfo;
}

/// Report references to the given generic parameter within the given type
/// using the given generic signature.
///
/// \param position The current position in terms of variance.
static GenericParameterReferenceInfo
findGenericParameterReferencesRec(CanGenericSignature genericSig,
                                  GenericTypeParamType *origParam,
                                  GenericTypeParamType *openedParam,
                                  Type type,
                                  TypePosition position,
                                  bool canBeCovariantResult) {
  // If there are no type parameters, we're done.
  if (!type->getCanonicalType()->hasTypeParameter())
    return GenericParameterReferenceInfo();

  // Tuples preserve variance.
  if (auto tuple = type->getAs<TupleType>()) {
    auto info = GenericParameterReferenceInfo();
    for (auto &elt : tuple->getElements()) {
      info |= findGenericParameterReferencesRec(
          genericSig, origParam, openedParam, elt.getType(), position,
          /*canBeCovariantResult=*/false);
    }

    return info;
  }

  // Function types preserve variance in the result type, and flip variance in
  // the parameter type.
  if (auto funcTy = type->getAs<AnyFunctionType>()) {
    return findGenericParameterReferencesInFunction(
        genericSig, origParam, openedParam, funcTy,
        position, canBeCovariantResult,
        /*skipParamIndex=*/std::nullopt);
  }

  // Metatypes preserve variance.
  if (auto metaTy = type->getAs<AnyMetatypeType>()) {
    return findGenericParameterReferencesRec(genericSig, origParam, openedParam,
                                             metaTy->getInstanceType(),
                                             position, canBeCovariantResult);
  }

  // Optionals preserve variance.
  if (auto optType = type->getOptionalObjectType()) {
    return findGenericParameterReferencesRec(
        genericSig, origParam, openedParam, optType,
        position, canBeCovariantResult);
  }

  // DynamicSelfType preserves variance.
  if (auto selfType = type->getAs<DynamicSelfType>()) {
    return findGenericParameterReferencesRec(genericSig, origParam, openedParam,
                                             selfType->getSelfType(), position,
                                             /*canBeCovariantResult=*/false);
  }

  if (auto *const nominal = type->getAs<NominalOrBoundGenericNominalType>()) {
    auto info = GenericParameterReferenceInfo();

    // Don't forget to look in the parent.
    if (const auto parent = nominal->getParent()) {
      info |= findGenericParameterReferencesRec(
          genericSig, origParam, openedParam, parent, TypePosition::Invariant,
          /*canBeCovariantResult=*/false);
    }

    // Most bound generic types are invariant.
    if (auto *const bgt = type->getAs<BoundGenericType>()) {
      if (bgt->isArray()) {
        // Swift.Array preserves variance in its 'Value' type.
        info |= findGenericParameterReferencesRec(
            genericSig, origParam, openedParam, bgt->getGenericArgs().front(),
            position, /*canBeCovariantResult=*/false);
      } else if (bgt->isDictionary()) {
        // Swift.Dictionary preserves variance in its 'Element' type.
        info |= findGenericParameterReferencesRec(
            genericSig, origParam, openedParam, bgt->getGenericArgs().front(),
            TypePosition::Invariant, /*canBeCovariantResult=*/false);
        info |= findGenericParameterReferencesRec(
            genericSig, origParam, openedParam, bgt->getGenericArgs().back(),
            position, /*canBeCovariantResult=*/false);
      } else {
        for (const auto &paramType : bgt->getGenericArgs()) {
          info |= findGenericParameterReferencesRec(
              genericSig, origParam, openedParam, paramType,
              TypePosition::Invariant, /*canBeCovariantResult=*/false);
        }
      }
    }

    return info;
  }

  // If the signature of an opaque result type has a same-type constraint
  // that references Self, it's invariant.
  if (auto opaque = type->getAs<OpaqueTypeArchetypeType>()) {
    auto info = GenericParameterReferenceInfo();
    auto opaqueSig = opaque->getDecl()->getOpaqueInterfaceGenericSignature();
    for (const auto &req : opaqueSig.getRequirements()) {
      switch (req.getKind()) {
      case RequirementKind::SameShape:
        llvm_unreachable("Same-shape requirement not supported here");

      case RequirementKind::Conformance:
      case RequirementKind::Layout:
        continue;

      case RequirementKind::SameType:
        info |= findGenericParameterReferencesRec(
            genericSig, origParam, openedParam, req.getFirstType(),
            TypePosition::Invariant, /*canBeCovariantResult=*/false);

        LLVM_FALLTHROUGH;

      case RequirementKind::Superclass:
        info |= findGenericParameterReferencesRec(
            genericSig, origParam, openedParam, req.getSecondType(),
            TypePosition::Invariant, /*canBeCovariantResult=*/false);
        break;
      }
    }

    return info;
  }

  if (auto *existential = type->getAs<ExistentialType>())
    type = existential->getConstraintType();

  // Protocol compositions are invariant.
  if (auto *comp = type->getAs<ProtocolCompositionType>()) {
    auto info = GenericParameterReferenceInfo();

    for (auto member : comp->getMembers()) {
      info |= findGenericParameterReferencesRec(
          genericSig, origParam, openedParam, member,
          TypePosition::Invariant, /*canBeCovariantResult=*/false);
    }

    return info;
  }

  // Packs are invariant.
  if (auto *pack = type->getAs<PackType>()) {
    auto info = GenericParameterReferenceInfo();

    // FIXME: Source compatibility remedy to allow existential opening in
    // the following case:
    // ```
    // protocol P {}
    // struct S<each T> {}
    // func foo<T: P>(_: T, _: S<T>? = nil) {}
    // let p: any P
    // foo(p)
    // ```
    //
    // for (auto arg : pack->getElementTypes()) {
    //   info |= findGenericParameterReferencesRec(
    //       genericSig, origParam, openedParam, arg,
    //       TypePosition::Invariant, /*canBeCovariantResult=*/false);
    // }
    (void)pack;

    return info;
  }

  // Pack expansions are invariant.
  if (auto *expansion = type->getAs<PackExpansionType>()) {
    return findGenericParameterReferencesRec(
        genericSig, origParam, openedParam, expansion->getPatternType(),
        TypePosition::Invariant, /*canBeCovariantResult=*/false);
  }

  // Specifically ignore parameterized protocols because we can erase them to
  // the upper bound.
  if (type->is<ParameterizedProtocolType>()) {
    return GenericParameterReferenceInfo();
  }

  // Everything else should be a type parameter.
  if (!type->isTypeParameter()) {
    ABORT([&](auto &out) {
      out << "Unhandled type:\n";
      type->dump(out);
    });
  }

  if (!type->getRootGenericParam()->isEqual(origParam)) {
    return GenericParameterReferenceInfo();
  }

  // A direct reference to 'Self'.
  if (type->is<GenericTypeParamType>()) {
    if (position == TypePosition::Covariant && canBeCovariantResult)
      return GenericParameterReferenceInfo::forCovariantGenericParamResult();

    return GenericParameterReferenceInfo::forDirectRef(position);
  }

  if (origParam != openedParam) {
    // Replace the original parameter with the parameter in the opened
    // signature.
    type = type.subst(
      [&](SubstitutableType *type) {
        ASSERT(type->isEqual(origParam));
        return openedParam;
      },
      MakeAbstractConformanceForGenericType());
  }

  if (genericSig) {
    // If the type parameter is beyond the domain of the opened
    // signature, ignore it.
    if (!genericSig->isValidTypeParameter(type)) {
      return GenericParameterReferenceInfo();
    }

    if (auto reducedTy = genericSig.getReducedType(type)) {
      if (!reducedTy->isEqual(type)) {
        // Note: origParam becomes openedParam for the recursive call,
        // because concreteTy is written in terms of genericSig and not
        // the signature of the old origParam.
        return findGenericParameterReferencesRec(
            CanGenericSignature(), openedParam, openedParam, reducedTy,
            position, canBeCovariantResult);
      }
    }
  }

  // A reference to an associated type rooted on 'Self'.
  return GenericParameterReferenceInfo::forDependentMemberTypeRef(position);
}

GenericParameterReferenceInfo
swift::findGenericParameterReferences(const ValueDecl *value,
                                      CanGenericSignature sig,
                                      GenericTypeParamType *origParam,
                                      GenericTypeParamType *openedParam,
                                      std::optional<unsigned> skipParamIndex) {
  if (isa<TypeDecl>(value))
    return GenericParameterReferenceInfo();

  auto type = value->getInterfaceType();

  // Skip invalid declarations.
  if (type->hasError())
    return GenericParameterReferenceInfo();

  // For functions and subscripts, take skipParamIndex into account.
  if (isa<AbstractFunctionDecl>(value) || isa<SubscriptDecl>(value)) {
    // And for a method, skip the 'self' parameter.
    if (value->hasCurriedSelf())
      type = type->castTo<AnyFunctionType>()->getResult();

    return ::findGenericParameterReferencesInFunction(
        sig, origParam, openedParam, type->castTo<AnyFunctionType>(),
        TypePosition::Covariant, /*canBeCovariantResult=*/true,
        skipParamIndex);
  }

  return ::findGenericParameterReferencesRec(sig, origParam, openedParam, type,
                                             TypePosition::Covariant,
                                             /*canBeCovariantResult=*/true);
}

GenericParameterReferenceInfo swift::findExistentialSelfReferences(
    const ValueDecl *value) {
  auto *dc = value->getDeclContext();
  ASSERT(dc->getSelfProtocolDecl());

  auto sig = dc->getGenericSignatureOfContext().getCanonicalSignature();
  auto genericParam = dc->getSelfInterfaceType()->castTo<GenericTypeParamType>();

  return findGenericParameterReferences(value, sig, genericParam, genericParam,
                                        std::nullopt);
}

bool HasSelfOrAssociatedTypeRequirementsRequest::evaluate(
    Evaluator &evaluator, ProtocolDecl *decl) const {
  // ObjC protocols do not require `any`.
  if (decl->isObjC())
    return false;

  for (auto member : decl->getMembers()) {
    // Existential types require `any` if the protocol has an associated type.
    if (isa<AssociatedTypeDecl>(member))
      return true;

    // For value members, look at their type signatures.
    if (auto valueMember = dyn_cast<ValueDecl>(member)) {
      const auto info = findExistentialSelfReferences(valueMember);
      if (info.hasNonCovariantRef() || info.hasDependentMemberTypeRef()) {
        return true;
      }
    }
  }

  // Check whether any of the inherited protocols require `any`.
  for (auto proto : decl->getInheritedProtocols()) {
    if (proto->hasSelfOrAssociatedTypeRequirements())
      return true;
  }

  return false;
}

/// A protocol member accessed with an existential value might have generic
/// constraints that require the ability to spell an opened archetype in order
/// to be satisfied. Such are
/// - superclass requirements, when the object is a non-'Self'-rooted type
///   parameter, and the subject is dependent on 'Self', e.g. U : G<Self.A>
/// - same-type requirements, when one side is dependent on 'Self', and the
///   other is a non-'Self'-rooted type parameter, e.g. U.Element == Self.
///
/// Because opened archetypes are not part of the surface language, these
/// constraints render the member inaccessible.
static bool doesMemberHaveUnfulfillableConstraintsWithExistentialBase(
    OpenedExistentialSignature existentialSig, const ValueDecl *member) {
  const auto sig =
      member->getInnermostDeclContext()->getGenericSignatureOfContext();

  // Fast path: the member is generic only over 'Self'.
  if (sig.getGenericParams().size() == 1) {
    return false;
  }

  class IsDependentOnOpenedExistentialSelf : public TypeWalker {
    OpenedExistentialSignature existentialSig;

  public:
    explicit IsDependentOnOpenedExistentialSelf(OpenedExistentialSignature existentialSig)
        : existentialSig(existentialSig) {}

    Action walkToTypePre(Type ty) override {
      // We're looking at the interface type of a protocol member, so it's written
      // in terms of `Self` (tau_0_0) and possibly type parameters at higher depth:
      //
      // <Self, ... where Self: P, ...>
      if (!ty->isTypeParameter()) {
        return Action::Continue;
      }

      if (ty->getRootGenericParam()->getDepth() > 0) {
        return Action::SkipNode;
      }

      // Ok, we found a type parameter rooted in `Self`. Replace `Self` with the
      // opened Self type in the existential signature, which looks like this:
      //
      // <..., Self where ..., Self: P>
      ty = ty.subst(
        [&](SubstitutableType *type) -> Type {
          return existentialSig.SelfType;
        },
        MakeAbstractConformanceForGenericType());

      // Make sure this is valid first.
      if (!existentialSig.OpenedSig->isValidTypeParameter(ty)) {
        return Action::SkipNode;
      }

      // If the existential type constrains Self.U to a type from the outer
      // context, then the reduced type of Self.U in the existential signature
      // will no longer contain Self.
      ty = existentialSig.OpenedSig.getReducedType(ty);

      if (!ty.findIf([&](Type t) -> bool {
          if (auto *paramTy = t->getAs<GenericTypeParamType>())
            return paramTy->isEqual(existentialSig.SelfType);
          return false;
        })) {
        return Action::SkipNode;
      }

      // Ok, we found a type that depends on the opened existential Self.
      return Action::Stop;
    }
  } isDependentOnSelf(existentialSig);

  for (const auto &req : sig.getRequirements()) {
    switch (req.getKind()) {
    case RequirementKind::Superclass: {
      if (req.getFirstType()->getRootGenericParam()->getDepth() > 0 &&
          req.getSecondType().walk(isDependentOnSelf)) {
        return true;
      }

      break;
    }
    case RequirementKind::SameType:
    case RequirementKind::SameShape: {
      const auto isNonSelfRootedTypeParam = [](Type ty) {
        return ty->isTypeParameter() &&
               ty->getRootGenericParam()->getDepth() > 0;
      };

      if ((isNonSelfRootedTypeParam(req.getFirstType()) &&
           req.getSecondType().walk(isDependentOnSelf)) ||
          (isNonSelfRootedTypeParam(req.getSecondType()) &&
           req.getFirstType().walk(isDependentOnSelf))) {
        return true;
      }

      break;
    }
    case RequirementKind::Conformance:
    case RequirementKind::Layout:
      break;
    }
  }

  return false;
}

ExistentialMemberAccessLimitation
swift::isMemberAvailableOnExistential(Type baseTy, const ValueDecl *member) {
  auto *dc = member->getDeclContext();
  if (!dc->getSelfProtocolDecl()) {
    return ExistentialMemberAccessLimitation::None;
  }

  auto &ctx = member->getASTContext();
  auto existentialSig = ctx.getOpenedExistentialSignature(baseTy);

  auto origParam = dc->getSelfInterfaceType()->castTo<GenericTypeParamType>();
  auto openedParam = existentialSig.SelfType->castTo<GenericTypeParamType>();

  // An accessor or non-storage member is not available if its interface type
  // contains a non-covariant reference to a 'Self'-rooted type parameter in the
  // context of the base type's existential signature.
  auto info = findGenericParameterReferences(
      member, existentialSig.OpenedSig, origParam, openedParam,
      std::nullopt);

  auto result = ExistentialMemberAccessLimitation::None;
  if (!info) {
    // Nothing to do.
  } else if (info.hasRef(TypePosition::Invariant)) {
    // An invariant reference is decisive.
    result = ExistentialMemberAccessLimitation::Unsupported;
  } else if (isa<AbstractFunctionDecl>(member)) {
    // Anything non-covariant is decisive for functions.
    if (info.hasRef(TypePosition::Contravariant)) {
      result = ExistentialMemberAccessLimitation::Unsupported;
    }
  } else {
    const auto isGetterUnavailable = info.hasRef(TypePosition::Contravariant);
    auto isSetterUnavailable = true;

    if (isa<VarDecl>(member)) {
      // For properties, the setter is unavailable if the interface type has a
      // covariant reference, which becomes contravariant is the setter.
      isSetterUnavailable = info.hasRef(TypePosition::Covariant);
    } else {
      // For subscripts specifically, we must scan the setter directly because
      // whether a covariant reference in the interface type becomes
      // contravariant in the setter depends on the location of the reference
      // (in the indices or the result type).
      auto *setter =
          cast<SubscriptDecl>(member)->getAccessor(AccessorKind::Set);
      const auto setterInfo = setter ? findGenericParameterReferences(
                                           setter, existentialSig.OpenedSig,
                                           origParam, openedParam, std::nullopt)
                                     : GenericParameterReferenceInfo();

      isSetterUnavailable = setterInfo.hasRef(TypePosition::Contravariant);
    }

    if (isGetterUnavailable && isSetterUnavailable) {
      result = ExistentialMemberAccessLimitation::Unsupported;
    } else if (isGetterUnavailable) {
      result = ExistentialMemberAccessLimitation::WriteOnly;
    } else if (isSetterUnavailable) {
      result = ExistentialMemberAccessLimitation::ReadOnly;
    }
  }

  // If the member access is not supported whatsoever, we are done.
  if (result == ExistentialMemberAccessLimitation::Unsupported)
    return result;

  // Before proceeding with the result, see if we find a generic requirement
  // that cannot be satisfied; if we do, the member is unavailable after all.
  if (doesMemberHaveUnfulfillableConstraintsWithExistentialBase(existentialSig,
                                                                member)) {
    return ExistentialMemberAccessLimitation::Unsupported;
  }

  return result;
}

std::optional<std::pair<TypeVariableType *, Type>>
swift::canOpenExistentialCallArgument(ValueDecl *callee, unsigned paramIdx,
                                      Type paramTy, Type argTy) {
  if (!callee)
    return std::nullopt;

  // Only applies to functions and subscripts.
  if (!isa<AbstractFunctionDecl>(callee) && !isa<SubscriptDecl>(callee))
    return std::nullopt;

  // Special semantics prohibit opening existentials.
  switch (TypeChecker::getDeclTypeCheckingSemantics(callee)) {
  case DeclTypeCheckingSemantics::OpenExistential:
  case DeclTypeCheckingSemantics::TypeOf:
    // type(of:) and _openExistential handle their own opening.
    return std::nullopt;

  case DeclTypeCheckingSemantics::Normal:
  case DeclTypeCheckingSemantics::WithoutActuallyEscaping:
    break;
  }

  // C++ function templates require specialization, which is not possible with
  // opened existential archetypes, so do not open.
  if (isa_and_nonnull<clang::FunctionTemplateDecl>(callee->getClangDecl()))
    return std::nullopt;

  // The actual parameter type needs to involve a type variable, otherwise
  // type inference won't be possible.
  if (!paramTy->hasTypeVariable())
    return std::nullopt;

  auto param = getParameterAt(callee, paramIdx);
  if (!param)
    return std::nullopt;

  // If the parameter is non-generic variadic, don't open.
  if (param->isVariadic())
    return std::nullopt;

  // The rvalue argument type needs to be an existential type or metatype
  // thereof.
  const auto rValueArgTy = argTy->getWithoutSpecifierType();
  if (!rValueArgTy->isAnyExistentialType())
    return std::nullopt;

  GenericTypeParamType *genericParam;
  TypeVariableType *typeVar;
  Type bindingTy;

  std::tie(genericParam, typeVar, bindingTy) = [=] {
    // Look through an inout and optional type.
    Type genericParam = param->getInterfaceType()
                            ->getInOutObjectType()
                            ->lookThroughSingleOptionalType();
    Type typeVar =
        paramTy->getInOutObjectType()->lookThroughSingleOptionalType();

    Type bindingTy = rValueArgTy;

    // Look through a metatype.
    if (genericParam->is<AnyMetatypeType>()) {
      genericParam = genericParam->getMetatypeInstanceType();
      typeVar = typeVar->getMetatypeInstanceType();
      bindingTy = bindingTy->getMetatypeInstanceType();
    }

    return std::tuple(genericParam->getAs<GenericTypeParamType>(),
                      typeVar->getAs<TypeVariableType>(), bindingTy);
  }();

  // The should have reached a type variable and corresponding generic
  // parameter.
  if (!typeVar || !genericParam)
    return std::nullopt;

  // Only allow opening the innermost generic parameters.
  auto genericContext = callee->getAsGenericContext();
  if (!genericContext || !genericContext->isGeneric())
    return std::nullopt;

  auto genericSig = callee->getInnermostDeclContext()
      ->getGenericSignatureOfContext().getCanonicalSignature();
  if (genericParam->getDepth() < genericSig->getMaxDepth())
    return std::nullopt;

  // The binding could be an existential metatype. Get the instance type for
  // conformance checks and to build an opened existential signature. If the
  // instance type is not an existential type, i.e., the metatype is nested,
  // bail out.
  const Type existentialTy = bindingTy->getMetatypeInstanceType();
  if (!existentialTy->isExistentialType())
    return std::nullopt;

  auto &ctx = callee->getASTContext();

  // If the existential argument conforms to all of protocol requirements on
  // the formal parameter's type, don't open unless ImplicitOpenExistentials is
  // enabled.

  // If all of the conformance requirements on the formal parameter's type
  // are self-conforming, don't open.
  if (!ctx.LangOpts.hasFeature(Feature::ImplicitOpenExistentials)) {
    bool containsNonSelfConformance = false;
    for (auto proto : genericSig->getRequiredProtocols(genericParam)) {
      auto conformance = lookupExistentialConformance(
          existentialTy, proto);
      if (conformance.isInvalid()) {
        containsNonSelfConformance = true;
        break;
      }
    }

    if (!containsNonSelfConformance)
      return std::nullopt;
  }

  auto existentialSig = ctx.getOpenedExistentialSignature(existentialTy);

  // Ensure that the formal parameter is only used in covariant positions,
  // because it won't match anywhere else.
  auto referenceInfo = findGenericParameterReferences(
      callee, existentialSig.OpenedSig, genericParam,
      existentialSig.SelfType->castTo<GenericTypeParamType>(),
      /*skipParamIdx=*/paramIdx);
  if (referenceInfo.hasNonCovariantRef())
    return std::nullopt;

  return std::pair(typeVar, bindingTy);
}

/// For each occurrence of a type **type** in `refTy` that satisfies
/// `predicateFn` in covariant position, **type** is erased to an
/// existential using `eraseFn`.
static Type typeEraseExistentialSelfReferences(
    Type refTy, TypePosition outermostPosition,
    llvm::function_ref<bool(Type)> containsFn,
    llvm::function_ref<bool(Type)> predicateFn,
    llvm::function_ref<Type(Type, TypePosition)> eraseFn) {
  if (!containsFn(refTy))
    return refTy;

  return refTy.transformWithPosition(
      outermostPosition,
      [&](TypeBase *t, TypePosition currPos) -> std::optional<Type> {
        if (!containsFn(t)) {
          return Type(t);
        }

        if (t->is<MetatypeType>()) {
          const auto instanceTy = t->getMetatypeInstanceType();
          auto erasedTy = typeEraseExistentialSelfReferences(
              instanceTy, currPos,
              containsFn, predicateFn, eraseFn);
          if (instanceTy.getPointer() == erasedTy.getPointer()) {
            return Type(t);
          }

          // - If the output instance type is an existential, but the input is
          //   not, wrap the output in an existential metatype.
          //
          //     X.Type → X → any Y → any Y.Type
          //
          // - Otherwise, both are existential or the output instance type is
          //   not existential; wrap the output in a singleton metatype.
          if (erasedTy->isAnyExistentialType() &&
              !erasedTy->isConstraintType() &&
              !(instanceTy->isAnyExistentialType() &&
                !instanceTy->isConstraintType())) {
            return Type(ExistentialMetatypeType::get(erasedTy));
          }

          return Type(MetatypeType::get(erasedTy));
        }

        // Opaque types whose substitutions involve this type parameter are
        // erased to their upper bound.
        if (auto opaque = dyn_cast<OpaqueTypeArchetypeType>(t)) {
          for (auto replacementType :
               opaque->getSubstitutions().getReplacementTypes()) {
            auto erasedReplacementType = typeEraseExistentialSelfReferences(
                replacementType, TypePosition::Covariant,
                containsFn, predicateFn, eraseFn);
            if (erasedReplacementType.getPointer() !=
                replacementType.getPointer())
              return opaque->getExistentialType();
          }
        }

        // Parameterized protocol types whose arguments involve this type
        // parameter are erased to the base type.
        if (auto parameterized = dyn_cast<ParameterizedProtocolType>(t)) {
          for (auto argType : parameterized->getArgs()) {
            auto erasedArgType = typeEraseExistentialSelfReferences(
                argType, TypePosition::Covariant,
                containsFn, predicateFn, eraseFn);
            if (erasedArgType.getPointer() != argType.getPointer())
              return parameterized->getBaseType();
          }
        }

        if (!predicateFn(t)) {
          // Recurse.
          return std::nullopt;
        }

        auto erasedTy = eraseFn(t, currPos);
        if (!erasedTy)
          return Type(t);

        return erasedTy;
      });
}

Type swift::typeEraseOpenedExistentialReference(
    Type type, Type existentialBaseType, TypeVariableType *openedTypeVar,
    TypePosition outermostPosition) {
  auto existentialSig =
    type->getASTContext().getOpenedExistentialSignature(
      existentialBaseType);

  auto applyOuterSubstitutions = [&](Type t) -> Type {
    if (t->hasTypeParameter()) {
      if (auto outerSubs = existentialSig.Generalization) {
        unsigned depth = existentialSig.OpenedSig->getMaxDepth();
        OuterSubstitutions replacer{outerSubs, depth};
        return t.subst(replacer, replacer);
      }
    }

    return t;
  };

  auto erase = [&](Type paramTy, TypePosition currPos) -> Type {
    switch (currPos) {
    case TypePosition::Covariant:
      break;

    case TypePosition::Contravariant:
    case TypePosition::Invariant:
    case TypePosition::Shape:
      return Type();
    }

    // The upper bounds of 'Self' is the existential base type.
    if (paramTy->is<GenericTypeParamType>())
      return existentialBaseType;

    return applyOuterSubstitutions(
        existentialSig.OpenedSig->getExistentialType(paramTy));
  };

  return typeEraseExistentialSelfReferences(
      type,
      outermostPosition,
      /*containsFn=*/[](Type t) {
        return t->hasTypeVariable();
      },
      /*predicateFn=*/[](Type t) {
        return t->isTypeVariableOrMember();
      },
      /*eraseFn=*/[&](Type t, TypePosition currPos) -> Type {
        bool found = false;
        auto paramTy = t.transformRec([&](Type t) -> std::optional<Type> {
          if (t.getPointer() == openedTypeVar) {
            found = true;
            return existentialSig.SelfType;
          }
          return std::nullopt;
        });

        if (!found)
          return Type();

        assert(paramTy->isTypeParameter());

        // This can happen with invalid code.
        if (!existentialSig.OpenedSig->isValidTypeParameter(paramTy)) {
          return Type(t);
        }

        // Check if this existential fixes this `Self`-rooted type to something
        // in the existential's outer generic signature.
        Type reducedTy = existentialSig.OpenedSig.getReducedType(paramTy);
        if (!reducedTy->isEqual(paramTy)) {
          reducedTy = applyOuterSubstitutions(reducedTy);

          auto erasedTy = typeEraseExistentialSelfReferences(
              reducedTy, currPos,
              [&](Type t) { return t->hasTypeParameter(); },
              [&](Type t) { return t->isTypeParameter(); },
              [&](Type t, TypePosition currPos) { return erase(t, currPos); });
          if (erasedTy.getPointer() == reducedTy.getPointer()) {
            return Type(t);
          }

          return erasedTy;
        }

        return erase(paramTy, currPos);
      });
}

Type swift::typeEraseOpenedArchetypesFromEnvironment(
    Type type, GenericEnvironment *env) {
  assert(env->getKind() == GenericEnvironment::Kind::OpenedExistential);

  return typeEraseExistentialSelfReferences(
      type,
      TypePosition::Covariant,
      /*containsFn=*/[](Type t) {
        return t->hasOpenedExistential();
      },
      /*predicateFn=*/[](Type t) {
        return t->is<ExistentialArchetypeType>();
      },
      /*eraseFn=*/[&](Type t, TypePosition currPos) {
        auto *openedTy = t->castTo<ExistentialArchetypeType>();
        if (openedTy->getGenericEnvironment() == env)
          return openedTy->getExistentialType();

        return Type();
      });
}
