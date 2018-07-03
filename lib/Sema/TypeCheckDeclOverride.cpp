//===--- TypeCheckOverride.cpp - Override Checking ------------------------===//
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
// This file implements semantic analysis for declaration overrides.
//
//===----------------------------------------------------------------------===//
#include "TypeChecker.h"
#include "CodeSynthesis.h"
#include "MiscDiagnostics.h"
#include "swift/AST/ASTVisitor.h"
#include "swift/AST/Decl.h"
#include "swift/AST/ParameterList.h"
using namespace swift;

static void adjustFunctionTypeForOverride(Type &type) {
  // Drop 'throws'.
  // FIXME: Do we want to allow overriding a function returning a value
  // with one returning Never?
  auto fnType = type->castTo<AnyFunctionType>();
  auto extInfo = fnType->getExtInfo();
  extInfo = extInfo.withThrows(false);
  if (fnType->getExtInfo() != extInfo)
    type = fnType->withExtInfo(extInfo);
}

/// Drop the optionality of the result type of the given function type.
static Type dropResultOptionality(Type type, unsigned uncurryLevel) {
  // We've hit the result type.
  if (uncurryLevel == 0) {
    if (auto objectTy = type->getOptionalObjectType())
      return objectTy;

    return type;
  }

  // Determine the input and result types of this function.
  auto fnType = type->castTo<AnyFunctionType>();
  auto parameters = fnType->getParams();
  Type resultType =
      dropResultOptionality(fnType->getResult(), uncurryLevel - 1);

  // Produce the resulting function type.
  if (auto genericFn = dyn_cast<GenericFunctionType>(fnType)) {
    return GenericFunctionType::get(genericFn->getGenericSignature(),
                                    parameters, resultType,
                                    fnType->getExtInfo());
  }

  return FunctionType::get(parameters, resultType, fnType->getExtInfo());
}

Type swift::getMemberTypeForComparison(ASTContext &ctx, ValueDecl *member,
                                       ValueDecl *derivedDecl,
                                       bool stripLabels) {
  auto *method = dyn_cast<AbstractFunctionDecl>(member);
  ConstructorDecl *ctor = nullptr;
  if (method)
    ctor = dyn_cast<ConstructorDecl>(method);

  auto abstractStorage = dyn_cast<AbstractStorageDecl>(member);
  assert((method || abstractStorage) && "Not a method or abstractStorage?");
  SubscriptDecl *subscript = nullptr;
  if (abstractStorage)
    subscript = dyn_cast<SubscriptDecl>(abstractStorage);

  auto memberType = member->getInterfaceType();
  if (derivedDecl) {
    auto *dc = derivedDecl->getDeclContext();
    auto owningType = dc->getDeclaredInterfaceType();
    assert(owningType);

    memberType = owningType->adjustSuperclassMemberDeclType(member, derivedDecl,
                                                            memberType);
    if (memberType->hasError())
      return memberType;
  }

  if (stripLabels)
    memberType = memberType->getUnlabeledType(ctx);

  if (method) {
    // For methods, strip off the 'Self' type.
    memberType = memberType->castTo<AnyFunctionType>()->getResult();
    adjustFunctionTypeForOverride(memberType);
  } else if (subscript) {
    // For subscripts, we don't have a 'Self' type, but turn it
    // into a monomorphic function type.
    auto funcTy = memberType->castTo<AnyFunctionType>();
    memberType = FunctionType::get(funcTy->getParams(), funcTy->getResult(),
                                   FunctionType::ExtInfo());
  } else {
    // For properties, strip off ownership.
    memberType = memberType->getReferenceStorageReferent();
  }

  // Ignore the optionality of initializers when comparing types;
  // we'll enforce this separately
  if (ctor) {
    memberType = dropResultOptionality(memberType, 1);
  }

  return memberType;
}

bool swift::isOverrideBasedOnType(ValueDecl *decl, Type declTy,
                                  ValueDecl *parentDecl, Type parentDeclTy) {
  auto *genericSig =
      decl->getInnermostDeclContext()->getGenericSignatureOfContext();

  auto canDeclTy = declTy->getCanonicalType(genericSig);
  auto canParentDeclTy = parentDeclTy->getCanonicalType(genericSig);

  auto declIUOAttr =
      decl->getAttrs().hasAttribute<ImplicitlyUnwrappedOptionalAttr>();
  auto parentDeclIUOAttr =
      parentDecl->getAttrs().hasAttribute<ImplicitlyUnwrappedOptionalAttr>();

  if (declIUOAttr != parentDeclIUOAttr)
    return false;

  // If this is a constructor, let's compare only parameter types.
  if (isa<ConstructorDecl>(decl)) {
    auto fnType1 = declTy->castTo<AnyFunctionType>();
    auto fnType2 = parentDeclTy->castTo<AnyFunctionType>();
    return AnyFunctionType::equalParams(fnType1->getParams(),
                                        fnType2->getParams());
  }

  return canDeclTy == canParentDeclTy;
}

/// Perform basic checking to determine whether a declaration can override a
/// declaration in a superclass.
static bool areOverrideCompatibleSimple(ValueDecl *decl,
                                        ValueDecl *parentDecl) {
  // If the number of argument labels does not match, these overrides cannot
  // be compatible.
  if (decl->getFullName().getArgumentNames().size() !=
        parentDecl->getFullName().getArgumentNames().size())
    return false;

  if (auto func = dyn_cast<FuncDecl>(decl)) {
    // Specific checking for methods.
    auto parentFunc = cast<FuncDecl>(parentDecl);
    if (func->isStatic() != parentFunc->isStatic())
      return false;
    if (func->isGeneric() != parentFunc->isGeneric())
      return false;
  } else if (auto ctor = dyn_cast<ConstructorDecl>(decl)) {
    auto parentCtor = cast<ConstructorDecl>(parentDecl);
    if (ctor->isGeneric() != parentCtor->isGeneric())
      return false;
  } else if (auto var = dyn_cast<VarDecl>(decl)) {
    auto parentVar = cast<VarDecl>(parentDecl);
    if (var->isStatic() != parentVar->isStatic())
      return false;
  } else if (auto subscript = dyn_cast<SubscriptDecl>(decl)) {
    auto parentSubscript = cast<SubscriptDecl>(parentDecl);
    if (subscript->isGeneric() != parentSubscript->isGeneric())
      return false;
  }

  return true;
}

static bool
diagnoseMismatchedOptionals(TypeChecker &TC, const ValueDecl *member,
                            const ParameterList *params, TypeLoc resultTL,
                            const ValueDecl *parentMember,
                            const ParameterList *parentParams, Type owningTy,
                            bool treatIUOResultAsError) {
  bool emittedError = false;
  Type plainParentTy = owningTy->adjustSuperclassMemberDeclType(
      parentMember, member, parentMember->getInterfaceType());
  const auto *parentTy = plainParentTy->castTo<FunctionType>();
  if (isa<AbstractFunctionDecl>(parentMember))
    parentTy = parentTy->getResult()->castTo<FunctionType>();

  // Check the parameter types.
  auto checkParam = [&](const ParamDecl *decl, const ParamDecl *parentDecl) {
    Type paramTy = decl->getType();
    Type parentParamTy = parentDecl->getType();

    if (!paramTy || !parentParamTy)
      return;

    TypeLoc TL = decl->getTypeLoc();
    if (!TL.getTypeRepr())
      return;

    bool paramIsOptional =  (bool) paramTy->getOptionalObjectType();
    bool parentIsOptional = (bool) parentParamTy->getOptionalObjectType();

    if (paramIsOptional == parentIsOptional)
      return;

    if (!paramIsOptional) {
      if (parentDecl->getAttrs()
              .hasAttribute<ImplicitlyUnwrappedOptionalAttr>())
        if (!treatIUOResultAsError)
          return;

      emittedError = true;
      auto diag = TC.diagnose(decl->getStartLoc(),
                              diag::override_optional_mismatch,
                              member->getDescriptiveKind(),
                              isa<SubscriptDecl>(member),
                              parentParamTy, paramTy);
      if (TL.getTypeRepr()->isSimple()) {
        diag.fixItInsertAfter(TL.getSourceRange().End, "?");
      } else {
        diag.fixItInsert(TL.getSourceRange().Start, "(");
        diag.fixItInsertAfter(TL.getSourceRange().End, ")?");
      }
      return;
    }

    if (!decl->getAttrs().hasAttribute<ImplicitlyUnwrappedOptionalAttr>())
      return;

    // Allow silencing this warning using parens.
    if (TL.getType()->hasParenSugar())
      return;

    TC.diagnose(decl->getStartLoc(), diag::override_unnecessary_IUO,
                member->getDescriptiveKind(), parentParamTy, paramTy)
      .highlight(TL.getSourceRange());

    auto sugaredForm =
      dyn_cast<ImplicitlyUnwrappedOptionalTypeRepr>(TL.getTypeRepr());
    if (sugaredForm) {
      TC.diagnose(sugaredForm->getExclamationLoc(),
                  diag::override_unnecessary_IUO_remove)
        .fixItRemove(sugaredForm->getExclamationLoc());
    }

    TC.diagnose(TL.getSourceRange().Start,
                diag::override_unnecessary_IUO_silence)
      .fixItInsert(TL.getSourceRange().Start, "(")
      .fixItInsertAfter(TL.getSourceRange().End, ")");
  };

  // FIXME: If we ever allow argument reordering, this is incorrect.
  ArrayRef<ParamDecl *> sharedParams = params->getArray();
  ArrayRef<ParamDecl *> sharedParentParams = parentParams->getArray();
  assert(sharedParams.size() == sharedParentParams.size());
  for_each(sharedParams, sharedParentParams, checkParam);

  if (!resultTL.getTypeRepr())
    return emittedError;

  auto checkResult = [&](TypeLoc resultTL, Type parentResultTy) {
    Type resultTy = resultTL.getType();
    if (!resultTy || !parentResultTy)
      return;

    if (!resultTy->getOptionalObjectType())
      return;

    TypeRepr *TR = resultTL.getTypeRepr();

    bool resultIsPlainOptional = true;
    if (member->getAttrs().hasAttribute<ImplicitlyUnwrappedOptionalAttr>())
      resultIsPlainOptional = false;

    if (resultIsPlainOptional || treatIUOResultAsError) {
      if (parentResultTy->getOptionalObjectType())
        return;
      emittedError = true;
      auto diag = TC.diagnose(resultTL.getSourceRange().Start,
                              diag::override_optional_result_mismatch,
                              member->getDescriptiveKind(),
                              isa<SubscriptDecl>(member),
                              parentResultTy, resultTy);
      if (auto optForm = dyn_cast<OptionalTypeRepr>(TR)) {
        diag.fixItRemove(optForm->getQuestionLoc());
      } else if (auto iuoForm =
          dyn_cast<ImplicitlyUnwrappedOptionalTypeRepr>(TR)) {
        diag.fixItRemove(iuoForm->getExclamationLoc());
      }
      return;
    }

    if (!parentResultTy->getOptionalObjectType())
      return;

    // Allow silencing this warning using parens.
    if (resultTy->hasParenSugar())
      return;

    TC.diagnose(resultTL.getSourceRange().Start,
                diag::override_unnecessary_result_IUO,
                member->getDescriptiveKind(), parentResultTy, resultTy)
      .highlight(resultTL.getSourceRange());

    auto sugaredForm = dyn_cast<ImplicitlyUnwrappedOptionalTypeRepr>(TR);
    if (sugaredForm) {
      TC.diagnose(sugaredForm->getExclamationLoc(),
                  diag::override_unnecessary_IUO_use_strict)
        .fixItReplace(sugaredForm->getExclamationLoc(), "?");
    }

    TC.diagnose(resultTL.getSourceRange().Start,
                diag::override_unnecessary_IUO_silence)
      .fixItInsert(resultTL.getSourceRange().Start, "(")
      .fixItInsertAfter(resultTL.getSourceRange().End, ")");
  };

  checkResult(resultTL, parentTy->getResult());
  return emittedError;
}

/// Make sure that there is an invalid 'override' attribute on the
/// given declaration.
static void makeInvalidOverrideAttr(TypeChecker &TC, ValueDecl *decl) {
  if (auto overrideAttr = decl->getAttrs().getAttribute<OverrideAttr>()) {
    overrideAttr->setInvalid();
  } else {
    auto attr = new (TC.Context) OverrideAttr(true);
    decl->getAttrs().add(attr);
    attr->setInvalid();
  }

  if (auto storage = dyn_cast<AbstractStorageDecl>(decl)) {
    if (auto getter = storage->getGetter())
      makeInvalidOverrideAttr(TC, getter);
    if (auto setter = storage->getSetter())
      makeInvalidOverrideAttr(TC, setter);
  }
}

/// Record that the \c overriding declarations overrides the
/// \c overridden declaration.
///
/// \returns true if an error occurred.
static bool recordOverride(TypeChecker &TC, ValueDecl *override,
                           ValueDecl *base, bool isKnownObjC = false);

/// If the difference between the types of \p decl and \p base is something
/// we feel confident about fixing (even partially), emit a note with fix-its
/// attached. Otherwise, no note will be emitted.
///
/// \returns true iff a diagnostic was emitted.
static bool noteFixableMismatchedTypes(TypeChecker &TC, ValueDecl *decl,
                                       const ValueDecl *base) {
  DiagnosticTransaction tentativeDiags(TC.Diags);

  {
    Type baseTy = base->getInterfaceType();
    if (baseTy->hasError())
      return false;

    Optional<InFlightDiagnostic> activeDiag;
    if (auto *baseInit = dyn_cast<ConstructorDecl>(base)) {
      // Special-case initializers, whose "type" isn't useful besides the
      // input arguments.
      auto *fnType = baseTy->getAs<AnyFunctionType>();
      baseTy = fnType->getResult();
      Type argTy = FunctionType::composeInput(TC.Context,
                                              baseTy->getAs<AnyFunctionType>()
                                                    ->getParams(),
                                              false);
      auto diagKind = diag::override_type_mismatch_with_fixits_init;
      unsigned numArgs = baseInit->getParameters()->size();
      activeDiag.emplace(TC.diagnose(decl, diagKind,
                                     /*plural*/std::min(numArgs, 2U),
                                     argTy));
    } else {
      if (isa<AbstractFunctionDecl>(base))
        baseTy = baseTy->getAs<AnyFunctionType>()->getResult();

      activeDiag.emplace(TC.diagnose(decl,
                                     diag::override_type_mismatch_with_fixits,
                                     base->getDescriptiveKind(), baseTy));
    }

    if (fixItOverrideDeclarationTypes(*activeDiag, decl, base))
      return true;
  }

  // There weren't any fixes we knew how to make. Drop this diagnostic.
  tentativeDiags.abort();
  return false;
}

namespace {
  enum class OverrideCheckingAttempt {
    PerfectMatch,
    MismatchedOptional,
    MismatchedTypes,
    BaseName,
    BaseNameWithMismatchedOptional,
    Final
  };

  OverrideCheckingAttempt &operator++(OverrideCheckingAttempt &attempt) {
    assert(attempt != OverrideCheckingAttempt::Final);
    attempt = static_cast<OverrideCheckingAttempt>(1+static_cast<int>(attempt));
    return attempt;
  }

  struct OverrideMatch {
    ValueDecl *Decl;
    bool IsExact;
    Type SubstType;
  };
}

static void diagnoseGeneralOverrideFailure(TypeChecker &TC,
                                           ValueDecl *decl,
                                           ArrayRef<OverrideMatch> matches,
                                           OverrideCheckingAttempt attempt) {
  switch (attempt) {
  case OverrideCheckingAttempt::PerfectMatch:
    TC.diagnose(decl, diag::override_multiple_decls_base,
                decl->getFullName());
    break;
  case OverrideCheckingAttempt::BaseName:
    TC.diagnose(decl, diag::override_multiple_decls_arg_mismatch,
                decl->getFullName());
    break;
  case OverrideCheckingAttempt::MismatchedOptional:
  case OverrideCheckingAttempt::MismatchedTypes:
  case OverrideCheckingAttempt::BaseNameWithMismatchedOptional:
    if (isa<ConstructorDecl>(decl))
      TC.diagnose(decl, diag::initializer_does_not_override);
    else if (isa<SubscriptDecl>(decl))
      TC.diagnose(decl, diag::subscript_does_not_override);
    else if (isa<VarDecl>(decl))
      TC.diagnose(decl, diag::property_does_not_override);
    else
      TC.diagnose(decl, diag::method_does_not_override);
    break;
  case OverrideCheckingAttempt::Final:
    llvm_unreachable("should have exited already");
  }

  for (auto match : matches) {
    auto matchDecl = match.Decl;
    if (attempt == OverrideCheckingAttempt::PerfectMatch) {
      TC.diagnose(matchDecl, diag::overridden_here);
      continue;
    }

    auto diag = TC.diagnose(matchDecl, diag::overridden_near_match_here,
                            matchDecl->getDescriptiveKind(),
                            matchDecl->getFullName());
    if (attempt == OverrideCheckingAttempt::BaseName) {
      fixDeclarationName(diag, cast<AbstractFunctionDecl>(decl),
                         matchDecl->getFullName());
    }
  }
}

static bool parameterTypesMatch(const ValueDecl *derivedDecl,
                                const ValueDecl *baseDecl,
                                TypeMatchOptions matchMode) {
  const ParameterList *derivedParams;
  const ParameterList *baseParams;
  if (auto *derived = dyn_cast<AbstractFunctionDecl>(derivedDecl)) {
    auto *base = dyn_cast<AbstractFunctionDecl>(baseDecl);
    if (!base)
      return false;
    baseParams = base->getParameterList(1);
    derivedParams = derived->getParameterList(1);
  } else {
    auto *base = dyn_cast<SubscriptDecl>(baseDecl);
    if (!base)
      return false;
    baseParams = base->getIndices();
    derivedParams = cast<SubscriptDecl>(derivedDecl)->getIndices();
  }

  if (baseParams->size() != derivedParams->size())
    return false;

  auto subs = SubstitutionMap::getOverrideSubstitutions(baseDecl, derivedDecl,
                                                        /*derivedSubs=*/None);

  for (auto i : indices(baseParams->getArray())) {
    auto baseItfTy = baseParams->get(i)->getInterfaceType();
    auto baseParamTy =
        baseDecl->getAsGenericContext()->mapTypeIntoContext(baseItfTy);
    baseParamTy = baseParamTy.subst(subs);
    auto derivedParamTy = derivedParams->get(i)->getInterfaceType();

    // Attempt contravariant match.
    if (baseParamTy->matchesParameter(derivedParamTy, matchMode))
      continue;

    // Try once more for a match, using the underlying type of an
    // IUO if we're allowing that.
    if (baseParams->get(i)
            ->getAttrs()
            .hasAttribute<ImplicitlyUnwrappedOptionalAttr>() &&
        matchMode.contains(TypeMatchFlags::AllowNonOptionalForIUOParam)) {
      baseParamTy = baseParamTy->getOptionalObjectType();
      if (baseParamTy->matches(derivedParamTy, matchMode))
        continue;
    }

    // If there is no match, then we're done.
    return false;
  }

  return true;
}

/// Determine which method or subscript this method or subscript overrides
/// (if any).
///
/// \returns true if an error occurred.
bool swift::checkOverrides(TypeChecker &TC, ValueDecl *decl) {
  if (decl->isInvalid() || decl->getOverriddenDecl())
    return false;

  auto *dc = decl->getDeclContext();

  auto owningTy = dc->getDeclaredInterfaceType();
  if (!owningTy)
    return false;

  auto classDecl = owningTy->getClassOrBoundGenericClass();
  if (!classDecl)
    return false;

  Type superclass = classDecl->getSuperclass();
  if (!superclass)
    return false;

  // Ignore accessor methods (e.g. getters and setters), they will be handled
  // when their storage decl is processed.
  if (isa<AccessorDecl>(decl))
    return false;

  auto method = dyn_cast<AbstractFunctionDecl>(decl);
  ConstructorDecl *ctor = nullptr;
  if (method)
    ctor = dyn_cast<ConstructorDecl>(method);

  auto abstractStorage = dyn_cast<AbstractStorageDecl>(decl);
  assert((method || abstractStorage) && "Not a method or abstractStorage?");
  SubscriptDecl *subscript = nullptr;
  if (abstractStorage)
    subscript = dyn_cast<SubscriptDecl>(abstractStorage);

  // Figure out the type of the declaration that we're using for comparisons.
  auto declTy = getMemberTypeForComparison(TC.Context, decl);

  // Look for members with the same name and matching types as this
  // one.
  auto attempt = OverrideCheckingAttempt::PerfectMatch;
  SmallVector<OverrideMatch, 2> matches;
  DeclName name = decl->getFullName();
  bool hadExactMatch = false;
  LookupResult members;

  do {
    switch (attempt) {
    case OverrideCheckingAttempt::PerfectMatch:
      break;
    case OverrideCheckingAttempt::MismatchedOptional:
      // Don't keep looking if the user didn't indicate it's an override.
      if (!decl->getAttrs().hasAttribute<OverrideAttr>())
        return false;
      break;
    case OverrideCheckingAttempt::MismatchedTypes:
      break;
    case OverrideCheckingAttempt::BaseName:
      // Don't keep looking if this is already a simple name, or if there
      // are no arguments.
      if (name.isSimpleName() || name.getArgumentNames().empty())
        return false;
      name = name.getBaseName();
      members.clear();
      break;
    case OverrideCheckingAttempt::BaseNameWithMismatchedOptional:
      break;
    case OverrideCheckingAttempt::Final:
      // Give up.
      return false;
    }

    if (members.empty()) {
      auto lookupOptions = defaultMemberLookupOptions;

      // Class methods cannot override declarations only
      // visible via dynamic dispatch.
      lookupOptions -= NameLookupFlags::DynamicLookup;

      // Class methods cannot override declarations only
      // visible as protocol requirements or protocol
      // extension members.
      lookupOptions -= NameLookupFlags::ProtocolMembers;
      lookupOptions -= NameLookupFlags::PerformConformanceCheck;

      members = TC.lookupMember(dc, superclass,
                                name, lookupOptions);
    }

    for (auto memberResult : members) {
      auto member = memberResult.getValueDecl();

      if (member->isInvalid())
        continue;

      if (member->getKind() != decl->getKind())
        continue;

      if (!dc->getAsClassOrClassExtensionContext())
        continue;

      auto parentDecl = cast<ValueDecl>(member);

      // Check whether there are any obvious reasons why the two given
      // declarations do not have an overriding relationship.
      if (!areOverrideCompatibleSimple(decl, parentDecl))
        continue;

      auto parentMethod = dyn_cast<AbstractFunctionDecl>(parentDecl);
      auto parentStorage = dyn_cast<AbstractStorageDecl>(parentDecl);
      assert(parentMethod || parentStorage);

      // If both are Objective-C, then match based on selectors or
      // subscript kind and check the types separately.
      bool objCMatch = false;
      if (parentDecl->isObjC() && decl->isObjC()) {
        if (method) {
          if (method->getObjCSelector() == parentMethod->getObjCSelector())
            objCMatch = true;
        } else if (auto *parentSubscript =
                     dyn_cast<SubscriptDecl>(parentStorage)) {
          // If the subscript kinds don't match, it's not an override.
          if (subscript->getObjCSubscriptKind()
                == parentSubscript->getObjCSubscriptKind())
            objCMatch = true;
        }

        // Properties don't need anything here since they are always
        // checked by name.
      }

      if (ctor) {
        // Factory methods cannot be overridden.
        auto parentCtor = cast<ConstructorDecl>(parentDecl);
        if (parentCtor->isFactoryInit())
          continue;
      }

      // Check whether the types are identical.
      auto parentDeclTy =
          getMemberTypeForComparison(TC.Context, parentDecl, decl);
      if (parentDeclTy->hasError())
        continue;

      if (isOverrideBasedOnType(decl, declTy, parentDecl, parentDeclTy)) {
        matches.push_back({parentDecl, true, parentDeclTy});
        hadExactMatch = true;
        continue;
      }

      // If this is a property, we accept the match and then reject it below
      // if the types don't line up, since you can't overload properties based
      // on types.
      if (isa<VarDecl>(parentDecl) ||
          attempt == OverrideCheckingAttempt::MismatchedTypes) {
        matches.push_back({parentDecl, false, parentDeclTy});
        continue;
      }

      // Failing that, check for subtyping.
      TypeMatchOptions matchMode = TypeMatchFlags::AllowOverride;
      if (attempt == OverrideCheckingAttempt::MismatchedOptional ||
          attempt == OverrideCheckingAttempt::BaseNameWithMismatchedOptional){
        matchMode |= TypeMatchFlags::AllowTopLevelOptionalMismatch;
      } else if (parentDecl->isObjC()) {
        matchMode |= TypeMatchFlags::AllowNonOptionalForIUOParam;
        matchMode |=
            TypeMatchFlags::IgnoreNonEscapingForOptionalFunctionParam;
      }

      auto declFnTy = declTy->getAs<AnyFunctionType>();
      auto parentDeclFnTy = parentDeclTy->getAs<AnyFunctionType>();
      if (declFnTy && parentDeclFnTy) {
        auto paramsAndResultMatch = [=]() -> bool {
          return parameterTypesMatch(decl, parentDecl, matchMode) &&
                 declFnTy->getResult()->matches(parentDeclFnTy->getResult(),
                                                matchMode);
        };

        if (declFnTy->matchesFunctionType(parentDeclFnTy, matchMode,
                                          paramsAndResultMatch)) {
          matches.push_back({parentDecl, objCMatch, parentDeclTy});
          hadExactMatch |= objCMatch;
          continue;
        }
      } else if (declTy->matches(parentDeclTy, matchMode)) {
        matches.push_back({parentDecl, objCMatch, parentDeclTy});
        hadExactMatch |= objCMatch;
        continue;
      }

      // Not a match. If we had an Objective-C match, this is a serious
      // problem.
      if (objCMatch) {
        if (method) {
          TC.diagnose(decl, diag::override_objc_type_mismatch_method,
                      method->getObjCSelector(), declTy);
        } else {
          TC.diagnose(decl, diag::override_objc_type_mismatch_subscript,
                      static_cast<unsigned>(
                        subscript->getObjCSubscriptKind()),
                      declTy);
        }
        TC.diagnose(parentDecl, diag::overridden_here_with_type,
                    parentDeclTy);

        // Put an invalid 'override' attribute here.
        makeInvalidOverrideAttr(TC, decl);

        return true;
      }
    }
    if (!matches.empty())
      break;

    ++attempt;
  } while (true);

  assert(!matches.empty());

  // If we had an exact match, throw away any non-exact matches.
  if (hadExactMatch)
    matches.erase(std::remove_if(matches.begin(), matches.end(),
                                 [&](OverrideMatch &match) {
                                   return !match.IsExact;
                                 }), matches.end());

  // If we override more than one declaration, complain.
  if (matches.size() > 1) {
    diagnoseGeneralOverrideFailure(TC, decl, matches, attempt);
    return true;
  }

  // If we have a single match (exact or not), take it.
  auto matchDecl = matches.front().Decl;
  auto matchType = matches.front().SubstType;
  bool emittedMatchError = false;

  // If the name of our match differs from the name we were looking for,
  // complain.
  if (decl->getFullName() != matchDecl->getFullName()) {
    auto diag = TC.diagnose(decl, diag::override_argument_name_mismatch,
                            isa<ConstructorDecl>(decl),
                            decl->getFullName(),
                            matchDecl->getFullName());
    fixDeclarationName(diag, cast<AbstractFunctionDecl>(decl),
                       matchDecl->getFullName());
    emittedMatchError = true;
  }

  // If we have an explicit ownership modifier and our parent doesn't,
  // complain.
  auto parentAttr =
      matchDecl->getAttrs().getAttribute<ReferenceOwnershipAttr>();
  if (auto ownershipAttr =
          decl->getAttrs().getAttribute<ReferenceOwnershipAttr>()) {
    ReferenceOwnership parentOwnership;
    if (parentAttr)
      parentOwnership = parentAttr->get();
    else
      parentOwnership = ReferenceOwnership::Strong;
    if (parentOwnership != ownershipAttr->get()) {
      TC.diagnose(decl, diag::override_ownership_mismatch,
                  parentOwnership, ownershipAttr->get());
      TC.diagnose(matchDecl, diag::overridden_here);
    }
  }

  // If a super method returns Self, and the subclass overrides it to
  // instead return the subclass type, complain.
  // This case gets this far because the type matching above specifically
  // strips out dynamic self via replaceCovariantResultType(), and that
  // is helpful in several cases - just not this one.
  if (decl->getASTContext().isSwiftVersionAtLeast(5) &&
      matchDecl->getInterfaceType()->hasDynamicSelfType() &&
      !decl->getInterfaceType()->hasDynamicSelfType() &&
      !classDecl->isFinal()) {
    TC.diagnose(decl, diag::override_dynamic_self_mismatch);
    TC.diagnose(matchDecl, diag::overridden_here);
  }

  // Check that the override has the required access level.
  // Overrides have to be at least as accessible as what they
  // override, except:
  //   - they don't have to be more accessible than their class and
  //   - a final method may be public instead of open.
  // Also diagnose attempts to override a non-open method from outside its
  // defining module.  This is not required for constructors, which are
  // never really "overridden" in the intended sense here, because of
  // course derived classes will change how the class is initialized.
  AccessLevel matchAccess = matchDecl->getFormalAccess(dc);
  if (matchAccess < AccessLevel::Open &&
      matchDecl->getModuleContext() != decl->getModuleContext() &&
      !isa<ConstructorDecl>(decl)) {
    TC.diagnose(decl, diag::override_of_non_open,
                decl->getDescriptiveKind());

  } else if (matchAccess == AccessLevel::Open &&
             classDecl->getFormalAccess(dc) ==
               AccessLevel::Open &&
             decl->getFormalAccess() != AccessLevel::Open &&
             !decl->isFinal()) {
    {
      auto diag = TC.diagnose(decl, diag::override_not_accessible,
                              /*setter*/false,
                              decl->getDescriptiveKind(),
                              /*fromOverridden*/true);
      fixItAccess(diag, decl, AccessLevel::Open);
    }
    TC.diagnose(matchDecl, diag::overridden_here);

  } else if (!isa<ConstructorDecl>(decl)) {
    auto matchAccessScope =
      matchDecl->getFormalAccessScope(dc);
    auto classAccessScope =
      classDecl->getFormalAccessScope(dc);
    auto requiredAccessScope =
      matchAccessScope.intersectWith(classAccessScope);
    auto scopeDC = requiredAccessScope->getDeclContext();

    bool shouldDiagnose = !decl->isAccessibleFrom(scopeDC);

    bool shouldDiagnoseSetter = false;
    if (!shouldDiagnose && matchDecl->isSettable(dc)){
      auto matchASD = cast<AbstractStorageDecl>(matchDecl);
      if (matchASD->isSetterAccessibleFrom(dc)) {
        auto matchSetterAccessScope = matchASD->getSetter()
          ->getFormalAccessScope(dc);
        auto requiredSetterAccessScope =
          matchSetterAccessScope.intersectWith(classAccessScope);
        auto setterScopeDC = requiredSetterAccessScope->getDeclContext();

        const auto *ASD = cast<AbstractStorageDecl>(decl);
        shouldDiagnoseSetter =
            ASD->isSettable(setterScopeDC) &&
            !ASD->isSetterAccessibleFrom(setterScopeDC);
      }
    }

    if (shouldDiagnose || shouldDiagnoseSetter) {
      bool overriddenForcesAccess =
        (requiredAccessScope->hasEqualDeclContextWith(matchAccessScope) &&
         matchAccess != AccessLevel::Open);
      AccessLevel requiredAccess =
        requiredAccessScope->requiredAccessForDiagnostics();
      {
        auto diag = TC.diagnose(decl, diag::override_not_accessible,
                                shouldDiagnoseSetter,
                                decl->getDescriptiveKind(),
                                overriddenForcesAccess);
        fixItAccess(diag, decl, requiredAccess,
                           shouldDiagnoseSetter);
      }
      TC.diagnose(matchDecl, diag::overridden_here);
    }
  }

  bool mayHaveMismatchedOptionals =
      (attempt == OverrideCheckingAttempt::MismatchedOptional ||
       attempt == OverrideCheckingAttempt::BaseNameWithMismatchedOptional);

  auto declIUOAttr =
      decl->getAttrs().hasAttribute<ImplicitlyUnwrappedOptionalAttr>();
  auto matchDeclIUOAttr =
      matchDecl->getAttrs().hasAttribute<ImplicitlyUnwrappedOptionalAttr>();

  // If this is an exact type match, we're successful!
  if (declIUOAttr == matchDeclIUOAttr && declTy->isEqual(matchType)) {
    // Nothing to do.

  } else if (method) {
    if (attempt == OverrideCheckingAttempt::MismatchedTypes) {
      auto diagKind = diag::method_does_not_override;
      if (ctor)
        diagKind = diag::initializer_does_not_override;
      TC.diagnose(decl, diagKind);
      noteFixableMismatchedTypes(TC, decl, matchDecl);
      TC.diagnose(matchDecl, diag::overridden_near_match_here,
                  matchDecl->getDescriptiveKind(),
                  matchDecl->getFullName());
      emittedMatchError = true;

    } else if (!isa<AccessorDecl>(method) &&
               (matchDecl->isObjC() || mayHaveMismatchedOptionals)) {
      // Private migration help for overrides of Objective-C methods.
      TypeLoc resultTL;
      if (auto *methodAsFunc = dyn_cast<FuncDecl>(method))
        resultTL = methodAsFunc->getBodyResultTypeLoc();
      emittedMatchError |= diagnoseMismatchedOptionals(
          TC, method, method->getParameterList(1), resultTL, matchDecl,
          cast<AbstractFunctionDecl>(matchDecl)->getParameterList(1),
          owningTy, mayHaveMismatchedOptionals);
    }
  } else if (auto subscript =
               dyn_cast_or_null<SubscriptDecl>(abstractStorage)) {
    // Otherwise, if this is a subscript, validate that covariance is ok.
    // If the parent is non-mutable, it's okay to be covariant.
    auto parentSubscript = cast<SubscriptDecl>(matchDecl);
    if (parentSubscript->getSetter()) {
      TC.diagnose(subscript, diag::override_mutable_covariant_subscript,
                  declTy, matchType);
      TC.diagnose(matchDecl, diag::subscript_override_here);
      return true;
    }

    if (attempt == OverrideCheckingAttempt::MismatchedTypes) {
      TC.diagnose(decl, diag::subscript_does_not_override);
      noteFixableMismatchedTypes(TC, decl, matchDecl);
      TC.diagnose(matchDecl, diag::overridden_near_match_here,
                  matchDecl->getDescriptiveKind(),
                  matchDecl->getFullName());
      emittedMatchError = true;

    } else if (mayHaveMismatchedOptionals) {
      emittedMatchError |= diagnoseMismatchedOptionals(
          TC, subscript, subscript->getIndices(),
          subscript->getElementTypeLoc(), matchDecl,
          cast<SubscriptDecl>(matchDecl)->getIndices(), owningTy,
          mayHaveMismatchedOptionals);
    }
  } else if (auto property = dyn_cast_or_null<VarDecl>(abstractStorage)) {
    auto propertyTy = property->getInterfaceType();
    auto parentPropertyTy = superclass->adjustSuperclassMemberDeclType(
        matchDecl, decl, matchDecl->getInterfaceType());

    if (!propertyTy->matches(parentPropertyTy,
                             TypeMatchFlags::AllowOverride)) {
      TC.diagnose(property, diag::override_property_type_mismatch,
                  property->getName(), propertyTy, parentPropertyTy);
      noteFixableMismatchedTypes(TC, decl, matchDecl);
      TC.diagnose(matchDecl, diag::property_override_here);
      return true;
    }

    // Differing only in Optional vs. ImplicitlyUnwrappedOptional is fine.
    bool IsSilentDifference = false;
    if (auto propertyTyNoOptional = propertyTy->getOptionalObjectType())
      if (auto parentPropertyTyNoOptional =
              parentPropertyTy->getOptionalObjectType())
        if (propertyTyNoOptional->isEqual(parentPropertyTyNoOptional))
          IsSilentDifference = true;

    // The overridden property must not be mutable.
    if (cast<AbstractStorageDecl>(matchDecl)->getSetter() &&
        !IsSilentDifference) {
      TC.diagnose(property, diag::override_mutable_covariant_property,
                  property->getName(), parentPropertyTy, propertyTy);
      TC.diagnose(matchDecl, diag::property_override_here);
      return true;
    }
  }

  // Catch-all to make sure we don't silently accept something we shouldn't.
  if (attempt != OverrideCheckingAttempt::PerfectMatch &&
      !emittedMatchError) {
    diagnoseGeneralOverrideFailure(TC, decl, matches, attempt);
  }

  return recordOverride(TC, decl, matchDecl);
}

namespace  {
  /// Attribute visitor that checks how the given attribute should be
  /// considered when overriding a declaration.
  ///
  /// Note that the attributes visited are those of the base
  /// declaration, so if you need to check that the overriding
  /// declaration doesn't have an attribute if the base doesn't have
  /// it, this isn't sufficient.
  class AttributeOverrideChecker
          : public AttributeVisitor<AttributeOverrideChecker> {
    TypeChecker &TC;
    ValueDecl *Base;
    ValueDecl *Override;

  public:
    AttributeOverrideChecker(TypeChecker &tc, ValueDecl *base,
                             ValueDecl *override)
      : TC(tc), Base(base), Override(override) { }

    /// Deleting this ensures that all attributes are covered by the visitor
    /// below.
    void visitDeclAttribute(DeclAttribute *A) = delete;

#define UNINTERESTING_ATTR(CLASS)                                              \
    void visit##CLASS##Attr(CLASS##Attr *) {}

    UNINTERESTING_ATTR(AccessControl)
    UNINTERESTING_ATTR(Alignment)
    UNINTERESTING_ATTR(CDecl)
    UNINTERESTING_ATTR(Consuming)
    UNINTERESTING_ATTR(DynamicMemberLookup)
    UNINTERESTING_ATTR(SILGenName)
    UNINTERESTING_ATTR(Exported)
    UNINTERESTING_ATTR(ForbidSerializingReference)
    UNINTERESTING_ATTR(GKInspectable)
    UNINTERESTING_ATTR(IBAction)
    UNINTERESTING_ATTR(IBDesignable)
    UNINTERESTING_ATTR(IBInspectable)
    UNINTERESTING_ATTR(IBOutlet)
    UNINTERESTING_ATTR(Indirect)
    UNINTERESTING_ATTR(Inline)
    UNINTERESTING_ATTR(Optimize)
    UNINTERESTING_ATTR(Inlinable)
    UNINTERESTING_ATTR(Effects)
    UNINTERESTING_ATTR(FixedLayout)
    UNINTERESTING_ATTR(Lazy)
    UNINTERESTING_ATTR(LLDBDebuggerFunction)
    UNINTERESTING_ATTR(Mutating)
    UNINTERESTING_ATTR(NonMutating)
    UNINTERESTING_ATTR(NonObjC)
    UNINTERESTING_ATTR(NoReturn)
    UNINTERESTING_ATTR(NSApplicationMain)
    UNINTERESTING_ATTR(NSCopying)
    UNINTERESTING_ATTR(NSManaged)
    UNINTERESTING_ATTR(ObjCBridged)
    UNINTERESTING_ATTR(Optional)
    UNINTERESTING_ATTR(Override)
    UNINTERESTING_ATTR(RawDocComment)
    UNINTERESTING_ATTR(Required)
    UNINTERESTING_ATTR(Convenience)
    UNINTERESTING_ATTR(Semantics)
    UNINTERESTING_ATTR(SetterAccess)
    UNINTERESTING_ATTR(UIApplicationMain)
    UNINTERESTING_ATTR(UsableFromInline)
    UNINTERESTING_ATTR(ObjCNonLazyRealization)
    UNINTERESTING_ATTR(UnsafeNoObjCTaggedPointer)
    UNINTERESTING_ATTR(SwiftNativeObjCRuntimeBase)
    UNINTERESTING_ATTR(ShowInInterface)
    UNINTERESTING_ATTR(Specialize)

    // These can't appear on overridable declarations.
    UNINTERESTING_ATTR(Prefix)
    UNINTERESTING_ATTR(Postfix)
    UNINTERESTING_ATTR(Infix)
    UNINTERESTING_ATTR(ReferenceOwnership)

    UNINTERESTING_ATTR(SynthesizedProtocol)
    UNINTERESTING_ATTR(RequiresStoredPropertyInits)
    UNINTERESTING_ATTR(Transparent)
    UNINTERESTING_ATTR(SILStored)
    UNINTERESTING_ATTR(Testable)

    UNINTERESTING_ATTR(WarnUnqualifiedAccess)
    UNINTERESTING_ATTR(DiscardableResult)

    UNINTERESTING_ATTR(ObjCMembers)
    UNINTERESTING_ATTR(ObjCRuntimeName)
    UNINTERESTING_ATTR(RestatedObjCConformance)
    UNINTERESTING_ATTR(Implements)
    UNINTERESTING_ATTR(StaticInitializeObjCMetadata)
    UNINTERESTING_ATTR(DowngradeExhaustivityCheck)
    UNINTERESTING_ATTR(ImplicitlyUnwrappedOptional)
    UNINTERESTING_ATTR(ClangImporterSynthesizedType)
    UNINTERESTING_ATTR(WeakLinked)
    UNINTERESTING_ATTR(Frozen)
#undef UNINTERESTING_ATTR

    void visitAvailableAttr(AvailableAttr *attr) {
      // FIXME: Check that this declaration is at least as available as the
      // one it overrides.
    }

    void visitRethrowsAttr(RethrowsAttr *attr) {
      // 'rethrows' functions are a subtype of ordinary 'throws' functions.
      // Require 'rethrows' on the override if it was there on the base,
      // unless the override is completely non-throwing.
      if (!Override->getAttrs().hasAttribute<RethrowsAttr>() &&
          cast<AbstractFunctionDecl>(Override)->hasThrows()) {
        TC.diagnose(Override, diag::override_rethrows_with_non_rethrows,
                    isa<ConstructorDecl>(Override));
        TC.diagnose(Base, diag::overridden_here);
      }
    }

    void visitFinalAttr(FinalAttr *attr) {
      // If this is an accessor, don't complain if we would have
      // complained about the storage declaration.
      if (auto accessor = dyn_cast<AccessorDecl>(Override)) {
        if (auto storageDecl = accessor->getStorage()) {
          if (storageDecl->getOverriddenDecl() &&
              storageDecl->getOverriddenDecl()->isFinal())
            return;
        }
      }

      // FIXME: Customize message to the kind of thing.
      auto baseKind = Base->getDescriptiveKind();
      switch (baseKind) {
      case DescriptiveDeclKind::StaticLet:
      case DescriptiveDeclKind::StaticVar:
      case DescriptiveDeclKind::StaticMethod:
        TC.diagnose(Override, diag::override_static, baseKind);
        break;
      default:
        TC.diagnose(Override, diag::override_final,
                    Override->getDescriptiveKind(), baseKind);
        break;
      }

      TC.diagnose(Base, diag::overridden_here);
    }

    void visitDynamicAttr(DynamicAttr *attr) {
      // Final overrides are not dynamic.
      if (Override->isFinal())
        return;

      makeDynamic(TC.Context, Override);
    }

    void visitObjCAttr(ObjCAttr *attr) {
      // Checking for overrides of declarations that are implicitly @objc
      // and occur in class extensions, because overriding will no longer be
      // possible under the Swift 4 rules.

      // We only care about the storage declaration.
      if (isa<AccessorDecl>(Override)) return;

      // If @objc was explicit or handled elsewhere, nothing to do.
      if (!attr->isSwift3Inferred()) return;

      // If we aren't warning about Swift 3 @objc inference, we're done.
      if (TC.Context.LangOpts.WarnSwift3ObjCInference ==
            Swift3ObjCInferenceWarnings::None)
        return;

      // If 'dynamic' was implicit, we'll already have warned about this.
      if (auto dynamicAttr = Base->getAttrs().getAttribute<DynamicAttr>()) {
        if (!dynamicAttr->isImplicit()) return;
      }

      // The overridden declaration needs to be in an extension.
      if (!isa<ExtensionDecl>(Base->getDeclContext())) return;

      // Complain.
      TC.diagnose(Override, diag::override_swift3_objc_inference,
                  Override->getDescriptiveKind(),
                  Override->getFullName(),
                  Base->getDeclContext()
                    ->getAsNominalTypeOrNominalTypeExtensionContext()
                    ->getName());
      TC.diagnose(Base, diag::make_decl_objc, Base->getDescriptiveKind())
        .fixItInsert(Base->getAttributeInsertionLoc(false),
                     "@objc ");
    }
  };
} // end anonymous namespace

/// Determine whether overriding the given declaration requires a keyword.
bool swift::overrideRequiresKeyword(ValueDecl *overridden) {
  if (auto ctor = dyn_cast<ConstructorDecl>(overridden)) {
    return ctor->isDesignatedInit() && !ctor->isRequired();
  }

  return true;
}

/// Returns true if a diagnostic about an accessor being less available
/// than the accessor it overrides would be redundant because we will
/// already emit another diagnostic.
static bool
isRedundantAccessorOverrideAvailabilityDiagnostic(TypeChecker &TC,
                                                  ValueDecl *override,
                                                  ValueDecl *base) {

  auto *overrideFn = dyn_cast<AccessorDecl>(override);
  auto *baseFn = dyn_cast<AccessorDecl>(base);
  if (!overrideFn || !baseFn)
    return false;

  AbstractStorageDecl *overrideASD = overrideFn->getStorage();
  AbstractStorageDecl *baseASD = baseFn->getStorage();
  if (overrideASD->getOverriddenDecl() != baseASD)
    return false;

  // If we have already emitted a diagnostic about an unsafe override
  // for the property, don't complain about the accessor.
  if (!TC.isAvailabilitySafeForOverride(overrideASD, baseASD)) {
    return true;
  }

  // Returns true if we will already diagnose a bad override
  // on the property's accessor of the given kind.
  auto accessorOverrideAlreadyDiagnosed = [&](AccessorKind kind) {
    FuncDecl *overrideAccessor = overrideASD->getAccessor(kind);
    FuncDecl *baseAccessor = baseASD->getAccessor(kind);
    if (overrideAccessor && baseAccessor &&
        !TC.isAvailabilitySafeForOverride(overrideAccessor, baseAccessor)) {
      return true;
    }
    return false;
  };

  // If we have already emitted a diagnostic about an unsafe override
  // for a getter or a setter, no need to complain about materializeForSet,
  // which is synthesized to be as available as both the getter and
  // the setter.
  if (overrideFn->isMaterializeForSet()) {
    if (accessorOverrideAlreadyDiagnosed(AccessorKind::Get) ||
        accessorOverrideAlreadyDiagnosed(AccessorKind::Set)) {
      return true;
    }
  }

  return false;
}

/// Diagnose an override for potential availability. Returns true if
/// a diagnostic was emitted and false otherwise.
static bool diagnoseOverrideForAvailability(TypeChecker &TC,
                                            ValueDecl *override,
                                            ValueDecl *base) {
  if (TC.isAvailabilitySafeForOverride(override, base))
    return false;

  // Suppress diagnostics about availability overrides for accessors
  // if they would be redundant with other diagnostics.
  if (isRedundantAccessorOverrideAvailabilityDiagnostic(TC, override, base))
    return false;

  if (auto *accessor = dyn_cast<AccessorDecl>(override)) {
    TC.diagnose(override, diag::override_accessor_less_available,
                accessor->getDescriptiveKind(),
                accessor->getStorage()->getBaseName());
    TC.diagnose(base, diag::overridden_here);
    return true;
  }

  TC.diagnose(override, diag::override_less_available,
              override->getBaseName());
  TC.diagnose(base, diag::overridden_here);

  return true;
}

static bool recordOverride(TypeChecker &TC, ValueDecl *override,
                           ValueDecl *base, bool isKnownObjC) {
  // Check property and subscript overriding.
  if (auto *baseASD = dyn_cast<AbstractStorageDecl>(base)) {
    auto *overrideASD = cast<AbstractStorageDecl>(override);

    // Make sure that the overriding property doesn't have storage.
    if (overrideASD->hasStorage() &&
        !(overrideASD->getWillSetFunc() || overrideASD->getDidSetFunc())) {
      bool downgradeToWarning = false;
      if (!TC.Context.isSwiftVersionAtLeast(5) &&
          overrideASD->getAttrs().hasAttribute<LazyAttr>()) {
        // Swift 4.0 had a bug where lazy properties were considered
        // computed by the time of this check. Downgrade this diagnostic to
        // a warning.
        downgradeToWarning = true;
      }
      auto diagID = downgradeToWarning ?
          diag::override_with_stored_property_warn :
          diag::override_with_stored_property;
      TC.diagnose(overrideASD, diagID,
                  overrideASD->getBaseName().getIdentifier());
      TC.diagnose(baseASD, diag::property_override_here);
      if (!downgradeToWarning)
        return true;
    }

    // Make sure that an observing property isn't observing something
    // read-only.  Observing properties look at change, read-only properties
    // have nothing to observe!
    bool baseIsSettable = baseASD->isSettable(baseASD->getDeclContext());
    if (baseIsSettable && TC.Context.LangOpts.EnableAccessControl) {
      baseIsSettable =
         baseASD->isSetterAccessibleFrom(overrideASD->getDeclContext());
    }
    if (overrideASD->getWriteImpl() == WriteImplKind::InheritedWithObservers
        && !baseIsSettable) {
      TC.diagnose(overrideASD, diag::observing_readonly_property,
                  overrideASD->getBaseName().getIdentifier());
      TC.diagnose(baseASD, diag::property_override_here);
      return true;
    }

    // Make sure we're not overriding a settable property with a non-settable
    // one.  The only reasonable semantics for this would be to inherit the
    // setter but override the getter, and that would be surprising at best.
    if (baseIsSettable && !override->isSettable(override->getDeclContext())) {
      TC.diagnose(overrideASD, diag::override_mutable_with_readonly_property,
                  overrideASD->getBaseName().getIdentifier());
      TC.diagnose(baseASD, diag::property_override_here);
      return true;
    }


    // Make sure a 'let' property is only overridden by 'let' properties.  A
    // let property provides more guarantees than the getter of a 'var'
    // property.
    if (auto VD = dyn_cast<VarDecl>(baseASD)) {
      if (VD->isLet()) {
        TC.diagnose(overrideASD, diag::override_let_property,
                    VD->getName());
        TC.diagnose(baseASD, diag::property_override_here);
        return true;
      }
    }
  }

  // Non-Objective-C declarations in extensions cannot override or
  // be overridden.
  if ((base->getDeclContext()->isExtensionContext() ||
       override->getDeclContext()->isExtensionContext()) &&
      !base->isObjC() && !isKnownObjC) {
    bool baseCanBeObjC = TC.canBeRepresentedInObjC(base);
    TC.diagnose(override, diag::override_decl_extension, baseCanBeObjC,
                !base->getDeclContext()->isExtensionContext());
    if (baseCanBeObjC) {
      SourceLoc insertionLoc =
        override->getAttributeInsertionLoc(/*forModifier=*/false);
      TC.diagnose(base, diag::overridden_here_can_be_objc)
        .fixItInsert(insertionLoc, "@objc ");
    } else {
      TC.diagnose(base, diag::overridden_here);
    }

    return true;
  }

  // If the overriding declaration does not have the 'override' modifier on
  // it, complain.
  if (!override->getAttrs().hasAttribute<OverrideAttr>() &&
      overrideRequiresKeyword(base)) {
    // FIXME: rdar://16320042 - For properties, we don't have a useful
    // location for the 'var' token.  Instead of emitting a bogus fixit, only
    // emit the fixit for 'func's.
    if (!isa<VarDecl>(override))
      TC.diagnose(override, diag::missing_override)
          .fixItInsert(override->getStartLoc(), "override ");
    else
      TC.diagnose(override, diag::missing_override);
    TC.diagnose(base, diag::overridden_here);
    override->getAttrs().add(
        new (TC.Context) OverrideAttr(SourceLoc()));
  }

  // If the overridden method is declared in a Swift Class Declaration,
  // dispatch will use table dispatch. If the override is in an extension
  // warn, since it is not added to the class vtable.
  //
  // FIXME: Only warn if the extension is in another module, and if
  // it is in the same module, update the vtable.
  if (auto *baseDecl = dyn_cast<ClassDecl>(base->getDeclContext())) {
    if (baseDecl->hasKnownSwiftImplementation() &&
        !base->isDynamic() && !isKnownObjC &&
        override->getDeclContext()->isExtensionContext()) {
      // For compatibility, only generate a warning in Swift 3
      TC.diagnose(override, (TC.Context.isSwiftVersion3()
        ? diag::override_class_declaration_in_extension_warning
        : diag::override_class_declaration_in_extension));
      TC.diagnose(base, diag::overridden_here);
    }
  }
  // If the overriding declaration is 'throws' but the base is not,
  // complain.
  if (auto overrideFn = dyn_cast<AbstractFunctionDecl>(override)) {
    if (overrideFn->hasThrows() &&
        !cast<AbstractFunctionDecl>(base)->hasThrows()) {
      TC.diagnose(override, diag::override_throws,
                  isa<ConstructorDecl>(override));
      TC.diagnose(base, diag::overridden_here);
    }

    if (!overrideFn->hasThrows() && base->isObjC() &&
        cast<AbstractFunctionDecl>(base)->hasThrows()) {
      TC.diagnose(override, diag::override_throws_objc,
                  isa<ConstructorDecl>(override));
      TC.diagnose(base, diag::overridden_here);
    }
  }

  // FIXME: Possibly should extend to more availability checking.
  if (auto *attr = base->getAttrs().getUnavailable(TC.Context)) {
    TC.diagnoseUnavailableOverride(override, base, attr);
  }

  if (!TC.getLangOpts().DisableAvailabilityChecking) {
    diagnoseOverrideForAvailability(TC, override, base);
  }

  /// Check attributes associated with the base; some may need to merged with
  /// or checked against attributes in the overriding declaration.
  AttributeOverrideChecker attrChecker(TC, base, override);
  for (auto attr : base->getAttrs()) {
    attrChecker.visit(attr);
  }

  if (auto overridingFunc = dyn_cast<FuncDecl>(override)) {
    overridingFunc->setOverriddenDecl(cast<FuncDecl>(base));
  } else if (auto overridingCtor = dyn_cast<ConstructorDecl>(override)) {
    overridingCtor->setOverriddenDecl(cast<ConstructorDecl>(base));
  } else if (auto overridingASD = dyn_cast<AbstractStorageDecl>(override)) {
    auto *baseASD = cast<AbstractStorageDecl>(base);
    overridingASD->setOverriddenDecl(baseASD);

    // Make sure we get consistent overrides for the accessors as well.
    assert(baseASD->getGetter());

    auto recordAccessorOverride = [&](AccessorKind kind) {
      // We need the same accessor on both.
      auto baseAccessor = baseASD->getAccessor(kind);
      if (!baseAccessor) return;
      auto overridingAccessor = overridingASD->getAccessor(kind);
      if (!overridingAccessor) return;

      // For setter accessors, we need the base's setter to be
      // accessible from the overriding context, or it's not an override.
      if ((kind == AccessorKind::Set ||
           kind == AccessorKind::MaterializeForSet) &&
          !baseASD->isSetterAccessibleFrom(overridingASD->getDeclContext()))
        return;

      // A materializeForSet for an override of storage with a
      // forced static dispatch materializeForSet is not itself an
      // override.
      if (kind == AccessorKind::MaterializeForSet &&
          baseAccessor->hasForcedStaticDispatch())
        return;

      // FIXME: Egregious hack to set an 'override' attribute.
      if (!overridingAccessor->getAttrs().hasAttribute<OverrideAttr>()) {
        auto loc = overridingASD->getOverrideLoc();
        overridingAccessor->getAttrs().add(
            new (TC.Context) OverrideAttr(loc));
      }

      recordOverride(TC, overridingAccessor, baseAccessor,
                     baseASD->isObjC());
    };

    recordAccessorOverride(AccessorKind::Get);
    recordAccessorOverride(AccessorKind::Set);
    recordAccessorOverride(AccessorKind::MaterializeForSet);
  } else {
    llvm_unreachable("Unexpected decl");
  }

  return false;
}

/// Minimize the set of overridden associated types, eliminating any
/// associated types that are overridden by other associated types.
static void minimizeOverriddenAssociatedTypes(
                           SmallVectorImpl<ValueDecl *> &assocTypes) {
  // Mark associated types that are "worse" than some other associated type,
  // because they come from an inherited protocol.
  bool anyWorse = false;
  std::vector<bool> worseThanAny(assocTypes.size(), false);
  for (unsigned i : indices(assocTypes)) {
    auto assoc1 = cast<AssociatedTypeDecl>(assocTypes[i]);
    auto proto1 = assoc1->getProtocol();
    for (unsigned j : range(i + 1, assocTypes.size())) {
      auto assoc2 = cast<AssociatedTypeDecl>(assocTypes[j]);
      auto proto2 = assoc2->getProtocol();
      if (proto1->inheritsFrom(proto2)) {
        anyWorse = true;
        worseThanAny[j] = true;
      } else if (proto2->inheritsFrom(proto1)) {
        anyWorse = true;
        worseThanAny[i] = true;
        break;
      }
    }
  }

  // If we didn't find any associated types that were "worse", we're done.
  if (!anyWorse) return;

  // Copy in the associated types that aren't worse than any other associated
  // type.
  unsigned nextIndex = 0;
  for (unsigned i : indices(assocTypes)) {
    if (worseThanAny[i]) continue;
    assocTypes[nextIndex++] = assocTypes[i];
  }

  assocTypes.erase(assocTypes.begin() + nextIndex, assocTypes.end());
}

/// Sort associated types just based on the protocol.
static int compareSimilarAssociatedTypes(ValueDecl *const *lhs,
                                         ValueDecl *const *rhs) {
  auto lhsProto = cast<AssociatedTypeDecl>(*lhs)->getProtocol();
  auto rhsProto = cast<AssociatedTypeDecl>(*rhs)->getProtocol();
  return TypeDecl::compare(lhsProto, rhsProto);
}

void TypeChecker::resolveOverriddenDecl(ValueDecl *VD) {
  // If this function or something it calls didn't set any overridden
  // declarations, it means that there are no overridden declarations. Set
  // the empty list.
  // Note: the request-evaluator would do this for free, but this function
  // is still fundamentally stateful.
  SWIFT_DEFER {
    if (!VD->overriddenDeclsComputed())
      (void)VD->setOverriddenDecls({ });
  };

  // For an associated type, compute the (minimized) set of overridden
  // declarations.
  if (auto assocType = dyn_cast<AssociatedTypeDecl>(VD)) {
    // Assume there are no overridden declarations for the purposes of this
    // computation.
    // FIXME: The request-evaluator will eventually handle this for us.
    (void)assocType->setOverriddenDecls({ });

    // Find associated types with the given name in all of the inherited
    // protocols.
    SmallVector<ValueDecl *, 4> inheritedAssociatedTypes;
    auto proto = assocType->getProtocol();
    proto->walkInheritedProtocols([&](ProtocolDecl *inheritedProto) {
      if (proto == inheritedProto) return TypeWalker::Action::Continue;

      // Objective-C protocols
      if (inheritedProto->isObjC()) return TypeWalker::Action::Continue;

      // Look for associated types with the same name.
      bool foundAny = false;
      for (auto member : inheritedProto->lookupDirect(
                                                      assocType->getFullName(),
                                                      /*ignoreNewExtensions=*/true)) {
        if (auto assocType = dyn_cast<AssociatedTypeDecl>(member)) {
          inheritedAssociatedTypes.push_back(assocType);
          foundAny = true;
        }
      }

      return foundAny ? TypeWalker::Action::SkipChildren
      : TypeWalker::Action::Continue;
    });

    // Minimize the set of inherited associated types, eliminating any that
    // themselves are overridden.
    minimizeOverriddenAssociatedTypes(inheritedAssociatedTypes);

    // Sort the set of inherited associated types.
    llvm::array_pod_sort(inheritedAssociatedTypes.begin(),
                         inheritedAssociatedTypes.end(),
                         compareSimilarAssociatedTypes);

    (void)assocType->setOverriddenDecls(inheritedAssociatedTypes);
    return;
  }

  // Only members of classes can override other declarations.
  if (!VD->getDeclContext()->getAsClassOrClassExtensionContext())
    return;

  // Types that aren't associated types cannot be overridden.
  if (isa<TypeDecl>(VD))
    return;

  // FIXME: We should check for the 'override' or 'required' keywords
  // here, to short-circuit checking in the common case.

  // FIXME: We should perform more minimal validation.
  validateDeclForNameLookup(VD);
}
