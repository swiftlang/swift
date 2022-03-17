//===--- ArgumentCompletion.cpp ---------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/IDE/ArgumentCompletion.h"
#include "swift/IDE/CodeCompletion.h"
#include "swift/IDE/CompletionLookup.h"
#include "swift/Sema/ConstraintSystem.h"
#include "swift/Sema/IDETypeChecking.h"

using namespace swift;
using namespace swift::ide;
using namespace swift::constraints;

/// Returns true if both types are null or if they are equal.
static bool nullableTypesEqual(Type LHS, Type RHS) {
  if (LHS.isNull() && RHS.isNull()) {
    return true;
  } else if (LHS.isNull() || RHS.isNull()) {
    // One type is null but the other is not.
    return false;
  } else {
    return LHS->isEqual(RHS);
  }
}

bool ArgumentTypeCheckCompletionCallback::addPossibleParams(
    const ArgumentTypeCheckCompletionCallback::Result &Res,
    SmallVectorImpl<PossibleParamInfo> &Params, SmallVectorImpl<Type> &Types) {
  if (!Res.ParamIdx) {
    // We don't really know much here. Suggest global results without a specific
    // expected type.
    return true;
  }

  if (Res.HasLabel) {
    // We already have a parameter label, suggest types
    Types.push_back(Res.ExpectedType);
    return true;
  }

  ArrayRef<AnyFunctionType::Param> ParamsToPass =
      Res.FuncTy->getAs<AnyFunctionType>()->getParams();

  ParameterList *PL = nullptr;
  if (Res.FuncD) {
    PL = swift::getParameterList(Res.FuncD);
  }
  assert(!PL || PL->size() == ParamsToPass.size());

  bool ShowGlobalCompletions = false;
  for (auto Idx : range(*Res.ParamIdx, ParamsToPass.size())) {
    bool IsCompletion = (Idx == Res.ParamIdx);

    // Stop at the first param claimed by other arguments.
    if (!IsCompletion && Res.ClaimedParamIndices.count(Idx) > 0) {
      break;
    }

    const AnyFunctionType::Param *P = &ParamsToPass[Idx];
    bool Required =
        !(PL && PL->get(Idx)->isDefaultArgument()) && !P->isVariadic();

    if (P->hasLabel() && !(IsCompletion && Res.IsNoninitialVariadic)) {
      // Suggest parameter label if parameter has label, we are completing in it
      // and it is not a variadic parameter that already has arguments
      PossibleParamInfo PP(P, Required);
      if (!llvm::is_contained(Params, PP)) {
        Params.push_back(std::move(PP));
      }
    } else {
      // We have a parameter that doesn't require a label. Suggest global
      // results for that type.
      ShowGlobalCompletions = true;
      Types.push_back(P->getPlainType());
    }
    if (Required) {
      // The user should only be suggested the first required param. Stop.
      break;
    }
  }
  return ShowGlobalCompletions;
}

void ArgumentTypeCheckCompletionCallback::sawSolution(const Solution &S) {
  TypeCheckCompletionCallback::sawSolution(S);

  Type ExpectedTy = getTypeForCompletion(S, CompletionExpr);
  if (!ExpectedTy) {
    return;
  }

  auto &CS = S.getConstraintSystem();

  Expr *ParentCall = CompletionExpr;
  while (ParentCall && ParentCall->getArgs() == nullptr) {
    ParentCall = CS.getParentExpr(ParentCall);
  }

  if (!ParentCall || ParentCall == CompletionExpr) {
    assert(false && "no containing call?");
    return;
  }

  auto ArgInfo = getCompletionArgInfo(ParentCall, CS);
  if (!ArgInfo) {
    assert(false && "bad parent call match?");
    return;
  }
  auto ArgIdx = ArgInfo->completionIdx;

  auto *CallLocator = CS.getConstraintLocator(ParentCall);
  auto *CalleeLocator = S.getCalleeLocator(CallLocator);
  auto SelectedOverload = S.getOverloadChoiceIfAvailable(CalleeLocator);
  if (!SelectedOverload) {
    return;
  }

  Type CallBaseTy = SelectedOverload->choice.getBaseType();
  if (CallBaseTy) {
    CallBaseTy = S.simplifyType(CallBaseTy)->getRValueType();
  }

  ValueDecl *FuncD = SelectedOverload->choice.getDeclOrNull();
  Type FuncTy = S.simplifyType(SelectedOverload->openedType)->getRValueType();

  // For completion as the arg in a call to the implicit [keypath: _] subscript
  // the solver can't know what kind of keypath is expected without an actual
  // argument (e.g. a KeyPath vs WritableKeyPath) so it ends up as a hole.
  // Just assume KeyPath so we show the expected keypath's root type to users
  // rather than '_'.
  if (SelectedOverload->choice.getKind() ==
      OverloadChoiceKind::KeyPathApplication) {
    auto Params = FuncTy->getAs<AnyFunctionType>()->getParams();
    if (Params.size() == 1 && Params[0].getPlainType()->is<UnresolvedType>()) {
      auto *KPDecl = CS.getASTContext().getKeyPathDecl();
      Type KPTy =
          KPDecl->mapTypeIntoContext(KPDecl->getDeclaredInterfaceType());
      Type KPValueTy = KPTy->castTo<BoundGenericType>()->getGenericArgs()[1];
      KPTy = BoundGenericType::get(KPDecl, Type(), {CallBaseTy, KPValueTy});
      FuncTy = FunctionType::get({Params[0].withType(KPTy)}, KPValueTy);
    }
  }

  // Find the parameter the completion was bound to (if any), as well as which
  // parameters are already bound (so we don't suggest them even when the args
  // are out of order).
  Optional<unsigned> ParamIdx;
  std::set<unsigned> ClaimedParams;
  bool IsNoninitialVariadic = false;

  ConstraintLocator *ArgumentLocator;
  ArgumentLocator =
      CS.getConstraintLocator(CallLocator, ConstraintLocator::ApplyArgument);
  auto ArgMatchChoices = S.argumentMatchingChoices.find(ArgumentLocator);
  if (ArgMatchChoices != S.argumentMatchingChoices.end()) {
    // We might not have argument matching choices when applying a subscript
    // found via @dynamicMemberLookup.
    auto Bindings = ArgMatchChoices->second.parameterBindings;

    for (auto i : indices(Bindings)) {
      bool Claimed = false;
      for (auto j : Bindings[i]) {
        if (j == ArgIdx) {
          assert(!ParamIdx);
          ParamIdx = i;
          IsNoninitialVariadic = llvm::any_of(
              Bindings[i], [j](unsigned other) { return other < j; });
        }
        // Synthesized args don't count.
        if (j < ArgInfo->argCount) {
          Claimed = true;
        }
      }
      if (Claimed) {
        ClaimedParams.insert(i);
      }
    }
  } else {
    // FIXME: We currently don't look through @dynamicMemberLookup applications
    // for subscripts (rdar://90363138)
  }

  bool HasLabel = false;
  if (auto PE = CS.getParentExpr(CompletionExpr)) {
    if (auto Args = PE->getArgs()) {
      HasLabel = !Args->getLabel(ArgIdx).empty();
    }
  }

  // If this is a duplicate of any other result, ignore this solution.
  if (llvm::any_of(Results, [&](const Result &R) {
        return R.FuncD == FuncD && nullableTypesEqual(R.FuncTy, FuncTy) &&
               nullableTypesEqual(R.BaseType, CallBaseTy) &&
               R.ParamIdx == ParamIdx &&
               R.IsNoninitialVariadic == IsNoninitialVariadic;
      })) {
    return;
  }

  Results.push_back({ExpectedTy, isa<SubscriptExpr>(ParentCall), FuncD, FuncTy,
                     ArgIdx, ParamIdx, std::move(ClaimedParams),
                     IsNoninitialVariadic, CallBaseTy, HasLabel});
}

void ArgumentTypeCheckCompletionCallback::deliverResults(
    bool IncludeSignature, SourceLoc Loc, DeclContext *DC,
    ide::CodeCompletionContext &CompletionCtx,
    CodeCompletionConsumer &Consumer) {
  ASTContext &Ctx = DC->getASTContext();
  CompletionLookup Lookup(CompletionCtx.getResultSink(), Ctx, DC,
                          &CompletionCtx);

  // Perform global completion as a fallback if we don't have any results.
  bool shouldPerformGlobalCompletion = Results.empty();
  SmallVector<Type, 8> ExpectedTypes;

  if (IncludeSignature && !Results.empty()) {
    Lookup.setHaveLParen(true);
    for (auto &Result : Results) {
      auto SemanticContext = SemanticContextKind::None;
      NominalTypeDecl *BaseNominal = nullptr;
      if (Result.BaseType) {
        Type BaseTy = Result.BaseType;
        if (auto InstanceTy = BaseTy->getMetatypeInstanceType()) {
          BaseTy = InstanceTy;
        }
        if ((BaseNominal = BaseTy->getAnyNominal())) {
          SemanticContext = SemanticContextKind::CurrentNominal;
          if (Result.FuncD &&
              Result.FuncD->getDeclContext()->getSelfNominalTypeDecl() !=
                  BaseNominal) {
            SemanticContext = SemanticContextKind::Super;
          }
        } else if (BaseTy->is<TupleType>() || BaseTy->is<SubstitutableType>()) {
          SemanticContext = SemanticContextKind::CurrentNominal;
        }
      }
      if (Result.IsSubscript) {
        assert(SemanticContext != SemanticContextKind::None);
        auto *SD = dyn_cast_or_null<SubscriptDecl>(Result.FuncD);
        Lookup.addSubscriptCallPattern(Result.FuncTy->getAs<AnyFunctionType>(),
                                       SD, SemanticContext);
      } else {
        auto *FD = dyn_cast_or_null<AbstractFunctionDecl>(Result.FuncD);
        Lookup.addFunctionCallPattern(Result.FuncTy->getAs<AnyFunctionType>(),
                                      FD, SemanticContext);
      }
    }
    Lookup.setHaveLParen(false);

    shouldPerformGlobalCompletion |=
        !Lookup.FoundFunctionCalls || Lookup.FoundFunctionsWithoutFirstKeyword;
  } else if (!Results.empty()) {
    SmallVector<PossibleParamInfo, 8> Params;
    for (auto &Ret : Results) {
      shouldPerformGlobalCompletion |=
          addPossibleParams(Ret, Params, ExpectedTypes);
    }
    Lookup.addCallArgumentCompletionResults(Params);
  }

  if (shouldPerformGlobalCompletion) {
    for (auto &Result : Results) {
      ExpectedTypes.push_back(Result.ExpectedType);
    }
    Lookup.setExpectedTypes(ExpectedTypes, false);
    Lookup.getValueCompletionsInDeclContext(Loc);
    Lookup.getSelfTypeCompletionInDeclContext(Loc, /*isForDeclResult=*/false);

    // Add any keywords that can be used in an argument expr position.
    addSuperKeyword(CompletionCtx.getResultSink(), DC);
    addExprKeywords(CompletionCtx.getResultSink(), DC);
  }

  deliverCompletionResults(CompletionCtx, Lookup, DC, Consumer);
}
