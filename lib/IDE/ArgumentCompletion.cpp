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
#include "swift/IDE/SelectedOverloadInfo.h"
#include "swift/Sema/ConstraintSystem.h"
#include "swift/Sema/IDETypeChecking.h"

using namespace swift;
using namespace swift::ide;
using namespace swift::constraints;

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

void ArgumentTypeCheckCompletionCallback::sawSolutionImpl(const Solution &S) {
  Type ExpectedTy = getTypeForCompletion(S, CompletionExpr);

  auto &CS = S.getConstraintSystem();

  Expr *ParentCall = CompletionExpr;
  while (ParentCall && ParentCall->getArgs() == nullptr) {
    ParentCall = CS.getParentExpr(ParentCall);
  }

  if (!ParentCall || ParentCall == CompletionExpr) {
    // We might not have a call that contains the code completion expression if
    // we type-checked the fallback code completion expression that only
    // contains the code completion token, but not the surrounding call.
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

  auto Info = getSelectedOverloadInfo(S, CalleeLocator);

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
  }

  bool HasLabel = false;
  if (auto PE = CS.getParentExpr(CompletionExpr)) {
    if (auto Args = PE->getArgs()) {
      HasLabel = !Args->getLabel(ArgIdx).empty();
    }
  }

  bool IsAsync = isContextAsync(S, DC);

  // If this is a duplicate of any other result, ignore this solution.
  if (llvm::any_of(Results, [&](const Result &R) {
        return R.FuncD == Info.Value &&
               nullableTypesEqual(R.FuncTy, Info.ValueTy) &&
               nullableTypesEqual(R.BaseType, Info.BaseTy) &&
               R.ParamIdx == ParamIdx &&
               R.IsNoninitialVariadic == IsNoninitialVariadic;
      })) {
    return;
  }

  llvm::SmallDenseMap<const VarDecl *, Type> SolutionSpecificVarTypes;
  getSolutionSpecificVarTypes(S, SolutionSpecificVarTypes);

  Results.push_back({ExpectedTy, isa<SubscriptExpr>(ParentCall), Info.Value,
                     Info.ValueTy, ArgIdx, ParamIdx, std::move(ClaimedParams),
                     IsNoninitialVariadic, Info.BaseTy, HasLabel, IsAsync,
                     SolutionSpecificVarTypes});
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
      if (SemanticContext == SemanticContextKind::None && Result.FuncD) {
        if (Result.FuncD->getDeclContext()->isTypeContext()) {
          SemanticContext = SemanticContextKind::CurrentNominal;
        } else if (Result.FuncD->getDeclContext()->isLocalContext()) {
          SemanticContext = SemanticContextKind::Local;
        } else if (Result.FuncD->getModuleContext() == DC->getParentModule()) {
          SemanticContext = SemanticContextKind::CurrentModule;
        }
      }
      if (Result.FuncTy) {
        if (auto FuncTy = Result.FuncTy->lookThroughAllOptionalTypes()
                              ->getAs<AnyFunctionType>()) {
          if (Result.IsSubscript) {
            assert(SemanticContext != SemanticContextKind::None);
            auto *SD = dyn_cast_or_null<SubscriptDecl>(Result.FuncD);
            Lookup.addSubscriptCallPattern(FuncTy, SD, SemanticContext);
          } else {
            auto *FD = dyn_cast_or_null<AbstractFunctionDecl>(Result.FuncD);
            Lookup.addFunctionCallPattern(FuncTy, FD, SemanticContext);
          }
        }
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
      Lookup.setSolutionSpecificVarTypes(Result.SolutionSpecificVarTypes);
    }
    Lookup.setExpectedTypes(ExpectedTypes, false);
    bool IsInAsyncContext = llvm::any_of(
        Results, [](const Result &Res) { return Res.IsInAsyncContext; });
    Lookup.setCanCurrDeclContextHandleAsync(IsInAsyncContext);
    Lookup.getValueCompletionsInDeclContext(Loc);
    Lookup.getSelfTypeCompletionInDeclContext(Loc, /*isForDeclResult=*/false);

    // Add any keywords that can be used in an argument expr position.
    addSuperKeyword(CompletionCtx.getResultSink(), DC);
    addExprKeywords(CompletionCtx.getResultSink(), DC);
  }

  deliverCompletionResults(CompletionCtx, Lookup, DC, Consumer);
}
