//===--- SignatureHelp.cpp ------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/IDE/SignatureHelp.h"
#include "ExprContextAnalysis.h"
#include "swift/AST/ASTDemangler.h"
#include "swift/AST/ConformanceLookup.h"
#include "swift/IDE/ArgumentCompletion.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/USRGeneration.h"
#include "swift/Basic/Assertions.h"
#include "swift/IDE/TypeCheckCompletionCallback.h"
#include "swift/Parse/IDEInspectionCallbacks.h"
#include "swift/Sema/IDETypeChecking.h"
#include "swift/Sema/ConstraintSystem.h"
#include "clang/AST/Attr.h"
#include "clang/AST/Decl.h"
#include "swift/IDE/SelectedOverloadInfo.h"

using namespace swift;
using namespace swift::ide;
using namespace swift::constraints;

namespace {
class SignatureHelpCallbacks : public CodeCompletionCallbacks,
                            public DoneParsingCallback {
  SignatureHelpConsumer &Consumer;
  SourceLoc Loc;
  CodeCompletionExpr *CCExpr = nullptr;
  DeclContext *CurDeclContext = nullptr;

                              
  void typeCheckWithLookup(TypeCheckCompletionCallback &Lookup,
                           SourceLoc CompletionLoc);

public:
  SignatureHelpCallbacks(Parser &P, SignatureHelpConsumer &Consumer)
      : CodeCompletionCallbacks(P), DoneParsingCallback(), Consumer(Consumer) {}

  // Only handle callbacks for argument completions.
  // {
  void completeCallArg(CodeCompletionExpr *E) override;
  // }

  void doneParsing(SourceFile *SrcFile) override;
};

void SignatureHelpCallbacks::completeCallArg(CodeCompletionExpr *E) {
  CurDeclContext = P.CurDeclContext;
  CCExpr = E;
}

void SignatureHelpCallbacks::doneParsing(SourceFile *SrcFile) {
  if (!CCExpr)
    return;

  SignatureHelpCallback Lookup(CCExpr);
  typeCheckWithLookup(Lookup, CCExpr->getLoc());

  SignatureHelpResult Result = Lookup.collectResults(CCExpr->getLoc(),
                                                  CurDeclContext);

  Consumer.handleResult(Result);
}

// TODO(a7medev): Share it with CodeCompletion or just simplify to typeCheckContextAt if possible.
void SignatureHelpCallbacks::typeCheckWithLookup(
    TypeCheckCompletionCallback &Lookup, SourceLoc CompletionLoc) {
  llvm::SaveAndRestore<TypeCheckCompletionCallback *> CompletionCollector(
      Context.CompletionCallback, &Lookup);
  typeCheckContextAt(
      TypeCheckASTNodeAtLocContext::declContext(CurDeclContext),
      CompletionLoc);

  // This (hopefully) only happens in cases where the expression isn't
  // typechecked during normal compilation either (e.g. member completion in a
  // switch case where there control expression is invalid). Having normal
  // typechecking still resolve even these cases would be beneficial for
  // tooling in general though.
  if (!Lookup.gotCallback()) {
    if (Context.TypeCheckerOpts.DebugConstraintSolver) {
      llvm::errs() << "--- Fallback typecheck for code completion ---\n";
    }
    Lookup.fallbackTypeCheck(CurDeclContext);
  }
}

} // anonymous namespace.

IDEInspectionCallbacksFactory *
swift::ide::makeSignatureHelpCallbacksFactory(SignatureHelpConsumer &Consumer) {

  // CC callback factory which produces 'SignatureCallbacks'.
  class SignatureHelpCallbacksFactoryImpl
      : public IDEInspectionCallbacksFactory {
    SignatureHelpConsumer &Consumer;

  public:
    SignatureHelpCallbacksFactoryImpl(
        SignatureHelpConsumer &Consumer) : Consumer(Consumer) {}

    Callbacks createCallbacks(Parser &P) override {
      auto Callback = std::make_shared<SignatureHelpCallbacks>(P, Consumer);
      return {Callback, Callback};
    }
  };

  return new SignatureHelpCallbacksFactoryImpl(Consumer);
}

//===----------------------------------------------------------------------===//
// SignatureHelpCallback
//===----------------------------------------------------------------------===//

void SignatureHelpCallback::sawSolutionImpl(const Solution &S) {
  auto &CS = S.getConstraintSystem();

  Type ExpectedTy = getTypeForCompletion(S, CompletionExpr);

  Expr *ParentCall = CompletionExpr;
  while (ParentCall && ParentCall->getArgs() == nullptr) {
    ParentCall = CS.getParentExpr(ParentCall);
  }
  if (auto TV = S.getType(CompletionExpr)->getAs<TypeVariableType>()) {
    auto Locator = TV->getImpl().getLocator();
    if (Locator->isLastElement<LocatorPathElt::PatternMatch>()) {
      // The code completion token is inside a pattern, which got rewritten from
      // a call by ResolvePattern. Thus, we aren't actually inside a call.
      // Rest 'ParentCall' to nullptr to reflect that.
      ParentCall = nullptr;
    }
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
  if (Info.getValue() && Info.getValue()->shouldHideFromEditor()) {
    return;
  }
  // Disallow invalid initializer references
  for (auto Fix : S.Fixes) {
    if (Fix->getLocator() == CalleeLocator &&
        Fix->getKind() == FixKind::AllowInvalidInitRef) {
      return;
    }
  }
  
  // Find the parameter the completion was bound to (if any), as well as which
  // parameters are already bound (so we don't suggest them even when the args
  // are out of order).
  std::optional<unsigned> ParamIdx;
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

  // If this is a duplicate of any other result, ignore this solution.
  if (llvm::any_of(Results, [&](const Result &R) {
        return R.FuncD == Info.getValue() &&
               nullableTypesEqual(R.FuncTy, Info.ValueTy) &&
               nullableTypesEqual(R.BaseType, Info.BaseTy) &&
               R.ParamIdx == ParamIdx &&
               R.IsNoninitialVariadic == IsNoninitialVariadic;
      })) {
    return;
  }
  
  AnyFunctionType *FuncTy = nullptr;
  if (Info.ValueTy) {
    FuncTy = Info.ValueTy->lookThroughAllOptionalTypes()->getAs<AnyFunctionType>();
  }
  
  Results.push_back(
      {isa<SubscriptExpr>(ParentCall), Info.getValue(), FuncTy, ArgIdx,
        ParamIdx, IsNoninitialVariadic, Info.BaseTy, ExpectedTy});
}

void SignatureHelpCallback::computeShadowedDecls(
    SmallPtrSetImpl<ValueDecl *> &ShadowedDecls) {
  for (size_t i = 0; i < Results.size(); ++i) {
    auto &ResultA = Results[i];
    for (size_t j = i + 1; j < Results.size(); ++j) {
      auto &ResultB = Results[j];
      if (!ResultA.FuncD || !ResultB.FuncD || !ResultA.FuncTy ||
          !ResultB.FuncTy) {
        continue;
      }
      if (ResultA.FuncD->getName() != ResultB.FuncD->getName()) {
        continue;
      }
      if (!ResultA.FuncTy->isEqual(ResultB.FuncTy)) {
        continue;
      }
      ProtocolDecl *inProtocolExtensionA =
          ResultA.FuncD->getDeclContext()->getExtendedProtocolDecl();
      ProtocolDecl *inProtocolExtensionB =
          ResultB.FuncD->getDeclContext()->getExtendedProtocolDecl();

      if (inProtocolExtensionA && !inProtocolExtensionB) {
        ShadowedDecls.insert(ResultA.FuncD);
      } else if (!inProtocolExtensionA && inProtocolExtensionB) {
        ShadowedDecls.insert(ResultB.FuncD);
      }
    }
  }
}

// TODO(a7medev): SignatureHelpResult shares almost everything with ArgumentTypeCheckCompletionCallback except for collectResult, probably move that to it instead.
SignatureHelpResult SignatureHelpCallback::collectResults(SourceLoc Loc,
                                                    DeclContext *DC) {
  SmallPtrSet<ValueDecl *, 4> ShadowedDecls;
  computeShadowedDecls(ShadowedDecls);
  
  SignatureHelpResult result(DC);
    
  for (auto &Result : Results) {
    // Only show call pattern completions if the function isn't
    // overridden.
    if (Result.FuncTy && ShadowedDecls.count(Result.FuncD) == 0) {
      result.Signatures.push_back(Result);
    }
  }
  
  return result;
}
