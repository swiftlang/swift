//===--- TypeCheckCodeCompletion.cpp - Type Checking for Code Completion --===//
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
// This file implements various entry points for use by lib/IDE/.
//
//===----------------------------------------------------------------------===//

#include "swift/Subsystems.h"
#include "ConstraintSystem.h"
#include "TypeChecker.h"
#include "TypeCheckObjC.h"
#include "TypeCheckType.h"
#include "CodeSynthesis.h"
#include "MiscDiagnostics.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/ASTVisitor.h"
#include "swift/AST/Attr.h"
#include "swift/AST/DiagnosticSuppression.h"
#include "swift/AST/ExistentialLayout.h"
#include "swift/AST/Identifier.h"
#include "swift/AST/ImportCache.h"
#include "swift/AST/Initializer.h"
#include "swift/AST/ModuleLoader.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/PrettyStackTrace.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/SourceFile.h"
#include "swift/AST/Type.h"
#include "swift/AST/TypeCheckRequests.h"
#include "swift/Basic/Statistic.h"
#include "swift/Basic/STLExtras.h"
#include "swift/Parse/Lexer.h"
#include "swift/Sema/IDETypeChecking.h"
#include "swift/Sema/CodeCompletionTypeChecking.h"
#include "swift/Strings.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/ADT/SmallSet.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/ADT/TinyPtrVector.h"
#include "llvm/ADT/Twine.h"
#include <algorithm>

using namespace swift;
using namespace constraints;

/// Find the declaration directly referenced by this expression.
static std::pair<ValueDecl *, FunctionRefKind>
findReferencedDecl(Expr *expr, DeclNameLoc &loc) {
  do {
    expr = expr->getSemanticsProvidingExpr();

    if (auto ice = dyn_cast<ImplicitConversionExpr>(expr)) {
      expr = ice->getSubExpr();
      continue;
    }

    if (auto dre = dyn_cast<DeclRefExpr>(expr)) {
      loc = dre->getNameLoc();
      return { dre->getDecl(), dre->getFunctionRefKind() };
    }

    return { nullptr, FunctionRefKind::Unapplied };
  } while (true);
}

// Check if \p E is a call expression to curried thunk of "KeyPath as function".
// i.e. '{ `$kp$` in { $0[keyPath: $kp$] } }(keypath)'
static bool isKeyPathCurriedThunkCallExpr(Expr *E) {
  auto CE = dyn_cast<CallExpr>(E);
  if (!CE)
    return false;
  auto thunk = dyn_cast<AutoClosureExpr>(CE->getFn());
  if (!thunk)
    return false;
  if (thunk->getParameters()->size() != 1 ||
      thunk->getParameters()->get(0)->getParameterName().str() != "$kp$")
    return false;

  auto PE = dyn_cast<ParenExpr>(CE->getArg());
  if (!PE)
    return false;
  return isa<KeyPathExpr>(PE->getSubExpr());
}

// Extract the keypath expression from the curried thunk expression.
static Expr *extractKeyPathFromCurryThunkCall(Expr *E) {
  assert(isKeyPathCurriedThunkCallExpr(E));
  auto call = cast<CallExpr>(E);
  auto arg = cast<ParenExpr>(call->getArg());
  return arg->getSubExpr();
}

namespace {

/// AST walker that "sanitizes" an expression for re-typechecking during
/// code completion.
///
/// FIXME: Remove this.
class SanitizeExpr : public ASTWalker {
  ASTContext &C;
  bool ShouldReusePrecheckedType;
  llvm::SmallDenseMap<OpaqueValueExpr *, Expr *, 4> OpenExistentials;

public:
  SanitizeExpr(ASTContext &C,
               bool shouldReusePrecheckedType)
    : C(C), ShouldReusePrecheckedType(shouldReusePrecheckedType) { }

  std::pair<bool, Expr *> walkToExprPre(Expr *expr) override {
    while (true) {

      // If we should reuse pre-checked types, don't sanitize the expression
      // if it's already type-checked.
      if (ShouldReusePrecheckedType && expr->getType())
        return { false, expr };

      // OpenExistentialExpr contains OpaqueValueExpr in its sub expression.
      if (auto OOE = dyn_cast<OpenExistentialExpr>(expr)) {
        auto archetypeVal = OOE->getOpaqueValue();
        auto base = OOE->getExistentialValue();

        bool inserted = OpenExistentials.insert({archetypeVal, base}).second;
        assert(inserted && "OpaqueValue appears multiple times?");
        (void)inserted;
        SWIFT_DEFER { OpenExistentials.erase(archetypeVal); };

        // Walk to and return the base expression to erase any existentials
        // within it.
        return { false, OOE->getSubExpr()->walk(*this) };
      }

      // Hacky, this behaves just like an OpenedExistential in that it changes
      // the expr tree.
      if (auto ISLE = dyn_cast<InterpolatedStringLiteralExpr>(expr)) {
        if (auto subExpr = ISLE->getAppendingExpr()->getSubExpr()) {
          if (auto opaqueValue = dyn_cast<OpaqueValueExpr>(subExpr)) {
            ISLE->getAppendingExpr()->setSubExpr(nullptr);
          }
        }
      }

      // Substitute OpaqueValue with its representing existental.
      if (auto OVE = dyn_cast<OpaqueValueExpr>(expr)) {
        auto value = OpenExistentials.find(OVE);

        if (value != OpenExistentials.end()) {
          expr = value->second;
          continue;
        } else {
          assert(OVE->isPlaceholder() &&
                 "Didn't see this OVE in a containing OpenExistentialExpr?");
        }
      }

      // Skip any implicit conversions applied to this expression.
      if (auto ICE = dyn_cast<ImplicitConversionExpr>(expr)) {
        expr = ICE->getSubExpr();
        continue;
      }

      // MakeTemporarilyEscapableExpr is typechecked expression.
      if (auto MTEE = dyn_cast<MakeTemporarilyEscapableExpr>(expr)) {
        expr = MTEE->getOriginalExpr();
        continue;
      }

      // Extract keypath from '{ `$kp$` in { $0[keyPath: $kp$] } }(keypath)'
      if (isKeyPathCurriedThunkCallExpr(expr)) {
        expr = extractKeyPathFromCurryThunkCall(expr);
        continue;
      }

      // Restore '@autoclosure'd value.
      if (auto ACE = dyn_cast<AutoClosureExpr>(expr)) {
        // This is only valid if the closure doesn't have parameters.
        if (ACE->getParameters()->size() == 0) {
          expr = ACE->getSingleExpressionBody();
          continue;
        }
        llvm_unreachable("other AutoClosureExpr must be handled specially");
      }

      // Remove any semantic expression injected by typechecking.
      if (auto EPE = dyn_cast<EditorPlaceholderExpr>(expr)) {
        EPE->setSemanticExpr(nullptr);
      }

      // Strip default arguments and varargs from type-checked call
      // argument lists.
      if (isa<ParenExpr>(expr) || isa<TupleExpr>(expr)) {
        if (shouldSanitizeArgumentList(expr))
          expr = sanitizeArgumentList(expr);
      }

      // If this expression represents keypath based dynamic member
      // lookup, let's convert it back to the original form of
      // member or subscript reference.
      if (auto *SE = dyn_cast<SubscriptExpr>(expr)) {
        if (auto *TE = dyn_cast<TupleExpr>(SE->getIndex())) {
          auto isImplicitKeyPathExpr = [](Expr *argExpr) -> bool {
            if (auto *KP = dyn_cast<KeyPathExpr>(argExpr))
              return KP->isImplicit();
            return false;
          };

          if (TE->isImplicit() && TE->getNumElements() == 1 &&
              TE->getElementName(0) == C.Id_dynamicMember &&
              isImplicitKeyPathExpr(TE->getElement(0))) {
            auto *keyPathExpr = cast<KeyPathExpr>(TE->getElement(0));
            auto *componentExpr = keyPathExpr->getParsedPath();

            if (auto *UDE = dyn_cast<UnresolvedDotExpr>(componentExpr)) {
              UDE->setBase(SE->getBase());
              return {true, UDE};
            }

            if (auto *subscript = dyn_cast<SubscriptExpr>(componentExpr)) {
              subscript->setBase(SE->getBase());
              return {true, subscript};
            }

            llvm_unreachable("unknown keypath component type");
          }
        }
      }

      // If this is a closure, only walk into its children if they
      // are type-checked in the context of the enclosing expression.
      if (auto closure = dyn_cast<ClosureExpr>(expr)) {
        if (!shouldTypeCheckInEnclosingExpression(closure))
          return { false, expr };
      }

      // Now, we're ready to walk into sub expressions.
      return {true, expr};
    }
  }

  bool isSyntheticArgumentExpr(const Expr *expr) {
    if (isa<DefaultArgumentExpr>(expr))
      return true;

    if (auto *varargExpr = dyn_cast<VarargExpansionExpr>(expr))
      if (isa<ArrayExpr>(varargExpr->getSubExpr()))
        return true;

    return false;
  }

  bool shouldSanitizeArgumentList(const Expr *expr) {
    if (auto *parenExpr = dyn_cast<ParenExpr>(expr)) {
      return isSyntheticArgumentExpr(parenExpr->getSubExpr());
    } else if (auto *tupleExpr = dyn_cast<TupleExpr>(expr)) {
      for (auto *arg : tupleExpr->getElements()) {
        if (isSyntheticArgumentExpr(arg))
          return true;
      }

      return false;
    } else {
      return isSyntheticArgumentExpr(expr);
    }
  }

  Expr *sanitizeArgumentList(Expr *original) {
    auto argList = getOriginalArgumentList(original);

    if (argList.args.size() == 1 &&
        argList.labels[0].empty() &&
        !isa<VarargExpansionExpr>(argList.args[0])) {
      auto *result =
        new (C) ParenExpr(argList.lParenLoc,
                          argList.args[0],
                          argList.rParenLoc,
                          argList.hasTrailingClosure);
      result->setImplicit();
      return result;
    }

    return TupleExpr::create(C,
                             argList.lParenLoc,
                             argList.args,
                             argList.labels,
                             argList.labelLocs,
                             argList.rParenLoc,
                             argList.hasTrailingClosure,
                             /*implicit=*/true);
  }

  Expr *walkToExprPost(Expr *expr) override {
    assert(!isa<ImplicitConversionExpr>(expr) &&
           "ImplicitConversionExpr should be eliminated in walkToExprPre");

    auto buildMemberRef = [&](Type memberType, Expr *base, SourceLoc dotLoc,
                              ConcreteDeclRef member, DeclNameLoc memberLoc,
                              bool implicit) -> Expr * {
      auto *memberRef = new (C)
          MemberRefExpr(base, dotLoc, member, memberLoc, implicit);

      if (memberType) {
        memberRef->setType(memberType);
        return memberRef;
      }

      return memberRef;
    };

    // A DotSyntaxCallExpr is a member reference that has already been
    // type-checked down to a call; turn it back into an overloaded
    // member reference expression.
    if (auto dotCall = dyn_cast<DotSyntaxCallExpr>(expr)) {
      DeclNameLoc memberLoc;
      auto memberAndFunctionRef = findReferencedDecl(dotCall->getFn(),
                                                     memberLoc);
      if (memberAndFunctionRef.first) {
        assert(!isa<ImplicitConversionExpr>(dotCall->getBase()));
        return buildMemberRef(dotCall->getType(),
                              dotCall->getBase(),
                              dotCall->getDotLoc(),
                              memberAndFunctionRef.first,
                              memberLoc, expr->isImplicit());
      }
    }

    if (auto *dynamicMember = dyn_cast<DynamicMemberRefExpr>(expr)) {
      if (auto memberRef = dynamicMember->getMember()) {
        assert(!isa<ImplicitConversionExpr>(dynamicMember->getBase()));
        return buildMemberRef(dynamicMember->getType(),
                              dynamicMember->getBase(),
                              dynamicMember->getDotLoc(),
                              memberRef,
                              dynamicMember->getNameLoc(),
                              expr->isImplicit());
      }
    }

    // A DotSyntaxBaseIgnoredExpr is a static member reference that has
    // already been type-checked down to a call where the argument doesn't
    // actually matter; turn it back into an overloaded member reference
    // expression.
    if (auto dotIgnored = dyn_cast<DotSyntaxBaseIgnoredExpr>(expr)) {
      DeclNameLoc memberLoc;
      auto memberAndFunctionRef = findReferencedDecl(dotIgnored->getRHS(),
                                                     memberLoc);
      if (memberAndFunctionRef.first) {
        assert(!isa<ImplicitConversionExpr>(dotIgnored->getLHS()));
        return buildMemberRef(dotIgnored->getType(),
                              dotIgnored->getLHS(),
                              dotIgnored->getDotLoc(),
                              memberAndFunctionRef.first,
                              memberLoc, expr->isImplicit());
      }
    }
    return expr;
  }

  /// Ignore declarations.
  bool walkToDeclPre(Decl *decl) override { return false; }
};

}  // end namespace

static Type
getTypeOfExpressionWithoutApplying(Expr *&expr, DeclContext *dc,
                                   ConcreteDeclRef &referencedDecl,
                                 FreeTypeVariableBinding allowFreeTypeVariables) {
  auto &Context = dc->getASTContext();

  expr = expr->walk(SanitizeExpr(Context,
                                 /*shouldReusePrecheckedType=*/false));

  FrontendStatsTracer StatsTracer(Context.Stats,
                                  "typecheck-expr-no-apply", expr);
  PrettyStackTraceExpr stackTrace(Context, "type-checking", expr);
  referencedDecl = nullptr;

  // Construct a constraint system from this expression.
  ConstraintSystem cs(dc, ConstraintSystemFlags::SuppressDiagnostics);

  // Attempt to solve the constraint system.
  const Type originalType = expr->getType();
  const bool needClearType = originalType && originalType->hasError();
  const auto recoverOriginalType = [&] () {
    if (needClearType)
      expr->setType(originalType);
  };

  // If the previous checking gives the expr error type, clear the result and
  // re-check.
  if (needClearType)
    expr->setType(Type());
  SolutionApplicationTarget target(
      expr, dc, CTP_Unused, Type(), /*isDiscarded=*/false);
  auto viable = cs.solve(target, allowFreeTypeVariables);
  if (!viable) {
    recoverOriginalType();
    return Type();
  }

  // Get the expression's simplified type.
  expr = target.getAsExpr();
  auto &solution = (*viable)[0];
  auto &solutionCS = solution.getConstraintSystem();
  Type exprType = solution.simplifyType(solutionCS.getType(expr));

  assert(exprType && !exprType->hasTypeVariable() &&
         "free type variable with FreeTypeVariableBinding::GenericParameters?");
  assert(exprType && !exprType->hasHole() &&
         "type hole with FreeTypeVariableBinding::GenericParameters?");

  if (exprType->hasError()) {
    recoverOriginalType();
    return Type();
  }

  // Dig the declaration out of the solution.
  auto semanticExpr = expr->getSemanticsProvidingExpr();
  auto topLocator = cs.getConstraintLocator(semanticExpr);
  referencedDecl = solution.resolveLocatorToDecl(topLocator);

  if (!referencedDecl.getDecl()) {
    // Do another check in case we have a curried call from binding a function
    // reference to a variable, for example:
    //
    //   class C {
    //     func instanceFunc(p1: Int, p2: Int) {}
    //   }
    //   func t(c: C) {
    //     C.instanceFunc(c)#^COMPLETE^#
    //   }
    //
    // We need to get the referenced function so we can complete the argument
    // labels. (Note that the requirement to have labels in the curried call
    // seems inconsistent with the removal of labels from function types.
    // If this changes the following code could be removed).
    if (auto *CE = dyn_cast<CallExpr>(semanticExpr)) {
      if (auto *UDE = dyn_cast<UnresolvedDotExpr>(CE->getFn())) {
        if (isa<TypeExpr>(UDE->getBase())) {
          auto udeLocator = cs.getConstraintLocator(UDE);
          auto udeRefDecl = solution.resolveLocatorToDecl(udeLocator);
          if (auto *FD = dyn_cast_or_null<FuncDecl>(udeRefDecl.getDecl())) {
            if (FD->isInstanceMember())
              referencedDecl = udeRefDecl;
          }
        }
      }
    }
  }

  // Recover the original type if needed.
  recoverOriginalType();
  return exprType;
}

static FunctionType *
getTypeOfCompletionOperatorImpl(DeclContext *DC, Expr *expr,
                                ConcreteDeclRef &referencedDecl) {
  auto &Context = DC->getASTContext();

  FrontendStatsTracer StatsTracer(Context.Stats,
                                  "typecheck-completion-operator", expr);
  PrettyStackTraceExpr stackTrace(Context, "type-checking", expr);

  expr = expr->walk(SanitizeExpr(Context,
                                 /*shouldReusePrecheckedType=*/true));

  ConstraintSystemOptions options;
  options |= ConstraintSystemFlags::SuppressDiagnostics;
  options |= ConstraintSystemFlags::ReusePrecheckedType;

  // Construct a constraint system from this expression.
  ConstraintSystem CS(DC, options);
  expr = CS.generateConstraints(expr, DC);
  if (!expr)
    return nullptr;

  if (CS.isDebugMode()) {
    auto &log = llvm::errs();
    log << "---Initial constraints for the given expression---\n";
    expr->dump(log);
    log << "\n";
    CS.print(log);
  }

  // Attempt to solve the constraint system.
  SmallVector<Solution, 4> viable;
  if (CS.solve(viable, FreeTypeVariableBinding::Disallow))
    return nullptr;

  auto &solution = viable[0];
  if (CS.isDebugMode()) {
    auto &log = llvm::errs();
    log << "---Solution---\n";
    solution.dump(log);
  }

  // Fill the results.
  Expr *opExpr = cast<ApplyExpr>(expr)->getFn();
  referencedDecl =
      solution.resolveLocatorToDecl(CS.getConstraintLocator(opExpr));

  // Return '(ArgType[, ArgType]) -> ResultType' as a function type.
  // We don't use the type of the operator expression because we want the types
  // of the *arguments* instead of the types of the parameters.
  Expr *argsExpr = cast<ApplyExpr>(expr)->getArg();
  SmallVector<FunctionType::Param, 2> argTypes;
  if (auto *PE = dyn_cast<ParenExpr>(argsExpr)) {
    argTypes.emplace_back(solution.simplifyType(CS.getType(PE->getSubExpr())));
  } else if (auto *TE = dyn_cast<TupleExpr>(argsExpr)) {
    for (auto arg : TE->getElements())
      argTypes.emplace_back(solution.simplifyType(CS.getType(arg)));
  }

  return FunctionType::get(argTypes, solution.simplifyType(CS.getType(expr)));
}

/// Return the type of operator function for specified LHS, or a null
/// \c Type on error.
FunctionType *
TypeChecker::getTypeOfCompletionOperator(DeclContext *DC, Expr *LHS,
                                         Identifier opName, DeclRefKind refKind,
                                         ConcreteDeclRef &referencedDecl) {

  // For the infix operator, find the actual LHS from pre-folded LHS.
  if (refKind == DeclRefKind::BinaryOperator)
    LHS = TypeChecker::findLHS(DC, LHS, opName);

  if (!LHS)
    return nullptr;

  auto LHSTy = LHS->getType();

  // FIXME: 'UnresolvedType' still might be typechecked by an operator.
  if (!LHSTy || LHSTy->is<UnresolvedType>())
    return nullptr;

  // Meta types and function types cannot be a operand of operator expressions.
  if (LHSTy->is<MetatypeType>() || LHSTy->is<AnyFunctionType>())
    return nullptr;

  auto Loc = LHS->getEndLoc();

  // Build temporary expression to typecheck.
  // We allocate these expressions on the stack because we know they can't
  // escape and there isn't a better way to allocate scratch Expr nodes.
  UnresolvedDeclRefExpr UDRE(DeclNameRef(opName), refKind, DeclNameLoc(Loc));
  auto *opExpr = TypeChecker::resolveDeclRefExpr(
      &UDRE, DC, /*replaceInvalidRefsWithErrors=*/true);

  switch (refKind) {
  case DeclRefKind::PostfixOperator: {
    // (postfix_unary_expr
    //   (declref_expr name=<opName>)
    //   (paren_expr
    //     (<LHS>)))
    ParenExpr Args(SourceLoc(), LHS, SourceLoc(),
                   /*hasTrailingClosure=*/false);
    PostfixUnaryExpr postfixExpr(opExpr, &Args);
    return getTypeOfCompletionOperatorImpl(DC, &postfixExpr,
                                           referencedDecl);
  }

  case DeclRefKind::BinaryOperator: {
    // (binary_expr
    //   (declref_expr name=<opName>)
    //   (tuple_expr
    //     (<LHS>)
    //     (code_completion_expr)))
    CodeCompletionExpr dummyRHS(Loc);
    auto Args = TupleExpr::create(
        DC->getASTContext(), SourceLoc(), {LHS, &dummyRHS}, {}, {}, SourceLoc(),
        /*hasTrailingClosure=*/false, /*isImplicit=*/true);
    BinaryExpr binaryExpr(opExpr, Args, /*isImplicit=*/true);

    return getTypeOfCompletionOperatorImpl(DC, &binaryExpr,
                                           referencedDecl);
  }

  default:
    llvm_unreachable("Invalid DeclRefKind for operator completion");
  }
}

bool TypeChecker::typeCheckForCodeCompletion(
    SolutionApplicationTarget &target,
    llvm::function_ref<void(const Solution &)> callback) {
  auto *DC = target.getDeclContext();
  auto &Context = DC->getASTContext();

  auto *expr = target.getAsExpr();
  if (!expr)
    return false;

  // First of all, let's check whether given target expression
  // does indeed have the code completion location in it.
  {
    auto range = expr->getSourceRange();
    if (range.isInvalid() ||
        !Context.SourceMgr.rangeContainsCodeCompletionLoc(range))
      return false;
  }

  FrontendStatsTracer StatsTracer(Context.Stats,
                                  "typecheck-for-code-completion", expr);
  PrettyStackTraceExpr stackTrace(Context, "code-completion", expr);

  expr = expr->walk(SanitizeExpr(Context,
                                 /*shouldReusePrecheckedType=*/false));

  enum class ContextKind {
    Expression,
    Application,
    StringInterpolation,
    SingleStmtClosure,
    MultiStmtClosure,
    ErrorExpression
  };

  class ContextFinder : public ASTWalker {
    using Context = std::pair<ContextKind, Expr *>;

    // Stack of all "interesting" contexts up to code completion expression.
    llvm::SmallVector<Context, 4> Contexts;

    Expr *CompletionExpr = nullptr;

  public:
    ContextFinder(Expr *E) {
      Contexts.push_back(std::make_pair(ContextKind::Expression, E));
    }

    std::pair<bool, Expr *> walkToExprPre(Expr *E) override {
      if (auto *closure = dyn_cast<ClosureExpr>(E)) {
        Contexts.push_back(std::make_pair(closure->hasSingleExpressionBody()
                                              ? ContextKind::SingleStmtClosure
                                              : ContextKind::MultiStmtClosure,
                                          closure));
      }

      if (isa<InterpolatedStringLiteralExpr>(E)) {
        Contexts.push_back(std::make_pair(ContextKind::StringInterpolation, E));
      }

      if (isa<ApplyExpr>(E)) {
        Contexts.push_back(std::make_pair(ContextKind::Application, E));
      }

      if (isa<CodeCompletionExpr>(E)) {
        CompletionExpr = E;
        return std::make_pair(false, nullptr);
      }

      if (auto *Error = dyn_cast<ErrorExpr>(E)) {
        Contexts.push_back(std::make_pair(ContextKind::ErrorExpression, E));
        if (auto *OrigExpr = Error->getOriginalExpr()) {
          OrigExpr->walk(*this);
          return std::make_pair(false, hasCompletionExpr() ? nullptr : E);
        }
      }

      return std::make_pair(true, E);
    }

    Expr *walkToExprPost(Expr *E) override {
      if (isa<ClosureExpr>(E) || isa<InterpolatedStringLiteralExpr>(E) ||
          isa<ApplyExpr>(E) || isa<ErrorExpr>(E))
        Contexts.pop_back();
      return E;
    }

    /// Check whether code completion expression is located inside of a
    /// multi-statement closure.
    bool locatedInMultiStmtClosure() const {
      return hasContext(ContextKind::MultiStmtClosure);
    }

    bool locatedInStringIterpolation() const {
      return hasContext(ContextKind::StringInterpolation);
    }

    bool hasCompletionExpr() const {
      return CompletionExpr;
    }

    Expr *getCompletionExpr() const {
      assert(CompletionExpr);
      return CompletionExpr;
    }

    ErrorExpr *getInnermostErrorExpr() const {
      for (const Context &curr : llvm::reverse(Contexts)) {
        if (curr.first == ContextKind::ErrorExpression)
          return cast<ErrorExpr>(curr.second);
      }
      return nullptr;
    }

    ClosureExpr *getOutermostMultiStmtClosure() const {
      for (const Context &curr : Contexts) {
        if (curr.first == ContextKind::MultiStmtClosure)
          return cast<ClosureExpr>(curr.second);
      }
      return nullptr;
    }

    /// As a fallback sometimes its useful to not only type-check
    /// code completion expression directly but instead add some
    /// of the enclosing context e.g. when completion is an argument
    /// to a call.
    Expr *getCompletionExprInContext() const {
      assert(CompletionExpr);

      auto &innerContext = Contexts.back();
      return innerContext.first == ContextKind::Application
                 ? innerContext.second
                 : CompletionExpr;
    }

  private:
    bool hasContext(ContextKind kind) const {
      return llvm::find_if(Contexts, [&kind](const Context &currContext) {
               return currContext.first == kind;
             }) != Contexts.end();
    }
  };

  ContextFinder contextAnalyzer(expr);
  expr->walk(contextAnalyzer);

  // If there was no completion expr (e.g. if the code completion location was
  // among tokens that were skipped over during parser error recovery) bail.
  if (!contextAnalyzer.hasCompletionExpr())
    return false;

  // If the completion expression is in a valid subexpression of an ErrorExpr,
  // fallback to trying the valid subexpression without any context. This can
  // happen for cases like `expectsBoolArg(foo.<complete>).` which becomes an
  // ErrorExpr due to the missing member name after the final dot.
  if (auto *errorExpr = contextAnalyzer.getInnermostErrorExpr()) {
    if (auto *origExpr = errorExpr->getOriginalExpr()) {
      SolutionApplicationTarget completionTarget(origExpr, DC, CTP_Unused,
                                                 /*contextualType=*/Type(),
                                                 /*isDiscarded=*/true);
      return typeCheckForCodeCompletion(completionTarget, callback);
    }
  }

  // Interpolation components are type-checked separately.
  if (contextAnalyzer.locatedInStringIterpolation())
    return false;

  // FIXME: There is currently no way to distinguish between
  // multi-statement closures which are function builder bodies
  // (that are type-checked together with enclosing context)
  // and regular closures which are type-checked separately.

  {
    // First, pre-check the expression, validating any types that occur in the
    // expression and folding sequence expressions.
    auto failedPreCheck = ConstraintSystem::preCheckExpression(
        expr, DC,
        /*replaceInvalidRefsWithErrors=*/true);

    target.setExpr(expr);

    if (failedPreCheck)
      return false;
  }

  enum class CompletionResult { Ok, NotApplicable, Fallback };

  auto solveForCodeCompletion =
      [&](SolutionApplicationTarget &target) -> CompletionResult {
    ConstraintSystemOptions options;
    options |= ConstraintSystemFlags::AllowFixes;
    options |= ConstraintSystemFlags::SuppressDiagnostics;

    ConstraintSystem cs(DC, options);

    llvm::SmallVector<Solution, 4> solutions;

    // If solve failed to generate constraints or with some other
    // issue, we need to fallback to type-checking code completion
    // expression directly.
    if (!cs.solveForCodeCompletion(target, solutions))
      return CompletionResult::Fallback;

    // If case type-check didn't produce any solutions, let's
    // attempt to type-check code completion expression without
    // enclosing context.
    if (solutions.empty())
      return CompletionResult::Fallback;

    // If code completion expression resides inside of multi-statement
    // closure body it code either be type-checker together with context
    // or not, it's impossible to say without trying. If solution
    // doesn't have a type for a code completion expression it means that
    // we have to wait until body of the closure is type-checked.
    if (contextAnalyzer.locatedInMultiStmtClosure()) {
      auto &solution = solutions.front();

      // Let's check whether closure participated in the type-check.
      if (solution.hasType(contextAnalyzer.getCompletionExpr())) {
        llvm::for_each(solutions, callback);
        return CompletionResult::Ok;
      }

      if (solutions.size() > 1)
        return CompletionResult::Fallback;

      auto *closure = contextAnalyzer.getOutermostMultiStmtClosure();
      auto closureType = solution.getResolvedType(closure);

      if (closureType->hasUnresolvedType())
        return CompletionResult::Fallback;

      return CompletionResult::NotApplicable;
    }

    llvm::for_each(solutions, callback);
    return CompletionResult::Ok;
  };

  switch (solveForCodeCompletion(target)) {
  case CompletionResult::Ok:
    return true;

  case CompletionResult::NotApplicable:
    return false;

  case CompletionResult::Fallback:
    break;
  }

  {
    auto *completionExpr = contextAnalyzer.getCompletionExpr();

    if (contextAnalyzer.locatedInMultiStmtClosure()) {
      auto completionInContext = contextAnalyzer.getCompletionExprInContext();
      // If pre-check fails, let's switch to code completion
      // expression without any enclosing context.
      if (ConstraintSystem::preCheckExpression(
              completionInContext, DC, /*replaceInvalidRefsWithErrors=*/true)) {
        completionExpr = contextAnalyzer.getCompletionExpr();
      } else {
        completionExpr = completionInContext;
      }
    }

    // If initial solve failed, let's fallback to checking only code completion
    // expresion without any context.
    SolutionApplicationTarget completionTarget(completionExpr, DC, CTP_Unused,
                                               /*contextualType=*/Type(),
                                               /*isDiscarded=*/true);

    switch (solveForCodeCompletion(completionTarget)) {
    case CompletionResult::Ok:
    case CompletionResult::Fallback:
      break;
    case CompletionResult::NotApplicable:
      llvm_unreachable("solve on CodeCompletionExpr produced not applicable?");
    }
  }
  return true;
}

static Optional<Type> getTypeOfCompletionContextExpr(
                        DeclContext *DC,
                        CompletionTypeCheckKind kind,
                        Expr *&parsedExpr,
                        ConcreteDeclRef &referencedDecl) {
  if (constraints::ConstraintSystem::preCheckExpression(
          parsedExpr, DC, /*replaceInvalidRefsWithErrors=*/true))
    return None;

  switch (kind) {
  case CompletionTypeCheckKind::Normal:
    // Handle below.
    break;

  case CompletionTypeCheckKind::KeyPath:
    referencedDecl = nullptr;
    if (auto keyPath = dyn_cast<KeyPathExpr>(parsedExpr)) {
      auto components = keyPath->getComponents();
      if (!components.empty()) {
        auto &last = components.back();
        if (last.isResolved()) {
          if (last.getKind() == KeyPathExpr::Component::Kind::Property)
            referencedDecl = last.getDeclRef();
          Type lookupTy = last.getComponentType();
          ASTContext &Ctx = DC->getASTContext();
          if (auto bridgedClass = Ctx.getBridgedToObjC(DC, lookupTy))
            return bridgedClass;
          return lookupTy;
        }
      }
    }

    return None;
  }

  Type originalType = parsedExpr->getType();
  if (auto T = getTypeOfExpressionWithoutApplying(parsedExpr, DC,
                 referencedDecl, FreeTypeVariableBinding::UnresolvedType))
    return T;

  // Try to recover if we've made any progress.
  if (parsedExpr &&
      !isa<ErrorExpr>(parsedExpr) &&
      parsedExpr->getType() &&
      !parsedExpr->getType()->hasError() &&
      (originalType.isNull() ||
       !parsedExpr->getType()->isEqual(originalType))) {
    return parsedExpr->getType();
  }

  return None;
}

/// Return the type of an expression parsed during code completion, or
/// a null \c Type on error.
Optional<Type> swift::getTypeOfCompletionContextExpr(
                        ASTContext &Ctx,
                        DeclContext *DC,
                        CompletionTypeCheckKind kind,
                        Expr *&parsedExpr,
                        ConcreteDeclRef &referencedDecl) {
  DiagnosticSuppression suppression(Ctx.Diags);

  // Try to solve for the actual type of the expression.
  return ::getTypeOfCompletionContextExpr(DC, kind, parsedExpr,
                                          referencedDecl);
}

/// Return the type of operator function for specified LHS, or a null
/// \c Type on error.
FunctionType *
swift::getTypeOfCompletionOperator(DeclContext *DC, Expr *LHS,
                                   Identifier opName, DeclRefKind refKind,
                                   ConcreteDeclRef &referencedDecl) {
  auto &ctx = DC->getASTContext();
  DiagnosticSuppression suppression(ctx.Diags);
  return TypeChecker::getTypeOfCompletionOperator(DC, LHS, opName, refKind,
                                                  referencedDecl);
}

bool swift::typeCheckExpression(DeclContext *DC, Expr *&parsedExpr) {
  auto &ctx = DC->getASTContext();

  parsedExpr = parsedExpr->walk(SanitizeExpr(ctx, /*shouldReusePrecheckedType=*/false));

  DiagnosticSuppression suppression(ctx.Diags);
  auto resultTy = TypeChecker::typeCheckExpression(parsedExpr, DC, Type(),
                                                   CTP_Unused);
  return !resultTy;
}

LookupResult
swift::lookupSemanticMember(DeclContext *DC, Type ty, DeclName name) {
  return TypeChecker::lookupMember(DC, ty, DeclNameRef(name), None);
}

void DotExprTypeCheckCompletionCallback::fallbackTypeCheck() {
  assert(!gotCallback());
  SolutionApplicationTarget completionTarget(CompletionExpr, DC, CTP_Unused,
                                             Type(), /*isDiscared=*/true);
  TypeChecker::typeCheckForCodeCompletion(
      completionTarget, [&](const Solution &S) { sawSolution(S); });
}

void DotExprTypeCheckCompletionCallback::
sawSolution(const constraints::Solution &S) {
  GotCallback = true;
  auto &CS = S.getConstraintSystem();

  auto GetType = [&](Expr *E) {
    // To aid code completion, we need to attempt to convert type holes
    // back into underlying generic parameters if possible, since type
    // of the code completion expression is used as "expected" (or contextual)
    // type so it's helpful to know what requirements it has to filter
    // the list of possible member candidates e.g.
    //
    // \code
    // func test<T: P>(_: [T]) {}
    //
    // test(42.#^MEMBERS^#)
    // \code
    //
    // It's impossible to resolve `T` in this case but code completion
    // expression should still have a type of `[T]` instead of `[<<hole>>]`
    // because it helps to produce correct contextual member list based on
    // a conformance requirement associated with generic parameter `T`.
    if (isa<CodeCompletionExpr>(E)) {
      auto completionTy = S.getType(E).transform([&](Type type) -> Type {
        if (auto *typeVar = type->getAs<TypeVariableType>())
          return S.getFixedType(typeVar);
        return type;
      });

      return S.simplifyType(completionTy.transform([&](Type type) {
        if (auto *hole = type->getAs<HoleType>()) {
          if (auto *typeVar =
                  hole->getOriginator().dyn_cast<TypeVariableType *>()) {
            if (auto *GP = typeVar->getImpl().getGenericParameter()) {
              // Code completion depends on generic parameter type being
              // represented in terms of `ArchetypeType` since it's easy
              // to extract protocol requirements from it.
              if (auto *GPD = GP->getDecl())
                return GPD->getInnermostDeclContext()->mapTypeIntoContext(GP);
            }
          }

          return Type(CS.getASTContext().TheUnresolvedType);
        }

        return type;
      }));
    }

    return S.getResolvedType(E);
  };

  auto *ParsedExpr = CompletionExpr->getBase();
  auto *SemanticExpr = ParsedExpr->getSemanticsProvidingExpr();

  auto BaseTy = GetType(ParsedExpr);
  // If base type couldn't be determined (e.g. because base expression
  // is an invalid reference), let's not attempt to do a lookup since
  // it wouldn't produce any useful results anyway.
  if (!BaseTy || BaseTy->is<UnresolvedType>())
    return;

  auto *Locator = CS.getConstraintLocator(SemanticExpr);
  Type ExpectedTy = GetType(CompletionExpr);
  Expr *ParentExpr = CS.getParentExpr(CompletionExpr);
  if (!ParentExpr)
    ExpectedTy = CS.getContextualType(CompletionExpr);

  auto *CalleeLocator = S.getCalleeLocator(Locator);
  ValueDecl *ReferencedDecl = nullptr;
  if (auto SelectedOverload = S.getOverloadChoiceIfAvailable(CalleeLocator))
    ReferencedDecl = SelectedOverload->choice.getDeclOrNull();

  auto Key = std::make_pair(BaseTy, ReferencedDecl);
  auto Ret = BaseToSolutionIdx.insert({Key, Results.size()});
  if (!Ret.second && ExpectedTy) {
    Results[Ret.first->getSecond()].ExpectedTypes.push_back(ExpectedTy);
  } else {
    bool ISDMT = S.isStaticallyDerivedMetatype(ParsedExpr);
    bool SingleExprBody = false;
    bool DisallowVoid = ExpectedTy
                            ? !ExpectedTy->isVoid()
                            : !ParentExpr && CS.getContextualTypePurpose(
                                                 CompletionExpr) != CTP_Unused;

    if (!ParentExpr) {
      if (CS.getContextualTypePurpose(CompletionExpr) == CTP_ReturnSingleExpr)
        SingleExprBody = true;
    } else if (auto *ParentCE = dyn_cast<ClosureExpr>(ParentExpr)) {
      if (ParentCE->hasSingleExpressionBody() &&
          ParentCE->getSingleExpressionBody() == CompletionExpr) {
        ASTNode Last = ParentCE->getBody()->getLastElement();
        if (!Last.isStmt(StmtKind::Return) || Last.isImplicit())
          SingleExprBody = true;
      }
    }

    Results.push_back(
        {BaseTy, ReferencedDecl, {}, DisallowVoid, ISDMT, SingleExprBody});
    if (ExpectedTy)
      Results.back().ExpectedTypes.push_back(ExpectedTy);
  }
}
