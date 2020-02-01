//===--- CSDiag.cpp - Constraint Diagnostics ------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file implements diagnostics for the type checker.
//
//===----------------------------------------------------------------------===//

#include "CSDiag.h"
#include "CSDiagnostics.h"
#include "CalleeCandidateInfo.h"
#include "ConstraintSystem.h"
#include "MiscDiagnostics.h"
#include "TypeCheckAvailability.h"
#include "TypoCorrection.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/Initializer.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/TypeMatcher.h"
#include "swift/AST/TypeWalker.h"
#include "swift/Basic/Defer.h"
#include "swift/Basic/StringExtras.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/Support/Compiler.h"
#include "llvm/Support/SaveAndRestore.h"

using namespace swift;
using namespace constraints;

namespace swift {
  Type replaceTypeParametersWithUnresolved(Type ty) {
    if (!ty) return ty;
    
    if (!ty->hasTypeParameter() && !ty->hasArchetype()) return ty;
    
    auto &ctx = ty->getASTContext();
    
    return ty.transform([&](Type type) -> Type {
      if (type->is<ArchetypeType>() ||
          type->isTypeParameter())
        return ctx.TheUnresolvedType;
      return type;
    });
  }

  Type replaceTypeVariablesWithUnresolved(Type ty) {
    if (!ty) return ty;
    
    if (!ty->hasTypeVariable()) return ty;
    
    auto &ctx = ty->getASTContext();
    
    return ty.transform([&](Type type) -> Type {
      if (type->isTypeVariableOrMember())
        return ctx.TheUnresolvedType;
      return type;
    });
  }
};

static bool isUnresolvedOrTypeVarType(Type ty) {
  return ty->isTypeVariableOrMember() || ty->is<UnresolvedType>();
}

/// Flags that can be used to control name lookup.
enum TCCFlags {
  /// Allow the result of the subexpression to be an lvalue.  If this is not
  /// specified, any lvalue will be forced to be loaded into an rvalue.
  TCC_AllowLValue = 0x01,
  
  /// Re-type-check the given subexpression even if the expression has already
  /// been checked already.  The client is asserting that infinite recursion is
  /// not possible because it has relaxed a constraint on the system.
  TCC_ForceRecheck = 0x02,
    
  /// tell typeCheckExpression that it is ok to produce an ambiguous result,
  /// it can just fill in holes with UnresolvedType and we'll deal with it.
  TCC_AllowUnresolvedTypeVariables = 0x04
};

using TCCOptions = OptionSet<TCCFlags>;

inline TCCOptions operator|(TCCFlags flag1, TCCFlags flag2) {
  return TCCOptions(flag1) | flag2;
}


namespace {
/// If a constraint system fails to converge on a solution for a given
/// expression, this class can produce a reasonable diagnostic for the failure
/// by analyzing the remnants of the failed constraint system. (Specifically,
/// left-over inactive, active and failed constraints.)
/// This class does not tune its diagnostics for a specific expression kind,
/// for that, you'll want to use an instance of the FailureDiagnosis class.
class FailureDiagnosis :public ASTVisitor<FailureDiagnosis, /*exprresult*/bool>{
  friend class ASTVisitor<FailureDiagnosis, /*exprresult*/bool>;
  
  Expr *expr = nullptr;
  ConstraintSystem &CS;

public:
  FailureDiagnosis(Expr *expr, ConstraintSystem &cs) : expr(expr), CS(cs) {
    assert(expr);
  }

  template<typename ...ArgTypes>
  InFlightDiagnostic diagnose(ArgTypes &&...Args) {
    return CS.getASTContext().Diags.diagnose(std::forward<ArgTypes>(Args)...);
  }

  /// Unless we've already done this, retypecheck the specified child of the
  /// current expression on its own, without including any contextual
  /// constraints or the parent expr nodes.  This is more likely to succeed than
  /// type checking the original expression.
  ///
  /// This mention may only be used on immediate children of the current expr
  /// node, because ClosureExpr parameters need to be treated specially.
  ///
  /// This can return a new expression (for e.g. when a UnresolvedDeclRef gets
  /// resolved) and returns null when the subexpression fails to typecheck.
  ///
  Expr *typeCheckChildIndependently(
      Expr *subExpr, Type convertType = Type(),
      ContextualTypePurpose convertTypePurpose = CTP_Unused,
      TCCOptions options = TCCOptions(),
      ExprTypeCheckListener *listener = nullptr,
      bool allowFreeTypeVariables = true);
  Expr *typeCheckChildIndependently(Expr *subExpr, TCCOptions options,
                                    bool allowFreeTypeVariables = true) {
    return typeCheckChildIndependently(subExpr, Type(), CTP_Unused, options,
                                       nullptr, allowFreeTypeVariables);
  }

  Type getTypeOfTypeCheckedChildIndependently(Expr *subExpr,
                                            TCCOptions options = TCCOptions()) {
    auto e = typeCheckChildIndependently(subExpr, options);
    return e ? CS.getType(e) : Type();
  }

  /// Find a nearest declaration context which could be used
  /// to type-check this sub-expression.
  DeclContext *findDeclContext(Expr *subExpr) const;

  /// Special magic to handle inout exprs and tuples in argument lists.
  Expr *typeCheckArgumentChildIndependently(Expr *argExpr, Type argType,
                                        const CalleeCandidateInfo &candidates,
                                            TCCOptions options = TCCOptions());

  void getPossibleTypesOfExpressionWithoutApplying(
      Expr *&expr, DeclContext *dc, SmallPtrSetImpl<TypeBase *> &types,
      FreeTypeVariableBinding allowFreeTypeVariables =
          FreeTypeVariableBinding::Disallow,
      ExprTypeCheckListener *listener = nullptr) {
    TypeChecker::getPossibleTypesOfExpressionWithoutApplying(
        expr, dc, types, allowFreeTypeVariables, listener);
    CS.cacheExprTypes(expr);
  }

  Type getTypeOfExpressionWithoutApplying(
      Expr *&expr, DeclContext *dc, ConcreteDeclRef &referencedDecl,
      FreeTypeVariableBinding allowFreeTypeVariables =
          FreeTypeVariableBinding::Disallow,
      ExprTypeCheckListener *listener = nullptr) {
    auto type =
        TypeChecker::getTypeOfExpressionWithoutApplying(expr, dc,
                                                        referencedDecl,
                                                        allowFreeTypeVariables,
                                                        listener);
    CS.cacheExprTypes(expr);
    return type;
  }

  /// Diagnose common failures due to applications of an argument list to an
  /// ApplyExpr or SubscriptExpr.
  bool diagnoseParameterErrors(CalleeCandidateInfo &CCI,
                               Expr *fnExpr, Expr *argExpr,
                               ArrayRef<Identifier> argLabels);

  /// Attempt to diagnose a specific failure from the info we've collected from
  /// the failed constraint system.
  bool diagnoseExprFailure();

  /// Emit an ambiguity diagnostic about the specified expression.
  void diagnoseAmbiguity(Expr *E);

  /// Attempt to produce a diagnostic for a mismatch between an expression's
  /// type and its assumed contextual type.
  bool diagnoseContextualConversionError(Expr *expr, Type contextualType,
                                         ContextualTypePurpose CTP,
                                         Type suggestedType = Type());

private:
  /// Validate potential contextual type for type-checking one of the
  /// sub-expressions, usually correct/valid types are the ones which
  /// either don't have type variables or are not generic, because
  /// generic types with left-over type variables or unresolved types
  /// degrade quality of diagnostics if allowed to be used as contextual.
  ///
  /// \param contextualType The candidate contextual type.
  /// \param CTP The contextual purpose attached to the given candidate.
  ///
  /// \returns Pair of validated type and it's purpose, potentially nullified
  /// if it wasn't an appropriate type to be used.
  std::pair<Type, ContextualTypePurpose>
  validateContextualType(Type contextualType, ContextualTypePurpose CTP);

  bool visitExpr(Expr *E);

  bool visitApplyExpr(ApplyExpr *AE);
  bool visitRebindSelfInConstructorExpr(RebindSelfInConstructorExpr *E);
};
} // end anonymous namespace

namespace {
  class ExprTypeSaverAndEraser {
    llvm::DenseMap<Expr*, Type> ExprTypes;
    llvm::DenseMap<TypeLoc*, Type> TypeLocTypes;
    llvm::DenseMap<Pattern*, Type> PatternTypes;
    ExprTypeSaverAndEraser(const ExprTypeSaverAndEraser&) = delete;
    void operator=(const ExprTypeSaverAndEraser&) = delete;
  public:

    ExprTypeSaverAndEraser(Expr *E) {
      struct TypeSaver : public ASTWalker {
        ExprTypeSaverAndEraser *TS;
        TypeSaver(ExprTypeSaverAndEraser *TS) : TS(TS) {}
        
        std::pair<bool, Expr *> walkToExprPre(Expr *expr) override {
          TS->ExprTypes[expr] = expr->getType();

          SWIFT_DEFER {
            assert((!expr->getType() || !expr->getType()->hasTypeVariable()
                    // FIXME: We shouldn't allow these, either.
                    || isa<LiteralExpr>(expr)) &&
                   "Type variable didn't get erased!");
          };

          // Preserve module expr type data to prevent further lookups.
          if (auto *declRef = dyn_cast<DeclRefExpr>(expr))
            if (isa<ModuleDecl>(declRef->getDecl()))
              return { false, expr };
          
          // Don't strip type info off OtherConstructorDeclRefExpr, because
          // CSGen doesn't know how to reconstruct it.
          if (isa<OtherConstructorDeclRefExpr>(expr))
            return { false, expr };
          
          // If a literal has a Builtin.Int or Builtin.FP type on it already,
          // then sema has already expanded out a call to
          //   Init.init(<builtinliteral>)
          // and we don't want it to make
          //   Init.init(Init.init(<builtinliteral>))
          // preserve the type info to prevent this from happening.
          if (isa<LiteralExpr>(expr) && !isa<InterpolatedStringLiteralExpr>(expr) &&
              !(expr->getType() && expr->getType()->hasError()))
            return { false, expr };
          
          expr->setType(nullptr);

          return { true, expr };
        }
        
        // If we find a TypeLoc (e.g. in an as? expr), save and erase it.
        bool walkToTypeLocPre(TypeLoc &TL) override {
          if (TL.getTypeRepr() && TL.getType()) {
            TS->TypeLocTypes[&TL] = TL.getType();
            TL.setType(Type());
          }
          return true;
        }
        
        std::pair<bool, Pattern*> walkToPatternPre(Pattern *P) override {
          if (P->hasType()) {
            TS->PatternTypes[P] = P->getType();
            P->setType(Type());
          }
          return { true, P };
        }
        
        // Don't walk into statements.  This handles the BraceStmt in
        // non-single-expr closures, so we don't walk into their body.
        std::pair<bool, Stmt *> walkToStmtPre(Stmt *S) override {
          return { false, S };
        }
      };
      
      E->walk(TypeSaver(this));
    }
    
    void restore() {
      for (auto exprElt : ExprTypes)
        exprElt.first->setType(exprElt.second);
      
      for (auto typelocElt : TypeLocTypes)
        typelocElt.first->setType(typelocElt.second);
      
      for (auto patternElt : PatternTypes)
        patternElt.first->setType(patternElt.second);
      
      // Done, don't do redundant work on destruction.
      ExprTypes.clear();
      TypeLocTypes.clear();
      PatternTypes.clear();
    }
    
    // On destruction, if a type got wiped out, reset it from null to its
    // original type.  This is helpful because type checking a subexpression
    // can lead to replacing the nodes in that subexpression.  However, the
    // failed ConstraintSystem still has locators pointing to the old nodes,
    // and if expr-specific diagnostics fail to turn up anything useful to say,
    // we go digging through failed constraints, and expect their locators to
    // still be meaningful.
    ~ExprTypeSaverAndEraser() {
      for (auto exprElt : ExprTypes)
        if (!exprElt.first->getType())
          exprElt.first->setType(exprElt.second);
      
      for (auto typelocElt : TypeLocTypes)
        if (!typelocElt.first->getType())
          typelocElt.first->setType(typelocElt.second);
      
      for (auto patternElt : PatternTypes)
        if (!patternElt.first->hasType())
          patternElt.first->setType(patternElt.second);
    }
  };
} // end anonymous namespace

/// Unless we've already done this, retypecheck the specified subexpression on
/// its own, without including any contextual constraints or parent expr
/// nodes.  This is more likely to succeed than type checking the original
/// expression.
///
/// This can return a new expression (for e.g. when a UnresolvedDeclRef gets
/// resolved) and returns null when the subexpression fails to typecheck.
Expr *FailureDiagnosis::typeCheckChildIndependently(
    Expr *subExpr, Type convertType, ContextualTypePurpose convertTypePurpose,
    TCCOptions options, ExprTypeCheckListener *listener,
    bool allowFreeTypeVariables) {

  // If this sub-expression is currently being diagnosed, refuse to recheck the
  // expression (which may lead to infinite recursion).  If the client is
  // telling us that it knows what it is doing, then believe it.
  if (!options.contains(TCC_ForceRecheck)) {
    if (CS.isExprBeingDiagnosed(subExpr)) {
      auto *savedExpr = CS.getExprBeingDiagnosed(subExpr);
      if (subExpr == savedExpr)
        return subExpr;

      CS.cacheExprTypes(savedExpr);
      return savedExpr;
    }
  }

  // Mark current expression as about to be diagnosed.
  CS.addExprForDiagnosis(subExpr, subExpr);

  // Validate contextual type before trying to use it.
  std::tie(convertType, convertTypePurpose) =
      validateContextualType(convertType, convertTypePurpose);

  // If we have no contextual type information and the subexpr is obviously a
  // overload set, don't recursively simplify this.  The recursive solver will
  // sometimes pick one based on arbitrary ranking behavior (e.g. like
  // which is the most specialized) even then all the constraints are being
  // fulfilled by UnresolvedType, which doesn't tell us anything.
  if (convertTypePurpose == CTP_Unused &&
      (isa<OverloadedDeclRefExpr>(subExpr->getValueProvidingExpr()))) {
    return subExpr;
  }

  // Save any existing type data of the subexpr tree, and reset it to null in
  // prep for re-type-checking the tree.  If things fail, we can revert the
  // types back to their original state.
  ExprTypeSaverAndEraser SavedTypeData(subExpr);
  
  // Store off the sub-expression, in case a new one is provided via the
  // type check operation.
  Expr *preCheckedExpr = subExpr;
  
  // Make sure that typechecker knows that this is an attempt
  // to diagnose a problem.
  TypeCheckExprOptions TCEOptions =
      TypeCheckExprFlags::SubExpressionDiagnostics;

  // Claim that the result is discarded to preserve the lvalue type of
  // the expression.
  if (options.contains(TCC_AllowLValue))
    TCEOptions |= TypeCheckExprFlags::IsDiscarded;

  // If there is no contextual type available, tell typeCheckExpression that it
  // is ok to produce an ambiguous result, it can just fill in holes with
  // UnresolvedType and we'll deal with it.
  if ((!convertType || options.contains(TCC_AllowUnresolvedTypeVariables)) &&
      allowFreeTypeVariables)
    TCEOptions |= TypeCheckExprFlags::AllowUnresolvedTypeVariables;

  // When we're type checking a single-expression closure, we need to reset the
  // DeclContext to this closure for the recursive type checking.  Otherwise,
  // if there is a closure in the subexpression, we can violate invariants.
  auto *DC = findDeclContext(subExpr);
  auto resultTy =
      TypeChecker::typeCheckExpression(subExpr, DC,
                                       TypeLoc::withoutLoc(convertType),
                                       convertTypePurpose, TCEOptions,
                                       listener, &CS);

  CS.cacheExprTypes(subExpr);

  // This is a terrible hack to get around the fact that typeCheckExpression()
  // might change subExpr to point to a new OpenExistentialExpr. In that case,
  // since the caller passed subExpr by value here, they would be left
  // holding on to an expression containing open existential types but
  // no OpenExistentialExpr, which breaks invariants enforced by the
  // ASTChecker.
  // Another reason why we need to do this is because diagnostics might pick
  // constraint anchor for re-typechecking which would only have opaque value
  // expression and not enclosing open existential, which is going to trip up
  // sanitizer.
  eraseOpenedExistentials(CS, subExpr);

  // If recursive type checking failed, then an error was emitted.  Return
  // null to indicate this to the caller.
  if (!resultTy)
    return nullptr;

  // If we type checked the result but failed to get a usable output from it,
  // just pretend as though nothing happened.
  if (resultTy->is<ErrorType>()) {
    subExpr = preCheckedExpr;
    if (subExpr->getType())
      CS.cacheType(subExpr);
    SavedTypeData.restore();
  }

  if (preCheckedExpr != subExpr)
    CS.addExprForDiagnosis(preCheckedExpr, subExpr);

  return subExpr;
}

DeclContext *FailureDiagnosis::findDeclContext(Expr *subExpr) const {
  if (auto *closure =
          dyn_cast<ClosureExpr>(subExpr->getSemanticsProvidingExpr()))
    return closure->getParent();

  struct DCFinder : public ASTWalker {
    DeclContext *DC, *CurrDC;
    Expr *SubExpr;

    DCFinder(DeclContext *DC, Expr *expr) : DC(DC), CurrDC(DC), SubExpr(expr) {}

    std::pair<bool, Expr *> walkToExprPre(Expr *E) override {
      if (E == SubExpr) {
        DC = CurrDC;
        return {false, nullptr};
      }

      if (auto *closure = dyn_cast<ClosureExpr>(E)) {
        CurrDC = closure;
        // If we have a ClosureExpr parent of the specified node, check to make
        // sure none of its arguments are type variables.  If so, these type
        // variables would be accessible to name lookup of the subexpression and
        // may thus leak in.  Reset them to UnresolvedTypes for safe measures.
        assert(llvm::all_of(*closure->getParameters(), [](const ParamDecl *PD) {
          if (PD->hasInterfaceType()) {
            auto paramTy = PD->getType();
            return !(paramTy->hasTypeVariable() || paramTy->hasError());
          }
          return true;
        }));
      }

      return {true, E};
    }

    Expr *walkToExprPost(Expr *E) override {
      if (auto *closure = dyn_cast<ClosureExpr>(E)) {
        assert(CurrDC == closure && "DeclContext imbalance");
        CurrDC = closure->getParent();
      }
      return E;
    }

  } finder(CS.DC, subExpr);

  expr->walk(finder);
  return finder.DC;
}

bool FailureDiagnosis::diagnoseContextualConversionError(
    Expr *expr, Type contextualType, ContextualTypePurpose CTP,
    Type suggestedType) {
  // If the constraint system has a contextual type, then we can test to see if
  // this is the problem that prevents us from solving the system.
  if (!contextualType)
    return false;

  // Try re-type-checking the expression without the contextual type to see if
  // it can work without it.  If so, the contextual type is the problem.  We
  // force a recheck, because "expr" is likely in our table with the extra
  // contextual constraint that we know we are relaxing.
  TCCOptions options = TCC_ForceRecheck;
  if (contextualType->is<InOutType>())
    options |= TCC_AllowLValue;

  auto *recheckedExpr = typeCheckChildIndependently(expr, options);
  auto exprType = recheckedExpr ? CS.getType(recheckedExpr) : Type();

  // If there is a suggested type and re-typecheck failed, let's use it.
  if (!exprType)
    exprType = suggestedType;

  // If it failed and diagnosed something, then we're done.
  if (!exprType)
    return CS.getASTContext().Diags.hadAnyError();

  // If we don't have a type for the expression, then we cannot use it in
  // conversion constraint diagnostic generation.  If the types match, then it
  // must not be the contextual type that is the problem.
  if (isUnresolvedOrTypeVarType(exprType) || exprType->isEqual(contextualType))
    return false;

  // Don't attempt fixits if we have an unsolved type variable, since
  // the recovery path's recursion into the type checker via typeCheckCast()
  // will confuse matters.
  if (exprType->hasTypeVariable())
    return false;

  ContextualFailure failure(
      CS, CTP, exprType, contextualType,
      CS.getConstraintLocator(expr, LocatorPathElt::ContextualType()));
  return failure.diagnoseAsError();
}

//===----------------------------------------------------------------------===//
// Diagnose assigning variable to itself.
//===----------------------------------------------------------------------===//

static bool isSymmetricBinaryOperator(const CalleeCandidateInfo &CCI) {
  // If we don't have at least one known candidate, don't trigger.
  if (CCI.candidates.empty()) return false;

  for (auto &candidate : CCI.candidates) {
    // Each candidate must be a non-assignment operator function.
    auto decl = dyn_cast_or_null<FuncDecl>(candidate.getDecl());
    if (!decl) return false;
    auto op = dyn_cast_or_null<InfixOperatorDecl>(decl->getOperatorDecl());
    if (!op || !op->getPrecedenceGroup() ||
        op->getPrecedenceGroup()->isAssignment())
      return false;

    // It must have exactly two parameters.
    auto params = decl->getParameters();
    if (params->size() != 2) return false;

    // Require the types to be the same.
    if (!params->get(0)->getInterfaceType()->isEqual(
          params->get(1)->getInterfaceType()))
      return false;
  }

  return true;
}

/// Determine whether any of the given callee candidates have a default value.
static bool candidatesHaveAnyDefaultValues(
    const CalleeCandidateInfo &candidates) {
  for (const auto &cand : candidates.candidates) {
    auto function = dyn_cast_or_null<AbstractFunctionDecl>(cand.getDecl());
    if (!function) continue;

    if (function->hasImplicitSelfDecl()) {
      if (!cand.skipCurriedSelf)
        return false;
    } else {
      if (cand.skipCurriedSelf)
        return false;
    }

    for (auto param : *function->getParameters()) {
      if (param->getDefaultArgumentKind() != DefaultArgumentKind::None)
        return true;
    }
  }

  return false;
}

/// Find the tuple element that can be initialized by a scalar.
static Optional<unsigned> getElementForScalarInitOfArg(
    const TupleType *tupleTy,
    const CalleeCandidateInfo &candidates) {
  // Empty tuples cannot be initialized with a scalar.
  if (tupleTy->getNumElements() == 0) return None;
  
  auto getElementForScalarInitSimple =
      [](const TupleType *tupleTy) -> Optional<unsigned> {
    Optional<unsigned> result = None;
    for (unsigned i = 0, e = tupleTy->getNumElements(); i != e; ++i) {
      // If we already saw a non-vararg field, then we have more than
      // one candidate field.
      if (result.hasValue()) {
        // Vararg fields are okay; they'll just end up being empty.
        if (tupleTy->getElement(i).isVararg())
          continue;

        // Give up.
        return None;
      }

      // Otherwise, remember this field number.
      result = i;
    }

    return result;
  };

  // If there aren't any candidates, we're done.
  if (candidates.empty()) return getElementForScalarInitSimple(tupleTy);

  // Dig out the candidate.
  const auto &cand = candidates[0];
  auto function = dyn_cast_or_null<AbstractFunctionDecl>(cand.getDecl());
  if (!function) return getElementForScalarInitSimple(tupleTy);

  if (function->hasImplicitSelfDecl()) {
    if (!cand.skipCurriedSelf)
      return getElementForScalarInitSimple(tupleTy);
  } else {
    if (cand.skipCurriedSelf)
      return getElementForScalarInitSimple(tupleTy);
  }

  auto paramList = function->getParameters();
  if (tupleTy->getNumElements() != paramList->size()) 
    return getElementForScalarInitSimple(tupleTy);

  // Find a tuple element without a default.
  Optional<unsigned> elementWithoutDefault;
  for (unsigned i : range(tupleTy->getNumElements())) {
    auto param = paramList->get(i);

    // Skip parameters with default arguments.
    if (param->getDefaultArgumentKind() != DefaultArgumentKind::None)
      continue;

    // If we already have an element without a default, check whether there are
    // two fields that need initialization.
    if (elementWithoutDefault) {
      // Variadic fields are okay; they'll just end up being empty.
      if (param->isVariadic()) continue;

      // If the element we saw before was variadic, it can be empty as well.
      auto priorParam = paramList->get(*elementWithoutDefault);
      if (!priorParam->isVariadic()) return None;
    }

    elementWithoutDefault = i;
  }

  if (elementWithoutDefault) return elementWithoutDefault;

  // All of the fields have default values; initialize the first one.
  return 0;
}

/// Return true if the argument of a CallExpr (or related node) has a trailing
/// closure.
static bool callArgHasTrailingClosure(Expr *E) {
  if (!E) return false;
  if (auto *PE = dyn_cast<ParenExpr>(E))
    return PE->hasTrailingClosure();
  else if (auto *TE = dyn_cast<TupleExpr>(E))
    return TE->hasTrailingClosure();
  return false;
}

/// Special magic to handle inout exprs and tuples in argument lists.
Expr *FailureDiagnosis::
typeCheckArgumentChildIndependently(Expr *argExpr, Type argType,
                                    const CalleeCandidateInfo &candidates,
                                    TCCOptions options) {
  // Grab one of the candidates (if present) and get its input list to help
  // identify operators that have implicit inout arguments.
  Type exampleInputType;
  if (!candidates.empty()) {
    exampleInputType = candidates[0].getArgumentType(CS.getASTContext());

    // If we found a single candidate, and have no contextually known argument
    // type information, use that one candidate as the type information for
    // subexpr checking.
    //
    // TODO: If all candidates have the same type for some argument, we could
    // pass down partial information.
    if (candidates.size() == 1 && !argType)
      argType = candidates[0].getArgumentType(CS.getASTContext());
  }
  
  // If our candidates are instance members at curry level #0, then the argument
  // being provided is the receiver type for the instance.  We produce better
  // diagnostics when we don't force the self type down.
  if (argType && !candidates.empty())
    if (auto decl = candidates[0].getDecl())
      if (decl->isInstanceMember() && !candidates[0].skipCurriedSelf &&
          !isa<SubscriptDecl>(decl))
        argType = Type();

  // Similarly, we get better results when we don't push argument types down
  // to symmetric operators.
  if (argType && isSymmetricBinaryOperator(candidates))
    argType = Type();
  

  // FIXME: This should all just be a matter of getting the type of the
  // sub-expression, but this doesn't work well when typeCheckChildIndependently
  // is over-conservative w.r.t. TupleExprs.
  auto *TE = dyn_cast<TupleExpr>(argExpr);
  if (!TE) {
    // If the argument isn't a tuple, it is some scalar value for a
    // single-argument call.
    if (exampleInputType && exampleInputType->is<InOutType>())
      options |= TCC_AllowLValue;

    // If the argtype is a tuple type with default arguments, or a labeled tuple
    // with a single element, pull the scalar element type for the subexpression
    // out.  If we can't do that and the tuple has default arguments, we have to
    // punt on passing down the type information, since type checking the
    // subexpression won't be able to find the default argument provider.
    if (argType) {
      if (auto *PT = dyn_cast<ParenType>(argType.getPointer())) {
        const auto &flags = PT->getParameterFlags();
        if (flags.isAutoClosure()) {
          auto resultTy = PT->castTo<FunctionType>()->getResult();
          argType = ParenType::get(PT->getASTContext(), resultTy);
        }
      } else if (auto argTT = argType->getAs<TupleType>()) {
        if (auto scalarElt = getElementForScalarInitOfArg(argTT, candidates)) {
          // If we found the single argument being initialized, use it.
          auto &arg = argTT->getElement(*scalarElt);
          
          // If the argument being specified is actually varargs, then we're
          // just specifying one element of a variadic list.  Use the type of
          // the individual varargs argument, not the overall array type.
          if (arg.isVararg())
            argType = arg.getVarargBaseTy();
          else if (arg.isAutoClosure())
            argType = arg.getType()->castTo<FunctionType>()->getResult();
          else
            argType = arg.getType();
        } else if (candidatesHaveAnyDefaultValues(candidates)) {
          argType = Type();
        }
      } else if (candidatesHaveAnyDefaultValues(candidates)) {
        argType = Type();
      }
    }

    auto CTPurpose = argType ? CTP_CallArgument : CTP_Unused;
    return typeCheckChildIndependently(argExpr, argType, CTPurpose, options);
  }

  // If we know the requested argType to use, use computeTupleShuffle to produce
  // the shuffle of input arguments to destination values.  It requires a
  // TupleType to compute the mapping from argExpr.  Conveniently, it doesn't
  // care about the actual types though, so we can just use 'void' for them.
  // FIXME: This doesn't need to be limited to tuple types.
  if (argType && argType->is<TupleType>()) {
    // Decompose the parameter type.
    SmallVector<AnyFunctionType::Param, 4> params;
    AnyFunctionType::decomposeInput(argType, params);
    
    // If we have a candidate function around, compute the position of its
    // default arguments.
    ParameterListInfo paramInfo;
    if (!candidates.empty()) {
      paramInfo = candidates[0].getParameterListInfo(params);
    } else {
      paramInfo = ParameterListInfo(params, nullptr, /*skipCurriedSelf=*/false);
    }

    // Form a set of call arguments, using a dummy type (Void), because the
    // argument/parameter matching code doesn't need it.
    auto voidTy = CS.getASTContext().TheEmptyTupleType;
    SmallVector<AnyFunctionType::Param, 4> args;
    for (unsigned i = 0, e = TE->getNumElements(); i != e; ++i) {
      args.push_back(AnyFunctionType::Param(voidTy, TE->getElementName(i), {}));
    }

    /// Use a match call argument listener that allows relabeling.
    struct RelabelMatchCallArgumentListener : MatchCallArgumentListener {
      bool relabelArguments(ArrayRef<Identifier> newNames) override {
        return false;
      }
    } listener;

    SmallVector<ParamBinding, 4> paramBindings;
    if (!matchCallArguments(args, params, paramInfo,
                            callArgHasTrailingClosure(argExpr),
                            /*allowFixes=*/true,
                            listener, paramBindings)) {
      SmallVector<Expr*, 4> resultElts(TE->getNumElements(), nullptr);
      SmallVector<TupleTypeElt, 4> resultEltTys(TE->getNumElements(), voidTy);

      // Perform analysis of the input elements.
      for (unsigned paramIdx : range(paramBindings.size())) {
        // Extract the parameter.
        const auto &param = params[paramIdx];

        // Determine the parameter type.
        if (param.isInOut())
          options |= TCC_AllowLValue;

        // Look at each of the arguments assigned to this parameter.
        auto currentParamType = param.getOldType();

        // Since this is diagnostics, let's make sure that parameter
        // marked as @autoclosure indeed has a function type, because
        // it can also be an error type and possibly unresolved type.
        if (param.isAutoClosure()) {
          if (auto *funcType = currentParamType->getAs<FunctionType>())
            currentParamType = funcType->getResult();
        }

        for (auto inArgNo : paramBindings[paramIdx]) {
          // Determine the argument type.
          auto currentArgType = TE->getElement(inArgNo);

          auto exprResult =
            typeCheckChildIndependently(currentArgType, currentParamType,
                                        CTP_CallArgument, options);

          // If there was an error type checking this argument, then we're done.
          if (!exprResult)
            return nullptr;

          auto resultTy = CS.getType(exprResult);
          resultElts[inArgNo] = exprResult;
          resultEltTys[inArgNo] = {resultTy->getInOutObjectType(),
                                   TE->getElementName(inArgNo),
                                   ParameterTypeFlags().withInOut(resultTy->is<InOutType>())};
        }
      }

      auto TT = TupleType::get(resultEltTys, CS.getASTContext());
      return CS.cacheType(TupleExpr::create(
          CS.getASTContext(), TE->getLParenLoc(), resultElts,
          TE->getElementNames(), TE->getElementNameLocs(), TE->getRParenLoc(),
          TE->hasTrailingClosure(), TE->isImplicit(), TT));
    }
  }
  
  // Get the simplified type of each element and rebuild the aggregate.
  SmallVector<TupleTypeElt, 4> resultEltTys;
  SmallVector<Expr*, 4> resultElts;

  TupleType *exampleInputTuple = nullptr;
  if (exampleInputType)
    exampleInputTuple = exampleInputType->getAs<TupleType>();

  for (unsigned i = 0, e = TE->getNumElements(); i != e; i++) {
    if (exampleInputTuple && i < exampleInputTuple->getNumElements() &&
        exampleInputTuple->getElement(i).isInOut())
      options |= TCC_AllowLValue;

    auto elExpr = typeCheckChildIndependently(TE->getElement(i), options);
    if (!elExpr) return nullptr; // already diagnosed.
    
    resultElts.push_back(elExpr);
    auto resFlags =
        ParameterTypeFlags().withInOut(elExpr->isSemanticallyInOutExpr());
    resultEltTys.push_back({CS.getType(elExpr)->getInOutObjectType(),
                            TE->getElementName(i), resFlags});
  }

  auto TT = TupleType::get(resultEltTys, CS.getASTContext());
  return CS.cacheType(TupleExpr::create(
      CS.getASTContext(), TE->getLParenLoc(), resultElts, TE->getElementNames(),
      TE->getElementNameLocs(), TE->getRParenLoc(), TE->hasTrailingClosure(),
      TE->isImplicit(), TT));
}

static DeclName getBaseName(DeclContext *context) {
  if (auto generic = context->getSelfNominalTypeDecl()) {
    return generic->getName();
  } else if (context->isModuleScopeContext())
    return context->getParentModule()->getName();
  else
    llvm_unreachable("Unsupported base");
};

static void emitFixItForExplicitlyQualifiedReference(
    DiagnosticEngine &de, UnresolvedDotExpr *UDE,
    decltype(diag::fix_unqualified_access_top_level) diag, DeclName baseName,
    DescriptiveDeclKind kind) {
  auto name = baseName.getBaseIdentifier();
  SmallString<32> namePlusDot = name.str();
  namePlusDot.push_back('.');

  de.diagnose(UDE->getLoc(), diag, namePlusDot, kind, name)
      .fixItInsert(UDE->getStartLoc(), namePlusDot);
}

void ConstraintSystem::diagnoseDeprecatedConditionalConformanceOuterAccess(
    UnresolvedDotExpr *UDE, ValueDecl *choice) {
  auto result =
      TypeChecker::lookupUnqualified(DC, UDE->getName(), UDE->getLoc());
  assert(result && "names can't just disappear");
  // These should all come from the same place.
  auto exampleInner = result.front();
  auto innerChoice = exampleInner.getValueDecl();
  auto innerDC = exampleInner.getDeclContext()->getInnermostTypeContext();
  auto innerParentDecl = innerDC->getSelfNominalTypeDecl();
  auto innerBaseName = getBaseName(innerDC);

  auto choiceKind = choice->getDescriptiveKind();
  auto choiceDC = choice->getDeclContext();
  auto choiceBaseName = getBaseName(choiceDC);
  auto choiceParentDecl = choiceDC->getAsDecl();
  auto choiceParentKind = choiceParentDecl
                              ? choiceParentDecl->getDescriptiveKind()
                              : DescriptiveDeclKind::Module;

  auto &DE = getASTContext().Diags;
  DE.diagnose(UDE->getLoc(),
              diag::warn_deprecated_conditional_conformance_outer_access,
              UDE->getName(), choiceKind, choiceParentKind, choiceBaseName,
              innerChoice->getDescriptiveKind(),
              innerParentDecl->getDescriptiveKind(), innerBaseName);

  emitFixItForExplicitlyQualifiedReference(
      getASTContext().Diags, UDE,
      diag::fix_deprecated_conditional_conformance_outer_access,
      choiceBaseName, choiceKind);
}

static SmallVector<AnyFunctionType::Param, 4>
decomposeArgType(Type argType, ArrayRef<Identifier> argLabels) {
  SmallVector<AnyFunctionType::Param, 4> result;
  AnyFunctionType::decomposeInput(argType, result);
  AnyFunctionType::relabelParams(result, argLabels);
  return result;
}

// Extract expression for failed argument number
static Expr *getFailedArgumentExpr(CalleeCandidateInfo CCI, Expr *argExpr) {
  if (auto *TE = dyn_cast<TupleExpr>(argExpr))
    return TE->getElement(CCI.failedArgument.argumentNumber);
  else if (auto *PE = dyn_cast<ParenExpr>(argExpr)) {
    assert(CCI.failedArgument.argumentNumber == 0 &&
           "Unexpected argument #");
    return PE->getSubExpr();
  } else {
    assert(CCI.failedArgument.argumentNumber == 0 &&
           "Unexpected argument #");
    return argExpr;
  }
}

/// If the candidate set has been narrowed down to a specific structural
/// problem, e.g. that there are too few parameters specified or that argument
/// labels don't match up, diagnose that error and return true.
bool FailureDiagnosis::diagnoseParameterErrors(CalleeCandidateInfo &CCI,
                                               Expr *fnExpr, Expr *argExpr,
                                               ArrayRef<Identifier> argLabels) {
  // If we have a failure where the candidate set differs on exactly one
  // argument, and where we have a consistent mismatch across the candidate set
  // (often because there is only one candidate in the set), then diagnose this
  // as a specific problem of passing something of the wrong type into a
  // parameter.
  //
  // We don't generally want to use this path to diagnose calls to
  // symmetrically-typed binary operators because it's likely that both
  // operands contributed to the type.
  if ((CCI.closeness == CC_OneArgumentMismatch ||
       CCI.closeness == CC_OneArgumentNearMismatch ||
       CCI.closeness == CC_OneGenericArgumentMismatch ||
       CCI.closeness == CC_OneGenericArgumentNearMismatch ||
       CCI.closeness == CC_GenericNonsubstitutableMismatch) &&
      CCI.failedArgument.isValid() &&
      !isSymmetricBinaryOperator(CCI)) {
    // Map the argument number into an argument expression.
    TCCOptions options = TCC_ForceRecheck;
    if (CCI.failedArgument.parameterType->is<InOutType>())
      options |= TCC_AllowLValue;

    // It could be that the argument doesn't conform to an archetype.
    Expr *badArgExpr = getFailedArgumentExpr(CCI, argExpr);

    // Re-type-check the argument with the expected type of the candidate set.
    // This should produce a specific and tailored diagnostic saying that the
    // type mismatches with expectations.
    Type paramType = CCI.failedArgument.parameterType;
    if (!typeCheckChildIndependently(badArgExpr, paramType,
                                     CTP_CallArgument, options))
      return true;
  }
  
  return false;
}

bool FailureDiagnosis::visitApplyExpr(ApplyExpr *callExpr) {
  auto *fnExpr = callExpr->getFn();
  auto fnType = CS.getType(fnExpr)->getRValueType();

  bool hasTrailingClosure = callArgHasTrailingClosure(callExpr->getArg());
  
  // Collect a full candidate list of callees based on the partially type
  // checked function.
  CalleeCandidateInfo calleeInfo(fnExpr, hasTrailingClosure, CS);

  // Filter list of the candidates based on the known function type.
  if (auto fn = fnType->getAs<AnyFunctionType>()) {
    using Closeness = CalleeCandidateInfo::ClosenessResultTy;

    calleeInfo.filterList([&](OverloadCandidate candidate) -> Closeness {
      auto resultType = candidate.getResultType();
      if (!resultType)
        return {CC_GeneralMismatch, {}};

      // FIXME: Handle matching of the generic types properly.
      // Currently we don't filter result types containing generic parameters
      // because there is no easy way to do that, and candidate set is going
      // to be pruned by matching of the argument types later on anyway, so
      // it's better to over report than to be too conservative.
      if (resultType->isEqual(fn->getResult()))
        return {CC_ExactMatch, {}};

      return {CC_GeneralMismatch, {}};
    });
  }

  // Filter the candidate list based on the argument we may or may not have.
  calleeInfo.filterContextualMemberList(callExpr->getArg());

  SmallVector<Identifier, 2> argLabelsScratch;
  ArrayRef<Identifier> argLabels =
    callExpr->getArgumentLabels(argLabelsScratch);
  if (diagnoseParameterErrors(calleeInfo, callExpr->getFn(),
                              callExpr->getArg(), argLabels))
    return true;

  Type argType;  // argument list, if known.
  if (auto FTy = fnType->getAs<AnyFunctionType>()) {
    argType = FunctionType::composeInput(CS.getASTContext(), FTy->getParams(),
                                         false);
  } else if (auto MTT = fnType->getAs<AnyMetatypeType>()) {
    // If we are constructing a tuple with initializer syntax, the expected
    // argument list is the tuple type itself - and there is no initdecl.
    auto instanceTy = MTT->getInstanceType();
    if (auto tupleTy = instanceTy->getAs<TupleType>()) {
      argType = tupleTy;
    }
  }

  // Get the expression result of type checking the arguments to the call
  // independently, so we have some idea of what we're working with.
  //
  auto argExpr = typeCheckArgumentChildIndependently(callExpr->getArg(),
                                                     argType, calleeInfo,
                                             TCC_AllowUnresolvedTypeVariables);
  if (!argExpr)
    return true; // already diagnosed.

  calleeInfo.filterListArgs(decomposeArgType(CS.getType(argExpr), argLabels));

  if (diagnoseParameterErrors(calleeInfo, callExpr->getFn(), argExpr,
                              argLabels))
    return true;

  // Force recheck of the arg expression because we allowed unresolved types
  // before, and that turned out not to help, and now we want any diagnoses
  // from disallowing them.
  argExpr = typeCheckArgumentChildIndependently(callExpr->getArg(), argType,
                                                calleeInfo, TCC_ForceRecheck);
  if (!argExpr)
    return true; // already diagnosed.

  auto overloadName = calleeInfo.declName;

  // Local function to check if the error with argument type is
  // related to contextual type information of the enclosing expression
  // rather than resolution of argument expression itself.
  auto isContextualConversionFailure = [&](Expr *argExpr) -> bool {
    // If we found an exact match, this must be a problem with a conversion from
    // the result of the call to the expected type. Diagnose this as a
    // conversion failure.
    if (calleeInfo.closeness == CC_ExactMatch)
      return true;

    if (!CS.getContextualType(callExpr) ||
        (calleeInfo.closeness != CC_ArgumentMismatch &&
         calleeInfo.closeness != CC_OneGenericArgumentMismatch))
      return false;

    CalleeCandidateInfo candidates(fnExpr, hasTrailingClosure, CS);

    // Filter original list of choices based on the deduced type of
    // argument expression after force re-check.
    candidates.filterContextualMemberList(argExpr);

    // One of the candidates matches exactly, which means that
    // this is a contextual type conversion failure, we can't diagnose here.
    return candidates.closeness == CC_ExactMatch;
  };

  // Otherwise, we have a generic failure.  Diagnose it with a generic error
  // message now.
  if (isa<BinaryExpr>(callExpr) && isa<TupleExpr>(argExpr)) {
    auto argTuple = cast<TupleExpr>(argExpr);
    auto lhsExpr = argTuple->getElement(0), rhsExpr = argTuple->getElement(1);
    auto lhsType = CS.getType(lhsExpr)->getRValueType();
    auto rhsType = CS.getType(rhsExpr)->getRValueType();

    if (isContextualConversionFailure(argTuple))
      return false;

    if (!lhsType->isEqual(rhsType)) {
      auto diag = diagnose(callExpr->getLoc(), diag::cannot_apply_binop_to_args,
                           overloadName, lhsType, rhsType);
      diag.highlight(lhsExpr->getSourceRange())
      .highlight(rhsExpr->getSourceRange());
    } else {
      diagnose(callExpr->getLoc(), diag::cannot_apply_binop_to_same_args,
               overloadName, lhsType)
      .highlight(lhsExpr->getSourceRange())
      .highlight(rhsExpr->getSourceRange());
    }

    calleeInfo.suggestPotentialOverloads(callExpr->getLoc());
    return true;
  }

  // If we have a failure where closeness is an exact match, but there is
  // still a failed argument, it is because one (or more) of the arguments
  // types are unresolved.
  if (calleeInfo.closeness == CC_ExactMatch && calleeInfo.failedArgument.isValid()) {
    diagnoseAmbiguity(getFailedArgumentExpr(calleeInfo, argExpr));
    return true;
  }

  if (isContextualConversionFailure(argExpr))
    return false;

  // Generate specific error messages for unary operators.
  if (isa<PrefixUnaryExpr>(callExpr) || isa<PostfixUnaryExpr>(callExpr)) {
    assert(!overloadName.empty());
    diagnose(argExpr->getLoc(), diag::cannot_apply_unop_to_arg, overloadName,
             CS.getType(argExpr));

    calleeInfo.suggestPotentialOverloads(argExpr->getLoc());
    return true;
  }

  if (CS.getType(argExpr)->hasUnresolvedType())
    return false;

  bool isInitializer = isa<TypeExpr>(fnExpr);
  if (isa<TupleExpr>(argExpr) &&
      cast<TupleExpr>(argExpr)->getNumElements() == 0) {
    // Emit diagnostics that say "no arguments".
    diagnose(fnExpr->getLoc(), diag::cannot_call_with_no_params,
             overloadName, isInitializer);
  } else {
    SmallVector<AnyFunctionType::Param, 8> params;
    AnyFunctionType::decomposeInput(CS.getType(argExpr), params);
    auto argString = AnyFunctionType::getParamListAsString(params);

    diagnose(fnExpr->getLoc(), diag::cannot_call_with_params,
             overloadName, argString, isInitializer);
  }

  // Did the user intend on invoking a different overload?
  calleeInfo.suggestPotentialOverloads(fnExpr->getLoc());
  return true;
}

bool FailureDiagnosis::
visitRebindSelfInConstructorExpr(RebindSelfInConstructorExpr *E) {
  // Don't walk the children for this node, it leads to multiple diagnostics
  // because of how sema injects this node into the type checker.
  return false;
}

bool FailureDiagnosis::visitExpr(Expr *E) {
  // Check each of our immediate children to see if any of them are
  // independently invalid.
  bool errorInSubExpr = false;
  
  E->forEachImmediateChildExpr([&](Expr *Child) -> Expr* {
    // If we already found an error, stop checking.
    if (errorInSubExpr) return Child;

    // Otherwise just type check the subexpression independently.  If that
    // succeeds, then we stitch the result back into our expression.
    if (typeCheckChildIndependently(Child, TCC_AllowLValue))
      return Child;

    // Otherwise, it failed, which emitted a diagnostic.  Keep track of this
    // so that we don't emit multiple diagnostics.
    errorInSubExpr = true;
    return Child;
  });
  
  // If any of the children were errors, we're done.
  if (errorInSubExpr)
    return true;
  
  // Otherwise, produce a more generic error.
  return false;
}


bool FailureDiagnosis::diagnoseExprFailure() {
  assert(expr);

  // Our general approach is to do a depth first traversal of the broken
  // expression tree, type checking as we go.  If we find a subtree that cannot
  // be type checked on its own (even to an incomplete type) then that is where
  // we focus our attention.  If we do find a type, we use it to check for
  // contextual type mismatches.
  return visit(expr);
}


/// Given a specific expression and the remnants of the failed constraint
/// system, produce a specific diagnostic.
///
/// This is guaranteed to always emit an error message.
///
void ConstraintSystem::diagnoseFailureFor(SolutionApplicationTarget target) {
  setPhase(ConstraintSystemPhase::Diagnostics);

  SWIFT_DEFER { setPhase(ConstraintSystemPhase::Finalization); };

  if (auto expr = target.getAsExpr()) {
    // Look through RebindSelfInConstructorExpr to avoid weird Sema issues.
    if (auto *RB = dyn_cast<RebindSelfInConstructorExpr>(expr))
      expr = RB->getSubExpr();

    FailureDiagnosis diagnosis(expr, *this);

    // Now, attempt to diagnose the failure from the info we've collected.
    if (diagnosis.diagnoseExprFailure())
      return;

    // If this is a contextual conversion problem, dig out some information.
    if (diagnosis.diagnoseContextualConversionError(
            expr,
            getContextualType(expr),
            getContextualTypePurpose(expr)))
      return;

    // If no one could find a problem with this expression or constraint system,
    // then it must be well-formed... but is ambiguous.  Handle this by diagnostic
    // various cases that come up.
    diagnosis.diagnoseAmbiguity(expr);
  } else {
    // Emit a poor fallback message.
    getASTContext().Diags.diagnose(
         target.getAsFunction()->getLoc(), diag::failed_to_produce_diagnostic);
  }
}

std::pair<Type, ContextualTypePurpose>
FailureDiagnosis::validateContextualType(Type contextualType,
                                         ContextualTypePurpose CTP) {
  if (!contextualType)
    return {contextualType, CTP};

  // Since some of the contextual types might be tuples e.g. subscript argument
  // is a tuple or paren wrapping a tuple, it's required to recursively check
  // its elements to determine nullability of the contextual type, because it
  // might contain archetypes.
  std::function<bool(Type)> shouldNullifyType = [&](Type type) -> bool {
    switch (type->getDesugaredType()->getKind()) {
    case TypeKind::PrimaryArchetype:
    case TypeKind::OpenedArchetype:
    case TypeKind::NestedArchetype:
    case TypeKind::Unresolved:
      return true;

    case TypeKind::BoundGenericEnum:
    case TypeKind::BoundGenericClass:
    case TypeKind::BoundGenericStruct:
    case TypeKind::UnboundGeneric:
    case TypeKind::GenericFunction:
    case TypeKind::Metatype:
      return type->hasUnresolvedType();

    case TypeKind::Tuple: {
      auto tupleType = type->getAs<TupleType>();
      for (auto &element : tupleType->getElements()) {
        if (shouldNullifyType(element.getType()))
            return true;
      }
      break;
    }

    default:
      return false;
    }

    return false;
  };

  bool shouldNullify = false;
  if (auto objectType = contextualType->getWithoutSpecifierType()) {
    // Note that simply checking for `objectType->hasUnresolvedType()` is not
    // appropriate in this case standalone, because if it's in a function,
    // for example, or inout type, we still want to preserve it's skeleton
    /// because that helps to diagnose inout argument issues. Complete
    // nullification is only appropriate for generic types with unresolved
    // types or standalone archetypes because that's going to give
    // sub-expression solver a chance to try and compute type as it sees fit
    // and higher level code would have a chance to check it, which avoids
    // diagnostic messages like `cannot convert (_) -> _ to (Int) -> Void`.
    shouldNullify = shouldNullifyType(objectType);
  }

  // If the conversion type contains no info, drop it.
  if (shouldNullify)
    return {Type(), CTP_Unused};

  // Remove all of the potentially leftover type variables or type parameters
  // from the contextual type to be used by new solver.
  contextualType = replaceTypeParametersWithUnresolved(contextualType);
  contextualType = replaceTypeVariablesWithUnresolved(contextualType);

  return {contextualType, CTP};
}

/// Emit an ambiguity diagnostic about the specified expression.
void FailureDiagnosis::diagnoseAmbiguity(Expr *E) {
  if (auto *assignment = dyn_cast<AssignExpr>(E)) {
    if (isa<DiscardAssignmentExpr>(assignment->getDest())) {
      auto *srcExpr = assignment->getSrc();
      diagnoseAmbiguity(srcExpr);
      return;
    }
  }

  // Unresolved/Anonymous ClosureExprs are common enough that we should give
  // them tailored diagnostics.
  if (auto CE = dyn_cast<ClosureExpr>(E->getValueProvidingExpr())) {
    diagnose(E->getLoc(), diag::cannot_infer_closure_type)
      .highlight(E->getSourceRange());
    return;
  }

  // Diagnose ".foo" expressions that lack context specifically.
  if (auto UME =
        dyn_cast<UnresolvedMemberExpr>(E->getSemanticsProvidingExpr())) {
    if (!CS.getContextualType(E)) {
      diagnose(E->getLoc(), diag::unresolved_member_no_inference,UME->getName())
        .highlight(SourceRange(UME->getDotLoc(),
                               UME->getNameLoc().getSourceRange().End));
      return;
    }
  }

  // Attempt to re-type-check the entire expression, allowing ambiguity, but
  // ignoring a contextual type.
  if (expr == E) {
    auto exprType = getTypeOfTypeCheckedChildIndependently(expr);
    // If it failed and diagnosed something, then we're done.
    if (!exprType) return;

    // If we were able to find something more specific than "unknown" (perhaps
    // something like "[_:_]" for a dictionary literal), include it in the
    // diagnostic.
    if (!isUnresolvedOrTypeVarType(exprType)) {
      diagnose(E->getLoc(), diag::specific_type_of_expression_is_ambiguous,
               exprType)
        .highlight(E->getSourceRange());
      return;
    }
  }

  // Before giving up completely let's try to see if there are any
  // fixes recorded by constraint generator, which point to structural
  // problems that might not result in solution even if fixed e.g.
  // missing members involved in protocol composition in expression
  // context which are interpreted as binary operator expressions instead.
  {
    bool diagnosed = false;
    for (auto *fix : CS.getFixes())
      diagnosed |= fix->diagnose();

    if (diagnosed)
      return;
  }

  // If there are no posted constraints or failures, then there was
  // not enough contextual information available to infer a type for the
  // expression.
  diagnose(E->getLoc(), diag::type_of_expression_is_ambiguous)
    .highlight(E->getSourceRange());
}
