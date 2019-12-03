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

  /// For an expression being type checked with a CTP_CalleeResult contextual
  /// type, try to diagnose a problem.
  bool diagnoseCalleeResultContextualConversionError();

  /// Attempt to produce a diagnostic for a mismatch between a call's
  /// type and its assumed contextual type.
  bool diagnoseCallContextualConversionErrors(ApplyExpr *callEpxr,
                                              Type contextualType,
                                              ContextualTypePurpose CTP);

  bool diagnoseImplicitSelfErrors(Expr *fnExpr, Expr *argExpr,
                                  CalleeCandidateInfo &CCI,
                                  ArrayRef<Identifier> argLabels);

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

  /// Check the specified closure to see if it is a multi-statement closure with
  /// an uninferred type.  If so, diagnose the problem with an error and return
  /// true.
  bool diagnoseAmbiguousMultiStatementClosure(ClosureExpr *closure);

  /// Given a result of name lookup that had no viable results, diagnose the
  /// unviable ones.
  void diagnoseUnviableLookupResults(MemberLookupResult &lookupResults,
                                     Expr *expr, Type baseObjTy, Expr *baseExpr,
                                     DeclName memberName, DeclNameLoc nameLoc,
                                     SourceLoc loc);

  bool diagnoseMemberFailures(
      Expr *E, Expr *baseEpxr, ConstraintKind lookupKind, DeclName memberName,
      FunctionRefKind funcRefKind, ConstraintLocator *locator,
      Optional<std::function<bool(ArrayRef<OverloadChoice>)>> callback = None,
      bool includeInaccessibleMembers = true);

  bool diagnoseTrailingClosureErrors(ApplyExpr *expr);

  bool
  diagnoseClosureExpr(ClosureExpr *closureExpr, Type contextualType,
                      llvm::function_ref<bool(Type, Type)> resultTypeProcessor);

  bool diagnoseSubscriptErrors(SubscriptExpr *SE, bool performingSet);

  bool visitExpr(Expr *E);
  bool visitIdentityExpr(IdentityExpr *E);
  bool visitTryExpr(TryExpr *E);
  bool visitTupleExpr(TupleExpr *E);
  
  bool visitUnresolvedMemberExpr(UnresolvedMemberExpr *E);
  bool visitUnresolvedDotExpr(UnresolvedDotExpr *UDE);
  bool visitArrayExpr(ArrayExpr *E);
  bool visitDictionaryExpr(DictionaryExpr *E);
  bool visitObjectLiteralExpr(ObjectLiteralExpr *E);

  bool visitSubscriptExpr(SubscriptExpr *SE);
  bool visitApplyExpr(ApplyExpr *AE);
  bool visitCoerceExpr(CoerceExpr *CE);
  bool visitIfExpr(IfExpr *IE);
  bool visitRebindSelfInConstructorExpr(RebindSelfInConstructorExpr *E);
  bool visitCaptureListExpr(CaptureListExpr *CLE);
  bool visitClosureExpr(ClosureExpr *CE);
};
} // end anonymous namespace

/// Given a result of name lookup that had no viable results, diagnose the
/// unviable ones.
void FailureDiagnosis::diagnoseUnviableLookupResults(
    MemberLookupResult &result, Expr *E, Type baseObjTy, Expr *baseExpr,
    DeclName memberName, DeclNameLoc nameLoc, SourceLoc loc) {
  SourceRange baseRange = baseExpr ? baseExpr->getSourceRange() : SourceRange();

  // If we found no results at all, mention that fact.
  if (result.UnviableCandidates.empty()) {
    MissingMemberFailure failure(CS, baseObjTy, memberName,
                                 CS.getConstraintLocator(E));
    auto diagnosed = failure.diagnoseAsError();
    assert(diagnosed && "Failed to produce missing member diagnostic");
    (void)diagnosed;
    return;
  }

  // Otherwise, we have at least one (and potentially many) viable candidates
  // sort them out.  If all of the candidates have the same problem (commonly
  // because there is exactly one candidate!) diagnose this.
  auto firstProblem = result.UnviableReasons[0];
  bool sameProblem = llvm::all_of(
      result.UnviableReasons,
      [&firstProblem](const MemberLookupResult::UnviableReason &problem) {
        return problem == firstProblem;
      });

  auto instanceTy = baseObjTy;
  if (auto *MTT = instanceTy->getAs<AnyMetatypeType>())
    instanceTy = MTT->getInstanceType();
  
  if (sameProblem) {
    // If the problem is the same for all of the choices, let's
    // just pick one which has a declaration.
    auto choice = llvm::find_if(
        result.UnviableCandidates,
        [&](const OverloadChoice &choice) { return choice.isDecl(); });

    // This code can't currently diagnose key path application
    // related failures.
    if (!choice)
      return;

    switch (firstProblem) {
    case MemberLookupResult::UR_WritableKeyPathOnReadOnlyMember:
    case MemberLookupResult::UR_ReferenceWritableKeyPathOnMutatingMember:
    case MemberLookupResult::UR_KeyPathWithAnyObjectRootType:
      break;

    case MemberLookupResult::UR_UnavailableInExistential: {
      InvalidMemberRefOnExistential failure(
          CS, instanceTy, memberName, CS.getConstraintLocator(E));
      failure.diagnoseAsError();
      return;
    }

    case MemberLookupResult::UR_InstanceMemberOnType:
    case MemberLookupResult::UR_TypeMemberOnInstance: {
      auto locatorKind = isa<SubscriptExpr>(E)
                             ? ConstraintLocator::SubscriptMember
                             : ConstraintLocator::Member;
      AllowTypeOrInstanceMemberFailure failure(
          CS, baseObjTy, choice->getDecl(), memberName,
          CS.getConstraintLocator(E, locatorKind));
      auto diagnosed = failure.diagnoseAsError();
      assert(diagnosed &&
             "Failed to produce missing or extraneous metatype diagnostic");
      (void)diagnosed;
      return;
    }
    case MemberLookupResult::UR_MutatingMemberOnRValue:
    case MemberLookupResult::UR_MutatingGetterOnRValue: {
      MutatingMemberRefOnImmutableBase failure(CS, choice->getDecl(),
                                               CS.getConstraintLocator(E));
      (void)failure.diagnose();
      return;
    }
        
    case MemberLookupResult::UR_Inaccessible: {
      // FIXME: What if the unviable candidates have different levels of access?
      //
      // If we found an inaccessible member of a protocol extension, it might
      // be declared 'public'. This can only happen if the protocol is not
      // visible to us, but the conforming type is. In this case, we need to
      // clamp the formal access for diagnostics purposes to the formal access
      // of the protocol itself.
      InaccessibleMemberFailure failure(CS, choice->getDecl(),
                                        CS.getConstraintLocator(E));
      auto diagnosed = failure.diagnoseAsError();
      assert(diagnosed && "failed to produce expected diagnostic");
      for (auto cand : result.UnviableCandidates) {
        if (!cand.isDecl())
          continue;

        auto *candidate = cand.getDecl();
        // failure is going to highlight candidate given to it,
        // we just need to handle the rest here.
        if (candidate != choice->getDecl())
          diagnose(candidate, diag::decl_declared_here,
                   candidate->getFullName());
      }
      return;
    }
    }
  }

  // Otherwise, we don't have a specific issue to diagnose.  Just say the vague
  // 'cannot use' diagnostic.
  if (!baseObjTy->isEqual(instanceTy))
    diagnose(loc, diag::could_not_use_type_member,
             instanceTy, memberName)
    .highlight(baseRange).highlight(nameLoc.getSourceRange());
  else
    diagnose(loc, diag::could_not_use_value_member,
             baseObjTy, memberName)
    .highlight(baseRange).highlight(nameLoc.getSourceRange());
  return;
}

namespace {
  class ExprTypeSaverAndEraser {
    llvm::DenseMap<Expr*, Type> ExprTypes;
    llvm::DenseMap<TypeLoc*, Type> TypeLocTypes;
    llvm::DenseMap<Pattern*, Type> PatternTypes;
    llvm::DenseMap<ParamDecl*, Type> ParamDeclInterfaceTypes;
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

          // If a ClosureExpr's parameter list has types on the decls, then
          // remove them so that they'll get regenerated from the
          // associated TypeLocs or resynthesized as fresh typevars.
          if (auto *CE = dyn_cast<ClosureExpr>(expr))
            for (auto P : *CE->getParameters()) {
              if (P->hasInterfaceType()) {
                TS->ParamDeclInterfaceTypes[P] = P->getInterfaceType();
                P->setInterfaceType(Type());
              }
            }
          
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

      for (auto paramDeclIfaceElt : ParamDeclInterfaceTypes) {
        assert(!paramDeclIfaceElt.first->isImmutable() ||
               !paramDeclIfaceElt.second->is<InOutType>());
        paramDeclIfaceElt.first->setInterfaceType(paramDeclIfaceElt.second->getInOutObjectType());
      }
      
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

      for (auto paramDeclIfaceElt : ParamDeclInterfaceTypes)
        if (!paramDeclIfaceElt.first->hasInterfaceType()) {
          paramDeclIfaceElt.first->setInterfaceType(
              getParamBaseType(paramDeclIfaceElt));
        }
    }

  private:
    static Type getParamBaseType(std::pair<ParamDecl *, Type> &storedParam) {
      ParamDecl *param;
      Type storedType;

      std::tie(param, storedType) = storedParam;

      // FIXME: We are currently in process of removing `InOutType`
      //        so `VarDecl::get{Interface}Type` is going to wrap base
      //        type into `InOutType` if its flag indicates that it's
      //        an `inout` parameter declaration. But such type can't
      //        be restored directly using `VarDecl::set{Interface}Type`
      //        caller needs additional logic to extract base type.
      if (auto *IOT = storedType->getAs<InOutType>()) {
        assert(param->isInOut());
        return IOT->getObjectType();
      }

      return storedType;
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
  
  // Disable structural checks, because we know that the overall expression
  // has type constraint problems, and we don't want to know about any
  // syntactic issues in a well-typed subexpression (which might be because
  // the context is missing).
  TypeCheckExprOptions TCEOptions = TypeCheckExprFlags::DisableStructuralChecks;

  // Make sure that typechecker knows that this is an attempt
  // to diagnose a problem.
  TCEOptions |= TypeCheckExprFlags::SubExpressionDiagnostics;

  // Don't walk into non-single expression closure bodies, because
  // ExprTypeSaver and TypeNullifier skip them too.
  TCEOptions |= TypeCheckExprFlags::SkipMultiStmtClosures;

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

bool FailureDiagnosis::diagnoseImplicitSelfErrors(
    Expr *fnExpr, Expr *argExpr, CalleeCandidateInfo &CCI,
    ArrayRef<Identifier> argLabels) {
  // If candidate list is empty it means that problem is somewhere else,
  // since we need to have candidates which might be shadowing other funcs.
  if (CCI.empty() || !CCI[0].getDecl())
    return false;

  auto &ctx = CS.getASTContext();
  // Call expression is formed as 'foo.bar' where 'foo' might be an
  // implicit "Self" reference, such use wouldn't provide good diagnostics
  // for situations where instance members have equal names to functions in
  // Swift Standard Library e.g. min/max.
  auto UDE = dyn_cast<UnresolvedDotExpr>(fnExpr);
  if (!UDE)
    return false;

  auto baseExpr = dyn_cast<DeclRefExpr>(UDE->getBase());
  if (!baseExpr)
    return false;

  auto baseDecl = baseExpr->getDecl();
  if (!baseExpr->isImplicit() || baseDecl->getFullName() != ctx.Id_self)
    return false;

  // Our base expression is an implicit 'self.' reference e.g.
  //
  // extension Sequence {
  //   func test() -> Int {
  //     return max(1, 2)
  //   }
  // }
  //
  // In this example the Sequence class already has two methods named 'max'
  // none of which accept two arguments, but there is a function in
  // Swift Standard Library called 'max' which does accept two arguments,
  // so user might have called that by mistake without realizing that
  // compiler would add implicit 'self.' prefix to the call of 'max'.
  auto argType = CS.getType(argExpr);
  // If argument wasn't properly type-checked, let's retry without changing AST.
  if (!argType || argType->hasUnresolvedType() || argType->hasTypeVariable() ||
      argType->hasTypeParameter()) {
    auto *argTuple = dyn_cast<TupleExpr>(argExpr);
    if (!argTuple) {
      // Bail out if we don't have a well-formed argument list.
      return false;
    }
    
    // Let's type check individual argument expressions without any
    // contextual information to try to recover an argument type that
    // matches what the user actually wrote instead of what the typechecker
    // expects.
    SmallVector<TupleTypeElt, 4> elts;
    for (unsigned i = 0, e = argTuple->getNumElements(); i < e; ++i) {
      ConcreteDeclRef ref = nullptr;
      auto *el = argTuple->getElement(i);
      auto typeResult =
          TypeChecker::getTypeOfExpressionWithoutApplying(el, CS.DC, ref);
      if (!typeResult)
        return false;
      auto flags = ParameterTypeFlags().withInOut(typeResult->is<InOutType>());
      elts.push_back(TupleTypeElt(typeResult->getInOutObjectType(),
                                  argTuple->getElementName(i),
                                  flags));
    }

    argType = TupleType::get(elts, CS.getASTContext());
  }

  auto typeKind = argType->getKind();
  if (typeKind != TypeKind::Tuple && typeKind != TypeKind::Paren)
    return false;

  // If argument type couldn't be properly resolved or has errors,
  // we can't diagnose anything in here, it points to the different problem.
  if (isUnresolvedOrTypeVarType(argType) || argType->hasError())
    return false;

  auto context = CS.DC;
  using CandidateMap =
      llvm::SmallDenseMap<ValueDecl *, llvm::SmallVector<OverloadChoice, 2>>;

  auto getBaseKind = [](ValueDecl *base) -> DescriptiveDeclKind {
    DescriptiveDeclKind kind = DescriptiveDeclKind::Module;
    if (!base)
      return kind;

    auto context = base->getDeclContext();
    do {
      if (isa<ExtensionDecl>(context))
        return DescriptiveDeclKind::Extension;

      if (auto nominal = dyn_cast<NominalTypeDecl>(context)) {
        kind = nominal->getDescriptiveKind();
        break;
      }

      context = context->getParent();
    } while (context);

    return kind;
  };

  auto diagnoseShadowing = [&](ValueDecl *base,
                               ArrayRef<OverloadChoice> candidates) -> bool {
    CalleeCandidateInfo calleeInfo(base ? base->getInterfaceType() : nullptr,
                                   candidates, CCI.hasTrailingClosure, CS,
                                   base);

    calleeInfo.filterListArgs(decomposeArgType(argType, argLabels));

    auto diagnostic = diag::member_shadows_global_function_near_match;
    switch (calleeInfo.closeness) {
    case CC_Unavailable:
    case CC_Inaccessible:
    case CC_SelfMismatch:
    case CC_ArgumentLabelMismatch:
    case CC_ArgumentCountMismatch:
    case CC_GeneralMismatch:
      return false;

    case CC_NonLValueInOut:
    case CC_OneArgumentNearMismatch:
    case CC_OneArgumentMismatch:
    case CC_OneGenericArgumentNearMismatch:
    case CC_OneGenericArgumentMismatch:
    case CC_ArgumentNearMismatch:
    case CC_ArgumentMismatch:
    case CC_GenericNonsubstitutableMismatch:
      break; // Near match cases

    case CC_ExactMatch:
      diagnostic = diag::member_shadows_global_function;
      break;
    }

    auto choice = calleeInfo.candidates[0].getDecl();
    auto baseKind = getBaseKind(base);
    auto baseName = getBaseName(choice->getDeclContext());

    auto origCandidate = CCI[0].getDecl();
    ctx.Diags.diagnose(UDE->getLoc(), diagnostic, UDE->getName(),
                      origCandidate->getDescriptiveKind(),
                      origCandidate->getFullName(),
                      choice->getDescriptiveKind(),
                      choice->getFullName(), baseKind, baseName);

    auto topLevelDiag = diag::fix_unqualified_access_top_level;
    if (baseKind == DescriptiveDeclKind::Module)
      topLevelDiag = diag::fix_unqualified_access_top_level_multi;

    emitFixItForExplicitlyQualifiedReference(ctx.Diags, UDE, topLevelDiag,
                                             baseName,
                                             choice->getDescriptiveKind());

    for (auto &candidate : calleeInfo.candidates) {
      if (auto decl = candidate.getDecl())
        ctx.Diags.diagnose(decl, diag::decl_declared_here, decl->getFullName());
    }

    return true;
  };

  // For each of the parent contexts, let's try to find any candidates
  // which have the same name and the same number of arguments as callee.
  while (context->getParent()) {
    auto result =
        TypeChecker::lookupUnqualified(context, UDE->getName(), UDE->getLoc());
    context = context->getParent();

    if (!result || result.empty())
      continue;

    CandidateMap candidates;
    for (const auto &candidate : result) {
      auto base = candidate.getBaseDecl();
      auto decl = candidate.getValueDecl();
      if ((base && base->isInvalid()) || decl->isInvalid())
        continue;

      // If base is present but it doesn't represent a valid nominal,
      // we can't use current candidate as one of the choices.
      if (base && !base->getInterfaceType()->getNominalOrBoundGenericNominal())
        continue;

      auto context = decl->getDeclContext();
      // We are only interested in static or global functions, because
      // there is no way to call anything else properly.
      if (!decl->isStatic() && !context->isModuleScopeContext())
        continue;

      OverloadChoice choice(base ? base->getInterfaceType() : nullptr,
                            decl, UDE->getFunctionRefKind());

      if (base) { // Let's group all of the candidates have a common base.
        candidates[base].push_back(choice);
        continue;
      }

      // If there is no base, it means this is one of the global functions,
      // let's try to diagnose its shadowing inline.
      if (diagnoseShadowing(base, choice))
        return true;
    }

    if (candidates.empty())
      continue;

    for (const auto &candidate : candidates) {
      if (diagnoseShadowing(candidate.getFirst(), candidate.getSecond()))
        return true;
    }
  }

  return false;
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
  if (auto *MTT = CS.getType(fnExpr)->getAs<MetatypeType>()) {
    auto instTy = MTT->getInstanceType();
    auto &DE = CS.getASTContext().Diags;
    if (instTy->getAnyNominal()) {
      // If we are invoking a constructor on a nominal type and there are
      // absolutely no candidates, then they must all be private.
      if (CCI.empty() || (CCI.size() == 1 && CCI.candidates[0].getDecl() &&
                              isa<ProtocolDecl>(CCI.candidates[0].getDecl()))) {
        DE.diagnose(fnExpr->getLoc(), diag::no_accessible_initializers,
                    instTy);
        return true;
      }
      // continue below
    } else if (!instTy->is<TupleType>()) {
      // If we are invoking a constructor on a non-nominal type, the expression
      // is malformed.
      SourceRange initExprRange(fnExpr->getSourceRange().Start,
                                argExpr->getSourceRange().End);
      DE.diagnose(fnExpr->getLoc(), instTy->isExistentialType() ?
                  diag::construct_protocol_by_name :
                  diag::non_nominal_no_initializers, instTy)
          .highlight(initExprRange);
      return true;
    }
  }

  // Try to diagnose errors related to the use of implicit self reference.
  if (diagnoseImplicitSelfErrors(fnExpr, argExpr, CCI, argLabels))
    return true;

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

bool FailureDiagnosis::diagnoseSubscriptErrors(SubscriptExpr *SE,
                                               bool inAssignmentDestination) {
  auto baseExpr = typeCheckChildIndependently(SE->getBase());
  if (!baseExpr) return true;
  auto baseType = CS.getType(baseExpr);

  if (isa<NilLiteralExpr>(baseExpr)) {
    diagnose(baseExpr->getLoc(), diag::cannot_subscript_nil_literal)
      .highlight(baseExpr->getSourceRange());
    return true;
  }

  std::function<bool(ArrayRef<OverloadChoice>)> callback =
      [&](ArrayRef<OverloadChoice> candidates) -> bool {
    CalleeCandidateInfo calleeInfo(Type(), candidates, SE->hasTrailingClosure(),
                                   CS, /*selfAlreadyApplied*/ false);

    // We're about to typecheck the index list, which needs to be processed with
    // self already applied.
    for (unsigned i = 0, e = calleeInfo.size(); i != e; ++i)
      calleeInfo.candidates[i].skipCurriedSelf = true;

    auto indexExpr =
        typeCheckArgumentChildIndependently(SE->getIndex(), Type(), calleeInfo);
    if (!indexExpr)
      return true;

    // Back to analyzing the candidate list with self applied.
    for (unsigned i = 0, e = calleeInfo.size(); i != e; ++i)
      calleeInfo.candidates[i].skipCurriedSelf = false;

    ArrayRef<Identifier> argLabels = SE->getArgumentLabels();
    if (diagnoseParameterErrors(calleeInfo, SE, indexExpr, argLabels))
      return true;

    auto indexType = CS.getType(indexExpr);

    auto decomposedBaseType = decomposeArgType(baseType, {Identifier()});
    auto decomposedIndexType = decomposeArgType(indexType, argLabels);
    calleeInfo.filterList(
        [&](OverloadCandidate cand) -> CalleeCandidateInfo::ClosenessResultTy {
          // Classify how close this match is.  Non-subscript decls don't match.
          auto subscriptDecl = dyn_cast_or_null<SubscriptDecl>(cand.getDecl());
          if (!subscriptDecl ||
              (inAssignmentDestination && !subscriptDecl->supportsMutation()))
            return {CC_GeneralMismatch, {}};

          // Check whether the self type matches.
          auto selfConstraint = CC_ExactMatch;
          if (calleeInfo.evaluateCloseness(cand, decomposedBaseType).first !=
              CC_ExactMatch)
            selfConstraint = CC_SelfMismatch;

          // Set a flag to look past the self argument to the indices.
          cand.skipCurriedSelf = true;

          // Explode out multi-index subscripts to find the best match.
          auto indexResult =
              calleeInfo.evaluateCloseness(cand, decomposedIndexType);
          if (selfConstraint > indexResult.first)
            return {selfConstraint, {}};
          return indexResult;
        });

    // If the closest matches all mismatch on self, we either have something
    // that cannot be subscripted, or an ambiguity.
    if (calleeInfo.closeness == CC_SelfMismatch) {
      diagnose(SE->getLoc(), diag::cannot_subscript_base, baseType)
          .highlight(SE->getBase()->getSourceRange());
      // FIXME: Should suggest overload set, but we're not ready for that until
      // it points to candidates and identifies the self type in the diagnostic.
      // calleeInfo.suggestPotentialOverloads(SE->getLoc());
      return true;
    }

    // Any other failures relate to the index list.
    for (unsigned i = 0, e = calleeInfo.size(); i != e; ++i)
      calleeInfo.candidates[i].skipCurriedSelf = true;

    // TODO: Is there any reason to check for CC_NonLValueInOut here?

    if (calleeInfo.closeness == CC_ExactMatch) {
      auto message = diag::ambiguous_subscript;

      // If there is an exact match on the argument with
      // a single candidate, let's type-check subscript
      // as a whole to figure out if there is any structural
      // problem after all.
      if (calleeInfo.size() == 1) {
        Expr *expr = SE;
        ConcreteDeclRef decl = nullptr;
        message = diag::cannot_subscript_with_index;

        if (TypeChecker::getTypeOfExpressionWithoutApplying(expr, CS.DC, decl))
          return false;

        // If we are down to a single candidate but with an unresolved
        // index type, we can substitute in the base type to get a simpler
        // and more concrete expected type for this subscript decl, in order
        // to diagnose a better error.
        if (baseType && indexType->hasUnresolvedType()) {
          auto cand = calleeInfo.candidates[0];
          auto candType = baseType->getTypeOfMember(CS.DC->getParentModule(),
                                                    cand.getDecl(), nullptr);
          if (auto *candFunc = candType->getAs<FunctionType>()) {
            auto paramsType = FunctionType::composeInput(CS.getASTContext(),
                                                         candFunc->getParams(),
                                                         false);
            if (!typeCheckChildIndependently(
                    indexExpr, paramsType, CTP_CallArgument, TCC_ForceRecheck))
              return true;
          }
        }
      }

      diagnose(SE->getLoc(), message, baseType, indexType)
          .highlight(indexExpr->getSourceRange())
          .highlight(baseExpr->getSourceRange());

      // FIXME: suggestPotentialOverloads should do this.
      // calleeInfo.suggestPotentialOverloads(SE->getLoc());
      for (auto candidate : calleeInfo.candidates)
        if (auto decl = candidate.getDecl())
          diagnose(decl, diag::found_candidate);
        else
          diagnose(candidate.getExpr()->getLoc(), diag::found_candidate);

      return true;
    }

    if (diagnoseParameterErrors(calleeInfo, SE, indexExpr, argLabels))
      return true;

    // Diagnose some simple and common errors.
    if (calleeInfo.diagnoseSimpleErrors(SE))
      return true;

    diagnose(SE->getLoc(), diag::cannot_subscript_with_index, baseType,
             indexType);

    calleeInfo.suggestPotentialOverloads(SE->getLoc());
    return true;
  };

  auto locator =
      CS.getConstraintLocator(SE, ConstraintLocator::SubscriptMember);

  return diagnoseMemberFailures(SE, baseExpr, ConstraintKind::ValueMember,
                                DeclBaseName::createSubscript(),
                                FunctionRefKind::DoubleApply, locator,
                                callback);
}

bool FailureDiagnosis::visitSubscriptExpr(SubscriptExpr *SE) {
  return diagnoseSubscriptErrors(SE, /* inAssignmentDestination = */ false);
}

namespace {
  /// Type checking listener for pattern binding initializers.
  class CalleeListener : public ExprTypeCheckListener {
    Type contextualType;
  public:
    explicit CalleeListener(Type contextualType)
      : contextualType(contextualType) { }

    bool builtConstraints(ConstraintSystem &cs, Expr *expr) override {
      // If we have no contextual type, there is nothing to do.
      if (!contextualType)
        return false;

      // If the expression is obviously something that produces a metatype,
      // then don't put a constraint on it.
      auto semExpr = expr->getValueProvidingExpr();
      if (isa<TypeExpr>(semExpr))
        return false;

      auto resultLocator =
        cs.getConstraintLocator(expr, ConstraintLocator::FunctionResult);
      auto resultType = cs.createTypeVariable(resultLocator,
                                              TVO_CanBindToLValue |
                                              TVO_CanBindToNoEscape);

      auto locator = cs.getConstraintLocator(expr);
      cs.addConstraint(ConstraintKind::FunctionResult,
                       cs.getType(expr),
                       resultType,
                       locator);

      cs.addConstraint(ConstraintKind::Conversion,
                       resultType,
                       contextualType,
                       locator);

      return false;
    }
  };
} // end anonymous namespace

static bool diagnoseClosureExplicitParameterMismatch(
    ConstraintSystem &CS, SourceLoc loc,
    ArrayRef<AnyFunctionType::Param> params,
    ArrayRef<AnyFunctionType::Param> args) {
  // We are not trying to diagnose structural problems with top-level
  // arguments here.
  if (params.size() != args.size())
    return false;

  for (unsigned i = 0, n = params.size(); i != n; ++i) {
    auto paramType = params[i].getOldType();
    auto argType = args[i].getOldType();

    if (auto paramFnType = paramType->getAs<AnyFunctionType>()) {
      if (auto argFnType = argType->getAs<AnyFunctionType>())
        return diagnoseClosureExplicitParameterMismatch(
            CS, loc, paramFnType->getParams(), argFnType->getParams());
    }

    if (!paramType || !argType || isUnresolvedOrTypeVarType(paramType) ||
        isUnresolvedOrTypeVarType(argType))
      continue;

    if (!TypeChecker::isConvertibleTo(argType, paramType, CS.DC)) {
      CS.getASTContext().Diags.diagnose(loc, diag::types_not_convertible,
                                        false, paramType, argType);
      return true;
    }
  }

  return false;
}

bool FailureDiagnosis::diagnoseTrailingClosureErrors(ApplyExpr *callExpr) {
  if (!callExpr->hasTrailingClosure())
    return false;

  auto *DC = CS.DC;
  auto *fnExpr = callExpr->getFn();
  auto *argExpr = callExpr->getArg();

  ClosureExpr *closureExpr = nullptr;
  if (auto *PE = dyn_cast<ParenExpr>(argExpr)) {
    closureExpr = dyn_cast<ClosureExpr>(PE->getSubExpr());
  } else {
    return false;
  }

  if (!closureExpr)
    return false;

  class CallResultListener : public ExprTypeCheckListener {
    Type expectedResultType;

  public:
    explicit CallResultListener(Type resultType)
        : expectedResultType(resultType) {}

    bool builtConstraints(ConstraintSystem &cs, Expr *expr) override {
      if (!expectedResultType)
        return false;

      auto resultType = cs.getType(expr);
      auto *locator = cs.getConstraintLocator(expr);

      // Since we know that this is trailing closure, format of the
      // type could be like this - ((Input) -> Result) -> ClosureResult
      // which we can leverage to create specific conversion for
      // result type of the call itself, this might help us gain
      // some valuable contextual information.
      if (auto *fnType = resultType->getAs<AnyFunctionType>()) {
        cs.addConstraint(ConstraintKind::Conversion, fnType->getResult(),
                         expectedResultType, locator);
      } else if (auto *typeVar = resultType->getAs<TypeVariableType>()) {
        auto tv = cs.createTypeVariable(cs.getConstraintLocator(expr),
                                        TVO_CanBindToLValue |
                                        TVO_PrefersSubtypeBinding |
                                        TVO_CanBindToNoEscape);

        auto extInfo = FunctionType::ExtInfo().withThrows();

        FunctionType::Param tvParam(tv);
        auto fTy = FunctionType::get({tvParam}, expectedResultType, extInfo);

        // Add a conversion constraint between the types.
        cs.addConstraint(ConstraintKind::Conversion, typeVar, fTy, locator,
                         /*isFavored*/ true);
      }

      return false;
    }
  };

  SmallPtrSet<TypeBase *, 4> possibleTypes;
  auto currentType = CS.simplifyType(CS.getType(fnExpr));

  // If current type has type variables or unresolved types
  // let's try to re-typecheck it to see if we can get some
  // more information about what is going on.
  if (currentType->hasTypeVariable() || currentType->hasUnresolvedType()) {
    auto contextualType = CS.getContextualType();
    CallResultListener listener(contextualType);
    getPossibleTypesOfExpressionWithoutApplying(
        fnExpr, CS.DC, possibleTypes, FreeTypeVariableBinding::UnresolvedType,
        &listener);

    // Looks like there is there a contextual mismatch
    // related to function type, let's try to diagnose it.
    if (possibleTypes.empty() && contextualType &&
        !contextualType->hasUnresolvedType())
      return diagnoseContextualConversionError(callExpr, contextualType,
                                               CS.getContextualTypePurpose());
  } else {
    possibleTypes.insert(currentType.getPointer());
  }

  for (Type type : possibleTypes) {
    auto *fnType = type->getAs<AnyFunctionType>();
    if (!fnType)
      continue;

    auto params = fnType->getParams();
    if (params.size() != 1)
      return false;

    Type paramType = params.front().getOldType();
    if (auto paramFnType = paramType->getAs<AnyFunctionType>()) {
      auto closureType = CS.getType(closureExpr);
      if (auto *argFnType = closureType->getAs<AnyFunctionType>()) {
        auto *params = closureExpr->getParameters();
        auto loc = params ? params->getStartLoc() : closureExpr->getStartLoc();
        if (diagnoseClosureExplicitParameterMismatch(
                CS, loc, argFnType->getParams(), paramFnType->getParams()))
          return true;
      }
    }

    auto processor = [&](Type resultType, Type expectedResultType) -> bool {
      if (resultType && expectedResultType) {
        if (!resultType->isEqual(expectedResultType)) {
          auto &DE = CS.getASTContext().Diags;
          DE.diagnose(closureExpr->getEndLoc(),
                      diag::cannot_convert_closure_result, resultType,
                      expectedResultType);
          return true;
        }

        // Looks like both actual and expected result types match,
        // there is nothing we can diagnose in this case.
        return false;
      }

      // If we got a result type, let's re-typecheck the function using it,
      // maybe we can find a problem where contextually we expect one type
      // but trailing closure produces completely different one.
      auto fnType = paramType->getAs<AnyFunctionType>();
      if (!fnType)
        return false;

      class ClosureCalleeListener : public ExprTypeCheckListener {
        FunctionType *InputType;
        Type ResultType;

      public:
        explicit ClosureCalleeListener(FunctionType *inputType, Type resultType)
            : InputType(inputType), ResultType(resultType) {}

        bool builtConstraints(ConstraintSystem &cs, Expr *expr) override {
          if (!ResultType)
            return false;

          AnyFunctionType::Param Input(InputType);
          auto expectedType = FunctionType::get({Input}, ResultType);
          cs.addConstraint(ConstraintKind::Conversion, cs.getType(expr),
                           expectedType, cs.getConstraintLocator(expr),
                           /*isFavored*/ true);
          return false;
        }
      };

      auto expectedArgType = FunctionType::get(fnType->getParams(), resultType,
                                               fnType->getExtInfo());

      llvm::SaveAndRestore<DeclContext *> SavedDC(CS.DC, DC);
      ClosureCalleeListener listener(expectedArgType, CS.getContextualType());
      return !typeCheckChildIndependently(callExpr->getFn(), Type(),
                                          CTP_CalleeResult, TCC_ForceRecheck,
                                          &listener);
    };

    // Let's see if there are any structural problems with closure itself.
    if (diagnoseClosureExpr(closureExpr, paramType, processor))
      return true;
  }

  return false;
}

/// Check if there failure associated with expression is related
/// to given contextual type.
bool FailureDiagnosis::diagnoseCallContextualConversionErrors(
    ApplyExpr *callExpr, Type contextualType, ContextualTypePurpose CTP) {
  if (!contextualType || contextualType->hasUnresolvedType())
    return false;

  auto typeCheckExpr = [&](Expr *expr, DeclContext *DC,
                           SmallPtrSetImpl<TypeBase *> &types) {
    getPossibleTypesOfExpressionWithoutApplying(
        expr, DC, types, FreeTypeVariableBinding::Disallow);
  };

  // First let's type-check expression without contextual type, and
  // see if that's going to produce a type, if so, let's type-check
  // again, this time using given contextual type.
  SmallPtrSet<TypeBase *, 4> withoutContextual;
  typeCheckExpr(callExpr, CS.DC, withoutContextual);

  // If there are no types returned, it means that problem was
  // nothing to do with contextual information, probably parameter/argument
  // mismatch.
  if (withoutContextual.empty())
    return false;

  Type exprType = withoutContextual.size() == 1 ? *withoutContextual.begin() : Type();
  return diagnoseContextualConversionError(callExpr, contextualType, CTP,
                                           exprType);
}

// Check if there is a structural problem in the function expression
// by performing type checking with the option to allow unresolved
// type variables. If that is going to produce a function type with
// unresolved result let's not re-typecheck the function expression,
// because it might produce unrelated diagnostics due to lack of
// contextual information.
static bool shouldTypeCheckFunctionExpr(FailureDiagnosis &FD, DeclContext *DC,
                                        Expr *fnExpr) {
  if (!isa<UnresolvedDotExpr>(fnExpr))
    return true;

  SmallPtrSet<TypeBase *, 4> fnTypes;
  FD.getPossibleTypesOfExpressionWithoutApplying(
      fnExpr, DC, fnTypes, FreeTypeVariableBinding::UnresolvedType);

  if (fnTypes.size() == 1) {
    // Some member types depend on the arguments to produce a result type,
    // type-checking such expressions without associated arguments is
    // going to produce unrelated diagnostics.
    if (auto fn = (*fnTypes.begin())->getAs<AnyFunctionType>()) {
      auto resultType = fn->getResult();
      if (resultType->hasUnresolvedType() || resultType->hasTypeVariable())
        return false;
    }
  }

  // Might be a structural problem related to the member itself.
  return true;
}

// Check if any candidate of the overload set can accept a specified
// number of arguments, regardless of parameter type or label information.
static bool isViableOverloadSet(const CalleeCandidateInfo &CCI,
                                size_t numArgs) {
  for (unsigned i = 0; i < CCI.size(); ++i) {
    auto &&cand = CCI[i];
    auto funcDecl = dyn_cast_or_null<AbstractFunctionDecl>(cand.getDecl());

    // If we don't have a func decl or we haven't resolved its parameters,
    // continue. The latter case can occur with `type(of:)`, which is introduced
    // as a type variable.
    if (!funcDecl || !cand.hasParameters())
      continue;

    auto params = cand.getParameters();
    bool hasVariadicParameter = false;
    auto pairMatcher = [&](unsigned argIdx, unsigned paramIdx) {
      hasVariadicParameter |= params[paramIdx].isVariadic();
      return true;
    };

    auto paramInfo = cand.getParameterListInfo(params);
    InputMatcher IM(params, paramInfo);
    auto result = IM.match(numArgs, pairMatcher);
    if (result == InputMatcher::IM_Succeeded)
      return true;
    if (result == InputMatcher::IM_HasUnclaimedInput && hasVariadicParameter)
      return true;
  }
  return false;
}

bool FailureDiagnosis::visitApplyExpr(ApplyExpr *callExpr) {
  // If this call involves trailing closure as an argument,
  // let's treat it specially, because re-typecheck of the
  // either function or arguments might results in diagnosing
  // of the unrelated problems due to luck of context.
  if (diagnoseTrailingClosureErrors(callExpr))
    return true;

  if (diagnoseCallContextualConversionErrors(callExpr, CS.getContextualType(),
                                             CS.getContextualTypePurpose()))
    return true;

  auto *fnExpr = callExpr->getFn();
  auto originalFnType = CS.getType(callExpr->getFn());

  if (shouldTypeCheckFunctionExpr(*this, CS.DC, fnExpr)) {
    // Type check the function subexpression to resolve a type for it if
    // possible.
    fnExpr = typeCheckChildIndependently(callExpr->getFn());
    if (!fnExpr) {
      return CS.getASTContext().Diags.hadAnyError();
    }
  }

  SWIFT_DEFER {
    if (!fnExpr) return;

    // If it's a member operator reference, put the operator back.
    if (auto operatorRef = fnExpr->getMemberOperatorRef())
      callExpr->setFn(operatorRef);
  };

  auto getFuncType = [](Type type) -> Type { return type->getRValueType(); };

  auto fnType = getFuncType(CS.getType(fnExpr));

  // Let's see if this has to do with member vs. property error
  // because sometimes when there is a member and a property declared
  // on the nominal type with the same name. Type-checking function
  // expression separately from arguments might produce solution for
  // the property instead of the member.
  if (!fnType->is<AnyFunctionType>() &&
    isa<UnresolvedDotExpr>(callExpr->getFn())) {
    fnExpr = callExpr->getFn();

    SmallPtrSet<TypeBase *, 4> types;
    getPossibleTypesOfExpressionWithoutApplying(fnExpr, CS.DC, types);

    auto isFunctionType = [getFuncType](Type type) -> bool {
      return type && getFuncType(type)->is<AnyFunctionType>();
    };

    auto fnTypes = std::find_if(types.begin(), types.end(), isFunctionType);
    if (fnTypes != types.end()) {
      auto funcType = getFuncType(*fnTypes);
      // If there is only one function type, let's use it.
      if (std::none_of(std::next(fnTypes), types.end(), isFunctionType))
        fnType = funcType;
    } else {
      fnType = getFuncType(originalFnType);
    }
  }

  // If we have a contextual type, and if we have an ambiguously typed function
  // result from our previous check, we re-type-check it using this contextual
  // type to inform the result type of the callee.
  //
  // We only do this as a second pass because the first pass we just did may
  // return something of obviously non-function-type.  If this happens, we
  // produce better diagnostics below by diagnosing this here rather than trying
  // to peel apart the failed conversion to function type.
  if (CS.getContextualType() &&
      (isUnresolvedOrTypeVarType(fnType) ||
       (fnType->is<AnyFunctionType>() && fnType->hasUnresolvedType()))) {
    // FIXME: Prevent typeCheckChildIndependently from transforming expressions,
    // because if we try to typecheck OSR expression with contextual type,
    // it'll end up converting it into DeclRefExpr based on contextual info,
    // instead let's try to get a type without applying and filter callee
    // candidates later on.
    CalleeListener listener(CS.getContextualType());

    if (isa<OverloadSetRefExpr>(fnExpr)) {
      assert(!cast<OverloadSetRefExpr>(fnExpr)->getReferencedDecl() &&
             "unexpected declaration reference");

      ConcreteDeclRef decl = nullptr;
      Type type = TypeChecker::getTypeOfExpressionWithoutApplying(
          fnExpr, CS.DC, decl, FreeTypeVariableBinding::UnresolvedType,
          &listener);

      if (type)
        fnType = getFuncType(type);
    } else {
      fnExpr = typeCheckChildIndependently(callExpr->getFn(), Type(),
                                           CTP_CalleeResult, TCC_ForceRecheck,
                                           &listener);
      if (!fnExpr)
        return true;

      fnType = getFuncType(CS.getType(fnExpr));
    }
  }

  // If we resolved a concrete expression for the callee, and it has
  // non-function/non-metatype type, then we cannot call it!
  if (!isUnresolvedOrTypeVarType(fnType) &&
      !fnType->is<AnyFunctionType>() && !fnType->is<MetatypeType>()) {
    auto arg = callExpr->getArg();
    auto isDynamicCallable =
        CS.DynamicCallableCache[fnType->getCanonicalType()].isValid();

    auto hasCallAsFunctionMethods = fnType->isCallableNominalType(CS.DC);

    // Diagnose specific @dynamicCallable errors.
    if (isDynamicCallable) {
      auto dynamicCallableMethods =
        CS.DynamicCallableCache[fnType->getCanonicalType()];

      // Diagnose dynamic calls with keywords on @dynamicCallable types that
      // don't define the `withKeywordArguments` method.
      if (auto tuple = dyn_cast<TupleExpr>(arg)) {
        bool hasArgLabel = llvm::any_of(
          tuple->getElementNames(), [](Identifier i) { return !i.empty(); });
        if (hasArgLabel &&
            dynamicCallableMethods.keywordArgumentsMethods.empty()) {
          diagnose(callExpr->getFn()->getStartLoc(),
                   diag::missing_dynamic_callable_kwargs_method, fnType);
          return true;
        }
      }
    }

    auto isExistentialMetatypeType = fnType->is<ExistentialMetatypeType>();
    if (isExistentialMetatypeType) {
      auto diag = diagnose(arg->getStartLoc(),
                           diag::missing_init_on_metatype_initialization);
      diag.highlight(fnExpr->getSourceRange());
    } else if (!isDynamicCallable) {
      auto diag = diagnose(arg->getStartLoc(),
                           diag::cannot_call_non_function_value, fnType);
      diag.highlight(fnExpr->getSourceRange());

      // If the argument is an empty tuple, then offer a
      // fix-it to remove the empty tuple and use the value
      // directly.
      if (auto tuple = dyn_cast<TupleExpr>(arg)) {
        if (tuple->getNumElements() == 0) {
          diag.fixItRemove(arg->getSourceRange());
        }
      }
    }

    // If the argument is a trailing ClosureExpr (i.e. {....}) and it is on
    // the line after the callee, then it's likely the user forgot to
    // write "do" before their brace stmt.
    // Note that line differences of more than 1 are diagnosed during parsing.
    if (auto *PE = dyn_cast<ParenExpr>(arg)) {
      if (PE->hasTrailingClosure() && isa<ClosureExpr>(PE->getSubExpr())) {
        auto *closure = cast<ClosureExpr>(PE->getSubExpr());
        auto &SM = CS.getASTContext().SourceMgr;
        if (closure->hasAnonymousClosureVars() &&
            closure->getParameters()->size() == 0 &&
            1 + SM.getLineNumber(callExpr->getFn()->getEndLoc()) ==
            SM.getLineNumber(closure->getStartLoc())) {
          diagnose(closure->getStartLoc(), diag::brace_stmt_suggest_do)
            .fixItInsert(closure->getStartLoc(), "do ");
        }
      }
    }

    // Use the existing machinery to provide more useful diagnostics for
    // @dynamicCallable calls, rather than cannot_call_non_function_value.
    if ((isExistentialMetatypeType || !isDynamicCallable) &&
    	  !hasCallAsFunctionMethods) {
      return true;
    }
  }
  
  bool hasTrailingClosure = callArgHasTrailingClosure(callExpr->getArg());
  
  // Collect a full candidate list of callees based on the partially type
  // checked function.
  CalleeCandidateInfo calleeInfo(fnExpr, hasTrailingClosure, CS);

  // In the case that function subexpression was resolved independently in
  // the first place, the resolved type may not provide the best diagnostic.
  // We consider the number of arguments to decide whether we'd go with it or
  // stay with the original one.
  if (fnExpr != callExpr->getFn()) {
    bool isInstanceMethodAsCurriedMemberOnType = false;
    if (!calleeInfo.empty()) {
      auto &&cand = calleeInfo[0];
      auto decl = cand.getDecl();
      if (decl && decl->isInstanceMember() && !cand.skipCurriedSelf &&
          cand.getParameters().size() == 1)
        isInstanceMethodAsCurriedMemberOnType = true;
    }

    // In terms of instance method as curried member on type, we should not
    // take the number of arguments into account.
    if (!isInstanceMethodAsCurriedMemberOnType) {
      size_t numArgs = 1;
      auto arg = callExpr->getArg();
      if (auto tuple = dyn_cast<TupleExpr>(arg)) {
        numArgs = tuple->getNumElements();
      }

      if (!isViableOverloadSet(calleeInfo, numArgs)) {
        CalleeCandidateInfo calleeInfoOrig(callExpr->getFn(),
                                           hasTrailingClosure, CS);
        if (isViableOverloadSet(calleeInfoOrig, numArgs)) {
          fnExpr = callExpr->getFn();
          fnType = getFuncType(CS.getType(fnExpr));
          calleeInfo = calleeInfoOrig;
        }
      }
    }
  }

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

  // Let's check whether this is a situation when callee expects
  // no arguments but N are given. Otherwise, just below
  // `typeCheckArgumentChild*` is going to use `()` is a contextual type which
  // is incorrect.
  if (argType && argType->isVoid()) {
    auto *argExpr = callExpr->getArg();
    if (isa<ParenExpr>(argExpr) ||
        (isa<TupleExpr>(argExpr) &&
         cast<TupleExpr>(argExpr)->getNumElements() > 0)) {
      diagnose(callExpr->getLoc(), diag::extra_argument_to_nullary_call)
          .highlight(argExpr->getSourceRange());
      return true;
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

  // Diagnose some simple and common errors.
  if (calleeInfo.diagnoseSimpleErrors(callExpr))
    return true;

  // Force recheck of the arg expression because we allowed unresolved types
  // before, and that turned out not to help, and now we want any diagnoses
  // from disallowing them.
  argExpr = typeCheckArgumentChildIndependently(callExpr->getArg(), argType,
                                                calleeInfo, TCC_ForceRecheck);
  if (!argExpr)
    return true; // already diagnosed.
  
  // Handle argument label mismatches when we have multiple candidates.
  if (calleeInfo.closeness == CC_ArgumentLabelMismatch) {
    auto args = decomposeArgType(CS.getType(argExpr), argLabels);

    // If we have multiple candidates that we fail to match, just say we have
    // the wrong labels and list the candidates out.
    diagnose(callExpr->getLoc(), diag::wrong_argument_labels_overload,
             getParamListAsString(args))
      .highlight(argExpr->getSourceRange());

    // Did the user intend on invoking a different overload?
    calleeInfo.suggestPotentialOverloads(fnExpr->getLoc());
    return true;
  }

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

    if (!CS.getContextualType() ||
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

    // TODO(diagnostics): There are still cases not yet handled by new
    // diagnostics framework e.g.
    //
    // var tuple = (1, 2, 3)
    // switch tuple {
    //   case (let (_, _, _)) + 1: break
    // }
    if (callExpr->isImplicit() && overloadName == "~=") {
      auto flags = ParameterTypeFlags();
      if (calleeInfo.candidates.size() == 1)
        if (auto fnType = calleeInfo.candidates[0].getFunctionType())
          flags = fnType->getParams()[0].getParameterFlags();

      auto *locator = CS.getConstraintLocator(
          callExpr,
          {ConstraintLocator::ApplyArgument,
           LocatorPathElt::ApplyArgToParam(0, 0, flags)},
          /*summaryFlags=*/0);

      ArgumentMismatchFailure failure(CS, lhsType, rhsType, locator);
      return failure.diagnosePatternMatchingMismatch();
    }

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

  SmallVector<AnyFunctionType::Param, 8> params;
  AnyFunctionType::decomposeInput(CS.getType(argExpr), params);
  auto argString = AnyFunctionType::getParamListAsString(params);

  // If we couldn't get the name of the callee, then it must be something of a
  // more complex "value of function type".
  if (overloadName.empty()) {
    // If we couldn't infer the result type of the closure expr, then we have
    // some sort of ambiguity, let the ambiguity diagnostic stuff handle this.
    if (auto ffty = fnType->getAs<AnyFunctionType>())
      if (ffty->getResult()->hasTypeVariable()) {
        diagnoseAmbiguity(fnExpr);
        return true;
      }
    
    // The most common unnamed value of closure type is a ClosureExpr, so
    // special case it.
    if (isa<ClosureExpr>(fnExpr->getValueProvidingExpr())) {
      if (fnType->hasTypeVariable())
        diagnose(argExpr->getStartLoc(), diag::cannot_invoke_closure, argString)
          .highlight(fnExpr->getSourceRange());
      else
        diagnose(argExpr->getStartLoc(), diag::cannot_invoke_closure_type,
                 fnType, argString)
          .highlight(fnExpr->getSourceRange());
      
    } else if (fnType->hasTypeVariable()) {
      diagnose(argExpr->getStartLoc(), diag::cannot_call_function_value,
               argString)
        .highlight(fnExpr->getSourceRange());
    } else {
      diagnose(argExpr->getStartLoc(), diag::cannot_call_value_of_function_type,
                fnType, argString)
        .highlight(fnExpr->getSourceRange());
    }
    
    return true;
  }

  if (auto MTT = fnType->getAs<MetatypeType>()) {
    if (MTT->getInstanceType()->isExistentialType()) {
      diagnose(fnExpr->getLoc(), diag::construct_protocol_value, fnType);
      return true;
    }
  }
  
  bool isInitializer = isa<TypeExpr>(fnExpr);
  if (isa<TupleExpr>(argExpr) &&
      cast<TupleExpr>(argExpr)->getNumElements() == 0) {
    // Emit diagnostics that say "no arguments".
    diagnose(fnExpr->getLoc(), diag::cannot_call_with_no_params,
             overloadName, isInitializer);
  } else {
    diagnose(fnExpr->getLoc(), diag::cannot_call_with_params,
             overloadName, argString, isInitializer);
  }

  // Did the user intend on invoking a different overload?
  calleeInfo.suggestPotentialOverloads(fnExpr->getLoc());
  return true;
}

bool FailureDiagnosis::visitCoerceExpr(CoerceExpr *CE) {
  // Coerce the input to whatever type is specified by the CoerceExpr.
  auto expr = typeCheckChildIndependently(CE->getSubExpr(),
                                          CS.getType(CE->getCastTypeLoc()),
                                          CTP_CoerceOperand);
  if (!expr)
    return true;

  auto ref = expr->getReferencedDecl();
  if (auto *decl = ref.getDecl()) {
    // Without explicit coercion we might end up
    // type-checking sub-expression as unavaible
    // declaration, let's try to diagnose that here.
    if (AvailableAttr::isUnavailable(decl))
      return diagnoseExplicitUnavailability(
          decl, expr->getSourceRange(), CS.DC, dyn_cast<ApplyExpr>(expr));
  }

  return false;
}

bool FailureDiagnosis::visitIfExpr(IfExpr *IE) {
  auto typeCheckClauseExpr = [&](Expr *clause, Type contextType = Type(),
                                 ContextualTypePurpose convertPurpose =
                                     CTP_Unused) -> Expr * {
    // Provide proper contextual type when type conversion is specified.
    return typeCheckChildIndependently(clause, contextType, convertPurpose,
                                       TCCOptions(), nullptr, false);
  };
  // Check all of the subexpressions independently.
  auto condExpr = typeCheckClauseExpr(IE->getCondExpr());
  if (!condExpr) return true;
  auto trueExpr = typeCheckClauseExpr(IE->getThenExpr(), CS.getContextualType(),
                                      CS.getContextualTypePurpose());
  if (!trueExpr) return true;
  auto falseExpr = typeCheckClauseExpr(
      IE->getElseExpr(), CS.getContextualType(), CS.getContextualTypePurpose());
  if (!falseExpr) return true;

  // If the true/false values already match, it must be a contextual problem.
  if (CS.getType(trueExpr)->isEqual(CS.getType(falseExpr)))
    return false;
  
  // Otherwise, the true/false result types must not be matching.
  diagnose(IE->getColonLoc(), diag::if_expr_cases_mismatch,
           CS.getType(trueExpr), CS.getType(falseExpr))
      .highlight(trueExpr->getSourceRange())
      .highlight(falseExpr->getSourceRange());
  return true;
}


bool FailureDiagnosis::
visitRebindSelfInConstructorExpr(RebindSelfInConstructorExpr *E) {
  // Don't walk the children for this node, it leads to multiple diagnostics
  // because of how sema injects this node into the type checker.
  return false;
}

bool FailureDiagnosis::visitCaptureListExpr(CaptureListExpr *CLE) {
  // Always walk into the closure of a capture list expression.
  return visitClosureExpr(CLE->getClosureBody());
}

static bool isInvalidClosureResultType(Type resultType) {
  return !resultType || resultType->hasUnresolvedType() ||
          resultType->hasTypeVariable() || resultType->hasArchetype();
}

bool FailureDiagnosis::visitClosureExpr(ClosureExpr *CE) {
  return diagnoseClosureExpr(
      CE, CS.getContextualType(),
      [&](Type resultType, Type expectedResultType) -> bool {
        if (isInvalidClosureResultType(expectedResultType))
          return false;

        // Following situations are possible:
        // * No result type - possible structurable problem in the body;
        // * Function result type - possible use of function without calling it,
        //   which is properly diagnosed by actual type-check call.
        if (resultType && !resultType->getRValueType()->is<AnyFunctionType>()) {
          if (!resultType->isEqual(expectedResultType)) {
            diagnose(CE->getEndLoc(), diag::cannot_convert_closure_result,
                     resultType, expectedResultType);
            return true;
          }
        }
        return false;
      });
}

bool FailureDiagnosis::diagnoseClosureExpr(
    ClosureExpr *CE, Type contextualType,
    llvm::function_ref<bool(Type, Type)> resultTypeProcessor) {
  // Look through IUO because it doesn't influence
  // neither parameter nor return type diagnostics itself,
  // but if we have function type inside, that might
  // signficantly improve diagnostic quality.
  // FIXME: We need to rework this with IUOs out of the type system.
  // if (contextualType) {
  //   if (auto IUO =
  //           CS.lookThroughImplicitlyUnwrappedOptionalType(contextualType))
  //     contextualType = IUO;
  // }

  Type expectedResultType;

  // If we have a contextual type available for this closure, apply it to the
  // ParamDecls in our parameter list.  This ensures that any uses of them get
  // appropriate types.
  if (contextualType && contextualType->is<FunctionType>()) {
    auto fnType = contextualType->getAs<FunctionType>();
    auto *params = CE->getParameters();
    auto inferredArgs = fnType->getParams();
    
    // It is very common for a contextual type to disagree with the argument
    // list built into the closure expr.  This can be because the closure expr
    // had an explicitly specified pattern, a la:
    //    { a,b in ... }
    // or could be because the closure has an implicitly generated one:
    //    { $0 + $1 }
    // in either case, we want to produce nice and clear diagnostics.
    unsigned actualArgCount = params->size();
    unsigned inferredArgCount = inferredArgs.size();

    if (actualArgCount != inferredArgCount) {
      if (inferredArgCount == 1 && actualArgCount > 1) {
        auto *argTupleTy = inferredArgs.front().getOldType()->getAs<TupleType>();
        // Let's see if inferred argument is actually a tuple inside of Paren.
        if (argTupleTy) {
          // Looks like the number of closure parameters matches number
          // of inferred arguments, which means we can we can emit an
          // error about an attempt to make use of tuple splat or tuple
          // destructuring and provide a proper fix-it.
          if (argTupleTy->getNumElements() == actualArgCount) {
            ClosureParamDestructuringFailure failure(
                CS, fnType, CS.getConstraintLocator(CE));
            return failure.diagnoseAsError();
          }
        }
      }

      // Extraneous arguments.
      if (inferredArgCount < actualArgCount) {
        auto diag = diagnose(
            params->getStartLoc(), diag::closure_argument_list_tuple, fnType,
            inferredArgCount, actualArgCount, (actualArgCount == 1));

        bool onlyAnonymousParams =
            std::all_of(params->begin(), params->end(),
                        [](ParamDecl *param) { return !param->hasName(); });

        // If closure expects no parameters but N was given,
        // and all of them are anonymous let's suggest removing them.
        if (inferredArgCount == 0 && onlyAnonymousParams) {
          auto inLoc = CE->getInLoc();
          auto &sourceMgr = CS.getASTContext().SourceMgr;

          if (inLoc.isValid())
            diag.fixItRemoveChars(params->getStartLoc(),
                                  Lexer::getLocForEndOfToken(sourceMgr, inLoc));
        }
        return true;
      }

      // Missing arguments are already diagnosed via new diagnostic framework.
      return false;
    }

    // Coerce parameter types here only if there are no unresolved
    TypeChecker::coerceParameterListToType(params, CE, fnType);
    expectedResultType = fnType->getResult();
  }

  // Defend against type variables from our constraint system leaking into
  // recursive constraints systems formed when checking the body of the
  // closure.  These typevars come into them when the body does name
  // lookups against the parameter decls.
  //
  // Handle this by rewriting the arguments to UnresolvedType().
  for (auto VD : *CE->getParameters()) {
    if (VD->hasInterfaceType() && (VD->getType()->hasTypeVariable() ||
                                   VD->getType()->hasError())) {
      VD->setInterfaceType(CS.getASTContext().TheUnresolvedType);
    }
  }

  // If this is a complex leaf closure, there is nothing more we can do.
  if (!CE->hasSingleExpressionBody())
    return false;

  if (isInvalidClosureResultType(expectedResultType))
    expectedResultType = Type();

  // When we're type checking a single-expression closure, we need to reset the
  // DeclContext to this closure for the recursive type checking.  Otherwise,
  // if there is a closure in the subexpression, we can violate invariants.
  {
    llvm::SaveAndRestore<DeclContext *> SavedDC(CS.DC, CE);

    // Explicitly disallow to produce solutions with unresolved type variables,
    // because there is no auxiliary logic which would handle that and it's
    // better to allow failure diagnosis to run directly on the closure body.
    // Note that presence of contextual type implicitly forbids such solutions,
    // but it's not always reset.

    if (expectedResultType && !CE->hasExplicitResultType()) {
      auto closure = CE->getSingleExpressionBody();
      ConcreteDeclRef decl = nullptr;
      // Let's try to compute result type without mutating AST and
      // using expected (contextual) result type, that's going to help
      // diagnose situations where contextual type expected one result
      // type but actual closure produces a different one without explicitly
      // declaring it (e.g. by using anonymous parameters).
      auto type = TypeChecker::getTypeOfExpressionWithoutApplying(
          closure, CS.DC, decl, FreeTypeVariableBinding::Disallow);

      if (type && resultTypeProcessor(type, expectedResultType))
        return true;
    }

    // If the closure had an expected result type, use it.
    if (CE->hasExplicitResultType())
      expectedResultType = CE->getExplicitResultTypeLoc().getType();

    // If we couldn't diagnose anything related to the contextual result type
    // let's run proper type-check with expected type and try to verify it.

    auto CTP = expectedResultType ? CTP_ClosureResult : CTP_Unused;
    auto *bodyExpr = typeCheckChildIndependently(CE->getSingleExpressionBody(),
                                                 expectedResultType, CTP,
                                                 TCCOptions(), nullptr, false);

    if (!bodyExpr)
      return true;

    if (resultTypeProcessor(CS.getType(bodyExpr), expectedResultType))
      return true;
  }

  // Otherwise, we can't produce a specific diagnostic.
  return false;
}

bool FailureDiagnosis::visitArrayExpr(ArrayExpr *E) {
  // If we had a contextual type, then it either conforms to
  // ExpressibleByArrayLiteral or it is an invalid contextual type.
  auto contextualType = CS.getContextualType();
  if (!contextualType) {
    return false;
  }

  // If our contextual type is an optional, look through them, because we're
  // surely initializing whatever is inside.
  contextualType = contextualType->lookThroughAllOptionalTypes();

  // Validate that the contextual type conforms to ExpressibleByArrayLiteral and
  // figure out what the contextual element type is in place.
  auto ALC =
      TypeChecker::getProtocol(CS.getASTContext(), E->getLoc(),
                               KnownProtocolKind::ExpressibleByArrayLiteral);
  if (!ALC)
    return visitExpr(E);

  // Check to see if the contextual type conforms.
  auto Conformance = TypeChecker::conformsToProtocol(
      contextualType, ALC, CS.DC, ConformanceCheckFlags::InExpression);
  if (Conformance) {
    Type contextualElementType =
        Conformance
            .getTypeWitnessByName(contextualType,
                                  CS.getASTContext().Id_ArrayLiteralElement)
            ->getDesugaredType();

    // Type check each of the subexpressions in place, passing down the contextual
    // type information if we have it.
    for (auto elt : E->getElements()) {
      if (typeCheckChildIndependently(elt, contextualElementType,
                                      CTP_ArrayElement) == nullptr) {
        return true;
      }
    }

    return false;
  }

  ContextualFailure failure(CS, CS.getType(E), contextualType,
                            CS.getConstraintLocator(E));
  if (failure.diagnoseConversionToDictionary())
    return true;

  // If that didn't turn up an issue, then we don't know what to do.
  // TODO: When a contextual type is missing, we could try to diagnose cases
  // where the element types mismatch... but theoretically they should type
  // unify to Any, so that could never happen?
  return false;
}

bool FailureDiagnosis::visitDictionaryExpr(DictionaryExpr *E) {
  Type contextualKeyType, contextualValueType;
  auto keyTypePurpose = CTP_Unused, valueTypePurpose = CTP_Unused;

  // If we had a contextual type, then it either conforms to
  // ExpressibleByDictionaryLiteral or it is an invalid contextual type.
  if (auto contextualType = CS.getContextualType()) {
    // If our contextual type is an optional, look through them, because we're
    // surely initializing whatever is inside.
    contextualType = contextualType->lookThroughAllOptionalTypes();

    auto DLC = TypeChecker::getProtocol(
        CS.getASTContext(), E->getLoc(),
        KnownProtocolKind::ExpressibleByDictionaryLiteral);
    if (!DLC) return visitExpr(E);

    // Validate the contextual type conforms to ExpressibleByDictionaryLiteral
    // and figure out what the contextual Key/Value types are in place.
    auto Conformance = TypeChecker::conformsToProtocol(
        contextualType, DLC, CS.DC, ConformanceCheckFlags::InExpression);
    if (Conformance.isInvalid()) {
      diagnose(E->getStartLoc(), diag::type_is_not_dictionary, contextualType)
        .highlight(E->getSourceRange());
      return true;
    }

    contextualKeyType =
        Conformance
            .getTypeWitnessByName(contextualType, CS.getASTContext().Id_Key)
            ->getDesugaredType();

    contextualValueType =
        Conformance
            .getTypeWitnessByName(contextualType, CS.getASTContext().Id_Value)
            ->getDesugaredType();

    assert(contextualKeyType && contextualValueType &&
           "Could not find Key/Value DictionaryLiteral associated types from"
           " contextual type conformance");
    
    keyTypePurpose = CTP_DictionaryKey;
    valueTypePurpose = CTP_DictionaryValue;
  }
  
  // Type check each of the subexpressions in place, passing down the contextual
  // type information if we have it.
  for (auto elt : E->getElements()) {
    auto TE = dyn_cast<TupleExpr>(elt);
    if (!TE || TE->getNumElements() != 2) continue;

    if (!typeCheckChildIndependently(TE->getElement(0),
                                     contextualKeyType, keyTypePurpose))
      return true;
    if (!typeCheckChildIndependently(TE->getElement(1),
                                     contextualValueType, valueTypePurpose))
      return true;
  }

  // If that didn't turn up an issue, then we don't know what to do.
  // TODO: When a contextual type is missing, we could try to diagnose cases
  // where the element types mismatch.  There is no Any equivalent since they
  // keys need to be hashable.
  return false;
}

/// When an object literal fails to typecheck because its protocol's
/// corresponding default type has not been set in the global namespace (e.g.
/// _ColorLiteralType), suggest that the user import the appropriate module for
/// the target.
bool FailureDiagnosis::visitObjectLiteralExpr(ObjectLiteralExpr *E) {
  // Type check the argument first.
  auto protocol = TypeChecker::getLiteralProtocol(CS.getASTContext(), E);
  if (!protocol)
    return false;
  auto constrName =
      TypeChecker::getObjectLiteralConstructorName(CS.getASTContext(), E);
  assert(constrName);
  auto *constr = dyn_cast_or_null<ConstructorDecl>(
      protocol->getSingleRequirement(constrName));
  if (!constr)
    return false;
  auto paramType = TypeChecker::getObjectLiteralParameterType(E, constr);
  if (!typeCheckChildIndependently(
        E->getArg(), paramType, CTP_CallArgument))
    return true;

  // Conditions for showing this diagnostic:
  // * The object literal protocol's default type is unimplemented
  if (TypeChecker::getDefaultType(protocol, CS.DC))
    return false;
  // * The object literal has no contextual type
  if (CS.getContextualType())
    return false;

  // Figure out what import to suggest.
  auto &Ctx = CS.getASTContext();
  const auto &target = Ctx.LangOpts.Target;
  StringRef importModule;
  StringRef importDefaultTypeName;
  if (protocol == Ctx.getProtocol(KnownProtocolKind::ExpressibleByColorLiteral)) {
    if (target.isMacOSX()) {
      importModule = "AppKit";
      importDefaultTypeName = "NSColor";
    } else if (target.isiOS() || target.isTvOS()) {
      importModule = "UIKit";
      importDefaultTypeName = "UIColor";
    }
  } else if (protocol == Ctx.getProtocol(
               KnownProtocolKind::ExpressibleByImageLiteral)) {
    if (target.isMacOSX()) {
      importModule = "AppKit";
      importDefaultTypeName = "NSImage";
    } else if (target.isiOS() || target.isTvOS()) {
      importModule = "UIKit";
      importDefaultTypeName = "UIImage";
    }
  } else if (protocol == Ctx.getProtocol( 
               KnownProtocolKind::ExpressibleByFileReferenceLiteral)) {
    importModule = "Foundation";
    importDefaultTypeName = "URL";
  }

  // Emit the diagnostic.
  const auto plainName = E->getLiteralKindPlainName();
  Ctx.Diags.diagnose(E->getLoc(), diag::object_literal_default_type_missing,
                     plainName);
  if (!importModule.empty()) {
    Ctx.Diags.diagnose(E->getLoc(), diag::object_literal_resolve_import,
                       importModule, importDefaultTypeName, plainName);
  }
  return true;
}

bool FailureDiagnosis::visitUnresolvedMemberExpr(UnresolvedMemberExpr *E) {
  // If we have no contextual type, there is no way to resolve this.  Just
  // diagnose this as an ambiguity.
  if (!CS.getContextualType())
    return false;

  // OTOH, if we do have a contextual type, we can provide a more specific
  // error.  Dig out the UnresolvedValueMember constraint for this expr node.
  Constraint *memberConstraint = nullptr;
  auto checkConstraint = [&](Constraint *C) {
    if (C->getKind() == ConstraintKind::UnresolvedValueMember &&
        simplifyLocatorToAnchor(C->getLocator()) == E)
      memberConstraint = C;
  };

  if (CS.failedConstraint)
    checkConstraint(CS.failedConstraint);
  for (auto &C : CS.getConstraints()) {
    if (memberConstraint) break;
    checkConstraint(&C);
  }
  
  // If we can't find the member constraint in question, then we failed.
  if (!memberConstraint)
    return false;

  std::function<bool(ArrayRef<OverloadChoice>)> callback = [&](
      ArrayRef<OverloadChoice> candidates) {
    bool hasTrailingClosure = callArgHasTrailingClosure(E->getArgument());

    // Dump all of our viable candidates into a CalleeCandidateInfo & sort it
    // out.
    CalleeCandidateInfo candidateInfo(Type(), candidates, hasTrailingClosure,
                                      CS);

    // Filter the candidate list based on the argument we may or may not have.
    candidateInfo.filterContextualMemberList(E->getArgument());

    // If we have multiple candidates, then we have an ambiguity.
    if (candidateInfo.size() != 1) {
      SourceRange argRange;
      if (auto arg = E->getArgument())
        argRange = arg->getSourceRange();
      diagnose(E->getNameLoc(), diag::ambiguous_member_overload_set,
               E->getName())
          .highlight(argRange);
      candidateInfo.suggestPotentialOverloads(E->getNameLoc().getBaseNameLoc());
      return true;
    }

    return false;
  };

  return diagnoseMemberFailures(E, nullptr, memberConstraint->getKind(),
                                memberConstraint->getMember(),
                                memberConstraint->getFunctionRefKind(),
                                memberConstraint->getLocator(), callback);
}

bool FailureDiagnosis::diagnoseMemberFailures(
    Expr *E, Expr *baseExpr, ConstraintKind lookupKind, DeclName memberName,
    FunctionRefKind funcRefKind, ConstraintLocator *locator,
    Optional<std::function<bool(ArrayRef<OverloadChoice>)>> callback,
    bool includeInaccessibleMembers) {
  auto isInitializer = memberName.isSimpleName(DeclBaseName::createConstructor());

  // Get the referenced base expression from the failed constraint, along with
  // the SourceRange for the member ref.  In "x.y", this returns the expr for x
  // and the source range for y.
  SourceRange memberRange;
  SourceLoc BaseLoc;
  DeclNameLoc NameLoc;

  Type baseTy, baseObjTy;
  // UnresolvedMemberExpr doesn't have "base" expression,
  // it's represented as ".foo", which means that we need
  // to get base from the context.
  if (auto *UME = dyn_cast<UnresolvedMemberExpr>(E)) {
    memberRange = E->getSourceRange();
    BaseLoc = E->getLoc();
    NameLoc = UME->getNameLoc();
    baseTy = CS.getContextualType();
    if (!baseTy)
      return false;

    // If we succeeded, get ready to do the member lookup.
    baseObjTy = baseTy->getRValueType();

    // If the base object is already a metatype type, then something weird is
    // going on.  For now, just generate a generic error.
    if (baseObjTy->is<MetatypeType>())
      return false;

    baseTy = baseObjTy = MetatypeType::get(baseObjTy);
  } else {
    memberRange = baseExpr->getSourceRange();
    if (locator)
      locator = simplifyLocator(CS, locator, memberRange);

    BaseLoc = baseExpr->getLoc();
    NameLoc = DeclNameLoc(memberRange.Start);

    // Retypecheck the anchor type, which is the base of the member expression.
    baseExpr = typeCheckChildIndependently(baseExpr, TCC_AllowLValue);
    if (!baseExpr)
      return true;

    baseTy = CS.getType(baseExpr);
    baseObjTy = baseTy->getWithoutSpecifierType();
  }

  // If the base type is an IUO, look through it.  Odds are, the code is not
  // trying to find a member of it.
  // FIXME: We need to rework this with IUOs out of the type system.
  // if (auto objTy = CS.lookThroughImplicitlyUnwrappedOptionalType(baseObjTy))
  //   baseTy = baseObjTy = objTy;

  // If the base of this property access is a function that takes an empty
  // argument list, then the most likely problem is that the user wanted to
  // call the function, e.g. in "a.b.c" where they had to write "a.b().c".
  // Produce a specific diagnostic + fixit for this situation.
  if (auto baseFTy = baseObjTy->getAs<AnyFunctionType>()) {
    if (baseExpr && baseFTy->getParams().empty()) {
      auto failure =
          MissingCallFailure(CS, CS.getConstraintLocator(baseExpr));
      return failure.diagnoseAsError();
    }
  }

  // If this is a tuple, then the index needs to be valid.
  if (auto tuple = baseObjTy->getAs<TupleType>()) {
    auto baseName = memberName.getBaseName();

    if (!baseName.isSpecial()) {
      StringRef nameStr = baseName.userFacingName();

      int fieldIdx = -1;
      // Resolve a number reference into the tuple type.
      unsigned Value = 0;
      if (!nameStr.getAsInteger(10, Value) && Value < tuple->getNumElements()) {
        fieldIdx = Value;
      } else {
        fieldIdx = tuple->getNamedElementId(memberName.getBaseIdentifier());
      }

      if (fieldIdx != -1)
        return false; // Lookup is valid.
    }

    diagnose(BaseLoc, diag::could_not_find_tuple_member, baseObjTy, memberName)
        .highlight(memberRange);
    return true;
  }

  // If this is initializer/constructor lookup we are dealing this.
  if (isInitializer) {
    // Let's check what is the base type we are trying to look it up on
    // because only MetatypeType is viable to find constructor on, as per
    // rules in ConstraintSystem::performMemberLookup.
    if (!baseTy->is<AnyMetatypeType>()) {
      baseTy = MetatypeType::get(baseTy, CS.getASTContext());
    }
  }

  // If base type has unresolved generic parameters, such might mean
  // that it's initializer with erroneous argument, otherwise this would
  // be a simple ambiguous archetype case, neither can be diagnosed here.
  if (baseTy->hasTypeParameter() && baseTy->hasUnresolvedType())
    return false;

  MemberLookupResult result =
      CS.performMemberLookup(lookupKind, memberName, baseTy, funcRefKind,
                             locator, includeInaccessibleMembers);

  switch (result.OverallResult) {
  case MemberLookupResult::Unsolved:
    // If we couldn't resolve a specific type for the base expression, then we
    // cannot produce a specific diagnostic.
    return false;

  case MemberLookupResult::ErrorAlreadyDiagnosed:
    // If an error was already emitted, then we're done, don't emit anything
    // redundant.
    return true;

  case MemberLookupResult::HasResults:
    break;
  }

  SmallVector<OverloadChoice, 4> viableCandidatesToReport;
  for (auto candidate : result.ViableCandidates)
    if (candidate.getKind() != OverloadChoiceKind::KeyPathApplication)
      viableCandidatesToReport.push_back(candidate);

  // Since the lookup was allowing inaccessible members, let's check
  // if it found anything of that sort, which is easy to diagnose.
  bool allUnavailable =
      !CS.getASTContext().LangOpts.DisableAvailabilityChecking;
  bool allInaccessible = true;
  for (auto &member : viableCandidatesToReport) {
    if (!member.isDecl()) {
      // if there is no declaration, this choice is implicitly available.
      allUnavailable = false;
      continue;
    }

    auto decl = member.getDecl();
    // Check availability of the found choice.
    if (!decl->getAttrs().isUnavailable(CS.getASTContext()))
      allUnavailable = false;

    if (decl->isAccessibleFrom(CS.DC))
      allInaccessible = false;
  }

  // diagnoseSimpleErrors() should have diagnosed this scenario.
  assert(!allInaccessible || viableCandidatesToReport.empty());

  if (result.UnviableCandidates.empty() && isInitializer &&
      !baseObjTy->is<AnyMetatypeType>()) {
    if (auto ctorRef = dyn_cast<UnresolvedDotExpr>(E)) {
      // Diagnose 'super.init', which can only appear inside another
      // initializer, specially.
      if (isa<SuperRefExpr>(ctorRef->getBase())) {
        diagnose(BaseLoc, diag::super_initializer_not_in_initializer);
        return true;
      }

      // Suggest inserting a call to 'type(of:)' to construct another object
      // of the same dynamic type.
      SourceRange fixItRng = ctorRef->getNameLoc().getSourceRange();

      // Surround the caller in `type(of:)`.
      diagnose(BaseLoc, diag::init_not_instance_member)
          .fixItInsert(fixItRng.Start, "type(of: ")
          .fixItInsertAfter(fixItRng.End, ")");
      return true;
    }
  }

  if (viableCandidatesToReport.empty()) {
    // If this was an optional type let's check if the base type
    // has requested member, if so - generate nice error saying that
    // optional was not unwrapped, otherwise say that type value has
    // no such member.
    if (auto *OT = dyn_cast<OptionalType>(baseObjTy.getPointer())) {
      auto optionalResult = CS.performMemberLookup(
          lookupKind, memberName, OT->getBaseType(), funcRefKind, locator,
          /*includeInaccessibleMembers*/ false);

      switch (optionalResult.OverallResult) {
      case MemberLookupResult::ErrorAlreadyDiagnosed:
        // If an error was already emitted, then we're done, don't emit anything
        // redundant.
        return true;

      case MemberLookupResult::Unsolved:
      case MemberLookupResult::HasResults:
        break;
      }

      if (!optionalResult.ViableCandidates.empty()) {
        MemberAccessOnOptionalBaseFailure failure(
            CS, CS.getConstraintLocator(baseExpr), memberName,
            /*resultOptional=*/false);
        return failure.diagnoseAsError();
      }
    }

    // FIXME: Dig out the property DeclNameLoc.
    diagnoseUnviableLookupResults(result, E, baseObjTy, baseExpr, memberName,
                                  NameLoc, BaseLoc);
    return true;
  }

  if (allUnavailable) {
    auto firstDecl = viableCandidatesToReport[0].getDecl();
    // FIXME: We need the enclosing CallExpr to rewrite the argument labels.
    if (diagnoseExplicitUnavailability(firstDecl, BaseLoc, CS.DC,
                                       /*call*/ nullptr))
      return true;
  }

  return callback.hasValue() ? (*callback)(viableCandidatesToReport) : false;
}

bool FailureDiagnosis::visitUnresolvedDotExpr(UnresolvedDotExpr *UDE) {
  auto *baseExpr = UDE->getBase();
  auto *locator = CS.getConstraintLocator(UDE, ConstraintLocator::Member);
  if (!locator)
    return false;

  return diagnoseMemberFailures(UDE, baseExpr, ConstraintKind::ValueMember,
                                UDE->getName(), UDE->getFunctionRefKind(),
                                locator);
}

/// A TupleExpr propagate contextual type information down to its children and
/// can be erroneous when there is a label mismatch etc.
bool FailureDiagnosis::visitTupleExpr(TupleExpr *TE) {
  // If we know the requested argType to use, use computeTupleShuffle to produce
  // the shuffle of input arguments to destination values.  It requires a
  // TupleType to compute the mapping from argExpr.  Conveniently, it doesn't
  // care about the actual types though, so we can just use 'void' for them.
  if (!CS.getContextualType() || !CS.getContextualType()->is<TupleType>())
    return visitExpr(TE);

  auto contextualTT = CS.getContextualType()->castTo<TupleType>();

  SmallVector<TupleTypeElt, 4> ArgElts;
  auto voidTy = CS.getASTContext().TheEmptyTupleType;

  for (unsigned i = 0, e = TE->getNumElements(); i != e; ++i)
    ArgElts.push_back({ voidTy, TE->getElementName(i) });
  auto TEType = TupleType::get(ArgElts, CS.getASTContext());

  if (!TEType->is<TupleType>())
    return visitExpr(TE);

  SmallVector<unsigned, 4> sources;
  
  // If the shuffle is invalid, then there is a type error.  We could diagnose
  // it specifically here, but the general logic does a fine job so we let it
  // do it.
  if (computeTupleShuffle(TEType->castTo<TupleType>()->getElements(),
                          contextualTT->getElements(), sources))
    return visitExpr(TE);

  // If we got a correct shuffle, we can perform the analysis of all of
  // the input elements, with their expected types.
  for (unsigned i = 0, e = sources.size(); i != e; ++i) {
    // Otherwise, it must match the corresponding expected argument type.
    unsigned inArgNo = sources[i];

    TCCOptions options;
    if (contextualTT->getElement(i).isInOut())
      options |= TCC_AllowLValue;

    auto actualType = contextualTT->getElementType(i);
    auto exprResult =
        typeCheckChildIndependently(TE->getElement(inArgNo), actualType,
                                    CS.getContextualTypePurpose(), options);
    // If there was an error type checking this argument, then we're done.
    if (!exprResult) return true;
  }
  
  return false;
}

/// An IdentityExpr doesn't change its argument, but it *can* propagate its
/// contextual type information down.
bool FailureDiagnosis::visitIdentityExpr(IdentityExpr *E) {
  auto contextualType = CS.getContextualType();

  // If we have a paren expr and our contextual type is a ParenType, remove the
  // paren expr sugar.
  if (contextualType)
    contextualType = contextualType->getWithoutParens();
  if (!typeCheckChildIndependently(E->getSubExpr(), contextualType,
                                   CS.getContextualTypePurpose()))
    return true;
  return false;
}

/// A TryExpr doesn't change it's argument, nor does it change the contextual
/// type.
bool FailureDiagnosis::visitTryExpr(TryExpr *E) {
  return visit(E->getSubExpr());
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
void ConstraintSystem::diagnoseFailureForExpr(Expr *expr) {
  setPhase(ConstraintSystemPhase::Diagnostics);

  SWIFT_DEFER { setPhase(ConstraintSystemPhase::Finalization); };

  // Look through RebindSelfInConstructorExpr to avoid weird Sema issues.
  if (auto *RB = dyn_cast<RebindSelfInConstructorExpr>(expr))
    expr = RB->getSubExpr();

  FailureDiagnosis diagnosis(expr, *this);

  // Now, attempt to diagnose the failure from the info we've collected.
  if (diagnosis.diagnoseExprFailure())
    return;

  // If this is a contextual conversion problem, dig out some information.
  if (diagnosis.diagnoseContextualConversionError(expr, getContextualType(),
                                                  getContextualTypePurpose()))
    return;

  // If no one could find a problem with this expression or constraint system,
  // then it must be well-formed... but is ambiguous.  Handle this by diagnostic
  // various cases that come up.
  diagnosis.diagnoseAmbiguity(expr);
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

/// Check the specified closure to see if it is a multi-statement closure with
/// an uninferred type.  If so, diagnose the problem with an error and return
/// true.
bool FailureDiagnosis::
diagnoseAmbiguousMultiStatementClosure(ClosureExpr *closure) {
  if (closure->hasSingleExpressionBody() ||
      closure->hasExplicitResultType())
    return false;

  auto closureType = CS.getType(closure)->getAs<AnyFunctionType>();
  if (!closureType ||
      !(closureType->getResult()->hasUnresolvedType() ||
        closureType->getResult()->hasTypeVariable()))
    return false;

  // Okay, we have a multi-statement closure expr that has no inferred result,
  // type, in the context of a larger expression.  The user probably expected
  // the compiler to infer the result type of the closure from the body of the
  // closure, which Swift doesn't do for multi-statement closures.  Try to be
  // helpful by digging into the body of the closure, looking for a return
  // statement, and inferring the result type from it.  If we can figure that
  // out, we can produce a fixit hint.
  class ReturnStmtFinder : public ASTWalker {
    SmallVectorImpl<ReturnStmt*> &returnStmts;
  public:
    ReturnStmtFinder(SmallVectorImpl<ReturnStmt*> &returnStmts)
      : returnStmts(returnStmts) {}

    // Walk through statements, so we find returns hiding in if/else blocks etc.
    std::pair<bool, Stmt *> walkToStmtPre(Stmt *S) override {
      // Keep track of any return statements we find.
      if (auto RS = dyn_cast<ReturnStmt>(S))
        returnStmts.push_back(RS);
      return { true, S };
    }
    
    // Don't walk into anything else, since they cannot contain statements
    // that can return from the current closure.
    std::pair<bool, Expr *> walkToExprPre(Expr *E) override {
      return { false, E };
    }
    std::pair<bool, Pattern*> walkToPatternPre(Pattern *P) override {
      return { false, P };
    }
    bool walkToDeclPre(Decl *D) override { return false; }
    bool walkToTypeLocPre(TypeLoc &TL) override { return false; }
    bool walkToTypeReprPre(TypeRepr *T) override { return false; }
    bool walkToParameterListPre(ParameterList *PL) override { return false; }
  };
  
  SmallVector<ReturnStmt*, 4> Returns;
  closure->getBody()->walk(ReturnStmtFinder(Returns));
  
  // If we found a return statement inside of the closure expression, then go
  // ahead and type check the body to see if we can determine a type.
  for (auto RS : Returns) {
    llvm::SaveAndRestore<DeclContext *> SavedDC(CS.DC, closure);

    // Otherwise, we're ok to type check the subexpr.
    Type resultType;
    if (RS->hasResult()) {
      auto resultExpr = RS->getResult();
      ConcreteDeclRef decl = nullptr;

      // If return expression uses closure parameters, which have/are
      // type variables, such means that we won't be able to
      // type-check result correctly and, unfortunately,
      // we are going to leak type variables from the parent
      // constraint system through declaration types.
      bool hasUnresolvedParams = false;
      resultExpr->forEachChildExpr([&](Expr *childExpr) -> Expr *{
        if (auto DRE = dyn_cast<DeclRefExpr>(childExpr)) {
          if (auto param = dyn_cast<ParamDecl>(DRE->getDecl())) {
            auto paramType =
                param->hasInterfaceType() ? param->getType() : Type();
            if (!paramType || paramType->hasTypeVariable()) {
              hasUnresolvedParams = true;
              return nullptr;
            }
          }
        }
        return childExpr;
      });

      if (hasUnresolvedParams)
        continue;

      ConstraintSystem::preCheckExpression(resultExpr, CS.DC, &CS);

      // Obtain type of the result expression without applying solutions,
      // because otherwise this might result in leaking of type variables,
      // since we are not resetting result statement and if expression is
      // successfully type-checked its type cleanup is going to be disabled
      // (we are allowing unresolved types), and as a side-effect it might
      // also be transformed e.g. OverloadedDeclRefExpr -> DeclRefExpr.
      auto type = TypeChecker::getTypeOfExpressionWithoutApplying(
          resultExpr, CS.DC, decl, FreeTypeVariableBinding::UnresolvedType);
      if (type)
        resultType = type->getRValueType();
    }
    
    // If we found a type, presuppose it was the intended result and insert a
    // fixit hint.
    if (resultType && !isUnresolvedOrTypeVarType(resultType)) {
      // If there is a location for an 'in' token, then the argument list was
      // specified somehow but no return type was.  Insert a "-> ReturnType "
      // before the in token.
      if (closure->getInLoc().isValid()) {
        diagnose(closure->getLoc(), diag::cannot_infer_closure_result_type)
            .fixItInsert(closure->getInLoc(), diag::insert_closure_return_type,
                         resultType, /*argListSpecified*/ false);
        return true;
      }
      
      // Otherwise, the closure must take zero arguments.  We know this
      // because the if one or more argument is specified, a multi-statement
      // closure *must* name them, or explicitly ignore them with "_ in".
      //
      // As such, we insert " () -> ReturnType in " right after the '{' that
      // starts the closure body.
      diagnose(closure->getLoc(), diag::cannot_infer_closure_result_type)
          .fixItInsertAfter(closure->getBody()->getLBraceLoc(),
                            diag::insert_closure_return_type, resultType,
                            /*argListSpecified*/ true);
      return true;
    }
  }
  
  diagnose(closure->getLoc(), diag::cannot_infer_closure_result_type);
  return true;
}

/// Emit an ambiguity diagnostic about the specified expression.
void FailureDiagnosis::diagnoseAmbiguity(Expr *E) {
  if (auto *assignment = dyn_cast<AssignExpr>(E)) {
    if (isa<DiscardAssignmentExpr>(assignment->getDest())) {
      auto *srcExpr = assignment->getSrc();

      bool diagnosedInvalidUseOfDiscardExpr = false;
      srcExpr->forEachChildExpr([&](Expr *expr) -> Expr * {
        if (auto *DAE = dyn_cast<DiscardAssignmentExpr>(expr)) {
          diagnose(DAE->getLoc(), diag::discard_expr_outside_of_assignment)
              .highlight(srcExpr->getSourceRange());
          diagnosedInvalidUseOfDiscardExpr = true;
          return nullptr;
        }

        return expr;
      });

      if (diagnosedInvalidUseOfDiscardExpr)
        return;

      diagnoseAmbiguity(srcExpr);
      return;
    }
  }

  // Unresolved/Anonymous ClosureExprs are common enough that we should give
  // them tailored diagnostics.
  if (auto CE = dyn_cast<ClosureExpr>(E->getValueProvidingExpr())) {
    // If this is a multi-statement closure with no explicit result type, emit
    // a note to clue the developer in.
    if (diagnoseAmbiguousMultiStatementClosure(CE))
      return;

    diagnose(E->getLoc(), diag::cannot_infer_closure_type)
      .highlight(E->getSourceRange());
    return;
  }

  // A DiscardAssignmentExpr (spelled "_") needs contextual type information to
  // infer its type. If we see one at top level, diagnose that it must be part
  // of an assignment so we don't get a generic "expression is ambiguous" error.
  if (isa<DiscardAssignmentExpr>(E)) {
    diagnose(E->getLoc(), diag::discard_expr_outside_of_assignment)
      .highlight(E->getSourceRange());
    return;
  }
  
  // Diagnose ".foo" expressions that lack context specifically.
  if (auto UME =
        dyn_cast<UnresolvedMemberExpr>(E->getSemanticsProvidingExpr())) {
    if (!CS.getContextualType()) {
      diagnose(E->getLoc(), diag::unresolved_member_no_inference,UME->getName())
        .highlight(SourceRange(UME->getDotLoc(),
                               UME->getNameLoc().getSourceRange().End));
      return;
    }
  }

  // Diagnose empty collection literals that lack context specifically.
  if (auto CE = dyn_cast<CollectionExpr>(E->getSemanticsProvidingExpr())) {
    if (CE->getNumElements() == 0) {
      diagnose(E->getLoc(), diag::unresolved_collection_literal)
        .highlight(E->getSourceRange());
      return;
    }
  }

  // Diagnose 'nil' without a contextual type.
  if (isa<NilLiteralExpr>(E->getSemanticsProvidingExpr())) {
    diagnose(E->getLoc(), diag::unresolved_nil_literal)
      .highlight(E->getSourceRange());
    return;
  }

  // A very common cause of this diagnostic is a situation where a closure expr
  // has no inferred type, due to being a multiline closure.  Check to see if
  // this is the case and (if so), speculatively diagnose that as the problem.
  bool didDiagnose = false;
  E->forEachChildExpr([&](Expr *subExpr) -> Expr*{
    auto closure = dyn_cast<ClosureExpr>(subExpr);
    if (!didDiagnose && closure)
      didDiagnose = diagnoseAmbiguousMultiStatementClosure(closure);
    
    return subExpr;
  });
  
  if (didDiagnose) return;
  

  
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

/// If an UnresolvedDotExpr, SubscriptMember, etc has been resolved by the
/// constraint system, return the decl that it references.
ValueDecl *ConstraintSystem::findResolvedMemberRef(ConstraintLocator *locator) {
  // Search through the resolvedOverloadSets to see if we have a resolution for
  // this member.  This is an O(n) search, but only happens when producing an
  // error diagnostic.
  auto *overload = findSelectedOverloadFor(locator);
  if (!overload)
    return nullptr;

  // We only want to handle the simplest decl binding.
  auto choice = overload->Choice;
  if (choice.getKind() != OverloadChoiceKind::Decl)
    return nullptr;

  return choice.getDecl();
}
