//===--- CSGen.cpp - Constraint Generator ---------------------------------===//
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
// This file implements constraint generation for the type checker.
//
//===----------------------------------------------------------------------===//
#include "ConstraintGraph.h"
#include "ConstraintSystem.h"
#include "TypeCheckType.h"
#include "swift/AST/ASTVisitor.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/Expr.h"
#include "swift/AST/GenericSignature.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/PrettyStackTrace.h"
#include "swift/AST/SubstitutionMap.h"
#include "swift/Sema/IDETypeChecking.h"
#include "swift/Subsystems.h"
#include "llvm/ADT/APInt.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/ADT/StringSwitch.h"
#include <utility>

using namespace swift;
using namespace swift::constraints;

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

static bool isArithmeticOperatorDecl(ValueDecl *vd) {
  return vd && 
  (vd->getBaseName() == "+" ||
   vd->getBaseName() == "-" ||
   vd->getBaseName() == "*" ||
   vd->getBaseName() == "/" ||
   vd->getBaseName() == "%");
}

static bool mergeRepresentativeEquivalenceClasses(ConstraintSystem &CS,
                                                  TypeVariableType* tyvar1,
                                                  TypeVariableType* tyvar2) {
  if (tyvar1 && tyvar2) {
    auto rep1 = CS.getRepresentative(tyvar1);
    auto rep2 = CS.getRepresentative(tyvar2);

    if (rep1 != rep2) {
      auto fixedType2 = CS.getFixedType(rep2);

      // If the there exists fixed type associated with the second
      // type variable, and we simply merge two types together it would
      // mean that portion of the constraint graph previously associated
      // with that (second) variable is going to be disconnected from its
      // new equivalence class, which is going to lead to incorrect solutions,
      // so we need to make sure to re-bind fixed to the new representative.
      if (fixedType2) {
        CS.addConstraint(ConstraintKind::Bind, fixedType2, rep1,
                         rep1->getImpl().getLocator());
      }

      CS.mergeEquivalenceClasses(rep1, rep2, /*updateWorkList*/ false);
      return true;
    }
  }

  return false;
}

namespace {
  
  /// Internal struct for tracking information about types within a series
  /// of "linked" expressions. (Such as a chain of binary operator invocations.)
  struct LinkedTypeInfo {
    unsigned haveIntLiteral : 1;
    unsigned haveFloatLiteral : 1;
    unsigned haveStringLiteral : 1;

    llvm::SmallSet<TypeBase*, 16> collectedTypes;

    llvm::SmallVector<TypeVariableType *, 16> intLiteralTyvars;
    llvm::SmallVector<TypeVariableType *, 16> floatLiteralTyvars;
    llvm::SmallVector<TypeVariableType *, 16> stringLiteralTyvars;

    llvm::SmallVector<BinaryExpr *, 4> binaryExprs;

    // TODO: manage as a set of lists, to speed up addition of binding
    // constraints.
    llvm::SmallVector<DeclRefExpr *, 16> anonClosureParams;
    
    LinkedTypeInfo() {
      haveIntLiteral = false;
      haveFloatLiteral = false;
      haveStringLiteral = false;
    }

    bool hasLiteral() {
      return haveIntLiteral || haveFloatLiteral || haveStringLiteral;
    }
  };

  /// Walks an expression sub-tree, and collects information about expressions
  /// whose types are mutually dependent upon one another.
  class LinkedExprCollector : public ASTWalker {
    
    llvm::SmallVectorImpl<Expr*> &LinkedExprs;
    ConstraintSystem &CS;

  public:
    LinkedExprCollector(llvm::SmallVectorImpl<Expr *> &linkedExprs,
                        ConstraintSystem &cs)
        : LinkedExprs(linkedExprs), CS(cs) {}

    std::pair<bool, Expr *> walkToExprPre(Expr *expr) override {

      if (CS.shouldReusePrecheckedType() &&
          !CS.getType(expr)->hasTypeVariable()) {
        return { false, expr };
      }

      // Store top-level binary exprs for further analysis.
      if (isa<BinaryExpr>(expr) ||
          
          // Literal exprs are contextually typed, so store them off as well.
          isa<LiteralExpr>(expr) ||

          // We'd like to take a look at implicit closure params, so store
          // them.
          isa<ClosureExpr>(expr) ||

          // We'd like to look at the elements of arrays and dictionaries.
          isa<ArrayExpr>(expr) ||
          isa<DictionaryExpr>(expr) ||

          // assignment expression can involve anonymous closure parameters
          // as source and destination, so it's beneficial for diagnostics if
          // we look at the assignment.
          isa<AssignExpr>(expr)) {
        LinkedExprs.push_back(expr);
        return {false, expr};
      }
      
      return { true, expr };
    }
    
    Expr *walkToExprPost(Expr *expr) override {
      return expr;
    }
    
    /// Ignore statements.
    std::pair<bool, Stmt *> walkToStmtPre(Stmt *stmt) override {
      return { false, stmt };
    }
    
    /// Ignore declarations.
    bool walkToDeclPre(Decl *decl) override { return false; }

    /// Ignore patterns.
    std::pair<bool, Pattern*> walkToPatternPre(Pattern *pat) override {
      return { false, pat };
    }

    /// Ignore types.
    bool walkToTypeLocPre(TypeLoc &TL) override { return false; }
  };
  
  /// Given a collection of "linked" expressions, analyzes them for
  /// commonalities regarding their types. This will help us compute a
  /// "best common type" from the expression types.
  class LinkedExprAnalyzer : public ASTWalker {
    
    LinkedTypeInfo &LTI;
    ConstraintSystem &CS;
    
  public:
    
    LinkedExprAnalyzer(LinkedTypeInfo &lti, ConstraintSystem &cs) :
        LTI(lti), CS(cs) {}
    
    std::pair<bool, Expr *> walkToExprPre(Expr *expr) override {

      if (CS.shouldReusePrecheckedType() &&
          !CS.getType(expr)->hasTypeVariable()) {
        return { false, expr };
      }
      
      if (isa<IntegerLiteralExpr>(expr)) {
        LTI.haveIntLiteral = true;
        auto tyvar = CS.getType(expr)->getAs<TypeVariableType>();

        if (tyvar) {
          LTI.intLiteralTyvars.push_back(tyvar);
        }
        
        return { false, expr };
      }
      
      if (isa<FloatLiteralExpr>(expr)) {
        LTI.haveFloatLiteral = true;
        auto tyvar = CS.getType(expr)->getAs<TypeVariableType>();

        if (tyvar) {
          LTI.floatLiteralTyvars.push_back(tyvar);
        }

        return { false, expr };
      }
      
      if (isa<StringLiteralExpr>(expr)) {
        LTI.haveStringLiteral = true;

        auto tyvar = CS.getType(expr)->getAs<TypeVariableType>();

        if (tyvar) {
          LTI.stringLiteralTyvars.push_back(tyvar);
        }

        return { false, expr };
      }

      if (isa<CollectionExpr>(expr)) {
        return { true, expr };
      }
      
      if (auto UDE = dyn_cast<UnresolvedDotExpr>(expr)) {
        
        if (CS.hasType(UDE))
          LTI.collectedTypes.insert(CS.getType(UDE).getPointer());
        
        // Don't recurse into the base expression.
        return { false, expr };
      }


      if (isa<ClosureExpr>(expr)) {
        return { true, expr };
      }

      if (auto FVE = dyn_cast<ForceValueExpr>(expr)) {
        LTI.collectedTypes.insert(CS.getType(FVE).getPointer());
        return { false, expr };
      }

      if (auto DRE = dyn_cast<DeclRefExpr>(expr)) {
        if (auto varDecl = dyn_cast<VarDecl>(DRE->getDecl())) {
          if (isa<ParamDecl>(varDecl) &&
              cast<ParamDecl>(varDecl)->isAnonClosureParam()) {
            LTI.anonClosureParams.push_back(DRE);
          } else if (CS.hasType(DRE)) {
            LTI.collectedTypes.insert(CS.getType(DRE).getPointer());
          }
          return { false, expr };
        } 
      }             

      // In the case of a function application, we would have already captured
      // the return type during constraint generation, so there's no use in
      // looking any further.
      if (isa<ApplyExpr>(expr) &&
          !(isa<BinaryExpr>(expr) || isa<PrefixUnaryExpr>(expr) ||
            isa<PostfixUnaryExpr>(expr))) {      
        return { false, expr };
      }

      if (isa<BinaryExpr>(expr)) {
        LTI.binaryExprs.push_back(dyn_cast<BinaryExpr>(expr));
      }  
      
      if (auto favoredType = CS.getFavoredType(expr)) {
        LTI.collectedTypes.insert(favoredType);

        return { false, expr };
      }

      // Optimize branches of a conditional expression separately.
      if (auto IE = dyn_cast<IfExpr>(expr)) {
        CS.optimizeConstraints(IE->getCondExpr());
        CS.optimizeConstraints(IE->getThenExpr());
        CS.optimizeConstraints(IE->getElseExpr());
        return { false, expr };
      }      

      // For exprs of a structural type that are not modeling argument lists,
      // avoid merging the type variables. (We need to allow for cases like
      // (Int, Int32).)
      if (isa<TupleExpr>(expr) && !isa<ApplyExpr>(Parent.getAsExpr())) {
        return { false, expr };
      }

      // Coercion exprs have a rigid type, so there's no use in gathering info
      // about them.
      if (auto *coercion = dyn_cast<CoerceExpr>(expr)) {
        // Let's not collect information about types initialized by
        // coercions just like we don't for regular initializer calls,
        // because that might lead to overly eager type variable merging.
        if (!coercion->isLiteralInit())
          LTI.collectedTypes.insert(CS.getType(expr).getPointer());
        return { false, expr };
      }

      // Don't walk into subscript expressions - to do so would risk factoring
      // the index expression into edge contraction. (We don't want to do this
      // if the index expression is a literal type that differs from the return
      // type of the subscript operation.)
      if (isa<SubscriptExpr>(expr) || isa<DynamicLookupExpr>(expr)) {
        return { false, expr };
      }
      
      // Don't walk into unresolved member expressions - we avoid merging type
      // variables inside UnresolvedMemberExpr and those outside, since they
      // should be allowed to behave independently in CS.
      if (isa<UnresolvedMemberExpr>(expr)) {
        return {false, expr };
      }

      return { true, expr };
    }
    
    /// Ignore statements.
    std::pair<bool, Stmt *> walkToStmtPre(Stmt *stmt) override {
      return { false, stmt };
    }
    
    /// Ignore declarations.
    bool walkToDeclPre(Decl *decl) override { return false; }

    /// Ignore patterns.
    std::pair<bool, Pattern*> walkToPatternPre(Pattern *pat) override {
      return { false, pat };
    }

    /// Ignore types.
    bool walkToTypeLocPre(TypeLoc &TL) override { return false; }
  };
  
  /// For a given expression, given information that is global to the
  /// expression, attempt to derive a favored type for it.
  bool computeFavoredTypeForExpr(Expr *expr, ConstraintSystem &CS) {
    LinkedTypeInfo lti;

    expr->walk(LinkedExprAnalyzer(lti, CS));

    // Link anonymous closure params of the same index.
    // TODO: As stated above, we should bucket these whilst collecting the
    // exprs to avoid quadratic behavior.
    for (auto acp1 : lti.anonClosureParams) {
      for (auto acp2 : lti.anonClosureParams) {
        if (acp1 == acp2)
          continue;

        if (acp1->getDecl()->getBaseName() == acp2->getDecl()->getBaseName()) {

          auto tyvar1 = CS.getType(acp1)->getAs<TypeVariableType>();
          auto tyvar2 = CS.getType(acp2)->getAs<TypeVariableType>();

          mergeRepresentativeEquivalenceClasses(CS, tyvar1, tyvar2);
        }
      }
    }

    auto mergeTypeVariables = [&](ArrayRef<TypeVariableType *> typeVars) {
      if (typeVars.size() < 2)
        return;

      auto rep1 = CS.getRepresentative(typeVars.front());
      for (unsigned i = 1, n = typeVars.size(); i != n; ++i) {
        auto rep2 = CS.getRepresentative(typeVars[i]);
        if (rep1 != rep2)
          CS.mergeEquivalenceClasses(rep1, rep2, /*updateWorkList*/ false);
      }
    };

    mergeTypeVariables(lti.intLiteralTyvars);
    mergeTypeVariables(lti.floatLiteralTyvars);
    mergeTypeVariables(lti.stringLiteralTyvars);

    if (lti.collectedTypes.size() == 1) {
      // TODO: Compute the BCT.

      // It's only useful to favor the type instead of
      // binding it directly to arguments/result types,
      // which means in case it has been miscalculated
      // solver can still make progress.
      auto favoredTy = (*lti.collectedTypes.begin())->getWithoutSpecifierType();
      CS.setFavoredType(expr, favoredTy.getPointer());

      // If we have a chain of identical binop expressions with homogeneous
      // argument types, we can directly simplify the associated constraint
      // graph.
      auto simplifyBinOpExprTyVars = [&]() {
        // Don't attempt to do linking if there are
        // literals intermingled with other inferred types.
        if (lti.hasLiteral())
          return;

        for (auto binExp1 : lti.binaryExprs) {
          for (auto binExp2 : lti.binaryExprs) {
            if (binExp1 == binExp2)
              continue;

            auto fnTy1 = CS.getType(binExp1)->getAs<TypeVariableType>();
            auto fnTy2 = CS.getType(binExp2)->getAs<TypeVariableType>();

            if (!(fnTy1 && fnTy2))
              return;

            auto ODR1 = dyn_cast<OverloadedDeclRefExpr>(binExp1->getFn());
            auto ODR2 = dyn_cast<OverloadedDeclRefExpr>(binExp2->getFn());

            if (!(ODR1 && ODR2))
              return;

            // TODO: We currently limit this optimization to known arithmetic
            // operators, but we should be able to broaden this out to
            // logical operators as well.
            if (!isArithmeticOperatorDecl(ODR1->getDecls()[0]))
              return;

            if (ODR1->getDecls()[0]->getBaseName() !=
                ODR2->getDecls()[0]->getBaseName())
              return;

            // All things equal, we can merge the tyvars for the function
            // types.
            auto rep1 = CS.getRepresentative(fnTy1);
            auto rep2 = CS.getRepresentative(fnTy2);

            if (rep1 != rep2) {
              CS.mergeEquivalenceClasses(rep1, rep2,
                                         /*updateWorkList*/ false);
            }

            auto odTy1 = CS.getType(ODR1)->getAs<TypeVariableType>();
            auto odTy2 = CS.getType(ODR2)->getAs<TypeVariableType>();

            if (odTy1 && odTy2) {
              auto odRep1 = CS.getRepresentative(odTy1);
              auto odRep2 = CS.getRepresentative(odTy2);

              // Since we'll be choosing the same overload, we can merge
              // the overload tyvar as well.
              if (odRep1 != odRep2)
                CS.mergeEquivalenceClasses(odRep1, odRep2,
                                           /*updateWorkList*/ false);
            }
          }
        }
      };

      simplifyBinOpExprTyVars();       

      return true;
    }    
    
    if (lti.haveFloatLiteral) {
      if (auto floatProto = CS.getASTContext().getProtocol(
              KnownProtocolKind::ExpressibleByFloatLiteral)) {
        if (auto defaultType = TypeChecker::getDefaultType(floatProto, CS.DC)) {
          if (!CS.getFavoredType(expr)) {
            CS.setFavoredType(expr, defaultType.getPointer());
          }
          return true;
        }
      }
    }
    
    if (lti.haveIntLiteral) {
      if (auto intProto = CS.getASTContext().getProtocol(
              KnownProtocolKind::ExpressibleByIntegerLiteral)) {
        if (auto defaultType = TypeChecker::getDefaultType(intProto, CS.DC)) {
          if (!CS.getFavoredType(expr)) {
            CS.setFavoredType(expr, defaultType.getPointer());
          }
          return true;
        }
      }
    }
    
    if (lti.haveStringLiteral) {
      if (auto stringProto = CS.getASTContext().getProtocol(
              KnownProtocolKind::ExpressibleByStringLiteral)) {
        if (auto defTy = TypeChecker::getDefaultType(stringProto, CS.DC)) {
          if (!CS.getFavoredType(expr)) {
            CS.setFavoredType(expr, defTy.getPointer());
          }
          return true;
        }
      }
    }
    
    return false;
  }
  
  /// Determine whether the given parameter type and argument should be
  /// "favored" because they match exactly.
  bool isFavoredParamAndArg(ConstraintSystem &CS, Type paramTy, Type argTy,
                            Type otherArgTy = Type()) {
    // Determine the argument type.
    argTy = argTy->getWithoutSpecifierType();

    // Do the types match exactly?
    if (paramTy->isEqual(argTy))
      return true;

    llvm::SmallSetVector<ProtocolDecl *, 2> literalProtos;
    if (auto argTypeVar = argTy->getAs<TypeVariableType>()) {
      auto constraints = CS.getConstraintGraph().gatherConstraints(
          argTypeVar, ConstraintGraph::GatheringKind::EquivalenceClass,
          [](Constraint *constraint) {
            return constraint->getKind() == ConstraintKind::LiteralConformsTo;
          });

      for (auto constraint : constraints) {
        literalProtos.insert(constraint->getProtocol());
      }
    }

    // Dig out the second argument type.
    if (otherArgTy)
      otherArgTy = otherArgTy->getWithoutSpecifierType();

    for (auto literalProto : literalProtos) {
      // If there is another, concrete argument, check whether it's type
      // conforms to the literal protocol and test against it directly.
      // This helps to avoid 'widening' the favored type to the default type for
      // the literal.
      if (otherArgTy && otherArgTy->getAnyNominal()) {
        if (otherArgTy->isEqual(paramTy) &&
            TypeChecker::conformsToProtocol(
                otherArgTy, literalProto, CS.DC,
                ConformanceCheckFlags::InExpression)) {
          return true;
        }
      } else if (Type defaultType =
                     TypeChecker::getDefaultType(literalProto, CS.DC)) {
        // If there is a default type for the literal protocol, check whether
        // it is the same as the parameter type.
        // Check whether there is a default type to compare against.
        if (paramTy->isEqual(defaultType))
          return true;
      }
    }

    return false;
  }

  /// Favor certain overloads in a call based on some basic analysis
  /// of the overload set and call arguments.
  ///
  /// \param expr The application.
  /// \param isFavored Determine whether the given overload is favored, passing
  /// it the "effective" overload type when it's being called.
  /// \param mustConsider If provided, a function to detect the presence of
  /// overloads which inhibit any overload from being favored.
  void favorCallOverloads(ApplyExpr *expr,
                          ConstraintSystem &CS,
                          llvm::function_ref<bool(ValueDecl *, Type)> isFavored,
                          std::function<bool(ValueDecl *)>
                              mustConsider = nullptr) {
    // Find the type variable associated with the function, if any.
    auto tyvarType = CS.getType(expr->getFn())->getAs<TypeVariableType>();
    if (!tyvarType || CS.getFixedType(tyvarType))
      return;
    
    // This type variable is only currently associated with the function
    // being applied, and the only constraint attached to it should
    // be the disjunction constraint for the overload group.
    auto disjunction = CS.getUnboundBindOverloadDisjunction(tyvarType);
    if (!disjunction)
      return;
    
    // Find the favored constraints and mark them.
    SmallVector<Constraint *, 4> newlyFavoredConstraints;
    unsigned numFavoredConstraints = 0;
    Constraint *firstFavored = nullptr;
    for (auto constraint : disjunction->getNestedConstraints()) {
      auto *decl = constraint->getOverloadChoice().getDeclOrNull();
      if (!decl)
        continue;

      if (mustConsider && mustConsider(decl)) {
        // Roll back any constraints we favored.
        for (auto favored : newlyFavoredConstraints)
          favored->setFavored(false);

        return;
      }

      Type overloadType =
          CS.getEffectiveOverloadType(constraint->getOverloadChoice(),
                                      /*allowMembers=*/true, CS.DC);
      if (!overloadType)
        continue;

      if (!decl->getAttrs().isUnavailable(CS.getASTContext()) &&
          !decl->getAttrs().hasAttribute<DisfavoredOverloadAttr>() &&
          isFavored(decl, overloadType)) {
        // If we might need to roll back the favored constraints, keep
        // track of those we are favoring.
        if (mustConsider && !constraint->isFavored())
          newlyFavoredConstraints.push_back(constraint);

        constraint->setFavored();
        ++numFavoredConstraints;
        if (!firstFavored)
          firstFavored = constraint;
      }
    }

    // If there was one favored constraint, set the favored type based on its
    // result type.
    if (numFavoredConstraints == 1) {
      auto overloadChoice = firstFavored->getOverloadChoice();
      auto overloadType =
        CS.getEffectiveOverloadType(overloadChoice, /*allowMembers=*/true,
                                    CS.DC);
      auto resultType = overloadType->castTo<AnyFunctionType>()->getResult();
      if (!resultType->hasTypeParameter())
        CS.setFavoredType(expr, resultType.getPointer());
    }
  }
  
  size_t getOperandCount(Type t) {
    size_t nOperands = 0;
    
    if (auto parenTy = dyn_cast<ParenType>(t.getPointer())) {
      if (parenTy->getDesugaredType())
        nOperands = 1;
    } else if (auto tupleTy = t->getAs<TupleType>()) {
      nOperands = tupleTy->getElementTypes().size();
    }
    
    return nOperands;
  }
  
  /// Return a pair, containing the total parameter count of a function, coupled
  /// with the number of non-default parameters.
  std::pair<size_t, size_t> getParamCount(ValueDecl *VD) {
    auto fTy = VD->getInterfaceType()->castTo<AnyFunctionType>();
    
    size_t nOperands = fTy->getParams().size();
    size_t nNoDefault = 0;
    
    if (auto AFD = dyn_cast<AbstractFunctionDecl>(VD)) {
      assert(!AFD->hasImplicitSelfDecl());
      for (auto param : *AFD->getParameters()) {
        if (!param->isDefaultArgument())
          nNoDefault++;
      }
    } else {
      nNoDefault = nOperands;
    }
    
    return { nOperands, nNoDefault };
  }
  
  /// Favor unary operator constraints where we have exact matches
  /// for the operand and contextual type.
  void favorMatchingUnaryOperators(ApplyExpr *expr,
                                   ConstraintSystem &CS) {
    // Determine whether the given declaration is favored.
    auto isFavoredDecl = [&](ValueDecl *value, Type type) -> bool {
      auto fnTy = type->getAs<AnyFunctionType>();
      if (!fnTy)
        return false;
      
      Type paramTy = FunctionType::composeInput(CS.getASTContext(),
                                                fnTy->getParams(), false);
      auto resultTy = fnTy->getResult();
      auto contextualTy = CS.getContextualType(expr);

      return isFavoredParamAndArg(
                 CS, paramTy,
                 CS.getType(expr->getArg())->getWithoutParens()) &&
             (!contextualTy || contextualTy->isEqual(resultTy));
    };
    
    favorCallOverloads(expr, CS, isFavoredDecl);
  }
  
  void favorMatchingOverloadExprs(ApplyExpr *expr,
                                  ConstraintSystem &CS) {
    // Find the argument type.
    size_t nArgs = getOperandCount(CS.getType(expr->getArg()));
    auto fnExpr = expr->getFn();
    
    // Check to ensure that we have an OverloadedDeclRef, and that we're not
    // favoring multiple overload constraints. (Otherwise, in this case
    // favoring is useless.
    if (auto ODR = dyn_cast<OverloadedDeclRefExpr>(fnExpr)) {
      bool haveMultipleApplicableOverloads = false;
      
      for (auto VD : ODR->getDecls()) {
        if (VD->getInterfaceType()->is<AnyFunctionType>()) {
          auto nParams = getParamCount(VD);
          
          if (nArgs == nParams.first) {
            if (haveMultipleApplicableOverloads) {
              return;
            } else {
              haveMultipleApplicableOverloads = true;
            }
          }
        }
      }
      
      // Determine whether the given declaration is favored.
      auto isFavoredDecl = [&](ValueDecl *value, Type type) -> bool {
        if (!type->is<AnyFunctionType>())
          return false;

        auto paramCount = getParamCount(value);
        
        return nArgs == paramCount.first ||
               nArgs == paramCount.second;
      };
      
      favorCallOverloads(expr, CS, isFavoredDecl);
      
    }
    
    if (auto favoredTy = CS.getFavoredType(expr->getArg())) {
      // Determine whether the given declaration is favored.
      auto isFavoredDecl = [&](ValueDecl *value, Type type) -> bool {
        auto fnTy = type->getAs<AnyFunctionType>();
        if (!fnTy)
          return false;

        auto paramTy =
            AnyFunctionType::composeInput(CS.getASTContext(), fnTy->getParams(),
                                          /*canonicalVararg*/ false);
        return favoredTy->isEqual(paramTy);
      };

      // This is a hack to ensure we always consider the protocol requirement
      // itself when calling something that has a default implementation in an
      // extension. Otherwise, the extension method might be favored if we're
      // inside an extension context, since any archetypes in the parameter
      // list could match exactly.
      auto mustConsider = [&](ValueDecl *value) -> bool {
        return isa<ProtocolDecl>(value->getDeclContext());
      };

      favorCallOverloads(expr, CS,
                         isFavoredDecl,
                         mustConsider);
    }
  }
  
  /// Favor binary operator constraints where we have exact matches
  /// for the operands and contextual type.
  void favorMatchingBinaryOperators(ApplyExpr *expr,
                                    ConstraintSystem &CS) {
    // If we're generating constraints for a binary operator application,
    // there are two special situations to consider:
    //  1. If the type checker has any newly created functions with the
    //     operator's name. If it does, the overloads were created after the
    //     associated overloaded id expression was created, and we'll need to
    //     add a new disjunction constraint for the new set of overloads.
    //  2. If any component argument expressions (nested or otherwise) are
    //     literals, we can favor operator overloads whose argument types are
    //     identical to the literal type, or whose return types are identical
    //     to any contextual type associated with the application expression.
    
    // Find the argument types.
    auto argTy = CS.getType(expr->getArg());
    auto argTupleTy = argTy->castTo<TupleType>();
    auto argTupleExpr = dyn_cast<TupleExpr>(expr->getArg());

    Type firstArgTy = argTupleTy->getElement(0).getType()->getWithoutParens();
    Type secondArgTy = argTupleTy->getElement(1).getType()->getWithoutParens();

    auto isOptionalWithMatchingObjectType = [](Type optional,
                                               Type object) -> bool {
      if (auto objTy = optional->getRValueType()->getOptionalObjectType())
        return objTy->getRValueType()->isEqual(object->getRValueType());

      return false;
    };

    auto isPotentialForcingOpportunity = [&](Type first, Type second) -> bool {
      return isOptionalWithMatchingObjectType(first, second) ||
             isOptionalWithMatchingObjectType(second, first);
    };

    // Determine whether the given declaration is favored.
    auto isFavoredDecl = [&](ValueDecl *value, Type type) -> bool {
      auto fnTy = type->getAs<AnyFunctionType>();
      if (!fnTy)
        return false;

      Expr *firstArg = argTupleExpr->getElement(0);
      auto firstFavoredTy = CS.getFavoredType(firstArg);
      Expr *secondArg = argTupleExpr->getElement(1);
      auto secondFavoredTy = CS.getFavoredType(secondArg);
      
      auto favoredExprTy = CS.getFavoredType(expr);
      
      if (isArithmeticOperatorDecl(value)) {
        // If the parent has been favored on the way down, propagate that
        // information to its children.
        // TODO: This is only valid for arithmetic expressions.
        if (!firstFavoredTy) {
          CS.setFavoredType(argTupleExpr->getElement(0), favoredExprTy);
          firstFavoredTy = favoredExprTy;
        }
        
        if (!secondFavoredTy) {
          CS.setFavoredType(argTupleExpr->getElement(1), favoredExprTy);
          secondFavoredTy = favoredExprTy;
        }
      }
      
      auto params = fnTy->getParams();
      if (params.size() != 2)
        return false;

      auto firstParamTy = params[0].getOldType();
      auto secondParamTy = params[1].getOldType();

      auto resultTy = fnTy->getResult();
      auto contextualTy = CS.getContextualType(expr);

      return (isFavoredParamAndArg(CS, firstParamTy, firstArgTy, secondArgTy) ||
              isFavoredParamAndArg(CS, secondParamTy, secondArgTy,
                                   firstArgTy)) &&
             firstParamTy->isEqual(secondParamTy) &&
             !isPotentialForcingOpportunity(firstArgTy, secondArgTy) &&
             (!contextualTy || contextualTy->isEqual(resultTy));
    };
    
    favorCallOverloads(expr, CS, isFavoredDecl);
  }
  
  class ConstraintOptimizer : public ASTWalker {
    ConstraintSystem &CS;
    
  public:
    
    ConstraintOptimizer(ConstraintSystem &cs) :
      CS(cs) {}
    
    std::pair<bool, Expr *> walkToExprPre(Expr *expr) override {

      if (CS.shouldReusePrecheckedType() &&
          !CS.getType(expr)->hasTypeVariable()) {
        return { false, expr };
      }
      
      if (auto applyExpr = dyn_cast<ApplyExpr>(expr)) {
        if (isa<PrefixUnaryExpr>(applyExpr) ||
            isa<PostfixUnaryExpr>(applyExpr)) {
          favorMatchingUnaryOperators(applyExpr, CS);
        } else if (isa<BinaryExpr>(applyExpr)) {
          favorMatchingBinaryOperators(applyExpr, CS);
        } else {
          favorMatchingOverloadExprs(applyExpr, CS);
        }
      }
      
      // If the paren expr has a favored type, and the subExpr doesn't,
      // propagate downwards. Otherwise, propagate upwards.
      if (auto parenExpr = dyn_cast<ParenExpr>(expr)) {
        if (!CS.getFavoredType(parenExpr->getSubExpr())) {
          CS.setFavoredType(parenExpr->getSubExpr(),
                            CS.getFavoredType(parenExpr));
        } else if (!CS.getFavoredType(parenExpr)) {
          CS.setFavoredType(parenExpr,
                            CS.getFavoredType(parenExpr->getSubExpr()));
        }
      }
      
      return { true, expr };
    }
    
    Expr *walkToExprPost(Expr *expr) override {
      return expr;
    }
    
    /// Ignore statements.
    std::pair<bool, Stmt *> walkToStmtPre(Stmt *stmt) override {
      return { false, stmt };
    }
    
    /// Ignore declarations.
    bool walkToDeclPre(Decl *decl) override { return false; }
  };
} // end anonymous namespace

namespace {

  class ConstraintGenerator : public ExprVisitor<ConstraintGenerator, Type> {
    ConstraintSystem &CS;
    DeclContext *CurDC;
    ConstraintSystemPhase CurrPhase;
    SmallVector<DeclContext*, 4> DCStack;

    static const unsigned numEditorPlaceholderVariables = 2;

    /// A buffer of type variables used for editor placeholders. We only
    /// use a small number of these (rotating through), to prevent expressions
    /// with a large number of editor placeholders from flooding the constraint
    /// system with type variables.
    TypeVariableType *editorPlaceholderVariables[numEditorPlaceholderVariables]
      = { nullptr, nullptr };
    unsigned currentEditorPlaceholderVariable = 0;

    /// Add constraints for a reference to a named member of the given
    /// base type, and return the type of such a reference.
    Type addMemberRefConstraints(Expr *expr, Expr *base, DeclName name,
                                 FunctionRefKind functionRefKind,
                                 ArrayRef<ValueDecl *> outerAlternatives) {
      // The base must have a member of the given name, such that accessing
      // that member through the base returns a value convertible to the type
      // of this expression.
      auto baseTy = CS.getType(base);
      auto tv = CS.createTypeVariable(
                  CS.getConstraintLocator(expr, ConstraintLocator::Member),
                  TVO_CanBindToLValue | TVO_CanBindToNoEscape);
      SmallVector<OverloadChoice, 4> outerChoices;
      for (auto decl : outerAlternatives) {
        outerChoices.push_back(OverloadChoice(Type(), decl, functionRefKind));
      }
      CS.addValueMemberConstraint(
          baseTy, name, tv, CurDC, functionRefKind, outerChoices,
          CS.getConstraintLocator(expr, ConstraintLocator::Member));
      return tv;
    }

    /// Add constraints for a reference to a specific member of the given
    /// base type, and return the type of such a reference.
    Type addMemberRefConstraints(Expr *expr, Expr *base, ValueDecl *decl,
                                 FunctionRefKind functionRefKind) {
      // If we're referring to an invalid declaration, fail.
      if (!decl)
        return nullptr;
      
      if (decl->isInvalid())
        return nullptr;

      auto memberLocator =
        CS.getConstraintLocator(expr, ConstraintLocator::Member);
      auto tv = CS.createTypeVariable(memberLocator,
                                      TVO_CanBindToLValue | TVO_CanBindToNoEscape);

      OverloadChoice choice =
          OverloadChoice(CS.getType(base), decl, functionRefKind);

      auto locator = CS.getConstraintLocator(expr, ConstraintLocator::Member);
      CS.addBindOverloadConstraint(tv, choice, locator, CurDC);
      return tv;
    }

    /// Add constraints for a subscript operation.
    Type addSubscriptConstraints(
        Expr *anchor, Type baseTy, Expr *index,
        ValueDecl *declOrNull, ArrayRef<Identifier> argLabels,
        bool hasTrailingClosure, ConstraintLocator *locator = nullptr,
        SmallVectorImpl<TypeVariableType *> *addedTypeVars = nullptr) {
      // Locators used in this expression.
      if (locator == nullptr)
        locator = CS.getConstraintLocator(anchor);

      auto fnLocator =
        CS.getConstraintLocator(locator,
                                ConstraintLocator::ApplyFunction);
      auto memberLocator =
        CS.getConstraintLocator(locator,
                                ConstraintLocator::SubscriptMember);
      auto resultLocator =
        CS.getConstraintLocator(locator,
                                ConstraintLocator::FunctionResult);

      associateArgumentLabels(memberLocator, {argLabels, hasTrailingClosure});

      Type outputTy;

      // For an integer subscript expression on an array slice type, instead of
      // introducing a new type variable we can easily obtain the element type.
      if (isa<SubscriptExpr>(anchor)) {

        auto isLValueBase = false;
        auto baseObjTy = baseTy;
        if (baseObjTy->is<LValueType>()) {
          isLValueBase = true;
          baseObjTy = baseObjTy->getWithoutSpecifierType();
        }
        
        if (CS.isArrayType(baseObjTy.getPointer())) {

          if (auto arraySliceTy = 
                dyn_cast<ArraySliceType>(baseObjTy.getPointer())) {
            baseObjTy = arraySliceTy->getDesugaredType();
          }
          
          auto indexExpr = index;
          
          if (auto parenExpr = dyn_cast<ParenExpr>(indexExpr)) {
            indexExpr = parenExpr->getSubExpr();
          }
          
          if (isa<IntegerLiteralExpr>(indexExpr)) {
            
            outputTy = baseObjTy->getAs<BoundGenericType>()->getGenericArgs()[0];
            
            if (isLValueBase)
              outputTy = LValueType::get(outputTy);
          }
        } else if (auto dictTy = CS.isDictionaryType(baseObjTy)) {
          auto keyTy = dictTy->first;
          auto valueTy = dictTy->second;

          if (isFavoredParamAndArg(CS, keyTy, CS.getType(index))) {
            outputTy = OptionalType::get(valueTy);
            
            if (isLValueBase)
              outputTy = LValueType::get(outputTy);
          }
        }
      }
      
      if (outputTy.isNull()) {
        outputTy = CS.createTypeVariable(resultLocator,
                                         TVO_CanBindToLValue | TVO_CanBindToNoEscape);
        if (addedTypeVars)
          addedTypeVars->push_back(outputTy->castTo<TypeVariableType>());
      }

      // FIXME: This can only happen when diagnostics successfully type-checked
      // sub-expression of the subscript and mutated AST, but under normal
      // circumstances subscript should never have InOutExpr as a direct child
      // until type checking is complete and expression is re-written.
      // Proper fix for such situation requires preventing diagnostics from
      // re-writing AST after successful type checking of the sub-expressions.
      if (auto inoutTy = baseTy->getAs<InOutType>()) {
        baseTy = LValueType::get(inoutTy->getObjectType());
      }

      // Add the member constraint for a subscript declaration.
      // FIXME: weak name!
      auto memberTy = CS.createTypeVariable(
          memberLocator, TVO_CanBindToLValue | TVO_CanBindToNoEscape);
      if (addedTypeVars)
        addedTypeVars->push_back(memberTy);

      // FIXME: synthesizeMaterializeForSet() wants to statically dispatch to
      // a known subscript here. This might be cleaner if we split off a new
      // UnresolvedSubscriptExpr from SubscriptExpr.
      if (auto decl = declOrNull) {
        OverloadChoice choice =
            OverloadChoice(baseTy, decl, FunctionRefKind::DoubleApply);
        CS.addBindOverloadConstraint(memberTy, choice, memberLocator,
                                     CurDC);
      } else {
        CS.addValueMemberConstraint(baseTy, DeclBaseName::createSubscript(),
                                    memberTy, CurDC,
                                    FunctionRefKind::DoubleApply,
                                    /*outerAlternatives=*/{},
                                    memberLocator);
      }

      // FIXME: Redesign the AST so that an ApplyExpr directly stores a list of
      // arguments together with their inout-ness, instead of a single
      // ParenExpr or TupleExpr.
      SmallVector<AnyFunctionType::Param, 8> params;
      AnyFunctionType::decomposeInput(CS.getType(index), params);

      // Add the constraint that the index expression's type be convertible
      // to the input type of the subscript operator.
      CS.addConstraint(ConstraintKind::ApplicableFunction,
                       FunctionType::get(params, outputTy),
                       memberTy,
                       fnLocator);

      Type fixedOutputType =
          CS.getFixedTypeRecursive(outputTy, /*wantRValue=*/false);
      if (!fixedOutputType->isTypeVariableOrMember()) {
        CS.setFavoredType(anchor, fixedOutputType.getPointer());
        outputTy = fixedOutputType;
      }

      return outputTy;
    }

  public:
    ConstraintGenerator(ConstraintSystem &CS, DeclContext *DC)
        : CS(CS), CurDC(DC ? DC : CS.DC), CurrPhase(CS.getPhase()) {
      // Although constraint system is initialized in `constraint
      // generation` phase, we have to set it here manually because e.g.
      // function builders could generate constraints for its body
      // in the middle of the solving.
      CS.setPhase(ConstraintSystemPhase::ConstraintGeneration);
    }

    virtual ~ConstraintGenerator() {
      CS.setPhase(CurrPhase);
      // We really ought to have this assertion:
      //   assert(DCStack.empty() && CurDC == CS.DC);
      // Unfortunately, ASTWalker is really bad at letting us establish
      // invariants like this because walkToExprPost isn't called if
      // something early-aborts the walk.
    }

    ConstraintSystem &getConstraintSystem() const { return CS; }

    void enterClosure(ClosureExpr *closure) {
      DCStack.push_back(CurDC);
      CurDC = closure;
    }

    void exitClosure(ClosureExpr *closure) {
      assert(CurDC == closure);
      CurDC = DCStack.pop_back_val();
    }
    
    virtual Type visitErrorExpr(ErrorExpr *E) {
      // FIXME: Can we do anything with error expressions at this point?
      return nullptr;
    }

    virtual Type visitCodeCompletionExpr(CodeCompletionExpr *E) {
      CS.Options |= ConstraintSystemFlags::SuppressDiagnostics;
      return CS.createTypeVariable(CS.getConstraintLocator(E),
                                   TVO_CanBindToLValue |
                                   TVO_CanBindToNoEscape);
    }

    Type visitNilLiteralExpr(NilLiteralExpr *expr) {
      auto &DE = CS.getASTContext().Diags;
      // If this is a standalone `nil` literal expression e.g.
      // `_ = nil`, let's diagnose it here because solver can't
      // attempt any types for it.
      if (!CS.isExprBeingDiagnosed(expr)) {
        auto *parentExpr = CS.getParentExpr(expr);

        // `_ = nil`
        if (auto *assignment = dyn_cast_or_null<AssignExpr>(parentExpr)) {
          if (isa<DiscardAssignmentExpr>(assignment->getDest())) {
            DE.diagnose(expr->getLoc(), diag::unresolved_nil_literal);
            return Type();
          }
        }

        if (!parentExpr && !CS.getContextualType(expr)) {
          DE.diagnose(expr->getLoc(), diag::unresolved_nil_literal);
          return Type();
        }
      }

      return visitLiteralExpr(expr);
    }

    Type visitLiteralExpr(LiteralExpr *expr) {
      // If the expression has already been assigned a type; just use that type.
      if (expr->getType())
        return expr->getType();

      auto protocol = TypeChecker::getLiteralProtocol(CS.getASTContext(), expr);
      if (!protocol)
        return nullptr;

      auto tv = CS.createTypeVariable(CS.getConstraintLocator(expr),
                                      TVO_PrefersSubtypeBinding |
                                      TVO_CanBindToNoEscape);
      CS.addConstraint(ConstraintKind::LiteralConformsTo, tv,
                       protocol->getDeclaredType(),
                       CS.getConstraintLocator(expr));
      return tv;
    }

    Type
    visitInterpolatedStringLiteralExpr(InterpolatedStringLiteralExpr *expr) {
      // Dig out the ExpressibleByStringInterpolation protocol.
      auto &ctx = CS.getASTContext();
      auto interpolationProto = TypeChecker::getProtocol(
          ctx, expr->getLoc(),
          KnownProtocolKind::ExpressibleByStringInterpolation);
      if (!interpolationProto) {
        ctx.Diags.diagnose(expr->getStartLoc(),
                           diag::interpolation_missing_proto);
        return nullptr;
      }

      // The type of the expression must conform to the
      // ExpressibleByStringInterpolation protocol.
      auto locator = CS.getConstraintLocator(expr);
      auto tv = CS.createTypeVariable(locator,
                                      TVO_PrefersSubtypeBinding |
                                      TVO_CanBindToNoEscape);
      CS.addConstraint(ConstraintKind::LiteralConformsTo, tv,
                       interpolationProto->getDeclaredType(),
                       locator);

      if (auto appendingExpr = expr->getAppendingExpr()) {
        auto associatedTypeDecl = interpolationProto->getAssociatedType(
          ctx.Id_StringInterpolation);
        if (associatedTypeDecl == nullptr) {
          ctx.Diags.diagnose(expr->getStartLoc(),
                             diag::interpolation_broken_proto);
          return nullptr;
        }

        auto interpolationTV = DependentMemberType::get(tv, associatedTypeDecl);

        auto appendingExprType = CS.getType(appendingExpr);
        auto appendingLocator = CS.getConstraintLocator(appendingExpr);

        // Must be Conversion; if it's Equal, then in semi-rare cases, the 
        // interpolation temporary variable cannot be @lvalue.
        CS.addConstraint(ConstraintKind::Conversion, appendingExprType,
                         interpolationTV, appendingLocator);
      }

      return tv;
    }

    Type visitMagicIdentifierLiteralExpr(MagicIdentifierLiteralExpr *expr) {
      switch (expr->getKind()) {
      case MagicIdentifierLiteralExpr::Column:
      case MagicIdentifierLiteralExpr::File:
      case MagicIdentifierLiteralExpr::Function:
      case MagicIdentifierLiteralExpr::Line:
        return visitLiteralExpr(expr);

      case MagicIdentifierLiteralExpr::DSOHandle: {
        // #dsohandle has type UnsafeMutableRawPointer.
        auto &ctx = CS.getASTContext();
        if (TypeChecker::requirePointerArgumentIntrinsics(ctx, expr->getLoc()))
          return nullptr;

        auto unsafeRawPointer = ctx.getUnsafeRawPointerDecl();
        return unsafeRawPointer->getDeclaredType();
      }
      }

      llvm_unreachable("Unhandled MagicIdentifierLiteralExpr in switch.");
    }

    Type visitObjectLiteralExpr(ObjectLiteralExpr *expr) {
      auto *exprLoc = CS.getConstraintLocator(expr);
      associateArgumentLabels(
          exprLoc, {expr->getArgumentLabels(), expr->hasTrailingClosure()});

      // If the expression has already been assigned a type; just use that type.
      if (expr->getType())
        return expr->getType();

      auto &de = CS.getASTContext().Diags;
      auto protocol = TypeChecker::getLiteralProtocol(CS.getASTContext(), expr);
      if (!protocol) {
        de.diagnose(expr->getLoc(), diag::use_unknown_object_literal_protocol,
                    expr->getLiteralKindPlainName());
        return nullptr;
      }

      auto tv = CS.createTypeVariable(exprLoc,
                                      TVO_PrefersSubtypeBinding |
                                      TVO_CanBindToNoEscape);
      
      CS.addConstraint(ConstraintKind::LiteralConformsTo, tv,
                       protocol->getDeclaredType(),
                       exprLoc);

      // The arguments are required to be argument-convertible to the
      // idealized parameter type of the initializer, which generally
      // simplifies the first label (e.g. "colorLiteralRed:") by stripping
      // all the redundant stuff about literals (leaving e.g. "red:").
      // Constraint application will quietly rewrite the type of 'args' to
      // use the right labels before forming the call to the initializer.
      auto constrName =
          TypeChecker::getObjectLiteralConstructorName(CS.getASTContext(),
                                                       expr);
      assert(constrName);
      auto *constr = dyn_cast_or_null<ConstructorDecl>(
          protocol->getSingleRequirement(constrName));
      if (!constr) {
        de.diagnose(protocol, diag::object_literal_broken_proto);
        return nullptr;
      }
      auto constrParamType =
          TypeChecker::getObjectLiteralParameterType(expr, constr);

      // Extract the arguments.
      SmallVector<AnyFunctionType::Param, 8> args;
      AnyFunctionType::decomposeInput(CS.getType(expr->getArg()), args);

      // Extract the parameters.
      SmallVector<AnyFunctionType::Param, 8> params;
      AnyFunctionType::decomposeInput(constrParamType, params);

      auto funcType = constr->getMethodInterfaceType()->castTo<FunctionType>();
      ::matchCallArguments(
          CS, funcType, args, params, ConstraintKind::ArgumentConversion,
          CS.getConstraintLocator(expr, ConstraintLocator::ApplyArgument));

      Type result = tv;
      if (constr->isFailable())
        result = OptionalType::get(result);

      return result;
    }

    Type visitDeclRefExpr(DeclRefExpr *E) {
      // If this is a ParamDecl for a closure argument that has an Unresolved
      // type, then this is a situation where CSDiags is trying to perform
      // error recovery within a ClosureExpr.  Just create a new type variable
      // for the decl that isn't bound to anything.  This will ensure that it
      // is considered ambiguous.
      if (auto *VD = dyn_cast<VarDecl>(E->getDecl())) {
        if (VD->hasInterfaceType() &&
            VD->getInterfaceType()->is<UnresolvedType>()) {
          return CS.createTypeVariable(CS.getConstraintLocator(E),
                                       TVO_CanBindToLValue |
                                       TVO_CanBindToNoEscape);
        }
      }

      // If we're referring to an invalid declaration, don't type-check.
      //
      // FIXME: If the decl is in error, we get no information from this.
      // We may, alternatively, want to use a type variable in that case,
      // and possibly infer the type of the variable that way.
      auto oldInterfaceTy = E->getDecl()->getInterfaceType();
      if (E->getDecl()->isInvalid()) {
        CS.setType(E, oldInterfaceTy);
        return nullptr;
      }

      auto locator = CS.getConstraintLocator(E);

      // If this is a 'var' or 'let' declaration with already
      // resolved type, let's favor it.
      if (auto *VD = dyn_cast<VarDecl>(E->getDecl())) {
        Type type;
        if (VD->hasInterfaceType()) {
          type = VD->getInterfaceType();
          if (type->hasTypeParameter())
            type = VD->getDeclContext()->mapTypeIntoContext(type);
          CS.setFavoredType(E, type.getPointer());
        }

        // This can only happen when failure diangostics is trying
        // to type-check expressions inside of a single-statement
        // closure which refer to anonymous parameters, in this case
        // let's either use type as written or allocate a fresh type
        // variable, just like we do for closure type.
        if (auto *PD = dyn_cast<ParamDecl>(VD)) {
          if (!CS.hasType(PD)) {
            if (type && type->hasUnboundGenericType())
              type = CS.openUnboundGenericType(type, locator);

            CS.setType(
                PD, type ? type
                         : CS.createTypeVariable(locator,
                                                 TVO_CanBindToLValue |
                                                 TVO_CanBindToNoEscape));
          }
        }
      }

      // Create an overload choice referencing this declaration and immediately
      // resolve it. This records the overload for use later.
      auto tv = CS.createTypeVariable(locator,
                                      TVO_CanBindToLValue |
                                      TVO_CanBindToNoEscape);

      OverloadChoice choice =
          OverloadChoice(Type(), E->getDecl(), E->getFunctionRefKind());
      CS.resolveOverload(locator, tv, choice, CurDC);
      return tv;
    }

    Type visitOtherConstructorDeclRefExpr(OtherConstructorDeclRefExpr *E) {
      return E->getType();
    }

    Type visitSuperRefExpr(SuperRefExpr *E) {
      if (E->getType())
        return E->getType();

      // Resolve the super type of 'self'.
      return getSuperType(E->getSelf(), E->getLoc(),
                          diag::super_not_in_class_method,
                          diag::super_with_no_base_class);
    }

    Type resolveTypeReferenceInExpression(TypeRepr *repr) {
      TypeLoc loc(repr);
      return resolveTypeReferenceInExpression(loc);
    }

    Type resolveTypeReferenceInExpression(TypeLoc &loc) {
      TypeResolutionOptions options(TypeResolverContext::InExpression);
      options |= TypeResolutionFlags::AllowUnboundGenerics;
      bool hadError = TypeChecker::validateType(
          CS.getASTContext(), loc, TypeResolution::forContextual(CS.DC),
          options);
      return hadError ? Type() : loc.getType();
    }

    Type visitTypeExpr(TypeExpr *E) {
      Type type;
      // If this is an implicit TypeExpr, don't validate its contents.
      auto &typeLoc = E->getTypeLoc();
      if (typeLoc.wasValidated()) {
        type = typeLoc.getType();
      } else if (typeLoc.hasLocation()) {
        type = resolveTypeReferenceInExpression(typeLoc);
      } else if (E->isImplicit() && CS.hasType(&typeLoc)) {
        type = CS.getType(typeLoc);
      }

      if (!type || type->hasError()) return Type();
      
      auto locator = CS.getConstraintLocator(E);
      type = CS.openUnboundGenericType(type, locator);
      CS.setType(E->getTypeLoc(), type);
      return MetatypeType::get(type);
    }

    Type visitDotSyntaxBaseIgnoredExpr(DotSyntaxBaseIgnoredExpr *expr) {
      llvm_unreachable("Already type-checked");
    }

    Type visitOverloadedDeclRefExpr(OverloadedDeclRefExpr *expr) {
      // For a reference to an overloaded declaration, we create a type variable
      // that will be equal to different types depending on which overload
      // is selected.
      auto locator = CS.getConstraintLocator(expr);
      auto tv = CS.createTypeVariable(locator,
                                      TVO_CanBindToLValue | TVO_CanBindToNoEscape);
      ArrayRef<ValueDecl*> decls = expr->getDecls();
      SmallVector<OverloadChoice, 4> choices;
      
      for (unsigned i = 0, n = decls.size(); i != n; ++i) {
        // If the result is invalid, skip it.
        // FIXME: Note this as invalid, in case we don't find a solution,
        // so we don't let errors cascade further.
        if (decls[i]->isInvalid())
          continue;

        OverloadChoice choice =
            OverloadChoice(Type(), decls[i], expr->getFunctionRefKind());
        choices.push_back(choice);
      }

      // If there are no valid overloads, give up.
      if (choices.empty())
        return nullptr;

      // Record this overload set.
      CS.addOverloadSet(tv, choices, CurDC, locator);
      return tv;
    }

    Type visitUnresolvedDeclRefExpr(UnresolvedDeclRefExpr *expr) {
      // This is an error case, where we're trying to use type inference
      // to help us determine which declaration the user meant to refer to.
      // FIXME: Do we need to note that we're doing some kind of recovery?
      return CS.createTypeVariable(CS.getConstraintLocator(expr),
                                   TVO_CanBindToLValue |
                                   TVO_CanBindToNoEscape);
    }
    
    Type visitMemberRefExpr(MemberRefExpr *expr) {
      return addMemberRefConstraints(expr, expr->getBase(),
                                     expr->getMember().getDecl(),
                                     /*FIXME:*/FunctionRefKind::DoubleApply);
    }
    
    Type visitDynamicMemberRefExpr(DynamicMemberRefExpr *expr) {
      llvm_unreachable("Already typechecked");
    }
    
    virtual Type visitUnresolvedMemberExpr(UnresolvedMemberExpr *expr) {
      auto baseLocator = CS.getConstraintLocator(
                            expr,
                            ConstraintLocator::MemberRefBase);
      FunctionRefKind functionRefKind =
        expr->getArgument() ? FunctionRefKind::DoubleApply
                            : FunctionRefKind::Compound;

      auto memberLocator
        = CS.getConstraintLocator(expr, ConstraintLocator::UnresolvedMember);

      // Since base type in this case is completely dependent on context it
      // should be marked as a potential hole.
      auto baseTy = CS.createTypeVariable(baseLocator, TVO_CanBindToNoEscape |
                                                           TVO_CanBindToHole);
      auto memberTy = CS.createTypeVariable(
          memberLocator, TVO_CanBindToLValue | TVO_CanBindToNoEscape);

      // An unresolved member expression '.member' is modeled as a value member
      // constraint
      //
      //   T0.Type[.member] == T1
      //
      // for fresh type variables T0 and T1, which pulls out a static
      // member, i.e., an enum case or a static variable.
      auto baseMetaTy = MetatypeType::get(baseTy);
      CS.addUnresolvedValueMemberConstraint(baseMetaTy, expr->getName(),
                                            memberTy, CurDC, functionRefKind,
                                            memberLocator);

      // If there is an argument, apply it.
      if (auto arg = expr->getArgument()) {
        // The result type of the function must be convertible to the base type.
        // TODO: we definitely want this to include ImplicitlyUnwrappedOptional;
        // does it need to include everything else in the world?
        auto outputTy = CS.createTypeVariable(
            CS.getConstraintLocator(expr, ConstraintLocator::FunctionResult),
            TVO_CanBindToNoEscape);
        CS.addConstraint(ConstraintKind::Conversion, outputTy, baseTy,
          CS.getConstraintLocator(expr, ConstraintLocator::RValueAdjustment));

        // The function/enum case must be callable with the given argument.

        // FIXME: Redesign the AST so that an UnresolvedMemberExpr directly
        // stores a list of arguments together with their inout-ness, instead of
        // a single ParenExpr or TupleExpr argument.
        SmallVector<AnyFunctionType::Param, 8> params;
        AnyFunctionType::decomposeInput(CS.getType(arg), params);

        CS.addConstraint(ConstraintKind::ApplicableFunction,
                         FunctionType::get(params, outputTy),
                         memberTy,
          CS.getConstraintLocator(expr, ConstraintLocator::ApplyFunction));

        associateArgumentLabels(
            CS.getConstraintLocator(expr),
            {expr->getArgumentLabels(), expr->hasTrailingClosure()});
        return baseTy;
      }

      // Otherwise, the member needs to be convertible to the base type.
      CS.addConstraint(ConstraintKind::Conversion, memberTy, baseTy,
        CS.getConstraintLocator(expr, ConstraintLocator::RValueAdjustment));
      
      // The member type also needs to be convertible to the context type, which
      // preserves lvalue-ness.
      auto resultTy = CS.createTypeVariable(memberLocator,
                                            TVO_CanBindToLValue |
                                            TVO_CanBindToNoEscape);
      CS.addConstraint(ConstraintKind::Conversion, memberTy, resultTy,
                       memberLocator);
      CS.addConstraint(ConstraintKind::Equal, resultTy, baseTy,
                       memberLocator);
      return resultTy;
    }

    Type visitUnresolvedDotExpr(UnresolvedDotExpr *expr) {
      // If this is Builtin.type_join*, just return any type and move
      // on since we're going to discard this, and creating any type
      // variables for the reference will cause problems.
      auto &ctx = CS.getASTContext();
      auto typeOperation = getTypeOperation(expr, ctx);
      if (typeOperation != TypeOperation::None)
        return ctx.TheAnyType;

      // If this is `Builtin.trigger_fallback_diagnostic()`, fail
      // without producing any diagnostics, in order to test fallback error.
      if (isTriggerFallbackDiagnosticBuiltin(expr, ctx))
        return Type();

      // Open a member constraint for constructor delegations on the
      // subexpr type.
      if (TypeChecker::getSelfForInitDelegationInConstructor(CS.DC, expr)) {
        auto baseTy = CS.getType(expr->getBase())
                        ->getWithoutSpecifierType();

        // 'self' or 'super' will reference an instance, but the constructor
        // is semantically a member of the metatype. This:
        //   self.init()
        //   super.init()
        // is really more like:
        //   self = Self.init()
        //   self.super = Super.init()
        baseTy = MetatypeType::get(baseTy, ctx);

        auto methodTy = CS.createTypeVariable(
            CS.getConstraintLocator(expr,
                                    ConstraintLocator::ApplyFunction),
            TVO_CanBindToNoEscape);

        // FIXME: Once TVO_PrefersSubtypeBinding is replaced with something
        // better, we won't need the second type variable at all.
        {
          auto argTy = CS.createTypeVariable(
              CS.getConstraintLocator(expr,
                                      ConstraintLocator::ApplyArgument),
              (TVO_CanBindToLValue |
               TVO_CanBindToInOut |
               TVO_CanBindToNoEscape |
               TVO_PrefersSubtypeBinding));
          CS.addConstraint(
              ConstraintKind::FunctionInput, methodTy, argTy,
              CS.getConstraintLocator(expr));
        }

        CS.addValueMemberConstraint(
            baseTy, expr->getName(), methodTy, CurDC,
            expr->getFunctionRefKind(),
            /*outerAlternatives=*/{},
            CS.getConstraintLocator(expr,
                                    ConstraintLocator::ConstructorMember));

        // The result of the expression is the partial application of the
        // constructor to the subexpression.
        return methodTy;
      }

      return addMemberRefConstraints(expr, expr->getBase(), expr->getName(),
                                     expr->getFunctionRefKind(),
                                     expr->getOuterAlternatives());
    }

    Type visitUnresolvedSpecializeExpr(UnresolvedSpecializeExpr *expr) {
      auto baseTy = CS.getType(expr->getSubExpr());
      
      // We currently only support explicit specialization of generic types.
      // FIXME: We could support explicit function specialization.
      auto &de = CS.getASTContext().Diags;
      if (baseTy->is<AnyFunctionType>()) {
        de.diagnose(expr->getSubExpr()->getLoc(),
                    diag::cannot_explicitly_specialize_generic_function);
        de.diagnose(expr->getLAngleLoc(),
                    diag::while_parsing_as_left_angle_bracket);
        return Type();
      }
      
      if (AnyMetatypeType *meta = baseTy->getAs<AnyMetatypeType>()) {
        if (BoundGenericType *bgt
              = meta->getInstanceType()->getAs<BoundGenericType>()) {
          ArrayRef<Type> typeVars = bgt->getGenericArgs();
          MutableArrayRef<TypeLoc> specializations =
              expr->getUnresolvedParams();

          // If we have too many generic arguments, complain.
          if (specializations.size() > typeVars.size()) {
            de.diagnose(expr->getSubExpr()->getLoc(),
                        diag::type_parameter_count_mismatch,
                        bgt->getDecl()->getName(),
                        typeVars.size(), specializations.size(),
                        false)
              .highlight(SourceRange(expr->getLAngleLoc(),
                                     expr->getRAngleLoc()));
            de.diagnose(bgt->getDecl(), diag::kind_declname_declared_here,
                        DescriptiveDeclKind::GenericType,
                        bgt->getDecl()->getName());
            return Type();
          }

          // Bind the specified generic arguments to the type variables in the
          // open type.
          auto locator = CS.getConstraintLocator(expr);
          for (size_t i = 0, size = specializations.size(); i < size; ++i) {
            TypeResolutionOptions options(TypeResolverContext::InExpression);
            options |= TypeResolutionFlags::AllowUnboundGenerics;
            if (TypeChecker::validateType(CS.getASTContext(),
                                specializations[i],
                                TypeResolution::forContextual(CS.DC),
                                options))
              return Type();

            CS.addConstraint(ConstraintKind::Bind,
                             typeVars[i], specializations[i].getType(),
                             locator);
          }
          
          return baseTy;
        } else {
          de.diagnose(expr->getSubExpr()->getLoc(), diag::not_a_generic_type,
                      meta->getInstanceType());
          de.diagnose(expr->getLAngleLoc(),
                      diag::while_parsing_as_left_angle_bracket);
          return Type();
        }
      }

      // FIXME: If the base type is a type variable, constrain it to a metatype
      // of a bound generic type.
      de.diagnose(expr->getSubExpr()->getLoc(),
                  diag::not_a_generic_definition);
      de.diagnose(expr->getLAngleLoc(),
                  diag::while_parsing_as_left_angle_bracket);
      return Type();
    }
    
    Type visitSequenceExpr(SequenceExpr *expr) {
      // If a SequenceExpr survived until CSGen, then there was an upstream
      // error that was already reported.
      return Type();
    }

    Type visitArrowExpr(ArrowExpr *expr) {
      // If an ArrowExpr survived until CSGen, then there was an upstream
      // error that was already reported.
      return Type();
    }

    Type visitIdentityExpr(IdentityExpr *expr) {
      return CS.getType(expr->getSubExpr());
    }

    Type visitAnyTryExpr(AnyTryExpr *expr) {
      return CS.getType(expr->getSubExpr());
    }

    Type visitOptionalTryExpr(OptionalTryExpr *expr) {
      auto valueTy = CS.createTypeVariable(CS.getConstraintLocator(expr),
                                           TVO_PrefersSubtypeBinding |
                                           TVO_CanBindToNoEscape);

      Type optTy = getOptionalType(expr->getSubExpr()->getLoc(), valueTy);
      if (!optTy)
        return Type();

      // Prior to Swift 5, 'try?' always adds an additional layer of optionality,
      // even if the sub-expression was already optional.
      if (CS.getASTContext().LangOpts.isSwiftVersionAtLeast(5)) {
        CS.addConstraint(ConstraintKind::Conversion,
                         CS.getType(expr->getSubExpr()), optTy,
                         CS.getConstraintLocator(expr));
      } else {
        CS.addConstraint(ConstraintKind::OptionalObject,
                         optTy, CS.getType(expr->getSubExpr()),
                         CS.getConstraintLocator(expr));
      }
      return optTy;
    }

    virtual Type visitParenExpr(ParenExpr *expr) {
      if (auto favoredTy = CS.getFavoredType(expr->getSubExpr())) {
        CS.setFavoredType(expr, favoredTy);
      }

      auto &ctx = CS.getASTContext();
      auto parenType = CS.getType(expr->getSubExpr())->getInOutObjectType();
      auto parenFlags = ParameterTypeFlags().withInOut(expr->isSemanticallyInOutExpr());
      return ParenType::get(ctx, parenType, parenFlags);
    }

    Type visitTupleExpr(TupleExpr *expr) {
      // The type of a tuple expression is simply a tuple of the types of
      // its subexpressions.
      SmallVector<TupleTypeElt, 4> elements;
      elements.reserve(expr->getNumElements());
      for (unsigned i = 0, n = expr->getNumElements(); i != n; ++i) {
        auto *elt = expr->getElement(i);
        auto ty = CS.getType(elt);
        auto flags = ParameterTypeFlags()
            .withInOut(elt->isSemanticallyInOutExpr())
            .withVariadic(isa<VarargExpansionExpr>(elt));
        elements.push_back(TupleTypeElt(ty->getInOutObjectType(),
                                        expr->getElementName(i), flags));
      }

      return TupleType::get(elements, CS.getASTContext());
    }

    Type visitSubscriptExpr(SubscriptExpr *expr) {
      ValueDecl *decl = nullptr;
      if (expr->hasDecl()) {
        decl = expr->getDecl().getDecl();
        if (decl->isInvalid())
          return Type();
      }

      return addSubscriptConstraints(expr, CS.getType(expr->getBase()),
                                     expr->getIndex(),
                                     decl, expr->getArgumentLabels(),
                                     expr->hasTrailingClosure());
    }
    
    Type visitArrayExpr(ArrayExpr *expr) {
      // An array expression can be of a type T that conforms to the
      // ExpressibleByArrayLiteral protocol.
      ProtocolDecl *arrayProto = TypeChecker::getProtocol(
          CS.getASTContext(), expr->getLoc(),
          KnownProtocolKind::ExpressibleByArrayLiteral);
      if (!arrayProto) {
        return Type();
      }

      // Assume that ExpressibleByArrayLiteral contains a single associated type.
      auto *elementAssocTy = arrayProto->getAssociatedTypeMembers()[0];
      if (!elementAssocTy)
        return Type();

      auto locator = CS.getConstraintLocator(expr);
      auto contextualType = CS.getContextualType(expr);
      Type contextualArrayType = nullptr;
      Type contextualArrayElementType = nullptr;
      
      // If a contextual type exists for this expression, apply it directly.
      Optional<Type> arrayElementType;
      if (contextualType &&
          (arrayElementType = ConstraintSystem::isArrayType(contextualType))) {
        // Is the array type a contextual type
        contextualArrayType = contextualType;
        contextualArrayElementType = *arrayElementType;

        CS.addConstraint(ConstraintKind::LiteralConformsTo, contextualType,
                         arrayProto->getDeclaredType(),
                         locator);
        
        unsigned index = 0;
        for (auto element : expr->getElements()) {
          CS.addConstraint(ConstraintKind::Conversion,
                           CS.getType(element),
                           contextualArrayElementType,
                           CS.getConstraintLocator(
                               expr, LocatorPathElt::TupleElement(index++)));
        }
        
        return contextualArrayType;
      }
      
      auto arrayTy = CS.createTypeVariable(locator,
                                           TVO_PrefersSubtypeBinding |
                                           TVO_CanBindToNoEscape);

      // The array must be an array literal type.
      CS.addConstraint(ConstraintKind::LiteralConformsTo, arrayTy,
                       arrayProto->getDeclaredType(),
                       locator);
      
      // Its subexpression should be convertible to a tuple (T.Element...).
      Type arrayElementTy = DependentMemberType::get(arrayTy, elementAssocTy);

      // Introduce conversions from each element to the element type of the
      // array.
      unsigned index = 0;
      for (auto element : expr->getElements()) {
        CS.addConstraint(ConstraintKind::Conversion,
                         CS.getType(element),
                         arrayElementTy,
                         CS.getConstraintLocator(
                           expr, LocatorPathElt::TupleElement(index++)));
      }

      // The array element type defaults to 'Any'.
      CS.addConstraint(ConstraintKind::Defaultable, arrayElementTy,
                       CS.getASTContext().TheAnyType, locator);

      return arrayTy;
    }

    static bool isMergeableValueKind(Expr *expr) {
      return isa<StringLiteralExpr>(expr) || isa<IntegerLiteralExpr>(expr) ||
             isa<FloatLiteralExpr>(expr);
    }

    Type visitDictionaryExpr(DictionaryExpr *expr) {
      ASTContext &C = CS.getASTContext();
      // A dictionary expression can be of a type T that conforms to the
      // ExpressibleByDictionaryLiteral protocol.
      // FIXME: This isn't actually used for anything at the moment.
      ProtocolDecl *dictionaryProto = TypeChecker::getProtocol(
          C, expr->getLoc(), KnownProtocolKind::ExpressibleByDictionaryLiteral);
      if (!dictionaryProto) {
        return Type();
      }

      // FIXME: Protect against broken standard library.
      auto keyAssocTy = dictionaryProto->getAssociatedType(C.Id_Key);
      auto valueAssocTy = dictionaryProto->getAssociatedType(C.Id_Value);

      auto locator = CS.getConstraintLocator(expr);
      auto contextualType = CS.getContextualType(expr);
      Type contextualDictionaryType = nullptr;
      Type contextualDictionaryKeyType = nullptr;
      Type contextualDictionaryValueType = nullptr;
      
      // If a contextual type exists for this expression, apply it directly.
      Optional<std::pair<Type, Type>> dictionaryKeyValue;
      if (contextualType &&
          (dictionaryKeyValue = ConstraintSystem::isDictionaryType(contextualType))) {
        // Is the contextual type a dictionary type?
        contextualDictionaryType = contextualType;
        std::tie(contextualDictionaryKeyType,
                 contextualDictionaryValueType) = *dictionaryKeyValue;
        
        // Form an explicit tuple type from the contextual type's key and value types.
        TupleTypeElt tupleElts[2] = { TupleTypeElt(contextualDictionaryKeyType),
                                      TupleTypeElt(contextualDictionaryValueType) };
        Type contextualDictionaryElementType = TupleType::get(tupleElts, C);
        
        CS.addConstraint(ConstraintKind::LiteralConformsTo, contextualType,
                         dictionaryProto->getDeclaredType(),
                         locator);
        
        unsigned index = 0;
        for (auto element : expr->getElements()) {
          CS.addConstraint(ConstraintKind::Conversion,
                           CS.getType(element),
                           contextualDictionaryElementType,
                           CS.getConstraintLocator(
                               expr, LocatorPathElt::TupleElement(index++)));
        }
        
        return contextualDictionaryType;
      }
      
      auto dictionaryTy = CS.createTypeVariable(locator,
                                                TVO_PrefersSubtypeBinding |
                                                TVO_CanBindToNoEscape);

      // The dictionary must be a dictionary literal type.
      CS.addConstraint(ConstraintKind::LiteralConformsTo, dictionaryTy,
                       dictionaryProto->getDeclaredType(),
                       locator);


      // Its subexpression should be convertible to a tuple ((T.Key,T.Value)...).
      ConstraintLocatorBuilder locatorBuilder(locator);
      auto dictionaryKeyTy = DependentMemberType::get(dictionaryTy,
                                                      keyAssocTy);
      auto dictionaryValueTy = DependentMemberType::get(dictionaryTy,
                                                        valueAssocTy);
      TupleTypeElt tupleElts[2] = { TupleTypeElt(dictionaryKeyTy),
                                    TupleTypeElt(dictionaryValueTy) };
      Type elementTy = TupleType::get(tupleElts, C);

      // Keep track of which elements have been "merged". This way, we won't create
      // needless conversion constraints for elements whose equivalence classes have
      // been merged.
      llvm::DenseSet<Expr *> mergedElements;

      // If no contextual type is present, Merge equivalence classes of key 
      // and value types as necessary.
      if (!CS.getContextualType(expr)) {
        for (auto element1 : expr->getElements()) {
          for (auto element2 : expr->getElements()) {
            if (element1 == element2)
              continue;

            auto tty1 = CS.getType(element1)->getAs<TupleType>();
            auto tty2 = CS.getType(element2)->getAs<TupleType>();

            if (tty1 && tty2) {
              auto mergedKey = false;
              auto mergedValue = false;

              auto keyTyvar1 = tty1->getElementTypes()[0]->
                                getAs<TypeVariableType>();
              auto keyTyvar2 = tty2->getElementTypes()[0]->
                                getAs<TypeVariableType>();

              auto keyExpr1 = cast<TupleExpr>(element1)->getElements()[0];
              auto keyExpr2 = cast<TupleExpr>(element2)->getElements()[0];

              if (keyExpr1->getKind() == keyExpr2->getKind() &&
                  isMergeableValueKind(keyExpr1)) {
                mergedKey = mergeRepresentativeEquivalenceClasses(CS,
                            keyTyvar1, keyTyvar2);
              }

              auto valueTyvar1 = tty1->getElementTypes()[1]->
                                  getAs<TypeVariableType>();
              auto valueTyvar2 = tty2->getElementTypes()[1]->
                                  getAs<TypeVariableType>();

              auto elemExpr1 = cast<TupleExpr>(element1)->getElements()[1];
              auto elemExpr2 = cast<TupleExpr>(element2)->getElements()[1];

              if (elemExpr1->getKind() == elemExpr2->getKind() &&
                isMergeableValueKind(elemExpr1)) {
                mergedValue = mergeRepresentativeEquivalenceClasses(CS, 
                                valueTyvar1, valueTyvar2);
              }

              if (mergedKey && mergedValue)
                mergedElements.insert(element2);
            }
          }
        }
      }      

      // Introduce conversions from each element to the element type of the
      // dictionary. (If the equivalence class of an element has already been
      // merged with a previous one, skip it.)
      unsigned index = 0;
      for (auto element : expr->getElements()) {
        if (!mergedElements.count(element))
          CS.addConstraint(ConstraintKind::Conversion,
                           CS.getType(element),
                           elementTy,
                           CS.getConstraintLocator(
                               expr, LocatorPathElt::TupleElement(index++)));
      }

      // The dictionary key type defaults to 'AnyHashable'.
      auto &ctx = CS.getASTContext();
      if (dictionaryKeyTy->isTypeVariableOrMember() &&
          ctx.getAnyHashableDecl()) {
        auto anyHashable = ctx.getAnyHashableDecl();
        CS.addConstraint(ConstraintKind::Defaultable, dictionaryKeyTy,
                         anyHashable->getDeclaredInterfaceType(), locator);
      }

      // The dictionary value type defaults to 'Any'.
      if (dictionaryValueTy->isTypeVariableOrMember()) {
        CS.addConstraint(ConstraintKind::Defaultable, dictionaryValueTy,
                         ctx.TheAnyType, locator);
      }

      return dictionaryTy;
    }

    Type visitDynamicSubscriptExpr(DynamicSubscriptExpr *expr) {
      return addSubscriptConstraints(expr, CS.getType(expr->getBase()),
                                     expr->getIndex(), /*decl*/ nullptr,
                                     expr->getArgumentLabels(),
                                     expr->hasTrailingClosure());
    }

    Type visitTupleElementExpr(TupleElementExpr *expr) {
      ASTContext &context = CS.getASTContext();
      Identifier name
        = context.getIdentifier(llvm::utostr(expr->getFieldNumber()));
      return addMemberRefConstraints(expr, expr->getBase(), name,
                                     FunctionRefKind::Unapplied,
                                     /*outerAlternatives=*/{});
    }

    /// Give each parameter in a ClosureExpr a fresh type variable if parameter
    /// types were not specified, and return the eventual function type.
    void getClosureParams(ClosureExpr *closureExpr,
                          SmallVectorImpl<AnyFunctionType::Param> &params) {
      auto *paramList = closureExpr->getParameters();
      unsigned i = 0;

      for (auto *param : *paramList) {
        auto *locator = CS.getConstraintLocator(
            closureExpr, LocatorPathElt::TupleElement(i++));
        Type paramType, internalType;

        // If a type was explicitly specified, use its opened type.
        if (param->getTypeRepr()) {
          paramType = closureExpr->mapTypeIntoContext(param->getInterfaceType());
          // FIXME: Need a better locator for a pattern as a base.
          paramType = CS.openUnboundGenericType(paramType, locator);
          internalType = paramType;
        } else {
          // Otherwise, create fresh type variables.
          paramType = CS.createTypeVariable(locator,
                                            TVO_CanBindToInOut |
                                            TVO_CanBindToNoEscape);
          internalType = CS.createTypeVariable(locator,
                                               TVO_CanBindToLValue |
                                               TVO_CanBindToNoEscape);
          CS.addConstraint(ConstraintKind::BindParam, paramType, internalType,
                           locator);
        }
        CS.setType(param, internalType);
        params.push_back(param->toFunctionParam(paramType));
      }
    }

    /// Produces a type for the given pattern, filling in any missing
    /// type information with fresh type variables.
    ///
    /// \param pattern The pattern.
    Type getTypeForPattern(Pattern *pattern, ConstraintLocatorBuilder locator) {
      switch (pattern->getKind()) {
      case PatternKind::Paren:
        // Parentheses don't affect the type.
        return getTypeForPattern(cast<ParenPattern>(pattern)->getSubPattern(),
                                 locator);
      case PatternKind::Var:
        // Var doesn't affect the type.
        return getTypeForPattern(cast<VarPattern>(pattern)->getSubPattern(),
                                 locator);
      case PatternKind::Any: {
        // If we have a type from an initializer expression, and that
        // expression does not produce an InOut type, use it.  This
        // will avoid exponential typecheck behavior in the case of
        // tuples, nested arrays, and dictionary literals.
        //
        // Otherwise, create a new type variable.
        if (auto boundExpr = locator.trySimplifyToExpr()) {
          if (!boundExpr->isSemanticallyInOutExpr())
            return CS.getType(boundExpr)->getRValueType();
        }

        return CS.createTypeVariable(CS.getConstraintLocator(locator),
                                     TVO_CanBindToNoEscape);
      }

      case PatternKind::Named: {
        auto var = cast<NamedPattern>(pattern)->getDecl();

        // If we have a type from an initializer expression, and that
        // expression does not produce an InOut type, use it.  This
        // will avoid exponential typecheck behavior in the case of
        // tuples, nested arrays, and dictionary literals.
        //
        // FIXME: This should be handled in the solver, not here.
        //
        // Otherwise, create a new type variable.
        auto ty = Type();
        if (!var->hasNonPatternBindingInit() &&
            !var->hasAttachedPropertyWrapper()) {
          if (auto boundExpr = locator.trySimplifyToExpr()) {
            if (!boundExpr->isSemanticallyInOutExpr())
              ty = CS.getType(boundExpr)->getRValueType();
          }
        }

        auto ROK = ReferenceOwnership::Strong;
        if (auto *OA = var->getAttrs().getAttribute<ReferenceOwnershipAttr>())
          ROK = OA->get();

        switch (optionalityOf(ROK)) {
        case ReferenceOwnershipOptionality::Required:
          if (ty && ty->getOptionalObjectType())
            return ty; // Already Optional<T>.
          // Create a fresh type variable to handle overloaded expressions.
          if (!ty || ty->is<TypeVariableType>())
            ty = CS.createTypeVariable(CS.getConstraintLocator(locator),
                                       TVO_CanBindToNoEscape);
          return TypeChecker::getOptionalType(var->getLoc(), ty);
        case ReferenceOwnershipOptionality::Allowed:
        case ReferenceOwnershipOptionality::Disallowed:
          break;
        }
        if (ty)
          return ty;
        return CS.createTypeVariable(CS.getConstraintLocator(locator),
                                     TVO_CanBindToNoEscape);
      }

      case PatternKind::Typed: {
        auto typedPattern = cast<TypedPattern>(pattern);
        // FIXME: Need a better locator for a pattern as a base.
        Type openedType = CS.openUnboundGenericType(typedPattern->getType(),
                                                    locator);

        // For a typed pattern, simply return the opened type of the pattern.
        // FIXME: Error recovery if the type is an error type?
        return openedType;
      }

      case PatternKind::Tuple: {
        auto tuplePat = cast<TuplePattern>(pattern);
        SmallVector<TupleTypeElt, 4> tupleTypeElts;
        tupleTypeElts.reserve(tuplePat->getNumElements());
        for (unsigned i = 0, e = tuplePat->getNumElements(); i != e; ++i) {
          auto &tupleElt = tuplePat->getElement(i);
          Type eltTy = getTypeForPattern(tupleElt.getPattern(),
                                         locator.withPathElement(
                                           LocatorPathElt::TupleElement(i)));
          tupleTypeElts.push_back(TupleTypeElt(eltTy, tupleElt.getLabel()));
        }
        return TupleType::get(tupleTypeElts, CS.getASTContext());
      }
      
      // Refutable patterns occur when checking the PatternBindingDecls in an
      // if/let or while/let condition.  They always require an initial value,
      // so they always allow unspecified types.
#define PATTERN(Id, Parent)
#define REFUTABLE_PATTERN(Id, Parent) case PatternKind::Id:
#include "swift/AST/PatternNodes.def"
        // TODO: we could try harder here, e.g. for enum elements to provide the
        // enum type.
        return CS.createTypeVariable(CS.getConstraintLocator(locator),
                                     TVO_CanBindToNoEscape);
      }

      llvm_unreachable("Unhandled pattern kind");
    }

    Type visitCaptureListExpr(CaptureListExpr *expr) {
      // The type of the capture list is just the type of its closure.
      return CS.getType(expr->getClosureBody());
    }

    /// Walk a closure body to determine if it's possible for
    /// it to return with a non-void result.
    static bool closureHasNoResult(ClosureExpr *expr) {
      // A walker that looks for 'return' statements that aren't
      // nested within closures or nested declarations.
      class FindReturns : public ASTWalker {
        bool FoundResultReturn = false;
        bool FoundNoResultReturn = false;

        std::pair<bool, Expr *> walkToExprPre(Expr *expr) override {
          return { false, expr };
        }
        bool walkToDeclPre(Decl *decl) override {
          return false;
        }
        std::pair<bool, Stmt *> walkToStmtPre(Stmt *stmt) override {
          // Record return statements.
          if (auto ret = dyn_cast<ReturnStmt>(stmt)) {
            // If it has a result, remember that we saw one, but keep
            // traversing in case there's a no-result return somewhere.
            if (ret->hasResult()) {
              FoundResultReturn = true;

            // Otherwise, stop traversing.
            } else {
              FoundNoResultReturn = true;
              return { false, nullptr };
            }
          }
          return { true, stmt };
        }
      public:
        bool hasNoResult() const {
          return FoundNoResultReturn || !FoundResultReturn;
        }
      };

      // Don't apply this to single-expression-body closures.
      if (expr->hasSingleExpressionBody())
        return false;

      auto body = expr->getBody();
      if (!body) return false;

      FindReturns finder;
      body->walk(finder);
      return finder.hasNoResult();
    }
    
    /// Walk a closure AST to determine if it can throw.
    bool closureCanThrow(ClosureExpr *expr) {
      // A walker that looks for 'try' or 'throw' expressions
      // that aren't nested within closures, nested declarations,
      // or exhaustive catches.
      class FindInnerThrows : public ASTWalker {
        ConstraintSystem &CS;
        bool FoundThrow = false;
        
        std::pair<bool, Expr *> walkToExprPre(Expr *expr) override {
          // If we've found a 'try', record it and terminate the traversal.
          if (isa<TryExpr>(expr)) {
            FoundThrow = true;
            return { false, nullptr };
          }

          // Don't walk into a 'try!' or 'try?'.
          if (isa<ForceTryExpr>(expr) || isa<OptionalTryExpr>(expr)) {
            return { false, expr };
          }
          
          // Do not recurse into other closures.
          if (isa<ClosureExpr>(expr))
            return { false, expr };
          
          return { true, expr };
        }
        
        bool walkToDeclPre(Decl *decl) override {
          // Do not walk into function or type declarations.
          if (!isa<PatternBindingDecl>(decl))
            return false;
          
          return true;
        }

        bool isSyntacticallyExhaustive(DoCatchStmt *stmt) {
          for (auto catchClause : stmt->getCatches()) {
            if (isSyntacticallyExhaustive(catchClause))
              return true;
          }

          return false;
        }

        bool isSyntacticallyExhaustive(CatchStmt *clause) {
          // If it's obviously non-exhaustive, great.
          if (clause->getGuardExpr())
            return false;

          // If we can show that it's exhaustive without full
          // type-checking, great.
          if (clause->isSyntacticallyExhaustive())
            return true;

          // Okay, resolve the pattern.
          Pattern *pattern = clause->getErrorPattern();
          pattern = TypeChecker::resolvePattern(pattern, CS.DC,
                                                /*isStmtCondition*/false);
          if (!pattern) return false;

          // Save that aside while we explore the type.
          clause->setErrorPattern(pattern);

          // Require the pattern to have a particular shape: a number
          // of is-patterns applied to an irrefutable pattern.
          pattern = pattern->getSemanticsProvidingPattern();
          while (auto isp = dyn_cast<IsPattern>(pattern)) {
            if (TypeChecker::validateType(CS.getASTContext(),
                                          isp->getCastTypeLoc(),
                                          TypeResolution::forContextual(CS.DC),
                                          TypeResolverContext::InExpression)) {
              return false;
            }

            if (!isp->hasSubPattern()) {
              pattern = nullptr;
              break;
            } else {
              pattern = isp->getSubPattern()->getSemanticsProvidingPattern();
            }
          }
          if (pattern && pattern->isRefutablePattern()) {
            return false;
          }

          // Okay, now it should be safe to coerce the pattern.
          // Pull the top-level pattern back out.
          pattern = clause->getErrorPattern();
          Type exnType = CS.getASTContext().getErrorDecl()->getDeclaredType();
          if (!exnType)
            return false;
          if (TypeChecker::coercePatternToType(pattern,
                                        TypeResolution::forContextual(CS.DC),
                                        exnType,
                                        TypeResolverContext::InExpression)) {
            return false;
          }

          clause->setErrorPattern(pattern);
          return clause->isSyntacticallyExhaustive();
        }
        
        std::pair<bool, Stmt *> walkToStmtPre(Stmt *stmt) override {
          // If we've found a 'throw', record it and terminate the traversal.
          if (isa<ThrowStmt>(stmt)) {
            FoundThrow = true;
            return { false, nullptr };
          }

          // Handle do/catch differently.
          if (auto doCatch = dyn_cast<DoCatchStmt>(stmt)) {
            // Only walk into the 'do' clause of a do/catch statement
            // if the catch isn't syntactically exhaustive.
            if (!isSyntacticallyExhaustive(doCatch)) {
              if (!doCatch->getBody()->walk(*this))
                return { false, nullptr };
            }

            // Walk into all the catch clauses.
            for (auto catchClause : doCatch->getCatches()) {
              if (!catchClause->walk(*this))
                return { false, nullptr };
            }

            // We've already walked all the children we care about.
            return { false, stmt };
          }
          
          return { true, stmt };
        }
        
      public:
        FindInnerThrows(ConstraintSystem &cs) : CS(cs) {}

        bool foundThrow() { return FoundThrow; }
      };
      
      if (expr->getThrowsLoc().isValid())
        return true;
      
      auto body = expr->getBody();
      
      if (!body)
        return false;
      
      auto tryFinder = FindInnerThrows(CS);
      body->walk(tryFinder);
      return tryFinder.foundThrow();
    }
    

    Type visitClosureExpr(ClosureExpr *expr) {
      
      // If a contextual function type exists, we can use that to obtain the
      // expected return type, rather than allocating a fresh type variable.
      auto contextualType = CS.getContextualType(expr);
      Type crt;
      
      if (contextualType) {
        if (auto cft = contextualType->getAs<AnyFunctionType>()) {
          crt = cft->getResult();
        }
      }
      
      // Closure expressions always have function type. In cases where a
      // parameter or return type is omitted, a fresh type variable is used to
      // stand in for that parameter or return type, allowing it to be inferred
      // from context.
      Type resultTy;
      if (expr->hasExplicitResultType() &&
          expr->getExplicitResultTypeLoc().getType()) {
        resultTy = expr->getExplicitResultTypeLoc().getType();
        CS.setFavoredType(expr, resultTy.getPointer());
      } else {
        auto locator =
          CS.getConstraintLocator(expr, ConstraintLocator::ClosureResult);

        if (expr->hasEmptyBody()) {
          resultTy = CS.createTypeVariable(locator, 0);

          // Closures with empty bodies should be inferred to return
          // ().
          CS.addConstraint(ConstraintKind::Bind, resultTy,
                           TupleType::getEmpty(CS.getASTContext()), locator);
        } else if (crt) {
          // Otherwise, use the contextual type if present.
          resultTy = crt;
        } else {
          // If no return type was specified, create a fresh type
          // variable for it.
          resultTy = CS.createTypeVariable(locator, 0);

          if (closureHasNoResult(expr)) {
            // Allow it to default to () if there are no return statements.
            CS.addConstraint(ConstraintKind::Defaultable, resultTy,
                             TupleType::getEmpty(CS.getASTContext()), locator);
          }
        }
      }

      // Give each parameter in a ClosureExpr a fresh type variable if parameter
      // types were not specified, and return the eventual function type.
      SmallVector<AnyFunctionType::Param, 4> paramTy;
      getClosureParams(expr, paramTy);

      auto extInfo = FunctionType::ExtInfo();
      if (closureCanThrow(expr))
        extInfo = extInfo.withThrows();

      return FunctionType::get(paramTy, resultTy, extInfo);
    }

    Type visitAutoClosureExpr(AutoClosureExpr *expr) {
      // AutoClosureExpr is introduced by CSApply.
      llvm_unreachable("Already type-checked");
    }

    Type visitInOutExpr(InOutExpr *expr) {
      // The address-of operator produces an explicit inout T from an lvalue T.
      // We model this with the constraint
      //
      //     S < lvalue T
      //
      // where T is a fresh type variable.
      auto lvalue = CS.createTypeVariable(CS.getConstraintLocator(expr),
                                          TVO_CanBindToNoEscape);
      auto bound = LValueType::get(lvalue);
      auto result = InOutType::get(lvalue);
      CS.addConstraint(ConstraintKind::Conversion,
                       CS.getType(expr->getSubExpr()), bound,
                       CS.getConstraintLocator(expr));
      return result;
    }

    Type visitVarargExpansionExpr(VarargExpansionExpr *expr) {
      // Create a fresh type variable.
      auto element = CS.createTypeVariable(CS.getConstraintLocator(expr),
                                           TVO_CanBindToNoEscape);

      // Try to build the appropriate type for a variadic argument list of
      // the fresh element type.  If that failed, just bail out.
      auto array = TypeChecker::getArraySliceType(expr->getLoc(), element);
      if (!array) return element;

      // Require the operand to be convertible to the array type.
      CS.addConstraint(ConstraintKind::Conversion,
                       CS.getType(expr->getSubExpr()), array,
                       CS.getConstraintLocator(expr));
      return array;
    }

    Type visitDynamicTypeExpr(DynamicTypeExpr *expr) {
      auto tv = CS.createTypeVariable(CS.getConstraintLocator(expr),
                                      TVO_CanBindToNoEscape);
      CS.addConstraint(ConstraintKind::DynamicTypeOf, tv,
                       CS.getType(expr->getBase()),
           CS.getConstraintLocator(expr, ConstraintLocator::RValueAdjustment));
      return tv;
    }

    Type visitOpaqueValueExpr(OpaqueValueExpr *expr) {
      assert(expr->isPlaceholder() && "Already type checked");
      return expr->getType();
    }

    Type visitDefaultArgumentExpr(DefaultArgumentExpr *expr) {
      return expr->getType();
    }

    Type visitApplyExpr(ApplyExpr *expr) {
      auto fnExpr = expr->getFn();

      SmallVector<Identifier, 4> scratch;
      associateArgumentLabels(
          CS.getConstraintLocator(expr),
          {expr->getArgumentLabels(scratch), expr->hasTrailingClosure()},
          /*labelsArePermanent=*/isa<CallExpr>(expr));

      if (auto *UDE = dyn_cast<UnresolvedDotExpr>(fnExpr)) {
        auto typeOperation = getTypeOperation(UDE, CS.getASTContext());
        if (typeOperation != TypeOperation::None)
          return resultOfTypeOperation(typeOperation, expr->getArg());
      }

      // The result type is a fresh type variable.
      Type resultType = CS.createTypeVariable(
          CS.getConstraintLocator(expr, ConstraintLocator::FunctionResult),
          TVO_CanBindToNoEscape);

      // A direct call to a ClosureExpr makes it noescape.
      FunctionType::ExtInfo extInfo;
      if (isa<ClosureExpr>(fnExpr->getSemanticsProvidingExpr()))
        extInfo = extInfo.withNoEscape();

      // FIXME: Redesign the AST so that an ApplyExpr directly stores a list of
      // arguments together with their inout-ness, instead of a single
      // ParenExpr or TupleExpr.
      SmallVector<AnyFunctionType::Param, 8> params;
      AnyFunctionType::decomposeInput(CS.getType(expr->getArg()), params);

      CS.addConstraint(ConstraintKind::ApplicableFunction,
                       FunctionType::get(params, resultType, extInfo),
                       CS.getType(expr->getFn()),
        CS.getConstraintLocator(expr, ConstraintLocator::ApplyFunction));

      // If we ended up resolving the result type variable to a concrete type,
      // set it as the favored type for this expression.
      Type fixedType =
          CS.getFixedTypeRecursive(resultType, /*wantRvalue=*/true);
      if (!fixedType->isTypeVariableOrMember()) {
        CS.setFavoredType(expr, fixedType.getPointer());
        resultType = fixedType;
      }

      return resultType;
    }

    Type getSuperType(VarDecl *selfDecl,
                      SourceLoc diagLoc,
                      Diag<> diag_not_in_class,
                      Diag<> diag_no_base_class) {
      DeclContext *typeContext = selfDecl->getDeclContext()->getParent();
      assert(typeContext && "constructor without parent context?!");

      auto &de = CS.getASTContext().Diags;
      ClassDecl *classDecl = typeContext->getSelfClassDecl();
      if (!classDecl) {
        de.diagnose(diagLoc, diag_not_in_class);
        return Type();
      }
      if (!classDecl->hasSuperclass()) {
        de.diagnose(diagLoc, diag_no_base_class);
        return Type();
      }

      auto selfTy = CS.DC->mapTypeIntoContext(
        typeContext->getDeclaredInterfaceType());
      auto superclassTy = selfTy->getSuperclass();

      if (selfDecl->getInterfaceType()->is<MetatypeType>())
        superclassTy = MetatypeType::get(superclassTy);

      return superclassTy;
    }
    
    Type visitRebindSelfInConstructorExpr(RebindSelfInConstructorExpr *expr) {
      // The result is void.
      return TupleType::getEmpty(CS.getASTContext());
    }
    
    Type visitIfExpr(IfExpr *expr) {
      // Condition must convert to Bool.
      auto boolDecl = CS.getASTContext().getBoolDecl();
      if (!boolDecl)
        return Type();

      CS.addConstraint(
          ConstraintKind::Conversion, CS.getType(expr->getCondExpr()),
          boolDecl->getDeclaredType(),
          CS.getConstraintLocator(expr, LocatorPathElt::Condition()));

      // The branches must be convertible to a common type.
      return CS.addJoinConstraint(CS.getConstraintLocator(expr),
          {
            { CS.getType(expr->getThenExpr()),
              CS.getConstraintLocator(expr->getThenExpr()) },
            { CS.getType(expr->getElseExpr()),
              CS.getConstraintLocator(expr->getElseExpr()) }
          });
    }
    
    virtual Type visitImplicitConversionExpr(ImplicitConversionExpr *expr) {
      llvm_unreachable("Already type-checked");
    }

    Type
    createTypeVariableAndDisjunctionForIUOCoercion(Type toType,
                                                   ConstraintLocator *locator) {
      auto typeVar = CS.createTypeVariable(locator, TVO_CanBindToNoEscape);
      CS.buildDisjunctionForImplicitlyUnwrappedOptional(typeVar, toType,
                                                        locator);
      return typeVar;
    }

    Type visitForcedCheckedCastExpr(ForcedCheckedCastExpr *expr) {
      auto fromExpr = expr->getSubExpr();
      if (!fromExpr) // Either wasn't constructed correctly or wasn't folded.
        return nullptr;

      // Validate the resulting type.
      TypeResolutionOptions options(TypeResolverContext::ExplicitCastExpr);
      options |= TypeResolutionFlags::AllowUnboundGenerics;
      if (TypeChecker::validateType(CS.getASTContext(),
                                    expr->getCastTypeLoc(),
                                    TypeResolution::forContextual(CS.DC),
                                    options))
        return nullptr;

      // Open the type we're casting to.
      auto toType = CS.openUnboundGenericType(expr->getCastTypeLoc().getType(),
                                              CS.getConstraintLocator(expr));
      CS.setType(expr->getCastTypeLoc(), toType);

      auto fromType = CS.getType(fromExpr);
      auto locator = CS.getConstraintLocator(expr);

      // The source type can be checked-cast to the destination type.
      CS.addConstraint(ConstraintKind::CheckedCast, fromType, toType, locator);

      // If the result type was declared IUO, add a disjunction for
      // bindings for the result of the coercion.
      auto *TR = expr->getCastTypeLoc().getTypeRepr();
      if (TR && TR->getKind() == TypeReprKind::ImplicitlyUnwrappedOptional)
        return createTypeVariableAndDisjunctionForIUOCoercion(toType, locator);

      return toType;
    }

    Type visitCoerceExpr(CoerceExpr *expr) {
      // Validate the resulting type.
      TypeResolutionOptions options(TypeResolverContext::ExplicitCastExpr);
      options |= TypeResolutionFlags::AllowUnboundGenerics;
      if (TypeChecker::validateType(CS.getASTContext(),
                                    expr->getCastTypeLoc(),
                                    TypeResolution::forContextual(CS.DC),
                                    options))
        return nullptr;

      // Open the type we're casting to.
      auto toType = CS.openUnboundGenericType(expr->getCastTypeLoc().getType(),
                                              CS.getConstraintLocator(expr));
      CS.setType(expr->getCastTypeLoc(), toType);

      auto fromType = CS.getType(expr->getSubExpr());
      auto locator = CS.getConstraintLocator(expr);

      // Add a conversion constraint for the direct conversion between
      // types.
      CS.addExplicitConversionConstraint(fromType, toType,
                                         /*allowFixes=*/true, locator);

      // If the result type was declared IUO, add a disjunction for
      // bindings for the result of the coercion.
      auto *TR = expr->getCastTypeLoc().getTypeRepr();
      if (TR && TR->getKind() == TypeReprKind::ImplicitlyUnwrappedOptional)
        return createTypeVariableAndDisjunctionForIUOCoercion(toType, locator);

      return toType;
    }

    Type visitConditionalCheckedCastExpr(ConditionalCheckedCastExpr *expr) {
      auto &ctx = CS.getASTContext();
      auto fromExpr = expr->getSubExpr();
      if (!fromExpr) // Either wasn't constructed correctly or wasn't folded.
        return nullptr;

      std::function<Expr *(Expr *)> nilLiteralExpr = [&](Expr *expr) -> Expr * {
        expr = expr->getSemanticsProvidingExpr();
        if (expr->getKind() == ExprKind::NilLiteral)
          return expr;

        if (auto *optionalEvalExpr = dyn_cast<OptionalEvaluationExpr>(expr))
          return nilLiteralExpr(optionalEvalExpr->getSubExpr());

        if (auto *bindOptionalExpr = dyn_cast<BindOptionalExpr>(expr))
          return nilLiteralExpr(bindOptionalExpr->getSubExpr());

        return nullptr;
      };

      if (auto nilLiteral = nilLiteralExpr(fromExpr)) {
        ctx.Diags.diagnose(nilLiteral->getLoc(),
                           diag::conditional_cast_from_nil);
        return nullptr;
      }

      // Validate the resulting type.
      TypeResolutionOptions options(TypeResolverContext::ExplicitCastExpr);
      options |= TypeResolutionFlags::AllowUnboundGenerics;
      if (TypeChecker::validateType(ctx,
                                    expr->getCastTypeLoc(),
                                    TypeResolution::forContextual(CS.DC),
                                    options))
        return nullptr;

      // Open the type we're casting to.
      auto toType = CS.openUnboundGenericType(expr->getCastTypeLoc().getType(),
                                              CS.getConstraintLocator(expr));
      CS.setType(expr->getCastTypeLoc(), toType);

      auto fromType = CS.getType(fromExpr);
      auto locator = CS.getConstraintLocator(expr);

      CS.addConstraint(ConstraintKind::CheckedCast, fromType, toType, locator);

      // If the result type was declared IUO, add a disjunction for
      // bindings for the result of the coercion.
      auto *TR = expr->getCastTypeLoc().getTypeRepr();
      if (TR && TR->getKind() == TypeReprKind::ImplicitlyUnwrappedOptional)
        return createTypeVariableAndDisjunctionForIUOCoercion(
            OptionalType::get(toType), locator);

      return OptionalType::get(toType);
    }

    Type visitIsExpr(IsExpr *expr) {
      // Validate the type.
      auto &ctx = CS.getASTContext();
      TypeResolutionOptions options(TypeResolverContext::ExplicitCastExpr);
      options |= TypeResolutionFlags::AllowUnboundGenerics;
      if (TypeChecker::validateType(ctx,
                                    expr->getCastTypeLoc(),
                                    TypeResolution::forContextual(CS.DC),
                                    options))
        return nullptr;

      // Open up the type we're checking.
      // FIXME: Locator for the cast type?
      auto toType = CS.openUnboundGenericType(expr->getCastTypeLoc().getType(),
                                              CS.getConstraintLocator(expr));
      CS.setType(expr->getCastTypeLoc(), toType);

      // Add a checked cast constraint.
      auto fromType = CS.getType(expr->getSubExpr());
      
      CS.addConstraint(ConstraintKind::CheckedCast, fromType, toType,
                       CS.getConstraintLocator(expr));

      // The result is Bool.
      auto boolDecl = ctx.getBoolDecl();

      if (!boolDecl) {
        ctx.Diags.diagnose(SourceLoc(), diag::broken_bool);
        return Type();
      }

      return boolDecl->getDeclaredType();
    }

    Type visitDiscardAssignmentExpr(DiscardAssignmentExpr *expr) {
      auto locator = CS.getConstraintLocator(expr);
      auto typeVar = CS.createTypeVariable(locator, TVO_CanBindToNoEscape);
      return LValueType::get(typeVar);
    }

    static Type genAssignDestType(Expr *expr, ConstraintSystem &CS) {
      if (auto *TE = dyn_cast<TupleExpr>(expr)) {
        SmallVector<TupleTypeElt, 4> destTupleTypes;
        for (unsigned i = 0; i !=  TE->getNumElements(); ++i) {
          Type subType = genAssignDestType(TE->getElement(i), CS);
          destTupleTypes.push_back(TupleTypeElt(subType, TE->getElementName(i)));
        }
        return TupleType::get(destTupleTypes, CS.getASTContext());
      } else {
        auto *locator = CS.getConstraintLocator(expr);

        auto isOrCanBeLValueType = [](Type type) {
          if (auto *typeVar = type->getAs<TypeVariableType>()) {
            return typeVar->getImpl().canBindToLValue();
          }
          return type->is<LValueType>();
        };

        auto exprType = CS.getType(expr);
        if (!isOrCanBeLValueType(exprType)) {
          // Pretend that destination is an l-value type.
          exprType = LValueType::get(exprType);
          (void)CS.recordFix(TreatRValueAsLValue::create(CS, locator));
        }

        auto *destTy = CS.createTypeVariable(locator, TVO_CanBindToNoEscape);
        CS.addConstraint(ConstraintKind::Bind, LValueType::get(destTy),
                         exprType, locator);
        return destTy;
      }
    }

    Type visitAssignExpr(AssignExpr *expr) {
      // Handle invalid code.
      if (!expr->getDest() || !expr->getSrc())
        return Type();
      Type destTy = genAssignDestType(expr->getDest(), CS);
      CS.addConstraint(ConstraintKind::Conversion,
                       CS.getType(expr->getSrc()), destTy,
                       CS.getConstraintLocator(expr));
      return TupleType::getEmpty(CS.getASTContext());
    }
    
    Type visitUnresolvedPatternExpr(UnresolvedPatternExpr *expr) {
      // If there are UnresolvedPatterns floating around after name binding,
      // they are pattern productions in invalid positions. However, we will
      // diagnose that condition elsewhere; to avoid unnecessary noise errors,
      // just plop an open type variable here.
      
      auto locator = CS.getConstraintLocator(expr);
      auto typeVar = CS.createTypeVariable(locator,
                                           TVO_CanBindToLValue |
                                           TVO_CanBindToNoEscape);
      return typeVar;
    }

    /// Get the type T?
    ///
    ///  This is not the ideal source location, but it's only used for
    /// diagnosing ill-formed standard libraries, so it really isn't
    /// worth QoI efforts.
    Type getOptionalType(SourceLoc optLoc, Type valueTy) {
      auto optTy = TypeChecker::getOptionalType(optLoc, valueTy);
      if (!optTy ||
          TypeChecker::requireOptionalIntrinsics(CS.getASTContext(), optLoc))
        return Type();

      return optTy;
    }

    Type visitBindOptionalExpr(BindOptionalExpr *expr) {
      // The operand must be coercible to T?, and we will have type T.
      auto locator = CS.getConstraintLocator(expr);

      auto objectTy = CS.createTypeVariable(locator,
                                            TVO_PrefersSubtypeBinding |
                                            TVO_CanBindToLValue |
                                            TVO_CanBindToNoEscape);
      
      // The result is the object type of the optional subexpression.
      CS.addConstraint(ConstraintKind::OptionalObject,
                       CS.getType(expr->getSubExpr()), objectTy,
                       locator);
      return objectTy;
    }
    
    Type visitOptionalEvaluationExpr(OptionalEvaluationExpr *expr) {
      // The operand must be coercible to T? for some type T.  We'd
      // like this to be the smallest possible nesting level of
      // optional types, e.g. T? over T??; otherwise we don't really
      // have a preference.
      auto valueTy = CS.createTypeVariable(CS.getConstraintLocator(expr),
                                           TVO_PrefersSubtypeBinding |
                                           TVO_CanBindToNoEscape);

      Type optTy = getOptionalType(expr->getSubExpr()->getLoc(), valueTy);
      if (!optTy)
        return Type();

      CS.addConstraint(ConstraintKind::Conversion,
                       CS.getType(expr->getSubExpr()), optTy,
                       CS.getConstraintLocator(expr));
      return optTy;
    }

    Type visitForceValueExpr(ForceValueExpr *expr) {
      // Force-unwrap an optional of type T? to produce a T.
      auto locator = CS.getConstraintLocator(expr);

      auto objectTy = CS.createTypeVariable(locator,
                                            TVO_PrefersSubtypeBinding |
                                            TVO_CanBindToLValue |
                                            TVO_CanBindToNoEscape);
      
      // The result is the object type of the optional subexpression.
      CS.addConstraint(ConstraintKind::OptionalObject,
                       CS.getType(expr->getSubExpr()), objectTy,
                       locator);
      return objectTy;
    }

    Type visitOpenExistentialExpr(OpenExistentialExpr *expr) {
      llvm_unreachable("Already type-checked");
    }
    Type visitMakeTemporarilyEscapableExpr(MakeTemporarilyEscapableExpr *expr) {
      llvm_unreachable("Already type-checked");
    }
    Type visitKeyPathApplicationExpr(KeyPathApplicationExpr *expr) {
      // This should only appear in already-type-checked solutions, but we may
      // need to re-check for failure diagnosis.
      auto locator = CS.getConstraintLocator(expr);
      auto projectedTy = CS.createTypeVariable(locator,
                                               TVO_CanBindToLValue |
                                               TVO_CanBindToNoEscape);
      CS.addKeyPathApplicationConstraint(CS.getType(expr->getKeyPath()),
                                         CS.getType(expr->getBase()),
                                         projectedTy,
                                         locator);
      return projectedTy;
    }
    
    Type visitEnumIsCaseExpr(EnumIsCaseExpr *expr) {
      return CS.getASTContext().getBoolDecl()->getDeclaredType();
    }

    Type visitLazyInitializerExpr(LazyInitializerExpr *expr) {
      llvm_unreachable("Already type-checked");
    }

    Type visitEditorPlaceholderExpr(EditorPlaceholderExpr *E) {
      if (E->getTypeLoc().isNull()) {
        auto locator = CS.getConstraintLocator(E);

        // A placeholder may have any type, but default to Void type if
        // otherwise unconstrained.
        auto &placeholderTy
          = editorPlaceholderVariables[currentEditorPlaceholderVariable];
        if (!placeholderTy) {
          placeholderTy = CS.createTypeVariable(locator, TVO_CanBindToNoEscape);

          CS.addConstraint(ConstraintKind::Defaultable,
                           placeholderTy,
                           TupleType::getEmpty(CS.getASTContext()),
                           locator);
        }

        // Move to the next placeholder variable.
        currentEditorPlaceholderVariable
          = (currentEditorPlaceholderVariable + 1) %
              numEditorPlaceholderVariables;

        return placeholderTy;
      }

      // NOTE: The type loc may be there but have failed to validate, in which
      // case we return the null type.
      return E->getType();
    }

    Type visitObjCSelectorExpr(ObjCSelectorExpr *E) {
      // #selector only makes sense when we have the Objective-C
      // runtime.
      auto &ctx = CS.getASTContext();
      if (!ctx.LangOpts.EnableObjCInterop) {
        ctx.Diags.diagnose(E->getLoc(), diag::expr_selector_no_objc_runtime);
        return nullptr;
      }

      
      // Make sure we can reference ObjectiveC.Selector.
      // FIXME: Fix-It to add the import?
      auto type = CS.getASTContext().getSelectorType();
      if (!type) {
        ctx.Diags.diagnose(E->getLoc(), diag::expr_selector_module_missing);
        return nullptr;
      }

      return type;
    }

    Type visitKeyPathExpr(KeyPathExpr *E) {
      if (E->isObjC())
        return CS.getType(E->getObjCStringLiteralExpr());
      
      auto kpDecl = CS.getASTContext().getKeyPathDecl();
      
      if (!kpDecl) {
        auto &de = CS.getASTContext().Diags;
        de.diagnose(E->getLoc(), diag::expr_keypath_no_keypath_type);
        return ErrorType::get(CS.getASTContext());
      }
      
      // For native key paths, traverse the key path components to set up
      // appropriate type relationships at each level.
      auto rootLocator =
          CS.getConstraintLocator(E, ConstraintLocator::KeyPathRoot);
      auto locator = CS.getConstraintLocator(E);
      Type root = CS.createTypeVariable(rootLocator, TVO_CanBindToNoEscape);

      // If a root type was explicitly given, then resolve it now.
      if (auto rootRepr = E->getRootType()) {
        auto rootObjectTy = resolveTypeReferenceInExpression(rootRepr);
        if (!rootObjectTy || rootObjectTy->hasError())
          return Type();
        rootObjectTy = CS.openUnboundGenericType(rootObjectTy, locator);
        // Allow \Derived.property to be inferred as \Base.property to
        // simulate a sort of covariant conversion from
        // KeyPath<Derived, T> to KeyPath<Base, T>.
        CS.addConstraint(ConstraintKind::Subtype, rootObjectTy, root, locator);
      }
      
      bool didOptionalChain = false;
      // We start optimistically from an lvalue base.
      Type base = LValueType::get(root);

      SmallVector<TypeVariableType *, 2> componentTypeVars;
      for (unsigned i : indices(E->getComponents())) {
        auto &component = E->getComponents()[i];
        auto memberLocator = CS.getConstraintLocator(
            locator, LocatorPathElt::KeyPathComponent(i));
        auto resultLocator = CS.getConstraintLocator(
            memberLocator, ConstraintLocator::KeyPathComponentResult);

        switch (auto kind = component.getKind()) {
        case KeyPathExpr::Component::Kind::Invalid:
          break;
        
        case KeyPathExpr::Component::Kind::UnresolvedProperty:
        // This should only appear in resolved ASTs, but we may need to
        // re-type-check the constraints during failure diagnosis.
        case KeyPathExpr::Component::Kind::Property: {
          auto memberTy = CS.createTypeVariable(resultLocator,
                                                TVO_CanBindToLValue |
                                                TVO_CanBindToNoEscape);
          componentTypeVars.push_back(memberTy);
          auto lookupName = kind == KeyPathExpr::Component::Kind::UnresolvedProperty
            ? component.getUnresolvedDeclName()
            : component.getDeclRef().getDecl()->getFullName();
          
          auto refKind = lookupName.isSimpleName()
            ? FunctionRefKind::Unapplied
            : FunctionRefKind::Compound;
          CS.addValueMemberConstraint(base, lookupName,
                                      memberTy,
                                      CurDC,
                                      refKind,
                                      /*outerAlternatives=*/{},
                                      memberLocator);
          base = memberTy;
          break;
        }
          
        case KeyPathExpr::Component::Kind::UnresolvedSubscript:
        // Subscript should only appear in resolved ASTs, but we may need to
        // re-type-check the constraints during failure diagnosis.
        case KeyPathExpr::Component::Kind::Subscript: {
          base = addSubscriptConstraints(E, base, component.getIndexExpr(),
                                         /*decl*/ nullptr,
                                         component.getSubscriptLabels(),
                                         /*hasTrailingClosure*/ false,
                                         memberLocator,
                                         &componentTypeVars);
          break;
        }

        case KeyPathExpr::Component::Kind::TupleElement: {
          // Note: If implemented, the logic in `getCalleeLocator` will need
          // updating to return the correct callee locator for this.
          llvm_unreachable("not implemented");
          break;
        }
                
        case KeyPathExpr::Component::Kind::OptionalChain: {
          didOptionalChain = true;

          // We can't assign an optional back through an optional chain
          // today. Force the base to an rvalue.
          auto rvalueTy = CS.createTypeVariable(resultLocator,
                                                TVO_CanBindToNoEscape);
          componentTypeVars.push_back(rvalueTy);
          CS.addConstraint(ConstraintKind::Equal, base, rvalueTy,
                           resultLocator);

          base = rvalueTy;
          LLVM_FALLTHROUGH;
        }
        case KeyPathExpr::Component::Kind::OptionalForce: {
          auto optionalObjTy = CS.createTypeVariable(resultLocator,
                                                     TVO_CanBindToLValue |
                                                     TVO_CanBindToNoEscape);
          componentTypeVars.push_back(optionalObjTy);

          CS.addConstraint(ConstraintKind::OptionalObject, base, optionalObjTy,
                           resultLocator);
          base = optionalObjTy;
          break;
        }
        
        case KeyPathExpr::Component::Kind::OptionalWrap: {
          // This should only appear in resolved ASTs, but we may need to
          // re-type-check the constraints during failure diagnosis.
          base = OptionalType::get(base);
          break;
        }
        case KeyPathExpr::Component::Kind::Identity:
          continue;
        }

        // By now, `base` is the result type of this component. Set it in the
        // constraint system so we can find it later.
        CS.setType(E, i, base);
      }
      
      // If there was an optional chaining component, the end result must be
      // optional.
      if (didOptionalChain) {
        auto objTy = CS.createTypeVariable(locator, TVO_CanBindToNoEscape);
        componentTypeVars.push_back(objTy);

        auto optTy = OptionalType::get(objTy);
        CS.addConstraint(ConstraintKind::Conversion, base, optTy,
                         locator);
        base = optTy;
      }

      auto baseLocator =
          CS.getConstraintLocator(E, ConstraintLocator::KeyPathValue);
      auto rvalueBase = CS.createTypeVariable(baseLocator,
                                              TVO_CanBindToNoEscape);
      CS.addConstraint(ConstraintKind::Equal, base, rvalueBase, locator);

      // The result is a KeyPath from the root to the end component.
      // The type of key path depends on the overloads chosen for the key
      // path components.
      auto typeLoc =
          CS.getConstraintLocator(locator, ConstraintLocator::KeyPathType);
      Type kpTy = CS.createTypeVariable(typeLoc, TVO_CanBindToNoEscape);
      CS.addKeyPathConstraint(kpTy, root, rvalueBase, componentTypeVars,
                              locator);
      return kpTy;
    }

    Type visitKeyPathDotExpr(KeyPathDotExpr *E) {
      llvm_unreachable("found KeyPathDotExpr in CSGen");
    }

    Type visitOneWayExpr(OneWayExpr *expr) {
      auto locator = CS.getConstraintLocator(expr);
      auto resultTypeVar = CS.createTypeVariable(locator, 0);
      CS.addConstraint(ConstraintKind::OneWayEqual, resultTypeVar,
                       CS.getType(expr->getSubExpr()), locator);
      return resultTypeVar;
    }

    Type visitTapExpr(TapExpr *expr) {
      DeclContext *varDC = expr->getVar()->getDeclContext();
      assert(varDC == CS.DC || (varDC && isa<AbstractClosureExpr>(varDC)) &&
             "TapExpr var should be in the same DeclContext we're checking it in!");
      
      auto locator = CS.getConstraintLocator(expr);
      auto tv = CS.createTypeVariable(locator, TVO_CanBindToNoEscape);

      if (auto subExpr = expr->getSubExpr()) {
        auto subExprType = CS.getType(subExpr);
        CS.addConstraint(ConstraintKind::Bind, subExprType, tv, locator);
      }

      return tv;
    }

    static bool isTriggerFallbackDiagnosticBuiltin(UnresolvedDotExpr *UDE,
                                                   ASTContext &Context) {
      auto *DRE = dyn_cast<DeclRefExpr>(UDE->getBase());
      if (!DRE)
        return false;

      if (DRE->getDecl() != Context.TheBuiltinModule)
        return false;

      auto member = UDE->getName().getBaseName().userFacingName();
      return member.equals("trigger_fallback_diagnostic");
    }

    enum class TypeOperation { None,
                               Join,
                               JoinInout,
                               JoinMeta,
                               JoinNonexistent,
                               OneWay,
    };

    static TypeOperation getTypeOperation(UnresolvedDotExpr *UDE,
                                          ASTContext &Context) {
      auto *DRE = dyn_cast<DeclRefExpr>(UDE->getBase());
      if (!DRE)
        return TypeOperation::None;

      if (DRE->getDecl() != Context.TheBuiltinModule)
        return TypeOperation::None;

      return llvm::StringSwitch<TypeOperation>(
                 UDE->getName().getBaseIdentifier().str())
          .Case("one_way", TypeOperation::OneWay)
          .Case("type_join", TypeOperation::Join)
          .Case("type_join_inout", TypeOperation::JoinInout)
          .Case("type_join_meta", TypeOperation::JoinMeta)
          .Case("type_join_nonexistent", TypeOperation::JoinNonexistent)
          .Default(TypeOperation::None);
    }

    Type resultOfTypeOperation(TypeOperation op, Expr *Arg) {
      auto *tuple = cast<TupleExpr>(Arg);

      auto *lhs = tuple->getElement(0);
      auto *rhs = tuple->getElement(1);

      switch (op) {
      case TypeOperation::None:
      case TypeOperation::OneWay:
        llvm_unreachable(
            "We should have a valid type operation at this point!");

      case TypeOperation::Join: {
        auto lhsMeta = CS.getType(lhs)->getAs<MetatypeType>();
        auto rhsMeta = CS.getType(rhs)->getAs<MetatypeType>();
        if (!lhsMeta || !rhsMeta)
          llvm_unreachable("Unexpected argument types for Builtin.type_join!");

        auto &ctx = lhsMeta->getASTContext();

        auto join =
            Type::join(lhsMeta->getInstanceType(), rhsMeta->getInstanceType());

        if (!join)
          return ErrorType::get(ctx);

        return MetatypeType::get(*join, ctx)->getCanonicalType();
      }

      case TypeOperation::JoinInout: {
        auto lhsInOut = CS.getType(lhs)->getAs<InOutType>();
        auto rhsMeta = CS.getType(rhs)->getAs<MetatypeType>();
        if (!lhsInOut || !rhsMeta)
          llvm_unreachable("Unexpected argument types for Builtin.type_join!");

        auto &ctx = lhsInOut->getASTContext();

        auto join =
            Type::join(lhsInOut, rhsMeta->getInstanceType());

        if (!join)
          return ErrorType::get(ctx);

        return MetatypeType::get(*join, ctx)->getCanonicalType();
      }

      case TypeOperation::JoinMeta: {
        auto lhsMeta = CS.getType(lhs)->getAs<MetatypeType>();
        auto rhsMeta = CS.getType(rhs)->getAs<MetatypeType>();
        if (!lhsMeta || !rhsMeta)
          llvm_unreachable("Unexpected argument types for Builtin.type_join!");

        auto &ctx = lhsMeta->getASTContext();

        auto join = Type::join(lhsMeta, rhsMeta);

        if (!join)
          return ErrorType::get(ctx);

        return *join;
      }

      case TypeOperation::JoinNonexistent: {
        auto lhsMeta = CS.getType(lhs)->getAs<MetatypeType>();
        auto rhsMeta = CS.getType(rhs)->getAs<MetatypeType>();
        if (!lhsMeta || !rhsMeta)
          llvm_unreachable("Unexpected argument types for Builtin.type_join_nonexistent!");

        auto &ctx = lhsMeta->getASTContext();

        auto join =
            Type::join(lhsMeta->getInstanceType(), rhsMeta->getInstanceType());

        // Verify that we could not compute a join.
        if (join)
          llvm_unreachable("Unexpected result from join - it should not have been computable!");

        // The return value is unimportant.
        return MetatypeType::get(ctx.TheAnyType)->getCanonicalType();
      }
      }
      llvm_unreachable("unhandled operation");
    }

    void associateArgumentLabels(ConstraintLocator *locator,
                                 ConstraintSystem::ArgumentInfo info,
                                 bool labelsArePermanent = true) {
      assert(locator && locator->getAnchor());
      // Record the labels.
      if (!labelsArePermanent)
        info.Labels = CS.allocateCopy(info.Labels);
      CS.ArgumentInfos[CS.getArgumentInfoLocator(locator)] = info;
    }
  };

  /// AST walker that "sanitizes" an expression for the
  /// constraint-based type checker.
  ///
  /// This is necessary because Sema fills in too much type information before
  /// the type-checker runs, causing redundant work, and for expression that
  /// have already been typechecked and may contain unhandled AST nodes.
  ///
  /// FIXME: Remove this one we no longer re-type check expressions during
  /// diagnostics and code completion.
  class SanitizeExpr : public ASTWalker {
    ConstraintSystem &CS;
    const bool eraseOpenExistentialsOnly;
    llvm::SmallDenseMap<OpaqueValueExpr *, Expr *, 4> OpenExistentials;

  public:
    SanitizeExpr(ConstraintSystem &cs, bool eraseOEsOnly = false)
        : CS(cs), eraseOpenExistentialsOnly(eraseOEsOnly) { }

    ASTContext &getASTContext() const { return CS.getASTContext(); }

    std::pair<bool, Expr *> walkToExprPre(Expr *expr) override {
      while (true) {

        // If we should reuse pre-checked types, don't sanitize the expression
        // if it's already type-checked.
        if (CS.shouldReusePrecheckedType() && expr->getType())
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
            assert((eraseOpenExistentialsOnly || OVE->isPlaceholder()) &&
                   "Didn't see this OVE in a containing OpenExistentialExpr?");
            // NOTE: In 'eraseOpenExistentialsOnly' mode, ASTWalker may walk
            // into other kind of expressions holding OVE.
          }
        }

        if (eraseOpenExistentialsOnly)
          return {true, expr};

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

        // Restore '@autoclosure'd value.
        if (auto ACE = dyn_cast<AutoClosureExpr>(expr)) {
          expr = ACE->getSingleExpressionBody();
          continue;
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
                TE->getElementName(0) == getASTContext().Id_dynamicMember &&
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
          new (getASTContext()) ParenExpr(argList.lParenLoc,
                                          argList.args[0],
                                          argList.rParenLoc,
                                          argList.hasTrailingClosure);
        result->setImplicit();
        return result;
      }

      return TupleExpr::create(getASTContext(),
                               argList.lParenLoc,
                               argList.args,
                               argList.labels,
                               argList.labelLocs,
                               argList.rParenLoc,
                               argList.hasTrailingClosure,
                               /*implicit=*/true);
    }

    Expr *walkToExprPost(Expr *expr) override {
      if (CS.hasType(expr)) {
        Type type = CS.getType(expr);
        if (type->hasOpenedExistential()) {
          type = type.transform([&](Type type) -> Type {
            if (auto archetype = type->getAs<OpenedArchetypeType>())
              return archetype->getOpenedExistentialType();
            return type;
          });
          CS.setType(expr, type);
          // Set new type to the expression directly.
          expr->setType(type);
        }
      }

      if (eraseOpenExistentialsOnly)
        return expr;

      assert(!isa<ImplicitConversionExpr>(expr) &&
             "ImplicitConversionExpr should be eliminated in walkToExprPre");

      auto buildMemberRef = [&](Type memberType, Expr *base, SourceLoc dotLoc,
                                ConcreteDeclRef member, DeclNameLoc memberLoc,
                                bool implicit) -> Expr * {
        auto *memberRef = new (getASTContext())
            MemberRefExpr(base, dotLoc, member, memberLoc, implicit);

        if (memberType) {
          memberRef->setType(memberType);
          return CS.cacheType(memberRef);
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

    // Don't walk into statements.  This handles the BraceStmt in
    // non-single-expr closures, so we don't walk into their body.
    std::pair<bool, Stmt *> walkToStmtPre(Stmt *S) override {
      return { false, S };
    }
  };

  class ConstraintWalker : public ASTWalker {
    ConstraintGenerator &CG;

  public:
    ConstraintWalker(ConstraintGenerator &CG) : CG(CG) { }

    std::pair<bool, Expr *> walkToExprPre(Expr *expr) override {

      if (CG.getConstraintSystem().shouldReusePrecheckedType()) {
        if (expr->getType()) {
          assert(!expr->getType()->hasTypeVariable());
          CG.getConstraintSystem().cacheType(expr);
          return { false, expr };
        }
      }

      // Note that the subexpression of a #selector expression is
      // unevaluated.
      if (auto sel = dyn_cast<ObjCSelectorExpr>(expr)) {
        CG.getConstraintSystem().UnevaluatedRootExprs.insert(sel->getSubExpr());
      }

      // Check an objc key-path expression, which fills in its semantic
      // expression as a string literal.
      if (auto keyPath = dyn_cast<KeyPathExpr>(expr)) {
        if (keyPath->isObjC()) {
          auto &cs = CG.getConstraintSystem();
          (void)TypeChecker::checkObjCKeyPathExpr(cs.DC, keyPath);
        }
      }

      // For closures containing only a single expression, the body participates
      // in type checking.
      if (auto closure = dyn_cast<ClosureExpr>(expr)) {
        auto &CS = CG.getConstraintSystem();
        if (closure->hasSingleExpressionBody()) {
          CG.enterClosure(closure);

          // Visit the closure itself, which produces a function type.
          auto funcTy = CG.visit(expr)->castTo<FunctionType>();
          CS.setType(expr, funcTy);
        }

        return { true, expr };
      }

      // Don't visit CoerceExpr with an empty sub expression. They may occur
      // if the body of a closure was not visited while pre-checking because
      // of an error in the closure's signature.
      if (auto coerceExpr = dyn_cast<CoerceExpr>(expr)) {
        if (!coerceExpr->getSubExpr()) {
          return { false, expr };
        }
      }

      // Don't visit IfExpr with empty sub expressions. They may occur
      // if the body of a closure was not visited while pre-checking because
      // of an error in the closure's signature.
      if (auto ifExpr = dyn_cast<IfExpr>(expr)) {
        if (!ifExpr->getThenExpr() || !ifExpr->getElseExpr())
          return { false, expr };
      }

      return { true, expr };
    }

    /// Once we've visited the children of the given expression,
    /// generate constraints from the expression.
    Expr *walkToExprPost(Expr *expr) override {
      // Translate special type-checker Builtin calls into simpler expressions.
      if (auto *apply = dyn_cast<ApplyExpr>(expr)) {
        auto fnExpr = apply->getFn();
        if (auto *UDE = dyn_cast<UnresolvedDotExpr>(fnExpr)) {
          auto &CS = CG.getConstraintSystem();
          auto typeOperation =
              ConstraintGenerator::getTypeOperation(UDE, CS.getASTContext());

          if (typeOperation == ConstraintGenerator::TypeOperation::OneWay) {
            // For a one-way constraint, create the OneWayExpr node.
            auto *arg = cast<ParenExpr>(apply->getArg())->getSubExpr();
            expr = new (CS.getASTContext()) OneWayExpr(arg);
          } else if (typeOperation !=
                         ConstraintGenerator::TypeOperation::None) {
            // Handle the Builtin.type_join* family of calls by replacing
            // them with dot_self_expr of type_expr with the type being the
            // result of the join.
            auto joinMetaTy =
                CG.resultOfTypeOperation(typeOperation, apply->getArg());
            auto joinTy = joinMetaTy->castTo<MetatypeType>()->getInstanceType();

            auto *TE = TypeExpr::createImplicit(joinTy, CS.getASTContext());
            CS.cacheType(TE);
            CS.setType(TE->getTypeLoc(), joinTy);

            auto *DSE = new (CS.getASTContext())
                DotSelfExpr(TE, SourceLoc(), SourceLoc(), CS.getType(TE));
            DSE->setImplicit();
            CS.cacheType(DSE);

            return DSE;
          }
        }
      }

      if (auto closure = dyn_cast<ClosureExpr>(expr)) {
        if (closure->hasSingleExpressionBody()) {
          CG.exitClosure(closure);

          auto &CS = CG.getConstraintSystem();
          Type closureTy = CS.getType(closure);

          // If the function type has an error in it, we don't want to solve the
          // system.
          if (closureTy && closureTy->hasError())
            return nullptr;

          // Visit the body. It's type needs to be convertible to the function's
          // return type.
          auto resultTy = closureTy->castTo<FunctionType>()->getResult();
          Type bodyTy = CS.getType(closure->getSingleExpressionBody());
          CG.getConstraintSystem().setFavoredType(expr, bodyTy.getPointer());
          CG.getConstraintSystem()
            .addConstraint(ConstraintKind::Conversion, bodyTy,
                           resultTy,
                           CG.getConstraintSystem()
                             .getConstraintLocator(
                               expr,
                               ConstraintLocator::ClosureResult));
          return expr;
        }
      }

      if (auto type = CG.visit(expr)) {
        auto &CS = CG.getConstraintSystem();
        auto simplifiedType = CS.simplifyType(type);

        CS.setType(expr, simplifiedType);

        return expr;
      }

      return nullptr;
    }

    /// Ignore statements.
    std::pair<bool, Stmt *> walkToStmtPre(Stmt *stmt) override {
      return { false, stmt };
    }

    /// Ignore declarations.
    bool walkToDeclPre(Decl *decl) override { return false; }
  };
} // end anonymous namespace

Expr *ConstraintSystem::generateConstraints(Expr *expr, DeclContext *dc) {
  InputExprs.insert(expr);

  // Remove implicit conversions from the expression.
  expr = expr->walk(SanitizeExpr(*this));

  // Walk the expression, generating constraints.
  ConstraintGenerator cg(*this, dc);
  ConstraintWalker cw(cg);
  
  Expr* result = expr->walk(cw);
  
  if (result)
    this->optimizeConstraints(result);

  return result;
}

Type ConstraintSystem::generateConstraints(Pattern *pattern,
                                           ConstraintLocatorBuilder locator) {
  ConstraintGenerator cg(*this, nullptr);
  return cg.getTypeForPattern(pattern, locator);
}

void ConstraintSystem::optimizeConstraints(Expr *e) {
  if (getASTContext().TypeCheckerOpts.DisableConstraintSolverPerformanceHacks)
    return;
  
  SmallVector<Expr *, 16> linkedExprs;
  
  // Collect any linked expressions.
  LinkedExprCollector collector(linkedExprs, *this);
  e->walk(collector);
  
  // Favor types, as appropriate.
  for (auto linkedExpr : linkedExprs) {
    computeFavoredTypeForExpr(linkedExpr, *this);
  }
  
  // Optimize the constraints.
  ConstraintOptimizer optimizer(*this);
  e->walk(optimizer);
}

bool swift::areGenericRequirementsSatisfied(
    const DeclContext *DC, GenericSignature sig,
    SubstitutionMap Substitutions, bool isExtension) {

  ConstraintSystemOptions Options;
  ConstraintSystem CS(const_cast<DeclContext *>(DC), Options);
  auto Loc = CS.getConstraintLocator(nullptr);

  // For every requirement, add a constraint.
  for (auto Req : sig->getRequirements()) {
    if (auto resolved = Req.subst(
          QuerySubstitutionMap{Substitutions},
          LookUpConformanceInModule(DC->getParentModule()))) {
      CS.addConstraint(*resolved, Loc);
    } else if (isExtension) {
      return false;
    }
    // Unresolved requirements are requirements of the function itself. This
    // does not prevent it from being applied. E.g. func foo<T: Sequence>(x: T).
  }

  // Having a solution implies the requirements have been fulfilled.
  return CS.solveSingle().hasValue();
}

void swift::eraseOpenedExistentials(ConstraintSystem &CS, Expr *&expr) {
  expr = expr->walk(SanitizeExpr(CS, /*eraseOEsOnly=*/true));
}

struct ResolvedMemberResult::Implementation {
  llvm::SmallVector<ValueDecl*, 4> AllDecls;
  unsigned ViableStartIdx;
  Optional<unsigned> BestIdx;
};

ResolvedMemberResult::ResolvedMemberResult(): Impl(new Implementation()) {};

ResolvedMemberResult::~ResolvedMemberResult() { delete Impl; };

ResolvedMemberResult::operator bool() const {
  return !Impl->AllDecls.empty();
}

bool ResolvedMemberResult::
hasBestOverload() const { return Impl->BestIdx.hasValue(); }

ValueDecl* ResolvedMemberResult::
getBestOverload() const { return Impl->AllDecls[Impl->BestIdx.getValue()]; }

ArrayRef<ValueDecl*> ResolvedMemberResult::
getMemberDecls(InterestedMemberKind Kind) {
  auto Result = llvm::makeArrayRef(Impl->AllDecls);
  switch (Kind) {
  case InterestedMemberKind::Viable:
    return Result.slice(Impl->ViableStartIdx);
  case InterestedMemberKind::Unviable:
    return Result.slice(0, Impl->ViableStartIdx);
  case InterestedMemberKind::All:
    return Result;
  }
  llvm_unreachable("unhandled kind");
}

ResolvedMemberResult
swift::resolveValueMember(DeclContext &DC, Type BaseTy, DeclName Name) {
  ResolvedMemberResult Result;
  assert(DC.getASTContext().areSemanticQueriesEnabled());
  ConstraintSystem CS(&DC, None);

  // Look up all members of BaseTy with the given Name.
  MemberLookupResult LookupResult = CS.performMemberLookup(
    ConstraintKind::ValueMember, Name, BaseTy, FunctionRefKind::SingleApply,
    nullptr, false);

  // Keep track of all the unviable members.
  for (auto Can : LookupResult.UnviableCandidates)
    Result.Impl->AllDecls.push_back(Can.getDecl());

  // Keep track of the start of viable choices.
  Result.Impl->ViableStartIdx = Result.Impl->AllDecls.size();

  // If no viable members, we are done.
  if (LookupResult.ViableCandidates.empty())
    return Result;

  // Try to figure out the best overload.
  ConstraintLocator *Locator = CS.getConstraintLocator(nullptr);
  TypeVariableType *TV = CS.createTypeVariable(Locator,
                                               TVO_CanBindToLValue |
                                               TVO_CanBindToNoEscape);
  CS.addOverloadSet(TV, LookupResult.ViableCandidates, &DC, Locator);
  Optional<Solution> OpSolution = CS.solveSingle();
  ValueDecl *Selected = nullptr;
  if (OpSolution.hasValue()) {
    Selected = OpSolution.getValue().overloadChoices[Locator].choice.getDecl();
  }
  for (OverloadChoice& Choice : LookupResult.ViableCandidates) {
    ValueDecl *VD = Choice.getDecl();

    // If this VD is the best overload, keep track of its index.
    if (VD == Selected)
      Result.Impl->BestIdx = Result.Impl->AllDecls.size();
    Result.Impl->AllDecls.push_back(VD);
  }
  return Result;
}

OriginalArgumentList
swift::getOriginalArgumentList(Expr *expr) {
  OriginalArgumentList result;

  auto add = [&](Expr *arg, Identifier label, SourceLoc labelLoc) {
    if (isa<DefaultArgumentExpr>(arg)) {
      return;
    }

    if (auto *varargExpr = dyn_cast<VarargExpansionExpr>(arg)) {
      if (auto *arrayExpr = dyn_cast<ArrayExpr>(varargExpr->getSubExpr())) {
        for (auto *elt : arrayExpr->getElements()) {
          result.args.push_back(elt);
          result.labels.push_back(label);
          result.labelLocs.push_back(labelLoc);

          label = Identifier();
          labelLoc = SourceLoc();
        }

        return;
      }
    }

    result.args.push_back(arg);
    result.labels.push_back(label);
    result.labelLocs.push_back(labelLoc);
  };

  if (auto *parenExpr = dyn_cast<ParenExpr>(expr)) {
    result.lParenLoc = parenExpr->getLParenLoc();
    result.rParenLoc = parenExpr->getRParenLoc();
    result.hasTrailingClosure = parenExpr->hasTrailingClosure();
    add(parenExpr->getSubExpr(), Identifier(), SourceLoc());
  } else if (auto *tupleExpr = dyn_cast<TupleExpr>(expr)) {
    result.lParenLoc = tupleExpr->getLParenLoc();
    result.rParenLoc = tupleExpr->getRParenLoc();
    result.hasTrailingClosure = tupleExpr->hasTrailingClosure();

    auto args = tupleExpr->getElements();
    auto labels = tupleExpr->getElementNames();
    auto labelLocs = tupleExpr->getElementNameLocs();
    for (unsigned i = 0, e = args.size(); i != e; ++i) {
      // Implicit TupleExprs don't always store label locations.
      add(args[i], labels[i],
          labelLocs.empty() ? SourceLoc() : labelLocs[i]);
    }
  } else {
    add(expr, Identifier(), SourceLoc());
  }

  return result;
}
