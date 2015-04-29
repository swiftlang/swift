//===--- CSGen.cpp - Constraint Generator ---------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file implements constraint generation for the type checker.
//
//===----------------------------------------------------------------------===//
#include "ConstraintGraph.h"
#include "ConstraintSystem.h"
#include "swift/AST/ASTVisitor.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/Attr.h"
#include "swift/AST/Expr.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/ADT/APInt.h"

using namespace swift;
using namespace swift::constraints;

/// \brief Skip any implicit conversions applied to this expression.
static Expr *skipImplicitConversions(Expr *expr) {
  while (auto ice = dyn_cast<ImplicitConversionExpr>(expr))
    expr = ice->getSubExpr();
  return expr;
}

/// \brief Find the declaration directly referenced by this expression.
static ValueDecl *findReferencedDecl(Expr *expr, SourceLoc &loc) {
  do {
    expr = expr->getSemanticsProvidingExpr();

    if (auto ice = dyn_cast<ImplicitConversionExpr>(expr)) {
      expr = ice->getSubExpr();
      continue;
    }

    if (auto dre = dyn_cast<DeclRefExpr>(expr)) {
      loc = dre->getLoc();
      return dre->getDecl();
    }

    return nullptr;
  } while (true);
}

/// \brief Return 'true' if the decl in question refers to an operator that
/// could be added to the global scope via a delayed protcol conformance.
/// Currently, this is only true for '==', which is added via an Equatable
/// conformance.
static bool isDelayedOperatorDecl(ValueDecl *vd) {
  return vd && (vd->getName().str() == "==");
}

namespace {
  
  /// Internal struct for tracking information about types within a series
  /// of "linked" expressions. (Such as a chain of binary operator invocations.)
  struct LinkedTypeInfo {
    uint haveIntLiteral : 1;
    uint haveFloatLiteral : 1;
    uint haveStringLiteral : 1;
    
    llvm::SmallSet<TypeBase*, 16> collectedTypes;
    
    LinkedTypeInfo() {
      haveIntLiteral = false;
      haveFloatLiteral = false;
    }
  };

  /// Walks an expression sub-tree, and collects information about expressions
  /// whose types are mutually dependent upon one another.
  class LinkedExprCollector : public ASTWalker {
    
    llvm::SmallVectorImpl<Expr*> &LinkedExprs;

  public:
    
    LinkedExprCollector(llvm::SmallVectorImpl<Expr*> &linkedExprs) :
        LinkedExprs(linkedExprs) {}
    
    std::pair<bool, Expr *> walkToExprPre(Expr *expr) override {
      
      // Store top-level binary exprs for further analysis.
      if (dyn_cast<BinaryExpr>(expr) ||
          
          // Literal exprs are contextually typed, so store them off as well.
          dyn_cast<LiteralExpr>(expr)) {
        LinkedExprs.push_back(expr);
        return {false, expr};
      }
      
      return { true, expr };
    }
    
    Expr *walkToExprPost(Expr *expr) override {
      return expr;
    }
    
    /// \brief Ignore statements.
    std::pair<bool, Stmt *> walkToStmtPre(Stmt *stmt) override {
      return { false, stmt };
    }
    
    /// \brief Ignore declarations.
    bool walkToDeclPre(Decl *decl) override { return false; }
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
      
      if (isa<IntegerLiteralExpr>(expr)) {
        LTI.haveIntLiteral = true;
        return {false, expr};
      }
      
      if (isa<FloatLiteralExpr>(expr)) {
        LTI.haveFloatLiteral = true;
        return {false, expr};
      }
      
      if (isa<StringLiteralExpr>(expr)) {
        LTI.haveStringLiteral = true;
        return {false, expr};
      }
      
      if (auto UDE = dyn_cast<UnresolvedDotExpr>(expr)) {
        
        if (UDE->getType() &&
            !isa<TypeVariableType>(UDE->getType().getPointer()))
          LTI.collectedTypes.insert(UDE->getType().getPointer());
        
        // Don't recurse into the base expression.
        return {false, expr};
      }
      
      if (auto favoredType = CS.getFavoredType(expr)) {
        LTI.collectedTypes.insert(favoredType);
        return {false, expr};
      }
      
      return { true, expr };
    }
    
    Expr *walkToExprPost(Expr *expr) override {
      return expr;
    }
    
    /// \brief Ignore statements.
    std::pair<bool, Stmt *> walkToStmtPre(Stmt *stmt) override {
      return { false, stmt };
    }
    
    /// \brief Ignore declarations.
    bool walkToDeclPre(Decl *decl) override { return false; }
  };
  
  /// For a given expression, given information that is global to the
  /// expression, attempt to derive a favored type for it.
  bool computeFavoredTypeForExpr(Expr *expr, ConstraintSystem &CS) {
    LinkedTypeInfo lti;
    
    expr->walk(LinkedExprAnalyzer(lti, CS));
    
    if (!lti.collectedTypes.empty()) {
      // TODO: Compute the BCT.
      CS.setFavoredType(expr, *lti.collectedTypes.begin());
      return true;
    }
    
    if (lti.haveFloatLiteral) {
      if (auto floatProto =
            CS.TC.Context.getProtocol(
                                  KnownProtocolKind::FloatLiteralConvertible)) {
        if (auto defaultType = CS.TC.getDefaultType(floatProto, CS.DC)) {
          CS.setFavoredType(expr, defaultType.getPointer());
          return true;
        }
      }
    }
    
    if (lti.haveIntLiteral) {
      if (auto intProto =
            CS.TC.Context.getProtocol(
                                KnownProtocolKind::IntegerLiteralConvertible)) {
        if (auto defaultType = CS.TC.getDefaultType(intProto, CS.DC)) {
          CS.setFavoredType(expr, defaultType.getPointer());
          return true;
        }
      }
    }
    
    if (lti.haveStringLiteral) {
      if (auto stringProto =
          CS.TC.Context.getProtocol(
                                KnownProtocolKind::StringLiteralConvertible)) {
        if (auto defaultType = CS.TC.getDefaultType(stringProto, CS.DC)) {
          CS.setFavoredType(expr, defaultType.getPointer());
          return true;
        }
      }
    }
    
    return false;
  }
  
  /// Determine whether the given parameter and argument type should be
  /// "favored" because they match exactly.
  bool isFavoredParamAndArg(ConstraintSystem &CS,
                            Type paramTy,
                            Type argTy,
                            Type otherArgTy) {
    if (argTy->getAs<LValueType>())
      argTy = argTy->getLValueOrInOutObjectType();
    
    if (!otherArgTy.isNull() &&
        otherArgTy->getAs<LValueType>())
      otherArgTy = otherArgTy->getLValueOrInOutObjectType();
    
    // Do the types match exactly?
    if (paramTy->isEqual(argTy))
      return true;
    
    // If the argument is a type variable created for a literal that has a
    // default type, this is a favored param/arg pair if the parameter is of
    // that default type.
    // Is the argument a type variable...
    if (auto argTypeVar = argTy->getAs<TypeVariableType>()) {
      if (auto proto = argTypeVar->getImpl().literalConformanceProto) {
        // If it's a struct type associated with the literal conformance,
        // test against it directly. This helps to avoid 'widening' the
        // favored type to the default type for the literal.
        if (!otherArgTy.isNull() &&
            otherArgTy->getAs<StructType>()) {
          
          if (CS.TC.conformsToProtocol(otherArgTy,
                                       proto,
                                       CS.DC,
                                       ConformanceCheckFlags::InExpression)) {
            return otherArgTy->isEqual(paramTy);
          }
        } else if (auto defaultTy = CS.TC.getDefaultType(proto, CS.DC)) {
          if (paramTy->isEqual(defaultTy)) {
            return true;
          }
        }
      }
    }
    
    return false;
  }
  
  /// Extracts == from a type's Equatable conformance.
  ///
  /// This only applies to types whose Equatable conformance can be derived.
  /// Performing the conformance check forces the function to be synthesized.
  void addNewEqualsOperatorOverloads(ConstraintSystem &CS,
                                     SmallVectorImpl<Constraint *> &newConstraints,
                                     Type paramTy,
                                     Type tyvarType,
                                     ConstraintLocator *csLoc) {
    ProtocolDecl *equatableProto =
    CS.TC.Context.getProtocol(KnownProtocolKind::Equatable);
    if (!equatableProto)
      return;
    
    paramTy = paramTy->getLValueOrInOutObjectType();
    paramTy = paramTy->getReferenceStorageReferent();
    
    auto nominal = paramTy->getAnyNominal();
    if (!nominal)
      return;
    if (!nominal->derivesProtocolConformance(equatableProto))
      return;
    
    ProtocolConformance *conformance = nullptr;
    if (!CS.TC.conformsToProtocol(paramTy, equatableProto,
                                  CS.DC, ConformanceCheckFlags::InExpression,
                                  &conformance))
      return;
    if (!conformance)
      return;
    
    auto requirement =
    equatableProto->lookupDirect(CS.TC.Context.Id_EqualsOperator);
    assert(requirement.size() == 1 && "broken Equatable protocol");
    ConcreteDeclRef witness =
    conformance->getWitness(requirement.front(), &CS.TC);
    if (!witness)
      return;
    
    // FIXME: If we ever have derived == for generic types, we may need to
    // revisit this.
    if (witness.getDecl()->getType()->hasArchetype())
      return;
    
    OverloadChoice choice{
      Type(), witness.getDecl(), /*specialized=*/false, CS
    };
    auto overload =
    Constraint::createBindOverload(CS, tyvarType, choice, csLoc);
    newConstraints.push_back(overload);
  }
  
  /// Favor certain overloads in a call based on some basic analysis
  /// of the overload set and call arguments.
  ///
  /// \param expr The application.
  /// \param isFavored Determine wheth the given overload is favored.
  /// \param createReplacements If provided, a function that creates a set of
  /// replacement fallback constraints.
  void favorCallOverloads(ApplyExpr *expr,
                          ConstraintSystem &CS,
                          std::function<bool(ValueDecl *)> isFavored,
                          std::function<void(TypeVariableType *tyvarType,
                                             ArrayRef<Constraint *>,
                                             SmallVectorImpl<Constraint *>&)>
                          createReplacements = nullptr) {
    // Find the type variable associated with the function, if any.
    auto fnType = expr->getFn()->getType();
    auto tyvarType = fnType->getAs<TypeVariableType>();
    if (!tyvarType)
      return;
    
    // This type variable is only currently associated with the function
    // being applied, and the only constraint attached to it should
    // be the disjunction constraint for the overload group.
    auto &CG = CS.getConstraintGraph();
    SmallVector<Constraint *, 4> constraints;
    CG.gatherConstraints(tyvarType, constraints);
    if (constraints.empty())
      return;
    
    // Look for the disjunction that binds the overload set.
    for (auto constraint : constraints) {
      if (constraint->getKind() != ConstraintKind::Disjunction)
        continue;
      
      auto oldConstraints = constraint->getNestedConstraints();
      auto csLoc = CS.getConstraintLocator(expr->getFn());
      
      // Only replace the disjunctive overload constraint.
      if (oldConstraints[0]->getKind() != ConstraintKind::BindOverload) {
        continue;
      }
      
      SmallVector<Constraint *, 4> favoredConstraints;
      
      TypeBase *favoredTy = nullptr;
      
      // Copy over the existing bindings, dividing the constraints up
      // into "favored" and non-favored lists.
      for (auto oldConstraint : oldConstraints) {
        auto overloadChoice = oldConstraint->getOverloadChoice();
        if (isFavored(overloadChoice.getDecl())) {
          favoredConstraints.push_back(oldConstraint);
          
          favoredTy = overloadChoice.getDecl()->
          getType()->getAs<AnyFunctionType>()->
          getResult().getPointer();
        }
      }
      
      if (favoredConstraints.size() == 1) {
        CS.setFavoredType(expr, favoredTy);
      }
      
      // If there might be replacement constraints, get them now.
      SmallVector<Constraint *, 4> replacementConstraints;
      if (createReplacements)
        createReplacements(tyvarType, oldConstraints, replacementConstraints);
      
      // If we did not find any favored constraints, just introduce
      // the replacement constraints (if they differ).
      if (favoredConstraints.empty()) {
        if (replacementConstraints.size() > oldConstraints.size()) {
          // Remove the old constraint.
          CS.removeInactiveConstraint(constraint);
          
          CS.addConstraint(
                           Constraint::createDisjunction(CS,
                                                         replacementConstraints,
                                                         csLoc));
        }
        break;
      }
      
      // Remove the original constraint from the inactive constraint
      // list and add the new one.
      CS.removeInactiveConstraint(constraint);
      
      // Create the disjunction of favored constraints.
      auto favoredConstraintsDisjunction =
      Constraint::createDisjunction(CS,
                                    favoredConstraints,
                                    csLoc);
      
      // If we didn't actually build a disjunction, clone
      // the underlying constraint so we can mark it as
      // favored.
      if (favoredConstraints.size() == 1) {
        favoredConstraintsDisjunction
        = favoredConstraintsDisjunction->clone(CS);
      }
      
      favoredConstraintsDisjunction->setFavored();
      
      // Find the disjunction of fallback constraints. If any
      // constraints were added here, create a new disjunction.
      Constraint *fallbackConstraintsDisjunction = constraint;
      if (replacementConstraints.size() > oldConstraints.size()) {
        fallbackConstraintsDisjunction =
        Constraint::createDisjunction(CS,
                                      replacementConstraints,
                                      csLoc);
      }
      
      // Form the (favored, fallback) disjunction.
      auto aggregateConstraints = {
        favoredConstraintsDisjunction,
        fallbackConstraintsDisjunction
      };
      
      CS.addConstraint(
                       Constraint::createDisjunction(CS,
                                                     aggregateConstraints,
                                                     csLoc));
      break;
    }
    
  }
  
  /// Determine whether or not a given NominalTypeDecl has a failable
  /// initializer member.
  bool hasFailableInits(NominalTypeDecl *NTD,
                        ConstraintSystem *CS) {
    
    // TODO: Note that we search manually, rather than invoking lookupMember
    // on the ConstraintSystem object. Because this is a hot path, this keeps
    // the overhead of the check low, and is twice as fast.
    if (!NTD->getSearchedForFailableInits()) {
      
      for (auto member : NTD->getMembers()) {
        if (auto CD = dyn_cast<ConstructorDecl>(member)) {
          if (CD->getFailability()) {
            NTD->setHasFailableInits();
            break;
          }
        }
      }
      
      if (!NTD->getHasFailableInits()) {
        for (auto extension : NTD->getExtensions()) {
          for (auto member : extension->getMembers()) {
            if (auto CD = dyn_cast<ConstructorDecl>(member)) {
              if (CD->getFailability()) {
                NTD->setHasFailableInits();
                break;
              }
            }
          }
        }
        
        if (!NTD->getHasFailableInits()) {
          for (auto parentTyLoc : NTD->getInherited()) {
            if (auto nominalType =
                parentTyLoc.getType()->getAs<NominalType>()) {
              if (hasFailableInits(nominalType->getDecl(), CS)) {
                NTD->setHasFailableInits();
                break;
              }
            }
          }
        }
      }
      
      NTD->setSearchedForFailableInits();
    }
    
    return NTD->getHasFailableInits();
  }
  
  Type getInnerParenType(const Type &t) {
    if (auto parenType = dyn_cast<ParenType>(t.getPointer())) {
      return getInnerParenType(parenType->getUnderlyingType());
    }
    
    return t;
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
  
  /// Favor unary operator constraints where we have exact matches
  /// for the operand and contextual type.
  void favorMatchingUnaryOperators(ApplyExpr *expr,
                                   ConstraintSystem &CS) {
    // Find the argument type.
    auto argTy = getInnerParenType(expr->getArg()->getType());
    
    // Determine whether the given declaration is favored.
    auto isFavoredDecl = [&](ValueDecl *value) -> bool {
      auto valueTy = value->getType();
      
      auto fnTy = valueTy->getAs<AnyFunctionType>();
      if (!fnTy)
        return false;
      
      // Figure out the parameter type.
      if (value->getDeclContext()->isTypeContext()) {
        fnTy = fnTy->getResult()->castTo<AnyFunctionType>();
      }
      
      Type paramTy = fnTy->getInput();
      auto resultTy = fnTy->getResult();
      auto contextualTy = CS.getContextualType(expr);
      
      return isFavoredParamAndArg(CS, paramTy, argTy, Type()) &&
      (!contextualTy || (*contextualTy)->isEqual(resultTy));
    };
    
    favorCallOverloads(expr, CS, isFavoredDecl);
  }
  
  void favorMatchingOverloadExprs(ApplyExpr *expr,
                                  ConstraintSystem &CS) {
    // Find the argument type.
    auto argTy = expr->getArg()->getType();
    size_t nArgs = getOperandCount(argTy);
    auto fnExpr = expr->getFn();
    
    // Check to ensure that we have an OverloadedDeclRef, and that we're not
    // favoring multiple overload constraints. (Otherwise, in this case
    // favoring is useless.
    if (auto ODR = dyn_cast<OverloadedDeclRefExpr>(fnExpr)) {
      bool haveMultipleApplicableOverloads = false;
      
      for (auto VD : ODR->getDecls()) {
        if (auto fnTy = VD->getType()->getAs<AnyFunctionType>()) {
          size_t nParams = getOperandCount(fnTy->getInput());
          
          if (nArgs == nParams) {
            if (haveMultipleApplicableOverloads) {
              return;
            } else {
              haveMultipleApplicableOverloads = true;
            }
          }
        }
      }
      
      // Determine whether the given declaration is favored.
      auto isFavoredDecl = [&](ValueDecl *value) -> bool {
        auto valueTy = value->getType();
        
        auto fnTy = valueTy->getAs<AnyFunctionType>();
        if (!fnTy)
          return false;
        
        return nArgs == getOperandCount(fnTy->getInput());
      };
      
      favorCallOverloads(expr, CS, isFavoredDecl);
      
    }
    
    if (auto favoredTy = CS.getFavoredType(expr->getArg())) {
      // Determine whether the given declaration is favored.
      auto isFavoredDecl = [&](ValueDecl *value) -> bool {
        auto valueTy = value->getType();
        
        auto fnTy = valueTy->getAs<AnyFunctionType>();
        if (!fnTy)
          return false;
        
        // Figure out the parameter type.
        if (value->getDeclContext()->isTypeContext()) {
          fnTy = fnTy->getResult()->castTo<AnyFunctionType>();
        }
        
        Type paramTy = fnTy->getInput();
        
        return favoredTy->isEqual(paramTy);
      };
      
      favorCallOverloads(expr, CS, isFavoredDecl);
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
    auto argTy = expr->getArg()->getType();
    auto argTupleTy = argTy->castTo<TupleType>();
    auto argTupleExpr = dyn_cast<TupleExpr>(expr->getArg());
    Type firstArgTy = getInnerParenType(argTupleTy->getElement(0).getType());
    Type secondArgTy =
    getInnerParenType(argTupleTy->getElement(1).getType());
    
    auto firstFavoredTy = CS.getFavoredType(argTupleExpr->getElement(0));
    auto secondFavoredTy = CS.getFavoredType(argTupleExpr->getElement(1));
    
    auto favoredExprTy = CS.getFavoredType(expr);
    
    // If the parent has been favored on the way down, propagate that
    // information to its children.
    if (!firstFavoredTy) {
      CS.setFavoredType(argTupleExpr->getElement(0), favoredExprTy);
      firstFavoredTy = favoredExprTy;
    }
    
    if (!secondFavoredTy) {
      CS.setFavoredType(argTupleExpr->getElement(1), favoredExprTy);
      secondFavoredTy = favoredExprTy;
    }
    
    if (firstFavoredTy && firstArgTy->getAs<TypeVariableType>()) {
      firstArgTy = firstFavoredTy;
    }
    
    if (secondFavoredTy && secondArgTy->getAs<TypeVariableType>()) {
      secondArgTy = secondFavoredTy;
    }
    
    // Determine whether the given declaration is favored.
    auto isFavoredDecl = [&](ValueDecl *value) -> bool {
      auto valueTy = value->getType();
      
      auto fnTy = valueTy->getAs<AnyFunctionType>();
      if (!fnTy)
        return false;
      
      // Figure out the parameter type.
      if (value->getDeclContext()->isTypeContext()) {
        fnTy = fnTy->getResult()->castTo<AnyFunctionType>();
      }
      
      Type paramTy = fnTy->getInput();
      auto paramTupleTy = paramTy->castTo<TupleType>();
      auto firstParamTy = paramTupleTy->getElement(0).getType();
      auto secondParamTy = paramTupleTy->getElement(1).getType();
      
      auto resultTy = fnTy->getResult();
      auto contextualTy = CS.getContextualType(expr);
      
      return
        (isFavoredParamAndArg(CS, firstParamTy, firstArgTy, secondArgTy) ||
         isFavoredParamAndArg(CS, secondParamTy, secondArgTy, firstArgTy)) &&
        firstParamTy->isEqual(secondParamTy) &&
        (!contextualTy || (*contextualTy)->isEqual(resultTy));
    };
    
    auto createReplacements
    = [&](TypeVariableType *tyvarType,
          ArrayRef<Constraint *> oldConstraints,
          SmallVectorImpl<Constraint *>& replacementConstraints) {
      auto declRef = dyn_cast<OverloadedDeclRefExpr>(expr->getFn());
      if (!declRef)
        return;
      
      if (!declRef->isPotentiallyDelayedGlobalOperator())
        return;
      
      Identifier eqOperator = CS.TC.Context.Id_EqualsOperator;
      if (declRef->getDecls()[0]->getName() != eqOperator)
        return;
      
      if (declRef->isSpecialized())
        return;
      
      replacementConstraints.append(oldConstraints.begin(),
                                    oldConstraints.end());
      
      auto csLoc = CS.getConstraintLocator(expr->getFn());
      addNewEqualsOperatorOverloads(CS, replacementConstraints, firstArgTy,
                                    tyvarType, csLoc);
      if (!firstArgTy->isEqual(secondArgTy)) {
        addNewEqualsOperatorOverloads(CS, replacementConstraints,
                                      secondArgTy,
                                      tyvarType, csLoc);
      }
    };
    
    favorCallOverloads(expr, CS, isFavoredDecl, createReplacements);
  }
  
  class ConstraintOptimizer : public ASTWalker {
    
    ConstraintSystem &CS;
    
  public:
    
    ConstraintOptimizer(ConstraintSystem &cs) :
      CS(cs) {}
    
    std::pair<bool, Expr *> walkToExprPre(Expr *expr) override {
      
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
    
    /// \brief Ignore statements.
    std::pair<bool, Stmt *> walkToStmtPre(Stmt *stmt) override {
      return { false, stmt };
    }
    
    /// \brief Ignore declarations.
    bool walkToDeclPre(Decl *decl) override { return false; }
  };
}

namespace {
  class ConstraintGenerator : public ExprVisitor<ConstraintGenerator, Type> {
    ConstraintSystem &CS;

    /// \brief Add constraints for a reference to a named member of the given
    /// base type, and return the type of such a reference.
    Type addMemberRefConstraints(Expr *expr, Expr *base, DeclName name) {
      // The base must have a member of the given name, such that accessing
      // that member through the base returns a value convertible to the type
      // of this expression.
      auto baseTy = base->getType();
      auto tv = CS.createTypeVariable(
                  CS.getConstraintLocator(expr, ConstraintLocator::Member),
                  TVO_CanBindToLValue);
      CS.addValueMemberConstraint(baseTy, name, tv,
        CS.getConstraintLocator(expr, ConstraintLocator::Member));
      return tv;
    }

    /// \brief Add constraints for a reference to a specific member of the given
    /// base type, and return the type of such a reference.
    Type addMemberRefConstraints(Expr *expr, Expr *base, ValueDecl *decl) {
      // If we're referring to an invalid declaration, fail.
      if (!decl)
        return nullptr;
      
      CS.getTypeChecker().validateDecl(decl, true);
      if (decl->isInvalid())
        return nullptr;

      auto memberLocator =
        CS.getConstraintLocator(expr, ConstraintLocator::Member);
      auto tv = CS.createTypeVariable(memberLocator, TVO_CanBindToLValue);
      
      // When the member is a property, this availability is for the
      // property itself and not for the getter or setter individually.
      // Unfortunately, this means that we do not support the scenario where
      // a getter and a setter have different availabilities.
      Optional<UnavailabilityReason> reason =
          CS.TC.checkDeclarationAvailability(decl, expr->getLoc(), CS.DC);
      OverloadChoice choice(base->getType(), decl, /*isSpecialized=*/false, CS,
                            reason);
      auto locator = CS.getConstraintLocator(expr, ConstraintLocator::Member);
      CS.addBindOverloadConstraint(tv, choice, locator);
      return tv;
    }

    /// \brief Add constraints for a subscript operation.
    Type addSubscriptConstraints(Expr *expr, Expr *base, Expr *index) {
      ASTContext &Context = CS.getASTContext();

      // Locators used in this expression.
      auto indexLocator
        = CS.getConstraintLocator(expr, ConstraintLocator::SubscriptIndex);
      auto resultLocator
        = CS.getConstraintLocator(expr, ConstraintLocator::SubscriptResult);
      
      Type outputTy;

      // The base type must have a subscript declaration with type
      // I -> inout? O, where I and O are fresh type variables. The index
      // expression must be convertible to I and the subscript expression
      // itself has type inout? O, where O may or may not be an lvalue.
      auto inputTv = CS.createTypeVariable(indexLocator, /*options=*/0);
      
      // For an integer subscript expression on an array slice type, instead of
      // introducing a new type variable we can easily obtain the element type.
      if (auto subscriptExpr = dyn_cast<SubscriptExpr>(expr)) {
        
        auto isLValueBase = false;
        auto baseTy = subscriptExpr->getBase()->getType();
        
        if (baseTy->getAs<LValueType>()) {
          isLValueBase = true;
          baseTy = baseTy->getLValueOrInOutObjectType();
        }
        
        if (auto arraySliceTy = dyn_cast<ArraySliceType>(baseTy.getPointer())) {
          baseTy = arraySliceTy->getDesugaredType();
          
          auto indexExpr = subscriptExpr->getIndex();
          
          if (auto parenExpr = dyn_cast<ParenExpr>(indexExpr)) {
            indexExpr = parenExpr->getSubExpr();
          }
          
          if(isa<IntegerLiteralExpr>(indexExpr)) {
            
            outputTy = baseTy->getAs<BoundGenericType>()->getGenericArgs()[0];
            
            if (isLValueBase)
              outputTy = LValueType::get(outputTy);
          }
        } else if (auto dictTy = CS.isDictionaryType(baseTy)) {
          auto keyTy = dictTy->first;
          auto valueTy = dictTy->second;
          
          if (isFavoredParamAndArg(CS, keyTy, index->getType(), Type())) {
            outputTy = OptionalType::get(valueTy);
            
            if (isLValueBase)
              outputTy = LValueType::get(outputTy);
          }
        }
      }
      
      if (outputTy.isNull()) {
        outputTy = CS.createTypeVariable(resultLocator,
                                              TVO_CanBindToLValue);
      } else {
        CS.setFavoredType(expr, outputTy.getPointer());
      }

      auto subscriptMemberLocator
        = CS.getConstraintLocator(expr, ConstraintLocator::SubscriptMember);

      // Add the member constraint for a subscript declaration.
      // FIXME: lame name!
      auto baseTy = base->getType();
      auto fnTy = FunctionType::get(inputTv, outputTy);
      CS.addValueMemberConstraint(baseTy, Context.Id_subscript,
                                  fnTy, subscriptMemberLocator);

      // Add the constraint that the index expression's type be convertible
      // to the input type of the subscript operator.
      CS.addConstraint(ConstraintKind::ArgumentTupleConversion,
                       index->getType(), inputTv, indexLocator);
      return outputTy;
    }

  public:
    ConstraintGenerator(ConstraintSystem &CS) : CS(CS) { }

    ConstraintSystem &getConstraintSystem() const { return CS; }
    
    Type visitErrorExpr(ErrorExpr *E) {
      // FIXME: Can we do anything with error expressions at this point?
      return nullptr;
    }

    Type visitLiteralExpr(LiteralExpr *expr) {
      // If the expression has already been assigned a builtin type,
      // this is synthesized code; just use that type.
      if (!expr->getType().isNull()) {
        if (expr->getType()->is<BuiltinType>())
          return expr->getType();
      }

      auto protocol = CS.getTypeChecker().getLiteralProtocol(expr);
      if (!protocol) {
        return nullptr;
      }

      auto tv = CS.createTypeVariable(CS.getConstraintLocator(expr),
                                      TVO_PrefersSubtypeBinding);
      
      tv->getImpl().literalConformanceProto = protocol;
      
      CS.addConstraint(ConstraintKind::ConformsTo, tv,
                       protocol->getDeclaredType(),
                       CS.getConstraintLocator(expr));
      return tv;
    }

    Type
    visitInterpolatedStringLiteralExpr(InterpolatedStringLiteralExpr *expr) {
      // Dig out the StringInterpolationConvertible protocol.
      auto &tc = CS.getTypeChecker();
      auto &C = CS.getASTContext();
      auto interpolationProto
        = tc.getProtocol(expr->getLoc(),
                         KnownProtocolKind::StringInterpolationConvertible);
      if (!interpolationProto) {
        tc.diagnose(expr->getStartLoc(), diag::interpolation_missing_proto);
        return nullptr;
      }

      // The type of the expression must conform to the
      // StringInterpolationConvertible protocol.
      auto locator = CS.getConstraintLocator(expr);
      auto tv = CS.createTypeVariable(locator, TVO_PrefersSubtypeBinding);
      CS.addConstraint(ConstraintKind::ConformsTo, tv,
                       interpolationProto->getDeclaredType(),
                       locator);

      // Each of the segments is passed as an argument to
      // init(stringInterpolationSegment:).
      unsigned index = 0;
      auto tvMeta = MetatypeType::get(tv);
      for (auto segment : expr->getSegments()) {
        auto locator = CS.getConstraintLocator(
                         expr,
                         LocatorPathElt::getInterpolationArgument(index++));
        auto segmentTyV = CS.createTypeVariable(locator, /*options=*/0);
        auto returnTyV = CS.createTypeVariable(locator, /*options=*/0);
        auto methodTy = FunctionType::get(segmentTyV, returnTyV);

        CS.addConstraint(Constraint::create(CS, ConstraintKind::Conversion,
                                            segment->getType(),
                                            segmentTyV,
                                            Identifier(),
                                            locator));

        DeclName segmentName(C, C.Id_init, { C.Id_StringInterpolationSegment });
        CS.addConstraint(Constraint::create(CS, ConstraintKind::ValueMember,
                                            tvMeta,
                                            methodTy,
                                            segmentName,
                                            locator));

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
        // __DSO_HANDLE__ has type UnsafeMutablePointer<Void>.
        auto &tc = CS.getTypeChecker();
        if (tc.requirePointerArgumentIntrinsics(expr->getLoc()))
          return nullptr;

        return CS.DC->getParentModule()->getDSOHandle()->getInterfaceType();
      }
      }
    }

    Type visitObjectLiteralExpr(ObjectLiteralExpr *expr) {
      auto &tc = CS.getTypeChecker();
      auto protocol = tc.getLiteralProtocol(expr);
      if (!protocol) {
        tc.diagnose(expr->getLoc(), diag::use_unknown_object_literal,
                    expr->getName());
        return nullptr;
      }

      auto tv = CS.createTypeVariable(CS.getConstraintLocator(expr),
                                      TVO_PrefersSubtypeBinding);
      
      tv->getImpl().literalConformanceProto = protocol;
      
      CS.addConstraint(ConstraintKind::ConformsTo, tv,
                       protocol->getDeclaredType(),
                       CS.getConstraintLocator(expr));

      // Add constraint on args.
      DeclName constrName = tc.getObjectLiteralConstructorName(expr);
      assert(constrName);
      ArrayRef<ValueDecl *> constrs = protocol->lookupDirect(constrName);
      if (constrs.size() != 1 || !isa<ConstructorDecl>(constrs.front())) {
        tc.diagnose(protocol, diag::object_literal_broken_proto);
        return nullptr;
      }
      auto *constr = cast<ConstructorDecl>(constrs.front());
      CS.addConstraint(ConstraintKind::ArgumentTupleConversion,
        expr->getArg()->getType(), constr->getArgumentType(),
        CS.getConstraintLocator(expr, ConstraintLocator::ApplyArgument));

      Type result = tv;
      if (constr->getFailability() != OTK_None) {
        result = OptionalType::get(constr->getFailability(), result);
      }

      return result;
    }

    Type visitDeclRefExpr(DeclRefExpr *E) {
      // If we're referring to an invalid declaration, don't type-check.
      //
      // FIXME: If the decl is in error, we get no information from this.
      // We may, alternatively, want to use a type variable in that case,
      // and possibly infer the type of the variable that way.
      CS.getTypeChecker().validateDecl(E->getDecl(), true);
      if (E->getDecl()->isInvalid())
        return nullptr;

      auto locator = CS.getConstraintLocator(E);
      
      Optional<UnavailabilityReason> reason =
          CS.TC.checkDeclarationAvailability(E->getDecl(),E->getLoc(), CS.DC);
      
      // If reason is not None and EnableExperimentalUnavailableAsOptional
      // is turned on, the type system will report the reference
      // as having an optional type when the it is resolved.
      // The diagnostic for when this optional does not type check is
      // currently not very helpful.
      // FIXME: When reporting a fix for an optional, we should include
      // the version range when the declaration is available.
      
      
      // Create an overload choice referencing this declaration and immediately
      // resolve it. This records the overload for use later.
      auto tv = CS.createTypeVariable(locator, TVO_CanBindToLValue);
      CS.resolveOverload(locator, tv,
                         OverloadChoice(Type(), E->getDecl(),
                                        E->isSpecialized(),
                                        CS,
                                        reason));
      
      if (E->getDecl()->getType() &&
          !E->getDecl()->getType()->getAs<TypeVariableType>()) {
        CS.setFavoredType(E, E->getDecl()->getType().getPointer());
      }

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

    Type visitTypeExpr(TypeExpr *E) {
      Type type;
      // If this is an implicit TypeExpr, don't validate its contents.
      if (auto *rep = E->getTypeRepr()) {
        TypeResolutionOptions options = TR_AllowUnboundGenerics;
        options |= TR_InExpression;
        type = CS.TC.resolveType(rep, CS.DC, options);
      } else {
        type = E->getTypeLoc().getType();
      }
      if (!type) return Type();
      
      auto locator = CS.getConstraintLocator(E);
      type = CS.openType(type, locator);
      E->getTypeLoc().setType(type, /*validated=*/true);
      return MetatypeType::get(type);
    }

    Type visitUnresolvedConstructorExpr(UnresolvedConstructorExpr *expr) {
      ASTContext &C = CS.getASTContext();
      
      // Open a member constraint for constructors on the subexpr type.
      // FIXME: the getRValueInstanceType() here is a hack to make the
      //   T.init withFoo(foo)
      // syntax type-check. We shouldn't rely on any kinds of adjustments to
      // the subexpression's type here, but dealing with this requires us to
      // clarify when we can refer to constructors with ".init".
      auto baseTy = expr->getSubExpr()->getType()
                      ->getLValueOrInOutObjectType()->getRValueInstanceType();
      auto argsTy = CS.createTypeVariable(
                      CS.getConstraintLocator(expr),
                      TVO_CanBindToLValue|TVO_PrefersSubtypeBinding);
      auto resultTy = CS.createTypeVariable(CS.getConstraintLocator(expr),
                                            /*options=*/0);
      auto methodTy = FunctionType::get(argsTy, resultTy);
      CS.addValueMemberConstraint(baseTy,
                                  C.Id_init,
        methodTy,
        CS.getConstraintLocator(expr, ConstraintLocator::ConstructorMember));
      
      // The result of the expression is the partial application of the
      // constructor to the subexpression.
      return methodTy;
    }
    
    Type visitDotSyntaxBaseIgnoredExpr(DotSyntaxBaseIgnoredExpr *expr) {
      llvm_unreachable("Already type-checked");
    }

    Type visitOverloadedDeclRefExpr(OverloadedDeclRefExpr *expr) {
      // For a reference to an overloaded declaration, we create a type variable
      // that will be equal to different types depending on which overload
      // is selected.
      auto locator = CS.getConstraintLocator(expr);
      auto tv = CS.createTypeVariable(locator, TVO_CanBindToLValue);
      ArrayRef<ValueDecl*> decls = expr->getDecls();
      SmallVector<OverloadChoice, 4> choices;
      
      if (decls.size()) {
        if (isDelayedOperatorDecl(decls[0])) {
          expr->setIsPotentiallyDelayedGlobalOperator();
        }
      }
      
      for (unsigned i = 0, n = decls.size(); i != n; ++i) {
        // If the result is invalid, skip it.
        // FIXME: Note this as invalid, in case we don't find a solution,
        // so we don't let errors cascade further.
        CS.getTypeChecker().validateDecl(decls[i], true);
        if (decls[i]->isInvalid())
          continue;

        // If the overload is potentially unavailable, record the reason
        // in the overload choice.
        auto unavailReason =
            CS.TC.checkDeclarationAvailability(decls[i], expr->getLoc(), CS.DC);
        
        choices.push_back(OverloadChoice(Type(), decls[i],
                                         expr->isSpecialized(),
                                         CS, unavailReason));
      }

      // If there are no valid overloads, give up.
      if (choices.empty())
        return nullptr;

      // Record this overload set.
      CS.addOverloadSet(tv, choices, locator);
      return tv;
    }

    Type visitOverloadedMemberRefExpr(OverloadedMemberRefExpr *expr) {
      // For a reference to an overloaded declaration, we create a type variable
      // that will be bound to different types depending on which overload
      // is selected.
      auto tv = CS.createTypeVariable(CS.getConstraintLocator(expr),
                                      TVO_CanBindToLValue);
      ArrayRef<ValueDecl*> decls = expr->getDecls();
      SmallVector<OverloadChoice, 4> choices;
      auto baseTy = expr->getBase()->getType();
      for (unsigned i = 0, n = decls.size(); i != n; ++i) {
        // If the result is invalid, skip it.
        // FIXME: Note this as invalid, in case we don't find a solution,
        // so we don't let errors cascade further.
        CS.getTypeChecker().validateDecl(decls[i], true);
        if (decls[i]->isInvalid())
          continue;

        auto unavailReason =
            CS.TC.checkDeclarationAvailability(decls[i], expr->getLoc(), CS.DC);
        
        choices.push_back(OverloadChoice(baseTy, decls[i],
                                         /*isSpecialized=*/false,
                                         CS, unavailReason));
      }

      // If there are no valid overloads, give up.
      if (choices.empty())
        return nullptr;

      // Record this overload set.
      auto locator = CS.getConstraintLocator(expr, ConstraintLocator::Member);
      CS.addOverloadSet(tv, choices, locator);
      return tv;
    }
    
    Type visitUnresolvedDeclRefExpr(UnresolvedDeclRefExpr *expr) {
      // This is an error case, where we're trying to use type inference
      // to help us determine which declaration the user meant to refer to.
      // FIXME: Do we need to note that we're doing some kind of recovery?
      return CS.createTypeVariable(CS.getConstraintLocator(expr),
                                   TVO_CanBindToLValue);
    }
    
    Type visitMemberRefExpr(MemberRefExpr *expr) {
      return addMemberRefConstraints(expr, expr->getBase(),
                                     expr->getMember().getDecl());
    }
    
    Type visitDynamicMemberRefExpr(DynamicMemberRefExpr *expr) {
      return addMemberRefConstraints(expr, expr->getBase(),
                                     expr->getMember().getDecl());
    }
    
    Type visitUnresolvedMemberExpr(UnresolvedMemberExpr *expr) {
      auto baseLocator = CS.getConstraintLocator(
                            expr,
                            ConstraintLocator::MemberRefBase);
      auto memberLocator
        = CS.getConstraintLocator(expr, ConstraintLocator::UnresolvedMember);
      auto baseTy = CS.createTypeVariable(baseLocator, /*options=*/0);
      auto memberTy = CS.createTypeVariable(memberLocator, TVO_CanBindToLValue);

      // An unresolved member expression '.member' is modeled as a value member
      // constraint
      //
      //   T0.Type[.member] == T1
      //
      // for fresh type variables T0 and T1, which pulls out a static
      // member, i.e., an enum case or a static variable.
      auto baseMetaTy = MetatypeType::get(baseTy);
      CS.addUnresolvedValueMemberConstraint(baseMetaTy, expr->getName(),
                                            memberTy, memberLocator);

      // If there is an argument, apply it.
      if (auto arg = expr->getArgument()) {
        // The result type of the function must be convertible to the base type.
        // TODO: we definitely want this to include ImplicitlyUnwrappedOptional; does it
        // need to include everything else in the world?
        auto outputTy
          = CS.createTypeVariable(
              CS.getConstraintLocator(expr, ConstraintLocator::ApplyFunction),
              /*options=*/0);
        CS.addConstraint(ConstraintKind::Conversion, outputTy, baseTy,
          CS.getConstraintLocator(expr, ConstraintLocator::RvalueAdjustment));

        // The function/enum case must be callable with the given argument.
        auto funcTy = FunctionType::get(arg->getType(), outputTy);
        CS.addConstraint(ConstraintKind::ApplicableFunction, funcTy,
          memberTy,
          CS.getConstraintLocator(expr, ConstraintLocator::ApplyFunction));
        
        return baseTy;
      }

      // Otherwise, the member needs to be convertible to the base type.
      CS.addConstraint(ConstraintKind::Conversion, memberTy, baseTy,
        CS.getConstraintLocator(expr, ConstraintLocator::RvalueAdjustment));
      
      // The member type also needs to be convertible to the context type, which
      // preserves lvalue-ness.
      auto resultTy = CS.createTypeVariable(memberLocator, TVO_CanBindToLValue);
      CS.addConstraint(ConstraintKind::Conversion, memberTy, resultTy,
                       memberLocator);
      CS.addConstraint(ConstraintKind::Equal, resultTy, baseTy,
                       memberLocator);
      return resultTy;
    }

    Type visitUnresolvedDotExpr(UnresolvedDotExpr *expr) {
      return addMemberRefConstraints(expr, expr->getBase(), expr->getName());
    }
    
    Type visitUnresolvedSelectorExpr(UnresolvedSelectorExpr *expr) {
      return addMemberRefConstraints(expr, expr->getBase(), expr->getName());
    }
    
    Type visitUnresolvedSpecializeExpr(UnresolvedSpecializeExpr *expr) {
      auto baseTy = expr->getSubExpr()->getType();
      
      // We currently only support explicit specialization of generic types.
      // FIXME: We could support explicit function specialization.
      auto &tc = CS.getTypeChecker();
      if (baseTy->is<AnyFunctionType>()) {
        tc.diagnose(expr->getSubExpr()->getLoc(),
                    diag::cannot_explicitly_specialize_generic_function);
        tc.diagnose(expr->getLAngleLoc(),
                    diag::while_parsing_as_left_angle_bracket);
        return Type();
      }
      
      if (AnyMetatypeType *meta = baseTy->getAs<AnyMetatypeType>()) {
        if (BoundGenericType *bgt
              = meta->getInstanceType()->getAs<BoundGenericType>()) {
          ArrayRef<Type> typeVars = bgt->getGenericArgs();
          ArrayRef<TypeLoc> specializations = expr->getUnresolvedParams();

          // If we have too many generic arguments, complain.
          if (specializations.size() > typeVars.size()) {
            tc.diagnose(expr->getSubExpr()->getLoc(),
                        diag::type_parameter_count_mismatch,
                        bgt->getDecl()->getName(),
                        typeVars.size(), specializations.size(),
                        false)
              .highlight(SourceRange(expr->getLAngleLoc(),
                                     expr->getRAngleLoc()));
            tc.diagnose(bgt->getDecl(), diag::generic_type_declared_here,
                        bgt->getDecl()->getName());
            return Type();
          }

          // Bind the specified generic arguments to the type variables in the
          // open type.
          auto locator = CS.getConstraintLocator(expr);
          for (size_t i = 0, size = specializations.size(); i < size; ++i) {
            CS.addConstraint(ConstraintKind::Equal,
                             typeVars[i], specializations[i].getType(),
                             locator);
          }
          
          return baseTy;
        } else {
          tc.diagnose(expr->getSubExpr()->getLoc(), diag::not_a_generic_type,
                      meta->getInstanceType());
          tc.diagnose(expr->getLAngleLoc(),
                      diag::while_parsing_as_left_angle_bracket);
          return Type();
        }
      }

      // FIXME: If the base type is a type variable, constrain it to a metatype
      // of a bound generic type.
      
      tc.diagnose(expr->getSubExpr()->getLoc(),
                  diag::not_a_generic_definition);
      tc.diagnose(expr->getLAngleLoc(),
                  diag::while_parsing_as_left_angle_bracket);
      return Type();
    }
    
    Type visitSequenceExpr(SequenceExpr *expr) {
      llvm_unreachable("Didn't even parse?");
    }

    Type visitIdentityExpr(IdentityExpr *expr) {
      expr->setType(expr->getSubExpr()->getType());
      return expr->getType();
    }

    Type visitParenExpr(ParenExpr *expr) {
      auto &ctx = CS.getASTContext();
      expr->setType(ParenType::get(ctx, expr->getSubExpr()->getType()));
      
      if (auto favoredTy = CS.getFavoredType(expr->getSubExpr())) {
        CS.setFavoredType(expr, favoredTy);
      }
      
      return expr->getType();
    }

    Type visitTupleExpr(TupleExpr *expr) {
      // The type of a tuple expression is simply a tuple of the types of
      // its subexpressions.
      SmallVector<TupleTypeElt, 4> elements;
      elements.reserve(expr->getNumElements());
      for (unsigned i = 0, n = expr->getNumElements(); i != n; ++i) {
        elements.push_back(TupleTypeElt(expr->getElement(i)->getType(),
                                        expr->getElementName(i)));
      }

      return TupleType::get(elements, CS.getASTContext());
    }

    Type visitSubscriptExpr(SubscriptExpr *expr) {
      return addSubscriptConstraints(expr, expr->getBase(), expr->getIndex());
    }
    
    Type visitArrayExpr(ArrayExpr *expr) {
      ASTContext &C = CS.getASTContext();
      
      // An array expression can be of a type T that conforms to the
      // ArrayLiteralConvertible protocol.
      auto &tc = CS.getTypeChecker();
      ProtocolDecl *arrayProto
        = tc.getProtocol(expr->getLoc(),
                         KnownProtocolKind::ArrayLiteralConvertible);
      if (!arrayProto) {
        return Type();
      }

      // FIXME: Protect against broken standard library.
      auto elementAssocTy = cast<AssociatedTypeDecl>(
                              arrayProto->lookupDirect(
                                C.getIdentifier("Element")).front());

      auto locator = CS.getConstraintLocator(expr);
      auto contextualType = CS.getContextualType(expr);
      Type *contextualArrayType = nullptr;
      Type contextualArrayElementType = nullptr;
      
      // If a contextual type exists for this expression, apply it directly.
      if (contextualType && CS.isArrayType(*contextualType)) {
        // Is the array type a contextual type
        contextualArrayType = contextualType;
        contextualArrayElementType =
            CS.getBaseTypeForArrayType(contextualType->getPointer());
        
        CS.addConstraint(ConstraintKind::ConformsTo, *contextualType,
                         arrayProto->getDeclaredType(),
                         locator);
        
        unsigned index = 0;
        for (auto element : expr->getElements()) {
          CS.addConstraint(ConstraintKind::Conversion,
                           element->getType(),
                           contextualArrayElementType,
                           CS.getConstraintLocator(expr,
                                                   LocatorPathElt::
                                                    getTupleElement(index++)));
        }
        
        return *contextualArrayType;
      }
      
      auto arrayTy = CS.createTypeVariable(locator, TVO_PrefersSubtypeBinding);

      // The array must be an array literal type.
      CS.addConstraint(ConstraintKind::ConformsTo, arrayTy,
                       arrayProto->getDeclaredType(),
                       locator);
      
      // Its subexpression should be convertible to a tuple (T.Element...).
      // FIXME: We should really go through the conformance above to extract
      // the element type, rather than just looking for the element type.
      // FIXME: Member constraint is still weird here.
      ConstraintLocatorBuilder builder(locator);
      auto arrayElementTy = CS.getMemberType(arrayTy, elementAssocTy,
                                             builder.withPathElement(
                                               ConstraintLocator::Member),
                                             /*options=*/0);

      // Introduce conversions from each element to the element type of the
      // array.
      unsigned index = 0;
      for (auto element : expr->getElements()) {
        CS.addConstraint(ConstraintKind::Conversion,
                         element->getType(),
                         arrayElementTy,
                         CS.getConstraintLocator(
                           expr,
                           LocatorPathElt::getTupleElement(index++)));
      }

      return arrayTy;
    }

    Type visitDictionaryExpr(DictionaryExpr *expr) {
      ASTContext &C = CS.getASTContext();
      // A dictionary expression can be of a type T that conforms to the
      // DictionaryLiteralConvertible protocol.
      // FIXME: This isn't actually used for anything at the moment.
      auto &tc = CS.getTypeChecker();
      ProtocolDecl *dictionaryProto
        = tc.getProtocol(expr->getLoc(),
                         KnownProtocolKind::DictionaryLiteralConvertible);
      if (!dictionaryProto) {
        return Type();
      }

      // FIXME: Protect against broken standard library.
      auto keyAssocTy = cast<AssociatedTypeDecl>(
                          dictionaryProto->lookupDirect(
                            C.getIdentifier("Key")).front());
      auto valueAssocTy = cast<AssociatedTypeDecl>(
                            dictionaryProto->lookupDirect(
                              C.getIdentifier("Value")).front());

      auto locator = CS.getConstraintLocator(expr);
      auto dictionaryTy = CS.createTypeVariable(locator,
                                                TVO_PrefersSubtypeBinding);

      // The array must be a dictionary literal type.
      CS.addConstraint(ConstraintKind::ConformsTo, dictionaryTy,
                       dictionaryProto->getDeclaredType(),
                       locator);


      // Its subexpression should be convertible to a tuple ((T.Key,T.Value)...).
      ConstraintLocatorBuilder locatorBuilder(locator);
      auto dictionaryKeyTy = CS.getMemberType(dictionaryTy,
                                              keyAssocTy,
                                              locatorBuilder.withPathElement(
                                                ConstraintLocator::Member),
                                              /*options=*/0);
      /// FIXME: ArrayElementType is a total hack here.
      auto dictionaryValueTy = CS.getMemberType(dictionaryTy,
                                                valueAssocTy,
                                                locatorBuilder.withPathElement(
                                                  ConstraintLocator::ArrayElementType),
                                                /*options=*/0);
      
      TupleTypeElt tupleElts[2] = { TupleTypeElt(dictionaryKeyTy),
                                    TupleTypeElt(dictionaryValueTy) };
      Type elementTy = TupleType::get(tupleElts, C);

      // Introduce conversions from each element to the element type of the
      // dictionary.
      unsigned index = 0;
      for (auto element : expr->getElements()) {
        CS.addConstraint(ConstraintKind::Conversion,
                         element->getType(),
                         elementTy,
                         CS.getConstraintLocator(
                           expr,
                           LocatorPathElt::getTupleElement(index++)));
      }

      return dictionaryTy;
    }

    Type visitDynamicSubscriptExpr(DynamicSubscriptExpr *expr) {
      return addSubscriptConstraints(expr, expr->getBase(), expr->getIndex());
    }

    Type visitTupleElementExpr(TupleElementExpr *expr) {
      ASTContext &context = CS.getASTContext();
      Identifier name
        = context.getIdentifier(llvm::utostr(expr->getFieldNumber()));
      return addMemberRefConstraints(expr, expr->getBase(), name);
    }

    /// \brief Produces a type for the given pattern, filling in any missing
    /// type information with fresh type variables.
    ///
    /// \param pattern The pattern.
    Type getTypeForPattern(Pattern *pattern, bool forFunctionParam,
                           ConstraintLocatorBuilder locator) {
      switch (pattern->getKind()) {
      case PatternKind::Paren:
        // Parentheses don't affect the type.
        return getTypeForPattern(cast<ParenPattern>(pattern)->getSubPattern(),
                                 forFunctionParam, locator);
      case PatternKind::Var:
        // Var doesn't affect the type.
        return getTypeForPattern(cast<VarPattern>(pattern)->getSubPattern(),
                                 forFunctionParam, locator);
      case PatternKind::Any:
        // For a pattern of unknown type, create a new type variable.
        return CS.createTypeVariable(CS.getConstraintLocator(locator),
                                     /*options=*/0);

      case PatternKind::Named: {
        auto var = cast<NamedPattern>(pattern)->getDecl();

        // For a named pattern without a type, create a new type variable
        // and use it as the type of the variable.
        Type ty = CS.createTypeVariable(CS.getConstraintLocator(locator),
                                        /*options=*/0);

        // For weak variables, use Optional<T>.
        if (auto *OA = var->getAttrs().getAttribute<OwnershipAttr>())
          if (!forFunctionParam && OA->get() == Ownership::Weak) {
            ty = CS.getTypeChecker().getOptionalType(var->getLoc(), ty);
            if (!ty) return Type();
          }

        // We want to set the variable's type here when type-checking
        // a function's parameter clauses because we're going to
        // type-check the entire function body within the context of
        // the constraint system.  In contrast, when type-checking a
        // variable binding, we really don't want to set the
        // variable's type because it can easily escape the constraint
        // system and become a dangling type reference.
        if (forFunctionParam)
          var->overwriteType(ty);
        return ty;
      }

      case PatternKind::Typed: {
        auto typedPattern = cast<TypedPattern>(pattern);
        // FIXME: Need a better locator for a pattern as a base.
        Type openedType = CS.openType(typedPattern->getType(), locator);
        if (auto weakTy = openedType->getAs<WeakStorageType>())
          openedType = weakTy->getReferentType();

        // For a typed pattern, simply return the opened type of the pattern.
        // FIXME: Error recovery if the type is an error type?
        return openedType;
      }

      case PatternKind::Tuple: {
        auto tuplePat = cast<TuplePattern>(pattern);
        SmallVector<TupleTypeElt, 4> tupleTypeElts;
        tupleTypeElts.reserve(tuplePat->getNumElements());
        for (unsigned i = 0, e = tuplePat->getNumElements(); i != e; ++i) {
          auto tupleElt = tuplePat->getElement(i);
          bool isVararg = tuplePat->hasVararg() && i == e-1;
          Type eltTy = getTypeForPattern(tupleElt.getPattern(),forFunctionParam,
                                         locator.withPathElement(
                                           LocatorPathElt::getTupleElement(i)));

          Type varArgBaseTy;
          tupleTypeElts.push_back(TupleTypeElt(eltTy, tupleElt.getLabel(),
                                               tupleElt.getDefaultArgKind(),
                                               isVararg));
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
                                     /*options=*/0);
      }

      llvm_unreachable("Unhandled pattern kind");
    }

    Type visitCaptureListExpr(CaptureListExpr *expr) {
      // The type of the capture list is just the type of its closure.
      expr->setType(expr->getClosureBody()->getType());
      return expr->getType();
    }
    
    /// \brief Walk a closure AST to determine if it can throw.
    static bool closureCanThrow(ClosureExpr *expr) {
      
      class FindInnerTryExpressions : public ASTWalker {
        bool FoundTry = false;
        
        std::pair<bool, Expr *> walkToExprPre(Expr *expr) override {
          
          // If we've found a 'try', record it and terminate the traversal.
          if (isa<TryExpr>(expr)) {
            FoundTry = true;
            return { false, nullptr };
          }
          
          // Do not recurse into other closures.
          if (isa<ClosureExpr>(expr))
            return { false, expr };
          
          return { true, expr };
        }
        
        bool walkToDeclPre(Decl *decl) override {
          // Do not walk into function or type declarations.
          if (isa<AbstractFunctionDecl>(decl) ||
              isa<AbstractStorageDecl>(decl) ||
              isa<OperatorDecl>(decl) ||
              isa<TypeDecl>(decl))
            return false;
          
          return true;
        }
        
        std::pair<bool, Stmt *> walkToStmtPre(Stmt *stmt) override {
          
          // Do not walk into the 'do' clause of a do/catch statement.
          if (isa<BraceStmt>(stmt) && Parent.getAsStmt() &&
              isa<DoCatchStmt>(Parent.getAsStmt())) {
            return { false, stmt };
          }
          
          return { true, stmt };
        }
        
      public:
        bool foundTry() { return FoundTry; }
      };
      
      if (expr->getThrowsLoc().isValid())
        return true;
      
      auto body = expr->getBody();
      
      if (!body)
        return false;
      
      auto tryFinder = FindInnerTryExpressions();
      body->walk(tryFinder);
      return tryFinder.foundTry();
    }
    

    Type visitClosureExpr(ClosureExpr *expr) {
      
      // If a contextual function type exists, we can use that to obtain the
      // expected return type, rather than allocating a fresh type variable.
      auto contextualType = CS.getContextualType(expr);
      Type crt;
      
      if (contextualType) {
        if (auto cft = (*contextualType)->getAs<AnyFunctionType>()) {
          crt = cft->getResult();
        }
      }
      
      // Closure expressions always have function type. In cases where a
      // parameter or return type is omitted, a fresh type variable is used to
      // stand in for that parameter or return type, allowing it to be inferred
      // from context.
      Type funcTy;
      if (expr->hasExplicitResultType()) {
        funcTy = expr->getExplicitResultTypeLoc().getType();
      } else if (!crt.isNull()) {
        funcTy = crt;
      } else{
        // If no return type was specified, create a fresh type
        // variable for it.
        funcTy = CS.createTypeVariable(
                   CS.getConstraintLocator(expr,
                                           ConstraintLocator::ClosureResult),
                   /*options=*/0);
      }

      // Walk through the patterns in the func expression, backwards,
      // computing the type of each pattern (which may involve fresh type
      // variables where parameter types where no provided) and building the
      // eventual function type.
      auto paramTy = getTypeForPattern(
                       expr->getParams(), /*forFunctionParam*/ true,
                       CS.getConstraintLocator(
                         expr,
                         LocatorPathElt::getTupleElement(0)));

      auto extInfo = FunctionType::ExtInfo();
      
      if (closureCanThrow(expr))
        extInfo = extInfo.withThrows();
      
      // FIXME: If we want keyword arguments for closures, add them here.
      funcTy = FunctionType::get(paramTy, funcTy, extInfo);

      return funcTy;
    }

    Type visitAutoClosureExpr(AutoClosureExpr *expr) {
      llvm_unreachable("Already type-checked");
    }

    Type visitModuleExpr(ModuleExpr *expr) {
      // Module expressions always have a fixed type.
      return expr->getType();
    }

    Type visitInOutExpr(InOutExpr *expr) {
      // The address-of operator produces an explicit inout T from an lvalue T.
      // We model this with the constraint
      //
      //     S < lvalue T
      //
      // where T is a fresh type variable.
      auto lvalue = CS.createTypeVariable(CS.getConstraintLocator(expr),
                                          /*options=*/0);
      auto bound = LValueType::get(lvalue);
      auto result = InOutType::get(lvalue);
      CS.addConstraint(ConstraintKind::Conversion,
                       expr->getSubExpr()->getType(), bound,
                       CS.getConstraintLocator(expr,
                                               ConstraintLocator::AddressOf));
      return result;
    }

    Type visitDynamicTypeExpr(DynamicTypeExpr *expr) {
      auto tv = CS.createTypeVariable(CS.getConstraintLocator(expr),
                                      /*options=*/0);
      CS.addConstraint(ConstraintKind::DynamicTypeOf, tv,
                       expr->getBase()->getType(),
           CS.getConstraintLocator(expr, ConstraintLocator::RvalueAdjustment));
      return tv;
    }

    Type visitThrowExpr(ThrowExpr *expr) {
      // The argument must be convertible to the exception type.
      if (Type exnType =
            CS.getTypeChecker().getExceptionType(CS.DC, expr->getThrowLoc())) {
        CS.addConstraint(ConstraintKind::Conversion,
                         expr->getSubExpr()->getType(), exnType,
                         CS.getConstraintLocator(expr,
                                          ConstraintLocator::ThrownException));
      }

      // TODO: 'throw' should really have bottom type.
      return TupleType::getEmpty(CS.TC.Context);
    }

    Type visitOpaqueValueExpr(OpaqueValueExpr *expr) {
      return expr->getType();
    }

    Type visitDefaultValueExpr(DefaultValueExpr *expr) {
      expr->setType(expr->getSubExpr()->getType());
      return expr->getType();
    }

    Type visitApplyExpr(ApplyExpr *expr) {
      Type outputTy;
      
      auto fnExpr = expr->getFn();
      
      if (isa<DeclRefExpr>(fnExpr)) {
        if (auto fnType = fnExpr->getType()->getAs<AnyFunctionType>()) {
          outputTy = fnType->getResult();
        }
      } else if (auto TE = dyn_cast<TypeExpr>(fnExpr)) {
        outputTy = TE->getType()->getAs<MetatypeType>()->getInstanceType();
        
        if (auto nominalType = outputTy->getAs<NominalType>()) {
          auto NTD = nominalType->getDecl();
          if (!(isa<ClassDecl>(NTD) || isa<StructDecl>(NTD)) ||
              hasFailableInits(NTD, &CS)) {
            outputTy = Type();
          }
        } else {
          outputTy = Type();
        }
      } else if (auto OSR = dyn_cast<OverloadSetRefExpr>(fnExpr)) {
        if (auto FD = dyn_cast<FuncDecl>(OSR->getDecls()[0])) {

          // If we've already agreed upon an overloaded return type, use it.
          if (FD->getHaveSearchedForCommonOverloadReturnType()) {
            
            if (FD->getHaveFoundCommonOverloadReturnType()) {
              outputTy = FD->getType()->getAs<AnyFunctionType>()->getResult();
            }
            
          } else {
          
            // Determine if the overloads all share a common return type.
            Type commonType;
            Type resultType;
            
            for (auto OD : OSR->getDecls()) {
              
              if (auto OFD = dyn_cast<FuncDecl>(OD)) {
                auto OFT = OFD->getType()->getAs<AnyFunctionType>();
                
                if (!OFT) {
                  commonType = Type();
                  break;
                }
                
                resultType = OFT->getResult();
                
                if (commonType.isNull()) {
                  commonType = resultType;
                } else if (!commonType->isEqual(resultType)) {
                  commonType = Type();
                  break;
                }
              } else {
                // TODO: unreachable?
                commonType = Type();
                break;
              }
            }
            
            // TODO: For now, disallow tyvar, archetype and function types.
            if (!(commonType.isNull() ||
                  commonType->getAs<TypeVariableType>() ||
                  commonType->getAs<ArchetypeType>() ||
                  commonType->getAs<AnyFunctionType>())) {
              outputTy = commonType;
            }
            
            // Set the search bits appropriately.
            for (auto OD : OSR->getDecls()) {
              if (auto OFD = dyn_cast<FuncDecl>(OD)) {
                OFD->setHaveSearchedForCommonOverloadReturnType();
                
                if (!outputTy.isNull())
                  OFD->setHaveFoundCommonOverloadReturnType();
              }
            }
          }
        }
      }
      
      // The function subexpression has some rvalue type T1 -> T2 for fresh
      // variables T1 and T2.
      if (outputTy.isNull()) {
        outputTy = CS.createTypeVariable(
                     CS.getConstraintLocator(expr,
                                             ConstraintLocator::ApplyFunction),
                                             /*options=*/0);
      } else {
        // Since we know what the output type is, we can set it as the favored
        // type of this expression.
        CS.setFavoredType(expr, outputTy.getPointer());
      }

      // A direct call to a ClosureExpr makes it noescape.
      FunctionType::ExtInfo extInfo;
      if (isa<ClosureExpr>(fnExpr->getSemanticsProvidingExpr()))
        extInfo = extInfo.withNoEscape();
      
      auto funcTy = FunctionType::get(expr->getArg()->getType(), outputTy,
                                      extInfo);

      CS.addConstraint(ConstraintKind::ApplicableFunction, funcTy,
        expr->getFn()->getType(),
        CS.getConstraintLocator(expr, ConstraintLocator::ApplyFunction));

      return outputTy;
    }

    Type getSuperType(ValueDecl *selfDecl,
                      SourceLoc diagLoc,
                      Diag<> diag_not_in_class,
                      Diag<> diag_no_base_class) {
      DeclContext *typeContext = selfDecl->getDeclContext()->getParent();
      assert(typeContext && "constructor without parent context?!");
      auto &tc = CS.getTypeChecker();
      ClassDecl *classDecl = typeContext->getDeclaredTypeInContext()
                               ->getClassOrBoundGenericClass();
      if (!classDecl) {
        tc.diagnose(diagLoc, diag_not_in_class);
        return Type();
      }
      if (!classDecl->hasSuperclass()) {
        tc.diagnose(diagLoc, diag_no_base_class);
        return Type();
      }

      Type superclassTy = typeContext->getDeclaredTypeInContext()
                            ->getSuperclass(&tc);
      if (selfDecl->getType()->is<AnyMetatypeType>())
        superclassTy = MetatypeType::get(superclassTy);
      return superclassTy;
    }
    
    Type visitRebindSelfInConstructorExpr(RebindSelfInConstructorExpr *expr) {
      // The result is void.
      return TupleType::getEmpty(CS.getASTContext());
    }
    
    Type visitIfExpr(IfExpr *expr) {
      // The conditional expression must conform to LogicValue.
      Expr *condExpr = expr->getCondExpr();
      auto logicValue
        = CS.getTypeChecker().getProtocol(expr->getQuestionLoc(),
                                          KnownProtocolKind::BooleanType);
      if (!logicValue)
        return Type();

      CS.addConstraint(ConstraintKind::ConformsTo, condExpr->getType(),
                       logicValue->getDeclaredType(),
                       CS.getConstraintLocator(condExpr));

      // The branches must be convertible to a common type.
      auto resultTy = CS.createTypeVariable(CS.getConstraintLocator(expr),
                                            TVO_PrefersSubtypeBinding);
      CS.addConstraint(ConstraintKind::Conversion,
                       expr->getThenExpr()->getType(), resultTy,
                       CS.getConstraintLocator(expr,
                                               ConstraintLocator::IfThen));
      CS.addConstraint(ConstraintKind::Conversion,
                       expr->getElseExpr()->getType(), resultTy,
                       CS.getConstraintLocator(expr,
                                               ConstraintLocator::IfElse));
      return resultTy;
    }
    
    Type visitImplicitConversionExpr(ImplicitConversionExpr *expr) {
      llvm_unreachable("Already type-checked");
    }

    Type visitForcedCheckedCastExpr(ForcedCheckedCastExpr *expr) {
      auto &tc = CS.getTypeChecker();
      
      // Validate the resulting type.
      TypeResolutionOptions options = TR_AllowUnboundGenerics;
      options |= TR_InExpression;
      if (tc.validateType(expr->getCastTypeLoc(), CS.DC, options))
        return nullptr;

      // Open the type we're casting to.
      // FIXME: Locator for the cast type?
      auto toType = CS.openType(expr->getCastTypeLoc().getType(),
                                CS.getConstraintLocator(expr));
      expr->getCastTypeLoc().setType(toType, /*validated=*/true);

      auto fromType = expr->getSubExpr()->getType();
      auto locator = CS.getConstraintLocator(expr,
                                     ConstraintLocator::CheckedCastOperand);

      // The source type can be checked-cast to the destination type.
      CS.addConstraint(ConstraintKind::CheckedCast, fromType, toType, locator);

      return toType;
    }

    Type visitCoerceExpr(CoerceExpr *expr) {
      auto &tc = CS.getTypeChecker();
      
      // Validate the resulting type.
      TypeResolutionOptions options = TR_AllowUnboundGenerics;
      options |= TR_InExpression;
      if (tc.validateType(expr->getCastTypeLoc(), CS.DC, options))
        return nullptr;

      // Open the type we're casting to.
      // FIXME: Locator for the cast type?
      auto toType = CS.openType(expr->getCastTypeLoc().getType(),
                                CS.getConstraintLocator(expr));
      expr->getCastTypeLoc().setType(toType, /*validated=*/true);

      auto fromType = expr->getSubExpr()->getType();
      auto locator = CS.getConstraintLocator(expr,
                                     ConstraintLocator::CheckedCastOperand);

      if (CS.shouldAttemptFixes()) {
        Constraint *coerceConstraint =
          Constraint::create(CS, ConstraintKind::ExplicitConversion,
                             fromType, toType, DeclName(), locator);
        Constraint *downcastConstraint =
          Constraint::createFixed(CS, ConstraintKind::CheckedCast,
                                  FixKind::CoerceToCheckedCast, fromType,
                                  toType, locator);
        coerceConstraint->setFavored();
        auto constraints = { coerceConstraint, downcastConstraint };
        CS.addConstraint(Constraint::createDisjunction(CS, constraints,
                                                       locator,
                                                       RememberChoice));
      } else {
        // The source type can be explicitly converted to the destination type.
        CS.addConstraint(ConstraintKind::ExplicitConversion, fromType, toType,
                         locator);
      }

      return toType;
    }

    Type visitConditionalCheckedCastExpr(ConditionalCheckedCastExpr *expr) {
      auto &tc = CS.getTypeChecker();

      // Validate the resulting type.
      TypeResolutionOptions options = TR_AllowUnboundGenerics;
      options |= TR_InExpression;
      if (tc.validateType(expr->getCastTypeLoc(), CS.DC, options))
        return nullptr;

      // Open the type we're casting to.
      // FIXME: Locator for the cast type?
      auto toType = CS.openType(expr->getCastTypeLoc().getType(),
                                CS.getConstraintLocator(expr));
      expr->getCastTypeLoc().setType(toType, /*validated=*/true);

      auto fromType = expr->getSubExpr()->getType();
      auto locator = CS.getConstraintLocator(
                       expr, ConstraintLocator::CheckedCastOperand);
      CS.addConstraint(ConstraintKind::CheckedCast, fromType, toType, locator);
      return OptionalType::get(toType);
    }

    Type visitIsExpr(IsExpr *expr) {
      // Validate the type.
      auto &tc = CS.getTypeChecker();
      TypeResolutionOptions options = TR_AllowUnboundGenerics;
      options |= TR_InExpression;
      if (tc.validateType(expr->getCastTypeLoc(), CS.DC, options))
        return nullptr;

      // Open up the type we're checking.
      // FIXME: Locator for the cast type?
      auto toType = CS.openType(expr->getCastTypeLoc().getType(),
                                CS.getConstraintLocator(expr));
      expr->getCastTypeLoc().setType(toType, /*validated=*/true);

      // Add a checked cast constraint.
      auto fromType = expr->getSubExpr()->getType();
      
      CS.addConstraint(ConstraintKind::CheckedCast, fromType, toType,
                       CS.getConstraintLocator(expr));

      // The result is Bool.
      return CS.getTypeChecker().lookupBoolType(CS.DC);
    }

    Type visitDiscardAssignmentExpr(DiscardAssignmentExpr *expr) {
      // '_' is only allowed in assignments, so give it an AssignDest locator.
      auto locator = CS.getConstraintLocator(expr, 
                                             ConstraintLocator::AssignDest);
      auto typeVar = CS.createTypeVariable(locator, /*options=*/0);
      return LValueType::get(typeVar);
    }
    
    Type visitAssignExpr(AssignExpr *expr) {
      // Compute the type to which the source must be converted to allow
      // assignment to the destination.
      auto destTy = CS.computeAssignDestType(expr->getDest(), expr->getLoc());
      if (!destTy)
        return Type();
      
      // The source must be convertible to the destination.
      auto assignLocator = CS.getConstraintLocator(expr,
                                               ConstraintLocator::AssignSource);
      CS.addConstraint(ConstraintKind::Conversion,
                       expr->getSrc()->getType(), destTy,
                       assignLocator);
      
      expr->setType(TupleType::getEmpty(CS.getASTContext()));
      return expr->getType();
    }
    
    Type visitUnresolvedPatternExpr(UnresolvedPatternExpr *expr) {
      // If there are UnresolvedPatterns floating around after name binding,
      // they are pattern productions in invalid positions.
      CS.TC.diagnose(expr->getLoc(), diag::pattern_in_expr,
                     expr->getSubPattern()->getKind());
      return Type();
    }

    /// Get the type T?
    ///
    ///  This is not the ideal source location, but it's only used for
    /// diagnosing ill-formed standard libraries, so it really isn't
    /// worth QoI efforts.
    Type getOptionalType(SourceLoc optLoc, Type valueTy) {
      auto optTy = CS.getTypeChecker().getOptionalType(optLoc, valueTy);
      if (!optTy || CS.getTypeChecker().requireOptionalIntrinsics(optLoc))
        return Type();

      return optTy;
    }

    Type visitBindOptionalExpr(BindOptionalExpr *expr) {
      // The operand must be coercible to T?, and we will have type T.
      auto locator = CS.getConstraintLocator(expr);

      auto objectTy = CS.createTypeVariable(locator,
                                            TVO_PrefersSubtypeBinding
                                            | TVO_CanBindToLValue);
      
      // The result is the object type of the optional subexpression.
      CS.addConstraint(ConstraintKind::OptionalObject,
                       expr->getSubExpr()->getType(), objectTy,
                       locator);
      return objectTy;
    }
    
    Type visitOptionalEvaluationExpr(OptionalEvaluationExpr *expr) {
      // The operand must be coercible to T? for some type T.  We'd
      // like this to be the smallest possible nesting level of
      // optional types, e.g. T? over T??; otherwise we don't really
      // have a preference.
      auto valueTy = CS.createTypeVariable(CS.getConstraintLocator(expr),
                                           TVO_PrefersSubtypeBinding);

      Type optTy = getOptionalType(expr->getSubExpr()->getLoc(), valueTy);
      if (!optTy)
        return Type();

      CS.addConstraint(ConstraintKind::Conversion,
                       expr->getSubExpr()->getType(), optTy,
                       CS.getConstraintLocator(expr));
      return optTy;
    }

    Type visitForceValueExpr(ForceValueExpr *expr) {
      // Force-unwrap an optional of type T? to produce a T.
      auto locator = CS.getConstraintLocator(expr);

      auto objectTy = CS.createTypeVariable(locator,
                                            TVO_PrefersSubtypeBinding
                                            | TVO_CanBindToLValue);
      
      // The result is the object type of the optional subexpression.
      CS.addConstraint(ConstraintKind::OptionalObject,
                       expr->getSubExpr()->getType(), objectTy,
                       locator);
      return objectTy;
    }

    Type visitOpenExistentialExpr(OpenExistentialExpr *expr) {
      llvm_unreachable("Already type-checked");
    }
    
    Type visitAvailabilityQueryExpr(AvailabilityQueryExpr *expr) {
      Type BoolType = CS.getTypeChecker().lookupBoolType(CS.DC);
      expr->setType(BoolType);
      return BoolType;
    }

    Type visitEditorPlaceholderExpr(EditorPlaceholderExpr *E) {
      return E->getType();
    }

  };

  /// \brief AST walker that "sanitizes" an expression for the
  /// constraint-based type checker.
  ///
  /// This is only necessary because Sema fills in too much type information
  /// before the type-checker runs, causing redundant work.
  class SanitizeExpr : public ASTWalker {
    TypeChecker &TC;
  public:
    SanitizeExpr(TypeChecker &tc) : TC(tc) { }

    std::pair<bool, Expr *> walkToExprPre(Expr *expr) override {
      // Don't recurse into default-value expressions.
      return { !isa<DefaultValueExpr>(expr), expr };
    }

    Expr *walkToExprPost(Expr *expr) override {
      if (auto implicit = dyn_cast<ImplicitConversionExpr>(expr)) {
        // Skip implicit conversions completely.
        return implicit->getSubExpr();
      }

      if (auto dotCall = dyn_cast<DotSyntaxCallExpr>(expr)) {
        // A DotSyntaxCallExpr is a member reference that has already been
        // type-checked down to a call; turn it back into an overloaded
        // member reference expression.
        SourceLoc memberLoc;
        if (auto member = findReferencedDecl(dotCall->getFn(), memberLoc)) {
          auto base = skipImplicitConversions(dotCall->getArg());
          auto members
            = TC.Context.AllocateCopy(ArrayRef<ValueDecl *>(&member, 1));
          return new (TC.Context) OverloadedMemberRefExpr(base,
                                   dotCall->getDotLoc(), members, memberLoc,
                                   expr->isImplicit());
        }
      }

      if (auto dotIgnored = dyn_cast<DotSyntaxBaseIgnoredExpr>(expr)) {
        // A DotSyntaxBaseIgnoredExpr is a static member reference that has
        // already been type-checked down to a call where the argument doesn't
        // actually matter; turn it back into an overloaded member reference
        // expression.
        SourceLoc memberLoc;
        if (auto member = findReferencedDecl(dotIgnored->getRHS(), memberLoc)) {
          auto base = skipImplicitConversions(dotIgnored->getLHS());
          auto members
            = TC.Context.AllocateCopy(ArrayRef<ValueDecl *>(&member, 1));
          return new (TC.Context) OverloadedMemberRefExpr(base,
                                    dotIgnored->getDotLoc(), members,
                                    memberLoc, expr->isImplicit());
        }
      }

      return expr;
    }

    /// \brief Ignore declarations.
    bool walkToDeclPre(Decl *decl) override { return false; }
  };

  class ConstraintWalker : public ASTWalker {
    ConstraintGenerator &CG;

  public:
    ConstraintWalker(ConstraintGenerator &CG) : CG(CG) { }

    std::pair<bool, Expr *> walkToExprPre(Expr *expr) override {
      // For closures containing only a single expression, the body participates
      // in type checking.
      if (auto closure = dyn_cast<ClosureExpr>(expr)) {
        if (closure->hasSingleExpressionBody()) {
          // Visit the closure itself, which produces a function type.
          auto funcTy = CG.visit(expr)->castTo<FunctionType>();
          expr->setType(funcTy);
        }

        return { true, expr };
      }

      // We don't visit default value expressions; they've already been
      // type-checked.
      if (isa<DefaultValueExpr>(expr)) {
        return { false, expr };
      }

      return { true, expr };
    }

    /// \brief Once we've visited the children of the given expression,
    /// generate constraints from the expression.
    Expr *walkToExprPost(Expr *expr) override {
      if (auto closure = dyn_cast<ClosureExpr>(expr)) {
        if (closure->hasSingleExpressionBody()) {
          // Visit the body. It's type needs to be convertible to the function's
          // return type.
          auto resultTy = closure->getResultType();
          auto bodyTy = closure->getSingleExpressionBody()->getType();
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
        expr->setType(CG.getConstraintSystem().simplifyType(type));
        return expr;
      }

      return nullptr;
    }

    /// \brief Ignore statements.
    std::pair<bool, Stmt *> walkToStmtPre(Stmt *stmt) override {
      return { false, stmt };
    }

    /// \brief Ignore declarations.
    bool walkToDeclPre(Decl *decl) override { return false; }
  };

  /// AST walker that records the keyword arguments provided at each
  /// call site.
  class ArgumentLabelWalker : public ASTWalker {
    ConstraintSystem &CS;
    llvm::DenseMap<Expr *, Expr *> ParentMap;

  public:
    ArgumentLabelWalker(ConstraintSystem &cs, Expr *expr) 
      : CS(cs), ParentMap(expr->getParentMap()) { }

    void associateArgumentLabels(Expr *arg, ArrayRef<Identifier> labels,
                                 bool labelsArePermanent) {
      // Our parent must be a call.
      auto call = dyn_cast_or_null<CallExpr>(ParentMap[arg]);
      if (!call) 
        return;

      // We must have originated at the call argument.
      if (arg != call->getArg())
        return;

      // Dig out the function, looking through, parenthses, ?, and !.
      auto fn = call->getFn();
      do {
        fn = fn->getSemanticsProvidingExpr();

        if (auto force = dyn_cast<ForceValueExpr>(fn)) {
          fn = force->getSubExpr();
          continue;
        }

        if (auto bind = dyn_cast<BindOptionalExpr>(fn)) {
          fn = bind->getSubExpr();
          continue;
        }

        break;
      } while (true);

      // Record the labels.
      if (!labelsArePermanent)
        labels = CS.allocateCopy(labels);
      CS.ArgumentLabels[CS.getConstraintLocator(fn)] = labels;
    }

    std::pair<bool, Expr *> walkToExprPre(Expr *expr) override {
      if (auto tuple = dyn_cast<TupleExpr>(expr)) {
        if (tuple->hasElementNames())
          associateArgumentLabels(expr, tuple->getElementNames(), true);
        else {
          llvm::SmallVector<Identifier, 4> names(tuple->getNumElements(),
                                                 Identifier()); 
          associateArgumentLabels(expr, names, false);
        }
      } else if (auto paren = dyn_cast<ParenExpr>(expr)) {
        associateArgumentLabels(paren, { Identifier() }, false);
      }

      return { true, expr };
    }
  };

} // end anonymous namespace

Expr *ConstraintSystem::generateConstraints(Expr *expr) {
  // Remove implicit conversions from the expression.
  expr = expr->walk(SanitizeExpr(getTypeChecker()));

  // Wall the expression to associate labeled argumets.
  expr->walk(ArgumentLabelWalker(*this, expr));

  // Walk the expression, generating constraints.
  ConstraintGenerator cg(*this);
  ConstraintWalker cw(cg);
  
  Expr* result = expr->walk(cw);
  
  if (result)
    this->optimizeConstraints(result);
  
  return result;
}

Expr *ConstraintSystem::generateConstraintsShallow(Expr *expr) {
  // Sanitize the expression.
  expr = SanitizeExpr(getTypeChecker()).walkToExprPost(expr);

  // Visit the top-level expression generating constraints.
  ConstraintGenerator cg(*this);
  auto type = cg.visit(expr);
  if (!type)
    return nullptr;
  
  this->optimizeConstraints(expr);
  
  expr->setType(type);
  return expr;
}

Type ConstraintSystem::generateConstraints(Pattern *pattern,
                                           ConstraintLocatorBuilder locator) {
  ConstraintGenerator cg(*this);
  return cg.getTypeForPattern(pattern, /*forFunctionParam*/ false, locator);
}

void ConstraintSystem::optimizeConstraints(Expr *e) {
  
  SmallVector<Expr *, 16> linkedExprs;
  
  // Collect any linked expressions.
  LinkedExprCollector collector(linkedExprs);
  e->walk(collector);
  
  // Favor types, as appropriate.
  for (auto linkedExpr : linkedExprs) {
    computeFavoredTypeForExpr(linkedExpr, *this);
  }
  
  // Optimize the constraints.
  ConstraintOptimizer optimizer(*this);
  e->walk(optimizer);
}
