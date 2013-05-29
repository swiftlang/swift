//===--- TypeCheckExpr.cpp - Type Checking for Expressions ----------------===//
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
// This file implements semantic analysis for expressions, analysing an
// expression tree in post-order, bottom-up, from leaves up to the root.
//
//===----------------------------------------------------------------------===//

#include "TypeChecker.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/ASTVisitor.h"
#include "swift/AST/Attr.h"
#include "swift/AST/ASTWalker.h"
#include "llvm/ADT/APInt.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/Support/SaveAndRestore.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/STLExtras.h"
using namespace swift;

//===----------------------------------------------------------------------===//
// Expression Semantic Analysis Routines
//===----------------------------------------------------------------------===//

Expr *convertLValueToRValue(TypeChecker &tc, LValueType *srcLV, Expr *E);

Expr *TypeChecker::convertToRValueOld(Expr *E) {
  assert(E && "no expression to load!");

  if (LValueType *lv = E->getType()->getAs<LValueType>())
    return convertLValueToRValue(*this, lv, E);

  return E;
}

bool TypeChecker::semaTupleExpr(TupleExpr *TE) {
  // Compute the result type.
  SmallVector<TupleTypeElt, 8> ResultTyElts(TE->getNumElements());

  for (unsigned i = 0, e = TE->getNumElements(); i != e; ++i) {
    Type EltTy = TE->getElement(i)->getType();
    Identifier Name = TE->getElementName(i);
    ResultTyElts[i] = TupleTypeElt(EltTy, Name);
  }
  
  TE->setType(TupleType::get(ResultTyElts, Context));
  return false;
}

Expr *TypeChecker::semaSubscriptExpr(SubscriptExpr *SE) {
  llvm_unreachable("Shouldn't get here");
}

/// collectArchetypeToExistentialSubstitutions - Collect a set of substitutions
/// from each archetype in the given protocol to a protocol composition type
/// that describes the requirements placed on that archetype.
static void
collectArchetypeToExistentialSubstitutions(ASTContext &Context,
                                           ProtocolDecl *Proto,
                                           Type ThisTy,
                                           TypeSubstitutionMap &Substitutions) {
  for (auto Member : Proto->getMembers()) {
    auto AssocType = dyn_cast<TypeAliasDecl>(Member);
    if (!AssocType)
      continue;
    
    ArchetypeType *Archetype
      = AssocType->getDeclaredType()->castTo<ArchetypeType>();
    
    // FIXME: Identify 'this' in a rather more sane way.
    if (AssocType->getName().str().equals("This"))
      Substitutions[Archetype] = ThisTy;
    else {
      if (Archetype->getConformsTo().size() == 1)
        Substitutions[Archetype]
          = Archetype->getConformsTo().front()->getDeclaredType();
      else {
        SmallVector<Type, 4> ConformsTo;
        std::transform(Archetype->getConformsTo().begin(),
                       Archetype->getConformsTo().end(),
                       std::back_inserter(ConformsTo),
                       [](ProtocolDecl *P) { return P->getDeclaredType(); });
        Substitutions[Archetype]
          = ProtocolCompositionType::get(Context, ConformsTo);
      }
    }
  }
}

Expr *TypeChecker::semaSubscriptExpr(ExistentialSubscriptExpr *E) {
  llvm_unreachable("Shouldn't get here");
}

Expr *TypeChecker::semaSubscriptExpr(ArchetypeSubscriptExpr *E) {
  llvm_unreachable("Shouldn't get here");
}

Expr *TypeChecker::semaSubscriptExpr(GenericSubscriptExpr *E) {
  llvm_unreachable("Shouldn't get here");
}

/// \brief Retrieve the declaration that this expression references.
static ValueDecl *getReferencedDecl(Expr *E) {
  E = E->getSemanticsProvidingExpr();
  if (auto SE = dyn_cast<SpecializeExpr>(E))
    E = SE->getSubExpr()->getSemanticsProvidingExpr();

  if (auto DRE = dyn_cast<DeclRefExpr>(E))
    return DRE->getDecl();
  if (auto MRE = dyn_cast<MemberRefExpr>(E))
    return MRE->getDecl();
  if (auto EMR = dyn_cast<ExistentialMemberRefExpr>(E))
    return EMR->getDecl();
  if (auto AMR = dyn_cast<ArchetypeMemberRefExpr>(E))
    return AMR->getDecl();
  if (auto GMR = dyn_cast<GenericMemberRefExpr>(E))
    return GMR->getDecl();
  if (auto Sub = dyn_cast<SubscriptExpr>(E))
    return Sub->getDecl();
  if (auto ES = dyn_cast<ExistentialSubscriptExpr>(E))
    return ES->getDecl();
  if (auto AS = dyn_cast<ArchetypeSubscriptExpr>(E))
    return AS->getDecl();
  if (auto GS = dyn_cast<GenericSubscriptExpr>(E))
    return GS->getDecl();

  llvm_unreachable("Expression does not reference a declaration");
}

void substituteInputSugarArgumentType(Type argTy,
                                      CanType resultTy,
                                      Type &resultSugarTy,
                                      bool &uniqueSugarTy) {
  // If we already failed finding a unique sugar, bail out.
  if (!uniqueSugarTy)
    return;
    
  if (TupleType *argTupleTy = argTy->getAs<TupleType>()) {
    // Recursively walk tuple arguments.
    for (auto &field : argTupleTy->getFields()) {
      substituteInputSugarArgumentType(field.getType(), resultTy,
                                       resultSugarTy, uniqueSugarTy);
      if (!uniqueSugarTy)
        return;
    }
  } else {
    if (argTy->getCanonicalType() == resultTy) {
      if (resultSugarTy) {
        // Make sure this argument's sugar is consistent with the sugar we
        // already found.
        if (argTy->isSpelledLike(resultSugarTy))
          return;
        uniqueSugarTy = false;
        return;
      } else {
        resultSugarTy = argTy;
      }
    }
  }
}

/// If the inputs to an apply expression use a consistent "sugar" type
/// (that is, a typealias or shorthand syntax) equivalent to the result type of
/// the function, set the result type of the expression to that sugar type.
Expr *TypeChecker::substituteInputSugarTypeForResult(ApplyExpr *E) {
  if (!E->getType() || E->getType()->is<ErrorType>())
    return E;
  
  Type argTy = E->getArg()->getType();
  
  CanType resultTy = E->getFn()->getType()->castTo<FunctionType>()->getResult()
    ->getCanonicalType();
  
  Type resultSugarTy; // null if no sugar found, set when sugar found
  bool uniqueSugarTy = true; // true if a unique sugar mapping found
  
  substituteInputSugarArgumentType(argTy, resultTy,
                                   resultSugarTy, uniqueSugarTy);
  
  if (resultSugarTy && uniqueSugarTy)
    E->setType(resultSugarTy);

  return E;
}

Expr *TypeChecker::semaApplyExpr(ApplyExpr *E) {
  Expr *E1 = E->getFn();
  Expr *E2 = E->getArg();

  // If the callee was erroneous, silently propagate the error up.
  if (E1->getType()->is<ErrorType>()) {
    E->setType(E1->getType());
    return E;
  }
  
  // If the args are erroneous, propagate the error up. If the callee type is
  // unresolved (for instance, if it's overloaded), we will also not be able
  // to resolve it, so propagate the error to the callee as well.
  if (E2->getType()->is<ErrorType>()) {
    E->setType(E2->getType());
    E1->setType(E2->getType());
    return E;
  }
  
  // If the arg is a 'super' ref, flag the apply as a 'super' call.
  bool isSuper = isa<SuperRefExpr>(E->getArg());

  // Perform lvalue-to-rvalue conversion on the function.
  E1 = convertToRValueOld(E1);
  if (!E1) return nullptr;

  // If we have a concrete function type, then we win.
  if (FunctionType *FT = E1->getType()->getAs<FunctionType>()) {
    // If this is an operator, make sure that the declaration found was declared
    // as such.
    CoercionKind Kind = CoercionKind::Normal;
    if (isa<PrefixUnaryExpr>(E) || isa<PostfixUnaryExpr>(E) ||
        isa<BinaryExpr>(E)) {
      auto D = getReferencedDecl(E1);
      
      if (!D->isOperator()) {
        diagnose(E1->getLoc(),
                 !isa<BinaryExpr>(E) ? diag::unary_op_without_attribute
                                     : diag::binary_op_without_attribute);
        return nullptr;
      }

      if (D->getAttrs().isAssignment())
        Kind = CoercionKind::Assignment;
    }
    
    // We have a function application.  Check that the argument type matches the
    // expected type of the function.
    // If this is, syntactically, an expression x.f where 'f' is a method in
    // the type of 'x', we're coercing the object argument ('this').
    if (isa<ThisApplyExpr>(E))
      E2 = coerceObjectArgument(E2, FT->getInput());
    else {
      CoercedExpr CoercedE2 = coerceToType(E2, FT->getInput(), Kind);
      switch (CoercedE2.getKind()) {
      case CoercionResult::Succeeded:
        E2 = CoercedE2.getExpr();
        break;

      case CoercionResult::Failed:
        E2 = 0;
        break;

      case CoercionResult::Unknowable:
        // Leave E2 as it is; we'll end up type-checking again later anyway.
        break;
      }
    }
    
    if (E2 == 0) {
      diagnose(E1->getLoc(), diag::while_converting_function_argument,
               FT->getInput())
        .highlight(E->getArg()->getSourceRange());
      return 0;
    }

    E->setFn(E1);
    E->setArg(E2);
    E->setType(FT->getResult());
    E->setIsSuper(isSuper);
    return substituteInputSugarTypeForResult(E);
  }
  
  // If the "function" is actually a type (i.e. its type is 'metatype'), then we
  // have an argument list applied to a type, which is construction of the
  // type.
  if (MetaTypeType *MT = E1->getType()->getAs<MetaTypeType>()) {
    // The metatype represents an arbitrary named type: dig through the
    // TypeAlias to see what we're dealing with.  If the typealias was erroneous
    // then silently squish this erroneous subexpression.
    Type Ty = MT->getInstanceType();

    E->setType(Ty);
    if (Ty->is<ErrorType>())
      return 0;  // Squelch an erroneous subexpression.

    // If the 'function' is a tuple type, coerce to it.
    if (Ty->is<TupleType>()) {
      E2 = coerceToType(E2, Ty);
      if (!E2)
        return 0;

      // FIXME: Need a new AST for "coerced with construction syntax".
      return E2;
    }

    // If the 'function' is a direct reference to a type, this is a
    // construction. The type needs to be a struct or oneof.
    if (!Ty->getNominalOrBoundGenericNominal()) {
      diagnose(E->getLoc(), diag::cannot_construct_type, Ty);
      E->setType(ErrorType::get(Context));
      return 0;
    }

    // Check for a nominal type with a constructor.
    SmallVector<ValueDecl *, 4> Viable;
    SmallVector<ValueDecl *, 4> ctors;
    if (lookupConstructors(Ty, ctors)) {
      auto Best = filterOverloadSet(ctors, false, Ty, E2, Type(), Viable);
      if (Best) {
        ValueDecl *BestDecl = Best.getDecl();
        Expr *Ref = new (Context) DeclRefExpr(BestDecl, E1->getStartLoc(),
                                              BestDecl->getType());
        E1 = new (Context) ConstructorRefCallExpr(Ref, E1);
        E1 = recheckTypes(E1);
        E->setFn(E1);
        return semaApplyExpr(E);
      }
    }
    if (!E2->getType()->isUnresolvedType()) {
      diagnose(E->getLoc(), diag::constructor_overload_fail, !Viable.empty(), Ty)
        .highlight(E->getSourceRange());
      printOverloadSetCandidates(Viable);
      return 0;
    }
    return E;
  }

  // Otherwise, the function's type must be unresolved or generic.  If it is
  // something else, we have a type error.
  if (!E1->getType()->isUnresolvedType() &&
      !E1->getType()->getAs<PolymorphicFunctionType>()) {
    diagnose(E1->getLoc(), diag::called_expr_isnt_function);
    return 0;
  }

  // Otherwise, we must have an application to overloaded set.  See if we can
  // resolve which overload member is based on the argument type.
  OverloadedExpr Ovl = getOverloadedExpr(E1);
  if (!Ovl) {
    // For polymorphic functions, we'll attempt to deduce the generic arguments.
    if (auto PolyFn = E1->getType()->getAs<PolymorphicFunctionType>()) {
      if (OverloadCandidate Ovl = checkPolymorphicApply(PolyFn,
                                                        CoercionKind::Normal,
                                                        E2, Type())) {
        E1 = buildSpecializeExpr(E1, Ovl.getType(), Ovl.getSubstitutions(),
                                 Ovl.getConformances(),
                                 /*ArchetypesAreOpen=*/true,
                                 /*OnlyInnermostParams=*/true);
        E->setFn(E1);
        return semaApplyExpr(E);
      }
    }

    // If not, just use the unresolved type.
    E->setType(UnstructuredUnresolvedType::get(Context));
    return E;
  }

  // Perform overload resolution.
  llvm::SmallVector<ValueDecl *, 4> Viable;
  auto Best = filterOverloadSet(Ovl.getCandidates(),
                                (isa<PrefixUnaryExpr>(E) ||
                                 isa<PostfixUnaryExpr>(E) ||
                                 isa<BinaryExpr>(E)),
                                 Ovl.getBaseType(), E2,
                                 Type(), Viable);
  
  // If we have a best candidate, build a call to it now.
  if (Best) {
    E1 = buildFilteredOverloadSet(Ovl, Best);
    E->setFn(convertToRValueOld(E1));
    return semaApplyExpr(E);
  }
  
  // If there are no more viable candidates, complain now.
  if (Viable.empty()) {
    diagnoseEmptyOverloadSet(E, Ovl.getCandidates());
    return nullptr;
  }
  
  // We have more than one viable candidate. This might be resolved when/if
  // we get a destination type to coerce to.  
  E->setType(UnstructuredUnresolvedType::get(Context));
  if (Viable.size() == Ovl.getCandidates().size())
    return E;
  
  // We have trimmed the overload set; rebuild the overload set so that we
  // no longer have to consider those non-matching candidates.
  // FIXME: We may simple want to mark them as non-viable in the overload set
  // itself, so we can provide them in diagnostics.
  E->setFn(buildFilteredOverloadSet(Ovl, Viable));
  return E;
}

//===----------------------------------------------------------------------===//
// Expression Reanalysis - SemaExpressionTree
//===----------------------------------------------------------------------===//

namespace {
/// SemaExpressionTree - This class implements bottom-up (aka "leaf to root",
/// analyzing 1 and 4 before the + in "1+4") semantic analysis of an
/// already-existing expression tree.  This is performed when a closure is
/// formed and anonymous decls like "$4" get a concrete type associated with
/// them.  During the initial parse, these decls get a 'unresolved' type, which
/// disables most semantic analysis associated with them.
///
/// When the expression tree is bound to a context, the anonymous decls get a
/// concrete type and we have to rescan the tree to assign types to
/// intermediate nodes, introduce type coercion etc.  This visitor does this
/// job.  Each visit method reanalyzes the children of a node, then reanalyzes
/// the node, and returns true on error.
class SemaExpressionTree : 
  public ASTWalker, public ExprVisitor<SemaExpressionTree, Expr*> {
public:
  TypeChecker &TC;
  
  Expr *visitErrorExpr(ErrorExpr *E) {
    if (E->getType().isNull())
      E->setType(TC.Context.TheErrorType);
    return E;
  }

  Expr *visitLiteralExpr(LiteralExpr *E) {
    if (E->getType().isNull())
      E->setType(UnstructuredUnresolvedType::get(TC.Context));
    return E;
  }
  Expr *visitDeclRefExpr(DeclRefExpr *E) {
    if (E->getDecl() == 0) {
      TC.diagnose(E->getLoc(), diag::use_undeclared_identifier);
      return 0;
    }
    
    // If the type of a decl is not computed yet, make the declref unresolved;
    // this can happen for references to "$0" etc.
    if (!E->getDecl()->hasType()) {
      if (E->getDecl()->isInvalid()) {
        E->setType(ErrorType::get(TC.Context));
        return 0;
      } else {
        E->setType(UnstructuredUnresolvedType::get(TC.Context));
        return E;
      }
    }
    
    // If the decl had an invalid type, then an error has already been emitted,
    // just propagate it up.
    if (E->getDecl()->getType()->is<ErrorType>())
      return 0;

    // TODO: QOI: If the decl had an "invalid" bit set, then return the error
    // object to improve error recovery.

    E->setType(E->getDecl()->getTypeOfReference());
    return E;
  }
  Expr *visitOverloadSetRefExpr(OverloadSetRefExpr *E) {
    E->setType(UnstructuredUnresolvedType::get(TC.Context));
    return E;
  }
  Expr *visitUnresolvedDeclRefExpr(UnresolvedDeclRefExpr *E) {
    llvm_unreachable("name binding should resolve all UnresolvedDeclRefExprs!");
    return 0;
  }
  Expr *visitUnresolvedSpecializeExpr(UnresolvedSpecializeExpr *E) {
    TC.diagnose(E->getLoc(), diag::requires_constraint_checker);
    return nullptr;
  }
  Expr *visitSuperRefExpr(SuperRefExpr *E) {
    TC.diagnose(E->getLoc(), diag::requires_constraint_checker);
    return nullptr;
  }
  Expr *visitOtherConstructorDeclRefExpr(OtherConstructorDeclRefExpr *E) {
    TC.diagnose(E->getLoc(), diag::requires_constraint_checker);
    return nullptr;
  }
  Expr *visitUnresolvedConstructorExpr(UnresolvedConstructorExpr *E) {
    TC.diagnose(E->getLoc(), diag::requires_constraint_checker);
    return nullptr;
  }
  Expr *visitMemberRefExpr(MemberRefExpr *E) {
    if (E->getDecl()->getType()->is<ErrorType>())
      return nullptr;
    
    // Ensure that the base is an lvalue, materializing it if is not an
    // lvalue yet.
    Type ContainerTy
      = E->getDecl()->getDeclContext()->getDeclaredTypeOfContext();
    if (Expr *Base = TC.coerceObjectArgument(E->getBase(), ContainerTy))
      E->setBase(Base);
    else
      return nullptr;

    // Compute the final lvalue type and we're done.
    E->setType(LValueType::get(E->getDecl()->getType(),
                               LValueType::Qual::DefaultForMemberAccess,
                               TC.Context));
    return E;
  }
    
  Expr *visitExistentialMemberRefExpr(ExistentialMemberRefExpr *E) {
    if (E->getDecl()->getType()->is<ErrorType>())
      return nullptr;
    
    // Ensure that the base is an lvalue, materializing it if is not an
    // lvalue yet.
    Type ContainerTy
      = E->getDecl()->getDeclContext()->getDeclaredTypeOfContext();
    if (Expr *Base = TC.coerceObjectArgument(E->getBase(), ContainerTy))
      E->setBase(Base);
    else
      return nullptr;
    
    // Determine the type of the member.
    Type MemberTy = E->getDecl()->getType();
    if (isa<FuncDecl>(E->getDecl())) {
      if (auto FuncTy = MemberTy->getAs<AnyFunctionType>())
        MemberTy = FuncTy->getResult();
    } else {
      MemberTy = LValueType::get(MemberTy,
                                 LValueType::Qual::DefaultForMemberAccess,
                                 TC.Context);
    }
      
    // For each of the archetypes in the protocol, substitute an existential
    // type that meets the same requirements.
    ProtocolDecl *Proto = ContainerTy->castTo<ProtocolType>()->getDecl();
    TypeSubstitutionMap Substitutions;
    collectArchetypeToExistentialSubstitutions(TC.Context, Proto, ContainerTy,
                                               Substitutions);
    
    // Substitute the existential types into the member type.
    Type SubstMemberTy = TC.substType(MemberTy, Substitutions);
    if (!SubstMemberTy) {
      E->setType(ErrorType::get(TC.Context));
      return nullptr;
    }

    // FIXME: For now, we don't allow any associated types to show up. We
    // may relax this restriction later.
    TC.validateTypeSimple(SubstMemberTy);
    if (!SubstMemberTy->isEqual(MemberTy)) {
      TC.diagnose(E->getDotLoc(), diag::existential_member_assoc_types,
                  E->getDecl()->getName(), Proto->getDeclaredType(),
                  MemberTy);
      TC.diagnose(E->getDecl(), diag::decl_declared_here,
                  E->getDecl()->getName());
    }
    
    E->setType(SubstMemberTy);
    return E;
  }

  Expr *visitArchetypeMemberRefExpr(ArchetypeMemberRefExpr *E) {
    if (E->getDecl()->getType()->is<ErrorType>())
      return nullptr;

    Type Archetype = E->getArchetype();
    bool isMetatypeBase = E->getBase()->getType()->is<MetaTypeType>();

    // If we're going to use the base, make sure it's an lvalue, materializing
    // it if needed.
    if (!E->isBaseIgnored() && !isMetatypeBase) {
      // Ensure that the base is an lvalue, materializing it if is not an
      // lvalue yet.
      if (Expr *Base = TC.coerceObjectArgument(E->getBase(), Archetype))
        E->setBase(Base);
      else
        return nullptr;
    }
    
    // Determine the type of the member.
    Type MemberTy = E->getDecl()->getType();

    if (!E->isBaseIgnored()) {
      if (auto Method = dyn_cast<FuncDecl>(E->getDecl())) {
        if (!isMetatypeBase || Method->isStatic()) {
          if (auto FuncTy = MemberTy->getAs<AnyFunctionType>())
            MemberTy = FuncTy->getResult();
        }
      } else {
        MemberTy = LValueType::get(MemberTy,
                                   LValueType::Qual::DefaultForMemberAccess,
                                   TC.Context);
      }
    }
    
    // Substitute each of the associated types into the member type.
    MemberTy = TC.substMemberTypeWithBase(MemberTy, E->getDecl(), Archetype);
    if (!MemberTy)
      return nullptr;

    E->setType(MemberTy);
    return E;
  }

  Expr *visitGenericMemberRefExpr(GenericMemberRefExpr *E) {
    if (E->getDecl()->getType()->is<ErrorType>()) {
      E->setType(ErrorType::get(TC.Context));
      return nullptr;
    }

    Type GenericTy = E->getBase()->getType()->getRValueType();
    if (GenericTy->is<MetaTypeType>())
      GenericTy = GenericTy->castTo<MetaTypeType>()->getInstanceType();

    // If we're going to use the base, make sure it's an lvalue, materializing
    // it if needed.
    if (!E->isBaseIgnored()) {
      // Ensure that the base is an lvalue, materializing it if is not an
      // lvalue yet.
      if (Expr *Base = TC.coerceObjectArgument(E->getBase(), GenericTy))
        E->setBase(Base);
      else {
        E->setType(ErrorType::get(TC.Context));
        return nullptr;
      }
    }
    
    // Substitute the known base into the member's type.
    CoercionContext CC(TC);
    GenericParamList *GenericParams = nullptr;
    Type MemberTy
      = TC.substBaseForGenericTypeMember(E->getDecl(), GenericTy,
                                         E->getDecl()->getTypeOfReference(),
                                         E->getDotLoc(), CC, &GenericParams);
    if (!MemberTy) {
      E->setType(ErrorType::get(TC.Context));
      return nullptr;
    }

    E->setSubstitutions(
      TC.encodeSubstitutions(GenericParams,
                             CC.Substitutions, CC.Conformance, true, false));
    E->setType(MemberTy);
    return E;
  }

  Expr *visitUnresolvedMemberExpr(UnresolvedMemberExpr *E) {
    E->setType(UnstructuredUnresolvedType::get(TC.Context));
    return E;
  }

  Expr *visitParenExpr(ParenExpr *E) {
    E->setType(E->getSubExpr()->getType());
    return E;
  }
  
  Expr *visitTupleExpr(TupleExpr *E) {
    if (TC.semaTupleExpr(E))
      return 0;
    return E;
  }
    
  Expr *visitCollectionExpr(CollectionExpr *E) {
    TC.diagnose(E->getLoc(), diag::requires_constraint_checker);
    return nullptr;
  }

  Expr *visitSubscriptExpr(SubscriptExpr *E) {
    return E;
  }

  Expr *visitExistentialSubscriptExpr(ExistentialSubscriptExpr *E) {
    return TC.semaSubscriptExpr(E);
  }
  Expr *visitArchetypeSubscriptExpr(ArchetypeSubscriptExpr *E) {
    return TC.semaSubscriptExpr(E);
  }
  Expr *visitGenericSubscriptExpr(GenericSubscriptExpr *E) {
    return TC.semaSubscriptExpr(E);
  }

  Expr *visitOverloadedSubscriptExpr(OverloadedSubscriptExpr *E) {
    E->setType(UnstructuredUnresolvedType::get(TC.Context));
    return E;
  }
    
  Expr *visitUnresolvedDotExpr(UnresolvedDotExpr *E) {
    return TC.semaUnresolvedDotExpr(E);
  }
    
  Expr *visitTupleElementExpr(TupleElementExpr *E) {
    // TupleElementExpr is fully resolved.
    // FIXME: This will not always be the case?
    assert(!E->getType()->isUnresolvedType());
    return E;
  }

  Expr *visitNewArrayExpr(NewArrayExpr *E) {
    if (TC.validateType(E->getElementTypeLoc()))
      return nullptr;

    Type resultType = E->getElementTypeLoc().getType();
    if (resultType->isUnresolvedType()) {
      E->setType(UnstructuredUnresolvedType::get(TC.Context));
      return E;
    } else if (resultType->is<ErrorType>()) {
      E->setType(resultType);
      return nullptr;
    }

    // Walk backward over the non-initial bounds.  These must all be
    // either slices (i.e. []) or constant (i.e. [6]).
    for (unsigned i = E->getBounds().size(); i != 1; --i) {
      auto &bound = E->getBounds()[i-1];

      // If it's a slice, build the appropriate slice type.
      if (!bound.Value) {
        resultType = TC.getArraySliceType(bound.Brackets.Start, resultType);
        if (resultType.isNull()) return nullptr;
        continue;
      }

      // Otherwise, build the appropriate constant array type.

      // Force the array type to be constant.
      if (TC.typeCheckArrayBound(bound.Value, /*requireConstant*/ true))
        return nullptr;

      // Compute the bound; we already checked that it was non-zero.
      IntegerLiteralExpr *lit = cast<IntegerLiteralExpr>(bound.Value);
      uint64_t size = lit->getValue().getZExtValue();

      resultType = ArrayType::get(resultType, size, TC.Context);
    }

    // Okay, we've built up the actual element type now.
    E->setElementType(resultType);

    // Convert the outer bound to some integral type.
    auto &outerBound = E->getBounds()[0];
    assert(outerBound.Value);
    if (TC.typeCheckArrayBound(outerBound.Value, /*requireConstant*/ false))
      return nullptr;

    // Try to build the appropriate slice type.
    resultType = TC.getArraySliceType(outerBound.Brackets.Start, resultType);
    if (resultType.isNull()) return nullptr;
    E->setType(resultType);

    // Delay checking the injection function while the bounds type is
    // unresolved.
    if (outerBound.Value->getType()->isUnresolvedType())
      return E;

    // Find the appropriate injection function.
    ArraySliceType *sliceType = cast<ArraySliceType>(resultType.getPointer());

    // Check that the injection member reference has the appropriate
    // function type.
    Expr* injectionFn =
        TC.buildArrayInjectionFnRef(sliceType, outerBound.Value->getType(),
                                    E->getNewLoc());
    if (!injectionFn)
      return nullptr;

    E->setInjectionFunction(injectionFn);

    return E;
  }

  Expr *visitMetatypeExpr(MetatypeExpr *E) {
    if (Expr *base = E->getBase()) {
      if (!base->getType()->is<ErrorType>())
        E->setType(MetaTypeType::get(base->getType(), TC.Context));
    }

    // Otherwise, TypeOfExpr is fully type-checked.
    return E;
  }

  Expr *visitOpaqueValueExpr(OpaqueValueExpr *E) {
    // OpaqueValueExpr is fully type-checked.
    return E;
  }

  Expr *visitZeroValueExpr(ZeroValueExpr *E) {
    // ZeroValueExpr is fully type-checked.
    return E;
  }

  Expr *visitDefaultValueExpr(DefaultValueExpr *E) {
    E->setType(E->getSubExpr()->getType());
    return E;
  }

  Expr *visitDotSyntaxBaseIgnoredExpr(DotSyntaxBaseIgnoredExpr *E) {
    // DotSyntaxBaseIgnoredExpr is fully type checked.
    return E;
  }

  Expr *visitCoerceExpr(CoerceExpr *E) {
    // The type of the expr is always the type that the MetaType LHS specifies.
    assert(!E->getType()->isUnresolvedType() &&"Type always specified by cast");
    if (TC.validateType(E->getTypeLoc())) {
      E->setType(ErrorType::get(TC.Context));
      return 0;
    }
    return E;
  }

  Expr *visitUncheckedDowncastExpr(UncheckedDowncastExpr *E) {
    // The type of the expr is always the type specified.
    assert(!E->getType()->isUnresolvedType() &&"Type always specified by cast");
    if (TC.validateType(E->getTypeLoc())) {
      E->setType(ErrorType::get(TC.Context));
      return 0;
    }

    Expr *subExpr = TC.coerceToRValue(E->getSubExpr());
    if (!subExpr) {
      E->setType(ErrorType::get(TC.Context));
      return 0;
    }
    E->setSubExpr(subExpr);

    Type ty = E->getType();
    Type srcTy = E->getSubExpr()->getType();
    auto srcArchetype = srcTy->getAs<ArchetypeType>();
    for (auto superTy = TC.getSuperClassOf(ty); superTy;
         superTy = TC.getSuperClassOf(superTy)) {
      if (superTy->isEqual(srcTy)) {
        // If the source had archetype type, convert to its superclass
        // first. Then, we downcast that object.
        if (srcArchetype) {
          subExpr = new (TC.Context) ArchetypeToSuperExpr(
                                                  subExpr,
                                                  srcArchetype->getSuperclass());
        }

        // If the destination type is an archetype, this is a
        // super-to-archetype cast.
        if (ty->is<ArchetypeType>()) {
          return new (TC.Context) UncheckedSuperToArchetypeExpr(subExpr,
                                                             subExpr->getEndLoc(),
                                                             subExpr->getEndLoc(),
                                                             E->getTypeLoc());
        }

        return E;
      }
    }
    
    return E;
  }

  Expr *visitUncheckedSuperToArchetypeExpr(UncheckedSuperToArchetypeExpr *E) {
    // The type of the expr is always the type specified.
    assert(!E->getType()->isUnresolvedType() &&"Type always specified by cast");
    TC.validateType(E->getTypeLoc());
    return E;
  }
    
  Expr *visitIsSubtypeExpr(IsSubtypeExpr *E) {
    // The type of the expr is always Bool.
    assert(!E->getType()->isUnresolvedType() &&"Type always Bool");
    return E;
  }

  Expr *visitImplicitConversionExpr(ImplicitConversionExpr *E) {
    assert(!E->getType()->isUnresolvedType());
    // Implicit conversions have been fully checked.
    return E;
  }
  
  Expr *visitAddressOfExpr(AddressOfExpr *E) {
    // Turn l-values into explicit l-values.
    if (E->getSubExpr()->getType()->is<LValueType>()) {
      if (auto *AddrOf = dyn_cast<AddressOfExpr>(
                           E->getSubExpr()->getSemanticsProvidingExpr()))
        TC.diagnose(E->getLoc(), diag::address_of_address)
          .highlight(AddrOf->getSourceRange())
          .fixItRemove(SourceRange(E->getLoc()));
      
      E->setType(E->getSubExpr()->getType());
      return E;
    }

    // Propagate out dependence.
    if (E->getSubExpr()->getType()->isUnresolvedType()) {
      E->setType(UnstructuredUnresolvedType::get(TC.Context));
      return E;
    }

    // Complain.
    TC.diagnose(E->getLoc(), diag::address_of_rvalue,
                E->getSubExpr()->getType())
      .highlight(E->getSourceRange());
    return nullptr;
  }

  Expr *makeBinOp(Expr *Op, Expr *LHS, Expr *RHS, bool TypeCheckAST);
  Expr *foldSequence(Expr *LHS,
                     ArrayRef<Expr*> &S, unsigned MinPrecedence,
                     bool TypeCheckAST);
  Expr *visitSequenceExpr(SequenceExpr *E, bool TypeCheckAST);

  Expr *visitSequenceExpr(SequenceExpr *E) {
    return visitSequenceExpr(E, /*TypeCheckAST=*/true);
  }

  void PreProcessBraceStmt(BraceStmt *BS);
  
  Expr *visitFuncExpr(FuncExpr *E) {
    llvm_unreachable("Should not walk into FuncExprs!");
  }

  Expr *visitPipeClosureExpr(PipeClosureExpr *expr) {
    llvm_unreachable("Should not walk into PipeClosureExprs!");
  }

  Expr *visitModuleExpr(ModuleExpr *E) {
    // ModuleExpr is fully resolved.
    assert(!E->getType()->isUnresolvedType());
    return E;
  }

  Expr *visitImplicitClosureExpr(ImplicitClosureExpr *E) {
    // ImplicitClosureExpr is fully resolved.
    assert(!E->getType()->isUnresolvedType());
    return E;
  }

  Expr *visitApplyExpr(ApplyExpr *E) {
    return TC.semaApplyExpr(E);
  }
    
  Expr *visitRebindThisInConstructorExpr(RebindThisInConstructorExpr *E) {
    TC.diagnose(E->getLoc(), diag::requires_constraint_checker);
    return nullptr;
  }
    
  Expr *visitIfExpr(IfExpr *E) {
    TC.diagnose(E->getLoc(), diag::requires_constraint_checker);
    return nullptr;
  }
    
  Expr *visitUnresolvedIfExpr(UnresolvedIfExpr *E) {
    llvm_unreachable("this node should be eliminated by name binding");
  }
  Expr *visitUnresolvedElseExpr(UnresolvedElseExpr *E) {
    llvm_unreachable("this node should be eliminated by name binding");
  }
  
  SemaExpressionTree(TypeChecker &tc) : TC(tc) {}
  

  Expr *doIt(Expr *E) {
    return E->walk(*this);
  }

  std::pair<bool, Expr *> walkToExprPre(Expr *E) override {
    // Do not walk into FuncExpr or explicit closures.  They are analyzed
    // modularly, so we don't need to recurse into them and reanalyze their
    // body.  This prevents N^2 re-sema activity with lots of nested closures.
    if (FuncExpr *FE = dyn_cast<FuncExpr>(E)) {
      TC.semaFuncExpr(FE, /*isFirstPass*/false, /*allowUnknownTypes*/true);
      return { false, E };
    }

    // Only walk into Explicit Closures if they haven't been seen at all yet.
    // This ensures that everything gets a type, even if it is an
    // UnstructuredUnresolvedType.
    return { E->getType().isNull(), E };
  }

  Expr *walkToExprPost(Expr *E) {
    // Dispatch to the right visitor case in the post-order walk.  We know
    // that the operands have already been processed and are valid.
    return this->visit(E);
  }

  std::pair<bool, Stmt *> walkToStmtPre(Stmt *S) override {
    // Never recurse into statements.
    return { false, S };
  }
};
} // end anonymous namespace.

/// Is the given expression a valid thing to use as the injection
/// function from the data for a newly-allocated array into the
/// given slice type?
Expr *TypeChecker::buildArrayInjectionFnRef(ArraySliceType *sliceType,
                                            Type lenTy, SourceLoc Loc) {
  // Build the expression "Slice<T>".
  Expr *sliceTypeRef =
    new (Context) MetatypeExpr(nullptr, Loc,
                               MetaTypeType::get(sliceType, Context));

  // Build the expression "Slice<T>.convertFromHeapArray".
  Expr *injectionFn = semaUnresolvedDotExpr(
    new (Context) UnresolvedDotExpr(sliceTypeRef, Loc,
               Context.getIdentifier("convertFromHeapArray"),
                                     Loc));
  if (!injectionFn) return nullptr;

  // The input is a tuple type:
  TupleTypeElt argTypes[3] = {
    // The first element is Builtin.RawPointer.
    // FIXME: this should probably be either UnsafePointer<T> or the
    // first two arguments should be combined into a byref(heap).
    Context.TheRawPointerType,

    // The second element is the owner pointer, Builtin.ObjectPointer.
    Context.TheObjectPointerType,

    // The third element is the bound type.  Maybe this should be a
    // target-specific size_t type?
    lenTy
  };

  Type input = TupleType::get(argTypes, Context);

  // The result is just the slice type.
  Type result = sliceType;

  FunctionType *fnTy = FunctionType::get(input, result, Context);

  // FIXME: this produces terrible diagnostics.
  return coerceToType(injectionFn, fnTy);
}

static Type makeSimilarLValue(Type objectType, Type lvalueType,
                              ASTContext &Context) {
  LValueType::Qual qs = lvalueType->castTo<LValueType>()->getQualifiers();
  return LValueType::get(objectType, qs, Context);
}

Expr *TypeChecker::semaUnresolvedDotExpr(UnresolvedDotExpr *E) {
  // If we already diagnosed this as an error, don't complain again.
  if (E->getType() && E->getType()->is<ErrorType>())
    return nullptr;

  Expr *Base = E->getBase();
  Type BaseTy = Base->getType();
  Identifier MemberName = E->getName();
  
  if (BaseTy->isUnresolvedType()) {
    E->setType(UnstructuredUnresolvedType::get(Context));
    return E;
  }
  
  if (BaseTy->is<ErrorType>())
    return nullptr;  // Squelch an erroneous subexpression.

  // If the base expression is not an lvalue, make everything inside it
  // materializable.
  if (!BaseTy->is<LValueType>()) {
    Base = coerceToMaterializable(Base);
    if (!Base)
      return nullptr;

    BaseTy = Base->getType();
  }

  if (TupleType *TT = BaseTy->getRValueType()->getAs<TupleType>()) {
    // Try to look up the field by name; if that doesn't work, look for a
    // numbered field.
    int FieldNo = TT->getNamedElementId(MemberName);
    if (FieldNo == -1) {
      StringRef NameStr = MemberName.str();
      unsigned Value = 0;
      if (!NameStr.getAsInteger(10, Value) && Value < TT->getFields().size())
        FieldNo = Value;
    }
    if (FieldNo == -1) {
      // FIXME: This diagnostic is a bit painful.
      diagnose(E->getDotLoc(), diag::no_member_of_tuple, TT, MemberName)
        .highlight(Base->getSourceRange())
        .highlight(SourceRange(E->getNameLoc()));
      return 0;
    }

    Type FieldType = TT->getElementType(FieldNo);
    if (BaseTy->is<LValueType>())
      FieldType = makeSimilarLValue(FieldType, Base->getType(), Context);

    return new (Context) TupleElementExpr(Base, E->getDotLoc(), FieldNo,
                                          E->getNameLoc(), FieldType);
  }

  // Perform name lookup.
  MemberLookup Lookup(BaseTy, MemberName, TU);
  
  if (!Lookup.isSuccess()) {
    // Strip off any lvalue-ness for diagnostic purposes.
    BaseTy = BaseTy->getRValueType();
    
    if (ModuleType *MT = BaseTy->getAs<ModuleType>()) {
      diagnose(E->getDotLoc(), diag::no_member_of_module, MT->getModule()->Name,
               MemberName)
        .highlight(Base->getSourceRange())
        .highlight(SourceRange(E->getNameLoc()));
    } else if (MetaTypeType *MTT = BaseTy->getAs<MetaTypeType>()) {
      diagnose(E->getDotLoc(), diag::no_member_of_metatype,
               MTT->getInstanceType(), MemberName)
        .highlight(Base->getSourceRange())
        .highlight(SourceRange(E->getNameLoc()));
    } else if (BaseTy->is<ProtocolType>() ||
               BaseTy->is<ProtocolCompositionType>()) {
      diagnose(E->getDotLoc(), diag::no_member_of_protocol,
               BaseTy, MemberName)
        .highlight(Base->getSourceRange())
        .highlight(SourceRange(E->getNameLoc()));
      
    } else {
      // FIXME: This diagnostic is ridiculously painful.
      diagnose(E->getDotLoc(), diag::no_valid_dot_expression, BaseTy,
               MemberName)
        .highlight(Base->getSourceRange())
        .highlight(SourceRange(E->getNameLoc()));
    }
    return 0;
      
  }

  bool IsMetatypeBase = BaseTy->is<MetaTypeType>();
  if (IsMetatypeBase && Lookup.Results.size() == 1) {
    MemberLookupResult R = Lookup.Results[0];
    // If we're looking in a metatype and find a use of a field of the type,
    // diagnose it.
    if (R.Kind == MemberLookupResult::MemberProperty) {
      diagnose(E->getNameLoc(), diag::no_member_of_metatype,
               BaseTy->castTo<MetaTypeType>()->getInstanceType(), MemberName)
        .highlight(SourceRange(E->getNameLoc()));
      return 0;
    }
  }

  // If the base is a tuple, we need to force it to be either a proper lvalue
  // or a proper rvalue; otherwise, the semantics are strange.
  if (!Base->getType()->is<LValueType>())
    Base = coerceToMaterializable(Base);

  return recheckTypes(buildMemberRefExpr(Base, E->getDotLoc(), Lookup,
                                         E->getNameLoc()));
}


/// getInfixData - If the specified expression is an infix binary
/// operator, return its infix operator attributes.
static InfixData getInfixData(TypeChecker &TC, Expr *E) {
  assert(!isa<UnresolvedElseExpr>(E) &&
         "should fold ':' as part of ternary folding");
  if (isa<UnresolvedIfExpr>(E)) {
    // Ternary has fixed precedence.
    return InfixData(100, Associativity::Right);
  } else if (DeclRefExpr *DRE = dyn_cast<DeclRefExpr>(E)) {
    if (Optional<InfixOperatorDecl*> maybeOp
      = TC.TU.lookupInfixOperator(DRE->getDecl()->getName(), E->getLoc())) {
      if (auto *op = *maybeOp)
        return op->getInfixData();
      TC.diagnose(DRE->getLoc(), diag::unknown_binop);
    }
  } else if (OverloadedDeclRefExpr *OO = dyn_cast<OverloadedDeclRefExpr>(E)) {
    Identifier name = OO->getDecls()[0]->getName();
    if (Optional<InfixOperatorDecl*> maybeOp
        = TC.TU.lookupInfixOperator(name, E->getLoc())) {
      if (auto *op = *maybeOp)
        return op->getInfixData();
      TC.diagnose(OO->getLoc(), diag::unknown_binop);
    }
  // Otherwise, complain.
  } else {
    TC.diagnose(E->getLoc(), diag::unknown_binop);
  }
  
  // Recover with an infinite-precedence left-associative operator.
  return InfixData(~0U, Associativity::Left);
}

Expr *SemaExpressionTree::makeBinOp(Expr *Op, Expr *LHS, Expr *RHS,
                                    bool TypeCheckAST) {
  if (!LHS || !RHS)
    return nullptr;

  // Build the argument to the operation.
  Expr *ArgElts[] = { LHS, RHS };
  auto ArgElts2 = TC.Context.AllocateCopy(MutableArrayRef<Expr*>(ArgElts));
  TupleExpr *Arg = new (TC.Context) TupleExpr(SourceLoc(), 
                                              ArgElts2, 0, SourceLoc(),
                                              /*hasTrailingClosure=*/false);
  if (TypeCheckAST && TC.semaTupleExpr(Arg))
    return nullptr;

  // Build the operation.
  auto result = new (TC.Context) BinaryExpr(Op, Arg);
  if (!TypeCheckAST)
    return result;

  return visit(result);
}

/// foldSequence - Take a sequence of expressions and fold a prefix of
/// it into a tree of BinaryExprs using precedence parsing.
Expr *SemaExpressionTree::foldSequence(Expr *LHS,
                                       ArrayRef<Expr*> &S,
                                       unsigned MinPrecedence,
                                       bool TypeCheckAST) {
  // Invariant: S is even-sized.
  // Invariant: All elements at even indices are operator references.
  assert(!S.empty());
  assert((S.size() & 1) == 0);
  
  // An "operator" for our purposes can be either a binary op, or the MHS of a
  // ternary.
  struct Op {
    enum { Null, Binary, Ternary } kind;
    union {
      Expr *binary;
      struct {
        SourceLoc question;
        swift::Expr *mhs;
        SourceLoc colon;
      } ternary;
    };
    InfixData infixData;
    
    Op() : kind(Null) {}
    
    Op(Expr *binary, InfixData data)
      : kind(Binary), binary(binary), infixData(data) {}
    Op(SourceLoc question, swift::Expr *mhs, SourceLoc colon, InfixData data)
      : kind(Ternary), ternary{question, mhs, colon}, infixData(data) {}
    
    SourceLoc getLoc() const {
      switch (kind) {
      case Null:
        llvm_unreachable("null op");
      case Binary:
        return binary->getLoc();
      case Ternary:
        return ternary.question;
      }
    }
    
    explicit operator bool() const { return kind != Null; }
  };
  
  /// Get the next binary or ternary operator, if appropriate to this pass.
  auto getNextOperator = [&]() -> Op {
    Expr *op = S[0];
    // If this is a ternary ':', stop here.
    // The outer parse will match it to a '?'.
    if (isa<UnresolvedElseExpr>(op)) {
      return {};
    }
    
    // If the operator's precedence is lower than the minimum, stop here.
    InfixData opInfo = getInfixData(TC, op);
    if (opInfo.getPrecedence() < MinPrecedence) return {};
    // If this is a ternary '?', do a maximal-munch parse of the middle operand
    // up to the matching ':'.
    if (isa<UnresolvedIfExpr>(op)) {
      Expr *MHS = S[1];
      assert(S.size() >= 4 &&
             "SequenceExpr doesn't have enough elts to complete ternary");
      S = S.slice(2);
      MHS = foldSequence(MHS, S, 0, TypeCheckAST);
      assert(S.size() >= 2 &&
             "folding MHS of ternary did not leave enough elts to complete ternary");
      assert(isa<UnresolvedElseExpr>(S[0]) &&
             "folding MHS of ternary did not end at ':'");
      return Op(op->getLoc(), MHS, S[0]->getLoc(), opInfo);
    }
    return Op(op, opInfo);
  };
  
  /// Finalize an operator expression.
  auto makeOperatorExpr = [&](Expr *LHS, Op Operator, Expr *RHS) -> Expr* {
    switch (Operator.kind) {
    case Op::Null:
      llvm_unreachable("should have break'ed on null Op");
        
    case Op::Binary:
      return makeBinOp(Operator.binary, LHS, RHS, TypeCheckAST);
    
    case Op::Ternary:
      return new (TC.Context) IfExpr(LHS,
                                     Operator.ternary.question,
                                     Operator.ternary.mhs,
                                     Operator.ternary.colon,
                                     RHS);
    }
  };
  
  // Extract out the first operator.
  Op Op1 = getNextOperator();
  if (!Op1) return LHS;
  
  // We will definitely be consuming at least one operator.
  // Pull out the prospective RHS and slice off the first two elements.
  Expr *RHS = S[1];
  S = S.slice(2);

  while (!S.empty()) {
    assert(!S.empty());
    assert((S.size() & 1) == 0);
    assert(Op1.infixData.getPrecedence() >= MinPrecedence);

    // Pull out the next binary operator.
    Expr *Op2 = S[0];
    // If this is a ternary ':', break out of the loop.
    if (isa<UnresolvedElseExpr>(Op2)) break;
  
    InfixData Op2Info = getInfixData(TC, Op2);
    // If the second operator's precedence is lower than the min
    // precedence, break out of the loop.
    if (Op2Info.getPrecedence() < MinPrecedence) break;
    
    // If the first operator's precedence is higher than the second
    // operator's precedence, or they have matching precedence and are
    // both left-associative, fold LHS and RHS immediately.
    if (Op1.infixData.getPrecedence() > Op2Info.getPrecedence() ||
        (Op1.infixData == Op2Info && Op1.infixData.isLeftAssociative())) {
      LHS = makeOperatorExpr(LHS, Op1, RHS);
      Op1 = getNextOperator();
      assert(Op1 && "should get a valid operator here");
      RHS = S[1];
      S = S.slice(2);
      continue;
    }

    // If the first operator's precedence is lower than the second
    // operator's precedence, recursively fold all such
    // higher-precedence operators starting from this point, then
    // repeat.
    if (Op1.infixData.getPrecedence() < Op2Info.getPrecedence()) {
      RHS = foldSequence(RHS, S, Op1.infixData.getPrecedence() + 1,
                         TypeCheckAST);
      continue;
    }

    // If the first operator's precedence is the same as the second
    // operator's precedence, and they're both right-associative,
    // recursively fold operators starting from this point, then
    // immediately fold LHS and RHS.
    if (Op1.infixData == Op2Info && Op1.infixData.isRightAssociative()) {
      RHS = foldSequence(RHS, S, Op1.infixData.getPrecedence(),
                         TypeCheckAST);
      LHS = makeOperatorExpr(LHS, Op1, RHS);

      // If we've drained the entire sequence, we're done.
      if (S.empty()) return LHS;

      // Otherwise, start all over with our new LHS.
      return foldSequence(LHS, S, MinPrecedence, TypeCheckAST);
    }

    // If we ended up here, it's because we have two operators
    // with mismatched or no associativity.
    assert(Op1.infixData.getPrecedence() == Op2Info.getPrecedence());
    assert(Op1.infixData.getAssociativity() != Op2Info.getAssociativity()
           || Op1.infixData.isNonAssociative());

    if (Op1.infixData.isNonAssociative()) {
      // FIXME: QoI ranges
      TC.diagnose(Op1.getLoc(), diag::non_assoc_adjacent);
    } else if (Op2Info.isNonAssociative()) {
      TC.diagnose(Op2->getLoc(), diag::non_assoc_adjacent);
    } else {
      TC.diagnose(Op1.getLoc(), diag::incompatible_assoc);
    }
    
    // Recover by arbitrarily binding the first two.
    LHS = makeOperatorExpr(LHS, Op1, RHS);
    return foldSequence(LHS, S, MinPrecedence, TypeCheckAST);
  }

  // Fold LHS (and MHS, if ternary) and RHS together and declare completion.

  return makeOperatorExpr(LHS, Op1, RHS);
}

/// foldSequence - Take a SequenceExpr and fold it into a tree of
/// BinaryExprs using precedence parsing.
Expr *SemaExpressionTree::visitSequenceExpr(SequenceExpr *E,
                                            bool TypeCheckAST) {
  ArrayRef<Expr*> Elts = E->getElements();
  assert(Elts.size() > 1 && "inadequate number of elements in sequence");
  assert((Elts.size() & 1) == 1 && "even number of elements in sequence");

  Expr *LHS = Elts[0];
  Elts = Elts.slice(1);

  Expr *Result = foldSequence(LHS, Elts, /*min precedence*/ 0,
                              TypeCheckAST);
  assert(Elts.empty());
  return Result;
}

Expr *TypeChecker::recheckTypes(Expr *E) {
  if (!E)
    return nullptr;
  
  return SemaExpressionTree(*this).visit(E);
}

namespace {
  // Check the initializer/body to make sure that we succeeded in resolving
  // all of the types contained within it.  If we've resolved everything, then
  // we're done processing the expression.  While we're doing the walk, keep
  // track of whether we have any literals without a resolved type.
  struct DependenceWalker : ASTWalker {
    DependenceWalker() { reset(); }
    
    void reset() {
      OneUnresolvedExpr = nullptr;
      HasUnresolvedLiterals = false;
    }
    
    Expr *walkToExprPost(Expr *E) {    
      assert(!isa<SequenceExpr>(E) && "Should have resolved this");
      
      if (E->getType()->isUnresolvedType()) {
        // Remember the first unresolved expression we come across.
        if (OneUnresolvedExpr == 0)
          OneUnresolvedExpr = E;
        
        // Also remember if we see any literals with unresolved types.
        if (isa<LiteralExpr>(E))
          HasUnresolvedLiterals = true;
      
        // func{} defaults to return () if we can't infer a result type.
        if (isa<FuncExpr>(E))
          HasUnresolvedLiterals = true;
      }
      return E;
    }
    
    std::pair<bool, Stmt *> walkToStmtPre(Stmt *S) override {
      // Never recurse into statements.
      return { false, S };
    }
    
    Expr *OneUnresolvedExpr;
    bool HasUnresolvedLiterals;
  }; 
}

static Type lookupGlobalType(TypeChecker &TC, StringRef name) {
  UnqualifiedLookup lookup(TC.Context.getIdentifier(name), &TC.TU);
  TypeDecl *TD = lookup.getSingleTypeResult();
  if (!TD)
    return Type();
  return TD->getDeclaredType();
}

/// \brief Retrieve the default literal type for the given literal kind.
Type TypeChecker::getDefaultLiteralType(LiteralKind kind) {
  Type *type;
  const char *name;
  switch (kind) {
  case LiteralKind::Char:
    type = &CharacterLiteralType;
    name = "CharacterLiteralType";
    break;

  case LiteralKind::ASCIIString:
  case LiteralKind::UTFString:
    type = &StringLiteralType;
    name = "StringLiteralType";
      break;

  case LiteralKind::Float:
    type = &FloatLiteralType;
    name = "FloatLiteralType";
    break;

  case LiteralKind::Int:
    type = &IntLiteralType;
    name = "IntegerLiteralType";
    break;

  case LiteralKind::Array:
    type = &ArrayLiteralType;
    name = "Slice";
    break;

  case LiteralKind::Dictionary:
    type = &DictionaryLiteralType;
    name = "Dictionary";
    break;
  }

  // If we haven't found the type yet, look for it now.
  if (!*type) {
    *type = lookupGlobalType(*this, name);

    // Strip off one level of sugar; we don't actually want to print
    // IntegerLiteralType anywhere.
    if (type && *type) {
      if (auto typeAlias = dyn_cast<NameAliasType>(type->getPointer()))
        *type = typeAlias->getDecl()->getUnderlyingType();
    }
  }

  return *type;
}

Type TypeChecker::getDefaultLiteralType(LiteralExpr *E) {
  if (isa<IntegerLiteralExpr>(E)) {
    if (!IntLiteralType) {
      IntLiteralType = getDefaultLiteralType(LiteralKind::Int);
      if (!IntLiteralType) {
        diagnose(E->getLoc(), diag::no_IntegerLiteralType_found);
        IntLiteralType = BuiltinIntegerType::get(32, Context);
      }
    }
    return IntLiteralType;
  }
  
  if (isa<FloatLiteralExpr>(E)) {
    if (!FloatLiteralType) {
      FloatLiteralType = getDefaultLiteralType(LiteralKind::Float);
      if (!FloatLiteralType) {
        diagnose(E->getLoc(), diag::no_FloatLiteralType_found);
        FloatLiteralType = Context.TheIEEE64Type;
      }
    }
    return FloatLiteralType;
  }
  
  if (isa<CharacterLiteralExpr>(E)) {
    if (!CharacterLiteralType) {
      CharacterLiteralType = getDefaultLiteralType(LiteralKind::Char);
      if (!CharacterLiteralType) {
        diagnose(E->getLoc(), diag::no_CharacterLiteralType_found);
        CharacterLiteralType = BuiltinIntegerType::get(32, Context);
      }
    }
    return CharacterLiteralType;
  }
  
  assert((isa<StringLiteralExpr>(E) || isa<InterpolatedStringLiteralExpr>(E)) &&
         "Unknown literal type");
  if (!StringLiteralType) {
    StringLiteralType = getDefaultLiteralType(LiteralKind::UTFString);
    if (!StringLiteralType) {
      diagnose(E->getLoc(), diag::no_StringLiteralType_found);
      StringLiteralType = Context.TheRawPointerType;
    }
  }
  return StringLiteralType;
}

bool TypeChecker::resolveUnresolvedLiterals(Expr *&E) {
  struct UpdateWalker : ASTWalker {
    UpdateWalker(TypeChecker &TC) : TC(TC) {}
    
    Expr *walkToExprPost(Expr *E) {
      // Process unresolved literals.
      if (!E->getType()->isUnresolvedType())
        return E;
      
      if (LiteralExpr *Lit = dyn_cast<LiteralExpr>(E))
        return TC.coerceToType(Lit, TC.getDefaultLiteralType(Lit));
        
      // func{} defaults to return () if we can't infer a result type.
      if (auto FE = dyn_cast<FuncExpr>(E)) {
        TC.semaFuncExpr(FE, false, false);
        return FE;
      }
      return E;
    }
    
    std::pair<bool, Stmt *> walkToStmtPre(Stmt *S) override {
      // Never recurse into statements.
      return { false, S };
    }
    
  private:    
    TypeChecker &TC;
    Type IntLiteralType, FloatLiteralType, CharacterLiteralType;
    Type StringLiteralType;
  };
  
  // Walk the tree again to update all the entries.  If this fails, give up.
  E = E->walk(UpdateWalker(*this));
  if (!E)
    return true;
  
  // Now that we've added some types to the mix, re-type-check the expression
  // tree and recheck for unresolved types.
  SemaExpressionTree SET(*this);
  E = SET.doIt(E);
  return E == nullptr;
}

Expr *TypeChecker::foldSequence(SequenceExpr *expr) {
  return SemaExpressionTree(*this).visitSequenceExpr(expr,
                                                     /*TypeCheckAST=*/false);
}

static bool semaFuncParamPatterns(TypeChecker *checker,
                                  ArrayRef<Pattern*> paramPatterns,
                                  bool isFirstPass,
                                  bool allowUnknownTypes) {
  bool badType = false;
  for (Pattern *P : paramPatterns) {
    if (P->hasType())
      continue;
    if (checker->typeCheckPattern(P, isFirstPass, allowUnknownTypes)) {
      badType = true;
      continue;
    }
  }
  return badType;
}

void TypeChecker::semaFuncExpr(FuncExpr *FE, bool isFirstPass,
                               bool allowUnknownTypes) {
  if (FE->getType() && !FE->getType()->isUnresolvedType())
    return;

  bool badType = false;
  if (FE->getBodyResultTypeLoc().getType()) {
    if (validateType(FE->getBodyResultTypeLoc())) {
      FE->getBodyResultTypeLoc().setInvalidType(Context);
      badType = true;
    }
  }
  
  badType = badType || semaFuncParamPatterns(this, FE->getArgParamPatterns(),
                                             isFirstPass, allowUnknownTypes);
  badType = badType || semaFuncParamPatterns(this, FE->getBodyParamPatterns(),
                                             isFirstPass, allowUnknownTypes);

  if (badType) {
    FE->setType(ErrorType::get(Context));
    return;
  }

  Type funcTy = FE->getBodyResultTypeLoc().getType();
  if (!funcTy) {
    if (allowUnknownTypes)
      funcTy = UnstructuredUnresolvedType::get(Context);
    else
      funcTy = TupleType::getEmpty(Context);
  }

  // FIXME: it would be nice to have comments explaining what this is all about.
  auto patterns = FE->getArgParamPatterns();
  bool isInstanceFunc = false;
  GenericParamList *genericParams = nullptr;
  GenericParamList *outerGenericParams = nullptr;
  if (FuncDecl *FD = FE->getDecl()) {
    isInstanceFunc = (bool)FD->computeThisType(&outerGenericParams);
    genericParams = FD->getGenericParams();
  }

  for (unsigned i = 0, e = patterns.size(); i != e; ++i) {
    Type argTy = patterns[e - i - 1]->getType();
    if (e - i - 1 == isInstanceFunc && genericParams) {
      funcTy = PolymorphicFunctionType::get(argTy, funcTy,
                                            genericParams,
                                            Context);
    } else if (e - i - 1 == 0 && outerGenericParams) {
      funcTy = PolymorphicFunctionType::get(argTy, funcTy,
                                            outerGenericParams,
                                            Context);
    } else {
      funcTy = FunctionType::get(argTy, funcTy, Context);
    }
  }
  FE->setType(funcTy);
}

namespace {
  class FindCapturedVars : public ASTWalker {
    TypeChecker &tc;
    llvm::SetVector<ValueDecl*> &captures;
    CapturingExpr *curExpr;

  public:
    std::pair<bool, Expr *> walkToExprPre(Expr *E) override {
      if (DeclRefExpr *DRE = dyn_cast<DeclRefExpr>(E)) {
        if (DRE->getDecl()->getDeclContext()->isLocalContext() &&
            DRE->getDecl()->getDeclContext() != curExpr) {
          // A [byref] parameter cannot be captured.
          // FIXME: As a temporary hack, ignore 'this', which is an implicit
          // [byref] parameter for instance methods of structs.
          if (DRE->getDecl()->getType()->is<LValueType>() && !
              DRE->getDecl()->getName().str().equals("this")) {
            tc.diagnose(DRE->getLoc(), diag::byref_capture,
                        DRE->getDecl()->getName());
          }
          captures.insert(DRE->getDecl());
        }
        return { false, E };
      }
      if (CapturingExpr *SubCE = dyn_cast<CapturingExpr>(E)) {
        for (auto D : SubCE->getCaptures())
          if (D->getDeclContext() != curExpr)
            captures.insert(D);
        return { false, E };
      }
      return { true, E };
    }

    FindCapturedVars(TypeChecker &tc,
                     llvm::SetVector<ValueDecl*> &captures,
                     CapturingExpr *curExpr)
      : tc(tc), captures(captures), curExpr(curExpr) {}

    void doWalk(Expr *E) {
      E->walk(*this);
    }
    void doWalk(Stmt *S) {
      S->walk(*this);
    }
  };
}

void TypeChecker::computeCaptures(CapturingExpr *capturing) {
  llvm::SetVector<ValueDecl*> Captures;
  FindCapturedVars finder(*this, Captures, capturing);
  if (auto closure = dyn_cast<ClosureExpr>(capturing))
    finder.doWalk(closure->getBody());
  else if (auto closure = dyn_cast<PipeClosureExpr>(capturing))
    finder.doWalk(closure->getBody());
  else {
    auto func = cast<FuncExpr>(capturing);
    if (auto body = func->getBody()) {
      finder.doWalk(body);
    }
  }
  ValueDecl** CaptureCopy
    = Context.AllocateCopy<ValueDecl*>(Captures.begin(), Captures.end());
  capturing->setCaptures(llvm::makeArrayRef(CaptureCopy, Captures.size()));
}

