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
#include "llvm/ADT/SmallPtrSet.h"
using namespace swift;

//===----------------------------------------------------------------------===//
// Expression Semantic Analysis Routines
//===----------------------------------------------------------------------===//

Expr *TypeChecker::convertToRValue(Expr *E) {
  assert(E && "no expression to load!");

  if (LValueType *lv = E->getType()->getAs<LValueType>())
    return convertLValueToRValue(lv, E);

  return E;
}

/// Perform in-place adjustments on expressions which might be
/// carrying un-materializable types internally.
static void convertToMaterializableHelper(TypeChecker &TC, Expr *E) {
  if (ParenExpr *PE = dyn_cast<ParenExpr>(E)) {
    convertToMaterializableHelper(TC, PE->getSubExpr());
    PE->setType(PE->getSubExpr()->getType());
  } else if (TupleExpr *TE = dyn_cast<TupleExpr>(E)) {
    bool anyChange = false;
    for (Expr *&eltRef : TE->getElements()) {
      Type oldType = eltRef->getType();
      Expr *newElt = TC.convertToMaterializable(eltRef);
      if (!newElt) return;

      // Remember if the type changed at all.  A superficial test is fine.
      if (newElt->getType().getPointer() != oldType.getPointer()) {
        eltRef = newElt;
        anyChange = true;
      }
    }

    // If we did anything, recreate the type.
    if (anyChange) TC.semaTupleExpr(TE);
  }

  // For now, those are the only expression kinds which can carry
  // internal l-values that affect the type.
}

/// Make the given expression have a materializable type if it doesn't
/// already.
Expr *TypeChecker::convertToMaterializable(Expr *E) {
  // Load l-values.
  if (LValueType *lv = E->getType()->getAs<LValueType>())
    return convertLValueToRValue(lv, E);

  // Recursively walk into tuples and parens, performing loads.
  convertToMaterializableHelper(*this, E);
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
  // Propagate errors up.
  if (SE->getBase()->getType()->is<ErrorType>() ||
      SE->getIndex()->getType()->is<ErrorType>()) {
    SE->setType(ErrorType::get(Context));
    return SE;
  }
  
  // If we already know what subscript declaration to use, do it now.
  if (SE->hasDecl()) {
    // Determine the type of the container.
    Type ContainerTy
      = SE->getDecl()->getDeclContext()->getDeclaredTypeOfContext();
    if (!ContainerTy)
      return nullptr;
    
    assert(!ContainerTy->isExistentialType() &&
           "Wrong expression kind for subscript into existential type");
    
    // Ensure that the base is an lvalue, materializing it if is not an
    // lvalue yet.
    if (Expr *Base = coerceObjectArgument(SE->getBase(), ContainerTy))
      SE->setBase(Base);
    else
      return nullptr;

    // Convert the indices appropriately.
    SubscriptDecl *SubDecl = SE->getDecl();
    Type IndexType = SubDecl->getIndices()->getType();
    Expr *Index = coerceToType(SE->getIndex(), IndexType);
    if (!Index) {
      diagnose(SE->getBase()->getLoc(), diag::while_converting_subscript_index,
               IndexType)
        << SE->getIndex()->getSourceRange();
      SE->setType(ErrorType::get(Context));
      return 0;
    }
    SE->setIndex(Index);

    // Compute the final lvalue type and we're done.
    SE->setType(LValueType::get(SubDecl->getElementType(),
                                LValueType::Qual::DefaultForMemberAccess,
                                Context));
    return SE;
  }
  
  // Determine the type of the base of this subscript expression.
  Type BaseTy = SE->getBase()->getType()->getRValueType();

  // Look for subscript operators in the base type.
  // FIXME: hard-coding of the name __subscript is ueber-lame.
  MemberLookup Lookup(BaseTy, Context.getIdentifier("__subscript"), TU);
  
  if (!Lookup.isSuccess()) {
    diagnose(SE->getLBracketLoc(), diag::no_subscript_declaration, BaseTy)
      << SE->getBase()->getSourceRange();
    SE->setType(ErrorType::get(Context));
    return SE;
  }
  
  llvm::SmallVector<ValueDecl *, 4> LookupResults;
  for (const auto &R : Lookup.Results)
    LookupResults.push_back(R.D);
  llvm::SmallVector<ValueDecl *, 4> Viable;
  ValueDecl *Best = filterOverloadSet(LookupResults,
                                      /*OperatorSyntax=*/true,
                                      BaseTy, SE->getIndex(),
                                      Type(), Viable);

  if (Best) {
    SubscriptDecl *Sub = cast<SubscriptDecl>(Best);
    if (BaseTy->isExistentialType()) {
      // We picked a subscript operator in an existential type; create the
      // appropriate AST node.
      return semaSubscriptExpr(
               new (Context) ExistentialSubscriptExpr(SE->getBase(),
                                                      SE->getLBracketLoc(),
                                                      SE->getIndex(),
                                                      SE->getRBracketLoc(),
                                                      Sub));
    }
    if (BaseTy->is<ArchetypeType>()) {
      // We picked a subscript operator in an archetype type; create the
      // appropriate AST node.
      return semaSubscriptExpr(
               new (Context) ArchetypeSubscriptExpr(SE->getBase(),
                                                    SE->getLBracketLoc(),
                                                    SE->getIndex(),
                                                    SE->getRBracketLoc(),
                                                    Sub));
    }

    // Simple case: perform semantic analysis now that we have the declaration.
    SE->setDecl(Sub);
    return semaSubscriptExpr(SE);
  }
  
  // If there were no viable candidates, complain.
  if (Viable.empty()) {
    diagnose(SE->getLBracketLoc(), diag::subscript_overload_fail,
             false, BaseTy, SE->getIndex()->getType())
      << SE->getBase()->getSourceRange() << SE->getIndex()->getSourceRange();
    printOverloadSetCandidates(Viable.empty()? LookupResults : Viable);
    SE->setType(ErrorType::get(Context));
    return SE;
  }
  
  // Create an overloaded subscript expression; type coercion may be able to
  // resolve it.
  return OverloadedSubscriptExpr::createWithCopy(SE->getBase(), Viable,
                                                 SE->getLBracketLoc(),
                                                 SE->getIndex(),
                                                 SE->getRBracketLoc());
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
        Substitutions[Archetype] = Archetype->getConformsTo().front();
      else
        Substitutions[Archetype]
          = ProtocolCompositionType::get(Context, SourceLoc(),
                                         Archetype->getConformsTo());
    }
  }
}

Expr *TypeChecker::semaSubscriptExpr(ExistentialSubscriptExpr *E) {
  // Propagate errors up.
  if (E->getDecl()->getType()->is<ErrorType>()) {
    E->setType(ErrorType::get(Context));
    return nullptr;
  }

  // Ensure that the base is an lvalue, materializing it if is not an
  // lvalue yet.
  Type ContainerTy
    = E->getDecl()->getDeclContext()->getDeclaredTypeOfContext();
  if (Expr *Base = coerceObjectArgument(E->getBase(), ContainerTy))
    E->setBase(Base);
  else {
    E->setType(ErrorType::get(Context));
    return nullptr;
  }

  // For each of the archetypes in the protocol, substitute an existential
  // type that meets the same requirements.
  ProtocolDecl *Proto = ContainerTy->castTo<ProtocolType>()->getDecl();
  TypeSubstitutionMap Substitutions;
  collectArchetypeToExistentialSubstitutions(Context, Proto, ContainerTy,
                                             Substitutions);
  SubscriptDecl *SubDecl = E->getDecl();

  // Determine the index type.
  Type IndexType = SubDecl->getIndices()->getType();
  Type SubstIndexType = substType(IndexType, Substitutions);
  if (!SubstIndexType)
    return nullptr;

  // FIXME: For now, we don't allow any associated types to show up. We
  // may relax this restriction later.
  if (!SubstIndexType->isEqual(IndexType)) {
    diagnose(E->getLBracketLoc(), diag::existential_subscript_assoc_types,
             Proto->getDeclaredType(), true, IndexType);
    diagnose(E->getDecl()->getLoc(), diag::subscript_decl_here);
    
    IndexType = SubstIndexType;
  }

  // Coerce the index argument to the index type.
  Expr *Index = coerceToType(E->getIndex(), IndexType);
  if (!Index) {
    diagnose(E->getBase()->getLoc(), diag::while_converting_subscript_index,
             IndexType)
      << E->getIndex()->getSourceRange();
    E->setType(ErrorType::get(Context));
    return nullptr;
  }
  E->setIndex(Index);

  // Determine the type of the member.
  Type ValueType = SubDecl->getElementType();

  // Substitute the existential types into the member type.
  Type SubstValueType = substType(ValueType, Substitutions);
  if (!SubstValueType) {
    E->setType(ErrorType::get(Context));
    return nullptr;
  }
  
  // FIXME: For now, we don't allow any associated types to show up. We
  // may relax this restriction later.
  if (!SubstValueType->isEqual(ValueType)) {
    diagnose(E->getLBracketLoc(), diag::existential_subscript_assoc_types,
             Proto->getDeclaredType(), false, ValueType);
    diagnose(E->getDecl()->getLoc(), diag::subscript_decl_here);
    ValueType = SubstValueType;
  }
  
  ValueType = LValueType::get(ValueType,
                              LValueType::Qual::DefaultForMemberAccess,
                              Context);
  E->setType(ValueType);
  return E;

}

Expr *TypeChecker::semaSubscriptExpr(ArchetypeSubscriptExpr *E) {
  // Propagate errors up.
  if (E->getDecl()->getType()->is<ErrorType>()) {
    E->setType(ErrorType::get(Context));
    return nullptr;
  }

  // Ensure that the base is an lvalue, materializing it if is not an
  // lvalue yet.
  Type ContainerTy = E->getBase()->getType()->getRValueType();
  
  if (Expr *Base = coerceObjectArgument(E->getBase(), ContainerTy))
    E->setBase(Base);
  else {
    E->setType(ErrorType::get(Context));
    return nullptr;
  }

  SubscriptDecl *SubDecl = E->getDecl();
  
  // Determine the index type.
  Type IndexType = SubDecl->getIndices()->getType();
  IndexType = substMemberTypeWithBase(IndexType, ContainerTy);
  if (!IndexType)
    return nullptr;
  
  // Coerce the index argument to the index type.
  Expr *Index = coerceToType(E->getIndex(), IndexType);
  if (!Index) {
    diagnose(E->getBase()->getLoc(), diag::while_converting_subscript_index,
             IndexType)
      << E->getIndex()->getSourceRange();
    E->setType(ErrorType::get(Context));
    return nullptr;
  }
  E->setIndex(Index);
  
  // Determine the value type.
  Type ValueType = SubDecl->getElementType();
  ValueType = substMemberTypeWithBase(ValueType, ContainerTy);
  if (!ValueType) {
    E->setType(ErrorType::get(Context));
    return nullptr;
  }
  
  ValueType = LValueType::get(ValueType,
                              LValueType::Qual::DefaultForMemberAccess,
                              Context);
  E->setType(ValueType);
  return E;
  
}

/// \brief Determine whether this expression refers to a type directly (ignoring
/// parentheses), rather than some variable of metatype type.
static bool isDirectTypeReference(Expr *E) {
  if (DeclRefExpr *DRE = dyn_cast<DeclRefExpr>(E->getSemanticsProvidingExpr()))
    return isa<TypeDecl>(DRE->getDecl());
  
  return false;
}

Expr *TypeChecker::semaApplyExpr(ApplyExpr *E) {
  Expr *E1 = E->getFn();
  Expr *E2 = E->getArg();

  // If the callee was erroneous, silently propagate the error up.
  if (E1->getType()->is<ErrorType>()) {
    E->setType(E1->getType());
    return E;
  }

  // Perform lvalue-to-rvalue conversion on the function.
  E1 = convertToRValue(E1);
  if (!E1) return nullptr;
  
  // If we have a concrete function type, then we win.
  if (FunctionType *FT = E1->getType()->getAs<FunctionType>()) {
    // If this is an operator, make sure that the declaration found was declared
    // as such.
    bool Assignment = false;
    if (isa<PrefixUnaryExpr>(E) || isa<PostfixUnaryExpr>(E) ||
        isa<BinaryExpr>(E)) {
      auto DRE = cast<DeclRefExpr>(E1);
      
      if (!DRE->getDecl()->isOperator()) {
        diagnose(E1->getLoc(),
                 !isa<BinaryExpr>(E) ? diag::unary_op_without_attribute
                                     : diag::binary_op_without_attribute);
        return nullptr;
      }
      
      Assignment = DRE->getDecl()->getAttrs().isAssignment();
    }
    
    // We have a function application.  Check that the argument type matches the
    // expected type of the function.
    // If this is, syntactically, an expression x.f where 'f' is a method in
    // the type of 'x', we're coercing the object argument ('this').
    if (isa<ThisApplyExpr>(E))
      E2 = coerceObjectArgument(E2, FT->getInput());
    else {
      CoercedExpr CoercedE2 = coerceToType(E2, FT->getInput(), Assignment);
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
        << E->getArg()->getSourceRange();
      return 0;
    }

    E->setFn(E1);
    E->setArg(E2);
    E->setType(FT->getResult());
    return E;
  }
  
  // If the "function" is actually a type (i.e. its type is 'metatype'), then we
  // have an argument list applied to a type, which is construction of the
  // type.
  if (MetaTypeType *MT = E1->getType()->getAs<MetaTypeType>()) {
    // The metatype represents an arbitrary named type: dig through the
    // TypeAlias to see what we're dealing with.  If the typealias was erroneous
    // then silently squish this erroneous subexpression.
    Type Ty = MT->getTypeDecl()->getDeclaredType();
    E->setType(Ty);
    if (Ty->is<ErrorType>())
      return 0;  // Squelch an erroneous subexpression.

    // If the 'function' is a direct reference to a type, and the type isn't
    // a struct or oneof, this is a cast.  If the type is a struct or oneof,
    // we first try to cast, then try to construct an object.
    bool IsCast = true;
    // FIXME: Should this check for coercions to any nominal type?
    if (Ty->is<StructType>() || Ty->is<OneOfType>())
      IsCast = isCoercibleToType(E2, Ty) != CoercionResult::Failed;
    if (IsCast) {
      if (!isDirectTypeReference(E1)) {
        // FIXME: This should be an error, and we should check it on
        // both codepaths.
        E1 = new (Context) DeclRefExpr(MT->getTypeDecl(), E1->getStartLoc(),
                                       MT->getTypeDecl()->getTypeOfReference());
      }

      CoercedExpr CoercedArg = coerceToType(E2, Ty);
      switch (CoercedArg.getKind()) {
      case CoercionResult::Succeeded:
        return new (Context) CoerceExpr(E1, CoercedArg);

      case CoercionResult::Failed:
        E->setType(ErrorType::get(Context));
        return nullptr;

      case CoercionResult::Unknowable:
        // Delay type checking. However, we do know the type of this expression.
        E->setType(Ty);
        return E;
      }
    }

    // Check for a struct or oneof with a constructor.
    SmallVector<ValueDecl *, 4> Viable;
    ConstructorLookup Ctors(Ty, TU);
    if (Ctors.isSuccess()) {
      ValueDecl *Best = filterOverloadSet(Ctors.Results, false,
                                          Ty, E2, Type(), Viable);
      ConstructorDecl *CD = dyn_cast_or_null<ConstructorDecl>(Best);
      if (CD) {
        Type ArgTy = CD->getArgumentType();
        CoercedExpr CoercedE2 = coerceToType(E2, ArgTy);
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
        assert(E2 && "Coercion shouldn't fail!");
        return new (Context) ConstructExpr(Ty, E1->getStartLoc(), CD, E2);
      }
      if (Best) {
        E1 = OverloadedDeclRefExpr::createWithCopy(Ctors.Results,
                                                   E1->getStartLoc());
        E->setFn(E1);
        return semaApplyExpr(E);
      }    
    }
    if (!E2->getType()->isUnresolvedType()) {
      diagnose(E->getLoc(), diag::constructor_overload_fail, !Viable.empty(), Ty)
        << E->getSourceRange();
      printOverloadSetCandidates(Viable);
      return 0;
    }
    return E;
  }
  
  // Otherwise, the function's type must be unresolved.  If it is something
  // else, we have a type error.
  if (!E1->getType()->isUnresolvedType()) {
    diagnose(E1->getLoc(), diag::called_expr_isnt_function);
    return 0;
  }
  
  // Otherwise, we must have an application to an overload set.  See if we can
  // resolve which overload member is based on the argument type.
  OverloadSetRefExpr *OS = dyn_cast<OverloadSetRefExpr>(E1);
  if (!OS) {
    // If not, just use the unresolved type.
    E->setType(UnstructuredUnresolvedType::get(Context));
    return E;
  }

  // Perform overload resolution.
  llvm::SmallVector<ValueDecl *, 4> Viable;
  ValueDecl *Best = filterOverloadSet(OS->getDecls(),
                                      (isa<PrefixUnaryExpr>(E) ||
                                       isa<PostfixUnaryExpr>(E) ||
                                       isa<BinaryExpr>(E)),
                                      OS->getBaseType(), E2,
                                      Type(), Viable);
  
  // If we have a best candidate, build a call to it now.
  if (Best) {
    E1 = buildFilteredOverloadSet(OS, Best);
    E->setFn(convertToRValue(E1));
    return semaApplyExpr(E);
  }
  
  // If there are no more viable candidates, complain now.
  if (Viable.empty()) {
    diagnoseEmptyOverloadSet(E, OS->getDecls());
    return nullptr;
  }
  
  // We have more than one viable candidate. This might be resolved when/if
  // we get a destination type to coerce to.  
  E->setType(UnstructuredUnresolvedType::get(Context));
  if (Viable.size() == OS->getDecls().size())
    return E;
  
  // We have trimmed the overload set; rebuild the overload set so that we
  // no longer have to consider those non-matching candidates.
  // FIXME: We may simple want to mark them as non-viable in the overload set
  // itself, so we can provide them in diagnostics.
  E->setFn(buildFilteredOverloadSet(OS, Viable));
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
      E->setType(UnstructuredUnresolvedType::get(TC.Context));
      return E;
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
    if (auto Method = dyn_cast<FuncDecl>(E->getDecl())) {
      if (!Method->isStatic()) {
        if (auto FuncTy = dyn_cast<FunctionType>(MemberTy))
          MemberTy = FuncTy->getResult();
      }
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
    if (!SubstMemberTy->isEqual(MemberTy)) {
      TC.diagnose(E->getDotLoc(), diag::existential_member_assoc_types,
                  E->getDecl()->getName(), Proto->getDeclaredType(),
                  MemberTy);
      TC.diagnose(E->getDecl()->getLoc(), diag::decl_declared_here,
                  E->getDecl()->getName());
    }
    
    E->setType(SubstMemberTy);
    return E;
  }

  Expr *visitArchetypeMemberRefExpr(ArchetypeMemberRefExpr *E) {
    if (E->getDecl()->getType()->is<ErrorType>())
      return nullptr;

    Type Archetype = E->getArchetype();

    // If we're going to use the base, make sure it's an lvalue, materializing
    // it if needed.
    if (!E->isBaseIgnored()) {
      // Ensure that the base is an lvalue, materializing it if is not an
      // lvalue yet.
      if (Expr *Base = TC.coerceObjectArgument(E->getBase(), Archetype))
        E->setBase(Base);
      else
        return nullptr;
    }
    
    // Determine the type of the member.
    Type MemberTy = E->getDecl()->getType();
    if (auto Method = dyn_cast<FuncDecl>(E->getDecl())) {
      if (!Method->isStatic()) {
        if (auto FuncTy = dyn_cast<FunctionType>(MemberTy))
          MemberTy = FuncTy->getResult();
      }
    } else {
      MemberTy = LValueType::get(MemberTy,
                                 LValueType::Qual::DefaultForMemberAccess,
                                 TC.Context);
    }

    // Substitute each of the associated types into the member type.
    MemberTy = TC.substMemberTypeWithBase(MemberTy, Archetype);
    if (!MemberTy)
      return nullptr;

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

  Expr *visitSubscriptExpr(SubscriptExpr *E) {
    return TC.semaSubscriptExpr(E);
  }
  Expr *visitExistentialSubscriptExpr(ExistentialSubscriptExpr *E) {
    return TC.semaSubscriptExpr(E);
  }
  Expr *visitArchetypeSubscriptExpr(ArchetypeSubscriptExpr *E) {
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
    if (TC.validateType(E->getElementType(), /*isFirstPass*/false)) {
      E->setElementType(ErrorType::get(TC.Context));
      return nullptr;
    }

    Type resultType = E->getElementType();
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

      // Otherwise, build the approproiate constant array type.

      // Force the array type to be constant.
      if (TC.typeCheckArrayBound(bound.Value, /*requireConstant*/ true))
        return nullptr;

      // Compute the bound, and require a non-zero value.
      IntegerLiteralExpr *lit = cast<IntegerLiteralExpr>(bound.Value);
      uint64_t size = lit->getValue().getZExtValue();
      if (size == 0) {
        TC.diagnose(lit->getLoc(), diag::new_array_bound_zero)
          << lit->getSourceRange();
        return nullptr;
      }

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
    ArraySliceType *sliceType = cast<ArraySliceType>(resultType);

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

  Expr *visitNewReferenceExpr(NewReferenceExpr *E) {
    if (TC.validateType(E->getType(), /*isFirstPass*/false)) {
      E->setType(ErrorType::get(TC.Context));
      return nullptr;
    }

    ClassType *CT = E->getType()->getAs<ClassType>();
    if (!CT) {
      TC.diagnose(E->getLoc(), diag::new_reference_not_class);
      return nullptr;
    }

    // We don't have an explicit argument; fake one up.
    Expr *Arg = new (TC.Context) TupleExpr(E->getLoc(),
                                           MutableArrayRef<Expr *>(),
                                           nullptr, E->getLoc());
    Arg->setType(TupleType::getEmpty(TC.Context));

    ConstructorLookup Ctors(CT, TC.TU);
    llvm::SmallVector<ValueDecl *, 4> Viable;
    ValueDecl *Best = TC.filterOverloadSet(Ctors.Results, false,
                                           CT, Arg, Type(), Viable);

    if (Best) {
      ConstructorDecl *CD = cast<ConstructorDecl>(Best);
      Arg = TC.coerceToType(Arg, CD->getArgumentType());
      assert(Arg && "Coercion shouldn't fail!");
      E->setCtor(CD);
      E->setCtorArg(Arg);
    } else if (!Viable.empty()) {
      TC.diagnose(E->getLoc(), diag::constructor_overload_fail, true, CT)
        << E->getSourceRange();
      TC.printOverloadSetCandidates(Viable);
    } else {
      // FIXME: We should diagnose this once we synthesize
      // default constructors yet.
    }

    return E;
  }

  Expr *visitOpaqueValueExpr(OpaqueValueExpr *E) {
    // OpaqueValueExpr is fully type-checked.
    return E;
  }
    
  Expr *visitDotSyntaxBaseIgnoredExpr(DotSyntaxBaseIgnoredExpr *E) {
    // DotSyntaxBaseIgnoredExpr is fully type checked.
    return E;
  }

  Expr *visitCoerceExpr(CoerceExpr *E) {
    // The type of the expr is always the type that the MetaType LHS specifies.
    assert(!E->getType()->isUnresolvedType() &&"Type always specified by cast");
    return E;
  }

  Expr *visitConstructExpr(ConstructExpr *E) {
    // The type of the expr is always the type that the MetaType LHS specifies.
    assert(!E->getType()->isUnresolvedType() &&"Type always specified by cast");
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
          << AddrOf->getSourceRange();
      
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
      << E->getSourceRange();
    return nullptr;
  }

  Expr *makeBinOp(Expr *Op, Expr *LHS, Expr *RHS);
  Expr *foldSequence(Expr *LHS, ArrayRef<Expr*> &S, unsigned MinPrecedence);
  Expr *visitSequenceExpr(SequenceExpr *E);
  
  void PreProcessBraceStmt(BraceStmt *BS);
  
  Expr *visitFuncExpr(FuncExpr *E) {
    llvm_unreachable("Should not walk into FuncExprs!");
  }
  
  Expr *visitModuleExpr(ModuleExpr *E) {
    // ModuleExpr is fully resolved.
    assert(!E->getType()->isUnresolvedType());
    return E;
  }

  Expr *visitExplicitClosureExpr(ExplicitClosureExpr *E) {
    assert(E->getType().isNull() &&
           "Shouldn't walk into typed ExplicitClosures");
    E->setType(UnstructuredUnresolvedType::get(TC.Context));
    return E;
  }
  Expr *visitImplicitClosureExpr(ImplicitClosureExpr *E) {
    llvm_unreachable("Should not walk into ClosureExprs!");
  }

  Expr *visitApplyExpr(ApplyExpr *E) {
    return TC.semaApplyExpr(E);
  }
  
  SemaExpressionTree(TypeChecker &tc) : TC(tc) {}
  

  Expr *doIt(Expr *E) {
    return E->walk(*this);
  }

  bool walkToExprPre(Expr *E) {
    // Do not walk into FuncExpr or explicit closures.  They are analyzed
    // modularly, so we don't need to recurse into them and reanalyze their
    // body.  This prevents N^2 re-sema activity with lots of nested closures.
    if (isa<FuncExpr>(E)) return false;

    // Only walk into Explicit Closures if they haven't been seen at all yet.
    // This ensures that everything gets a type, even if it is an
    // UnstructuredUnresolvedType.
    return !isa<ExplicitClosureExpr>(E) || E->getType().isNull();
  }

  Expr *walkToExprPost(Expr *E) {
    // Dispatch to the right visitor case in the post-order walk.  We know
    // that the operands have already been processed and are valid.
    return this->visit(E);
  }

  bool walkToStmtPre(Stmt *S) {
    // Never recurse into statements.
    return false;
  }
};
} // end anonymous namespace.

/// Is the given expression a valid thing to use as the injection
/// function from the data for a newly-allocated array into the
/// given slice type?
Expr *TypeChecker::buildArrayInjectionFnRef(ArraySliceType *sliceType,
                                            Type lenTy, SourceLoc Loc) {
  // Build the expression "Slice<T>".
  Type implType = sliceType->getImplementationType();
  ValueDecl *implTypeDecl;
  // FIXME: There should be a better way to handle this.
  if (isa<NameAliasType>(implType))
    implTypeDecl = cast<NameAliasType>(implType)->getDecl();
  else if (NominalType *Nominal = dyn_cast<NominalType>(implType))
    implTypeDecl = Nominal->getDecl();
  else
    llvm_unreachable("Non-nominal slice type");
  
  Expr *sliceTypeRef =
    recheckTypes(new (Context) DeclRefExpr(implTypeDecl, Loc));
  if (!sliceTypeRef) return nullptr;

  // Build the expression "Slice<T>.convertFromHeapArray".
  Expr *injectionFn = semaUnresolvedDotExpr(
    new (Context) UnresolvedDotExpr(sliceTypeRef, Loc,
               Context.getIdentifier("convertFromHeapArray"),
                                     Loc));
  if (!injectionFn) return nullptr;

  // The input is a tuple type:
  TupleTypeElt argTypes[3];

  // The first element is Builtin.RawPointer.
  // FIXME: this should probably be either UnsafePointer<T> or the
  // first two arguments should be combined into a byref(heap).
  argTypes[0] = TupleTypeElt(Context.TheRawPointerType, Identifier());

  // The second element is the owner pointer, Builtin.ObjectPointer.
  argTypes[1] = TupleTypeElt(Context.TheObjectPointerType, Identifier());

  // The third element is the bound type.  Maybe this should be a
  // target-specific size_t type?
  argTypes[2] = TupleTypeElt(lenTy, Identifier());

  TupleType *input = TupleType::get(argTypes, Context);

  // The result is just the slice type.
  Type result = sliceType;

  FunctionType *fnTy = FunctionType::get(input, result, Context);

  // FIXME: this produces terrible diagnostics.
  return coerceToType(injectionFn, fnTy);
}


Expr *TypeChecker::semaUnresolvedDotExpr(UnresolvedDotExpr *E) {
  Expr *Base = E->getBase();
  Type BaseTy = Base->getType();
  Identifier MemberName = E->getName();
  
  if (BaseTy->isUnresolvedType()) {
    E->setType(UnstructuredUnresolvedType::get(Context));
    return E;
  }
  
  if (BaseTy->is<ErrorType>())
    return 0;  // Squelch an erroneous subexpression.

  // Perform name lookup.
  MemberLookup Lookup(BaseTy, MemberName, TU);
  
  if (!Lookup.isSuccess()) {
    // FIXME: This diagnostic is a bit painful.
    diagnose(E->getDotLoc(), diag::no_valid_dot_expression, BaseTy)
      << Base->getSourceRange() << SourceRange(E->getNameLoc(),E->getNameLoc());
    return 0;
  }

  bool IsMetatypeBase = Base->getType()->is<MetaTypeType>();
  if (IsMetatypeBase && Lookup.Results.size() == 1) {
    MemberLookupResult R = Lookup.Results[0];
    if (R.Kind == MemberLookupResult::TupleElement ||
        R.Kind == MemberLookupResult::MemberProperty) {
      diagnose(E->getNameLoc(), diag::no_valid_dot_expression, Base->getType());
      return 0;
    }
  }

  // If the base is a tuple, we need to force it to be either a proper lvalue
  // or a proper rvalue; otherwise, the semantics are strange.
  if (!Base->getType()->is<LValueType>())
    Base = convertToMaterializable(Base);

  return recheckTypes(Lookup.createResultAST(Base, E->getDotLoc(),
                                             E->getNameLoc(), Context));
}


/// getInfixData - If the specified expression is an infix binary
/// operator, return its precedence.
static InfixData getInfixData(TypeChecker &TC, Expr *E) {
  if (DeclRefExpr *DRE = dyn_cast<DeclRefExpr>(E)) {
    if (DRE->getDecl()->getAttrs().isInfix())
      return DRE->getDecl()->getAttrs().getInfixData();

    TC.diagnose(DRE->getLoc(), diag::binop_not_infix);

  // If this is an overload set, the entire overload set is required
  // to have the same infix data.
  } else if (OverloadedDeclRefExpr *OO = dyn_cast<OverloadedDeclRefExpr>(E)) {
    ValueDecl *FirstDecl = nullptr;
    InfixData Infix;
    for (auto D : OO->getDecls()) {
      // It is possible some unary operators got mixed into the overload set.
      if (!D->getAttrs().isInfix())
        continue;
      
      if (Infix.isValid() && Infix != D->getAttrs().getInfixData()) {
        TC.diagnose(OO->getLoc(), diag::binop_mismatched_infix);
        TC.diagnose(FirstDecl->getStartLoc(), diag::first_declaration);
        TC.diagnose(D->getStartLoc(), diag::second_declaration);
        return Infix;
      }
      
      Infix = D->getAttrs().getInfixData();
      FirstDecl = D;
    }

    if (Infix.isValid())
      return Infix;

    TC.diagnose(OO->getLoc(), diag::binop_not_overloaded);

  // Otherwise, complain.
  } else {
    TC.diagnose(E->getLoc(), diag::unknown_binop);
  }
  
  // Recover with an infinite-precedence left-associative operator.
  return InfixData(~0U, Associativity::Left);
}

Expr *SemaExpressionTree::makeBinOp(Expr *Op, Expr *LHS, Expr *RHS) {
  if (!LHS || !RHS)
    return nullptr;

  // Build the argument to the operation.
  Expr *ArgElts[] = { LHS, RHS };
  auto ArgElts2 = TC.Context.AllocateCopy(MutableArrayRef<Expr*>(ArgElts));
  TupleExpr *Arg = new (TC.Context) TupleExpr(SourceLoc(), 
                                              ArgElts2, 0, SourceLoc());
  if (TC.semaTupleExpr(Arg))
    return nullptr;

  // Build the operation.
  return visit(new (TC.Context) BinaryExpr(Op, Arg));
}

/// foldSequence - Take a sequence of expressions and fold a prefix of
/// it into a tree of BinaryExprs using precedence parsing.
Expr *SemaExpressionTree::foldSequence(Expr *LHS, ArrayRef<Expr*> &S,
                                       unsigned MinPrecedence) {
  // Invariant: S is even-sized.
  // Invariant: All elements at even indices are operator references.
  assert(!S.empty());
  assert((S.size() & 1) == 0);

  // Extract out the first operator.  If its precedence is lower
  // than the minimum, stop here.
  Expr *Op1 = S[0];
  InfixData Op1Info = getInfixData(TC, Op1);
  if (Op1Info.getPrecedence() < MinPrecedence) return LHS;

  // We will definitely be consuming at least one operator.
  // Pull out the prospective RHS and slice off the first two elements.
  Expr *RHS = S[1];
  S = S.slice(2);

  while (!S.empty()) {
    assert(!S.empty());
    assert((S.size() & 1) == 0);
    assert(Op1Info.getPrecedence() >= MinPrecedence);

    // Pull out the next binary operator.
    Expr *Op2 = S[0];
    InfixData Op2Info = getInfixData(TC, Op2);

    // If the second operator's precedence is lower than the min
    // precedence, break out of the loop.
    if (Op2Info.getPrecedence() < MinPrecedence) break;

    // If the first operator's precedence is higher than the second
    // operator's precedence, or they have matching precedence and are
    // both left-associative, fold LHS and RHS immediately.
    if (Op1Info.getPrecedence() > Op2Info.getPrecedence() ||
        (Op1Info == Op2Info && Op1Info.isLeftAssociative())) {
      LHS = makeBinOp(Op1, LHS, RHS);
      RHS = S[1];
      Op1 = Op2;
      Op1Info = Op2Info;
      S = S.slice(2);
      continue;
    }

    // If the first operator's precedence is lower than the second
    // operator's precedence, recursively fold all such
    // higher-precedence operators starting from this point, then
    // repeat.
    if (Op1Info.getPrecedence() < Op2Info.getPrecedence()) {
      RHS = foldSequence(RHS, S, Op1Info.getPrecedence() + 1);
      continue;
    }

    // If the first operator's precedence is the same as the second
    // operator's precedence, and they're both right-associative,
    // recursively fold operators starting from this point, then
    // immediately fold LHS and RHS.
    if (Op1Info == Op2Info && Op1Info.isRightAssociative()) {
      RHS = foldSequence(RHS, S, Op1Info.getPrecedence());
      LHS = makeBinOp(Op1, LHS, RHS);

      // If we've drained the entire sequence, we're done.
      if (S.empty()) return LHS;

      // Otherwise, start all over with our new LHS.
      return foldSequence(LHS, S, MinPrecedence);
    }

    // If we ended up here, it's because we have two operators
    // with mismatched or no associativity.
    assert(Op1Info.getPrecedence() == Op2Info.getPrecedence());
    assert(Op1Info.getAssociativity() != Op2Info.getAssociativity() ||
           Op1Info.isNonAssociative());

    if (Op1Info.isNonAssociative()) {
      // FIXME: QoI ranges
      TC.diagnose(Op1->getLoc(), diag::non_assoc_adjacent);
    } else if (Op2Info.isNonAssociative()) {
      TC.diagnose(Op2->getLoc(), diag::non_assoc_adjacent);
    } else {
      TC.diagnose(Op1->getLoc(), diag::incompatible_assoc);
    }
    
    // Recover by arbitrarily binding the first two.
    LHS = makeBinOp(Op1, LHS, RHS);
    return foldSequence(LHS, S, MinPrecedence);
  }

  // Fold LHS and RHS together and declare completion.
  return makeBinOp(Op1, LHS, RHS);
}

/// foldSequence - Take a SequenceExpr and fold it into a tree of
/// BinaryExprs using precedence parsing.
Expr *SemaExpressionTree::visitSequenceExpr(SequenceExpr *E) {
  ArrayRef<Expr*> Elts = E->getElements();
  assert(Elts.size() > 1 && "inadequate number of elements in sequence");
  assert((Elts.size() & 1) == 1 && "even number of elements in sequence");

  Expr *LHS = Elts[0];
  Elts = Elts.slice(1);

  Expr *Result = foldSequence(LHS, Elts, /*min precedence*/ 0);
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
      }
      return E;
    }
    
    bool walkToStmtPre(Stmt *S) {
      // Never recurse into statements.
      return false;
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

Type TypeChecker::getDefaultLiteralType(LiteralExpr *E) {
  if (isa<IntegerLiteralExpr>(E)) {
    if (!IntLiteralType) {
      IntLiteralType = lookupGlobalType(*this, "IntegerLiteralType");
      if (!IntLiteralType) {
        diagnose(E->getLoc(), diag::no_IntegerLiteralType_found);
        IntLiteralType = BuiltinIntegerType::get(32, Context);
      }
    }
    return IntLiteralType;
  }
  
  if (isa<FloatLiteralExpr>(E)) {
    if (!FloatLiteralType) {
      FloatLiteralType = lookupGlobalType(*this, "FloatLiteralType");
      if (!FloatLiteralType) {
        diagnose(E->getLoc(), diag::no_FloatLiteralType_found);
        FloatLiteralType = Context.TheIEEE64Type;
      }
    }
    return FloatLiteralType;
  }
  
  if (isa<CharacterLiteralExpr>(E)) {
    if (!CharacterLiteralType) {
      CharacterLiteralType = lookupGlobalType(*this, "CharacterLiteralType");
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
    StringLiteralType = lookupGlobalType(*this, "StringLiteralType");
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
      if (E->getType()->isUnresolvedType()) {
        if (LiteralExpr *Lit = dyn_cast<LiteralExpr>(E))
          return TC.coerceToType(Lit, TC.getDefaultLiteralType(Lit));        
      }
      return E;
    }
    
    bool walkToStmtPre(Stmt *S) {
      // Never recurse into statements.
      return false;
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

bool TypeChecker::typeCheckExpression(Expr *&E, Type ConvertType) {
  SemaExpressionTree SET(*this);
  E = SET.doIt(E);

  if (E == 0) return true;

  // If our context specifies a type, apply it to the expression.
  if (ConvertType) {
    CoercedExpr CoercedE = coerceToType(E, ConvertType);
    switch (CoercedE.getKind()) {
    case CoercionResult::Succeeded:
      E = CoercedE;
      break;

    case CoercionResult::Failed:
      return true;

    case CoercionResult::Unknowable:
      // We may have literals to resolve.
      break;
    }
  }
  
  DependenceWalker dependence;
  E->walk(dependence);

  // Fast path: if we found nothing unresolved, we're done.
  if (!dependence.OneUnresolvedExpr)
    return false;

  // Otherwise, if we found any unresolved literals, then force them to
  // the library specified default type for the appropriate literal kind.
  if (dependence.HasUnresolvedLiterals) {
    if (resolveUnresolvedLiterals(E))
      return true;
    
    // If our context specifies a type, apply it to the expression.
    if (ConvertType) {
      E = coerceToType(E, ConvertType);
      if (E == 0) return true;
    }

    dependence.reset();
    E->walk(dependence);
  }
  
  // If there are no unresolved expressions, then we're done.
  if (dependence.OneUnresolvedExpr == 0) return false;

  // Otherwise, emit an error about the ambiguity.
  diagnose(dependence.OneUnresolvedExpr->getLoc(),
           diag::ambiguous_expression_unresolved)
    << dependence.OneUnresolvedExpr->getSourceRange();
  E = 0;
  return true;
}

bool TypeChecker::semaFunctionSignature(FuncExpr *FE) {
  bool hadError = false;
  for (unsigned i = FE->getParamPatterns().size(); i != 0; --i) {
    Pattern *pattern = FE->getParamPatterns()[i - 1];
    if (typeCheckPattern(pattern, /*isFirstPass*/false)) {
      hadError = true;
      continue;
    }
  }
  if (!hadError && validateType(FE->getType(), /*isFirstPass*/false))
    hadError = true;
  if (hadError)
    FE->setType(ErrorType::get(Context));
  return hadError;
}

static bool convertWithMethod(TypeChecker &TC, Expr *&E, Identifier method,
                              bool (*isTypeOkay)(Type),
                              Diag<Type> diagnostic) {
  // If it's just an l2r conversion away from the right type, we're done.
  if (LValueType *lvalue = E->getType()->getAs<LValueType>()) {
    if (isTypeOkay(lvalue->getObjectType())) {
      Expr *newExpr = TC.convertToRValue(E);
      if (!newExpr) return true;
      E = newExpr;
      return false;
    }

  // Otherwise, if it *is* the right type, we're done.
  } else if (isTypeOkay(E->getType())) {
    return false;
  }

  // We allow up to two iterations here: one to convert to some
  // intermediate type, and one to convert from that to an acceptable
  // type.  (The diagnostic here assumes normal cade which uses the
  // swift standard library.)
  unsigned numConversionsRemaining = 2;
  Type origType = E->getType();
  do {
    UnresolvedDotExpr *UDE =
      new (TC.Context) UnresolvedDotExpr(E, E->getStartLoc(), method,
                                         E->getEndLoc());
    Expr *checkedDot = TC.semaUnresolvedDotExpr(UDE);
    if (!checkedDot) return true;
    E = checkedDot;

    TupleExpr *callArgs =
      new (TC.Context) TupleExpr(E->getStartLoc(), MutableArrayRef<Expr *>(), 0,
                                 E->getEndLoc(),
                                 TupleType::getEmpty(TC.Context));
    CallExpr *CE = new (TC.Context) CallExpr(E, callArgs, Type());
    Expr *CheckedCall = TC.semaApplyExpr(CE);
    if (!CheckedCall) return true;
    E = CheckedCall;

    // We're done as soon as we reach an acceptable type.
    if (isTypeOkay(E->getType())) return false;

    --numConversionsRemaining;
  } while (numConversionsRemaining != 0);

  TC.diagnose(E->getLoc(), diagnostic, origType)
    << E->getSourceRange();
  return true;
}

static bool isBuiltinIntegerType(Type T) {
  return T->is<BuiltinIntegerType>();
}

/// Type check an array bound.
bool TypeChecker::typeCheckArrayBound(Expr *&E, bool constantRequired) {
  // If it's an integer literal expression, just convert the type directly.
  if (isa<IntegerLiteralExpr>(E)) {
    // FIXME: the choice of 64-bit is rather arbitrary.
    E->setType(BuiltinIntegerType::get(64, Context));
    return false;
  }

  // Otherwise, if a constant expression is required, fail.
  if (constantRequired) {
    diagnose(E->getLoc(), diag::non_constant_array)
      << E->getSourceRange();
    return true;
  }

  // Just quietly accept if the type is unresolved.
  if (E->getType()->isUnresolvedType())
    return false;

  // Otherwise, apply .getArrayBoundValue() until we get an acceptable
  // integer type.
  Identifier methodName = Context.getIdentifier("getArrayBoundValue");
  return convertWithMethod(*this, E, methodName, &isBuiltinIntegerType,
                           diag::array_bound_convert_limit_reached);
}

static bool isBuiltinI1(Type T) {
  if (BuiltinIntegerType *BT = T->getAs<BuiltinIntegerType>())
    return BT->getBitWidth() == 1;
  return false;
}

bool TypeChecker::typeCheckCondition(Expr *&E) {
  if (typeCheckExpression(E))
    return true;

  Identifier methodName = Context.getIdentifier("getLogicValue");
  return convertWithMethod(*this, E, methodName, &isBuiltinI1,
                           diag::condition_convert_limit_reached);
}

bool TypeChecker::typeCheckAssignment(Expr *&Dest, SourceLoc EqualLoc,
                                      Expr *&Src) {
  SemaExpressionTree SET(*this);
  
  // Type check the destination.
  Dest = SET.doIt(Dest);
  if (!Dest) return true;
  
  // Type check the source.
  Src = SET.doIt(Src);
  if (!Src) return true;
  
  // Determine whether the destination and source have unresolved expressions.
  DependenceWalker DestDependence;
  Dest->walk(DestDependence);

  DependenceWalker SrcDependence;
  Src->walk(SrcDependence);

  bool IsDestUnresolved = DestDependence.OneUnresolvedExpr != nullptr;
  bool IsSrcUnresolved = SrcDependence.OneUnresolvedExpr != nullptr;

  // If both destination and source are unresolved, try resolving unresolved
  // literals.
  if (IsDestUnresolved && IsSrcUnresolved) {
    // Resolve unresolved literals in the destination, first.
    if (resolveUnresolvedLiterals(Dest))
      return true;

    // Is the destination still unresolved?
    DestDependence.reset();
    Dest->walk(DestDependence);
    IsDestUnresolved = DestDependence.OneUnresolvedExpr != nullptr;
    
    // If the destination is still unresolved, resolve unresolved literals in
    // the source as well.
    if (IsDestUnresolved) {
      if (resolveUnresolvedLiterals(Src))
        return true;
      
      // Is the destination still unresolved?
      SrcDependence.reset();
      Src->walk(SrcDependence);
      IsSrcUnresolved = SrcDependence.OneUnresolvedExpr != nullptr;
    }
  }
  
  // If the destination is not unresolved, coerce the source to the object type
  // of the destination.
  if (!IsDestUnresolved) {
    Type DestTy = Dest->getType();
    if (LValueType *DestLV = DestTy->getAs<LValueType>())
      DestTy = DestLV->getObjectType();
    else {
      if (!DestTy->is<ErrorType>())
        diagnose(EqualLoc, diag::assignment_lhs_not_lvalue)
          << Dest->getSourceRange();
      
      return true;
    }
    
    Src = coerceToType(Src, DestTy);
    return Src == nullptr;    
  }

  // If the source is not unresolved (but the destination is), coerce the
  // destination to an lvalue of the source type.
  if (!IsSrcUnresolved) {
    Src = convertToRValue(Src);
    if (!Src) return true;
    
    Type SrcTy = Src->getType();
    Dest = coerceToType(Dest,
                        LValueType::get(SrcTy,
                                        LValueType::Qual::NonHeap,
                                        Context),
                        /*Assignment=*/true);
    return Dest == nullptr;
  }
  
  // Both source and destination are still unresolved.
  diagnose(DestDependence.OneUnresolvedExpr->getLoc(),
           diag::ambiguous_expression_unresolved)
    << DestDependence.OneUnresolvedExpr->getSourceRange();
  return true;
}
