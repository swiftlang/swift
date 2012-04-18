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
#include "NameLookup.h"
#include "swift/AST/ASTVisitor.h"
#include "swift/AST/Attr.h"
#include "swift/AST/ASTWalker.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/ADT/SmallPtrSet.h"
using namespace swift;

//===----------------------------------------------------------------------===//
// Expression Semantic Analysis Routines
//===----------------------------------------------------------------------===//

/// Build a reference to the given declaration as an r-value.
Expr *TypeChecker::buildDeclRefRValue(ValueDecl *val, SourceLoc loc) {
  if (!val->isReferencedAsLValue()) {
    return new (Context) DeclRefExpr(val, loc, val->getType());
  }

  Type lvalueType = LValueType::get(val->getType(),
                                    LValueType::Qual::DefaultForVar,
                                    Context);
  Expr *E = new (Context) DeclRefExpr(val, loc, lvalueType);
  return new (Context) LoadExpr(E, val->getType());
}

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
    Type EltTy;
    
    // If the element value is missing, it has the value of the default
    // expression of the result type, which must be known.
    Expr *Elt = TE->getElement(i);
    if (Elt == 0) {
      assert(TE->getType() && isa<TupleType>(TE->getType()) && 
             "Can't have default value without a result type");
      EltTy = cast<TupleType>(TE->getType())->getElementType(i);
    } else {
      EltTy = Elt->getType();
    }

    // If a name was specified for this element, use it.
    Identifier Name = TE->getElementName(i);
    ResultTyElts[i] = TupleTypeElt(EltTy, Name);
  }
  
  TE->setType(TupleType::get(ResultTyElts, Context));
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
    if (isa<UnaryExpr>(E) &&
        !cast<DeclRefExpr>(E1)->getDecl()->isOperator()) {
      diagnose(E1->getLoc(), diag::unary_op_without_attribute);
      return 0;
    }
    
    if (isa<BinaryExpr>(E) &&
        !cast<DeclRefExpr>(E1)->getDecl()->getAttrs().isInfix()) {
      diagnose(E1->getLoc(), diag::binary_op_without_attribute);
      return 0;
    }
    
    // We have a function application.  Check that the argument type matches the
    // expected type of the function.
    E2 = coerceToType(E2, FT->getInput());
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
    Type Ty = MT->getTypeDecl()->getUnderlyingType();
    if (Ty->is<ErrorType>())
      return 0;  // Squelch an erroneous subexpression.

    // The only well formed version of this is a oneof (or sugar for one) that
    // is constructed.
    // TODO: It might make sense to be able to "default construct" protocols if
    // they have a construction member.
    // TODO: We could allow constructing a tuple this way, though it is somewhat
    // silly and pointless to do so.
    if (OneOfType *OOT = Ty->getAs<OneOfType>()) {
      if (!OOT->getElements().empty()) {
        SmallVector<ValueDecl *, 4> Methods;
        
        // Look for extension methods with the same name as the class.
        // FIXME: This should look specifically for constructors.
        TU.lookupGlobalExtensionMethods(Ty, OOT->getDecl()->getName(),
                                        Methods);
        
        // Add each of the one-of elements.
        Methods.insert(Methods.begin(),
                       OOT->getElements().begin(), OOT->getElements().end());
        Expr *FnRef = OverloadedDeclRefExpr::createWithCopy(Methods,
                                                            E1->getStartLoc());
        
        // FIXME: This loses source information!
        return semaApplyExpr(new (Context) ConstructorCallExpr(FnRef, E2));
      }
    }
  }
  
  // Otherwise, the function's type must be dependent.  If it is something else,
  // we have a type error.
  if (!E1->getType()->isDependentType()) {
    diagnose(E1->getLoc(), diag::called_expr_isnt_function);
    return 0;
  }
  
  // Otherwise, we must have an application to an overload set.  See if we can
  // resolve which overload member is based on the argument type.
  OverloadSetRefExpr *OS = dyn_cast<OverloadSetRefExpr>(E1);
  if (!OS) {
    // If not, just use the dependent type.
    E->setType(UnstructuredDependentType::get(Context));
    return E;
  }

  // Perform overload resolution.
  llvm::SmallVector<ValueDecl *, 4> Viable;
  ValueDecl *Best = filterOverloadSet(OS->getDecls(), OS->getBaseType(), E2,
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
  E->setType(UnstructuredDependentType::get(Context));
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
/// them.  During the initial parse, these decls get a 'dependent' type, which
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

  Expr *visitIntegerLiteralExpr(IntegerLiteralExpr *E) {
    if (E->getType().isNull())
      E->setType(UnstructuredDependentType::get(TC.Context));
    return E;
  }
  Expr *visitFloatLiteralExpr(FloatLiteralExpr *E) {
    if (E->getType().isNull())
      E->setType(UnstructuredDependentType::get(TC.Context));
    return E;
  }
  Expr *visitStringLiteralExpr(StringLiteralExpr *E) {
    if (E->getType().isNull())
      E->setType(UnstructuredDependentType::get(TC.Context));
    return E;
  }
  Expr *visitDeclRefExpr(DeclRefExpr *E) {
    if (E->getDecl() == 0) {
      TC.diagnose(E->getLoc(), diag::use_undeclared_identifier);
      return 0;
    }

    // If the type of a decl is not computed yet, make the declref dependent;
    // this can happen for references to "$0" etc.
    if (!E->getDecl()->hasType()) {
      E->setType(UnstructuredDependentType::get(TC.Context));
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
    E->setType(UnstructuredDependentType::get(TC.Context));
    return E;
  }
  Expr *visitUnresolvedDeclRefExpr(UnresolvedDeclRefExpr *E) {
    llvm_unreachable("name binding should resolve all UnresolvedDeclRefExprs!");
    return 0;
  }
  Expr *visitMemberRefExpr(MemberRefExpr *E) {
    if (E->getDecl()->getType()->is<ErrorType>())
      return 0;
    
    // If the base is an lvalue, so is the member reference.
    Type ResultTy = E->getDecl()->getTypeOfReference();
    if (!E->getBase()->getType()->is<LValueType>()) {
      if (LValueType *ResultRefTy = ResultTy->getAs<LValueType>())
        ResultTy = ResultRefTy->getObjectType();
    }
    
    E->setType(ResultTy);
    return E;
  }
  Expr *visitUnresolvedMemberExpr(UnresolvedMemberExpr *E) {
    E->setType(UnstructuredDependentType::get(TC.Context));
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
  Expr *visitUnresolvedDotExpr(UnresolvedDotExpr *E) {
    return TC.semaUnresolvedDotExpr(E);
  }
  
  Expr *visitTupleElementExpr(TupleElementExpr *E) {
    // TupleElementExpr is fully resolved.
    // FIXME: This will not always be the case?
    assert(!E->getType()->isDependentType());
    return E;
  }

  Expr *visitNewArrayExpr(NewArrayExpr *E) {
    if (TC.validateType(E->getElementType()))
      return nullptr;

    // FIXME
    E->setType(E->getElementType());
    return E;
  }
  
  Expr *visitDotSyntaxBaseIgnoredExpr(DotSyntaxBaseIgnoredExpr *E) {
    // DotSyntaxBaseIgnoredExpr is fully type checked.
    return E;
  }

  Expr *visitImplicitConversionExpr(ImplicitConversionExpr *E) {
    assert(!E->getType()->isDependentType());
    // Implicit conversions have been fully checked.
    return E;
  }
  
  Expr *visitAddressOfExpr(AddressOfExpr *E) {
    // Turn l-values into explicit l-values.
    if (LValueType *type = E->getSubExpr()->getType()->getAs<LValueType>()) {
      if (!type->isExplicit())
        type = LValueType::get(type->getObjectType(),
                               type->getQualifiers().withoutImplicit(),
                               TC.Context);
      E->setType(type);
      return E;
    }

    // Propagate out dependence.
    if (E->getSubExpr()->getType()->isDependentType()) {
      E->setType(UnstructuredDependentType::get(TC.Context));
      return E;
    }

    // Complain.
    TC.diagnose(E->getLoc(), diag::address_of_rvalue,
                E->getSubExpr()->getType())
      << E->getSourceRange();
    return nullptr;
  }
  
  Expr *visitSequenceExpr(SequenceExpr *E);
  
  void PreProcessBraceStmt(BraceStmt *BS);
  
  Expr *visitFuncExpr(FuncExpr *E) {
    llvm_unreachable("Should not walk into FuncExprs!");
  }
  
  Expr *visitModuleExpr(ModuleExpr *E) {
    // ModuleExpr is fully resolved.
    assert(!E->getType()->isDependentType());
    return E;
  }

  Expr *visitExplicitClosureExpr(ExplicitClosureExpr *E) {
    assert(E->getType().isNull() &&
           "Shouldn't walk into typed ExplicitClosures");
    E->setType(UnstructuredDependentType::get(TC.Context));
    return E;
  }
  Expr *visitImplicitClosureExpr(ImplicitClosureExpr *E) {
    llvm_unreachable("Should not walk into ClosureExprs!");
  }

  Expr *visitApplyExpr(ApplyExpr *E) {
    return TC.semaApplyExpr(E);
  }
  Expr *visitBinaryExpr(BinaryExpr *E) {
    // This is necessary because ASTWalker doesn't visit the
    // TupleExpr right now.
    if (TupleExpr *Tuple = dyn_cast<TupleExpr>(E->getArg())) {
      if (TC.semaTupleExpr(Tuple))
        return 0;
    }
    return visitApplyExpr(E);
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
    // This ensures that everything gets a type, even if it is a UnstructuredDependentType.
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


Expr *TypeChecker::semaUnresolvedDotExpr(UnresolvedDotExpr *E) {
  Expr *Base = E->getBase();
  Type BaseTy = Base->getType();
  Identifier MemberName = E->getName();
  
  if (BaseTy->isDependentType()) {
    E->setType(UnstructuredDependentType::get(Context));
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
        TC.diagnose(FirstDecl->getLocStart(), diag::first_declaration);
        TC.diagnose(D->getLocStart(), diag::second_declaration);
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

static Expr *makeBinOp(TypeChecker &TC, Expr *Op, Expr *LHS, Expr *RHS) {
  Expr *ArgElts[] = { LHS, RHS };
  auto ArgElts2 = TC.Context.AllocateCopy(MutableArrayRef<Expr*>(ArgElts));
  TupleExpr *Arg = new (TC.Context) TupleExpr(SourceLoc(), 
                                              ArgElts2, 0, SourceLoc());
  return new (TC.Context) BinaryExpr(Op, Arg);
}

/// foldSequence - Take a sequence of expressions and fold a prefix of
/// it into a tree of BinaryExprs using precedence parsing.
static Expr *foldSequence(TypeChecker &TC, Expr *LHS, ArrayRef<Expr*> &S,
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
      LHS = makeBinOp(TC, Op1, LHS, RHS);
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
      RHS = foldSequence(TC, RHS, S, Op1Info.getPrecedence() + 1);
      continue;
    }

    // If the first operator's precedence is the same as the second
    // operator's precedence, and they're both right-associative,
    // recursively fold operators starting from this point, then
    // immediately fold LHS and RHS.
    if (Op1Info == Op2Info && Op1Info.isRightAssociative()) {
      RHS = foldSequence(TC, RHS, S, Op1Info.getPrecedence());
      LHS = makeBinOp(TC, Op1, LHS, RHS);

      // If we've drained the entire sequence, we're done.
      if (S.empty()) return LHS;

      // Otherwise, start all over with our new LHS.
      return foldSequence(TC, LHS, S, MinPrecedence);
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
    LHS = makeBinOp(TC, Op1, LHS, RHS);
    return foldSequence(TC, LHS, S, MinPrecedence);
  }

  // Fold LHS and RHS together and declare completion.
  return makeBinOp(TC, Op1, LHS, RHS);
}

/// foldSequence - Take a SequenceExpr and fold it into a tree of
/// BinaryExprs using precedence parsing.
Expr *TypeChecker::foldSequence(SequenceExpr *E) {
  ArrayRef<Expr*> Elts = E->getElements();
  assert(Elts.size() > 1 && "inadequate number of elements in sequence");
  assert((Elts.size() & 1) == 1 && "even number of elements in sequence");

  Expr *LHS = Elts[0];
  Elts = Elts.slice(1);

  Expr *Result = ::foldSequence(*this, LHS, Elts, /*min precedence*/ 0);
  assert(Elts.empty());
  return Result;
}

Expr *SemaExpressionTree::visitSequenceExpr(SequenceExpr *E) {
  llvm_unreachable("visiting sequence expression during normal type-checking!");
}

Expr *TypeChecker::recheckTypes(Expr *E) {
  if (!E)
    return nullptr;
  
  return SemaExpressionTree(*this).visit(E);
}

bool TypeChecker::typeCheckExpression(Expr *&E, Type ConvertType) {
  SemaExpressionTree SET(*this);
  E = SET.doIt(E);

  if (E == 0) return true;

  // If our context specifies a type, apply it to the expression.
  if (ConvertType) {
    E = coerceToType(E, ConvertType);
    if (E == 0) return true;
  }
  
  // Check the initializer/body to make sure that we succeeded in resolving
  // all of the types contained within it.  If we've resolved everything, then
  // we're done processing the expression.  While we're doing the walk, keep
  // track of whether we have any literals without a resolved type.
  struct DependenceWalker : ASTWalker {
    DependenceWalker() { reset(); }

    void reset() {
      OneDependentExpr = nullptr;
      HasDependentLiterals = false;
    }

    Expr *walkToExprPost(Expr *E) {    
      assert(!isa<SequenceExpr>(E) && "Should have resolved this");
    
      if (E->getType()->isDependentType()) {
        // Remember the first dependent expression we come across.
        if (OneDependentExpr == 0)
          OneDependentExpr = E;

        // Also remember if we see any literals with dependent types.
        if (isa<IntegerLiteralExpr>(E) || isa<FloatLiteralExpr>(E) ||
            isa<StringLiteralExpr>(E))
          HasDependentLiterals = true;
      }
      return E;
    }

    bool walkToStmtPre(Stmt *S) {
      // Never recurse into statements.
      return false;
    }

    Expr *OneDependentExpr;
    bool HasDependentLiterals;
  };
  DependenceWalker dependence;
  E->walk(dependence);

  // Fast path: if we found nothing dependent, we're done.
  if (!dependence.OneDependentExpr)
    return false;

  // Otherwise, if we found any dependent literals, then force them to
  // the library specified default type for the appropriate literal kind.
  if (dependence.HasDependentLiterals) {
    struct UpdateWalker : ASTWalker {
      UpdateWalker(TypeChecker &TC) : TC(TC) {}

      Expr *walkToExprPost(Expr *E) {
        // Process dependent literals.
        if (E->getType()->isDependentType()) {
          if (IntegerLiteralExpr *lit = dyn_cast<IntegerLiteralExpr>(E))
            return TC.coerceToType(lit, getIntLiteralType(lit->getLoc()));

          if (FloatLiteralExpr *lit = dyn_cast<FloatLiteralExpr>(E))
            return TC.coerceToType(lit, getFloatLiteralType(lit->getLoc()));
          
          if (StringLiteralExpr *lit = dyn_cast<StringLiteralExpr>(E))
            return TC.coerceToType(lit, getStringLiteralType(lit->getLoc()));
        }
        return E;
      }

      bool walkToStmtPre(Stmt *S) {
        // Never recurse into statements.
        return false;
      }

    private:
      Type lookupGlobalType(StringRef name) {
        TypeAliasDecl *TAD =
          TC.TU.lookupGlobalType(TC.Context.getIdentifier(name),
                                 NLKind::UnqualifiedLookup);
        return (TAD ? TAD->getAliasType() : nullptr);
      }

      Type getIntLiteralType(SourceLoc loc) {
        if (IntLiteralType.isNull()) {
          IntLiteralType = lookupGlobalType("IntegerLiteralType");
          if (IntLiteralType.isNull()) {
            TC.diagnose(loc, diag::no_IntegerLiteralType_found);
            IntLiteralType = BuiltinIntegerType::get(32, TC.Context);
          }
        }
        return IntLiteralType;
      }

      Type getFloatLiteralType(SourceLoc loc) {
        if (FloatLiteralType.isNull()) {
          FloatLiteralType = lookupGlobalType("FloatLiteralType");
          if (FloatLiteralType.isNull()) {
            TC.diagnose(loc, diag::no_FloatLiteralType_found);
            FloatLiteralType = TC.Context.TheIEEE64Type;
          }
        }
        return FloatLiteralType;
      }
      Type getStringLiteralType(SourceLoc loc) {
        if (StringLiteralType.isNull()) {
          StringLiteralType = lookupGlobalType("StringLiteralType");
          if (StringLiteralType.isNull()) {
            TC.diagnose(loc, diag::no_StringLiteralType_found);
            StringLiteralType = TC.Context.TheRawPointerType;
          }
        }
        return StringLiteralType;
      }

      TypeChecker &TC;
      Type IntLiteralType, FloatLiteralType, StringLiteralType;
    };

    // Walk the tree again to update all the entries.  If this fails, give up.
    E = E->walk(UpdateWalker(*this));
    if (E == 0) return true;
    
    // Now that we've added some types to the mix, re-type-check the expression
    // tree and recheck for dependent types.
    E = SET.doIt(E);
    if (E == 0) return true;

    // If our context specifies a type, apply it to the expression.
    if (ConvertType) {
      E = coerceToType(E, ConvertType);
      if (E == 0) return true;
    }

    dependence.reset();
    E->walk(dependence);
  }
  
  // If there are no dependent expressions, then we're done.
  if (dependence.OneDependentExpr == 0) return false;

  // Otherwise, emit an error about the ambiguity.
  diagnose(dependence.OneDependentExpr->getLoc(),
           diag::ambiguous_expression_unresolved)
    << dependence.OneDependentExpr->getSourceRange();
  E = 0;
  return true;
}

bool TypeChecker::semaFunctionSignature(FuncExpr *FE) {
  bool hadError = false;
  for (unsigned i = FE->getParamPatterns().size(); i != 0; --i) {
    Pattern *pattern = FE->getParamPatterns()[i - 1];
    if (typeCheckPattern(pattern)) {
      hadError = true;
      continue;
    }
  }
  return hadError;
}


bool TypeChecker::typeCheckCondition(Expr *&E) {
  if (typeCheckExpression(E))
    return true;
  if (!(E = convertToRValue(E)))
    return true;

  CanType BuiltinI1(BuiltinIntegerType::get(1, Context));
  unsigned ConvertLimit = 0;
  Type OrigType = E->getType();
  while (E->getType()->getCanonicalType() != BuiltinI1) {
    // We allow up to two iterations here: one to convert to bool, and one to
    // convert from bool to i1.  (The diagnostic here assumes normal cade
    // which uses the swift standard library.)
    if (ConvertLimit == 2) {
      diagnose(E->getStartLoc(), diag::condition_convert_limit_reached,
               OrigType);
      return true;
    }
    Identifier LogicValId = Context.getIdentifier("getLogicValue");
    UnresolvedDotExpr *UDE = 
        new (Context) UnresolvedDotExpr(E, E->getStartLoc(), LogicValId,
                                        E->getEndLoc());
    E = semaUnresolvedDotExpr(UDE);
    if (!E)
      return true;
    TupleExpr *CallArgs =
        new (Context) TupleExpr(E->getStartLoc(), MutableArrayRef<Expr *>(), 0,
                                E->getEndLoc(), TupleType::getEmpty(Context));
    CallExpr *CE =
        new (Context) CallExpr(E, CallArgs, Type());
    E = semaApplyExpr(CE);
    if (!E)
      return true;
    ++ConvertLimit;
  }
  return false;
}
