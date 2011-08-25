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

#include "TypeChecking.h"
#include "swift/AST/ASTVisitor.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/ADT/Twine.h"
using namespace swift;

//===----------------------------------------------------------------------===//
// Expression Semantic Analysis Routines
//===----------------------------------------------------------------------===//

bool TypeChecker::semaTupleExpr(TupleExpr *TE) {
  // A tuple expr with a single subexpression and no name is just a grouping
  // paren.
  if (TE->isGroupingParen()) {
    TE->Ty = TE->SubExprs[0]->Ty;
    return false;
  }
  
  // Compute the result type.
  SmallVector<TupleTypeElt, 8> ResultTyElts(TE->NumSubExprs);
  
  for (unsigned i = 0, e = TE->NumSubExprs; i != e; ++i) {
    // If the element value is missing, it has the value of the default
    // expression of the result type, which must be known.
    if (TE->SubExprs[i] == 0) {
      assert(TE->Ty && isa<TupleType>(TE->Ty.getPointer()) && 
             "Can't have default value without a result type");
      
      ResultTyElts[i].Ty =
      cast<TupleType>(TE->Ty.getPointer())->getElementType(i);
      
      // FIXME: What about a default value that is dependent?
      if (ResultTyElts[i].Ty->is<DependentType>()) {
        TE->Ty = ResultTyElts[i].Ty;
        return false;
      }
    } else {
      // If any of the tuple element types is dependent, the whole tuple should
      // have dependent type.
      if (TE->SubExprs[i]->Ty->is<DependentType>()) {
        TE->Ty = TE->SubExprs[i]->Ty;
        return false;
      }
      
      ResultTyElts[i].Ty = TE->SubExprs[i]->Ty;
    }
    
    // If a name was specified for this element, use it.
    if (TE->SubExprNames)
      ResultTyElts[i].Name = TE->SubExprNames[i];
  }
  
  TE->Ty = TupleType::get(ResultTyElts, Context);
  return false;
}

bool TypeChecker::semaApplyExpr(ApplyExpr *E) {
  // If we have a BinaryExpr, make sure to recursively check the tuple argument.
  if (BinaryExpr *BE = dyn_cast<BinaryExpr>(E)) {
    if (semaTupleExpr(BE->getArgTuple()))
      return true;
  }
  
  Expr *&E1 = E->Fn;
  Expr *&E2 = E->Arg;
  
  // If we have a concrete function type, then we win.
  if (FunctionType *FT = E1->Ty->getAs<FunctionType>()) {
    // If this is an operator, make sure that the declaration found was declared
    // as such.
    if (isa<UnaryExpr>(E) && !cast<DeclRefExpr>(E1)->D->Attrs.isUnary) {
      error(E1->getLocStart(),
            "use of unary operator without 'unary' attribute specified");
      return true;
    }
    
    if (isa<BinaryExpr>(E) &&
               !cast<DeclRefExpr>(E1)->D->Attrs.isInfix()) {
      error(E1->getLocStart(),
            "use of unary operator without 'unary' attribute specified");
      return true;
    }
    
    // We have a function application.  Check that the argument type matches the
    // expected type of the function.
    E2 = convertToType(E2, FT->Input);
    if (E2 == 0) {
      note(E1->getLocStart(),
           "while converting function argument to expected type");
      return true;
    }
    
    E->Ty = FT->Result;
    return false;
  }
  
  // Otherwise, the function's type must be dependent.  If it is something else,
  // we have a type error.
  if (!E1->Ty->is<DependentType>()) {
    error(E1->getLocStart(), "called expression isn't a function");
    return true;
  }
  
  // Okay, if the argument is also dependent, we can't do anything here.  Just
  // wait until something gets resolved.
  if (E2->Ty->is<DependentType>()) {
    E->Ty = E2->Ty;
    return false;
  }
  
  // Okay, we have a typed argument and untyped function.  See if we can infer
  // a type for that function.
  
  // Otherwise, we must have an application to an overload set.  See if we can
  // resolve which overload member is based on the argument type.
  OverloadSetRefExpr *OS = dyn_cast<OverloadSetRefExpr>(E1);
  if (!OS) {
    E->Ty = E1->Ty;
    return false;
  }
  
  ValueDecl *BestCandidateFound = 0;
  Expr::ConversionRank BestRank = Expr::CR_Invalid;
  
  for (ValueDecl *Fn : OS->Decls) {
    Type ArgTy = Fn->Ty->getAs<FunctionType>()->Input;
    // If we found an exact match, disambiguate the overload set.
    Expr::ConversionRank Rank = E2->getRankOfConversionTo(ArgTy, Context);
    
    // If this conversion is worst than our best candidate, ignore it.
    if (Rank > BestRank)
      continue;
    
    // If this is better than our previous candidate, use it!
    if (Rank < BestRank) {
      BestRank = Rank;
      BestCandidateFound = Fn;
      continue;
    }
    
    // Otherwise, this is a repeat of an existing candidate with the same
    // rank.  This means that the candidates at this rank are ambiguous with
    // respect to each other, so none can be used.  If something comes along
    // with a lower rank we can use it though.
    BestCandidateFound = 0;
  }
  
  // If we found a successful match, resolve the overload set to it and continue
  // type checking.
  if (BestCandidateFound) {
    E1 = new (Context) DeclRefExpr(BestCandidateFound, OS->Loc,
                                   BestCandidateFound->Ty);
    return semaApplyExpr(E);
  }
  
  // Otherwise we have either an ambiguity between multiple possible candidates
  // or not candidate at all.
  if (BestRank != Expr::CR_Invalid)
    error(E1->getLocStart(), "overloading ambiguity found");
  else if (isa<BinaryExpr>(E))
    error(E1->getLocStart(), "no candidates found for binary operator");
  else if (isa<UnaryExpr>(E))
    error(E1->getLocStart(), "no candidates found for unary operator");
  else
    error(E1->getLocStart(), "no candidates found for call");
  
  // Print out the candidate set.
  for (auto TheDecl : OS->Decls) {
    Type ArgTy = TheDecl->Ty->getAs<FunctionType>()->Input;
    if (E2->getRankOfConversionTo(ArgTy, Context) != BestRank)
      continue;
    note(TheDecl->getLocStart(), "found this candidate");
  }
  return true;
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
class SemaExpressionTree : public ASTVisitor<SemaExpressionTree, Expr*, Stmt*> {
public:
  TypeChecker &TC;
  
  Expr *visitIntegerLiteralExpr(IntegerLiteralExpr *E) {
    return E;
  }
  Expr *visitDeclRefExpr(DeclRefExpr *E) {
    if (E->D == 0) {
      TC.error(E->Loc, "use of undeclared identifier");
      return 0;
    }
    
    // If the decl had an invalid type, then an error has already been emitted,
    // just propagate it up.
    if (E->D->Ty->is<ErrorType>())
      return 0;
    
    // TODO: QOI: If the decl had an "invalid" bit set, then return the error
    // object to improve error recovery.
    E->Ty = E->D->Ty;
    return E;
  }
  Expr *visitOverloadSetRefExpr(OverloadSetRefExpr *E) {
    E->Ty = DependentType::get(TC.Context);
    return E;
  }
  Expr *visitUnresolvedDeclRefExpr(UnresolvedDeclRefExpr *E) {
    assert(0 && "UnresolvedDeclRefExpr should be resolved by name binding!");
    return 0;
  }
  Expr *visitUnresolvedMemberExpr(UnresolvedMemberExpr *E) {
    E->Ty = DependentType::get(TC.Context);
    return E;
  }
  
  Expr *visitTupleExpr(TupleExpr *E) {
    if (TC.semaTupleExpr(E))
      return 0;
    return E;
  }
  Expr *visitUnresolvedDotExpr(UnresolvedDotExpr *E);
  
  Expr *visitUnresolvedScopedIdentifierExpr
  (UnresolvedScopedIdentifierExpr *E) {
    TypeBase *TypeScope =
    E->TypeDecl->UnderlyingTy->getCanonicalType(TC.Context);
    
    // Look through type aliases etc.
    OneOfType *DT = dyn_cast<OneOfType>(TypeScope);
    
    // Reject things like int::x.
    if (DT == 0) {
      TC.error(E->TypeDeclLoc, "invalid type '" + E->TypeDecl->Name.str() +
               "' for scoped access");
      return 0;
    }
    
    if (DT->Elements.empty()) {
      TC.error(E->TypeDeclLoc, "oneof '" + E->TypeDecl->Name.str() +
               "' is not complete or has no elements");
      return 0;
    }
    
    OneOfElementDecl *Elt = DT->getElement(E->Name);
    if (Elt == 0) {
      TC.error(E->NameLoc, "'" + E->Name.str() + "' is not a member of '" +
               E->TypeDecl->Name.str() + "'");
      return 0;
    }
    
    return visit(new (TC.Context) DeclRefExpr(Elt, E->TypeDeclLoc));
  }
  
  Expr *visitTupleElementExpr(TupleElementExpr *E) {
    // TupleElementExpr is fully resolved.
    assert(!E->Ty->is<DependentType>());
    return E;
  }
  
  Expr *visitTupleShuffleExpr(TupleShuffleExpr *E) {
    // TupleShuffleExpr is fully resolved.
    assert(!E->Ty->is<DependentType>());
    return E;
  }
  
  Expr *visitSequenceExpr(SequenceExpr *E);
  
  void PreProcessBraceStmt(BraceStmt *BS);
  
  Expr *visitFuncExpr(FuncExpr *E) {
    return E;
  }
  
  Expr *visitClosureExpr(ClosureExpr *E) {
    assert(0 && "Should not walk into ClosureExprs!");
    return 0;
  }
  
  Expr *visitAnonClosureArgExpr(AnonClosureArgExpr *E) {
    // Nothing we can do here.  These remain as resolved or unresolved as they
    // always were.  If no type is assigned, we give them a dependent type so
    // that we get resolution later.
    if (E->Ty.isNull())
      E->Ty = DependentType::get(TC.Context);
    return E;
  }
  
  
  Expr *visitApplyExpr(ApplyExpr *E) {
    return TC.semaApplyExpr(E) ? 0 : E;
  }
  Expr *visitCallExpr(CallExpr *E) { return visitApplyExpr(E); }
  Expr *visitUnaryExpr(UnaryExpr *E) { return visitApplyExpr(E); }
  Expr *visitBinaryExpr(BinaryExpr *E) { return visitApplyExpr(E); }
  
  Stmt *visitSemiStmt(SemiStmt *S) {
    return S;
  }
  
  Stmt *visitAssignStmt(AssignStmt *S) {
    // Coerce the source to the destination type.
    S->Src = TC.convertToType(S->Src, S->Dest->Ty);
    if (S->Src == 0) {
      TC.note(S->EqualLoc,
              "while converting assigned value to destination type");
      return 0;
    }
    
    return S;
  }
  
  Stmt *visitBraceStmt(BraceStmt *BS) {
    assert(0 && "BraceStmts should be processed in the prepass");
    return 0;
  }
  
  Stmt *visitReturnStmt(ReturnStmt *RS) {
    // FIXME: Convert the subexpr to the return type..
    return RS;
  }
  
  Stmt *visitIfStmt(IfStmt *IS) {
    // The if condition must have __builtin_int1 type.  This is after the
    // conversion function is added by sema.
    IS->Cond = TC.convertToType(IS->Cond, TC.Context.TheInt1Type);
    if (IS->Cond == 0)
      return 0;
    
    return IS;
  }
  
  Stmt *visitWhileStmt(WhileStmt *WS) {
    // The if condition must have __builtin_int1 type.  This is after the
    // conversion function is added by sema.
    WS->Cond = TC.convertToType(WS->Cond, TC.Context.TheInt1Type);
    if (WS->Cond == 0)
      return 0;
    
    return WS;
  }
  
  SemaExpressionTree(TypeChecker &tc) : TC(tc) {}
  

  Expr *doIt(Expr *E) {
    return E->walk(^Expr*(Expr *E, WalkOrder Order) {
      // This is implemented as a postorder walk.
      if (Order == WalkOrder::PreOrder) {
        // Do not walk into ClosureExpr.  Anonexprs within a nested closures
        // will have already been resolved, so we don't need to recurse into it.
        // This also prevents N^2 re-sema activity with lots of nested closures.
        if (isa<ClosureExpr>(E)) return 0;
        
        return E;
      }
      
      // Dispatch to the right visitor case in the post-order walk.  We know
      // that the operands have already been processed and are valid.
      return this->visit(E);
    }, ^Stmt*(Stmt *S, WalkOrder Order) {
      // This is implemented as a postorder walk.
      if (Order == WalkOrder::PreOrder) {
        // Do not descend into BraceStmt's, because we want to handle the var
        // initializers in a custom way.  Instead, just call visitBraceStmt in
        // the prepass (which itself manually descends) and then tell the walker
        // to not dive into it.
        if (BraceStmt *BS = dyn_cast<BraceStmt>(S)) {
          this->PreProcessBraceStmt(BS);
          return 0;
        }
        
        return S;
      }
      
      // Dispatch to the right visitor case in the post-order walk.  We know
      // that the operands have already been processed and are valid.
      return this->visit(S);
    });
  }
  
  Stmt *doIt(Stmt *S) {
    return S->walk(^Expr*(Expr *E, WalkOrder Order) {
      // This is implemented as a postorder walk.
      if (Order == WalkOrder::PreOrder) {
        // Do not walk into ClosureExpr.  Anonexprs within a nested closures
        // will have already been resolved, so we don't need to recurse into it.
        // This also prevents N^2 re-sema activity with lots of nested closures.
        if (isa<ClosureExpr>(E)) return 0;
        
        return E;
      }
      
      // Dispatch to the right visitor case in the post-order walk.  We know
      // that the operands have already been processed and are valid.
      return this->visit(E);
    }, ^Stmt*(Stmt *S, WalkOrder Order) {
      // This is implemented as a postorder walk.
      if (Order == WalkOrder::PreOrder) {
        // Do not descend into BraceStmt's, because we want to handle the var
        // initializers in a custom way.  Instead, just call visitBraceStmt in
        // the prepass (which itself manually descends) and then tell the walker
        // to not dive into it.
        if (BraceStmt *BS = dyn_cast<BraceStmt>(S)) {
          this->PreProcessBraceStmt(BS);
          return 0;
        }
        
        return S;
      }
      
      // Dispatch to the right visitor case in the post-order walk.  We know
      // that the operands have already been processed and are valid.
      return this->visit(S);
    });

  }
};
} // end anonymous namespace.

Expr *SemaExpressionTree::visitUnresolvedDotExpr(UnresolvedDotExpr *E) {
  // If the base expression hasn't been found yet, then we can't process this
  // value.
  if (E->SubExpr == 0) {
    E->Ty = DependentType::get(TC.Context);
    return E;
  }
  
  Type SubExprTy = E->SubExpr->Ty;
  if (SubExprTy->is<DependentType>())
    return E;
  
  // First, check to see if this is a reference to a field in the type.
  
  // If this is a member access to a oneof with a single element constructor,
  // allow direct access to the type underlying the single element.  This
  // allows element access on structs, for example.
  if (OneOfType *DT = SubExprTy->getAs<OneOfType>())
    if (DT->hasSingleElement())
      SubExprTy = DT->Elements[0]->ArgumentType;
  
  if (TupleType *TT = SubExprTy->getAs<TupleType>()) {
    // If the field name exists, we win.
    int FieldNo = TT->getNamedElementId(E->Name);
    if (FieldNo != -1)
      return new (TC.Context) 
      TupleElementExpr(E->SubExpr, E->DotLoc, FieldNo, E->NameLoc,
                       TT->getElementType(FieldNo));
    
    // Okay, the field name was invalid.  If this is a dollarident like $4,
    // process it as a field index.
    if (E->Name.str().startswith("$")) {
      unsigned Value = 0;
      if (!E->Name.str().substr(1).getAsInteger(10, Value)) {
        if (Value >= TT->Fields.size()) {
          TC.error(E->NameLoc, "field number is too large for tuple");
          return 0;
        }
        
        return new (TC.Context) 
        TupleElementExpr(E->SubExpr, E->DotLoc, FieldNo, E->NameLoc,
                         TT->getElementType(Value));
      }
    }
  }
  
  // Next, check to see if "a.f" is actually being used as sugar for "f a",
  // which is a function application of 'a' to 'f'.
  if (!E->ResolvedDecls.empty()) {
    assert(E->ResolvedDecls[0]->Ty->is<FunctionType>() &&
           "Should have only bound to functions");
    Expr *FnRef;
    // Apply the base value to the function there is a single candidate in the
    // set then this is directly resolved, otherwise it is an overload case..
    if (E->ResolvedDecls.size() == 1)
      FnRef = new (TC.Context) DeclRefExpr(E->ResolvedDecls[0], E->NameLoc,
                                           E->ResolvedDecls[0]->Ty);
    else
      FnRef = new (TC.Context) OverloadSetRefExpr(E->ResolvedDecls, E->NameLoc,
                                                  DependentType::get(TC.Context));
    
    CallExpr *Call = new (TC.Context) CallExpr(FnRef, E->SubExpr, Type());
    if (TC.semaApplyExpr(Call))
      return 0;
    
    return Call;
  }
  
  // TODO: Otherwise, do an argument dependent lookup in the namespace of the
  // base type.
  
  TC.error(E->DotLoc, "base type '" + SubExprTy->getString() + 
           "' has no valid '.' expression for this field");
  return 0;
}


/// getBinOp - If the specified expression is an infix binary operator, return
/// its precedence, otherwise return -1.
static int getBinOp(Expr *E, TypeChecker &TC) {
  if (DeclRefExpr *DRE = dyn_cast<DeclRefExpr>(E))
    return DRE->D->Attrs.InfixPrecedence;
  
  // If this is an overload set, the entire overload set is required to have the
  // same precedence level.
  if (OverloadSetRefExpr *OO = dyn_cast<OverloadSetRefExpr>(E)) {
    int Precedence = -1;
    for (auto D : OO->Decls) {
      // It is possible some unary operators got mixed into the overload set.
      if (D->Attrs.InfixPrecedence == -1)
        continue;
      
      if (Precedence != -1 && Precedence != D->Attrs.InfixPrecedence) {
        TC.error(E->getLocStart(),
                 "binary operator with multiple overloads of disagreeing precedence found");
        return -2;
      }
      
      Precedence = D->Attrs.InfixPrecedence;
    }
    
    return Precedence;
  }
  
  // Not a binary operator.
  return -1;
}

/// ReduceBinaryExprs - We have a sequence of unary expressions;  parse
/// it, using binary precedence rules, into a sequence of binary operators.
static Expr *ReduceBinaryExprs(SequenceExpr *E, unsigned &Elt,
                               TypeChecker &TC, unsigned MinPrec = 0) {
  Expr *LHS = E->Elements[Elt];
  
  // The parser only produces well-formed sequences.
  
  while (Elt+1 != E->NumElements) {
    assert(Elt+2 < E->NumElements);
    
    Expr *LeftOperator = E->Elements[Elt+1];
    int LeftPrecedence = getBinOp(LeftOperator, TC);
    if (LeftPrecedence < (int)MinPrec) {
      if (LeftPrecedence == -1)
        TC.error(LeftOperator->getLocStart(),
                 "operator is not a binary operator");
      return 0;
    }
    
    Elt += 2;
    Expr *RHS = E->Elements[Elt];
    
    // Get the precedence of the operator immediately to the right of the RHS
    // of the RHS of the binop (if present).  This is looking for the multiply
    // in "x+y*z".
    if (Elt+1 != E->NumElements) {
      assert(Elt+2 < E->NumElements);
      Expr *RightOperator = E->Elements[Elt+1];
      int RightPrecedence = getBinOp(RightOperator, TC);
      if (RightPrecedence == -2) return 0;
      
      // TODO: All operators are left associative at the moment.
      
      // If the next operator binds more tightly with RHS than we do,
      // evaluate the RHS as a complete subexpression first.  This
      // happens with "x+y*z", where we want to reduce y*z to a single
      // sub-expression of the add.
      if (LeftPrecedence < RightPrecedence) {
        RHS = ReduceBinaryExprs(E, Elt, TC, (unsigned) (LeftPrecedence+1));
        if (!RHS) return 0;
      } else if (RightPrecedence == -1) {
        TC.error(RightOperator->getLocStart(),
                 "operator is not a binary operator");
      }
    }
    
    // Okay, we've finished the parse, form the AST node for the binop now.
    Expr *ArgElts[] = { LHS, RHS };
    Expr **ArgElts2 = TC.Context.AllocateCopy<Expr*>(ArgElts, ArgElts+2);
    TupleExpr *Arg = new (TC.Context) TupleExpr(LHS->getLocStart(), ArgElts2, 0,
                                                2, SMLoc(), false);
    BinaryExpr *RBE = new (TC.Context) BinaryExpr(LeftOperator, Arg);
    if (TC.semaApplyExpr(RBE))
      return 0;
    
    LHS = RBE;
  }
  
  return LHS;
}

Expr *SemaExpressionTree::visitSequenceExpr(SequenceExpr *E) {
  unsigned i = 0;
  return ReduceBinaryExprs(E, i, TC);
}


void SemaExpressionTree::PreProcessBraceStmt(BraceStmt *BS) {
  SmallVector<BraceStmt::ExprStmtOrDecl, 32> NewElements;
  
  // Braces have to manually walk into subtrees for expressions, because we
  // terminate the walk we're in for them (so we can handle decls custom).
  for (unsigned i = 0, e = BS->NumElements; i != e; ++i) {
    if (Expr *SubExpr = BS->Elements[i].dyn_cast<Expr*>()) {
      if ((SubExpr = doIt(SubExpr)) == 0)
        continue;
      
      // Otherwise it is a normal expression or an as-yet-unresolved sequence.
      NewElements.push_back(SubExpr);
      continue;
    }
    
    if (Stmt *SubStmt = BS->Elements[i].dyn_cast<Stmt*>()) {
      if ((SubStmt = doIt(SubStmt)))
        NewElements.push_back(SubStmt);
      continue;
    }
    
    Decl *D = BS->Elements[i].get<Decl*>();
    NewElements.push_back(D);
    
    if (TypeAliasDecl *TAD = dyn_cast<TypeAliasDecl>(D))
      TC.typeCheck(TAD);
    
    if (ValueDecl *VD = dyn_cast<ValueDecl>(D))
      TC.typeCheck(VD);
  }
  
  // If any of the elements of the braces has a function type (which indicates
  // that a function didn't get called), then produce an error.
  // TODO: What about tuples which contain functions by-value that are dead?
  for (unsigned i = 0, e = NewElements.size(); i != e; ++i)
    if (NewElements[i].is<Expr*>() &&
        NewElements[i].get<Expr*>()->Ty->is<FunctionType>())
      // TODO: QOI: Add source range.
      TC.error(NewElements[i].get<Expr*>()->getLocStart(),
               "expression resolves to an unevaluated function");
  
  // Reinstall the list now that we potentially mutated it.
  if (NewElements.size() <= BS->NumElements) {
    memcpy(BS->Elements, NewElements.data(),
           NewElements.size()*sizeof(BS->Elements[0]));
    BS->NumElements = NewElements.size();
  } else {
    BS->Elements = 
    TC.Context.AllocateCopy<BraceStmt::ExprStmtOrDecl>(NewElements.begin(),
                                                       NewElements.end());
    BS->NumElements = NewElements.size();
  }
}


Expr *TypeChecker::typeCheckExpression(Expr *E) {
  SemaExpressionTree SET(*this);
  return SET.doIt(E);
}

void TypeChecker::typeCheckTranslationUnit(TranslationUnitDecl *TUD) {
  SemaExpressionTree SET(*this);
  SET.PreProcessBraceStmt(TUD->Body);
}
