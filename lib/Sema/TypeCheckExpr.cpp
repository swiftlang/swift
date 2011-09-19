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
    TE->setType(TE->SubExprs[0]->getType());
    return false;
  }
  
  // Compute the result type.
  SmallVector<TupleTypeElt, 8> ResultTyElts(TE->NumSubExprs);
  
  for (unsigned i = 0, e = TE->NumSubExprs; i != e; ++i) {
    // If the element value is missing, it has the value of the default
    // expression of the result type, which must be known.
    if (TE->SubExprs[i] == 0) {
      assert(TE->getType() && isa<TupleType>(TE->getType()) && 
             "Can't have default value without a result type");
      
      ResultTyElts[i].Ty =
        cast<TupleType>(TE->getType())->getElementType(i);
      
      // FIXME: What about a default value that is dependent?
      if (ResultTyElts[i].Ty->is<DependentType>()) {
        TE->setType(ResultTyElts[i].Ty);
        return false;
      }
    } else {
      // If any of the tuple element types is dependent, the whole tuple should
      // have dependent type.
      if (TE->SubExprs[i]->getType()->is<DependentType>()) {
        TE->setType(TE->SubExprs[i]->getType());
        return false;
      }
      
      ResultTyElts[i].Ty = TE->SubExprs[i]->getType();
    }
    
    // If a name was specified for this element, use it.
    if (TE->SubExprNames)
      ResultTyElts[i].Name = TE->SubExprNames[i];
  }
  
  TE->setType(TupleType::get(ResultTyElts, Context));
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
  if (FunctionType *FT = E1->getType()->getAs<FunctionType>()) {
    // If this is an operator, make sure that the declaration found was declared
    // as such.
    if (isa<UnaryExpr>(E) && !cast<DeclRefExpr>(E1)->D->Name.isOperator()) {
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
    
    E->setType(FT->Result);
    return false;
  }
  
  // Otherwise, the function's type must be dependent.  If it is something else,
  // we have a type error.
  if (!E1->getType()->is<DependentType>()) {
    error(E1->getLocStart(), "called expression isn't a function");
    return true;
  }
  
  // Okay, if the argument is also dependent, we can't do anything here.  Just
  // wait until something gets resolved.
  if (E2->getType()->is<DependentType>()) {
    E->setType(E2->getType());
    return false;
  }
  
  // Okay, we have a typed argument and untyped function.  See if we can infer
  // a type for that function.
  
  // Otherwise, we must have an application to an overload set.  See if we can
  // resolve which overload member is based on the argument type.
  OverloadSetRefExpr *OS = dyn_cast<OverloadSetRefExpr>(E1);
  if (!OS) {
    E->setType(E1->getType());
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
class SemaExpressionTree : public ExprVisitor<SemaExpressionTree, Expr*> {
public:
  TypeChecker &TC;
  
  Expr *visitIntegerLiteralExpr(IntegerLiteralExpr *E) {
    return E;
  }
  Expr *visitFloatLiteralExpr(FloatLiteralExpr *E) {
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
    E->setType(E->D->Ty);
    return E;
  }
  Expr *visitOverloadSetRefExpr(OverloadSetRefExpr *E) {
    E->setType(DependentType::get(TC.Context));
    return E;
  }
  Expr *visitUnresolvedDeclRefExpr(UnresolvedDeclRefExpr *E) {
    llvm_unreachable("name binding should resolve all UnresolvedDeclRefExprs!");
    return 0;
  }
  Expr *visitUnresolvedMemberExpr(UnresolvedMemberExpr *E) {
    E->setType(DependentType::get(TC.Context));
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
    llvm_unreachable("UnresolvedScopedIdentifierExpr should be resolved "
                     "by name binding!");
  }

  Expr *visitTupleElementExpr(TupleElementExpr *E) {
    // TupleElementExpr is fully resolved.
    assert(!E->getType()->is<DependentType>());
    return E;
  }
  
  Expr *visitTupleShuffleExpr(TupleShuffleExpr *E) {
    // TupleShuffleExpr is fully resolved.
    assert(!E->getType()->is<DependentType>());
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
    if (E->getType().isNull())
      E->setType(DependentType::get(TC.Context));
    return E;
  }
  
  
  Expr *visitApplyExpr(ApplyExpr *E) {
    return TC.semaApplyExpr(E) ? 0 : E;
  }
  Expr *visitCallExpr(CallExpr *E) { return visitApplyExpr(E); }
  Expr *visitUnaryExpr(UnaryExpr *E) { return visitApplyExpr(E); }
  Expr *visitBinaryExpr(BinaryExpr *E) { return visitApplyExpr(E); }
  Expr *visitProtocolElementExpr(ProtocolElementExpr *E) {
    return visitApplyExpr(E);
  }

  
  SemaExpressionTree(TypeChecker &tc) : TC(tc) {}
  

  Expr *doIt(Expr *E) {
    return E->walk(^Expr*(Expr *E, WalkOrder Order) {
      // This is implemented as a postorder walk.
      if (Order == WalkOrder::PreOrder) {
        // Do not walk into ClosureExpr.  Anonexprs within a nested closures
        // will have already been resolved, so we don't need to recurse into it.
        // This also prevents N^2 re-sema activity with lots of nested closures.
        if (isa<ClosureExpr>(E) || isa<FuncExpr>(E)) return 0;
        
        return E;
      }
      
      // Dispatch to the right visitor case in the post-order walk.  We know
      // that the operands have already been processed and are valid.
      return this->visit(E);
    }, ^Stmt*(Stmt *S, WalkOrder Order) {
      // Never recurse into statements.
      return 0;
    });
  }
};
} // end anonymous namespace.

Expr *SemaExpressionTree::visitUnresolvedDotExpr(UnresolvedDotExpr *E) {
  // If the base expression hasn't been found yet, then we can't process this
  // value.
  if (E->SubExpr == 0) {
    E->setType(DependentType::get(TC.Context));
    return E;
  }
  
  Type SubExprTy = E->SubExpr->getType();
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
  
  // Check in the context of a protocol.
  if (ProtocolType *PT = SubExprTy->getAs<ProtocolType>()) {
    for (ValueDecl *VD : PT->Elements) {
      if (VD->Name == E->Name) {
        // The protocol value is applied via a DeclRefExpr.
        Expr *Fn = new (TC.Context) DeclRefExpr(VD, E->NameLoc, VD->Ty);
        
        ApplyExpr *Call = new (TC.Context) 
          ProtocolElementExpr(Fn, E->DotLoc, E->SubExpr);
        if (TC.semaApplyExpr(Call))
          return 0;
        return Call;
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
                 "binary operator with multiple overloads of disagreeing "
                 "precedence found");
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

bool TypeChecker::typeCheckExpression(Expr *&E, Type ConvertType) {
  SemaExpressionTree SET(*this);
  E = SET.doIt(E);
  
  // If our context specifies a type, apply it to the expression.
  if (E && ConvertType)
    E = convertToType(E, ConvertType);
  
  if (E == 0) return true;
  
  // Check the initializer/body to make sure that we succeeded in resolving
  // all of the types contained within it.  We should not have any
  // DependentType's left for subexpressions.
  E = E->walk(^(Expr *E, WalkOrder Order) {
    // Ignore the preorder walk.  We'd rather diagnose use of unresolved types
    // during the postorder walk so that the inner most expressions are 
    // diagnosed before the outermost ones.
    if (Order == WalkOrder::PreOrder)
      return E;
    
    assert(!isa<SequenceExpr>(E) && "Should have resolved this");
    
    // Use is to strip off sugar.
    if (!E->getType()->is<DependentType>())
      return E;
    
    error(E->getLocStart(),
          "ambiguous expression was not resolved to a concrete type");
    return 0;
  }, ^Stmt*(Stmt *S, WalkOrder Order) {
    // Never recurse into statements.
    return 0;
  });

  
  return E == 0;
}
