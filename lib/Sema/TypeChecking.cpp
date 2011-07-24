//===--- TypeChecking.cpp - Type Checking ---------------------------------===//
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
// This file implements semantic analysis for expressions, and other pieces
// that require final type checking.  If this passes a translation unit with no
// errors, then it is good to go.
//
//===----------------------------------------------------------------------===//

// FIXME: Entrypoint declared in Parser.h
#include "swift/Parse/Parser.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/AST/ExprVisitor.h"
#include "swift/AST/Types.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/ADT/Twine.h"
#include "llvm/Support/SourceMgr.h"
using namespace swift;


namespace {
  class TypeChecker {
  public:
    ASTContext &Context;
    TypeChecker(ASTContext &C) : Context(C) {}
    
    void note(SMLoc Loc, const Twine &Message) {
      Context.SourceMgr.PrintMessage(Loc, Message, "note");
    }
    void warning(SMLoc Loc, const Twine &Message) {
      Context.SourceMgr.PrintMessage(Loc, Message, "warning");
    }
    void error(SMLoc Loc, const Twine &Message) {
      Context.setHadError();
      Context.SourceMgr.PrintMessage(Loc, Message, "error");
    }
    
    bool validateType(Type &T);

    void typeCheck(TypeAliasDecl *TAD);

    void typeCheck(ValueDecl *VD, SmallVectorImpl<Expr*> &ExcessExprs) {
      // No types to resolved for a ElementRefDecl.
      if (ElementRefDecl *ERD = dyn_cast<ElementRefDecl>(VD))
        return typeCheckERD(ERD);
      if (VarDecl *Var = dyn_cast<VarDecl>(VD))
        return typeCheckVarDecl(Var, ExcessExprs);
      
      if (isa<OneOfElementDecl>(VD))
        return;  // FIXME: No type checking required for this?
      
      typeCheckValueDecl(cast<ValueDecl>(VD), ExcessExprs);
    }
    
    void typeCheckERD(ElementRefDecl *ERD);
    void typeCheckVarDecl(VarDecl *VD, SmallVectorImpl<Expr*> &ExcessExprs);
    bool typeCheckValueDecl(ValueDecl *VD,
                            SmallVectorImpl<Expr*> &ExcessExprs);
    
    void validateAttributes(DeclAttributes &Attrs, Type Ty);
    
    bool validateVarName(Type Ty, DeclVarName *Name);

    /// checkBody - Type check an expression that is used in a top-level
    /// context like a var/func body, or tuple default value.  If DestTy is
    /// specified, the expression is coerced to the requested type.
    ///
    /// If the body turns out to be a sequence, this returns the single element
    /// with the excess in the provided SmallVector.  If the SmallVector is not
    /// provided, errors are emitted for the excess expressions.
    void checkBody(Expr *&E, Type DestTy,
                   SmallVectorImpl<Expr*> *ExcessElements);
    

    /// convertToType - Do semantic analysis of an expression in a context that
    /// expects a particular type.  This performs a conversion to that type if
    /// the types don't match and diagnoses cases where the conversion cannot be
    /// performed.
    ///
    /// This emits a diagnostic and returns null on error.
    Expr *convertToType(Expr *E, Type Ty);
  };
}  // end anonymous namespace


//===----------------------------------------------------------------------===//
// Expression Semantic Analysis Routines
//===----------------------------------------------------------------------===//

// Each expression node has an implementation here, which takes the properties
// of the expression, validates it and computes a result type.  This is shared
// logic between the Sema Action implementations and type inferencing code.
//
// These each produce diagnostics and return true on error.  On success, they
// may mutate the input values, then return false.

static bool SemaDeclRefExpr(ValueDecl *D, SMLoc Loc, Type &ResultTy,
                            TypeChecker &TC) {
  if (D == 0) {
    TC.error(Loc, "use of undeclared identifier");
    return true;
  }
  
  // TODO: QOI: If the decl had an "invalid" bit set, then return the error
  // object to improve error recovery.
  
  ResultTy = D->Ty;
  return false;
}

static bool SemaTupleExpr(TupleExpr *TE, TypeChecker &TC) {
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
  
  TE->Ty = TupleType::get(ResultTyElts, TC.Context);
  return false;
}

static bool SemaApplyExpr(Expr *&E1, Expr *&E2, Type &ResultTy,
                          TypeChecker &TC) {
  // If we have a concrete function type, then we win.
  if (FunctionType *FT = E1->Ty->getAs<FunctionType>()) {
    // We have a function application.  Check that the argument type matches the
    // expected type of the function.
    E2 = TC.convertToType(E2, FT->Input);
    if (E2 == 0) {
      TC.note(E1->getLocStart(),
              "while converting function argument to expected type");
      return true;
    }  
    ResultTy = FT->Result;
    return false;
  }
  
  // Otherwise, we must have an application to an overload set.  See if we can
  // resolve which overload member is based on the argument type.
  if (!E2->Ty->is<DependentType>()) {
    OverloadSetRefExpr *OS = cast<OverloadSetRefExpr>(E1);
    int BestCandidateFound = -1;
    Expr::ConversionRank BestRank = Expr::CR_Invalid;
    
    for (unsigned i = 0, e = OS->Decls.size(); i != e; ++i) {
      Type ArgTy = OS->Decls[i]->Ty->getAs<FunctionType>()->Input;
      // If we found an exact match, disambiguate the overload set.
      Expr::ConversionRank Rank =
        E2->getRankOfConversionTo(ArgTy, TC.Context);
      
      // If this conversion is worst than our best candidate, ignore it.
      if (Rank > BestRank)
        continue;
      
      // If this is better than our previous candidate, use it!
      if (Rank < BestRank) {
        BestRank = Rank;
        BestCandidateFound = i;
        continue;
      }
      
      // Otherwise, this is a repeat of an existing candidate with the same
      // rank.  This means that the candidates at this rank are ambiguous with
      // respect to each other, so none can be used.  If something comes along
      // with a lower rank we can use it though.
      BestCandidateFound = -1;
    }
    
    if (BestCandidateFound != -1) {
      E1 = new (TC.Context) DeclRefExpr(OS->Decls[BestCandidateFound], OS->Loc,
                                        OS->Decls[BestCandidateFound]->Ty);
      return SemaApplyExpr(E1, E2, ResultTy, TC);
    }
    
    // FIXME: Emit an error here if we have an overload resolution failure.
  }
  
  // Otherwise, we can't resolve the argument type yet.
  ResultTy = E2->Ty;
  return false;
}

/// SemaSequenceExpr - Perform semantic analysis for sequence expressions.
static bool SemaSequenceExpr(Expr **Elements, unsigned NumElements,
                             Type &ResultTy, TypeChecker &TC) {
  // If any of the operands of the sequence have a dependent type, then so does
  // the sequence.  Dependent values can be resolved to function types, so the
  // last value of the sequence may be an operand to the function, not the
  // result of the sequence.
  for (unsigned i = 0; i != NumElements; ++i)
    if (Elements[i]->Ty->is<DependentType>()) {
      ResultTy = Elements[i]->Ty;
      return false;
    }
  
  ResultTy = Elements[NumElements-1]->Ty;
  return false;
}

/// SemaBinaryExpr - Perform semantic analysis of binary expressions.
/// OpFn is null if this is an assignment (FIXME: we don't have generics yet).
static bool SemaBinaryExpr(Expr *&LHS, Expr *&OpFn, Expr *&RHS, Type &ResultTy,
                           TypeChecker &TC) {
  // If this is an assignment, then we coerce the RHS to the LHS.
  if (OpFn == 0) {
    RHS = TC.convertToType(RHS, LHS->Ty);
    if (LHS == 0) {
      TC.note(RHS->getLocStart(),
              "while converting assigned value to destination type");
      return true; 
    }
    
    ResultTy = TupleType::getEmpty(TC.Context);
    return false;
  }

  // If this is an overloaded binary operator, try to resolve which candidate
  // is the right one.
  if (OverloadSetRefExpr *OO = dyn_cast<OverloadSetRefExpr>(OpFn)) {
    // Pick the best candidate according to their conversion rank.
    int BestCandidateFound = -1;
    Expr::ConversionRank BestRank = Expr::CR_Invalid;
    
    for (unsigned i = 0, e = OO->Decls.size(); i != e; ++i) {
      ValueDecl *Fn = OO->Decls[i];
      
      FunctionType *FnTy = cast<FunctionType>(Fn->Ty.getPointer());
      TupleType *Input = cast<TupleType>(FnTy->Input.getPointer());
      assert(Input->Fields.size() == 2 &&"Sema error validating infix fn type");

      Expr::ConversionRank Rank =
        std::max(LHS->getRankOfConversionTo(Input->getElementType(0),
                                            TC.Context),
                 RHS->getRankOfConversionTo(Input->getElementType(1),
                                            TC.Context));
      
      // If this conversion is worst than our best candidate, ignore it.
      if (Rank > BestRank)
        continue;
      
      // If this is better than our previous candidate, use it!
      if (Rank < BestRank) {
        BestRank = Rank;
        BestCandidateFound = i;
        continue;
      }
      
      // Otherwise, this is a repeat of an existing candidate with the same
      // rank.  This means that the candidates at this rank are ambiguous with
      // respect to each other, so none can be used.  If something comes along
      // with a lower rank we can use it though.
      BestCandidateFound = -1;
    }
    
    if (BestCandidateFound != -1) {
      ValueDecl *Fn = OO->Decls[BestCandidateFound];
      OpFn = new (TC.Context) DeclRefExpr(Fn, OO->Loc, Fn->Ty);
      return SemaBinaryExpr(LHS, OpFn, RHS, ResultTy, TC);
    }
    
    // FIXME: Emit an error about overload resolution failure.
    ResultTy = DependentType::get(TC.Context);
    return false;
  }
  
  // Parser verified that OpFn has an Infix Precedence.  Sema verified that OpFn
  // only has InfixPrecedence if it takes a 2 element tuple as input.
  FunctionType *FnTy = cast<FunctionType>(OpFn->Ty.getPointer());
  TupleType *Input = cast<TupleType>(FnTy->Input.getPointer());
  assert(Input->Fields.size() == 2 && "Sema error validating infix fn type");
  
  // Verify that the LHS/RHS have the right type and do conversions as needed.
  LHS = TC.convertToType(LHS, Input->getElementType(0));
  if (LHS == 0) {
    TC.note(OpFn->getLocStart(),
            "while converting left side of binary operator to expected type");
    return true;
  }
  
  RHS = TC.convertToType(RHS, Input->getElementType(1));
  if (RHS == 0) {
    TC.note(OpFn->getLocStart(),
            "while converting right side of binary operator to expected type");
    return true;
  }
  
  ResultTy = FnTy->Result;
  return false;
}


//===----------------------------------------------------------------------===//
// Expression Reanalysis - SemaExpressionTree
//===----------------------------------------------------------------------===//

namespace {
  /// SemaExpressionTree - This class implements top-down (aka "leaf to root",
  /// analyzing 1 and 4 before the + in "1+4") semantic analysis of an
  /// already-existing expression tree.  This is performed when a closure is
  /// formed and anonymous decls like "_4" get a concrete type associated with
  /// them.  During the initial parse, these decls get a 'dependent' type, which
  /// disables most semantic analysis associated with them.
  ///
  /// When the expression tree is bound to a context, the anonymous decls get a
  /// concrete type and we have to rescan the tree to assign types to
  /// intermediate nodes, introduce type coercion etc.  This visitor does this
  /// job.  Each visit method reanalyzes the children of a node, then reanalyzes
  /// the node, and returns true on error.
  class SemaExpressionTree : public ExprVisitor<SemaExpressionTree, Expr*> {
    friend class ExprVisitor<SemaExpressionTree, Expr*>;
    TypeChecker &TC;
    
    Expr *VisitIntegerLiteralExpr(IntegerLiteralExpr *E) {
      return E;
    }
    Expr *VisitDeclRefExpr(DeclRefExpr *E) {
      if (SemaDeclRefExpr(E->D, E->Loc, E->Ty, TC)) return 0;
      return E;
    }
    Expr *VisitOverloadSetRefExpr(OverloadSetRefExpr *E) {
      E->Ty = DependentType::get(TC.Context);
      return E;
    }
    Expr *VisitUnresolvedDeclRefExpr(UnresolvedDeclRefExpr *E) {
      assert(0 && "UnresolvedDeclRefExpr should be resolved by name binding!");
      return 0;
    }
    Expr *VisitUnresolvedMemberExpr(UnresolvedMemberExpr *E) {
      E->Ty = DependentType::get(TC.Context);
      return E;
    }
    
    Expr *VisitTupleExpr(TupleExpr *E) {
      if (SemaTupleExpr(E, TC))
        return 0;
      return E;
    }
    Expr *VisitUnresolvedDotExpr(UnresolvedDotExpr *E);
    
    Expr *VisitUnresolvedScopedIdentifierExpr
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
      
      return Visit(new (TC.Context) DeclRefExpr(Elt, E->TypeDeclLoc));
    }
    
    Expr *VisitTupleElementExpr(TupleElementExpr *E) {
      // TupleElementExpr is fully resolved.
      assert(!E->Ty->is<DependentType>());
      return E;
    }
    
    Expr *VisitTupleShuffleExpr(TupleShuffleExpr *E) {
      // TupleShuffleExpr is fully resolved.
      assert(!E->Ty->is<DependentType>());
      return E;
    }
    
    Expr *VisitApplyExpr(ApplyExpr *E) {
      if (SemaApplyExpr(E->Fn, E->Arg, E->Ty, TC))
        return 0;
      return E;
    }
    Expr *VisitSequenceExpr(SequenceExpr *E);
    
    Expr *VisitBraceExpr(BraceExpr *E) {
      assert(0 && "BraceExprs should be processed in the prepass");
      return 0;
    }
    void PreProcessBraceExpr(BraceExpr *E);
    
    Expr *VisitClosureExpr(ClosureExpr *E) {
      assert(0 && "Should not walk into ClosureExprs!");
      return 0;
    }
    
    Expr *VisitAnonClosureArgExpr(AnonClosureArgExpr *E) {
      // Nothing we can do here.  These remain as resolved or unresolved as they
      // always were.  If no type is assigned, we give them a dependent type so
      // that we get resolution later.
      if (E->Ty.isNull())
        E->Ty = DependentType::get(TC.Context);
      return E;
    }
    
    Expr *VisitBinaryExpr(BinaryExpr *E) {
      if (SemaBinaryExpr(E->LHS, E->Fn, E->RHS, E->Ty, TC))
        return 0;
      
      return E;
    }
    
    Expr *VisitIfExpr(IfExpr *E);
    
    SemaExpressionTree(TypeChecker &tc) : TC(tc) {}
    
    static Expr *WalkFn(Expr *E, Expr::WalkOrder Order, void *set) {
      SemaExpressionTree &SET = *static_cast<SemaExpressionTree*>(set);
      // This is implemented as a postorder walk.
      if (Order == Expr::WalkOrder::PreOrder) {
        // Do not walk into ClosureExpr's.  Anonexprs within a nested closure
        // will have already been resolved, so we don't need to recurse into it.
        // This also prevents N^2 re-sema activity with lots of nested closures.
        if (isa<ClosureExpr>(E)) return 0;
        
        // Do not descend into BraceExpr's, because we want to handle the var
        // initializers in a custom way.  Instead, just call VisitBraceExpr in
        // the prepass (which itself manually descends) and then tell the walker
        // to not dive into it.
        if (BraceExpr *BE = dyn_cast<BraceExpr>(E)) {
          SET.PreProcessBraceExpr(BE);
          return 0;
        }
        
        return E;
      }
      
      // Dispatch to the right visitor case in the post-order walk.  We know
      // that the operands have already been processed and are valid.
      return SET.Visit(E);
    }

    Expr *doIt(Expr *E) {
      return E->WalkExpr(WalkFn, this);
    }
    
  public:
    static Expr *doIt(Expr *E, TypeChecker &TC) {
      SemaExpressionTree SET(TC);
      return SET.doIt(E);
    }
  };
} // end anonymous namespace.

Expr *SemaExpressionTree::VisitUnresolvedDotExpr(UnresolvedDotExpr *E) {
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
        
    Type ResultTy;
    if (SemaApplyExpr(FnRef, E->SubExpr, ResultTy, TC))
      return 0;
    
    return new (TC.Context) ApplyExpr(FnRef, E->SubExpr, ResultTy);
  }
  
  // TODO: Otherwise, do an argument dependent lookup in the namespace of the
  // base type.
  
  TC.error(E->DotLoc, "base type '" + SubExprTy->getString() + 
           "' has no valid '.' expression for this field");
  return 0;
}


/// getBinOp - If the specified expression is an infix binary operator, return
/// its precedence, otherwise return -1.
static int getBinOp(SequenceExpr *E, unsigned Elt) {
  if (Elt >= E->NumElements) return -1;
  
  if (DeclRefExpr *DRE = dyn_cast<DeclRefExpr>(E->Elements[Elt]))
    return DRE->D->Attrs.InfixPrecedence;
  
  // If this is an overload set, the entire overload set is known to have the
  // same precedence level.
  if (OverloadSetRefExpr *OO = dyn_cast<OverloadSetRefExpr>(E->Elements[Elt]))
    return OO->Decls[0]->Attrs.InfixPrecedence;
  
  // Not a binary operator.
  return -1;
}

static void ReduceJuxtaposedExprs(SequenceExpr *E, unsigned Elt,
                                  TypeChecker &TC);


/// ReduceBinaryExprs - If the specified element is the start of a binary
/// expression sequence, form the binary expression, reducing it down into a
/// single expression.
static void ReduceBinaryExprs(SequenceExpr *E, unsigned Elt,
                              TypeChecker &TC, unsigned MinPrec = 0) {
  
  // If the expression we're looking at is that start of a juxtaposed sequence,
  // reduce it down to a single value first.
  ReduceJuxtaposedExprs(E, Elt, TC);
  
  while (1) {
    int ThisPrec = getBinOp(E, Elt+1);
    if (ThisPrec < (int)MinPrec)
      break;
    
    // TODO: Support ternary operators some day.
    
    // Get the next expression, which is the RHS of the binary operator.
    if (Elt+2 == E->NumElements) {
      TC.error(E->Elements[Elt+1]->getLocStart(),
               "expected expression after binary operator");
      --E->NumElements;  // Discard the binary operator.
      break;
    }
    
    // If the RHS is the start of a juxtaposed sequence, reduce it down to a
    // single expression.
    ReduceJuxtaposedExprs(E, Elt+2, TC);
    
    // Get the precedence of the operator immediately to the right of the RHS
    // of the RHS of the binop (if present).  This is looking for the multiply
    // in "x+y*z".
    int NextPrec = getBinOp(E, Elt+3);
    
    // TODO: All operators are left associative at the moment.
    
    // If the next operator binds more tightly with RHS than we do, evaluate the
    // RHS as a complete subexpression first.  This happens with "x+y*z", where
    // we want to reduce y*z to a single sub-expression of the add.
    if (ThisPrec < NextPrec) {
      // Only parse things on the RHS that bind more tightly than the current
      // operator.
      ReduceBinaryExprs(E, Elt+2, TC, ThisPrec+1);
      
      NextPrec = getBinOp(E, Elt+3);
    }
    assert(NextPrec <= ThisPrec && "Recursion didn't work!");

    // Okay, we've finished the parse, form the AST node for the binop now.
    BinaryExpr *RBE =
      new (TC.Context) BinaryExpr(E->Elements[Elt], E->Elements[Elt+1],
                                  E->Elements[Elt+2]);
    if (!SemaBinaryExpr(RBE->LHS, RBE->Fn, RBE->RHS, RBE->Ty, TC))
      E->Elements[Elt] = RBE;
    
    // The binop and the RHS are now consumed, move everything down.
    memmove(E->Elements+Elt+1, E->Elements+Elt+3, 
            (E->NumElements-Elt-3)*sizeof(E->Elements[0]));
    E->NumElements -= 2;
  }
}

/// isKnownToBeAFunction - return true if this expression is known to be a
/// function, and therefore allows application of a value to it.  This controls
/// "parsing" of SequenceExprs into their underlying expression.
static bool isKnownToBeAFunction(Expr *E) {
  // If the expression has function type, then it's clearly a function.
  if (E->Ty->is<FunctionType>())
    return true;
  
  // If the expression is an overload set and all members are functions, then
  // clearly we have a function!
  if (OverloadSetRefExpr *OSRE = dyn_cast<OverloadSetRefExpr>(E)) {
    for (ValueDecl *Elt :OSRE->Decls)
      if (!Elt->Ty->is<FunctionType>())
        return false;
    return true;
  }
  
  return false;
}

/// ReduceJuxtaposedExprs - Given an element of a sequence expression, check to
/// see if it is the start of a juxtaposed sequence.  If so, reduce it down to
/// a single element.  This allows functions to bind to their arguments.
static void ReduceJuxtaposedExprs(SequenceExpr *E, unsigned Elt,
                                  TypeChecker &TC) {
  // If there are no subsequent expression, then it isn't juxtaposed.
  if (Elt+1 == E->NumElements) return;

  Expr *EltExpr = E->Elements[Elt];
  
  // If we have a .foo suffix, and if it isn't bound to a base expression, then
  // we just found our base.
  if (UnresolvedDotExpr *UDE = dyn_cast<UnresolvedDotExpr>(E->Elements[Elt+1]))
    if (UDE->SubExpr == 0) {
      // Set EltExpr as the base expression.
      UDE->SubExpr = EltExpr;
      
      memmove(E->Elements+Elt, E->Elements+Elt+1,
              (E->NumElements-Elt-1)*sizeof(E->Elements[0]));
      --E->NumElements;
      
      // Reprocess the newly formed expression.
      EltExpr = SemaExpressionTree::doIt(UDE, TC);
      if (EltExpr)
        E->Elements[Elt] = EltExpr;
      
      return ReduceJuxtaposedExprs(E, Elt, TC);
    }

  // If this expression isn't a function, then it doesn't juxtapose.
  if (!isKnownToBeAFunction(EltExpr))
    return;
  
   // If this is a function, then it can juxtapose.  Note that the grammar
  // effectively imposed by this is ambiguous with the top level of sequenced
  // expressions: "f() g()" is initially parsed as 4 exprs in a sequence:
  // "f () g ()".  Because f and g are functions, they bind to their arguments
  // yielding "(f()) (g())".  This also handles more complex cases like:
  //     A + B C * D
  // Which can be parsed either as:
  //     A + (B C) * D         <-- Juxtaposition here
  //     (A + B) (C * D)       <-- Juxtaposition at a higher level.
  // This is disambiguated based on whether B has function type or not.  If so,
  // it binds tightly to C.
  Expr *ArgExpr = E->Elements[Elt+1];

  // Okay, we have a function application, analyze it.
  Type ResultTy;
  if (!SemaApplyExpr(EltExpr, ArgExpr, ResultTy, TC)) {
    E->Elements[Elt] = new (TC.Context) ApplyExpr(EltExpr, ArgExpr, ResultTy);
    // Drop the argument.
    memmove(E->Elements+Elt+1, E->Elements+Elt+2, 
            (E->NumElements-Elt-2)*sizeof(E->Elements[0]));
  } else {
    // Drop the function.  FIXME: This is terrible recovery.
    memmove(E->Elements+Elt, E->Elements+Elt+1,
            (E->NumElements-Elt-1)*sizeof(E->Elements[0]));
    // Replace the argument with a dead empty tuple.
    E->Elements[Elt] =
      new (TC.Context) TupleExpr(SMLoc(), 0, 0, 0, SMLoc(),
                                 false, false, TupleType::getEmpty(TC.Context));
  }
  --E->NumElements;
  
  // Check to see if there are more functions arguments to apply, as in,
  // something like  f()()  where f has type ()->()->().
  ReduceJuxtaposedExprs(E, Elt, TC);
}

Expr *SemaExpressionTree::VisitSequenceExpr(SequenceExpr *E) {
  // If types of leaves were newly resolved, then this sequence may have
  // just changed from a sequence of operations into a binary expression,
  // function application or something else.  Check this now.  This is
  // actually effectively just parsing logic.
  for (unsigned i = 0; i < E->NumElements; ++i) {
    // If Elts[i] is the start of a binary expression sequence, reduce it down
    // to a single element.
    ReduceBinaryExprs(E, i, TC);
  }

  assert(E->NumElements != 0 && "Must have at least one element");
  
  // If we reduced this down to a single element, then we're done.
  if (E->NumElements == 1)
    return E->Elements[0];
  
  if (SemaSequenceExpr(E->Elements, E->NumElements, E->Ty, TC)) return 0;
  
  // If the expression is a top level tuple expression like "(x, y)" and it was
  // immediately preceded by an identifier, like "f(x,y)" then diagnose this as
  // an error.  The tuple is pointless and it is likely that the user expected
  // the function to bind to the arguments.  Examples could be: f.a().b when
  // a is declared as taking a single argument.
  for (unsigned i = 0, e = E->NumElements; i != e; ++i) {
    // If not fully resolved, bail out.
    if (E->Elements[i]->Ty->is<DependentType>()) break;
    
    if (TupleExpr *TE = dyn_cast<TupleExpr>(E->Elements[i]))
      if (TE->IsPrecededByIdentifier) {
        TC.error(TE->getLocStart(),
               "tuple expression isn't bound to identifier, add ' ' before it");
        // Drop this expression.
        memmove(E->Elements+i, E->Elements+i+1, 
                (E->NumElements-i-1)*sizeof(E->Elements[0]));
        --E->NumElements;
        
        if (E->NumElements == 1)
          return E->Elements[0];
      }
  }
  
  return E;
}

Expr *SemaExpressionTree::VisitIfExpr(IfExpr *E) {
  // The if condition must have __builtin_int1 type.  This is after the
  // conversion function is added by sema.
  if (!E->Cond->Ty->isEqual(TC.Context.TheInt1Type, TC.Context)) {
    TC.error(E->Cond->getLocStart(), "expression of type '" +
             E->Cond->Ty->getString() + "' is not legal in a condition");
    return 0;
  }
  
  // The Then/Else bodies must have '()' type.
  E->Then = TC.convertToType(E->Then, TupleType::getEmpty(TC.Context));
  if (E->Then == 0) {
    TC.note(E->IfLoc, "while processing 'if' body");
    return 0;
  }
  
  if (E->Else) {
    E->Else = TC.convertToType(E->Else, TupleType::getEmpty(TC.Context));
    if (E->Else == 0) {
      TC.note(E->ElseLoc, "while processing 'else' body of 'if'");
      return 0;
    }
  }

  E->Ty = TupleType::getEmpty(TC.Context);
  return E;
}


void SemaExpressionTree::PreProcessBraceExpr(BraceExpr *E) {
  SmallVector<Expr*, 4> ExcessExprs;
  
  SmallVector<BraceExpr::ExprOrDecl, 32> NewElements;
  
  // Braces have to manually walk into subtrees for expressions, because we
  // terminate the walk we're in for them (so we can handle decls custom).
  for (unsigned i = 0, e = E->NumElements; i != e; ++i) {
    if (Expr *SubExpr = E->Elements[i].dyn_cast<Expr*>()) {
      if ((SubExpr = doIt(SubExpr)) == 0)
        E->MissingSemi = false;
      else
        NewElements.push_back(SubExpr);
      continue;
    }
    
    Decl *D = E->Elements[i].get<Decl*>();
    NewElements.push_back(D);
    
    if (TypeAliasDecl *TAD = dyn_cast<TypeAliasDecl>(D))
      TC.typeCheck(TAD);

    if (ValueDecl *VD = dyn_cast<ValueDecl>(D)) {
      ExcessExprs.clear();
      TC.typeCheck(VD, ExcessExprs);
      
      // If we have something like 'var x = 4 foo()', then install foo() as an
      // expression *after* the VarDecl.
      for (Expr *Elt : ExcessExprs)
        NewElements.push_back(Elt);
    }
  }
  
  // If any of the elements of the braces has a function type (which indicates
  // that a function didn't get called), then produce an error.  We don't do
  // this for the last element in the 'missing semi' case, because the brace
  // expr as a whole has the function result.
  // TODO: What about tuples which contain functions by-value that are dead?
  for (unsigned i = 0, e = NewElements.size()-(E->MissingSemi ?1:0);i != e; ++i)
    if (NewElements[i].is<Expr*>() &&
        NewElements[i].get<Expr*>()->Ty->is<FunctionType>())
      // TODO: QOI: Add source range.
      TC.error(NewElements[i].get<Expr*>()->getLocStart(),
               "expression resolves to an unevaluated function");
  
  if (E->MissingSemi)
    E->Ty = NewElements.back().get<Expr*>()->Ty;
  else
    E->Ty = TupleType::getEmpty(TC.Context);
  
  // Reinstall the list now that we potentially mutated it.
  if (NewElements.size() <= E->NumElements) {
    memcpy(E->Elements, NewElements.data(),
           NewElements.size()*sizeof(E->Elements[0]));
    E->NumElements = NewElements.size();
  } else {
    E->Elements = 
      TC.Context.AllocateCopy<BraceExpr::ExprOrDecl>(NewElements.begin(),
                                                     NewElements.end());
    E->NumElements = NewElements.size();
  }
}

//===----------------------------------------------------------------------===//
// BindAndValidateClosureArgs - When a closure is formed, this walks an AST to
// update AnonClosureArgExpr to be of the right type.
//===----------------------------------------------------------------------===//

namespace {
struct RewriteAnonArgExpr {
  Type FuncInputTy;
  TypeChecker &TC;
  
  RewriteAnonArgExpr(Type funcInputTy, TypeChecker &tc)
    : FuncInputTy(funcInputTy), TC(tc) {}
  
  static Expr *WalkFn(Expr *E, Expr::WalkOrder Order, void *rewriter) {
    RewriteAnonArgExpr &Rewriter = *static_cast<RewriteAnonArgExpr*>(rewriter);
    Type FuncInputTy = Rewriter.FuncInputTy;
  
    if (Order == Expr::WalkOrder::PreOrder) {
      // If this is a ClosureExpr, don't walk into it.  This would find *its*
      // anonymous closure arguments, not ours.
      if (isa<ClosureExpr>(E)) return 0; // Don't recurse into it.
      
      // Otherwise, do recurse into it.  We handle anon args in the postorder
      // visitation.
      return E;
    }
  
    // If we found a closure argument, process it.
    AnonClosureArgExpr *A = dyn_cast<AnonClosureArgExpr>(E);
    if (A == 0) return E;  
    
    // If the input to the function is a non-tuple, only $0 is valid, if it is a
    // tuple, then $0..$N are valid depending on the number of inputs to the
    // tuple.
    unsigned NumInputArgs = 1;
    if (TupleType *TT = dyn_cast<TupleType>(FuncInputTy.getPointer()))
      NumInputArgs = TT->Fields.size();
    
    assert(A->Ty->is<DependentType>() && "Anon arg already has a type?");
    
    // Verify that the argument number isn't too large, e.g. using $4 when the
    // bound function only has 2 inputs.
    if (A->ArgNo >= NumInputArgs) {
      Rewriter.TC.error(A->Loc,
               "use of invalid anonymous argument, with number higher than"
               " # arguments to bound function");
      return 0;
    }
    
    // Assign the AnonDecls their actual concrete types now that we know the
    // context they are being used in.
    if (TupleType *TT = dyn_cast<TupleType>(FuncInputTy.getPointer())) {
      A->Ty = TT->getElementType(A->ArgNo);
    } else {
      assert(NumInputArgs == 1 && "Must have unary case");
      A->Ty = FuncInputTy;
    }
    return A;
  }
};
} // end anonymous namespace

/// BindAndValidateClosureArgs - The specified list of anonymous closure
/// arguments was bound to a closure function with the specified input
/// arguments.  Validate the argument list and, if valid, allocate and return
/// a pointer to the argument to be used for the ClosureExpr.
static bool
BindAndValidateClosureArgs(Expr *Body, Type FuncInput, TypeChecker &TC) {  
  RewriteAnonArgExpr Rewriter(FuncInput, TC);
  
  // Walk the body and rewrite any anonymous arguments.  Note that this
  // isn't a particularly efficient way to handle this, because we walk subtrees
  // even if they have no anonymous arguments.
  return Body->WalkExpr(RewriteAnonArgExpr::WalkFn, &Rewriter) == 0;
}


//===----------------------------------------------------------------------===//
// SemaCoerceBottomUp
//===----------------------------------------------------------------------===//

namespace {
  /// SemaCoerceBottomUp - This class implements bottom-up semantic analysis
  /// (aka "root to leaf", using the type of "+" to infer the type of "a" in
  /// "a+1") of an already-existing expression tree.  This is performed when an
  /// expression with dependent type is used in a context that forces a specific
  /// type.  
  ///
  /// Each visit method reanalyzes the node to see if the type can be propagated
  /// into it.  If not, it returns it.  If so it checks to see if the type
  /// is contradictory (in which case it returns NULL) otherwise it applies the
  /// type (possibly recursively) and returns the new/updated expression.
  class SemaCoerceBottomUp : public ExprVisitor<SemaCoerceBottomUp, Expr*> {
    friend class ExprVisitor<SemaCoerceBottomUp, Expr*>;
    TypeChecker &TC;
    Type DestTy;
    
    Expr *VisitIntegerLiteralExpr(IntegerLiteralExpr *E) {
      assert(0 && "Integer literals never have dependent type!");
      return 0;
    }
    Expr *VisitDeclRefExpr(DeclRefExpr *E) {
      return E;
    }
    
    Expr *VisitOverloadSetRefExpr(OverloadSetRefExpr *E) {
      // If any decl that is in the overload set exactly matches the expected
      // type, then select it.
      // FIXME: Conversion ranking.
      for (ValueDecl *VD : E->Decls) {
        if (VD->Ty->isEqual(DestTy, TC.Context))
          return new (TC.Context) DeclRefExpr(VD, E->Loc, VD->Ty);
      }
      return E;
    }
    
    // If this is an UnresolvedMemberExpr, then this provides the type we've
    // been looking for!
    Expr *VisitUnresolvedMemberExpr(UnresolvedMemberExpr *UME) {
      // The only valid type for an UME is a OneOfType.
      OneOfType *DT = DestTy->getAs<OneOfType>();
      if (DT == 0) {
        TC.error(UME->getLocStart(),
                 "dependent reference to member '" + UME->Name.str() +
                 "' cannot convert to '" + DestTy->getString() + "'");
        return 0;
      }
      
      // The oneof type must have an element of the specified name.
      OneOfElementDecl *DED = DT->getElement(UME->Name);
      if (DED == 0) {
        TC.error(UME->getLocStart(),
                 "type '" + DestTy->getString() + "' has no member named '" +
                 UME->Name.str() + "'");
        TC.note(DT->OneOfLoc, "type declared here");
        return 0;
      }
      
      // If it does, then everything is good, resolve the reference.
      return new (TC.Context) DeclRefExpr(DED, UME->ColonLoc, DED->Ty);
    }  
    
    Expr *VisitTupleExpr(TupleExpr *E);
    
    Expr *VisitUnresolvedDeclRefExpr(UnresolvedDeclRefExpr *E) {
      return E;
    }
    Expr *VisitUnresolvedDotExpr(UnresolvedDotExpr *E) {
      return E;
    }
    
    Expr *VisitUnresolvedScopedIdentifierExpr
    (UnresolvedScopedIdentifierExpr *E) {
      assert(0 && "This node should be resolved already!");
    }
    
    Expr *VisitTupleElementExpr(TupleElementExpr *E) {
      // TupleElementExpr is fully resolved.
      assert(0 && "This node doesn't exist for dependent types");
      return 0;
    }
    
    Expr *VisitTupleShuffleExpr(TupleShuffleExpr *E) {
      // TupleElementExpr is fully resolved.
      assert(0 && "This node doesn't exist for dependent types");
      return 0;
    }

    
    Expr *VisitApplyExpr(ApplyExpr *E) {
      // FIXME: Given an ApplyExpr of "a b" where "a" is an overloaded value, we
      // may be able to prune the overload set based on the known result type.
      // Doing this may allow the ambiguity to resolve by removing candidates
      // that caused the ambiguity.  For example if we know that the destination
      // type is 'int', and we had "int -> int" and "SomeTy -> float", we can
      // prune the second one, and then recursively apply 'int' to b.
      return E;
    }
    Expr *VisitSequenceExpr(SequenceExpr *E) {
      // If we have ":f x" and the result type of the Apply is a OneOfType, then
      // :f must be an element constructor for the oneof value.  Note that
      // handling this syntactically causes us to reject "(:f) x" as ambiguous.
      if (E->NumElements == 2) {
        if (UnresolvedMemberExpr *UME =
              dyn_cast<UnresolvedMemberExpr>(E->Elements[0]))
          if (OneOfType *DT = DestTy->getAs<OneOfType>()) {
            // The oneof type must have an element of the specified name.
            OneOfElementDecl *DED = DT->getElement(UME->Name);
            if (DED == 0 || !DED->Ty->is<FunctionType>()) {
              TC.error(UME->getLocStart(), "invalid type '" +
                       DestTy->getString() + "' to initialize member");
              return 0;
            }
              
            Expr *Fn = new (TC.Context) DeclRefExpr(DED, UME->ColonLoc,DED->Ty);
            
            if (SemaApplyExpr(Fn, E->Elements[1], E->Ty, TC))
              return 0;
            
            return new (TC.Context) ApplyExpr(Fn, E->Elements[1], E->Ty);
          }
      }
      
      // FIXME: Apply to last value of sequence.
      return E;
    }
    Expr *VisitBraceExpr(BraceExpr *E) {
      assert(E->MissingSemi && "Can't have dependent type when return ()");
      assert(E->NumElements && "Can't have 0 elements + missing semi");
      assert(E->Elements[E->NumElements-1].is<Expr*>() && "Decl is ()");
 
      Expr *LastVal = E->Elements[E->NumElements-1].get<Expr*>();
      
      LastVal = convertToType(LastVal, DestTy, true, TC);
      if (LastVal == 0) return 0;

      // Update the end of the brace expression.
      E->Elements[E->NumElements-1] = LastVal;
      return E;
    }
    Expr *VisitClosureExpr(ClosureExpr *E) {
      return E;      
    }
    
    Expr *VisitAnonClosureArgExpr(AnonClosureArgExpr *E) {
      return E;
    }

    Expr *VisitBinaryExpr(BinaryExpr *E) {
      // TODO: If the function is an overload set and the result type that we're
      // coercing onto the binop is completely incompatible with some elements
      // of the overload set, trim them out.      
      return E;
    }
    
    Expr *VisitIfExpr(IfExpr *E) {
      return E;
    }

    
    SemaCoerceBottomUp(TypeChecker &tc, Type destTy) : TC(tc), DestTy(destTy) {
      assert(!DestTy->is<DependentType>());
    }
    Expr *doIt(Expr *E) {
      return Visit(E);
    }
  public:
    
    /// convertToType - This is the main entrypoint to SemaCoerceBottomUp.
    static Expr *convertToType(Expr *E, Type DestTy, bool IgnoreAnonDecls,
                               TypeChecker &TC);

  private:
    static Expr *convertScalarToTupleType(Expr *E, TupleType *DestTy,
                                          unsigned FieldNo, TypeChecker &TC);
    static Expr *
    convertTupleToTupleType(Expr *E, unsigned NumExprElements,
                            TupleType *DestTy, TypeChecker &TC);
  };
} // end anonymous namespace.


Expr *SemaCoerceBottomUp::VisitTupleExpr(TupleExpr *E) {
  // If we're providing a type for a tuple expr, we have a couple of
  // different cases.  If the tuple has a single element and the destination
  // type is not a tuple type, then this just recursively forces the scalar
  // type into the single element.
  if (E->isGroupingParen()) {
    E->SubExprs[0] = convertToType(E->SubExprs[0], DestTy, true, TC);
    if (E->SubExprs[0] == 0) return 0;
    
    E->Ty = E->SubExprs[0]->Ty;
    return E;
  }
  
  return convertToType(E, DestTy, true, TC);
}

/// convertTupleToTupleType - Given an expression that has tuple type, convert
/// it to have some other tuple type.
///
/// The caller gives us a list of the expressions named arguments and a count of
/// tuple elements for E in the IdentList+NumIdents array.  DestTy specifies the
/// type to convert to, which is known to be a TupleType.
Expr *
SemaCoerceBottomUp::convertTupleToTupleType(Expr *E, unsigned NumExprElements,
                                            TupleType *DestTy, TypeChecker &TC){
  
  // If the tuple expression or destination type have named elements, we
  // have to match them up to handle the swizzle case for when:
  //   (.y = 4, .x = 3)
  // is converted to type:
  //   (.x = int, .y = int)
  SmallVector<Identifier, 8> IdentList(NumExprElements);
  
  // Check to see if this conversion is ok by looping over all the destination
  // elements and seeing if they are provided by the input.
  
  // Keep track of which input elements are used.
  // TODO: Record where the destination elements came from in the AST.
  SmallVector<bool, 16> UsedElements(NumExprElements);
  SmallVector<int, 16>  DestElementSources(DestTy->Fields.size(), -1);

  if (TupleType *ETy = E->Ty->getAs<TupleType>()) {
    assert(ETy->Fields.size() == NumExprElements && "Expr #elements mismatch!");
    for (unsigned i = 0, e = ETy->Fields.size(); i != e; ++i)
      IdentList[i] = ETy->Fields[i].Name;
    
    // First off, see if we can resolve any named values from matching named
    // inputs.
    for (unsigned i = 0, e = DestTy->Fields.size(); i != e; ++i) {
      const TupleTypeElt &DestElt = DestTy->Fields[i];
      // If this destination field is named, first check for a matching named
      // element in the input, from any position.
      if (DestElt.Name.empty()) continue;
      
      int InputElement = -1;
      for (unsigned j = 0; j != NumExprElements; ++j)
        if (IdentList[j] == DestElt.Name) {
          InputElement = j;
          break;
        }
      if (InputElement == -1) continue;
      
      DestElementSources[i] = InputElement;
      UsedElements[InputElement] = true;
    }
  }
  
  // Next step, resolve (in order) unmatched named results and unnamed results
  // to any left-over unnamed input.
  unsigned NextInputValue = 0;
  for (unsigned i = 0, e = DestTy->Fields.size(); i != e; ++i) {
    // If we already found an input to satisfy this output, we're done.
    if (DestElementSources[i] != -1) continue;
    
    // Scan for an unmatched unnamed input value.
    while (1) {
      // If we didn't find any input values, we ran out of inputs to use.
      if (NextInputValue == NumExprElements)
        break;
      
      // If this input value is unnamed and unused, use it!
      if (!UsedElements[NextInputValue] && IdentList[NextInputValue].empty())
        break;
      
      ++NextInputValue;
    }
    
    // If we ran out of input values, we either don't have enough sources to
    // fill the dest (as in when assigning (1,2) to (int,int,int), or we ran out
    // and default values should be used.
    if (NextInputValue == NumExprElements) {
      if (DestTy->Fields[i].Init != 0) {
        // If the default initializer should be used, leave the
        // DestElementSources field set to -2.
        DestElementSources[i] = -2;
        continue;
      }        
     
      // If this is a TupleExpr (common case) get a more precise location for
      // the element we care about.
      SMLoc ErrorLoc = E->getLocStart();
      if (TupleExpr *TE = dyn_cast<TupleExpr>(E))
        ErrorLoc = TE->RParenLoc;
      
      if (DestTy->Fields[i].Name.empty())
        TC.error(ErrorLoc, "no value to initialize tuple element #" +
                 Twine(i) + " in expression of type '" +
                 E->Ty->getString() + "'");
      else
        TC.error(ErrorLoc, "no value to initialize tuple element '" +
                 DestTy->Fields[i].Name.str() + "' (#" + Twine(i) +
                 ") in expression of type '" + E->Ty->getString() + "'");
      return 0;
    }
    
    // Okay, we found an input value to use.
    DestElementSources[i] = NextInputValue;
    UsedElements[NextInputValue] = true;
  }
  
  // If there were any unused input values, we fail.
  for (unsigned i = 0, e = UsedElements.size(); i != e; ++i)
    if (!UsedElements[i]) {
      // If this is a TupleExpr (common case) get a more precise location for
      // the element we care about.
      SMLoc ErrorLoc = E->getLocStart();
      if (TupleExpr *TE = dyn_cast<TupleExpr>(E))
        if (Expr *SubExp = TE->SubExprs[i])
          ErrorLoc = SubExp->getLocStart();
      
    if (IdentList[i].empty())
      TC.error(ErrorLoc, "element #" + Twine(i) +
               " of tuple value not used when converting to type '" +
               DestTy->getString() + "'");
    else
      TC.error(ErrorLoc, "tuple element '" + IdentList[i].str() +
               "' (#" + Twine(i) + ") of tuple value not used when "
               "converting to type '" + DestTy->getString() + "'");
      return 0;
    }
  
  // It looks like the elements line up, walk through them and see if the types
  // either agree or can be converted.  If the expression is a TupleExpr, we do
  // this conversion in place.
  TupleExpr *TE = dyn_cast<TupleExpr>(E);
  if (TE && TE->NumSubExprs != 1 && TE->NumSubExprs == DestTy->Fields.size()) {
    SmallVector<Expr*, 8> OrigElts(TE->SubExprs,
                                         TE->SubExprs+TE->NumSubExprs);
    
    for (unsigned i = 0, e = DestTy->Fields.size(); i != e; ++i) {
      // Extract the input element corresponding to this destination element.
      unsigned SrcField = DestElementSources[i];
      assert(SrcField != ~0U && "dest field not found?");
      
      // If SrcField is -2, then the destination element should use its default
      // value.
      if (SrcField == -2U) {
        TE->SubExprs[i] = 0;
        continue;
      }
      
      // Check to see if the src value can be converted to the destination
      // element type.
      Expr *Elt = OrigElts[SrcField];
      
      Elt = convertToType(Elt, DestTy->getElementType(i), true, TC);
      // TODO: QOI: Include a note about this failure!
      if (Elt == 0) return 0;
      TE->SubExprs[i] = Elt;
      
      if (DestTy->Fields[i].Name.get()) {
        // Allocate the array on the first element with a name.
        if (TE->SubExprNames == 0)
          TE->SubExprNames =
          TC.Context.Allocate<Identifier>(DestTy->Fields.size());
        
        TE->SubExprNames[i] = DestTy->Fields[i].Name;
      } else if (TE->SubExprNames)
        TE->SubExprNames[i] = Identifier();
    }
    
    // Okay, we updated the tuple in place.
    E->Ty = DestTy;
    return E;
  }
  
  // Otherwise, if it isn't a tuple literal, we unpack the source elementwise so
  // we can do elementwise conversions as needed, then rebuild a new TupleExpr
  // of the right destination type.
  TupleType *ETy = E->Ty->getAs<TupleType>();
  SmallVector<int, 16> NewElements(DestTy->Fields.size());
  
  for (unsigned i = 0, e = DestTy->Fields.size(); i != e; ++i) {
    // Extract the input element corresponding to this destination element.
    unsigned SrcField = DestElementSources[i];
    assert(SrcField != ~0U && "dest field not found?");
    
    if (SrcField == -2U) {
      // Use the default element for the tuple.
      NewElements[i] = -1;
      continue;
    }
    
    if (ETy->getElementType(SrcField)->getCanonicalType(TC.Context) !=
        DestTy->getElementType(i)->getCanonicalType(TC.Context)) {
      TC.error(E->getLocStart(), "element #" + Twine(i) +
               " of tuple value has type '" +
               ETy->getElementType(SrcField)->getString() +
               "', but expected type '" + 
               DestTy->getElementType(i)->getString() + "'");
      return 0;
    }
    
    NewElements[i] = SrcField;
  }
  
  // If we got here, the type conversion is successful, create a new TupleExpr.  
  ArrayRef<int> Mapping = TC.Context.AllocateCopy(NewElements);
  
  return new (TC.Context) TupleShuffleExpr(E, Mapping, DestTy);
}

/// convertScalarToTupleType - Convert the specified expression to the specified
/// tuple type, which is known to be initializable with one element.
Expr *SemaCoerceBottomUp::convertScalarToTupleType(Expr *E, TupleType *DestTy,
                                                   unsigned ScalarField,
                                                   TypeChecker &TC) {
  // If the destination is a tuple type with at most one element that has no
  // default value, see if the expression's type is convertable to the
  // element type.  This handles assigning 4 to "(a = 4, b : int)".
  Type ScalarType = DestTy->getElementType(ScalarField);
  Expr *ERes = convertToType(E, ScalarType, false, TC);
  if (ERes == 0) return 0;
  
  unsigned NumFields = DestTy->Fields.size();
  
  // Must allocate space for the AST node.
  Expr **NewSE = TC.Context.Allocate<Expr*>(NumFields);
  
  bool NeedsNames = false;
  for (unsigned i = 0, e = NumFields; i != e; ++i) {
    if (i == (unsigned)ScalarField)
      NewSE[i] = ERes;
    else
      NewSE[i] = 0;
    
    NeedsNames |= DestTy->Fields[i].Name != Identifier();
  }
  
  // Handle the name if the element is named.
  Identifier *NewName = 0;
  if (NeedsNames) {
    NewName = TC.Context.Allocate<Identifier>(NumFields);
    for (unsigned i = 0, e = NumFields; i != e; ++i)
      NewName[i] = DestTy->Fields[i].Name;
  }
  
  return new (TC.Context) TupleExpr(E->getLocStart(), NewSE, NewName,
                                    NumFields, SMLoc(), false, false, DestTy);
}

/// HandleConversionToType - This is the recursive implementation of
/// ConvertToType.  It does produces diagnostics and returns null on failure.
///
/// NOTE: This needs to be kept in synch with getConversionRank in Expr.cpp.
///
Expr *SemaCoerceBottomUp::convertToType(Expr *E, Type DestTy,
                                        bool IgnoreAnonDecls, TypeChecker &TC) {
  // If we have an exact match, we're done.
  if (E->Ty->getCanonicalType(TC.Context) ==
         DestTy->getCanonicalType(TC.Context))
    return E;
  
  assert(!DestTy->is<DependentType>() &&
         "Result of conversion can't be dependent");

  // If the expression is a grouping parenthesis and it has a dependent type,
  // just force the type through it, regardless of what DestTy is.
  if (TupleExpr *TE = dyn_cast<TupleExpr>(E))
    if (TE->isGroupingParen()) {
      TE->SubExprs[0] = convertToType(TE->SubExprs[0], DestTy,
                                      IgnoreAnonDecls, TC);
      if (TE->SubExprs[0] == 0) return 0;
      TE->Ty = TE->SubExprs[0]->Ty;
      return TE;
    }
  
  if (TupleType *TT = DestTy->getAs<TupleType>()) {
    // Type conversions are carefully ranked so that they "do the right thing",
    // because they can be highly ambiguous.  For example, consider something
    // like foo(4, 5) when foo is declared to take ((int,int=3), int=6).  This
    // could be parsed as either ((4,5), 6) or ((4,3),5), but the later one is
    // the "right" answer.
    
    // If the element of the tuple has dependent type and is a TupleExpr, try to
    // convert it.
    if (TupleExpr *TE = dyn_cast<TupleExpr>(E))
      return convertTupleToTupleType(TE, TE->NumSubExprs, TT,TC);

    // If the is a scalar to tuple conversion, form the tuple and return it.
    int ScalarFieldNo = TT->getFieldForScalarInit();
    if (ScalarFieldNo != -1)
      return convertScalarToTupleType(E, TT, ScalarFieldNo, TC);
    
    // If the input is a tuple and the output is a tuple, see if we can convert
    // each element.
    if (TupleType *ETy = E->Ty->getAs<TupleType>())
      return convertTupleToTupleType(E, ETy->Fields.size(), TT, TC);
  }
  
  // Otherwise, check to see if this is an auto-closure case.  This case happens
  // when we convert an expression E to a function type whose result is E's
  // type.
  if (FunctionType *FT = DestTy->getAs<FunctionType>()) {
    // If we bound any anonymous closure arguments, validate them and resolve
    // their types.
    if (!IgnoreAnonDecls && BindAndValidateClosureArgs(E, FT->Input, TC))
      return 0;

    // If there are any live anonymous closure arguments, this level will use
    // them and remove them.  When binding something like $0+$1 to
    // (int,int)->(int,int)->() the arguments bind to the first level, not the
    // inner level.  To handle this, we ignore anonymous decls in the recursive
    // case here.
    Expr *ERes = convertToType(E, FT->Result, true, TC);
    if (ERes == 0) return 0;
  
    // Now that the AnonClosureArgExpr's potentially have a type, redo semantic
    // analysis from the leaves of the expression tree up.
    ERes = SemaExpressionTree::doIt(ERes, TC);
    if (ERes == 0)
      return 0;
    
    return new (TC.Context) ClosureExpr(ERes, DestTy);
  }
  
  // If the input expression has a dependent type, then there are two cases:
  // first this could be an AnonDecl whose type will be specified by a larger
  // context, second, this could be a context sensitive expression value like
  // :foo.  If this is a context sensitive expression, propagate the type down
  // into the subexpression.
  if (E->Ty->is<DependentType>())
    return SemaCoerceBottomUp(TC, DestTy).doIt(E);
  
  // Could not do the conversion.
  TC.error(E->getLocStart(), "invalid conversion from type '" +
           E->Ty->getString() + "' to '" + DestTy->getString() + "'");
  return 0;
}



Expr *TypeChecker::convertToType(Expr *E, Type DestTy) {
  return SemaCoerceBottomUp::convertToType(E, DestTy, false, *this);
}

//===----------------------------------------------------------------------===//
// Type Validation
//===----------------------------------------------------------------------===//

/// validateType - Types can contain expressions (in the default values for
/// tuple elements), and thus need semantic analysis to ensure that these
/// expressions are valid and that they have the appropriate conversions etc.
///
/// This returns true if the type is invalid.
bool TypeChecker::validateType(Type &InTy) {
  assert(InTy && "Cannot validate null types!");

  TypeBase *T = InTy.getPointer();
  // FIXME: Verify that these aren't circular and infinite size.
  
  // If a type has a canonical type, then it is known safe.
  if (T->hasCanonicalTypeComputed()) return false;

  bool IsValid = true;
  
  switch (T->Kind) {
  case TypeKind::Unresolved:
  case TypeKind::BuiltinInt1:
  case TypeKind::BuiltinInt8:
  case TypeKind::BuiltinInt16:
  case TypeKind::BuiltinInt32:
  case TypeKind::BuiltinInt64:
  case TypeKind::Dependent:
    return false;
  case TypeKind::OneOf:
    for (OneOfElementDecl *Elt : cast<OneOfType>(T)->Elements) {
      if (Elt->ArgumentType.isNull()) continue;
      IsValid &= !validateType(Elt->ArgumentType);
      if (!IsValid) break;
    }
    break;
  case TypeKind::NameAlias:
    IsValid = !validateType(cast<NameAliasType>(T)->TheDecl->UnderlyingTy);
    break;
  case TypeKind::Tuple: {
    TupleType *TT = cast<TupleType>(T);
    
    // Okay, we found an uncanonicalized tuple type, which might have default
    // values.  If so, we'll potentially have to update it.
    for (unsigned i = 0, e = TT->Fields.size(); i != e; ++i) {
      // The element has *at least* a type or an initializer, so we start by
      // verifying each individually.
      Type EltTy = TT->Fields[i].Ty;
      if (EltTy && validateType(EltTy)) {
        IsValid = false;
        break;
      }

      Expr *EltInit = TT->Fields[i].Init;
      if (EltInit == 0) continue;
      
      SMLoc InitLoc = EltInit->getLocStart();
      checkBody(EltInit, EltTy, 0);
      if (EltInit == 0) {
        note(InitLoc, "while converting default tuple value to element type");
        IsValid = false;
        break;
      }
        
      // If both a type and an initializer are specified, make sure the
      // initializer's type agrees with the (redundant) type.
      assert(EltTy.isNull() || 
             EltTy->getCanonicalType(Context) ==
             EltInit->Ty->getCanonicalType(Context));
      EltTy = EltInit->Ty;

      TT->updateInitializedElementType(i, EltTy, EltInit);
    }
    break;
  }
      
  case TypeKind::Function: {
    FunctionType *FT = cast<FunctionType>(T);
    if ((InTy = FT->Input, validateType(InTy)) ||
        (InTy = FT->Result, validateType(InTy))) {
      IsValid = false;
      break;
    }
    InTy = FT;
    break;
  }
  case TypeKind::Array:
    ArrayType *AT = cast<ArrayType>(T);
    if (InTy = AT->Base, validateType(InTy)) {
      IsValid = false;
      break;
    }
    InTy = AT;
    // FIXME: We need to check AT->Size! (It also has to be convertible to int).
    break;
  }

  // If we determined that this type is invalid, erase it in the caller.
  if (!IsValid) {
    // FIXME: This should set the type to some Error type, which is
    // distinguishable from unresolved.
    InTy = UnresolvedType::get(Context);
    return true;
  }

  // Now that we decided that this type is ok, get the canonical type for it so
  // that we never reanalyze it again.
  // If it is ever a performance win to avoid computing canonical types, we can
  // just keep a SmallPtrSet of analyzed Types in TypeChecker.
  InTy->getCanonicalType(Context);
  
  // FIXME: This isn't good enough: top-level stuff can have these as well and
  // their types need to be resolved at the end of name binding.  Perhaps we
  // should require them to have explicit types even if they have values and 
  // let the value mismatch be detected at typechecking time? 
  return false;
}

//===----------------------------------------------------------------------===//
// Type Checking Entrypoint
//===----------------------------------------------------------------------===//

/// validateAttributes - Check that the func/var declaration attributes are ok.
void TypeChecker::validateAttributes(DeclAttributes &Attrs, Type Ty) {
  // If the decl has an infix precedence specified, then it must be a function
  // whose input is a two element tuple.
  if (Attrs.InfixPrecedence != -1) {
    bool IsError = true;
    if (FunctionType *FT = dyn_cast<FunctionType>(Ty.getPointer()))
      if (TupleType *TT = dyn_cast<TupleType>(FT->Input.getPointer()))
        IsError = TT->Fields.size() != 2;
    if (IsError) {
      error(Attrs.LSquareLoc, "function with 'infix' specified must take "
            "a two element tuple as input");
      Attrs.InfixPrecedence = -1;
    }
  }
}

/// DiagnoseUnresolvedTypes - This function is invoked on all nodes in an
/// expression tree checking to make sure they don't contain any DependentTypes.
static Expr *DiagnoseUnresolvedTypes(Expr *E, Expr::WalkOrder Order,
                                     void *Data) {
  // Ignore the preorder walk.  We'd rather diagnose use of unresolved types
  // during the postorder walk so that the inner most expressions are diagnosed
  // before the outermost ones.
  if (Order == Expr::WalkOrder::PreOrder)
    return E;
  
  // Use is to strip off sugar.
  if (!E->Ty->is<DependentType>())
    return E;
  
  TypeChecker &TC = *(TypeChecker*)Data;
  TC.error(E->getLocStart(),
           "ambiguous expression was not resolved to a concrete type");
  return 0;
}


/// checkBody - Type check an expression that is used in a top-level
/// context like a var/func body, or tuple default value.  If DestTy is
/// specified, the expression is coerced to the requested type.
///
/// If the body turns out to be a sequence, this returns the single element
/// with the excess in the provided smallvector.
void TypeChecker::checkBody(Expr *&E, Type DestTy,
                            SmallVectorImpl<Expr*> *ExcessElements) {
  assert(E != 0 && "Can't check a null body!");
  E = SemaExpressionTree::doIt(E, *this);
  if (E == 0) return;
  
  // If the top-level expression is a sequence expression, then we have
  // something like:
  //   var x = 4 foo()
  // where the "foo()" portion of this expression is actually executed
  // separately and independently of the var.  If the user really really wanted
  // something silly like this, then they should have used parens, as in:
  //  var x = (4 foo())
  if (SequenceExpr *SE = dyn_cast<SequenceExpr>(E)) {
    E = SE->Elements[0];
    if (ExcessElements)
      ExcessElements->append(SE->Elements+1, SE->Elements+SE->NumElements);
    else {
      // If this context doesn't want any extra expressions, reject them. This
      // is not the best error message in the world :).
      for (unsigned i = 1, e = SE->NumElements; i != e; ++i)
        error(SE->Elements[i]->getLocStart(),
              "expected a singular expression: this expression is unbound");
    }
  }
  
  if (DestTy)
    E = convertToType(E, DestTy);
  
  // Check the initializer/body to make sure that we succeeded in resolving
  // all of the types contained within it.  We should not have any
  // DependentType's left for subexpressions.
  if (E)
    E = E->WalkExpr(DiagnoseUnresolvedTypes, this);
}

void TypeChecker::typeCheck(TypeAliasDecl *TAD) {
  validateType(TAD->UnderlyingTy);
}

void TypeChecker::typeCheckERD(ElementRefDecl *ERD) {
  // If the type is already resolved we're done.  ElementRefDecls are simple.
  if (!ERD->Ty->is<DependentType>()) return;
  
  if (Type T = ElementRefDecl::getTypeForPath(ERD->VD->Ty, ERD->AccessPath))
    ERD->Ty = T;
  else {
    error(ERD->getLocStart(), "'" + ERD->Name.str() +
          "' is an invalid index for '" + ERD->VD->Ty->getString() +
          "'");
    // FIXME: This should be "invalid"
    ERD->Ty = TupleType::getEmpty(Context);
  }
}

bool TypeChecker::validateVarName(Type Ty, DeclVarName *Name) {
  // Check for a type specifier mismatch on this level.
  assert(Ty && "This lookup should never fail");

  // If this is a simple varname, then it matches any type, and we're done.
  if (Name->isSimple())
    return false;

  // If we're peering into an unresolved type, we can't analyze it yet.
  if (Ty->is<DependentType>()) return false;

  // If we have a single-element oneof (like a struct) then we allow matching
  // the struct elements with the tuple syntax.
  if (OneOfType *OOT = Ty->getAs<OneOfType>())
    if (OOT->hasSingleElement())
      Ty = OOT->getElement(0)->ArgumentType;
  
  // If we have a complex case, Ty must be a tuple and the name specifier must
  // have the correct number of elements.
  TupleType *AccessedTuple = Ty->getAs<TupleType>();
  if (AccessedTuple == 0) {
    error(Name->LPLoc, "name specifier matches '" + Ty->getString() +
          "' which is not a tuple");
    return true;
  }

  // Verify the # elements line up.
  if (Name->Elements.size() != AccessedTuple->Fields.size()) {
    error(Name->LPLoc, "name specifier matches '" + Ty->getString() +
          "' which requires " + Twine(AccessedTuple->Fields.size()) +
          " names, but has " + Twine(Name->Elements.size()));
    return true;
  }
  
  // Okay, everything looks good at this level, recurse.
  for (unsigned i = 0, e = Name->Elements.size(); i != e; ++i) {
    if (validateVarName(AccessedTuple->Fields[i].Ty, Name->Elements[i]))
      return true;
  }

  return false;
}

void TypeChecker::typeCheckVarDecl(VarDecl *VD,
                                   SmallVectorImpl<Expr*> &ExcessExprs) {
  // Type check the ValueDecl part of a VarDecl.
  if (typeCheckValueDecl(VD, ExcessExprs))
    return;
  
  // If the VarDecl had a name specifier, verify that it lines up with the
  // actual type of the VarDecl.
  if (VD->NestedName && validateVarName(VD->Ty, VD->NestedName))
    VD->NestedName = 0;
}


bool TypeChecker::typeCheckValueDecl(ValueDecl *VD,
                                     SmallVectorImpl<Expr*> &ExcessExprs){
  if (validateType(VD->Ty)) {
    VD->Init = 0;
    return true;
  }

  // Validate that the initializers type matches the expected type.
  if (VD->Init == 0) {
    // If we have no initializer and the type is dependent, then the initializer
    // was invalid and removed.
    if (VD->Ty->is<DependentType>()) return true;
  } else if (VD->Ty->is<DependentType>()) {
    checkBody(VD->Init, 0, &ExcessExprs);
    if (VD->Init == 0)
      note(VD->getLocStart(),
           "while converting 'var' initializer to declared type");
    else
      VD->Ty = VD->Init->Ty;
  } else {
    // If both a type and an initializer are specified, make sure the
    // initializer's type agrees (or converts) to the redundant type.
    checkBody(VD->Init, VD->Ty, &ExcessExprs);
    if (VD->Init == 0) {
      if (isa<VarDecl>(VD))
        note(VD->getLocStart(),
             "while converting 'var' initializer to declared type");
      else
        note(VD->getLocStart(), "while converting body to declared type");
    }
  }
  
  validateAttributes(VD->Attrs, VD->Ty);
  return false;
}

/// performTypeChecking - Once parsing and namebinding are complete, these
/// walks the AST to resolve types and diagnose problems therein.
///
/// FIXME: This should be moved out to somewhere else.
void swift::performTypeChecking(TranslationUnitDecl *TUD, ASTContext &Ctx) {
  TypeChecker TC(Ctx);
  
  // Type check the top-level BraceExpr.  This sorts out any top-level
  // expressions and recursively processes the rest of the translation unit.
  TUD->Body =
    cast_or_null<BraceExpr>(SemaExpressionTree::doIt(TUD->Body, TC));
}
