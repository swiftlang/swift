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
#include "swift/AST/Type.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/ADT/Twine.h"
#include "llvm/Support/SourceMgr.h"
using namespace swift;
using llvm::isa;
using llvm::dyn_cast;
using llvm::cast;
using llvm::SMLoc;


namespace {
  class TypeChecker {
  public:
    ASTContext &Context;
    TypeChecker(ASTContext &C) : Context(C) {}
    
    void note(SMLoc Loc, const llvm::Twine &Message) {
      Context.SourceMgr.PrintMessage(Loc, Message, "note");
    }
    void warning(SMLoc Loc, const llvm::Twine &Message) {
      Context.SourceMgr.PrintMessage(Loc, Message, "warning");
    }
    void error(SMLoc Loc, const llvm::Twine &Message) {
      Context.setHadError();
      Context.SourceMgr.PrintMessage(Loc, Message, "error");
    }
    
    bool validateType(Type *&T);

    void typeCheck(TypeAliasDecl *TAD);

    void typeCheck(ValueDecl *VD, llvm::SmallVectorImpl<Expr*> &ExcessExprs) {
      // No types to resolved for a ElementRefDecl.
      if (ElementRefDecl *ERD = dyn_cast<ElementRefDecl>(VD))
        return typeCheck(ERD);
      if (VarDecl *Var = dyn_cast<VarDecl>(VD))
        return typeCheck(Var, ExcessExprs);
      
      typeCheck(cast<FuncDecl>(VD), ExcessExprs);
    }
    
    void typeCheck(ElementRefDecl *ERD);
    void typeCheck(FuncDecl *FD, llvm::SmallVectorImpl<Expr*> &ExcessExprs);
    void typeCheck(VarDecl *VD, llvm::SmallVectorImpl<Expr*> &ExcessExprs);
    
    void validateAttributes(DeclAttributes &Attrs, Type *Ty);
    
    bool validateVarName(Type *Ty, DeclVarName *Name);

    // Utility Functions
    enum ConversionReason {
      CR_BinOpLHS,  // Left side of binary operator.
      CR_BinOpRHS,  // Right side of binary operator.
      CR_FuncApply, // Application of function argument.
      CR_VarInit,   // Var initializer
      CR_TupleInit, // Tuple element initializer.
      CR_FuncBody   // Function body specification.
    };

    /// checkBody - Type check an expression that is used in a top-level
    /// context like a var/func body, or tuple default value.  If DestTy is
    /// specified, the expression is coerced to the requested type.  The
    /// specified ConversionReason is used to produce a diagnostic on error.
    ///
    /// If the body turns out to be a sequence, this returns the single element
    /// with the excess in the provided SmallVector.  If the SmallVector is not
    /// provided, errors are emitted for the excess expressions.
    void checkBody(Expr *&E, Type *DestTy, ConversionReason Res,
                   llvm::SmallVectorImpl<Expr*> *ExcessElements);
    

    /// convertToType - Do semantic analysis of an expression in a context that
    /// expects a particular type.  This does conversion to that type if the types
    /// don't match and diagnoses cases where the conversion cannot be performed.
    /// The Reason specifies why this conversion is happening, for diagnostic
    /// purposes.
    ///
    /// This emits a diagnostic and returns null on error.
    Expr *convertToType(Expr *E, Type *Ty, bool IgnoreAnonDecls,
                        ConversionReason Reason);
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

static bool SemaDeclRefExpr(ValueDecl *D, SMLoc Loc, Type *&ResultTy,
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

static bool SemaDotIdentifier(Expr *E, SMLoc DotLoc,
                              Identifier Name, SMLoc NameLoc,
                              Type *&ResultTy, int &FieldNo, TypeChecker &TC) {
  if (isa<DependentType>(E->Ty)) {
    FieldNo = -1;
    ResultTy = E->Ty;
    return false;
  }
  
  Type *ETy = E->Ty;

  // If this is a member access to a oneof with a single element constructor,
  // allow direct access to the type underlying the single element.  This
  // allows element access on structs, for example.
  if (OneOfType *DT = ETy->getAs<OneOfType>()) {
    if (DT->Elements.size() == 1 && DT->Elements[0]->ArgumentType)
      ETy = DT->Elements[0]->ArgumentType;
  }
  
  if (TupleType *TT = ETy->getAs<TupleType>()) {
    // If the field name exists, we win.
    FieldNo = TT->getNamedElementId(Name);
    if (FieldNo != -1) {
      ResultTy = TT->getElementType(FieldNo);
      return false;
    }

    // Okay, the field name was invalid.  If this is a dollarident like $4,
    // process it as a field index.
    if (Name.getLength() > 1 && Name.get()[0] == '$') {
      unsigned Value = 0;
      if (!llvm::StringRef(Name.get()+1).getAsInteger(10, Value)) {
        if (Value >= TT->Fields.size()) {
          TC.error(NameLoc, "field number is too large for tuple");
          return true;
        }
        
        FieldNo = Value;
        ResultTy = TT->getElementType(Value);
        return false;
      }
    }
    
    // Otherwise, we just have an unknown field name.
    TC.error(NameLoc, "unknown field in tuple");
    return true;
  }
  
  TC.error(DotLoc, "base type of field access has no fields");
  return true;
}


static bool SemaTupleExpr(SMLoc LPLoc, Expr **SubExprs,
                          Identifier *SubExprNames,
                          unsigned NumSubExprs, SMLoc RPLoc,
                          Type *&ResultTy, TypeChecker &TC) {
  // A tuple expr with a single subexpression and no name is just a grouping
  // paren.
  if (NumSubExprs == 1 && (SubExprNames == 0 || SubExprNames[0].get() == 0)) {
    ResultTy = SubExprs[0]->Ty;
    return false;
  }
  
  // Compute the result type.
  llvm::SmallVector<TupleTypeElt, 8> ResultTyElts(NumSubExprs);
  
  for (unsigned i = 0, e = NumSubExprs; i != e; ++i) {
    // If the element value is missing, it has the value of the default
    // expression of the result type, which must be known.
    if (SubExprs[i] == 0) {
      assert(ResultTy && isa<TupleType>(ResultTy) && 
             "Can't have default value without a result type");
      
      ResultTyElts[i].Ty = cast<TupleType>(ResultTy)->getElementType(i);
      
      // FIXME: What about a default value that is dependent?
      if (isa<DependentType>(ResultTyElts[i].Ty)) {
        ResultTy = ResultTyElts[i].Ty;
        return false;
      }
    } else {
      // If any of the tuple element types is dependent, the whole tuple should
      // have dependent type.
      if (isa<DependentType>(SubExprs[i]->Ty)) {
        ResultTy = SubExprs[i]->Ty;
        return false;
      }
    
      ResultTyElts[i].Ty = SubExprs[i]->Ty;
    }

    // If a name was specified for this element, use it.
    if (SubExprNames)
      ResultTyElts[i].Name = SubExprNames[i];
  }
  
  ResultTy = TC.Context.getTupleType(ResultTyElts);
  return false;
}

static bool SemaApplyExpr(Expr *&E1, Expr *&E2, Type *&ResultTy,
                          TypeChecker &TC) {
  FunctionType *FT = cast<FunctionType>(E1->Ty);
  
  // We have a function application.  Check that the argument type matches the
  // expected type of the function.
  E2 = TC.convertToType(E2, FT->Input, false, TypeChecker::CR_FuncApply);
  if (E2 == 0)
    return true;
  
  ResultTy = FT->Result;
  return false;
}

/// SemaSequenceExpr - Perform semantic analysis for sequence expressions.
static bool SemaSequenceExpr(Expr **Elements, unsigned NumElements,
                             Type *&ResultTy, TypeChecker &TC) {
  // If any of the operands of the sequence have a dependent type, then so does
  // the sequence.  Dependent values can be resolved to function types, so the
  // last value of the sequence may be an operand to the function, not the
  // result of the sequence.
  for (unsigned i = 0; i != NumElements; ++i)
    if (isa<DependentType>(Elements[i]->Ty)) {
      ResultTy = Elements[i]->Ty;
      return false;
    }
  
  ResultTy = Elements[NumElements-1]->Ty;
  return false;
}

/// SemaBinaryExpr - Perform semantic analysis of binary expressions.
/// OpFn is null if this is an assignment (FIXME: we don't have generics yet).
static bool SemaBinaryExpr(Expr *&LHS, ValueDecl *OpFn,
                           SMLoc OpLoc, Expr *&RHS, Type *&ResultTy,
                           TypeChecker &TC) {
  if (isa<DependentType>(LHS->Ty)) {
    ResultTy = TC.Context.TheDependentType;
    return false; 
  }
  
  // If this is an assignment, then we coerce the RHS to the LHS.
  if (OpFn == 0) {
    RHS = TC.convertToType(RHS, LHS->Ty, false, TypeChecker::CR_BinOpRHS);
    if (LHS == 0) return true;
    
    ResultTy = TC.Context.TheEmptyTupleType;
    return false;
  }
  
  // Parser verified that OpFn has an Infix Precedence.  Sema verified that OpFn
  // only has InfixPrecedence if it takes a 2 element tuple as input.
  assert(OpFn->Attrs.InfixPrecedence != -1 &&
         "Sema and parser should verify that only binary predicates are used"); 
  FunctionType *FnTy = cast<FunctionType>(OpFn->Ty);
  TupleType *Input = cast<TupleType>(FnTy->Input);
  assert(Input->Fields.size() == 2 && "Sema error validating infix fn type");
  
  // Verify that the LHS/RHS have the right type and do conversions as needed.
  LHS = TC.convertToType(LHS, Input->getElementType(0), false,
                         TypeChecker::CR_BinOpLHS);
  if (LHS == 0) return true;
  
  RHS = TC.convertToType(RHS, Input->getElementType(1), false,
                         TypeChecker::CR_BinOpRHS);
  if (RHS == 0) return true;
  
  ResultTy = FnTy->Result;
  return false;
}


//===----------------------------------------------------------------------===//
// Expression Reanalysis - SemaExpressionTree
//===----------------------------------------------------------------------===//

static Expr *HandleConversionToType(Expr *E, Type *DestTy, bool IgnoreAnonDecls,
                                    TypeChecker &TC);


/// ConvertExprToTupleType - Given an expression that has tuple type, convert it
/// to have some other tuple type.
///
/// The caller gives us a list of the expressions named arguments and a count of
/// tuple elements for E in the IdentList+NumIdents array.  DestTy specifies the
/// type to convert to, which is known to be a TupleType.
static Expr *
ConvertExprToTupleType(Expr *E, llvm::ArrayRef<Identifier> IdentList,
                       TupleType *DestTy, TypeChecker &TC) {
  
  // Check to see if this conversion is ok by looping over all the destination
  // elements and seeing if they are provided by the input.
  
  // Keep track of which input elements are used.
  // TODO: Record where the destination elements came from in the AST.
  llvm::SmallVector<bool, 16> UsedElements(IdentList.size());
  llvm::SmallVector<int, 16>  DestElementSources(DestTy->Fields.size(), -1);
  
  
  // First off, see if we can resolve any named values from matching named
  // inputs.
  for (unsigned i = 0, e = DestTy->Fields.size(); i != e; ++i) {
    const TupleTypeElt &DestElt = DestTy->Fields[i];
    // If this destination field is named, first check for a matching named
    // element in the input, from any position.
    if (DestElt.Name.get() == 0) continue;
    
    int InputElement = -1;
    for (unsigned j = 0, e2 = IdentList.size(); j != e2; ++j)
      if (IdentList[j] == DestElt.Name) {
        InputElement = j;
        break;
      }
    if (InputElement == -1) continue;
    
    DestElementSources[i] = InputElement;
    UsedElements[InputElement] = true;
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
      if (NextInputValue == IdentList.size())
        break;
      
      // If this input value is unnamed and unused, use it!
      if (!UsedElements[NextInputValue] && IdentList[NextInputValue].get() == 0)
        break;
      
      ++NextInputValue;
    }

    // If we ran out of input values, we either don't have enough sources to
    // fill the dest (as in when assigning (1,2) to (int,int,int), or we ran out
    // and default values should be used.
    if (NextInputValue == IdentList.size()) {
      // TODO: QOI: it would be good to indicate which dest element doesn't have
      // a value in this sort of assignment failure.
      if (DestTy->Fields[i].Init == 0)
        return 0;

      // If the default initializer should be used, leave the DestElementSources
      // field set to -2.
      DestElementSources[i] = -2;
      continue;
    }
    
    // Okay, we found an input value to use.
    DestElementSources[i] = NextInputValue;
    UsedElements[NextInputValue] = true;
  }
  
  // If there were any unused input values, we fail.
  // TODO: QOI: this should become a note if it really fails.
  for (unsigned i = 0, e = UsedElements.size(); i != e; ++i)
    if (!UsedElements[i])
      return 0;
  
  // It looks like the elements line up, walk through them and see if the types
  // either agree or can be converted.  If the expression is a TupleExpr, we do
  // this conversion in place.
  TupleExpr *TE = dyn_cast<TupleExpr>(E);
  if (TE && TE->NumSubExprs != 1 && TE->NumSubExprs == DestTy->Fields.size()) {
    llvm::SmallVector<Expr*, 8> OrigElts(TE->SubExprs,
                                         TE->SubExprs+TE->NumSubExprs);
    llvm::SmallVector<Identifier, 8> OrigNames;
    if (TE->SubExprNames)
      OrigNames.append(TE->SubExprNames, TE->SubExprNames+TE->NumSubExprs);
    
    for (unsigned i = 0, e = DestTy->Fields.size(); i != e; ++i) {
      // FIXME: This turns the AST into an ASDAG, which is seriously bad.  We
      // should add a more tailored AST representation for this.
      
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
      Elt = HandleConversionToType(Elt, DestTy->getElementType(i), true, TC);
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
  llvm::SmallVector<Expr*, 16> NewElements(DestTy->Fields.size());
  
  Identifier *NewNames = 0;
  
  for (unsigned i = 0, e = DestTy->Fields.size(); i != e; ++i) {
    // FIXME: This turns the AST into an ASDAG, which is seriously bad.  We
    // should add a more tailored AST representation for this.
    
    // Extract the input element corresponding to this destination element.
    unsigned SrcField = DestElementSources[i];
    assert(SrcField != ~0U && "dest field not found?");
  
    if (SrcField == -2U) {
      // Use the default element for the tuple.
      NewElements[i] = 0;
    } else {
      Type *NewEltTy = ETy->getElementType(SrcField);
      Expr *Src = new (TC.Context)
        TupleElementExpr(E, llvm::SMLoc(), SrcField, llvm::SMLoc(), NewEltTy);
      
      // Check to see if the src value can be converted to the destination
      // element type.
      Src = HandleConversionToType(Src, DestTy->getElementType(i), true, TC);
      // TODO: QOI: Include a note about this failure!
      if (Src == 0) return 0;
      NewElements[i] = Src;
    }
    
    if (DestTy->Fields[i].Name.get()) {
      // Allocate the array on the first element with a name.
      if (NewNames == 0)
        NewNames = TC.Context.Allocate<Identifier>(DestTy->Fields.size());
      
      NewNames[i] = DestTy->Fields[i].Name;
    }
  }
  
  // If we got here, the type conversion is successful, create a new TupleExpr.  
  // FIXME: Do this for dependent types, to resolve: foo($0, 4);
  // FIXME: Add default values.
  Expr **NewSE =
    TC.Context.AllocateCopy<Expr*>(NewElements.begin(), NewElements.end());
  
  return new (TC.Context) TupleExpr(llvm::SMLoc(), NewSE, NewNames,
                                    NewElements.size(), llvm::SMLoc(), DestTy);
}

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
    
    Expr *VisitIntegerLiteral(IntegerLiteral *E) {
      return E;
    }
    Expr *VisitDeclRefExpr(DeclRefExpr *E) {
      if (SemaDeclRefExpr(E->D, E->Loc, E->Ty, TC)) return 0;
      return E;
    }
    Expr *VisitUnresolvedDeclRefExpr(UnresolvedDeclRefExpr *E) {
      assert(0 && "UnresolvedDeclRefExpr should be resolved by name binding!");
      return 0;
    }
    Expr *VisitUnresolvedMemberExpr(UnresolvedMemberExpr *E) {
      E->Ty = TC.Context.TheDependentType;
      return E;
    }
    
    Expr *VisitTupleExpr(TupleExpr *E) {
      if (SemaTupleExpr(E->LParenLoc, E->SubExprs, E->SubExprNames,
                        E->NumSubExprs, E->RParenLoc, E->Ty, TC))
        return 0;
      return E;
    }
    Expr *VisitUnresolvedDotExpr(UnresolvedDotExpr *E) {
      int FieldNo = -1;
      if (SemaDotIdentifier(E->SubExpr, E->DotLoc, E->Name, E->NameLoc, E->Ty,
                            FieldNo, TC))
        return 0;
      
      assert(FieldNo != -1 && "Resolved type shouldnt return a dependent expr");
      
      // TODO: Eventually . can be useful for things other than tuples.
      return new (TC.Context) 
        TupleElementExpr(E->SubExpr, E->DotLoc, FieldNo, E->NameLoc, E->Ty);
    }
    Expr *VisitTupleElementExpr(TupleElementExpr *E) {
      // TupleElementExpr is fully resolved.
      assert(!isa<DependentType>(E->Ty));
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
      if (E->Ty == 0)
        E->Ty = TC.Context.TheDependentType;
      return E;
    }
    
    Expr *VisitBinaryExpr(BinaryExpr *E) {
      if (SemaBinaryExpr(E->LHS, E->Fn, E->OpLoc, E->RHS, E->Ty, TC))
        return 0;
      
      return E;
    }
    
    SemaExpressionTree(TypeChecker &tc) : TC(tc) {}
    
    static Expr *WalkFn(Expr *E, Expr::WalkOrder Order, void *set) {
      SemaExpressionTree &SET = *static_cast<SemaExpressionTree*>(set);
      // This is implemented as a postorder walk.
      if (Order == Expr::Walk_PreOrder) {
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

/// getBinOp - Return the ValueDecl for the expression if it is an infix binary
/// operator, otherwise return null.
static ValueDecl *getBinOp(SequenceExpr *E, unsigned Elt) {
  if (Elt >= E->NumElements) return 0;
  
  DeclRefExpr *DRE = dyn_cast<DeclRefExpr>(E->Elements[Elt]);
  if (DRE == 0) return 0;
  
  if (DRE->D->Attrs.InfixPrecedence == -1)
    return 0;
  return DRE->D;
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
    ValueDecl *ThisOp = getBinOp(E, Elt+1);
    
    int ThisPrec = ThisOp ? ThisOp->Attrs.InfixPrecedence : -1;
    if (ThisPrec < (int)MinPrec)
      break;
    
    SMLoc ThisOpLoc = E->Elements[Elt+1]->getLocStart();

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
    ValueDecl *NextOp = getBinOp(E, Elt+3);
    int NextPrec = NextOp ? NextOp->Attrs.InfixPrecedence : -1;
    
    // TODO: All operators are left associative at the moment.
    
    // If the next operator binds more tightly with RHS than we do, evaluate the
    // RHS as a complete subexpression first.  This happens with "x+y*z", where
    // we want to reduce y*z to a single sub-expression of the add.
    if (ThisPrec < NextPrec) {
      // Only parse things on the RHS that bind more tightly than the current
      // operator.
      ReduceBinaryExprs(E, Elt+2, TC, ThisPrec+1);
      
      NextOp = getBinOp(E, Elt+3);
      NextPrec = NextOp ? NextOp->Attrs.InfixPrecedence : -1;
    }
    assert(NextPrec <= ThisPrec && "Recursion didn't work!");

    // Okay, we've finished the parse, form the AST node for the binop now.
    BinaryExpr *RBE =
      new (TC.Context) BinaryExpr(E->Elements[Elt], ThisOp, ThisOpLoc,
                                  E->Elements[Elt+2]);
    if (!SemaBinaryExpr(RBE->LHS, RBE->Fn, RBE->OpLoc, RBE->RHS, RBE->Ty, TC))
      E->Elements[Elt] = RBE;
    
    // The binop and the RHS are now consumed, move everything down.
    memmove(E->Elements+Elt+1, E->Elements+Elt+3, 
            (E->NumElements-Elt-3)*sizeof(E->Elements[0]));
    E->NumElements -= 2;
  }
}

/// ReduceJuxtaposedExprs - Given an element of a sequence expression, check to
/// see if it is the start of a juxtaposed sequence.  If so, reduce it down to
/// a single element.  This allows functions to bind to their arguments.
static void ReduceJuxtaposedExprs(SequenceExpr *E, unsigned Elt,
                                  TypeChecker &TC) {
  // If there are no subsequent expression, then it isn't juxtaposed.
  if (Elt+1 == E->NumElements) return;

  Expr *EltExpr = E->Elements[Elt];
  
  // If this expression isn't a function type, then it doesn't juxtapose.
  if (EltExpr->Ty->getAs<FunctionType>() == 0)
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
  // This is disambiguated based on whether B has function type or not.

  // If EltExpr a directly named function, it should bind very tightly to its
  // single argument:   f a + b    -->  (f a) + b
  // If this is some other expression that returns something of function type,
  // then reduce the subexpression first and then apply it to the function.
  // This gives:    f a + b    -->  f (a + b)
  if (!isa<DeclRefExpr>(EltExpr))
    ReduceBinaryExprs(E, Elt+1, TC);    
  
  Expr *ArgExpr = E->Elements[Elt+1];

  // Okay, we have a function application, analyze it.
  Type *ResultTy = 0;
  if (!SemaApplyExpr(EltExpr, ArgExpr, ResultTy, TC)) {
    E->Elements[Elt] = new (TC.Context) ApplyExpr(EltExpr, ArgExpr, ResultTy);
    // Drop the argument.
    memmove(E->Elements+Elt+1, E->Elements+Elt+2, 
            (E->NumElements-Elt-2)*sizeof(E->Elements[0]));
  } else {
    // Drop the function.  FIXME: This is terrible recovery.
    memmove(E->Elements+Elt, E->Elements+Elt+1,
            (E->NumElements-Elt-1)*sizeof(E->Elements[0]));
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
  for (unsigned i = 0; i != E->NumElements; ++i) {
    // If Elts[i] is the start of a binary expression sequence, reduce it down
    // to a single element.
    ReduceBinaryExprs(E, i, TC);
  }

  assert(E->NumElements != 0 && "Must have at least one element");
  
  // If we reduced this down to a single element, then we're done.
  if (E->NumElements == 1)
    return E->Elements[0];
  
  if (SemaSequenceExpr(E->Elements, E->NumElements, E->Ty, TC)) return 0;
  return E;
}

void SemaExpressionTree::PreProcessBraceExpr(BraceExpr *E) {
  llvm::SmallVector<Expr*, 4> ExcessExprs;
  
  llvm::SmallVector<BraceExpr::ExprOrDecl, 32> NewElements;
  
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
      for (unsigned i = 0, e = ExcessExprs.size(); i != e; ++i)
        NewElements.push_back(ExcessExprs[i]);
    }
  }
  
  // If any of the elements of the braces has a function type (which indicates
  // that a function didn't get called), then produce an error.  We don't do
  // this for the last element in the 'missing semi' case, because the brace
  // expr as a whole has the function result.
  // TODO: What about tuples which contain functions by-value that are dead?
  for (unsigned i = 0, e = NewElements.size()-(E->MissingSemi ?1:0);i != e; ++i)
    if (NewElements[i].is<Expr*>() &&
        isa<FunctionType>(NewElements[i].get<Expr*>()->Ty))
      // TODO: QOI: Add source range.
      TC.error(NewElements[i].get<Expr*>()->getLocStart(),
               "expression resolves to an unevaluated function");
  
  if (E->MissingSemi)
    E->Ty = NewElements.back().get<Expr*>()->Ty;
  else
    E->Ty = TC.Context.TheEmptyTupleType;
  
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
    Type *DestTy;
    
    Expr *VisitIntegerLiteral(IntegerLiteral *E) {
      assert(0 && "Integer literals never have dependent type!");
      return 0;
    }
    Expr *VisitDeclRefExpr(DeclRefExpr *E) {
      return E;
    }
    
    // If this is an UnresolvedMemberExpr, then this provides the type we've
    // been looking for!
    Expr *VisitUnresolvedMemberExpr(UnresolvedMemberExpr *UME) {
      // The only valid type for an UME is a OneOfType.
      OneOfType *DT = DestTy->getAs<OneOfType>();
      if (DT == 0) return 0;
      
      // The oneof type must have an element of the specified name.
      OneOfElementDecl *DED = DT->getElement(UME->Name);
      if (DED == 0) return 0;
      
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
    
    Expr *VisitTupleElementExpr(TupleElementExpr *E) {
      // TupleElementExpr is fully resolved.
      assert(0 && "This node doesn't exist for dependent types");
      return 0;
    }
    
    Expr *VisitApplyExpr(ApplyExpr *E) {
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
            if (DED == 0 || !isa<FunctionType>(DED->Ty)) return 0;
          
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
      
      LastVal = HandleConversionToType(LastVal, DestTy, true, TC);
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
      return E;
    }
    
  public:
    SemaCoerceBottomUp(TypeChecker &tc, Type *destTy) : TC(tc), DestTy(destTy) {
      assert(!isa<DependentType>(DestTy));
    }
    Expr *doIt(Expr *E) {
      return Visit(E);
    }
  };
} // end anonymous namespace.


Expr *SemaCoerceBottomUp::VisitTupleExpr(TupleExpr *E) {
  // If we're providing a type for a tuple expr, we have a couple of
  // different cases.  If the tuple has a single element and the destination
  // type is not a tuple type, then this just recursively forces the scalar
  // type into the single element.
  if (E->NumSubExprs == 1) {
    Expr *ERes = HandleConversionToType(E->SubExprs[0], DestTy, true, TC);
    if (ERes == 0) return 0;
    
    E->SubExprs[0] = ERes;
    return E;
  }
  
  return HandleConversionToType(E, DestTy, true, TC);
}


//===----------------------------------------------------------------------===//
// Type Conversion
//===----------------------------------------------------------------------===//

namespace {
struct RewriteAnonArgExpr {
  Type *FuncInputTy;
  TypeChecker &TC;
  
  RewriteAnonArgExpr(Type *funcInputTy, TypeChecker &tc)
    : FuncInputTy(funcInputTy), TC(tc) {}
  
  static Expr *WalkFn(Expr *E, Expr::WalkOrder Order, void *rewriter) {
    RewriteAnonArgExpr &Rewriter = *static_cast<RewriteAnonArgExpr*>(rewriter);
    Type *FuncInputTy = Rewriter.FuncInputTy;
  
    if (Order == Expr::Walk_PreOrder) {
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
    if (TupleType *TT = dyn_cast<TupleType>(FuncInputTy))
      NumInputArgs = TT->Fields.size();
    
    assert(isa<DependentType>(A->Ty) && "Anon arg already has a type?");
    
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
    if (TupleType *TT = dyn_cast<TupleType>(FuncInputTy)) {
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
BindAndValidateClosureArgs(Expr *Body, Type *FuncInput, TypeChecker &TC) {  
  RewriteAnonArgExpr Rewriter(FuncInput, TC);
  
  // Walk the body and rewrite any anonymous arguments.  Note that this
  // isn't a particularly efficient way to handle this, because we walk subtrees
  // even if they have no anonymous arguments..
  return Body->WalkExpr(RewriteAnonArgExpr::WalkFn, &Rewriter) == 0;
}

/// getTupleFieldForScalarInit - If the specified tuple type can be assigned a
/// scalar value, return the element number that the scalar provides.  For this
/// to be true, the tuple has to be non-empty, and must have at most one element
/// lacking a default value.
static int getTupleFieldForScalarInit(TupleType *TT) {
  if (TT->Fields.size() == 0) return -1;
  
  int FieldWithoutDefault = -1;
  for (unsigned i = 0, e = TT->Fields.size(); i != e; ++i) {
    // Ignore fields with a default value.
    if (TT->Fields[i].Init) continue;

    // If we already saw a field missing a default value, then we cannot assign
    // a scalar to this tuple.
    if (FieldWithoutDefault != -1)
      return -1;
    
    // Otherwise, remember this field number.
    FieldWithoutDefault = i;    
  }
  
  // If all the elements have default values, the scalar initializes the first
  // value in the tuple.
  return FieldWithoutDefault == -1 ? 0 : FieldWithoutDefault;
}

/// HandleScalarConversionToTupleType - Check to see if the destination type
/// (which is known to be a tuple) has a single element that requires an
/// initializer and if the specified expression can convert to that type.  If
/// so, convert the scalar to the tuple type.
static Expr *HandleScalarConversionToTupleType(Expr *E, Type *DestTy,
                                               TypeChecker &TC) {
  TupleType *TT = DestTy->getAs<TupleType>();
  
  // If the destination is a tuple type with at most one element that has no
  // default value, see if the expression's type is convertable to the
  // element type.  This handles assigning 4 to "(a = 4, b : int)".
  int ScalarField = getTupleFieldForScalarInit(TT);
  if (ScalarField == -1) return 0;
  
  Type *ScalarType = TT->getElementType(ScalarField);
  Expr *ERes = HandleConversionToType(E, ScalarType, false, TC);
  if (ERes == 0) return 0;
  
  unsigned NumFields = TT->Fields.size();
  
  // Must allocate space for the AST node.
  Expr **NewSE = TC.Context.Allocate<Expr*>(NumFields);
  
  bool NeedsNames = false;
  for (unsigned i = 0, e = NumFields; i != e; ++i) {
    if (i == (unsigned)ScalarField)
      NewSE[i] = ERes;
    else
      NewSE[i] = 0;
    
    NeedsNames |= TT->Fields[i].Name != Identifier();
  }
  
  // Handle the name if the element is named.
  Identifier *NewName = 0;
  if (NeedsNames) {
    NewName = TC.Context.Allocate<Identifier>(NumFields);
    for (unsigned i = 0, e = NumFields; i != e; ++i)
      NewName[i] = TT->Fields[i].Name;
  }
  
  return new (TC.Context) TupleExpr(E->getLocStart(), NewSE, NewName,
                                    NumFields, SMLoc(), DestTy);
}

/// HandleConversionToType - This is the recursive implementation of
/// ConvertToType.  It does not produce diagnostics, it just returns null on
/// failure.
static Expr *HandleConversionToType(Expr *E, Type *DestTy, bool IgnoreAnonDecls,
                                    TypeChecker &TC) {
  Type *CanDestTy = TC.Context.getCanonicalType(DestTy);
  // If we have an exact match, we're done.
  if (TC.Context.getCanonicalType(E->Ty) == CanDestTy)
    return E;
  
  assert(!isa<DependentType>(DestTy) &&
         "Result of conversion can't be dependent");
  
  if (TupleType *TT = DestTy->getAs<TupleType>()) {
    // If this is a scalar to tuple conversion, form the tuple and return it.
    if (Expr *Res = HandleScalarConversionToTupleType(E, DestTy, TC))
      return Res;
    
    // If the input is a tuple and the output is a tuple, see if we can convert
    // each element.
    if (TupleType *ETy = E->Ty->getAs<TupleType>()) {
      llvm::SmallVector<Identifier, 8> ExprIdentMapping;
      for (unsigned i = 0, e = ETy->Fields.size(); i != e; ++i)
        ExprIdentMapping.push_back(ETy->Fields[i].Name);
      
      if (Expr *Res = ConvertExprToTupleType(E, ExprIdentMapping, TT, TC))
        return Res;
    }
    
    // If the element of the tuple has dependent type and is a TupleExpr, try to
    // convert it.
    if (E->Ty->getAs<DependentType>() && isa<TupleExpr>(E)) {
      // If the tuple expression or destination type have named elements, we
      // have to match them up to handle the swizzle case for when:
      //   (.y = 4, .x = 3)
      // is converted to type:
      //   (.x = int, .y = int)
      llvm::SmallVector<Identifier, 8> Idents(cast<TupleExpr>(E)->NumSubExprs);
      return ConvertExprToTupleType(E, Idents, TT, TC);
    }
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
    Expr *ERes = HandleConversionToType(E, FT->Result, true, TC);
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
  if (isa<DependentType>(E->Ty))
    return SemaCoerceBottomUp(TC, DestTy).doIt(E);
  
  // Could not do the conversion.
  return 0;
}



Expr *TypeChecker::convertToType(Expr *E, Type *DestTy, bool IgnoreAnonDecls,
                                 ConversionReason Reason) {
  if (Expr *ERes = HandleConversionToType(E, DestTy, IgnoreAnonDecls, *this))
    return ERes;
  
  // FIXME: We only have this because diagnostics are terrible right now.  When
  // we have expression underlining this should be removed.
  E->dump();
  
  // TODO: QOI: Source ranges + print the type.
  switch (Reason) {
  case CR_BinOpLHS:
    error(E->getLocStart(), "left hand side of binary operator has type '" +
          E->Ty->getString() + "', expected '" + DestTy->getString() + "'");
    break;
  case CR_BinOpRHS: 
    error(E->getLocStart(),"right hand side of binary operator has type '" +
          E->Ty->getString() + "', expected '" + DestTy->getString() + "'");
    break;
  case CR_FuncApply:
    error(E->getLocStart(), "argument to function invocation has type '" +
          E->Ty->getString() + "', expected '" + DestTy->getString() + "'");
    break;
  case CR_VarInit:
    error(E->getLocStart(),
          "cannot convert initializer type '" + E->Ty->getString() +
          "' to explicitly specified type '" + DestTy->getString() + "'");
    break;
  case CR_TupleInit:
    error(E->getLocStart(),
          "cannot convert default value type '" + E->Ty->getString() +
          "' to explicitly specified type '" + DestTy->getString() + "'");
    break;
  case CR_FuncBody:
    error(E->getLocStart(),
          "function body type '" + E->Ty->getString() +
          "' doesn't match specified prototype '" + DestTy->getString() + "'");
    break;
  }
  
  return 0;
}

//===----------------------------------------------------------------------===//
// Type Validation
//===----------------------------------------------------------------------===//

/// validateType - Types can contain expressions (in the default values for
/// tuple elements), and thus need semantic analysis to ensure that these
/// expressions are valid and that they have the appropriate conversions etc.
///
/// This returns true if the type is invalid.
bool TypeChecker::validateType(Type *&T) {
  assert(T && "Cannot validate null types!");

  // FIXME: Verify that these aren't circular and infinite size.
  
  // If a type has a canonical type, then it is known safe.
  if (T->hasCanonicalTypeComputed()) return false;

  bool IsValid = true;
  
  switch (T->Kind) {
  case UnresolvedTypeKind:
  case BuiltinInt32Kind:
  case DependentTypeKind:
    return false;
  case OneOfTypeKind: {
    OneOfType *OOT = cast<OneOfType>(T);
    for (unsigned i = 0, e = OOT->Elements.size(); i != e; ++i) {
      if (OOT->Elements[i]->ArgumentType == 0) continue;
      IsValid &= !validateType(OOT->Elements[i]->ArgumentType);
      if (!IsValid) break;
    }
    break;
  }
  case NameAliasTypeKind:
    IsValid =!validateType(llvm::cast<NameAliasType>(T)->TheDecl->UnderlyingTy);
    break;
  case TupleTypeKind: {
    TupleType *TT = llvm::cast<TupleType>(T);
    
    // Okay, we found an uncanonicalized tuple type, which might have default
    // values.  If so, we'll potentially have to update it.
    for (unsigned i = 0, e = TT->Fields.size(); i != e; ++i) {
      // The element has *at least* a type or an initializer, so we start by
      // verifying each individually.
      Type *EltTy = TT->Fields[i].Ty;
      if (EltTy && validateType(EltTy)) {
        IsValid = false;
        break;
      }

      Expr *EltInit = TT->Fields[i].Init;
      if (EltInit == 0) continue;
      
      checkBody(EltInit, EltTy, CR_TupleInit, 0);
      if (EltInit == 0) {
        IsValid = false;
        break;
      }
        
      // If both a type and an initializer are specified, make sure the
      // initializer's type agrees with the (redundant) type.
      if (EltTy) {
        EltInit = convertToType(EltInit, EltTy, false, CR_TupleInit);
        if (EltInit == 0) {
          IsValid = false;
          break;
        }
      } else {
        EltTy = EltInit->Ty;
      }

      TT->updateInitializedElementType(i, EltTy, EltInit);
    }
    break;
  }
      
  case FunctionTypeKind: {
    FunctionType *FT = llvm::cast<FunctionType>(T);
    if ((T = FT->Input, validateType(T)) ||
        (T = FT->Result, validateType(T))) {
      IsValid = false;
      break;
    }
    T = FT;
    break;
  }
  case ArrayTypeKind:
    ArrayType *AT = llvm::cast<ArrayType>(T);
    if (T = AT->Base, validateType(T)) {
      IsValid = false;
      break;
    }
    T = AT;
    // FIXME: We need to check AT->Size! (It also has to be convertible to int).
    break;
  }

  // If we determined that this type is invalid, erase it in the caller.
  if (!IsValid) {
    // FIXME: This should set the type to some Error type, which is
    // distinguishable from unresolved.
    T = Context.TheUnresolvedType;
    return true;
  }

  // Now that we decided that this type is ok, get the canonical type for it so
  // that we never reanalyze it again.
  // If it is ever a performance win to avoid computing canonical types, we can
  // just keep a SmallPtrSet of analyzed Types in TypeChecker.
  Context.getCanonicalType(T);
  
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
void TypeChecker::validateAttributes(DeclAttributes &Attrs, Type *Ty) {
  // If the decl has an infix precedence specified, then it must be a function
  // whose input is a two element tuple.
  if (Attrs.InfixPrecedence != -1) {
    bool IsError = true;
    if (FunctionType *FT = dyn_cast<FunctionType>(Ty))
      if (TupleType *TT = dyn_cast<TupleType>(FT->Input))
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
  if (Order == Expr::Walk_PreOrder)
    return E;
  
  // Use getAs to strip off sugar.
  if (E->Ty->getAs<DependentType>() == 0)
    return E;
  
  TypeChecker &TC = *(TypeChecker*)Data;
  E->dump();  // FIXME: This is a gross hack because our diagnostics suck.
  TC.error(E->getLocStart(),
           "ambiguous expression could not resolve a concrete type");
  return 0;
}


/// checkBody - Type check an expression that is used in a top-level
/// context like a var/func body, or tuple default value.  If DestTy is
/// specified, the expression is coerced to the requested type.  The
/// specified ConversionReason is used to produce a diagnostic on error.
///
/// If the body turns out to be a sequence, this returns the single element
/// with the excess in the provided smallvector.
void TypeChecker::checkBody(Expr *&E, Type *DestTy, ConversionReason Res,
                            llvm::SmallVectorImpl<Expr*> *ExcessElements) {
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
    E = convertToType(E, DestTy, false, Res);
  
  // Check the initializer/body to make sure that we succeeded in resolving
  // all of the types contained within it.  We should not have any
  // DependentType's left for subexpressions.
  if (E && E->WalkExpr(DiagnoseUnresolvedTypes, this) == 0)
    E = 0;
}

void TypeChecker::typeCheck(TypeAliasDecl *TAD) {
  validateType(TAD->UnderlyingTy);
}

void TypeChecker::typeCheck(ElementRefDecl *ERD) {
  // If the type is already resolved we're done.  ElementRefDecls are simple.
  if (!isa<DependentType>(ERD->Ty)) return;
  
  if (Type *T = ElementRefDecl::getTypeForPath(ERD->VD->Ty, ERD->AccessPath))
    ERD->Ty = T;
  else {
    error(ERD->getLocStart(), "'" + ERD->Name.str() +
          "' is an invalid index for '" + ERD->VD->Ty->getString() +
          "'");
    // FIXME: This should be "invalid"
    ERD->Ty = Context.TheEmptyTupleType;
  }
}

bool TypeChecker::validateVarName(Type *Ty, DeclVarName *Name) {
  // Check for a type specifier mismatch on this level.
  assert(Ty && "This lookup should never fail");

  // If this is a simple varname, then it matches any type, and we're done.
  if (Name->isSimple())
    return false;

  // If we're peering into an unresolved type, we can't analyze it yet.
  if (Ty->getAs<DependentType>() != 0) return false;

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
          "' which requires " + llvm::Twine(AccessedTuple->Fields.size()) +
          " names, but has " + llvm::Twine(Name->Elements.size()));
    return true;
  }
  
  // Okay, everything looks good at this level, recurse.
  for (unsigned i = 0, e = Name->Elements.size(); i != e; ++i) {
    if (validateVarName(AccessedTuple->Fields[i].Ty, Name->Elements[i]))
      return true;
  }

  return false;
}

void TypeChecker::typeCheck(VarDecl *VD,
                            llvm::SmallVectorImpl<Expr*> &ExcessExprs) {
  if (validateType(VD->Ty)) return;

  // Check Init.  
  if (VD->Init == 0) {
    // If we have no initializer and the type is dependent, then the initializer
    // was invalid and removed.
    if (isa<DependentType>(VD->Ty)) return;
  } else if (isa<DependentType>(VD->Ty)) {
    checkBody(VD->Init, 0, CR_VarInit, &ExcessExprs);
    if (VD->Init)
      VD->Ty = VD->Init->Ty;
  } else {
    // If both a type and an initializer are specified, make sure the
    // initializer's type agrees (or converts) to the redundant type.
    checkBody(VD->Init, VD->Ty, CR_VarInit, &ExcessExprs);
  }
  
  validateAttributes(VD->Attrs, VD->Ty);
  
  // If the VarDecl had a name specifier, verify that it lines up with the
  // actual type of the VarDecl.
  if (VD->NestedName && validateVarName(VD->Ty, VD->NestedName))
    VD->NestedName = 0;
}


void TypeChecker::typeCheck(FuncDecl *FD,
                            llvm::SmallVectorImpl<Expr*> &ExcessExprs) {
  if (validateType(FD->Ty)) return;

  // Validate that the body's type matches the function's type if this isn't a
  // external function.
  if (FD->Init)
    checkBody(FD->Init, FD->Ty, CR_FuncBody, &ExcessExprs);
  
  validateAttributes(FD->Attrs, FD->Ty);
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
    llvm::cast_or_null<BraceExpr>(SemaExpressionTree::doIt(TUD->Body, TC));
}
