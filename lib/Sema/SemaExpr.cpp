//===--- SemaExpr.cpp - Swift Expression Semantic Analysis ----------------===//
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
//  This file implements semantic analysis for Swift expressions.
//
//===----------------------------------------------------------------------===//

#include "swift/Sema/SemaExpr.h"
#include "swift/Sema/Sema.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/ExprVisitor.h"
#include "swift/AST/Type.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/ADT/PointerIntPair.h"
#include "llvm/ADT/Twine.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/SMLoc.h"
using namespace swift;
using llvm::NullablePtr;
using llvm::isa;
using llvm::cast;
using llvm::dyn_cast;

//===----------------------------------------------------------------------===//
// Expression Semantic Analysis Routines
//===----------------------------------------------------------------------===//

// Each expression node has an implementation here, which takes the properties
// of the expression, validates it and computes a result type.  This is shared
// logic between the Sema Action implementations and type inferencing code.
//
// These each produce diagnostics and return true on error.  On success, they
// may mutate the input values, then return false.


static bool SemaIntegerLiteral(Type *&ResultTy, SemaExpr &SE) {
  ResultTy = SE.S.Context.TheInt32Type;
  return false;
}

static bool SemaDeclRefExpr(ValueDecl *D, llvm::SMLoc Loc, Type *&ResultTy,
                            SemaExpr &SE) {
  if (D == 0) {
    SE.Error(Loc, "use of undeclared identifier");
    return true;
  }
  
  // TODO: QOI: If the decl had an "invalid" bit set, then return the error
  // object to improve error recovery.
  
  ResultTy = D->Ty;
  return false;
}

static bool SemaBraceExpr(llvm::SMLoc LBLoc, 
                          llvm::PointerUnion<Expr*, ValueDecl*> *Elements,
                          unsigned NumElements,
                          bool HasMissingSemi, llvm::SMLoc RBLoc, 
                          Type *&ResultTy, SemaExpr &SE) {
  // If any of the elements of the braces has a function type (which indicates
  // that a function didn't get called), then produce an error.  We don't do
  // this for the last element in the 'missing semi' case, because the brace
  // expr as a whole has the function result.
  // TODO: What about tuples which contain functions by-value that are dead?
  for (unsigned i = 0; i != NumElements-(HasMissingSemi ? 1 : 0); ++i)
    if (Elements[i].is<Expr*>() &&
        isa<FunctionType>(Elements[i].get<Expr*>()->Ty))
      // TODO: QOI: Add source range.
      SE.Error(Elements[i].get<Expr*>()->getLocStart(),
               "expression resolves to an unevaluated function");
  
  if (HasMissingSemi)
    ResultTy = Elements[NumElements-1].get<Expr*>()->Ty;
  else
    ResultTy = SE.S.Context.TheEmptyTupleType;
  
  return false;
}

static bool SemaDotIdentifier(Expr *E, llvm::SMLoc DotLoc,
                              Identifier Name, llvm::SMLoc NameLoc,
                              Type *&ResultTy, int &FieldNo, SemaExpr &SE) {
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
    OneOfDecl *DD = DT->TheDecl;
    if (DD->NumElements == 1 && DD->Elements[0]->ArgumentType)
      ETy = DD->Elements[0]->ArgumentType;
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
        if (Value >= TT->NumFields) {
          SE.Error(NameLoc, "field number is too large for tuple");
          return true;
        }
        
        FieldNo = Value;
        ResultTy = TT->getElementType(Value);
        return false;
      }
    }
    
    // Otherwise, we just have an unknown field name.
    SE.Error(NameLoc, "unknown field in tuple");
    return true;
  }
  
  SE.Error(DotLoc, "base type of field access has no fields");
  return true;
}


static bool SemaTupleExpr(llvm::SMLoc LPLoc, Expr **SubExprs,
                          Identifier *SubExprNames,
                          unsigned NumSubExprs, llvm::SMLoc RPLoc,
                          Type *&ResultTy, SemaExpr &SE) {
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
  
  ResultTy = SE.S.Context.getTupleType(ResultTyElts.data(), NumSubExprs);
  return false;
}

static bool SemaApplyExpr(Expr *&E1, Expr *&E2, Type *&ResultTy, SemaExpr &SE) {
  if (isa<DependentType>(E1->Ty)) {
    ResultTy = E1->Ty;
    return false;
  }
  
  // If this expression in the sequence is just a floating value that isn't
  // a function, then we have a discarded value, such as "4 5".  Just return
  // true so that it gets properly sequenced.
  FunctionType *FT = dyn_cast<FunctionType>(E1->Ty);
  if (FT == 0) {
    SE.Error(E1->getLocStart(),
             "function application requires value of function type");
    return true;
  }
  
  // Otherwise, we do have a function application.  Check that the argument
  // type matches the expected type of the function.
  E2 = SE.ConvertToType(E2, FT->Input, false, SemaExpr::CR_FuncApply);
  if (E2 == 0)
    return true;
  
  ResultTy = FT->Result;
  return false;
}

/// SemaSequenceExpr - Perform semantic analysis for sequence expressions.
static bool SemaSequenceExpr(Expr **Elements, unsigned NumElements,
                             Type *&ResultTy, SemaExpr &SE) {
  ResultTy = Elements[NumElements-1]->Ty;
  return false;
}

/// SemaBinaryExpr - Perform semantic analysis of binary expressions.
/// OpFn is null if this is an assignment (FIXME: we don't have generics yet).
static bool SemaBinaryExpr(Expr *&LHS, ValueDecl *OpFn,
                           llvm::SMLoc OpLoc, Expr *&RHS, Type *&ResultTy,
                           SemaExpr &SE) {
  if (isa<DependentType>(LHS->Ty)) {
    ResultTy = SE.S.Context.TheDependentType;
    return false; 
  }
  
  // If this is an assignment, then we coerce the RHS to the LHS.
  if (OpFn == 0) {
    RHS = SE.ConvertToType(RHS, LHS->Ty, false, SemaExpr::CR_BinOpRHS);
    if (LHS == 0) return true;
    
    ResultTy = SE.S.Context.TheEmptyTupleType;
    return false;
  }
  
  // Parser verified that OpFn has an Infix Precedence.  Sema verified that OpFn
  // only has InfixPrecedence if it takes a 2 element tuple as input.
  assert(OpFn->Attrs.InfixPrecedence != -1 &&
         "Sema and parser should verify that only binary predicates are used"); 
  FunctionType *FnTy = llvm::cast<FunctionType>(OpFn->Ty);
  TupleType *Input = llvm::cast<TupleType>(FnTy->Input);
  assert(Input->NumFields == 2 && "Sema error validating infix fn type");
  
  // Verify that the LHS/RHS have the right type and do conversions as needed.
  LHS = SE.ConvertToType(LHS, Input->getElementType(0), false,
                         SemaExpr::CR_BinOpLHS);
  if (LHS == 0) return true;
  
  RHS = SE.ConvertToType(RHS, Input->getElementType(1), false,
                         SemaExpr::CR_BinOpRHS);
  if (RHS == 0) return true;
  
  ResultTy = FnTy->Result;
  return false;
}

//===----------------------------------------------------------------------===//
// Action Implementations
//===----------------------------------------------------------------------===//

/// getJuxtapositionGreediness - This returns an enum that specifies how
/// tightly juxtaposition binds for a subexpression.  This allows functions
/// to bind to their arguments tightly.  When this returns something other
/// than NonGreedy, Sema is guaranteeing that juxtaposition will result in an
/// ApplyExpr.
SemaExpr::JuxtapositionGreediness
SemaExpr::getJuxtapositionGreediness(Expr *E) const {
  assert(E && "Expression shouldn't be null");
  if (!llvm::isa<FunctionType>(E->Ty))
    return JG_NonGreedy;
  
  // If E is a function, it should bind very tightly to its argument:
  // f a + b    -->  (f a) + b
  if (isa<DeclRefExpr>(E))
    return JG_LocallyGreedy;
  
  // If E is some expression that returns a function, it should bind loosely to
  // its argument:  f a + b    -->  f (a + b)
  return JG_Greedy;
}


NullablePtr<Expr> SemaExpr::ActOnNumericConstant(llvm::StringRef Text,
                                                 llvm::SMLoc Loc) {
  // The integer literal must fit in 64-bits.
  unsigned long long Val;
  if (Text.getAsInteger(0, Val)) {
    Error(Loc, "invalid immediate for integer literal, value too large");
    Text = "1";
  }
  
  
  Type *ResultTy = 0;
  if (SemaIntegerLiteral(ResultTy, *this)) return 0;
  return new (S.Context) IntegerLiteral(Text, Loc, ResultTy);
}

NullablePtr<Expr> 
SemaExpr::ActOnDollarIdentExpr(llvm::StringRef Text, llvm::SMLoc Loc) {
  ValueDecl *D = S.decl.GetAnonDecl(Text, Loc);
  if (D == 0) return 0;
  
  Type *ResultTy = 0;
  if (SemaDeclRefExpr(D, Loc, ResultTy, *this)) return 0;
  
  return new (S.Context) DeclRefExpr(D, Loc, D->Ty);
}

NullablePtr<Expr> 
SemaExpr::ActOnIdentifierExpr(llvm::StringRef Text, llvm::SMLoc Loc) {
  ValueDecl *D = S.decl.LookupValueName(S.Context.getIdentifier(Text));
  
  Type *ResultTy = 0;
  if (SemaDeclRefExpr(D, Loc, ResultTy, *this)) return 0;
  
  return new (S.Context) DeclRefExpr(D, Loc, D->Ty);
}

llvm::NullablePtr<Expr> SemaExpr::
ActOnScopedIdentifierExpr(llvm::StringRef ScopeName, llvm::SMLoc ScopeLoc,
                          llvm::SMLoc ColonColonLoc,
                          llvm::StringRef Name, llvm::SMLoc NameLoc) {
  // Note: this is very simplistic support for scoped name lookup, extend when
  // needed.
  Type *TypeScope = S.Context.getNamedType(S.Context.getIdentifier(ScopeName));
  if (TypeScope == 0) {
    Error(ScopeLoc, "unknown type name '" + ScopeName + "' in expression");
    return 0;
  }
  
  // Look through type aliases etc.
  OneOfType *DT = dyn_cast<OneOfType>(S.Context.getCanonicalType(TypeScope));
  
  // Reject things like int::x.
  if (DT == 0) {
    Error(ScopeLoc, "invalid type '" + ScopeName + "' for scoped access");
    return 0;
  }
  
  if (DT->TheDecl->NumElements == 0) {
    Error(ScopeLoc, "oneof '" + ScopeName +
          "' is not complete or has no elements");
    return 0;
  }
  
  OneOfElementDecl *Elt = DT->TheDecl->getElement(S.Context.getIdentifier(Name));
  if (Elt == 0) {
    Error(ScopeLoc, "'" + Name + "' is not a member of '" + ScopeName + "'");
    return 0;
  }

  return new (S.Context) DeclRefExpr(Elt, ScopeLoc, Elt->Ty);
}

llvm::NullablePtr<Expr>
SemaExpr::ActOnUnresolvedMemberExpr(llvm::SMLoc ColonLoc, llvm::SMLoc NameLoc,
                                    llvm::StringRef Name) {
  // Handle :foo by just making an AST node.
  return new (S.Context) UnresolvedMemberExpr(ColonLoc, NameLoc,
                                              S.Context.getIdentifier(Name),
                                              S.Context.TheDependentType);
}


NullablePtr<Expr> 
SemaExpr::ActOnBraceExpr(llvm::SMLoc LBLoc,
                         const llvm::PointerUnion<Expr*, ValueDecl*> *Elements,
                         unsigned NumElements, bool HasMissingSemi,
                         llvm::SMLoc RBLoc) {
  llvm::PointerUnion<Expr*, ValueDecl*> *NewElements = 
  (llvm::PointerUnion<Expr*, ValueDecl*> *)
  S.Context.Allocate(sizeof(*Elements)*NumElements, 8);
  memcpy(NewElements, Elements, sizeof(*Elements)*NumElements);
  
  Type *ResultTy = 0;
  if (SemaBraceExpr(LBLoc, NewElements, NumElements, HasMissingSemi, RBLoc,
                    ResultTy, *this))
    return 0;
  
  return new (S.Context) BraceExpr(LBLoc, NewElements, NumElements,
                                   HasMissingSemi, RBLoc, ResultTy);
}

llvm::NullablePtr<Expr>
SemaExpr::ActOnDotIdentifier(Expr *E, llvm::SMLoc DotLoc,
                             llvm::StringRef NameStr,
                             llvm::SMLoc NameLoc) {
  Identifier Name = S.Context.getIdentifier(NameStr);
  
  Type *ResultTy = 0;
  int FieldNo = -1;
  if (SemaDotIdentifier(E, DotLoc, Name, NameLoc, ResultTy, FieldNo, *this))
    return 0;
  
  // If the field number is -1, the the base expression is dependent.
  if (FieldNo == -1)
    return new (S.Context) UnresolvedDotExpr(E, DotLoc, Name, NameLoc,ResultTy);
  
  // TODO: Eventually . can be useful for things other than tuples.
  return new (S.Context) TupleElementExpr(E, DotLoc, FieldNo, NameLoc,ResultTy);
}

llvm::NullablePtr<Expr>
SemaExpr::ActOnArraySubscript(Expr *Base, llvm::SMLoc LLoc, Expr *Idx,
                              llvm::SMLoc RLoc) {
  // FIXME: Implement.  This should lookup "subscript(Base, Idx)" as a function
  // call.
  return Base;
}


NullablePtr<Expr> 
SemaExpr::ActOnTupleExpr(llvm::SMLoc LPLoc, Expr *const *SubExprs,
                         const Identifier *SubExprNames,
                         unsigned NumSubExprs, llvm::SMLoc RPLoc) {
  
  Expr **NewSubExprs =
    (Expr**)S.Context.Allocate(sizeof(SubExprs[0])*NumSubExprs, 8);
  memcpy(NewSubExprs, SubExprs, sizeof(SubExprs[0])*NumSubExprs);

  Identifier *NewSubExprsNames = 0;
  if (SubExprNames) {
    NewSubExprsNames =
      (Identifier*)S.Context.Allocate(sizeof(SubExprNames[0])*NumSubExprs, 8);
    memcpy(NewSubExprsNames, SubExprNames, sizeof(SubExprNames[0])*NumSubExprs);
  }

  Type *ResultTy = 0;
  if (SemaTupleExpr(LPLoc, NewSubExprs, NewSubExprsNames, NumSubExprs, RPLoc,
                    ResultTy, *this))
    return 0;
  
  return new (S.Context) TupleExpr(LPLoc, NewSubExprs, NewSubExprsNames,
                                   NumSubExprs, RPLoc, ResultTy);
}

/// ActOnJuxtaposition - This is invoked whenever the parser sees a
/// juxtaposition operation.  If the two expressions can be applied, this
/// returns the new expression (which can be null) and false, otherwise it
/// return null and true to indicate that they are sequenced.
llvm::PointerIntPair<Expr*, 1, bool>
SemaExpr::ActOnJuxtaposition(Expr *E1, Expr *E2) {
  // If this expression in the sequence is just a floating value that isn't
  // a function, then we have a discarded value, such as "4 5".  Just return
  // true so that it gets properly sequenced.  If the input is a dependent type,
  // we assume that it is a function so that ($0 1 2 3) binds correctly.  If it
  // is not a function, parens or ; can be used to disambiguate.
  if (!isa<FunctionType>(E1->Ty) && !isa<DependentType>(E1->Ty))
    return llvm::PointerIntPair<Expr*, 1, bool>(0, true);
  
  // Okay, we have a function application, analyze it.
  Type *ResultTy = 0;
  if (SemaApplyExpr(E1, E2, ResultTy, *this))
    return llvm::PointerIntPair<Expr*, 1, bool>(0, false);
  
  E1 = new (S.Context) ApplyExpr(E1, E2, ResultTy);
  return llvm::PointerIntPair<Expr*, 1, bool>(E1, false);
}


llvm::NullablePtr<Expr>
SemaExpr::ActOnSequence(Expr **Elements, unsigned NumElements) {
  assert(NumElements != 0 && "Empty sequence isn't possible");
  
  Expr **NewElements =(Expr**)
  S.Context.Allocate(sizeof(*NewElements)*NumElements, 8);
  memcpy(NewElements, Elements, sizeof(*NewElements)*NumElements);
  
  Type *ResultTy = 0;
  if (SemaSequenceExpr(NewElements, NumElements, ResultTy, *this)) return 0;
  
  return new (S.Context) SequenceExpr(NewElements, NumElements, ResultTy);
}

NullablePtr<Expr> 
SemaExpr::ActOnBinaryExpr(Expr *LHS, ValueDecl *OpFn,
                          llvm::SMLoc OpLoc, Expr *RHS) {
  Type *ResultTy = 0;
  if (SemaBinaryExpr(LHS, OpFn, OpLoc, RHS, ResultTy, *this)) return 0;
  
  return new (S.Context) BinaryExpr(LHS, OpFn, OpLoc, RHS, ResultTy);
}

//===----------------------------------------------------------------------===//
// Expression Reanalysis - SemaExpressionTree
//===----------------------------------------------------------------------===//

static Expr *HandleConversionToType(Expr *E, Type *DestTy, bool IgnoreAnonDecls,
                                    SemaExpr &SE);


/// ConvertExprToTupleType - Given an expression that has tuple type, convert it
/// to have some other tuple type.
///
/// The caller gives us a list of the expressions named arguments and a count of
/// tuple elements for E in the IdentList+NumIdents array.  DestTy specifies the
/// type to convert to, which is known to be a TupleType.
static Expr *
ConvertExprToTupleType(Expr *E, Identifier *IdentList, unsigned NumIdents,
                       TupleType *DestTy, SemaExpr &SE) {
  
  // Check to see if this conversion is ok by looping over all the destination
  // elements and seeing if they are provided by the input.
  
  // Keep track of which input elements are used.
  // TODO: Record where the destination elements came from in the AST.
  llvm::SmallVector<bool, 16> UsedElements(NumIdents);
  llvm::SmallVector<int, 16>  DestElementSources(DestTy->NumFields, -1);
  
  
  // First off, see if we can resolve any named values from matching named
  // inputs.
  for (unsigned i = 0, e = DestTy->NumFields; i != e; ++i) {
    const TupleTypeElt &DestElt = DestTy->Fields[i];
    // If this destination field is named, first check for a matching named
    // element in the input, from any position.
    if (DestElt.Name.get() == 0) continue;
    
    int InputElement = -1;
    for (unsigned j = 0; j != NumIdents; ++j)
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
  for (unsigned i = 0, e = DestTy->NumFields; i != e; ++i) {
    // If we already found an input to satisfy this output, we're done.
    if (DestElementSources[i] != -1) continue;
    
    // Scan for an unmatched unnamed input value.
    while (1) {
      // If we didn't find any input values, we ran out of inputs to use.
      if (NextInputValue == NumIdents)
        break;
      
      // If this input value is unnamed and unused, use it!
      if (!UsedElements[NextInputValue] && IdentList[NextInputValue].get() == 0)
        break;
      
      ++NextInputValue;
    }

    // If we ran out of input values, we either don't have enough sources to
    // fill the dest (as in when assigning (1,2) to (int,int,int), or we ran out
    // and default values should be used.
    if (NextInputValue == NumIdents) {
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
  if (TE && TE->NumSubExprs != 1 && TE->NumSubExprs == DestTy->NumFields) {
    llvm::SmallVector<Expr*, 8> OrigElts(TE->SubExprs,
                                         TE->SubExprs+TE->NumSubExprs);
    llvm::SmallVector<Identifier, 8> OrigNames;
    if (TE->SubExprNames)
      OrigNames.append(TE->SubExprNames, TE->SubExprNames+TE->NumSubExprs);
    
    for (unsigned i = 0, e = DestTy->NumFields; i != e; ++i) {
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
      Elt = HandleConversionToType(Elt, DestTy->getElementType(i), true, SE);
      // TODO: QOI: Include a note about this failure!
      if (Elt == 0) return 0;
      TE->SubExprs[i] = Elt;
      
      if (DestTy->Fields[i].Name.get()) {
        // Allocate the array on the first element with a name.
        if (TE->SubExprNames == 0) {
          TE->SubExprNames =
          (Identifier*)SE.S.Context.Allocate(sizeof(Identifier)*
                                             DestTy->NumFields, 8);
          memset(TE->SubExprNames, 0, sizeof(Identifier)*DestTy->NumFields);
        }
        
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
  llvm::SmallVector<Expr*, 16> NewElements(DestTy->NumFields);
  
  Identifier *NewNames = 0;
  
  for (unsigned i = 0, e = DestTy->NumFields; i != e; ++i) {
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
      Expr *Src = new (SE.S.Context)
        TupleElementExpr(E, llvm::SMLoc(), SrcField, llvm::SMLoc(), NewEltTy);
      
      // Check to see if the src value can be converted to the destination
      // element type.
      Src = HandleConversionToType(Src, DestTy->getElementType(i), true, SE);
      // TODO: QOI: Include a note about this failure!
      if (Src == 0) return 0;
      NewElements[i] = Src;
    }
    
    if (DestTy->Fields[i].Name.get()) {
      // Allocate the array on the first element with a name.
      if (NewNames == 0) {
        NewNames = (Identifier*)SE.S.Context.Allocate(sizeof(Identifier)*
                                                      DestTy->NumFields, 8);
        memset(NewNames, 0, sizeof(Identifier)*DestTy->NumFields);
      }
      
      NewNames[i] = DestTy->Fields[i].Name;
    }
  }
  
  // If we got here, the type conversion is successful, create a new TupleExpr.  
  // FIXME: Do this for dependent types, to resolve: foo($0, 4);
  // FIXME: Add default values.
  Expr **NewSE =
  (Expr**)SE.S.Context.Allocate(sizeof(Expr*)*DestTy->NumFields, 8);
  memcpy(NewSE, NewElements.data(), sizeof(Expr*)*DestTy->NumFields);
  
  return new (SE.S.Context) TupleExpr(llvm::SMLoc(), NewSE, NewNames,
                                      NewElements.size(), llvm::SMLoc(),DestTy);
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
    SemaExpr &SE;
    
    Expr *VisitIntegerLiteral(IntegerLiteral *E) {
      if (SemaIntegerLiteral(E->Ty, SE)) return 0;
      return E;
    }
    Expr *VisitDeclRefExpr(DeclRefExpr *E) {
      if (SemaDeclRefExpr(E->D, E->Loc, E->Ty, SE)) return 0;
      return E;
    }
    Expr *VisitUnresolvedMemberExpr(UnresolvedMemberExpr *E) {
      return E;
    }
    
    Expr *VisitTupleExpr(TupleExpr *E) {
      for (unsigned i = 0, e = E->NumSubExprs; i != e; ++i)
        if (E->SubExprs[i])
          if ((E->SubExprs[i] = Visit(E->SubExprs[i])) == 0)
            return 0;
      
      if (SemaTupleExpr(E->LParenLoc, E->SubExprs, E->SubExprNames,
                        E->NumSubExprs, E->RParenLoc, E->Ty, SE)) return 0;
      return E;
    }
    Expr *VisitUnresolvedDotExpr(UnresolvedDotExpr *E) {
      int FieldNo = -1;
      if ((E->SubExpr = Visit(E->SubExpr)) == 0 ||
          SemaDotIdentifier(E->SubExpr, E->DotLoc, E->Name, E->NameLoc, E->Ty,
                            FieldNo, SE))
        return 0;
      
      assert(FieldNo != -1 && "Resolved type should return a dependent expr");
      
      // TODO: Eventually . can be useful for things other than tuples.
      return new (SE.S.Context) 
        TupleElementExpr(E->SubExpr, E->DotLoc, FieldNo, E->NameLoc, E->Ty);
    }
    Expr *VisitTupleElementExpr(TupleElementExpr *E) {
      // TupleElementExpr is fully resolved.
      assert(!isa<DependentType>(E->Ty));
      E->SubExpr = Visit(E->SubExpr);
      return E;
    }
    
    Expr *VisitApplyExpr(ApplyExpr *E) {
      if ((E->Fn = Visit(E->Fn)) == 0 ||
          (E->Arg = Visit(E->Arg)) == 0 ||
          SemaApplyExpr(E->Fn, E->Arg, E->Ty, SE))
        return 0;
      return E;
    }
    Expr *VisitSequenceExpr(SequenceExpr *E) {
      for (unsigned i = 0, e = E->NumElements; i != e; ++i)
        if ((E->Elements[i] = Visit(E->Elements[i])) == 0)
          return 0;
      
      if (SemaSequenceExpr(E->Elements, E->NumElements, E->Ty, SE)) return 0;
      return E;
    }
    Expr *VisitBraceExpr(BraceExpr *E) {
      for (unsigned i = 0, e = E->NumElements; i != e; ++i)
        if (Expr *SubExpr = E->Elements[i].dyn_cast<Expr*>()) {
          E->Elements[i] = Visit(SubExpr);
          if (E->Elements[i].get<Expr*>() == 0) return 0;
        } else if (Expr *Init = E->Elements[i].get<ValueDecl*>()->Init) {
          if ((Init = Visit(Init)) == 0) return 0;
          E->Elements[i].get<ValueDecl*>()->Ty = Init->Ty;
          E->Elements[i].get<ValueDecl*>()->Init = Init;
        }
      
      if (SemaBraceExpr(E->LBLoc, E->Elements, E->NumElements,
                        E->MissingSemi, E->RBLoc, E->Ty, SE)) return 0;
      return E;
    }
    Expr *VisitClosureExpr(ClosureExpr *E) {
      // Anon-decls within a nested closure will have already been resolved, so
      // we don't need to recurse into it.  This also prevents N^2 re-sema
      // activity with lots of nested closures.
      return E;      
    }
    Expr *VisitBinaryExpr(BinaryExpr *E) {
      if ((E->LHS = Visit(E->LHS)) == 0 ||
          (E->RHS = Visit(E->RHS)) == 0 ||
          SemaBinaryExpr(E->LHS, E->Fn, E->OpLoc, E->RHS, E->Ty, SE))
        return 0;
      
      return E;
    }
    
  public:
    SemaExpressionTree(SemaExpr &se) : SE(se) {}
    Expr *doIt(Expr *E) {
      return Visit(E);
    }
  };
} // end anonymous namespace.

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
    SemaExpr &SE;
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
      OneOfElementDecl *DED = DT->TheDecl->getElement(UME->Name);
      if (DED == 0) return 0;
      
      // If it does, then everything is good, resolve the reference.
      return new (SE.S.Context) DeclRefExpr(DED, UME->ColonLoc, DED->Ty);
    }  
    
    Expr *VisitTupleExpr(TupleExpr *E);
    
    Expr *VisitUnresolvedDotExpr(UnresolvedDotExpr *E) {
      return E;
    }
    
    Expr *VisitTupleElementExpr(TupleElementExpr *E) {
      // TupleElementExpr is fully resolved.
      assert(0 && "This node doesn't exist for dependent types");
      return 0;
    }
    
    Expr *VisitApplyExpr(ApplyExpr *E) {
      // If we have ":f x" and the result type of the Apply is a OneOfType, then
      // :f must be an element constructor for the oneof value.  Note that
      // handling this syntactically causes us to reject "(:f) x" as ambiguous.
      if (UnresolvedMemberExpr *UME = dyn_cast<UnresolvedMemberExpr>(E->Fn))
        if (OneOfType *DT = DestTy->getAs<OneOfType>()) {
          // The oneof type must have an element of the specified name.
          OneOfElementDecl *DED = DT->TheDecl->getElement(UME->Name);
          if (DED == 0) return 0;
          
          E->Fn = new (SE.S.Context) DeclRefExpr(DED, UME->ColonLoc, DED->Ty);
          if (SemaApplyExpr(E->Fn, E->Arg, E->Ty, SE))
            return 0;
        }
      return E;
    }
    Expr *VisitSequenceExpr(SequenceExpr *E) {
      // FIXME: Apply to last value of sequence.
      return E;
    }
    Expr *VisitBraceExpr(BraceExpr *E) {
      assert(E->MissingSemi && "Can't have dependent type when return ()");
      assert(E->NumElements && "Can't have 0 elements + missing semi");
      assert(E->Elements[E->NumElements-1].is<Expr*>() && "Decl is ()");
 
      Expr *LastVal = E->Elements[E->NumElements-1].get<Expr*>();
      
      LastVal = HandleConversionToType(LastVal, DestTy, true, SE);
      if (LastVal == 0) return 0;

      // Update the end of the brace expression.
      E->Elements[E->NumElements-1] = LastVal;
      return E;
    }
    Expr *VisitClosureExpr(ClosureExpr *E) {
      return E;      
    }
    Expr *VisitBinaryExpr(BinaryExpr *E) {
      return E;
    }
    
  public:
    SemaCoerceBottomUp(SemaExpr &se, Type *destTy) : SE(se), DestTy(destTy) {
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
    Expr *ERes = HandleConversionToType(E->SubExprs[0], DestTy, true, SE);
    if (ERes == 0) return 0;
    
    E->SubExprs[0] = ERes;
    return E;
  }
  
  return HandleConversionToType(E, DestTy, true, SE);
}


//===----------------------------------------------------------------------===//
// Utility Functions
//===----------------------------------------------------------------------===//


/// BindAndValidateClosureArgs - The specified list of anonymous closure
/// arguments was bound to a closure function with the specified input
/// arguments.  Validate the argument list and, if valid, allocate and return
/// a pointer to the argument to be used for the ClosureExpr.
static llvm::NullablePtr<AnonDecl> *
BindAndValidateClosureArgs(Expr *&Body, Type *FuncInput, SemaDecl &SD) {
  const llvm::NullablePtr<AnonDecl> *AnonArgs = SD.AnonClosureArgs.data();
  unsigned NumAnonArgs = SD.AnonClosureArgs.size();
  
  // If the input to the function is a non-tuple, only $0 is valid, if it is a
  // tuple, then $0..$N are valid depending on the number of inputs to the
  // tuple.
  unsigned NumInputArgs = 1;
  if (TupleType *TT = dyn_cast<TupleType>(FuncInput))
    NumInputArgs = TT->NumFields;
  
  // Verify that the code didn't use too many anonymous arguments, e.g. using _4
  // when the bound function only has 2 inputs.
  if (NumAnonArgs > NumInputArgs) {
    for (unsigned i = NumInputArgs; i != NumAnonArgs; ++i) {
      // Ignore elements not used.
      if (AnonArgs[i].isNull()) continue;
      
      SD.Error(AnonArgs[i].get()->UseLoc,
               "use of invalid anonymous argument, with number higher than"
               " # arguments to bound function");
    }
    NumAnonArgs = NumInputArgs;
  }
  
  // Return the right number of inputs.
  llvm::NullablePtr<AnonDecl> *NewInputs =(llvm::NullablePtr<AnonDecl>*)
    SD.S.Context.Allocate(sizeof(*NewInputs)*NumInputArgs, 8);
  for (unsigned i = 0, e = NumInputArgs; i != e; ++i)
    if (i < NumAnonArgs)
      NewInputs[i] = AnonArgs[i];
    else
      NewInputs[i] = 0;
  
  // Assign the AnonDecls their actual concrete types now that we know the
  // context they are being used in.
  if (TupleType *TT = dyn_cast<TupleType>(FuncInput)) {
    for (unsigned i = 0, e = NumAnonArgs; i != e; ++i) {
      if (NewInputs[i].isNull()) continue;
      NewInputs[i].get()->Ty = TT->getElementType(i);
    }
  } else if (NumInputArgs) {
    assert(NumInputArgs == 1 && NewInputs[0].isNonNull() &&
           "Must have unary case");
    assert(isa<DependentType>(NewInputs[0].get()->Ty) &&
           "AnonDecl doesn't have dependent type?");
    NewInputs[0].get()->Ty = FuncInput;
  }

  // We used/consumed the anonymous closure arguments.
  SD.AnonClosureArgs.clear();
  return NewInputs;
}

/// getTupleFieldForScalarInit - If the specified tuple type can be assigned a
/// scalar value, return the element number that the scalar provides.  For this
/// to be true, the tuple has to be non-empty, and must have at most one element
/// lacking a default value.
static int getTupleFieldForScalarInit(TupleType *TT) {
  if (TT->NumFields == 0) return -1;
  
  int FieldWithoutDefault = -1;
  for (unsigned i = 0, e = TT->NumFields; i != e; ++i) {
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
                                               SemaExpr &SE) {
  TupleType *TT = DestTy->getAs<TupleType>();
  
  // If the destination is a tuple type with at most one element that has no
  // default value, see if the expression's type is convertable to the
  // element type.  This handles assigning 4 to "(a = 4, b : int)".
  int ScalarField = getTupleFieldForScalarInit(TT);
  if (ScalarField == -1) return 0;
  
  Type *ScalarType = TT->getElementType(ScalarField);
  Expr *ERes = HandleConversionToType(E, ScalarType, false, SE);
  if (ERes == 0) return 0;
  
  unsigned NumFields = TT->NumFields;
  
  // Must allocate space for the AST node.
  Expr **NewSE = (Expr**)SE.S.Context.Allocate(sizeof(Expr*)*NumFields, 8);
  
  bool NeedsNames = false;
  for (unsigned i = 0, e = NumFields; i != e; ++i) {
    if (i == ScalarField)
      NewSE[i] = ERes;
    else
      NewSE[i] = 0;
    
    NeedsNames |= TT->Fields[i].Name != Identifier();
  }
  
  // Handle the name if the element is named.
  Identifier *NewName = 0;
  if (NeedsNames) {
    NewName = (Identifier*)SE.S.Context.Allocate(sizeof(Identifier) *
                                                 NumFields, 8);
    for (unsigned i = 0, e = NumFields; i != e; ++i)
      NewName[i] = TT->Fields[i].Name;
  }
  
  return new (SE.S.Context) TupleExpr(E->getLocStart(), NewSE, NewName,
                                      NumFields, llvm::SMLoc(), DestTy);
}


/// HandleConversionToType - This is the recursive implementation of
/// ConvertToType.  It does not produce diagnostics, it just returns null on
/// failure.
static Expr *HandleConversionToType(Expr *E, Type *DestTy, bool IgnoreAnonDecls,
                                    SemaExpr &SE) {
  Type *CanDestTy = SE.S.Context.getCanonicalType(DestTy);
  // If we have an exact match, we're done.
  if (SE.S.Context.getCanonicalType(E->Ty) == CanDestTy)
    return E;
  
  assert(!isa<DependentType>(DestTy) &&
         "Result of conversion can't be dependent");
  
  if (TupleType *TT = DestTy->getAs<TupleType>()) {
    // If this is a scalar to tuple conversion, form the tuple and return it.
    if (Expr *Res = HandleScalarConversionToTupleType(E, DestTy, SE))
      return Res;
    
    // If the input is a tuple and the output is a tuple, see if we can convert
    // each element.
    if (TupleType *ETy = E->Ty->getAs<TupleType>()) {
      llvm::SmallVector<Identifier, 8> ExprIdentMapping;
      for (unsigned i = 0, e = ETy->NumFields; i != e; ++i)
        ExprIdentMapping.push_back(ETy->Fields[i].Name);
      
      if (Expr *Res = ConvertExprToTupleType(E, ExprIdentMapping.data(),
                                             ExprIdentMapping.size(), TT, SE))
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
      return ConvertExprToTupleType(E, Idents.data(), Idents.size(), TT, SE);
    }
  }
  
  // Otherwise, check to see if this is an auto-closure case.  This case happens
  // when we convert an expression E to a function type whose result is E's
  // type.
  if (FunctionType *FT = DestTy->getAs<FunctionType>()) {
    // If we bound any anonymous closure arguments, validate them and resolve
    // their types.
    llvm::NullablePtr<AnonDecl> *ActualArgList = 0;
    if (!IgnoreAnonDecls && !SE.S.decl.AnonClosureArgs.empty()) {
      ActualArgList = BindAndValidateClosureArgs(E, FT->Input, SE.S.decl);
      if (ActualArgList == 0)
        return 0;
    }

    // If there are any live anonymous closure arguments, this level will use
    // them and remove them.  When binding something like $0+$1 to
    // (int,int)->(int,int)->() the arguments bind to the first level, not the
    // inner level.  To handle this, we ignore anonymous decls in the recursive
    // case here.
    Expr *ERes = HandleConversionToType(E, FT->Result, true, SE);
    if (ERes == 0) return 0;
  
    // Now that the AnonDecls potentially have a type applied to them, redo
    // semantic analysis from the leaves of the expression tree up.
    ERes = SemaExpressionTree(SE).doIt(ERes);
    if (ERes == 0)
      return 0;
    
    return new (SE.S.Context) ClosureExpr(ERes, ActualArgList, DestTy);
  }
  
  // If the input expression has a dependent type, then there are two cases:
  // first this could be an AnonDecl whose type will be specified by a larger
  // context, second, this could be a context sensitive expression value like
  // :foo.  If this is a context sensitive expression, propagate the type down
  // into the subexpression.
  if (isa<DependentType>(E->Ty))
    return SemaCoerceBottomUp(SE, DestTy).doIt(E);
  
  // Could not do the conversion.
  return 0;
}



Expr *SemaExpr::ConvertToType(Expr *E, Type *DestTy, bool IgnoreAnonDecls,
                              ConversionReason Reason) {
  if (Expr *ERes = HandleConversionToType(E, DestTy, IgnoreAnonDecls, *this))
    return ERes;
  
  // FIXME: We only have this because diagnostics are terrible right now.
  E->dump();
  DestTy->dump();
  
  // TODO: QOI: Source ranges + print the type.
  switch (Reason) {
  case CR_BinOpLHS:
    Error(E->getLocStart(), "left hand side of binary operator has wrong type");
    break;
  case CR_BinOpRHS: 
    Error(E->getLocStart(),"right hand side of binary operator has wrong type");
    break;
  case CR_FuncApply:
    Error(E->getLocStart(), "argument to function invocation has wrong type");
    break;
  case CR_VarInit:
    Error(E->getLocStart(),
          "explicitly specified type doesn't match initializer expression");
    break;
  case CR_TupleInit:
    Error(E->getLocStart(),
          "explicitly specified type doesn't match default value type");
    break;
  case CR_FuncBody:
    Error(E->getLocStart(),
          "type of func body doesn't match specified prototype");
    break;
  }
  
  return 0;
}

