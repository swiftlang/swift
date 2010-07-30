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
#include "swift/AST/Expr.h"
#include "swift/AST/Type.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/ADT/PointerIntPair.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/SMLoc.h"
using namespace swift;
using llvm::NullablePtr;

//===----------------------------------------------------------------------===//
// Utility Functions
//===----------------------------------------------------------------------===//


/// BindAndValidateClosureArgs - The specified list of anonymous closure
/// arguments was bound to a closure function with the specified input
/// arguments.  Validate the argument list and, if valid, allocate and return
/// a pointer to the argument to be used for the ClosureExpr.
static llvm::NullablePtr<AnonDecl> *
BindAndValidateClosureArgs(Type *FuncInput, SemaDecl &SD){
  
  const llvm::NullablePtr<AnonDecl> *AnonArgs = SD.AnonClosureArgs.data();
  unsigned NumAnonArgs = SD.AnonClosureArgs.size();
  
  // If the input to the function is a non-tuple, only _0 is valid, if it is a
  // tuple, then _0.._N are valid depending on the number of inputs to the
  // tuple.
  unsigned NumInputArgs = 1;
  if (TupleType *TT = llvm::dyn_cast<TupleType>(FuncInput))
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
  
  // TODO: Do type resolution of the subexpression now that we know the actual
  // types of the arguments.
  
  // Return the right number of inputs.
  llvm::NullablePtr<AnonDecl> *NewInputs =(llvm::NullablePtr<AnonDecl>*)
    SD.S.Context.Allocate(sizeof(*NewInputs)*NumInputArgs, 8);
  for (unsigned i = 0, e = NumInputArgs; i != e; ++i)
    if (i < NumAnonArgs)
      NewInputs[i] = AnonArgs[i];
    else
      NewInputs[i] = 0;
  
  // We used/consumed the anonymous closure arguments.
  SD.AnonClosureArgs.clear();
  return NewInputs;
}

Expr *SemaExpr::HandleConversionToType(Expr *E, Type *OrigDestTy,
                                       bool IgnoreAnonDecls,
                                       ConversionReason Reason) {
  // If we have an exact match of the (canonical) types, we're done.
  Type *DestTy = S.Context.getCanonicalType(OrigDestTy);
  Type *ETy = S.Context.getCanonicalType(E->Ty);
  if (ETy == DestTy) return E;
  
  assert(!DestTy->Dependent && "Result of conversion can't be dependent");
  
  // If the input is a tuple and the output is a tuple with the same number of
  // elements, see if we can convert each element.
  // FIXME: Do this for "funcdecl4(funcdecl3(), 12);"
  // FIXME: Do this for dependent types, to resolve: foo(_0, 4);
  
  // Otherwise, check to see if this is an auto-closure case.  This case happens
  // when we convert an expression E to a function type whose result is E's
  // type.
  if (FunctionType *FT = llvm::dyn_cast<FunctionType>(DestTy)) {
    // If there are any live anonymous closure arguments, this level will use
    // them and remove them.  When binding something like _0+_1 to
    // (int,int)->(int,int)->() the arguments bind to the first level, not the
    // inner level.  To handle this, we ignore anonymous decls in the recursive
    // case here.
    if (Expr *ERes = HandleConversionToType(E, FT->Result, true, Reason)) {
      // If we bound any anonymous closure arguments, validate them and resolve
      // their types.
      llvm::NullablePtr<AnonDecl> *ActualArgList = 0;
      if (!IgnoreAnonDecls && !S.decl.AnonClosureArgs.empty())
        ActualArgList = BindAndValidateClosureArgs(FT->Input, S.decl);      
      return new (S.Context) ClosureExpr(ERes, ActualArgList, OrigDestTy);
    }
  }
  
  // FIXME: QOI: Source ranges + print the type.
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
  case CR_FuncBody:
    Error(E->getLocStart(),
          "type of func body doesn't match specified prototype");
    break;
  }
  
  return 0;
}

//===----------------------------------------------------------------------===//
// Action Implementations
//===----------------------------------------------------------------------===//

NullablePtr<Expr> SemaExpr::ActOnNumericConstant(llvm::StringRef Text,
                                                 llvm::SMLoc Loc) {
  return new (S.Context) IntegerLiteral(Text, Loc, S.Context.IntType);
}

NullablePtr<Expr> 
SemaExpr::ActOnIdentifierExpr(llvm::StringRef Text, llvm::SMLoc Loc) {
  NamedDecl *D = S.decl.LookupName(S.Context.getIdentifier(Text));

  // If this identifier is _0 -> _9, then it is a use of an implicit anonymous
  // closure argument.
  if (D == 0 && Text.size() == 2 &&
      Text[0] == '_' && Text[1] >= '0' && Text[1] <= '9')
    D = S.decl.GetAnonDecl(Text, Loc);
                                 
  // TODO: QOI: If the decl had an "invalid" bit set, then return the error
  // object to improve error recovery.
  if (D)
    return new (S.Context) DeclRefExpr(D, Loc, D->Ty);
  
  Error(Loc, "use of undeclared identifier");
  return 0;
}

NullablePtr<Expr> 
SemaExpr::ActOnBraceExpr(llvm::SMLoc LBLoc,
                         const llvm::PointerUnion<Expr*, NamedDecl*> *Elements,
                         unsigned NumElements, bool HasMissingSemi,
                         llvm::SMLoc RBLoc) {
  // If any of the elements of the braces has a function type (which indicates
  // that a function didn't get called), then produce an error.  We don't do
  // this for the last element in the 'missing semi' case, because the brace
  // expr as a whole has the function result.
  // TODO: What about tuples which contain functions by-value that are dead?
  for (unsigned i = 0; i != NumElements-(HasMissingSemi ? 1 : 0); ++i)
    if (Elements[i].is<Expr*>() &&
        llvm::isa<FunctionType>(Elements[i].get<Expr*>()->Ty))
      // TODO: QOI: Add source range.
      Error(Elements[i].get<Expr*>()->getLocStart(),
            "expression resolves to an unevaluated function");
  
  Type *ResultTy;
  if (HasMissingSemi)
    ResultTy = Elements[NumElements-1].get<Expr*>()->Ty;
  else
    ResultTy = S.Context.VoidType;
  
  llvm::PointerUnion<Expr*, NamedDecl*> *NewElements = 
    (llvm::PointerUnion<Expr*, NamedDecl*> *)
    S.Context.Allocate(sizeof(*Elements)*NumElements, 8);
  memcpy(NewElements, Elements, sizeof(*Elements)*NumElements);
  
  return new (S.Context) BraceExpr(LBLoc, NewElements, NumElements,
                                   HasMissingSemi, RBLoc, ResultTy);
}

NullablePtr<Expr> 
SemaExpr::ActOnTupleExpr(llvm::SMLoc LPLoc, Expr **SubExprs,
                         unsigned NumSubExprs, llvm::SMLoc RPLoc) {
  
  // A tuple expr with a single subexpression is just a grouping paren.
  Type *ResultTy;
  if (NumSubExprs == 1) {
    ResultTy = SubExprs[0]->Ty;
  } else {
    // Compute the result type.
    llvm::SmallVector<TupleType::TypeOrDecl, 8> ResultTyElts;
    
    for (unsigned i = 0, e = NumSubExprs; i != e; ++i)
      ResultTyElts.push_back(SubExprs[i]->Ty);
    
    ResultTy = S.Context.getTupleType(ResultTyElts.data(), NumSubExprs);
  }
  
  Expr **NewSubExprs = (Expr**)S.Context.Allocate(sizeof(Expr*)*NumSubExprs, 8);
  memcpy(NewSubExprs, SubExprs, sizeof(Expr*)*NumSubExprs);
  return new (S.Context) TupleExpr(LPLoc, NewSubExprs, NumSubExprs,
                                   RPLoc, ResultTy);
}

/// ActOnJuxtaposition - This is invoked whenever the parser sees a
/// juxtaposition operation.  If the two expressions can be applied, this
/// returns the new expression (which can be null) and false, otherwise it
/// return null and true to indicate that they are sequenced.
llvm::PointerIntPair<Expr*, 1, bool>
SemaExpr::ActOnJuxtaposition(Expr *E1, Expr *E2) {
  // If this expression in the sequence is just a floating value that isn't
  // a function, then we have a discarded value, such as "4 5".  Just return
  // true so that it gets properly sequenced.
  FunctionType *FT = llvm::dyn_cast<FunctionType>(E1->Ty);
  if (FT == 0)
    return llvm::PointerIntPair<Expr*, 1, bool>(0, true);
  
  // Otherwise, we do have a function application.  Check that the argument
  // type matches the expected type of the function.
  E2 = HandleConversionToType(E2, FT->Input, false, CR_FuncApply);
  if (E2 == 0)
    return llvm::PointerIntPair<Expr*, 1, bool>(0, false);
    
  E1 = new (S.Context) ApplyExpr(E1, E2, FT->Result);
  return llvm::PointerIntPair<Expr*, 1, bool>(E1, false);
}


llvm::NullablePtr<Expr>
SemaExpr::ActOnSequence(Expr **Exprs, unsigned NumExprs) {
  assert(NumExprs != 0 && "Empty sequence isn't possible");

  Expr **NewElements =(Expr**)
    S.Context.Allocate(sizeof(*NewElements)*NumExprs, 8);
  memcpy(NewElements, Exprs, sizeof(*NewElements)*NumExprs);
  return new (S.Context) SequenceExpr(NewElements, NumExprs);
}


NullablePtr<Expr> 
SemaExpr::ActOnBinaryExpr(Expr *LHS, NamedDecl *OpFn,
                          llvm::SMLoc OpLoc, Expr *RHS) {
  // Parser verified that OpFn has an Infix Precedence.  Sema verified that OpFn
  // only has InfixPrecedence if it takes a 2 element tuple as input.
  assert(OpFn->Attrs.InfixPrecedence != -1 &&
         "Sema and parser should verify that only binary predicates are used"); 
  FunctionType *FnTy = llvm::cast<FunctionType>(OpFn->Ty);
  TupleType *Input = llvm::cast<TupleType>(FnTy->Input);
  assert(Input->NumFields == 2 && "Sema error validating infix fn type");

  // Verify that the LHS/RHS have the right type and do conversions as needed.
  LHS = HandleConversionToType(LHS, Input->getElementType(0), false,
                                      CR_BinOpLHS);
  if (LHS == 0) return 0;
    
  RHS = HandleConversionToType(RHS, Input->getElementType(1), false,
                               CR_BinOpRHS);
  if (RHS == 0) return 0;
  
  return new (S.Context) BinaryExpr(LHS, OpFn, OpLoc, RHS,
                                    FnTy->Result);
}
