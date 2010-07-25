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
#include "llvm/ADT/NullablePtr.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/SMLoc.h"
using namespace swift;
using llvm::NullablePtr;

NullablePtr<Expr> SemaExpr::ActOnNumericConstant(llvm::StringRef Text,
                                                 llvm::SMLoc Loc) {
  return new (S.Context) IntegerLiteral(Text, Loc, S.Context.IntType);
}

NullablePtr<Expr> 
SemaExpr::ActOnIdentifierExpr(llvm::StringRef Text, llvm::SMLoc Loc) {
  NamedDecl *D = S.decl.LookupName(S.Context.getIdentifier(Text));
  if (D == 0) {
    Error(Loc, "use of undeclared identifier");
    return 0;
  }
  
  // TODO: QOI: If the decl had an "invalid" bit set, then return the error
  // object to improve error recovery.
  return new (S.Context) DeclRefExpr(D, Loc, D->Ty);
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

llvm::NullablePtr<Expr>
SemaExpr::ActOnSequence(Expr **Exprs, unsigned NumExprs) {
  assert(NumExprs != 0 && "Empty sequence isn't possible");

  llvm::PointerUnion<Expr*, NamedDecl*> *NewElements = 
    (llvm::PointerUnion<Expr*, NamedDecl*> *)
    S.Context.Allocate(sizeof(*NewElements)*NumExprs, 8);
  unsigned NewNumElements = 0;

  // Loop over all of the expressions, splitting instances of function
  // application out into ApplyExpr nodes.
  for (unsigned i = 0; i != NumExprs; ) {
    Type *ExprTy = Exprs[i]->Ty;
    // If this expression in the sequence is just a floating value that isn't
    // a function, then we have a discarded value, such as "4 5".  Just add it
    // to the sequence to be computed and move on.  Alternatively, if this is a
    // function at the end of the sequence, do the same: the sequence's value is
    // the function.
    if (!llvm::isa<FunctionType>(ExprTy) ||  // Non-function value.
        i == NumExprs-1) {                   // Last expr in sequence?
      NewElements[NewNumElements++] = Exprs[i++];
      continue;
    }
    
    // Otherwise, we do have a function application.  Check that the argument
    // type matches the expected type of the function.
    FunctionType *FT = llvm::cast<FunctionType>(ExprTy);
    
    if (S.Context.getCanonicalType(FT->Input) !=
        S.Context.getCanonicalType(Exprs[i+1]->Ty)) {
      // FIXME: QOI: Source ranges + print the type.
      Error(Exprs[i]->getLocStart(),
            "operator to function invocation has wrong type");
      return 0;
    }
    
    // FIXME: Do the check.  Need type predicates.
    Exprs[i+1] = new (S.Context) ApplyExpr(Exprs[i], Exprs[i+1],
                                           FT->Result);
    ++i;
  }
  
  if (NewNumElements == 1)
    return NewElements[0].get<Expr*>();
  
  // FIXME: Generating a BraceExpr node is a hack here, add a new Expr node.
  Type *ResultTy = NewElements[NewNumElements-1].get<Expr*>()->Ty;
  return new (S.Context) BraceExpr(NewElements[0].get<Expr*>()->getLocStart(),
                                   NewElements, NewNumElements,
                                   true, llvm::SMLoc(), ResultTy);
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
  
  if (S.Context.getCanonicalType(Input->getElementType(0)) != 
      S.Context.getCanonicalType(LHS->Ty)) {
    // TODO: QOI: source range + types.
    Error(OpLoc, "left hand side of binary operator has wrong type");
    return 0;
  }
    
  if (S.Context.getCanonicalType(Input->getElementType(1)) != 
      S.Context.getCanonicalType(RHS->Ty)) {
    // TODO: QOI: source range + types.
    Error(OpLoc, "right hand side of binary operator has wrong type");
    return 0;
  }

  
  // FIXME: Do sema here to check that the right types are being passed.
  
  return new (S.Context) BinaryExpr(LHS, OpFn, OpLoc, RHS,
                                    FnTy->Result);
}
