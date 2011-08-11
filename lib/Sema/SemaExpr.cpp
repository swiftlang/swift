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
#include "swift/AST/Stmt.h"
#include "swift/AST/Types.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/ADT/PointerIntPair.h"
#include "llvm/ADT/Twine.h"
#include "llvm/Support/SMLoc.h"
using namespace swift;

//===----------------------------------------------------------------------===//
// Action Implementations
//===----------------------------------------------------------------------===//

NullablePtr<Expr> SemaExpr::ActOnNumericConstant(StringRef Text,
                                                 SMLoc Loc) {
  // The integer literal must fit in 64-bits.
  unsigned long long Val;
  if (Text.getAsInteger(0, Val)) {
    error(Loc, "invalid immediate for integer literal, value too large");
    Text = "1";
  }

  // The type of an integer literal is always "integer_literal_type", which
  // should be defined by the library.
  Identifier TyName = S.Context.getIdentifier("integer_literal_type");
  Type Ty = S.decl.LookupTypeName(TyName, Loc)->getAliasType(S.Context);
  return new (S.Context) IntegerLiteralExpr(Text, Loc, Ty);
}


NullablePtr<Expr> 
SemaExpr::ActOnIdentifierExpr(Identifier Text, SMLoc Loc) {
  ValueDecl *D = S.decl.LookupValueName(Text);
  
  if (D == 0)
    return new (S.Context) UnresolvedDeclRefExpr(Text, Loc);
  
  return new (S.Context) DeclRefExpr(D, Loc);
}

NullablePtr<Expr> SemaExpr::
ActOnScopedIdentifierExpr(Identifier ScopeName, SMLoc ScopeLoc,
                          SMLoc ColonColonLoc,
                          Identifier Name, SMLoc NameLoc) {
  // Note: this is very simplistic support for scoped name lookup, extend when
  // needed.
  TypeAliasDecl *TypeScopeDecl = S.decl.LookupTypeName(ScopeName, ScopeLoc);
  
  return new (S.Context) UnresolvedScopedIdentifierExpr(TypeScopeDecl, ScopeLoc,
                                                        ColonColonLoc, NameLoc,
                                                        Name);
}

NullablePtr<Expr> 
SemaExpr::ActOnTupleExpr(SMLoc LPLoc, Expr *const *SubExprs,
                         const Identifier *SubExprNames,
                         unsigned NumSubExprs, SMLoc RPLoc,
                         bool IsPrecededByIdentifier) {
  
  Expr **NewSubExprs =
    S.Context.AllocateCopy<Expr*>(SubExprs, SubExprs+NumSubExprs);

  Identifier *NewSubExprsNames = 0;
  if (SubExprNames)
    NewSubExprsNames =
      S.Context.AllocateCopy<Identifier>(SubExprNames,SubExprNames+NumSubExprs);

  bool IsGrouping = false;
  if (NumSubExprs == 1 &&
      (SubExprNames == 0 || SubExprNames[0].empty()))
    IsGrouping = true;
  
  return new (S.Context) TupleExpr(LPLoc, NewSubExprs, NewSubExprsNames,
                                   NumSubExprs, RPLoc, IsGrouping,
                                   IsPrecededByIdentifier);
}

/// ActOnCondition - Handle a condition to an if/while statement, inserting
/// the call that will convert to a 1-bit type.
Expr *SemaExpr::ActOnCondition(Expr *Cond) {
  assert(Cond);  // Else may be null.
  
  // The condition needs to be convertible to a logic value.  Build a call to
  // "convertToLogicValue" passing in the condition as an argument.
  Identifier C2LVFuncId = S.Context.getIdentifier("convertToLogicValue");
  Expr *C2LVFunc = ActOnIdentifierExpr(C2LVFuncId, Cond->getLocStart()).get();
  
  return new (S.Context) ApplyExpr(C2LVFunc, Cond,
                                   UnresolvedType::get(S.Context));
}


/// FuncTypePiece - This little enum is used by AddFuncArgumentsToScope to keep
/// track of where in a function type it is currently looking.  This affects how
/// the decls are processed and created.
enum class FuncTypePiece {
  Function,  // Looking at the initial functiontype itself.
  Input,     // Looking at the input to the function type
  Output     // Looking at the output to the function type.
};

/// AddFuncArgumentsToScope - Walk the type specified for a Func object (which
/// is known to be a FunctionType on the outer level) creating and adding named
/// arguments to the current scope.  This causes redefinition errors to be
/// emitted.
static void AddFuncArgumentsToScope(Type Ty,
                                    SmallVectorImpl<unsigned> &AccessPath,
                                    FuncTypePiece Mode,
                                    SMLoc FuncLoc, 
                                    SmallVectorImpl<ArgDecl*> &ArgDecls,
                                    SemaExpr &SE) {
  // Handle the function case first.
  if (Mode == FuncTypePiece::Function) {
    FunctionType *FT = cast<FunctionType>(Ty.getPointer());
    AccessPath.push_back(0);
    AddFuncArgumentsToScope(FT->Input, AccessPath, FuncTypePiece::Input,
                            FuncLoc, ArgDecls, SE);
    
    AccessPath.back() = 1;
    
    // If this is a->b->c then we treat b as an input, not (b->c) as an output.
    if (isa<FunctionType>(FT->Result.getPointer()))
      AddFuncArgumentsToScope(FT->Result, AccessPath,
                              FuncTypePiece::Function, FuncLoc, ArgDecls, SE);
    else    
      AddFuncArgumentsToScope(FT->Result, AccessPath,
                              FuncTypePiece::Output, FuncLoc, ArgDecls, SE);
    AccessPath.pop_back();
    return;
  }
  
  // Otherwise, we're looking at an input or output to the func.  The only type
  // we currently dive into is the humble tuple, which can be recursive.  This
  // should dive in syntactically.
  ///
  /// Note that we really *do* want dyn_cast here, not getAs, because we do not
  /// want to look through type aliases or other sugar, we want to see what the
  /// user wrote in the func declaration.
  TupleType *TT = dyn_cast<TupleType>(Ty.getPointer());
  if (TT == 0) return;
  
  
  AccessPath.push_back(0);
  
  // For tuples, recursively processes their elements (to handle cases like:
  //    (x : (a : int, b : int), y : int) -> ...
  // and create decls for any named elements.
  for (unsigned i = 0, e = TT->Fields.size(); i != e; ++i) {
    AccessPath.back() = 1;
    AddFuncArgumentsToScope(TT->Fields[i].Ty, AccessPath, Mode, FuncLoc,
                            ArgDecls, SE);
    
    // If this field is named, create the argument decl for it.
    Identifier Name = TT->Fields[i].Name;
    // Ignore unnamed fields.
    if (Name.get() == 0) continue;
    
    
    // Create the argument decl for this named argument.
    ArgDecl *AD = new (SE.S.Context) ArgDecl(FuncLoc, Name, TT->Fields[i].Ty);
    ArgDecls.push_back(AD);
    
    // Eventually we should mark the input/outputs as readonly vs writeonly.
    //bool isInput = Mode == FuncTypePiece::Input;
    
    SE.S.decl.AddToScope(AD);
  }
  
  AccessPath.pop_back();
}


FuncExpr *SemaExpr::ActOnFuncExprStart(SMLoc FuncLoc, Type FuncTy) {
  SmallVector<unsigned, 8> AccessPath;
  SmallVector<ArgDecl*, 8> ArgDecls;
  AddFuncArgumentsToScope(FuncTy, AccessPath, FuncTypePiece::Function,
                          FuncLoc, ArgDecls, *this);
  
  ArrayRef<ArgDecl*> Args = ArgDecls;
  
  return new (S.Context) FuncExpr(FuncLoc, FuncTy,
                                  S.Context.AllocateCopy(Args));
}


