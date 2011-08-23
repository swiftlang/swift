//===--- TypeChecking.h - Type Checking Class -------------------*- C++ -*-===//
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
//  This file defines the TypeChecking class.
//
//===----------------------------------------------------------------------===//

#ifndef TYPECHECKING_H
#define TYPECHECKING_H

#include "swift/AST/AST.h"

namespace swift {

class TypeChecker {
public:
  ASTContext &Context;
  TypeChecker(ASTContext &C) : Context(C) {}
  
  void note(SMLoc Loc, const Twine &Message);
  void warning(SMLoc Loc, const Twine &Message);
  void error(SMLoc Loc, const Twine &Message);
  
  bool validateType(ValueDecl *VD);
  bool validateType(Type T);
  
  bool semaTupleExpr(TupleExpr *TE);
  bool semaApplyExpr(ApplyExpr *E);
  
  Expr *typeCheckExpression(Expr *E);
  void typeCheckTranslationUnit(TranslationUnitDecl *D);
  
  
  
  void typeCheck(TypeAliasDecl *TAD);
  
  void typeCheck(ValueDecl *VD) {
    // No types to resolved for a ElementRefDecl.
    if (ElementRefDecl *ERD = dyn_cast<ElementRefDecl>(VD))
      return typeCheckERD(ERD);
    if (VarDecl *Var = dyn_cast<VarDecl>(VD))
      return typeCheckVarDecl(Var);
    
    if (isa<OneOfElementDecl>(VD))
      return;  // FIXME: No type checking required for this?
    
    typeCheckValueDecl(cast<ValueDecl>(VD));
  }
  
  void typeCheckERD(ElementRefDecl *ERD);
  void typeCheckVarDecl(VarDecl *VD);
  bool typeCheckValueDecl(ValueDecl *VD);
  
  void validateAttributes(DeclAttributes &Attrs, Type Ty);
  
  bool validateVarName(Type Ty, DeclVarName *Name);
  
  /// checkBody - Type check an expression that is used in a top-level
  /// context like a var/func body, or tuple default value.  If DestTy is
  /// specified, the expression is coerced to the requested type.
  void checkBody(Expr *&E, Type DestTy);
  
  
  /// convertToType - Do semantic analysis of an expression in a context that
  /// expects a particular type.  This performs a conversion to that type if
  /// the types don't match and diagnoses cases where the conversion cannot be
  /// performed.
  ///
  /// This emits a diagnostic and returns null on error.
  Expr *convertToType(Expr *E, Type Ty);
};

  
  
} // end namespace swift

#endif
