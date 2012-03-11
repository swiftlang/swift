//===--- TypeChecker.h - Type Checking Class --------------------*- C++ -*-===//
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
#include "swift/AST/Diagnostics.h"

namespace swift {

class TypeChecker {
public:
  TranslationUnit &TU;
  ASTContext &Context;
  TypeChecker(TranslationUnit &TU) : TU(TU), Context(TU.Ctx) {}
  
  template<typename ...ArgTypes>
  InFlightDiagnostic diagnose(ArgTypes... Args) {
    return Context.Diags.diagnose(Args...);
  }
  
  bool validateType(ValueDecl *VD);
  bool validateType(Type T);

  bool semaFunctionSignature(FuncExpr *FE);
  bool semaTupleExpr(TupleExpr *TE);
  Expr *semaApplyExpr(ApplyExpr *E);
  Expr *semaUnresolvedDotExpr(UnresolvedDotExpr *E);
  void typeCheckIgnoredExpr(Expr *E);
  
  bool typeCheckExpression(Expr *&E, Type ConvertType = Type());
  void typeCheckDecl(Decl *D);
  bool typeCheckPattern(Pattern *P);
  bool convertToType(Pattern *P, Type Ty);
  bool typeCheckCondition(Expr *&E);

  /// convertToType - Do semantic analysis of an expression in a context that
  /// expects a particular type.  This performs a conversion to that type if
  /// the types don't match and diagnoses cases where the conversion cannot be
  /// performed.
  ///
  /// This emits a diagnostic and returns null on error.
  Expr *convertToType(Expr *E, Type Ty);

  Expr *buildDeclRefRValue(ValueDecl *D, SourceLoc loc);
  Expr *convertToRValue(Expr *E);
  Expr *convertLValueToRValue(LValueType *SrcLT, Expr *E);
  Expr *convertToMaterializable(Expr *E);

  Expr *foldSequence(SequenceExpr *E);
  
  /// applyTypeToLiteral - Apply the specified type to the integer or float
  /// literal expression (which is known to have dependent type), performing
  /// semantic analysis and returning null on a semantic error or the new AST to
  /// use on success.
  Expr *applyTypeToLiteral(Expr *E, Type Ty);
};

  
  
} // end namespace swift

#endif
