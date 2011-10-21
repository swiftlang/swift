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
#include "swift/Basic/DiagnosticEngine.h"

namespace swift {

class TypeChecker {
public:
  ASTContext &Context;
  TypeChecker(ASTContext &C) : Context(C) {}
  
  template<typename ...ArgTypes>
  void diagnose(ArgTypes... Args) {
    Context.Diags.diagnose(Args...);
  }

  void note(SourceLoc Loc, const Twine &Message) __attribute__((deprecated)) {
    Context.note(Loc, Message);
  }
  void warning(SourceLoc Loc, const Twine &Message) __attribute__((deprecated)){
    Context.warning(Loc, Message);
  }
  void error(SourceLoc Loc, const Twine &Message) __attribute__((deprecated)) {
    Context.error(Loc, Message);
  }
  
  bool validateType(ValueDecl *VD);
  bool validateType(Type T);
  
  bool semaTupleExpr(TupleExpr *TE);
  bool semaApplyExpr(ApplyExpr *E);
  
  bool typeCheckExpression(Expr *&E, Type ConvertType = Type());
  void typeCheckDecl(Decl *D);

  
  bool bindAndValidateClosureArgs(Expr *Body, Type FuncInput);

  /// convertToType - Do semantic analysis of an expression in a context that
  /// expects a particular type.  This performs a conversion to that type if
  /// the types don't match and diagnoses cases where the conversion cannot be
  /// performed.
  ///
  /// This emits a diagnostic and returns null on error.
  Expr *convertToType(Expr *E, Type Ty);

  Expr *convertToRValue(Expr *E);

  Expr *foldSequence(SequenceExpr *E);
};

  
  
} // end namespace swift

#endif
