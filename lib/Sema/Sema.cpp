//===--- Sema.cpp - Swift Language Semantic Analysis ----------------------===//
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
//  This file implements semantic analysis for Swift.
//
//===----------------------------------------------------------------------===//

#include "swift/Sema/Sema.h"
#include "swift/AST/Decl.h"
using namespace swift;

Sema::Sema(ASTContext &context) : Context(context), type(*this), expr(*this) {
}



VarDecl *Sema::ActOnVarDecl(llvm::SMLoc VarLoc, llvm::StringRef Name, Type *Ty,
                            swift::Expr *Init) {
 
  // Diagnose when we don't have a type or an expression.
  if (Ty == 0 && Init == 0) {
    expr.Error(VarLoc, "var declaration must specify a type if no "
               "initializer is specified");
    // TODO: Recover better by making var have 'invalid' type.
    return 0;
  }
  
  return new (Context) VarDecl(VarLoc, Name, Ty, Init);
}
