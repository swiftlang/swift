//===--- SILFunction.cpp - Defines the SILFunction data structure ---------===//
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

#include "swift/SIL/Function.h"
#include "swift/SIL/SILModule.h"
#include "swift/SIL/SILConstant.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Types.h"
using namespace swift;

Function::~Function() {
}

SILModule::SILModule(ASTContext &Context, bool hasTopLevel) :
  Context(Context), toplevel(nullptr) {
    
  if (hasTopLevel)
    toplevel = new (*this) Function(*this);
}

SILModule::~SILModule() {
}

Type SILConstant::getType() const {
  // FIXME: This won't work for intermediate curry decls. This should probably
  // live on the SILModule.
  if (ValueDecl *vd = loc.dyn_cast<ValueDecl*>()) {
    if (VarDecl *var = dyn_cast<VarDecl>(vd)) {
      // Global vars of type T are handled by () -> [byref] T functions.
      ASTContext &C = var->getASTContext();
      return FunctionType::get(TupleType::getEmpty(C),
                               var->getTypeOfReference(), C);
    }
    return vd->getTypeOfReference();
  } else if (CapturingExpr *e = loc.dyn_cast<CapturingExpr*>()) {
    return e->getType();
  }
  llvm_unreachable("unexpected constant loc");
}