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
#include "swift/AST/ASTContext.h"
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

SILConstant::SILConstant(SILConstant::Loc baseLoc) {
  if (ValueDecl *vd = baseLoc.dyn_cast<ValueDecl*>()) {
    // Explicit getters and setters have independent decls, but we also need
    // to reference implicit getters and setters. For consistency, generate
    // getter and setter constants as references to the parent decl.
    if (FuncDecl *fd = dyn_cast<FuncDecl>(vd)) {
      if (fd->isGetterOrSetter()) {
        if (Decl *getterFor = fd->getGetterDecl()) {
          loc = cast<ValueDecl>(getterFor);
          id = Getter;
        } else if (Decl *setterFor = fd->getSetterDecl()) {
          loc = cast<ValueDecl>(setterFor);
          id = Setter;
        } else {
          llvm_unreachable("no getter or setter decl?!");
        }
        return;
      }
    }
  }
  
  loc = baseLoc;
  id = 0;
}

Type SILConstant::getType() const {
  // FIXME: This should probably be a method on SILModule that caches the
  // type information.
  if (ValueDecl *vd = loc.dyn_cast<ValueDecl*>()) {
    ASTContext &C = vd->getASTContext();
    Type contextType = vd->getDeclContext()->getDeclaredTypeOfContext();
    Type propertyType;
    // If this is a property accessor, derive the property type.
    // FIXME: this is a dumb get-things-working kludge. we can get the getter/
    // setter types from somewhere else.
    if (id & SILConstant::Getter) {
      propertyType = FunctionType::get(TupleType::getEmpty(C),
                                       vd->getType(), C);
    }
    if (id & SILConstant::Setter) {
      TupleTypeElt valueParam(vd->getType(), C.getIdentifier("value"));
      propertyType = FunctionType::get(TupleType::get(valueParam, C),
                                       TupleType::getEmpty(C), C);
    }
    if (propertyType)
      return contextType
        ? FunctionType::get(LValueType::get(contextType,
                                            LValueType::Qual::DefaultForType,
                                            C),
                            propertyType, C)
        : propertyType;

    if (VarDecl *var = dyn_cast<VarDecl>(vd)) {
      // Global vars of type T are handled by () -> [byref] T functions.
      return FunctionType::get(TupleType::getEmpty(C),
                               var->getTypeOfReference(), C);
    }
    return vd->getTypeOfReference();
  } else if (CapturingExpr *e = loc.dyn_cast<CapturingExpr*>()) {
    return e->getType();
  }
  llvm_unreachable("unexpected constant loc");
}