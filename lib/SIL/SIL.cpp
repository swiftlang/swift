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
#include "swift/AST/Pattern.h"
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

/// Get the type of a property accessor, () -> T for a getter or (value:T) -> ()
/// for a setter.
static Type getPropertyType(unsigned id, Type valueType, ASTContext &C) {
  if (id & SILConstant::Getter) {
    return FunctionType::get(TupleType::getEmpty(C), valueType, C);
  }
  if (id & SILConstant::Setter) {
    TupleTypeElt valueParam(valueType, C.getIdentifier("value"));
    return FunctionType::get(TupleType::get(valueParam, C),
                                     TupleType::getEmpty(C), C);
  }
  llvm_unreachable("not a property constant");
}

/// Get the type of a subscript accessor, Index -> PropertyAccessor.
static Type getSubscriptPropertyType(unsigned id,
                                     Type indexType,
                                     Type elementType,
                                     ASTContext &C) {
  Type propertyType = getPropertyType(id, elementType, C);
  return FunctionType::get(indexType, propertyType, C);
}

/// Get the type of the 'this' parameter for methods of a type.
static Type getMethodThisType(Type thisType, ASTContext &C) {
  if (thisType->hasReferenceSemantics()) {
    return thisType;
  } else {
    return LValueType::get(thisType, LValueType::Qual::DefaultForType, C);
  }
}

/// Get the type of a method of function type M in context type This,
/// <T,U,...> This -> M,
/// or the type M of the function itself if there is no context type.
static Type getMethodTypeInContext(Type /*nullable*/ contextType,
                                   Type methodType,
                                   ASTContext &C) {
  if (!contextType)
    return methodType;
  Type thisType = getMethodThisType(contextType, C);
  
  if (UnboundGenericType *ugt = contextType->getAs<UnboundGenericType>()) {
    return PolymorphicFunctionType::get(thisType, methodType,
                                        ugt->getDecl()->getGenericParams(), C);
  }

  return FunctionType::get(thisType, methodType, C);
}

/// Get the type of a global variable accessor function, () -> [byref] T.
static Type getGlobalAccessorType(Type varAddressType, ASTContext &C) {
  return FunctionType::get(TupleType::getEmpty(C),
                           varAddressType, C);
}

Type SILModule::makeConstantType(SILConstant c) {
  // TODO: mangle function types for address-only indirect arguments and returns
  if (ValueDecl *vd = c.loc.dyn_cast<ValueDecl*>()) {
    ASTContext &C = vd->getASTContext();
    Type /*nullable*/ contextType =
      vd->getDeclContext()->getDeclaredTypeOfContext();
    if (SubscriptDecl *sd = dyn_cast<SubscriptDecl>(vd)) {
      // If this is a subscript accessor, derive the accessor type.
      Type subscriptType = getSubscriptPropertyType(c.id,
                                                    sd->getIndices()->getType(),
                                                    sd->getElementType(), C);
      return getMethodTypeInContext(contextType, subscriptType, C);
    } else {
      Type propertyType;
      // If this is a property accessor, derive the property type.
      if (c.id & (SILConstant::Getter | SILConstant::Setter)) {
        Type propertyType = getPropertyType(c.id, vd->getType(), C);
        return getMethodTypeInContext(contextType, propertyType, C);
      }

      // If it's a global var, derive the initializer/accessor function type
      // () -> [byref] T
      if (VarDecl *var = dyn_cast<VarDecl>(vd)) {
        assert(!var->isProperty() && "constant ref to non-physical global var");
        return getGlobalAccessorType(var->getTypeOfReference(), C);
      }
      
      // Otherwise, return the Swift-level type.
      return vd->getTypeOfReference();
    }
  } else if (CapturingExpr *e = c.loc.dyn_cast<CapturingExpr*>()) {
    return e->getType();
  }
  llvm_unreachable("unexpected constant loc");
}