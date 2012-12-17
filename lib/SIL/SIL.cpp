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
Type SILModule::getPropertyType(unsigned id, Type valueType) const {
  if (id & SILConstant::Getter) {
    return FunctionType::get(TupleType::getEmpty(Context), valueType, Context);
  }
  if (id & SILConstant::Setter) {
    TupleTypeElt valueParam(valueType, Context.getIdentifier("value"));
    return FunctionType::get(TupleType::get(valueParam, Context),
                             TupleType::getEmpty(Context),
                             Context);
  }
  llvm_unreachable("not a property constant");
}

/// Get the type of a subscript accessor, Index -> PropertyAccessor.
Type SILModule::getSubscriptPropertyType(unsigned id,
                                     Type indexType,
                                     Type elementType) const {
  Type propertyType = getPropertyType(id, elementType);
  return FunctionType::get(indexType, propertyType, Context);
}

/// Get the type of the 'this' parameter for methods of a type.
Type SILModule::getMethodThisType(Type thisType) const {
  if (thisType->hasReferenceSemantics()) {
    return thisType;
  } else {
    return LValueType::get(thisType, LValueType::Qual::DefaultForType, Context);
  }
}

Type SILModule::getMethodTypeInContext(Type /*nullable*/ contextType,
                                       Type methodType) const {
  if (!contextType)
    return methodType;
  Type thisType = getMethodThisType(contextType);
  
  if (UnboundGenericType *ugt = contextType->getAs<UnboundGenericType>()) {
    return PolymorphicFunctionType::get(thisType, methodType,
                                        ugt->getDecl()->getGenericParams(),
                                        Context);
  }

  return FunctionType::get(thisType, methodType, Context);
}

/// Get the type of a global variable accessor function, () -> [byref] T.
static Type getGlobalAccessorType(Type varAddressType, ASTContext &C) {
  return FunctionType::get(TupleType::getEmpty(C),
                           varAddressType, C);
}

Type SILModule::makeConstantType(SILConstant c) {
  // TODO: mangle function types for address-only indirect arguments and returns
  if (ValueDecl *vd = c.loc.dyn_cast<ValueDecl*>()) {
    Type /*nullable*/ contextType =
      vd->getDeclContext()->getDeclaredTypeOfContext();
    if (SubscriptDecl *sd = dyn_cast<SubscriptDecl>(vd)) {
      // If this is a subscript accessor, derive the accessor type.
      Type subscriptType = getSubscriptPropertyType(c.id,
                                                    sd->getIndices()->getType(),
                                                    sd->getElementType());
      return getMethodTypeInContext(contextType, subscriptType);
    } else {
      Type propertyType;
      // If this is a property accessor, derive the property type.
      if (c.id & (SILConstant::Getter | SILConstant::Setter)) {
        Type propertyType = getPropertyType(c.id, vd->getType());
        return getMethodTypeInContext(contextType, propertyType);
      }

      // If it's a global var, derive the initializer/accessor function type
      // () -> [byref] T
      if (VarDecl *var = dyn_cast<VarDecl>(vd)) {
        assert(!var->isProperty() && "constant ref to non-physical global var");
        return getGlobalAccessorType(var->getTypeOfReference(), Context);
      }
      
      // Otherwise, return the Swift-level type.
      return vd->getTypeOfReference();
    }
  } else if (CapturingExpr *e = c.loc.dyn_cast<CapturingExpr*>()) {
    return e->getType();
  }
  llvm_unreachable("unexpected constant loc");
}
