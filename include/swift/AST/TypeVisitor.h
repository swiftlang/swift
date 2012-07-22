//===-- TypeVisitor.h - Type Visitor ----------------------------*- C++ -*-===//
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
// This file defines the TypeVisitor class.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_TYPEVISITOR_H
#define SWIFT_AST_TYPEVISITOR_H

#include "swift/AST/Types.h"
#include "llvm/Support/ErrorHandling.h"

namespace swift {
  
/// TypeVisitor - This is a simple visitor class for Swift types.
template<typename ImplClass, typename RetTy = void> 
class TypeVisitor {
public:

  RetTy visit(Type T) {
    switch (T->getKind()) {
#define TYPE(CLASS, PARENT) \
    case TypeKind::CLASS: \
      return static_cast<ImplClass*>(this) \
        ->visit##CLASS##Type(static_cast<CLASS##Type*>(T.getPointer()));
#include "swift/AST/TypeNodes.def"
    }
    llvm_unreachable("Not reachable, all cases handled");
  }
  
  // Provide default implementations of abstract "visit" implementations that
  // just chain to their base class.  This allows visitors to just implement
  // the base behavior and handle all subclasses if they desire.  Since this is
  // a template, it will only instantiate cases that are used and thus we still
  // require full coverage of the AST nodes by the visitor.
#define ABSTRACT_TYPE(CLASS, PARENT)                         \
  RetTy visit##CLASS##Type(CLASS##Type *T) {                 \
     return static_cast<ImplClass*>(this)->visit##PARENT(T); \
  }
#define TYPE(CLASS, PARENT) ABSTRACT_TYPE(CLASS, PARENT)
#include "swift/AST/TypeNodes.def"

};
  
} // end namespace swift
  
#endif
