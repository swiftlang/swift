//===--- CanTypeVisitor.h - TypeVisitor specialization ----------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines CanTypeVisitor, a specialized version of
// TypeVisitor for visiting fully type-checked canonical types.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_CANTYPEVISITOR_H
#define SWIFT_AST_CANTYPEVISITOR_H

#include "swift/AST/Types.h"

namespace swift {

/// This is a specialization of swift::TypeVisitor which:
///   - works only on canonical and fully-checked types and
///   - preserves the canonicality of the visited types in the
///     static (C++) type.
template<typename ImplClass, typename RetTy = void, typename... Args>
class CanTypeVisitor {
public:
  RetTy visit(CanType T, Args... args) {
    switch (T->getKind()) {
#define UNCHECKED_TYPE(CLASS, PARENT) \
    case TypeKind::CLASS:
#define SUGARED_TYPE(CLASS, PARENT) \
    case TypeKind::CLASS:
#define TYPE(CLASS, PARENT)
#include "swift/AST/TypeNodes.def"
      llvm::dbgs() << "non-canonical or unchecked " << T << "\n";
      llvm_unreachable("non-canonical or unchecked type");

#define UNCHECKED_TYPE(CLASS, PARENT)
#define SUGARED_TYPE(CLASS, PARENT)
#define TYPE(CLASS, PARENT)                                  \
    case TypeKind::CLASS:                                    \
      return static_cast<ImplClass*>(this)                   \
        ->visit##CLASS##Type(cast<CLASS##Type>(T),           \
                             ::std::forward<Args>(args)...);
#include "swift/AST/TypeNodes.def"
    }
    llvm_unreachable("Not reachable, all cases handled");
  }
  
  // Provide default implementations of abstract "visit" implementations that
  // just chain to their base class.  This allows visitors to just implement
  // the base behavior and handle all subclasses if they desire.  Since this is
  // a template, it will only instantiate cases that are used and thus we still
  // require full coverage of the AST nodes by the visitor.
#define ABSTRACT_TYPE(CLASS, PARENT)                           \
  RetTy visit##CLASS##Type(Can##CLASS##Type T, Args... args) { \
     return static_cast<ImplClass*>(this)                      \
              ->visit##PARENT(T, std::forward<Args>(args)...); \
  }
#define TYPE(CLASS, PARENT) ABSTRACT_TYPE(CLASS, PARENT)
#define ABSTRACT_SUGARED_TYPE(CLASS, PARENT)
#define SUGARED_TYPE(CLASS, PARENT)
#define UNCHECKED_TYPE(CLASS, PARENT)
#include "swift/AST/TypeNodes.def"
};

} // end namespace swift
  
#endif
