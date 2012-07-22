//===-- TypeVisitor.h - IR-gen TypeVisitor specialization -------*- C++ -*-===//
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
// This file defines swift::irgen::TypeVisitor.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_TYPEVISITOR_H
#define SWIFT_IRGEN_TYPEVISITOR_H

#include "swift/AST/TypeVisitor.h"

namespace swift {
namespace irgen {
  
/// irgen::TypeVisitor - This is a specialization of
/// swift::TypeVisitor which works only on canonical types and
/// which automatically ignores certain AST node kinds.
template<typename ImplClass, typename RetTy = void> 
class TypeVisitor : public swift::TypeVisitor<ImplClass, RetTy> {
public:

  RetTy visit(CanType T) {
    return swift::TypeVisitor<ImplClass, RetTy>::visit(Type(T));
  }

#define TYPE(Id, Parent)
#define UNCHECKED_TYPE(Id, Parent) \
  RetTy visit##Id##Type(Id##Type *T) { \
    llvm_unreachable(#Id "Type should not survive to IR-gen"); \
  }
#define SUGARED_TYPE(Id, Parent) \
  RetTy visit##Id##Type(Id##Type *T) { \
    llvm_unreachable(#Id "Type should not survive canonicalization"); \
  }
#include "swift/AST/TypeNodes.def"

};  
  
} // end namespace irgen
} // end namespace swift
  
#endif
