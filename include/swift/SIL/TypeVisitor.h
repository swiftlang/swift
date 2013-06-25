//===-- TypeVisitor.h - SILGen TypeVisitor specialization -------*- C++ -*-===//
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
// This file defines swift::Lowering::TypeVisitor.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_LOWERING_TYPEVISITOR_H
#define SWIFT_LOWERING_TYPEVISITOR_H

#include "swift/AST/TypeVisitor.h"

namespace swift {
namespace Lowering {

/// Lowering::TypeVisitor - This is a specialization of
/// swift::TypeVisitor which works only on canonical types and
/// which automatically ignores certain AST node kinds.
template<typename ImplClass, typename RetTy = void, typename... Args>
class TypeVisitor : public swift::TypeVisitor<ImplClass, RetTy, Args...> {
public:

  template <class... As> RetTy visit(CanType type, Args... args) {
    return swift::TypeVisitor<ImplClass, RetTy, Args...>
                            ::visit(Type(type), std::forward<Args>(args)...);
  }

#define TYPE(Id, Parent)
#define UNCHECKED_TYPE(Id, Parent) \
  template <class... As> RetTy visit##Id##Type(Id##Type *T, Args... args) { \
    llvm_unreachable(#Id "Type should not survive to IR-gen"); \
  }
#define SUGARED_TYPE(Id, Parent) \
  template <class... As> RetTy visit##Id##Type(Id##Type *T, Args... args) { \
    llvm_unreachable(#Id "Type should not survive canonicalization"); \
  }
#include "swift/AST/TypeNodes.def"

  template <class... As>
  RetTy visitUnboundGenericType(UnboundGenericType *T, Args... args) {
    llvm_unreachable("UnboundGenericType should not survive Sema");
  }
};
  
} // end namespace Lowering
} // end namespace swift
  
#endif
