//===--- TypeVisitor.h - IR-gen TypeVisitor specialization ------*- C++ -*-===//
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
// This file defines various type visitors that are useful in
// IR-generation.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_TYPEVISITOR_H
#define SWIFT_IRGEN_TYPEVISITOR_H

#include "swift/AST/CanTypeVisitor.h"

namespace swift {
namespace irgen {

/// ReferenceTypeVisitor - This is a specialization of CanTypeVisitor
/// which automatically ignores non-reference types.
template <typename ImplClass, typename RetTy = void, typename... Args>
class ReferenceTypeVisitor : public CanTypeVisitor<ImplClass, RetTy, Args...> {
#define TYPE(Id) \
  RetTy visit##Id##Type(Can##Id##Type T, Args... args) { \
    llvm_unreachable(#Id "Type is not a reference type"); \
  }
  TYPE(BoundGenericEnum)
  TYPE(BoundGenericStruct)
  TYPE(BuiltinFloat)
  TYPE(BuiltinInteger)
  TYPE(BuiltinRawPointer)
  TYPE(BuiltinVector)
  TYPE(LValue)
  TYPE(Metatype)
  TYPE(Module)
  TYPE(Enum)
  TYPE(ReferenceStorage)
  TYPE(Struct)
  TYPE(Tuple)
#undef TYPE

  // BuiltinNativeObject
  // BuiltinBridgeObject
  // Class
  // BoundGenericClass
  // Protocol
  // ProtocolComposition
  // Archetype
  // Function
};
  
} // end namespace irgen
} // end namespace swift
  
#endif
