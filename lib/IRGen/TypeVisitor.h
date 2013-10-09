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
// This file defines various type visitors that are useful in
// IR-generation.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_TYPEVISITOR_H
#define SWIFT_IRGEN_TYPEVISITOR_H

#include "swift/AST/CanTypeVisitor.h"

namespace swift {
namespace irgen {

/// ReferenceTypeVisitor - This is a specialization of irgen::TypeVisitor
/// which automatically ignores non-reference types.
template <typename ImplClass, typename RetTy = void, typename... Args>
class ReferenceTypeVisitor : public CanTypeVisitor<ImplClass, RetTy, Args...> {
#define TYPE(Id) \
  RetTy visit##Id##Type(Can##Id##Type T, Args... args) { \
    llvm_unreachable(#Id "Type is not a reference type"); \
  }
  TYPE(Array)
  TYPE(BoundGenericEnum)
  TYPE(BoundGenericStruct)
  TYPE(BuiltinFloat)
  TYPE(BuiltinInteger)
  TYPE(BuiltinRawPointer)
  TYPE(BuiltinVector)
  TYPE(LValue)
  TYPE(MetaType)
  TYPE(Module)
  TYPE(Enum)
  TYPE(ReferenceStorage)
  TYPE(Struct)
  TYPE(Tuple)
#undef TYPE

  // BuiltinObjectPointer
  // BuiltinObjCPointer
  // Class
  // BoundGenericClass
  // Protocol
  // ProtocolComposition
  // Archetype
  // Function
  // PolymorphicFunction
};

/// SubstTypeVisitor - This is a specialized type visitor for visiting
/// both a type and the result of substituting it.  The original type
/// drives the selection, not the substitution result.
///
/// For most type kinds, the substitution type preserves the same
/// structure as the original, and so the methods you declare
/// should look like:
///   visitFunctionType(FunctionType *origTy, FunctionType *substTy);
///
/// Archetypes are an exception, and the second parameter should just
/// be a CanType:
///   visitArchetypeType(ArchetypeType *origTy, CanType substTy);
///
/// In addition, all the leaf type kinds map to the same function:
///   visitLeafType(CanType origTy, CanType substTy);
template<typename Impl, typename RetTy = void> class SubstTypeVisitor {
protected:
  typedef SubstTypeVisitor super;

public:
  RetTy visit(CanType origTy, CanType substTy) {
    switch (origTy->getKind()) {
#define TYPE(Id, Parent)
#define UNCHECKED_TYPE(Id, Parent) \
    case TypeKind::Id: \
      llvm_unreachable(#Id "Type should not survive to IR-gen");
#define SUGARED_TYPE(Id, Parent) \
    case TypeKind::Id: \
      llvm_unreachable(#Id "Type should not survive canonicalization");
#include "swift/AST/TypeNodes.def"

    case TypeKind::BuiltinFloat:
    case TypeKind::BuiltinInteger:
    case TypeKind::BuiltinObjectPointer:
    case TypeKind::BuiltinObjCPointer:
    case TypeKind::BuiltinRawPointer:
    case TypeKind::BuiltinVector:
    case TypeKind::Class:
    case TypeKind::Module:
    case TypeKind::Enum:
    case TypeKind::Protocol:
    case TypeKind::ProtocolComposition:
    case TypeKind::Struct:
      return static_cast<Impl*>(this)->visitLeafType(origTy, substTy);

    case TypeKind::Archetype:
      return static_cast<Impl*>(this)
        ->visitArchetypeType(cast<ArchetypeType>(origTy),
                             substTy);

    case TypeKind::GenericFunction:
    case TypeKind::GenericTypeParam:
    case TypeKind::DependentMember:
      // FIXME: This should duplicate, then subsume, the archetype path?
      llvm_unreachable("can't visit dependent types");

#define DISPATCH(Concrete)                                      \
    case TypeKind::Concrete:                                    \
      return static_cast<Impl*>(this)                           \
        ->visit##Concrete##Type(cast<Concrete##Type>(origTy),   \
                                cast<Concrete##Type>(substTy));
    DISPATCH(Array)
    DISPATCH(BoundGenericClass)
    DISPATCH(BoundGenericEnum)
    DISPATCH(BoundGenericStruct)
    DISPATCH(Function)
    DISPATCH(LValue)
    DISPATCH(MetaType)
    DISPATCH(PolymorphicFunction)
    DISPATCH(SILFunction)
    DISPATCH(UnownedStorage)
    DISPATCH(WeakStorage)
    DISPATCH(Tuple)
#undef DISPATCH
    }
    llvm_unreachable("bad type kind");
  }

#define DEFER_TO_SUPERTYPE(Concrete, Abstract)                               \
  RetTy visit##Concrete##Type(Can##Concrete##Type origTy,                    \
                              Can##Concrete##Type substTy) {                 \
    return static_cast<Impl*>(this)->visit##Abstract##Type(origTy, substTy); \
  }
  DEFER_TO_SUPERTYPE(Function, AnyFunction)
  DEFER_TO_SUPERTYPE(PolymorphicFunction, AnyFunction)
  DEFER_TO_SUPERTYPE(BoundGenericClass, BoundGeneric)
  DEFER_TO_SUPERTYPE(BoundGenericEnum, BoundGeneric)
  DEFER_TO_SUPERTYPE(BoundGenericStruct, BoundGeneric)
  DEFER_TO_SUPERTYPE(UnownedStorage, ReferenceStorage)
  DEFER_TO_SUPERTYPE(WeakStorage, ReferenceStorage)
#undef DEFER_TO_SUPERTYPE
};
  
} // end namespace irgen
} // end namespace swift
  
#endif
