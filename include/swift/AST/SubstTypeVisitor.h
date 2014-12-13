//===-- SubstTypeVisitor.h - Visitor for substituted types ------*- C++ -*-===//
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
// This file defines SubstTypeVisitor, a CRTP-based visitor pattern for
// working with the results of type substitution.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_SUBSTTYPEVISITOR_H
#define SWIFT_AST_SUBSTTYPEVISITOR_H

#include "swift/AST/CanTypeVisitor.h"

namespace swift {

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
/// In addition to the usual delegation rules for visitors, all the
/// leaf type kinds map to the same function:
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
    case TypeKind::BuiltinNativeObject:
    case TypeKind::BuiltinBridgeObject:
    case TypeKind::BuiltinUnknownObject:
    case TypeKind::BuiltinRawPointer:
    case TypeKind::BuiltinUnsafeValueBuffer:
    case TypeKind::BuiltinVector:
    case TypeKind::Class: // FIXME: not a leaf because of the parent type
    case TypeKind::Module:
    case TypeKind::DynamicSelf:
    case TypeKind::Enum: // FIXME: not a leaf because of the parent type
    case TypeKind::ExistentialMetatype:
    case TypeKind::Protocol:
    case TypeKind::ProtocolComposition:
    case TypeKind::Struct: // FIXME: not a leaf because of the parent type
      return static_cast<Impl*>(this)->visitLeafType(origTy, substTy);

    case TypeKind::Archetype:
      return static_cast<Impl*>(this)
        ->visitArchetypeType(cast<ArchetypeType>(origTy), substTy);

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
    DISPATCH(BoundGenericClass)
    DISPATCH(BoundGenericEnum)
    DISPATCH(BoundGenericStruct)
    DISPATCH(Function)
    DISPATCH(LValue)
    DISPATCH(InOut)
    DISPATCH(Metatype)
    DISPATCH(PolymorphicFunction)
    DISPATCH(SILBlockStorage)
    DISPATCH(SILFunction)
    DISPATCH(UnmanagedStorage)
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
    return static_cast<Impl*>(this)->visit##Abstract(origTy, substTy); \
  }
  DEFER_TO_SUPERTYPE(Function, AnyFunctionType)
  DEFER_TO_SUPERTYPE(PolymorphicFunction, AnyFunctionType)
  DEFER_TO_SUPERTYPE(BoundGenericClass, BoundGenericType)
  DEFER_TO_SUPERTYPE(BoundGenericEnum, BoundGenericType)
  DEFER_TO_SUPERTYPE(BoundGenericStruct, BoundGenericType)
  DEFER_TO_SUPERTYPE(UnmanagedStorage, ReferenceStorageType)
  DEFER_TO_SUPERTYPE(UnownedStorage, ReferenceStorageType)
  DEFER_TO_SUPERTYPE(WeakStorage, ReferenceStorageType)
  DEFER_TO_SUPERTYPE(ReferenceStorage, Type)
  DEFER_TO_SUPERTYPE(BoundGeneric, Type)
  DEFER_TO_SUPERTYPE(AnyFunction, Type)
  DEFER_TO_SUPERTYPE(LValue, Type)
  DEFER_TO_SUPERTYPE(InOut, Type)
  DEFER_TO_SUPERTYPE(Metatype, Type)
  DEFER_TO_SUPERTYPE(SILBlockStorage, Type)
  DEFER_TO_SUPERTYPE(SILFunction, Type)
  DEFER_TO_SUPERTYPE(Tuple, Type)
#undef DEFER_TO_SUPERTYPE

  RetTy visitArchetypeType(CanArchetypeType origTy, CanType substTy) {
    return static_cast<Impl*>(this)->visitType(origTy, substTy);
  }
  RetTy visitLeafType(CanType origTy, CanType substTy) {
    return static_cast<Impl*>(this)->visitType(origTy, substTy);
  }
};
  
} // end namespace swift
  
#endif
