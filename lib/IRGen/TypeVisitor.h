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

  RetTy visitDeducibleGenericParamType(DeducibleGenericParamType *T) {
    llvm_unreachable("DeducibleGenericParamType should not survive Sema");
  }
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
///   visitArchetypeType(FunctionType *origTy, CanType substTy);
///
/// In addition, all the leaf type kinds map to the same function:
///   visitLeafType(CanType origTy, CanType substTy);
template<typename Impl, typename RetTy = void> class SubstTypeVisitor {
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

    case TypeKind::DeducibleGenericParam:
      llvm_unreachable("DeducibleGenericParamType should not survive Sema");

    case TypeKind::BuiltinFloat:
    case TypeKind::BuiltinInteger:
    case TypeKind::BuiltinObjectPointer:
    case TypeKind::BuiltinObjCPointer:
    case TypeKind::BuiltinRawPointer:
    case TypeKind::Class:
    case TypeKind::Module:
    case TypeKind::OneOf:
    case TypeKind::Protocol:
    case TypeKind::ProtocolComposition:
    case TypeKind::Struct:
      return static_cast<Impl*>(this)->visitLeafType(origTy, substTy);

    case TypeKind::Archetype:
      return static_cast<Impl*>(this)
        ->visitArchetypeType(cast<ArchetypeType>(origTy),
                             substTy);

    case TypeKind::Array:
      return static_cast<Impl*>(this)
        ->visitArrayType(cast<ArrayType>(origTy),
                         cast<ArrayType>(substTy));

    case TypeKind::BoundGeneric:
      return static_cast<Impl*>(this)
        ->visitBoundGenericType(cast<BoundGenericType>(origTy),
                                cast<BoundGenericType>(substTy));

    case TypeKind::Function:
      return static_cast<Impl*>(this)
        ->visitFunctionType(cast<FunctionType>(origTy),
                            cast<FunctionType>(substTy));

    case TypeKind::PolymorphicFunction:
      return static_cast<Impl*>(this)
        ->visitPolymorphicFunctionType(cast<PolymorphicFunctionType>(origTy),
                                       cast<PolymorphicFunctionType>(substTy));

    case TypeKind::LValue:
      return static_cast<Impl*>(this)
        ->visitLValueType(cast<LValueType>(origTy),
                          cast<LValueType>(substTy));

    case TypeKind::MetaType:
      return static_cast<Impl*>(this)
        ->visitMetaTypeType(cast<MetaTypeType>(origTy),
                            cast<MetaTypeType>(substTy));

    case TypeKind::Tuple:
      return static_cast<Impl*>(this)
        ->visitTupleType(cast<TupleType>(origTy),
                         cast<TupleType>(substTy));
    }
    llvm_unreachable("bad type kind");
  }

  RetTy visitFunctionType(FunctionType *origTy, FunctionType *substTy) {
    return static_cast<Impl*>(this)->visitAnyFunctionType(origTy, substTy);
  }

  RetTy visitPolymorphicFunctionType(PolymorphicFunctionType *origTy,
                                     PolymorphicFunctionType *substTy) {
    return static_cast<Impl*>(this)->visitAnyFunctionType(origTy, substTy);
  }
};
  
} // end namespace irgen
} // end namespace swift
  
#endif
