//===--- GenType.h - Auxiliary Interface for Type IR Generation -*- C++ -*-===//
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
// This file defines the private interface used for turning AST types
// into LLVM IR types.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_GENTYPE_H
#define SWIFT_IRGEN_GENTYPE_H

#include "llvm/ADT/DenseMap.h"

namespace llvm {
  class Constant;
}

namespace swift {
  class ArchetypeType;
  class ArrayType;
  class CanType;
  class ClassDecl;
  class AnyFunctionType;
  class LValueType;
  class MetaTypeType;
  class ModuleType;
  class NominalTypeDecl;
  class OneOfDecl;
  class ProtocolCompositionType;
  class ProtocolDecl;
  class ProtocolType;
  class StructDecl;
  class TupleType;
  class TypeBase;
  class Type;

namespace irgen {
  class Alignment;
  class IRGenModule;
  class ProtocolInfo;
  class Size;
  class TypeInfo;

/// The helper class for generating types.
class TypeConverter {
  IRGenModule &IGM;
  llvm::DenseMap<TypeBase*, const TypeInfo*> Types;
  llvm::DenseMap<ProtocolDecl*, const ProtocolInfo*> Protocols;
  llvm::DenseMap<TypeBase*, llvm::Constant*> TrivialWitnessTables;
  const TypeInfo *FirstType;
  const ProtocolInfo *FirstProtocol;

  friend class GenProto;

  static const TypeInfo *createPrimitive(llvm::Type *T,
                                         Size size, Alignment align);

  const TypeInfo *convertType(CanType T);
  const TypeInfo *convertTupleType(TupleType *T);
  const TypeInfo *convertOneOfType(OneOfDecl *D);
  const TypeInfo *convertStructType(StructDecl *D);
  const TypeInfo *convertClassType(ClassDecl *D);
  const TypeInfo *convertFunctionType(AnyFunctionType *T);
  const TypeInfo *convertArchetypeType(ArchetypeType *T);
  const TypeInfo *convertArrayType(ArrayType *T);
  const TypeInfo *convertLValueType(LValueType *T);
  const TypeInfo *convertMetaTypeType(MetaTypeType *T);
  const TypeInfo *convertModuleType(ModuleType *T);
  const TypeInfo *convertProtocolType(ProtocolType *T);
  const TypeInfo *convertProtocolCompositionType(ProtocolCompositionType *T);
  const TypeInfo *convertBuiltinObjectPointer();
  const TypeInfo *convertBuiltinObjCPointer();
  const TypeInfo *convertBoundGenericType(NominalTypeDecl *D);

public:
  TypeConverter(IRGenModule &IGM);
  ~TypeConverter();
  const TypeInfo &getFragileTypeInfo(Type T);
  const ProtocolInfo &getProtocolInfo(ProtocolDecl *P);
};

} // end namespace irgen
} // end namespace swift

#endif
