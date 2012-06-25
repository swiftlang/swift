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

namespace swift {
  class ArrayType;
  class CanType;
  class ClassType;
  class FunctionType;
  class LValueType;
  class MetaTypeType;
  class ModuleType;
  class OneOfType;
  class ProtocolType;
  class StructType;
  class TupleType;
  class TypeBase;
  class Type;

namespace irgen {
  class Alignment;
  class IRGenModule;
  class Size;
  class TypeInfo;

/// The helper class for generating types.
class TypeConverter {
  llvm::DenseMap<TypeBase*, const TypeInfo*> Converted;
  const TypeInfo *FirstConverted;

  static const TypeInfo *createPrimitive(llvm::Type *T,
                                         Size size, Alignment align);
    
  static const TypeInfo *convertType(IRGenModule &IGM, CanType T);
  static const TypeInfo *convertTupleType(IRGenModule &IGM, TupleType *T);
  static const TypeInfo *convertOneOfType(IRGenModule &IGM, OneOfType *T);
  static const TypeInfo *convertStructType(IRGenModule &IGM, StructType *T);
  static const TypeInfo *convertClassType(IRGenModule &IGM, ClassType *T);
  static const TypeInfo *convertFunctionType(IRGenModule &IGM, FunctionType *T);
  static const TypeInfo *convertArrayType(IRGenModule &IGM, ArrayType *T);
  static const TypeInfo *convertLValueType(IRGenModule &IGM, LValueType *T);
  static const TypeInfo *convertMetaTypeType(IRGenModule &IGM, MetaTypeType *T);
  static const TypeInfo *convertModuleType(IRGenModule &IGM, ModuleType *T);
  static const TypeInfo *convertProtocolType(IRGenModule &IGM, ProtocolType *T);
  static const TypeInfo *convertBuiltinObjectPointer(IRGenModule &IGM);
  static const TypeInfo *convertBuiltinObjCPointer(IRGenModule &IGM);

 public:
  TypeConverter();
  ~TypeConverter();
  static const TypeInfo &getFragileTypeInfo(IRGenModule &IGM, Type T);
};

} // end namespace irgen
} // end namespace swift

#endif
