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
// This file defines the interface used 
// the AST into LLVM IR.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_GENTYPE_H
#define SWIFT_IRGEN_GENTYPE_H

#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/OwningPtr.h"

namespace llvm {
  class Type;
}

namespace swift {
  class ArrayType;
  class FunctionType;
  class OneOfType;
  class TupleType;
  class Type;

namespace irgen {
  class IRGenModule;

/// Information about the IR representation and generation of the
/// given type.
class TypeInfo {
  TypeInfo(const TypeInfo &) = delete;
  TypeInfo &operator=(const TypeInfo &) = delete;

  friend class TypeConverter;
  mutable const TypeInfo *NextConverted;

public:
  TypeInfo(llvm::Type *Type, unsigned Size, unsigned Alignment)
    : NextConverted(0), Type(Type), SizeInBytes(Size),
      AlignmentInBytes(Alignment) {}

  virtual ~TypeInfo() {}

  /// The LLVM representation of a value of this type.
  llvm::Type *Type;

  /// The size of this type in bytes.
  unsigned SizeInBytes;

  /// The alignment of this type in bytes.
  unsigned AlignmentInBytes;

  /// Answers whether this type info has been completed.
  bool isComplete() const { return AlignmentInBytes != 0; }

private:
  virtual void _anchor();
};

/// The hepler class for generating types.
class TypeConverter {
  llvm::DenseMap<TypeBase*, const TypeInfo*> Converted;
  const TypeInfo *FirstConverted;
    
  static const TypeInfo *convertType(IRGenModule &IGM, Type T);
  static const TypeInfo *convertTupleType(IRGenModule &IGM, TupleType *T);
  static const TypeInfo *convertOneOfType(IRGenModule &IGM, OneOfType *T);
  static const TypeInfo *convertFunctionType(IRGenModule &IGM, FunctionType *T);
  static const TypeInfo *convertArrayType(IRGenModule &IGM, ArrayType *T);

 public:
  TypeConverter();
  ~TypeConverter();
  static const TypeInfo &getFragileTypeInfo(IRGenModule &IGM, Type T);
};

} // end namespace irgen
} // end namespace swift

#endif
