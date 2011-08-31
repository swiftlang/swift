//===--- GenTypes.cpp - Swift IR Generation For Types ---------------------===//
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
//  This file implements IR generation for types in Swift.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/Decl.h"
#include "swift/AST/Types.h"
#include "llvm/DerivedTypes.h"
#include "llvm/Support/ErrorHandling.h"

#include "GenType.h"
#include "IRGenFunction.h"
#include "IRGenModule.h"
#include "LValue.h"
#include "RValue.h"

using namespace swift;
using namespace irgen;

namespace {
  /// Basic IR generation for primitive types, which are always
  /// represented as a single scalar.
  class PrimitiveTypeInfo : public TypeInfo {
  public:
    PrimitiveTypeInfo(llvm::Type *Type, Size S, Alignment A)
      : TypeInfo(Type, S, A) {}

    RValueSchema getSchema() const {
      return RValueSchema::forScalars(getStorageType());
    }

    RValue load(IRGenFunction &IGF, const LValue &LV) const {
      llvm::Value *Load =
        IGF.Builder.CreateLoad(LV.getAddress(), LV.getAlignment(),
                               LV.getAddress()->getName() + ".load");
      return RValue::forScalars(Load);
    }

    void store(IRGenFunction &IGF, const RValue &RV, const LValue &LV) const {
      assert(RV.isScalar() && RV.getScalars().size() == 1);
      IGF.Builder.CreateStore(RV.getScalars()[0], LV.getAddress(),
                              LV.getAlignment());
    }
  };
}

void TypeInfo::_anchor() {}

static TypeInfo *invalidTypeInfo() { return (TypeInfo*) 1; }

TypeConverter::TypeConverter() : FirstConverted(invalidTypeInfo()) {}

TypeConverter::~TypeConverter() {
  // Delete all the converted type infos.
  for (const TypeInfo *I = FirstConverted; I != invalidTypeInfo(); ) {
    const TypeInfo *Cur = I;
    I = Cur->NextConverted;
    delete Cur;
  }
}

/// Get the fragile type information for the given type.
const TypeInfo &IRGenFunction::getFragileTypeInfo(Type T) {
  return IGM.getFragileTypeInfo(T);
}

/// Get the fragile IR type for the given type.
llvm::Type *IRGenModule::getFragileType(Type T) {
  return getFragileTypeInfo(T).StorageType;
}

/// Get the fragile type information for the given type.
const TypeInfo &IRGenModule::getFragileTypeInfo(Type T) {
  return TypeConverter::getFragileTypeInfo(*this, T);
}

const TypeInfo &TypeConverter::getFragileTypeInfo(IRGenModule &IGM, Type T) {
  assert(!T.isNull());
  auto Entry = IGM.Types.Converted.find(T.getPointer());
  if (Entry != IGM.Types.Converted.end())
    return *Entry->second;

  const TypeInfo *Result = convertType(IGM, T);
  IGM.Types.Converted[T.getPointer()] = Result;

  // If the type info hasn't been added to the list of types, do so.
  if (!Result->NextConverted) {
    Result->NextConverted = IGM.Types.FirstConverted;
    IGM.Types.FirstConverted = Result;
  }

  return *Result;
}

const TypeInfo *TypeConverter::convertType(IRGenModule &IGM, Type T) {
  TypeBase *TB = T.getPointer();
  switch (TB->Kind) {
  case TypeKind::Error:
    llvm_unreachable("generating an error type");
  case TypeKind::Dependent:
    llvm_unreachable("generating a dependent type");
  case TypeKind::BuiltinFloat32:
    return new PrimitiveTypeInfo(llvm::Type::getFloatTy(IGM.getLLVMContext()),
                                 Size(4), Alignment(4));
  case TypeKind::BuiltinFloat64:
    return new PrimitiveTypeInfo(llvm::Type::getDoubleTy(IGM.getLLVMContext()),
                                 Size(8), Alignment(8));
  case TypeKind::BuiltinInt1:
    return new PrimitiveTypeInfo(IGM.Int1Ty, Size(1), Alignment(1));
  case TypeKind::BuiltinInt8:
    return new PrimitiveTypeInfo(IGM.Int8Ty, Size(1), Alignment(1));
  case TypeKind::BuiltinInt16:
    return new PrimitiveTypeInfo(IGM.Int16Ty, Size(2), Alignment(2));
  case TypeKind::BuiltinInt32:
    return new PrimitiveTypeInfo(IGM.Int32Ty, Size(4), Alignment(4));
  case TypeKind::BuiltinInt64:
    return new PrimitiveTypeInfo(IGM.Int64Ty, Size(8), Alignment(8));
  case TypeKind::NameAlias:
    return &getFragileTypeInfo(IGM,
                               cast<NameAliasType>(TB)->TheDecl->UnderlyingTy);
  case TypeKind::Tuple:
    return convertTupleType(IGM, cast<TupleType>(TB));
  case TypeKind::OneOf:
    return convertOneOfType(IGM, cast<OneOfType>(TB));
  case TypeKind::Function:
    return convertFunctionType(IGM, cast<FunctionType>(TB));
  case TypeKind::Array:
    return convertArrayType(IGM, cast<ArrayType>(TB));
  }
  llvm_unreachable("bad type kind");
}
