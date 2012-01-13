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
#include "llvm/ADT/SmallString.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/ErrorHandling.h"

#include "GenType.h"
#include "IRGenFunction.h"
#include "IRGenModule.h"
#include "Address.h"
#include "RValue.h"
#include "Explosion.h"

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

    unsigned getExplosionSize(ExplosionKind kind) const {
      return 1;
    }

    void getExplosionSchema(ExplosionSchema &schema) const {
      schema.add(ExplosionSchema::Element::forScalar(getStorageType()));
    }

    RValue load(IRGenFunction &IGF, Address addr) const {
      return RValue::forScalars(IGF.Builder.CreateLoad(addr));
    }

    void loadExplosion(IRGenFunction &IGF, Address addr, Explosion &e) const {
      e.add(IGF.Builder.CreateLoad(addr));
    }

    void store(IRGenFunction &IGF, const RValue &RV, Address addr) const {
      assert(RV.isScalar() && RV.getScalars().size() == 1);
      IGF.Builder.CreateStore(RV.getScalars()[0], addr);
    }

    void storeExplosion(IRGenFunction &IGF, Explosion &e, Address addr) const {
      IGF.Builder.CreateStore(e.claimNext(), addr);
    }
  };
}

void TypeInfo::_anchor() {}

void TypeInfo::explode(IRGenFunction &IGF, const RValue &rvalue,
                       Explosion &explosion) const {
  if (rvalue.isScalar())
    return explosion.add(rvalue.getScalars());

  Address addr(rvalue.getAggregateAddress(), StorageAlignment);
  loadExplosion(IGF, addr, explosion);
}

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
  llvm::LLVMContext &Ctx = IGM.getLLVMContext();
  TypeBase *TB = T.getPointer();
  switch (TB->Kind) {
  case TypeKind::Error:
    llvm_unreachable("generating an error type");
  case TypeKind::Dependent:
    llvm_unreachable("generating a dependent type");
  case TypeKind::MetaType:
  case TypeKind::Module:
    llvm_unreachable("cannot codegen this type - no runtime representation");
  case TypeKind::BuiltinFloat:
    switch (cast<BuiltinFloatType>(T)->getFPKind()) {
    case BuiltinFloatType::IEEE16:
      return new PrimitiveTypeInfo(llvm::Type::getHalfTy(Ctx),
                                   Size(2), Alignment(2));
    case BuiltinFloatType::IEEE32:
      return new PrimitiveTypeInfo(llvm::Type::getFloatTy(Ctx),
                                   Size(4), Alignment(4));
    case BuiltinFloatType::IEEE64:
      return new PrimitiveTypeInfo(llvm::Type::getDoubleTy(Ctx),
                                   Size(8), Alignment(8));
    case BuiltinFloatType::IEEE80:
      return new PrimitiveTypeInfo(llvm::Type::getX86_FP80Ty(Ctx),
                                   Size(10), Alignment(16));
    case BuiltinFloatType::IEEE128:
      return new PrimitiveTypeInfo(llvm::Type::getFP128Ty(Ctx),
                                   Size(16), Alignment(16));
    case BuiltinFloatType::PPC128:
      return new PrimitiveTypeInfo(llvm::Type::getPPC_FP128Ty(Ctx),
                                   Size(16), Alignment(16));
    }
    llvm_unreachable("bad builtin floating-point type kind");
  case TypeKind::BuiltinInteger: {
    unsigned BitWidth = cast<BuiltinIntegerType>(T)->getBitWidth();
    unsigned ByteSize = (BitWidth+7U)/8U;
    // Round up the memory size and alignment to a power of 2. 
    if (!llvm::isPowerOf2_32(ByteSize))
      ByteSize = llvm::NextPowerOf2(ByteSize);
    
    return new PrimitiveTypeInfo(llvm::IntegerType::get(Ctx, BitWidth),
                                 Size(ByteSize), Alignment(ByteSize));
  }
  case TypeKind::Paren:
    return &getFragileTypeInfo(IGM, cast<ParenType>(TB)->getUnderlyingType());
  case TypeKind::NameAlias:
    return &getFragileTypeInfo(IGM,
                      cast<NameAliasType>(TB)->TheDecl->getUnderlyingType());
  case TypeKind::Tuple:
    return convertTupleType(IGM, cast<TupleType>(TB));
  case TypeKind::OneOf:
    return convertOneOfType(IGM, cast<OneOfType>(TB));
  case TypeKind::Function:
    return convertFunctionType(IGM, cast<FunctionType>(TB));
  case TypeKind::Array:
    return convertArrayType(IGM, cast<ArrayType>(TB));
  case TypeKind::Protocol:
    llvm_unreachable("protocol not handled in IRGen yet");
  }
  llvm_unreachable("bad type kind");
}

/// emitTypeAlias - Emit a type alias.  You wouldn't think that these
/// would need IR support, but we apparently want to emit struct and
/// oneof declarations as these instead of as their own declarations.
void IRGenModule::emitTypeAlias(Type underlyingType) {
  if (OneOfType *oneof = dyn_cast<OneOfType>(underlyingType))
    return emitOneOfType(oneof);
}

/// createNominalType - Create a new nominal type.
llvm::StructType *IRGenModule::createNominalType(TypeAliasDecl *alias) {
  llvm::SmallString<32> typeName;
  llvm::raw_svector_ostream nameStream(typeName);
  mangle(nameStream, alias);
  
  return llvm::StructType::create(getLLVMContext(), nameStream.str());
}

/// Compute the explosion schema for the given type.
void IRGenModule::getExplosionSchema(Type type, ExplosionSchema &schema) {
  // As an optimization, avoid actually building a TypeInfo for any
  // obvious TupleTypes.  This assumes that a TupleType's explosion
  // schema is always the concatenation of its components schemata.
  if (TupleType *tuple = dyn_cast<TupleType>(type)) {
    for (const TupleTypeElt &field : tuple->Fields)
      getExplosionSchema(field.Ty, schema);
    return;
  }

  // Okay, that didn't work;  just do the general thing.
  getFragileTypeInfo(type).getExplosionSchema(schema);
}

