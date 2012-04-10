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
#include "llvm/Target/TargetData.h"

#include "GenType.h"
#include "IRGenFunction.h"
#include "IRGenModule.h"
#include "Address.h"
#include "Explosion.h"
#include "Linking.h"

using namespace swift;
using namespace irgen;

bool TypeInfo::isSingleRetainablePointer(ResilienceScope scope) const {
  return false;
}

ExplosionSchema TypeInfo::getSchema(ExplosionKind kind) const {
  ExplosionSchema schema(kind);
  getSchema(schema);
  return schema;
}

/// Copy a value from one object to a new object, directly taking
/// responsibility for anything it might have.  This is like C++
/// move-initialization, except the old object will not be destroyed.
void TypeInfo::initializeWithTake(IRGenFunction &IGF,
                                  Address destAddr, Address srcAddr) const {
  // Prefer loads and stores if we won't make a million of them.
  // Maybe this should also require the scalars to have a fixed offset.
  ExplosionSchema schema = getSchema(ExplosionKind::Maximal);
  if (!schema.containsAggregate() && schema.size() <= 2) {
    Explosion copy(ExplosionKind::Maximal);
    load(IGF, srcAddr, copy);
    initialize(IGF, copy, destAddr);
    return;
  }

  // Otherwise, use a memcpy.
  IGF.emitMemCpy(destAddr.getAddress(), srcAddr.getAddress(),
                 StorageSize, std::min(destAddr.getAlignment(),
                                       srcAddr.getAlignment()));
}

static TypeInfo *invalidTypeInfo() { return (TypeInfo*) 1; }

namespace {
  /// Basic IR generation for primitive types, which are always
  /// represented as a single scalar.
  class PrimitiveTypeInfo : public TypeInfo {
  public:
    PrimitiveTypeInfo(llvm::Type *Type, Size S, Alignment A)
      : TypeInfo(Type, S, A, IsPOD) {}

    unsigned getExplosionSize(ExplosionKind kind) const {
      return 1;
    }

    void getSchema(ExplosionSchema &schema) const {
      schema.add(ExplosionSchema::Element::forScalar(getStorageType()));
    }

    void load(IRGenFunction &IGF, Address addr, Explosion &e) const {
      e.addUnmanaged(IGF.Builder.CreateLoad(addr));
    }

    void assign(IRGenFunction &IGF, Explosion &e, Address addr) const {
      IGF.Builder.CreateStore(e.claimUnmanagedNext(), addr);
    }

    void initialize(IRGenFunction &IGF, Explosion &e, Address addr) const {
      IGF.Builder.CreateStore(e.claimUnmanagedNext(), addr);
    }

    void reexplode(IRGenFunction &IGF, Explosion &src, Explosion &dest) const {
      src.transferInto(dest, 1);
    }

    void manage(IRGenFunction &IGF, Explosion &src, Explosion &dest) const {
      src.transferInto(dest, 1);
    }

    void destroy(IRGenFunction &IGF, Address addr) const {}
  };
}

/// Constructs a type info which performs simple loads and stores of
/// the given IR type.
const TypeInfo *TypeConverter::createPrimitive(llvm::Type *type,
                                               Size size, Alignment align) {
  return new PrimitiveTypeInfo(type, size, align);
}

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

const TypeInfo &TypeConverter::getFragileTypeInfo(IRGenModule &IGM,
                                                  Type sugaredTy) {
  assert(!sugaredTy.isNull());
  CanType canonicalTy = sugaredTy->getCanonicalType();

  auto entry = IGM.Types.Converted.find(canonicalTy.getPointer());
  if (entry != IGM.Types.Converted.end())
    return *entry->second;

  const TypeInfo *result = convertType(IGM, canonicalTy);
  IGM.Types.Converted[canonicalTy.getPointer()] = result;

  // If the type info hasn't been added to the list of types, do so.
  if (!result->NextConverted) {
    result->NextConverted = IGM.Types.FirstConverted;
    IGM.Types.FirstConverted = result;
  }

  return *result;
}

const TypeInfo *TypeConverter::convertType(IRGenModule &IGM, CanType canTy) {
  llvm::LLVMContext &Ctx = IGM.getLLVMContext();
  TypeBase *ty = canTy.getPointer();
  switch (ty->getKind()) {
#define UNCHECKED_TYPE(id, parent) \
  case TypeKind::id: \
    llvm_unreachable("found a " #id "Type in IR-gen");
#define SUGARED_TYPE(id, parent) \
  case TypeKind::id: \
    llvm_unreachable("converting a " #id "Type after canonicalization");
#define TYPE(id, parent)
#include "swift/AST/TypeNodes.def"
  case TypeKind::MetaType:
    return convertMetaTypeType(IGM, cast<MetaTypeType>(ty));
  case TypeKind::Module:
    return convertModuleType(IGM, cast<ModuleType>(ty));
  case TypeKind::BuiltinRawPointer:
    return createPrimitive(IGM.Int8PtrTy, IGM.getPointerSize(),
                           IGM.getPointerAlignment());
  case TypeKind::BuiltinObjectPointer:
    return convertBuiltinObjectPointer(IGM);
  case TypeKind::BuiltinFloat:
    switch (cast<BuiltinFloatType>(ty)->getFPKind()) {
    case BuiltinFloatType::IEEE16:
      return createPrimitive(llvm::Type::getHalfTy(Ctx),
                             Size(2), Alignment(2));
    case BuiltinFloatType::IEEE32:
      return createPrimitive(llvm::Type::getFloatTy(Ctx),
                             Size(4), Alignment(4));
    case BuiltinFloatType::IEEE64:
      return createPrimitive(llvm::Type::getDoubleTy(Ctx),
                             Size(8), Alignment(8));
    case BuiltinFloatType::IEEE80:
      return createPrimitive(llvm::Type::getX86_FP80Ty(Ctx),
                             Size(10), Alignment(16));
    case BuiltinFloatType::IEEE128:
      return createPrimitive(llvm::Type::getFP128Ty(Ctx),
                             Size(16), Alignment(16));
    case BuiltinFloatType::PPC128:
      return createPrimitive(llvm::Type::getPPC_FP128Ty(Ctx),
                             Size(16), Alignment(16));
    }
    llvm_unreachable("bad builtin floating-point type kind");
  case TypeKind::BuiltinInteger: {
    unsigned BitWidth = cast<BuiltinIntegerType>(ty)->getBitWidth();
    unsigned ByteSize = (BitWidth+7U)/8U;
    // Round up the memory size and alignment to a power of 2. 
    if (!llvm::isPowerOf2_32(ByteSize))
      ByteSize = llvm::NextPowerOf2(ByteSize);
    
    return createPrimitive(llvm::IntegerType::get(Ctx, BitWidth),
                           Size(ByteSize), Alignment(ByteSize));
  }
  case TypeKind::LValue:
    return convertLValueType(IGM, cast<LValueType>(ty));
  case TypeKind::Tuple:
    return convertTupleType(IGM, cast<TupleType>(ty));
  case TypeKind::OneOf:
    return convertOneOfType(IGM, cast<OneOfType>(ty));
  case TypeKind::Function:
    return convertFunctionType(IGM, cast<FunctionType>(ty));
  case TypeKind::Array:
    return convertArrayType(IGM, cast<ArrayType>(ty));
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
  LinkEntity::forNonFunction(alias).mangle(nameStream);
  
  return llvm::StructType::create(getLLVMContext(), nameStream.str());
}

/// Compute the explosion schema for the given type.
void IRGenModule::getSchema(Type type, ExplosionSchema &schema) {
  // As an optimization, avoid actually building a TypeInfo for any
  // obvious TupleTypes.  This assumes that a TupleType's explosion
  // schema is always the concatenation of its component's schemata.
  if (TupleType *tuple = dyn_cast<TupleType>(type)) {
    for (const TupleTypeElt &field : tuple->getFields())
      getSchema(field.getType(), schema);
    return;
  }

  // Okay, that didn't work;  just do the general thing.
  getFragileTypeInfo(type).getSchema(schema);
}
