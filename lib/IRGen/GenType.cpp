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

#include "FixedTypeInfo.h"
#include "GenType.h"
#include "IRGenFunction.h"
#include "IRGenModule.h"
#include "Address.h"
#include "Explosion.h"
#include "Linking.h"
#include "ProtocolInfo.h"
#include "ScalarTypeInfo.h"

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
    loadAsTake(IGF, srcAddr, copy);
    initialize(IGF, copy, destAddr);
    return;
  }

  // Otherwise, use a memcpy.
  IGF.emitMemCpy(destAddr, srcAddr, StorageSize);
}

/// Copy a value from one object to a new object.  This is just the
/// default implementation.
void TypeInfo::initializeWithCopy(IRGenFunction &IGF,
                                  Address destAddr, Address srcAddr) const {
  // Use memcpy if that's legal.
  if (isPOD(ResilienceScope::Local)) {
    return initializeWithTake(IGF, destAddr, srcAddr);
  }

  // Otherwise explode and re-implode.
  Explosion copy(ExplosionKind::Maximal);
  load(IGF, srcAddr, copy);
  initialize(IGF, copy, destAddr);
}

/// Return the size and alignment of this type.
/// TODO: this needs to be potentially virtual.
std::pair<llvm::Value*,llvm::Value*>
TypeInfo::getSizeAndAlignment(IRGenFunction &IGF) const {
  return std::make_pair(llvm::ConstantInt::get(IGF.IGM.SizeTy,
                                               StorageSize.getValue()),
                        llvm::ConstantInt::get(IGF.IGM.SizeTy,
                                               StorageAlignment.getValue()));
}

// Eventually optimizable.
llvm::Value *TypeInfo::getSizeOnly(IRGenFunction &IGF) const {
  return getSizeAndAlignment(IGF).first;
}

// Eventually optimizable.
llvm::Value *TypeInfo::getAlignmentOnly(IRGenFunction &IGF) const {
  return getSizeAndAlignment(IGF).second;
}

namespace {
  class PrimitiveTypeInfo :
    public PODSingleScalarTypeInfo<PrimitiveTypeInfo, FixedTypeInfo> {
  public:
    PrimitiveTypeInfo(llvm::Type *storage, Size size, Alignment align)
      : PODSingleScalarTypeInfo(storage, size, align) {}
  };
}

/// Constructs a type info which performs simple loads and stores of
/// the given IR type.
const TypeInfo *TypeConverter::createPrimitive(llvm::Type *type,
                                               Size size, Alignment align) {
  return new PrimitiveTypeInfo(type, size, align);
}

static TypeInfo *invalidTypeInfo() { return (TypeInfo*) 1; }
static ProtocolInfo *invalidProtocolInfo() { return (ProtocolInfo*) 1; }

TypeConverter::TypeConverter(IRGenModule &IGM)
  : IGM(IGM), FirstType(invalidTypeInfo()),
    FirstProtocol(invalidProtocolInfo()) {}

TypeConverter::~TypeConverter() {
  // Delete all the converted type infos.
  for (const TypeInfo *I = FirstType; I != invalidTypeInfo(); ) {
    const TypeInfo *Cur = I;
    I = Cur->NextConverted;
    delete Cur;
  }

  for (const ProtocolInfo *I = FirstProtocol; I != invalidProtocolInfo(); ) {
    const ProtocolInfo *Cur = I;
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
  return Types.getFragileTypeInfo(T);
}

const TypeInfo &TypeConverter::getFragileTypeInfo(Type sugaredTy) {
  assert(!sugaredTy.isNull());
  CanType canonicalTy = sugaredTy->getCanonicalType();

  auto entry = Types.find(canonicalTy.getPointer());
  if (entry != Types.end())
    return *entry->second;

  const TypeInfo *result = convertType(canonicalTy);
  Types[canonicalTy.getPointer()] = result;

  // If the type info hasn't been added to the list of types, do so.
  if (!result->NextConverted) {
    result->NextConverted = FirstType;
    FirstType = result;
  }

  return *result;
}

const TypeInfo *TypeConverter::convertType(CanType canTy) {
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
    return convertMetaTypeType(cast<MetaTypeType>(ty));
  case TypeKind::Module:
    return convertModuleType(cast<ModuleType>(ty));
  case TypeKind::BuiltinRawPointer:
    return createPrimitive(IGM.Int8PtrTy, IGM.getPointerSize(),
                           IGM.getPointerAlignment());
  case TypeKind::BuiltinObjectPointer:
    return convertBuiltinObjectPointer();
  case TypeKind::BuiltinObjCPointer:
    return convertBuiltinObjCPointer();
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

  case TypeKind::Archetype:
    return convertArchetypeType(cast<ArchetypeType>(ty));
  case TypeKind::LValue:
    return convertLValueType(cast<LValueType>(ty));
  case TypeKind::Tuple:
    return convertTupleType(cast<TupleType>(ty));
  case TypeKind::OneOf:
    return convertOneOfType(cast<OneOfType>(ty));
  case TypeKind::Struct:
    return convertStructType(cast<StructType>(ty));
  case TypeKind::Class:
    return convertClassType(cast<ClassType>(ty));
  case TypeKind::Function:
    return convertFunctionType(cast<FunctionType>(ty));
  case TypeKind::Array:
    return convertArrayType(cast<ArrayType>(ty));
  case TypeKind::Protocol:
    return convertProtocolType(cast<ProtocolType>(ty));
      
  case TypeKind::ProtocolComposition:
      llvm_unreachable("ProtocolComposition types are unimplemented");
  }
  llvm_unreachable("bad type kind");
}

/// createNominalType - Create a new nominal type.
llvm::StructType *IRGenModule::createNominalType(TypeDecl *type) {
  llvm::SmallString<32> typeName;
  if (type->getDeclContext()->isLocalContext()) {
    typeName = type->getName().str();
    typeName.append(".local");
  } else {
    llvm::raw_svector_ostream nameStream(typeName);
    LinkEntity::forNonFunction(type).mangle(nameStream);
  }
  return llvm::StructType::create(getLLVMContext(), typeName.str());
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
