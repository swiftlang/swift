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
#include "swift/AST/PrettyStackTrace.h"
#include "swift/AST/Types.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/Support/ErrorHandling.h"

#include "FixedTypeInfo.h"
#include "GenType.h"
#include "IRGenFunction.h"
#include "IRGenModule.h"
#include "Address.h"
#include "Explosion.h"
#include "Linking.h"
#include "ProtocolInfo.h"
#include "ScalarTypeInfo.h"
#include "TypeVisitor.h"

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

Address TypeInfo::getAddressForPointer(llvm::Value *ptr) const {
  assert(ptr->getType()->getPointerElementType() == StorageType);
  return Address(ptr, StorageAlignment);
}

Address TypeInfo::getUndefAddress() const {
  return Address(llvm::UndefValue::get(getStorageType()->getPointerTo(0)),
                 StorageAlignment);
}

/// Whether this type is known to be empty.
bool TypeInfo::isKnownEmpty() const {
  if (auto fixed = dyn_cast<FixedTypeInfo>(this))
    return fixed->isKnownEmpty();
  return false;
}

/// Copy a value from one object to a new object, directly taking
/// responsibility for anything it might have.  This is like C++
/// move-initialization, except the old object will not be destroyed.
void FixedTypeInfo::initializeWithTake(IRGenFunction &IGF,
                                       Address destAddr,
                                       Address srcAddr) const {
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
  IGF.emitMemCpy(destAddr, srcAddr, getFixedSize());
}

/// Copy a value from one object to a new object.  This is just the
/// default implementation.
void FixedTypeInfo::initializeWithCopy(IRGenFunction &IGF,
                                       Address destAddr,
                                       Address srcAddr) const {
  // Use memcpy if that's legal.
  if (isPOD(ResilienceScope::Local)) {
    return initializeWithTake(IGF, destAddr, srcAddr);
  }

  // Otherwise explode and re-implode.
  Explosion copy(ExplosionKind::Maximal);
  load(IGF, srcAddr, copy);
  initialize(IGF, copy, destAddr);
}

static llvm::Constant *asSizeConstant(IRGenModule &IGM, Size size) {
  return llvm::ConstantInt::get(IGM.SizeTy, size.getValue());
}

/// Return the size and alignment of this type.
std::pair<llvm::Value*,llvm::Value*>
FixedTypeInfo::getSizeAndAlignmentMask(IRGenFunction &IGF) const {
  return std::make_pair(FixedTypeInfo::getSize(IGF),
                        FixedTypeInfo::getAlignmentMask(IGF));
}

llvm::Value *FixedTypeInfo::getSize(IRGenFunction &IGF) const {
  return FixedTypeInfo::getStaticSize(IGF.IGM);
}
llvm::Constant *FixedTypeInfo::getStaticSize(IRGenModule &IGM) const {
  return asSizeConstant(IGM, getFixedSize());
}

llvm::Value *FixedTypeInfo::getAlignmentMask(IRGenFunction &IGF) const {
  return FixedTypeInfo::getStaticAlignmentMask(IGF.IGM);
}
llvm::Constant *FixedTypeInfo::getStaticAlignmentMask(IRGenModule &IGM) const {
  return asSizeConstant(IGM, Size(getFixedAlignment().getValue() - 1));
}

llvm::Value *FixedTypeInfo::getStride(IRGenFunction &IGF) const {
  return FixedTypeInfo::getStaticStride(IGF.IGM);
}
llvm::Constant *FixedTypeInfo::getStaticStride(IRGenModule &IGM) const {
  return asSizeConstant(IGM, getFixedStride());
}

namespace {
  /// A TypeInfo implementation for empty types.
  struct EmptyTypeInfo : ScalarTypeInfo<EmptyTypeInfo, FixedTypeInfo> {
    EmptyTypeInfo(llvm::Type *ty)
      : ScalarTypeInfo(ty, Size(0), Alignment(1), IsPOD) {}
    unsigned getExplosionSize(ExplosionKind kind) const { return 0; }
    void getSchema(ExplosionSchema &schema) const {}
    void load(IRGenFunction &IGF, Address addr, Explosion &e) const {}
    void loadAsTake(IRGenFunction &IGF, Address addr, Explosion &e) const {}
    void assign(IRGenFunction &IGF, Explosion &e, Address addr) const {}
    void initialize(IRGenFunction &IGF, Explosion &e, Address addr) const {}
    void copy(IRGenFunction &IGF, Explosion &src, Explosion &dest) const {}
    void retain(IRGenFunction &IGF, Explosion &e) const {}
    void release(IRGenFunction &IGF, Explosion &e) const {}
    void destroy(IRGenFunction &IGF, Address addr) const {}
  };

  /// A TypeInfo implementation for types represented as a single
  /// scalar type.
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

/// Add a temporary forward declaration for a type.  This will live
/// only until a proper mapping is added.
void TypeConverter::addForwardDecl(TypeBase *key, llvm::Type *type) {
  assert(key->isCanonical());
  assert(!Types.Cache.count(key) && "entry already exists for type!");
  Types.Cache.insert(std::make_pair(key, type));
}

const TypeInfo &IRGenModule::getWitnessTablePtrTypeInfo() {
  return Types.getWitnessTablePtrTypeInfo();
}

const TypeInfo &TypeConverter::getWitnessTablePtrTypeInfo() {
  if (WitnessTablePtrTI) return *WitnessTablePtrTI;
  WitnessTablePtrTI = createPrimitive(IGM.WitnessTablePtrTy,
                                      IGM.getPointerSize(),
                                      IGM.getPointerAlignment());
  WitnessTablePtrTI->NextConverted = FirstType;
  FirstType = WitnessTablePtrTI;
  return *WitnessTablePtrTI;
}

const TypeInfo &IRGenModule::getTypeMetadataPtrTypeInfo() {
  return Types.getTypeMetadataPtrTypeInfo();
}

const TypeInfo &TypeConverter::getTypeMetadataPtrTypeInfo() {
  if (TypeMetadataPtrTI) return *TypeMetadataPtrTI;
  TypeMetadataPtrTI = createPrimitive(IGM.TypeMetadataPtrTy,
                                      IGM.getPointerSize(),
                                      IGM.getPointerAlignment());
  TypeMetadataPtrTI->NextConverted = FirstType;
  FirstType = TypeMetadataPtrTI;
  return *TypeMetadataPtrTI;
}

/// Get the fragile type information for the given type.
const TypeInfo &IRGenFunction::getFragileTypeInfo(Type T) {
  return IGM.getFragileTypeInfo(T->getCanonicalType());
}

/// Get the fragile type information for the given type.
const TypeInfo &IRGenFunction::getFragileTypeInfo(CanType T) {
  return IGM.getFragileTypeInfo(T);
}

/// Get the fragile type information for the given type.
const TypeInfo &IRGenFunction::getFragileTypeInfo(SILType T) {
  return IGM.getFragileTypeInfo(T);
}

/// Get a pointer to the storage type for the given type.  Note that,
/// unlike fetching the type info and asking it for the storage type,
/// this operation will succeed for forward-declarations.
llvm::PointerType *IRGenModule::getStoragePointerType(CanType T) {
  return getStorageType(T)->getPointerTo();
}

/// Get the storage type for the given type.  Note that, unlike
/// fetching the type info and asking it for the storage type, this
/// operation will succeed for forward-declarations.
llvm::Type *IRGenModule::getStorageType(CanType T) {
  auto entry = Types.getTypeEntry(T);
  if (auto ti = entry.dyn_cast<const TypeInfo*>()) {
    return ti->getStorageType();
  } else {
    return entry.get<llvm::Type*>();
  }
}

/// Get the fragile type information for the given type.
const TypeInfo &IRGenModule::getFragileTypeInfo(Type T) {
  return getFragileTypeInfo(T->getCanonicalType());
}

/// Get the fragile type information for the given type.
const TypeInfo &IRGenModule::getFragileTypeInfo(CanType T) {
  return Types.getCompleteTypeInfo(T);
}

/// Get the fragile type information for the given type.
const TypeInfo &IRGenModule::getFragileTypeInfo(SILType T) {
  return Types.getCompleteTypeInfo(T.getSwiftRValueType());
}

/// 
const TypeInfo &TypeConverter::getCompleteTypeInfo(CanType T) {
  auto entry = getTypeEntry(T);
  assert(entry.is<const TypeInfo*>() && "getting TypeInfo recursively!");
  auto &ti = *entry.get<const TypeInfo*>();
  assert(ti.isComplete());
  return ti;
}

TypeCacheEntry TypeConverter::getTypeEntry(CanType canonicalTy) {
  auto it = Types.Cache.find(canonicalTy.getPointer());
  if (it != Types.Cache.end())
    return it->second;

  // Convert the type.
  TypeCacheEntry convertedEntry = convertType(canonicalTy);
  auto convertedTI = convertedEntry.dyn_cast<const TypeInfo*>();

  // If that gives us a forward declaration (which can happen with
  // bound generic types), don't propagate that into the cache here,
  // because we won't know how to clear it later.
  if (!convertedTI) return convertedEntry;

  auto &entry = Types.Cache[canonicalTy.getPointer()];
  assert(entry == TypeCacheEntry() ||
         (entry.is<llvm::Type*>() &&
          entry.get<llvm::Type*>() == convertedTI->getStorageType()));
  entry = convertedTI;

  // If the type info hasn't been added to the list of types, do so.
  if (!convertedTI->NextConverted) {
    convertedTI->NextConverted = FirstType;
    FirstType = convertedTI;
  }

  return convertedTI;
}

/// A convenience for grabbing the TypeInfo for a class declaration.
const TypeInfo &TypeConverter::getFragileTypeInfo(ClassDecl *theClass) {
  // If we have generic parameters, use the bound-generics conversion
  // routine.  This does an extra level of caching based on the common
  // class decl.
  TypeCacheEntry entry;
  if (theClass->getGenericParams()) {
    entry = convertBoundGenericType(theClass);

  // Otherwise, use the declared type.
  } else {
    entry = getTypeEntry(theClass->getDeclaredType()->getCanonicalType());
  }

  // This will always yield a TypeInfo because forward-declarations
  // are unnecessary when converting class types.
  return *entry.get<const TypeInfo*>();
}

TypeCacheEntry TypeConverter::convertType(CanType canTy) {
  PrettyStackTraceType stackTrace(IGM.Context, "converting", canTy);

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
  case TypeKind::DeducibleGenericParam:
    llvm_unreachable("deducible generic parameter");

  case TypeKind::UnboundGeneric:
    llvm_unreachable("unbound generic type");

  case TypeKind::MetaType:
    return convertMetaTypeType(cast<MetaTypeType>(ty));
  case TypeKind::Module:
    return convertModuleType(cast<ModuleType>(ty));
  case TypeKind::BuiltinRawPointer:
    return createPrimitive(IGM.Int8PtrTy, IGM.getPointerSize(),
                           IGM.getPointerAlignment());
  case TypeKind::BuiltinOpaquePointer:
    return createPrimitive(IGM.OpaquePtrTy, IGM.getPointerSize(),
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
  case TypeKind::BoundGenericClass:
  case TypeKind::BoundGenericOneOf:
  case TypeKind::BoundGenericStruct:
    return convertBoundGenericType(cast<BoundGenericType>(ty)->getDecl());
  case TypeKind::LValue:
    return convertLValueType(cast<LValueType>(ty));
  case TypeKind::Tuple:
    return convertTupleType(cast<TupleType>(ty));
  case TypeKind::OneOf:
    return convertOneOfType(cast<OneOfType>(ty)->getDecl());
  case TypeKind::Struct:
    return convertStructType(cast<StructType>(ty)->getDecl());
  case TypeKind::Class:
    return convertClassType(cast<ClassType>(ty)->getDecl());
  case TypeKind::Function:
  case TypeKind::PolymorphicFunction:
    return convertFunctionType(cast<AnyFunctionType>(ty));
  case TypeKind::Array:
    llvm_unreachable("array types should be lowered by SILGen");
  case TypeKind::Protocol:
    return convertProtocolType(cast<ProtocolType>(ty));
  case TypeKind::ProtocolComposition:
    return convertProtocolCompositionType(cast<ProtocolCompositionType>(ty));
  }
  llvm_unreachable("bad type kind");
}

/// Convert an l-value type.  For non-heap l-values, this is always
/// just a bare pointer.  For heap l-values, this is a pair of a bare
/// pointer with an object reference.
const TypeInfo *TypeConverter::convertLValueType(LValueType *T) {
  auto referenceType = IGM.getStoragePointerType(CanType(T->getObjectType()));
  
  // If it's not a heap l-value, just use the reference type as a
  // primitive pointer.
  return createPrimitive(referenceType, IGM.getPointerSize(),
                         IGM.getPointerAlignment());
}

static void overwriteForwardDecl(llvm::DenseMap<TypeBase*, TypeCacheEntry> &cache,
                                 TypeBase *key, const TypeInfo *result) {
  assert(cache.count(key) && "no forward declaration?");
  assert(cache[key].is<llvm::Type*>() && "overwriting real entry!");
  cache[key] = result;
}

TypeCacheEntry TypeConverter::convertBoundGenericType(NominalTypeDecl *decl) {
  assert(decl->getGenericParams());

  // Look to see if we've already emitted this type under a different
  // set of arguments.  We cache under the unbound type, which should
  // never collide with anything.
  //
  // FIXME: this isn't really inherently good; we might want to use
  // different type implementations for different applications.
  assert(decl->getDeclaredType()->isCanonical());
  assert(decl->getDeclaredType()->is<UnboundGenericType>());
  TypeBase *key = decl->getDeclaredType().getPointer();
  auto entry = Types.Cache.find(key);
  if (entry != Types.Cache.end())
    return entry->second;

  switch (decl->getKind()) {
#define NOMINAL_TYPE_DECL(ID, PARENT)
#define DECL(ID, PARENT) \
  case DeclKind::ID:
#include "swift/AST/DeclNodes.def"
    llvm_unreachable("not a nominal type declaration");

  case DeclKind::Protocol:
    llvm_unreachable("protocol types don't take generic parameters");

  case DeclKind::Class: {
    auto result = convertClassType(cast<ClassDecl>(decl));
    assert(!Types.Cache.count(key));
    Types.Cache.insert(std::make_pair(key, result));
    return result;
  }

  case DeclKind::OneOf: {
    auto result = convertOneOfType(cast<OneOfDecl>(decl));
    overwriteForwardDecl(Types.Cache, key, result);
    return result;
  }

  case DeclKind::Struct: {
    auto result = convertStructType(cast<StructDecl>(decl));
    overwriteForwardDecl(Types.Cache, key, result);
    return result;
  }
  }
  llvm_unreachable("bad declaration kind");
}

const TypeInfo *TypeConverter::convertModuleType(ModuleType *T) {
  return new EmptyTypeInfo(IGM.Int8Ty);
}

const TypeInfo *TypeConverter::convertMetaTypeType(MetaTypeType *T) {
  // Certain metatypes have trivial representation, and we only
  // actually need to materialize them when converting to a more
  // generic representation.
  if (IGM.hasTrivialMetatype(CanType(T->getInstanceType())))
    return new EmptyTypeInfo(IGM.Int8Ty);

  return &getTypeMetadataPtrTypeInfo();
}

/// createNominalType - Create a new nominal type.
llvm::StructType *IRGenModule::createNominalType(TypeDecl *decl) {
  llvm::SmallString<32> typeName;
  if (decl->getDeclContext()->isLocalContext()) {
    typeName = decl->getName().str();
    typeName.append(".local");
  } else {
    auto type = decl->getDeclaredType()->getCanonicalType();
    LinkEntity::forTypeMangling(type).mangle(typeName);
  }
  return llvm::StructType::create(getLLVMContext(), typeName.str());
}

/// createNominalType - Create a new nominal LLVM type for the given
/// protocol composition type.  Protocol composition types are
/// structural in the swift type system, but LLVM's type system
/// doesn't really care about this distinction, and it's nice to
/// distinguish different cases.
llvm::StructType *
IRGenModule::createNominalType(ProtocolCompositionType *type) {
  llvm::SmallString<32> typeName;

  SmallVector<ProtocolDecl *, 4> protocols;
  type->isExistentialType(protocols);

  typeName.append("protocol<");
  for (unsigned i = 0, e = protocols.size(); i != e; ++i) {
    if (i) typeName.push_back(',');
    LinkEntity::forNonFunction(protocols[i]).mangle(typeName);
  }
  typeName.push_back('>');
  return llvm::StructType::create(getLLVMContext(), typeName.str());
}

/// Compute the explosion schema for the given type.
ExplosionSchema IRGenModule::getSchema(CanType type, ExplosionKind kind) {
  ExplosionSchema schema(kind);
  getSchema(type, schema);
  return schema;
}

/// Compute the explosion schema for the given type.
void IRGenModule::getSchema(CanType type, ExplosionSchema &schema) {
  // As an optimization, avoid actually building a TypeInfo for any
  // obvious TupleTypes.  This assumes that a TupleType's explosion
  // schema is always the concatenation of its component's schemas.
  if (TupleType *tuple = dyn_cast<TupleType>(type)) {
    for (const TupleTypeElt &field : tuple->getFields())
      getSchema(CanType(field.getType()), schema);
    return;
  }

  // Okay, that didn't work;  just do the general thing.
  getFragileTypeInfo(type).getSchema(schema);
}

/// Compute the explosion schema for the given type.
unsigned IRGenModule::getExplosionSize(CanType type, ExplosionKind kind) {
  // As an optimization, avoid actually building a TypeInfo for any
  // obvious TupleTypes.  This assumes that a TupleType's explosion
  // schema is always the concatenation of its component's schemas.
  if (TupleType *tuple = dyn_cast<TupleType>(type)) {
    unsigned count = 0;
    for (const TupleTypeElt &field : tuple->getFields())
      count += getExplosionSize(CanType(field.getType()), kind);
    return count;
  }

  // Okay, that didn't work;  just do the general thing.
  return getFragileTypeInfo(type).getExplosionSize(kind);
}

/// Determine whether this type is a single value that is passed
/// indirectly at the given level.
llvm::PointerType *IRGenModule::isSingleIndirectValue(CanType type,
                                                      ExplosionKind kind) {
  if (auto *archetype = dyn_cast<ArchetypeType>(type)) {
    if (!archetype->isClassBounded())
      return OpaquePtrTy;
  }

  ExplosionSchema schema(kind);
  getSchema(type, schema);
  if (schema.size() == 1 && schema.begin()->isAggregate())
    return schema.begin()->getAggregateType()->getPointerTo(0);
  return nullptr;
}

/// Determine whether this type requires an indirect result.
llvm::PointerType *IRGenModule::requiresIndirectResult(CanType type,
                                                       ExplosionKind kind) {
  auto &ti = getFragileTypeInfo(type);
  ExplosionSchema schema = ti.getSchema(kind);
  if (schema.requiresIndirectResult())
    return ti.getStorageType()->getPointerTo();
  return nullptr;
}

/// Determine whether this type is known to be POD.
bool IRGenModule::isPOD(CanType type, ResilienceScope scope) {
  if (isa<ArchetypeType>(type)) return false;
  if (isa<ClassType>(type)) return false;
  if (isa<BoundGenericClassType>(type)) return false;
  if (auto tuple = dyn_cast<TupleType>(type)) {
    for (auto &elt : tuple->getFields())
      if (!isPOD(CanType(elt.getType()), scope))
        return false;
    return true;
  }
  return getFragileTypeInfo(type).isPOD(scope);
}


namespace {
  struct ClassifyTypeSize : irgen::TypeVisitor<ClassifyTypeSize, ObjectSize> {
    IRGenModule &IGM;
    ResilienceScope Scope;
    ClassifyTypeSize(IRGenModule &IGM, ResilienceScope scope)
      : IGM(IGM), Scope(scope) {}

#define ALWAYS(KIND, RESULT) \
    ObjectSize visit##KIND##Type(KIND##Type *t) { return ObjectSize::RESULT; }

    ALWAYS(Builtin, Fixed)
    ALWAYS(AnyFunction, Fixed)
    ALWAYS(Class, Fixed)
    ALWAYS(BoundGenericClass, Fixed)
    ALWAYS(Archetype, Dependent)
    ALWAYS(Protocol, Dependent)
    ALWAYS(ProtocolComposition, Dependent)
    ALWAYS(LValue, Dependent)
#undef ALWAYS

    ObjectSize visitTupleType(TupleType *tuple) {
      ObjectSize result = ObjectSize::Fixed;
      for (auto &field : tuple->getFields()) {
        result = std::max(result, visit(CanType(field.getType())));
      }
      return result;
    }

    ObjectSize visitArrayType(ArrayType *array) {
      return visit(CanType(array->getBaseType()));
    }

    ObjectSize visitType(TypeBase *type) {
      // FIXME: resilience!
      return ObjectSize::Fixed;
    }
  };
}

ObjectSize IRGenModule::classifyTypeSize(CanType type, ResilienceScope scope) {
  return ClassifyTypeSize(*this, scope).visit(type);
}
