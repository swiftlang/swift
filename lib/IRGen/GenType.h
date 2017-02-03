//===--- GenType.h - Auxiliary Interface for Type IR Generation -*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
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
#include "llvm/ADT/FoldingSet.h"
#include "llvm/ADT/ilist.h"
#include "llvm/ADT/ilist_node.h"
#include "IRGenModule.h"

namespace swift {
  class ArchetypeBuilder;
  class ArchetypeType;
  class CanType;
  class ClassDecl;
  class AnyFunctionType;
  class InOutType;
  class MetatypeType;
  class ModuleType;
  class NominalTypeDecl;
  class EnumDecl;
  class ProtocolCompositionType;
  class ProtocolDecl;
  class ProtocolType;
  class SILFunctionType;
  class StructDecl;
  class TupleType;
  class TypeBase;
  class Type;
  class EnumDecl;
  class UnownedStorageType;
  class WeakStorageType;
  enum IsTake_t : bool;
  
namespace irgen {
  class Alignment;
  class ProtocolInfo;
  class Size;
  class FixedTypeInfo;
  class LoadableTypeInfo;
  class TypeInfo;
  class UnownedTypeInfo;
  class WeakTypeInfo;
  
/// Either a type or a forward-declaration.
typedef llvm::PointerUnion<const TypeInfo*, llvm::Type*> TypeCacheEntry;

/// The helper class for generating types.
class TypeConverter {
public:
  IRGenModule &IGM;
private:
  llvm::DenseMap<ProtocolDecl*, const ProtocolInfo*> Protocols;
  const TypeInfo *FirstType;
  
  const ProtocolInfo *FirstProtocol;
  const LoadableTypeInfo *NativeObjectTI = nullptr;
  const LoadableTypeInfo *UnknownObjectTI = nullptr;
  const LoadableTypeInfo *BridgeObjectTI = nullptr;
  const LoadableTypeInfo *RawPointerTI = nullptr;
  const LoadableTypeInfo *WitnessTablePtrTI = nullptr;
  const LoadableTypeInfo *TypeMetadataPtrTI = nullptr;
  const LoadableTypeInfo *ObjCClassPtrTI = nullptr;
  const LoadableTypeInfo *EmptyTI = nullptr;

  const TypeInfo *ResilientStructTI = nullptr;
  
  llvm::DenseMap<std::pair<unsigned, unsigned>, const LoadableTypeInfo *>
    OpaqueStorageTypes;

  const LoadableTypeInfo *NonFixedBoxTI = nullptr;
  const LoadableTypeInfo *EmptyBoxTI = nullptr;
  llvm::DenseMap<std::pair<unsigned, unsigned>, const LoadableTypeInfo *>
    PODBoxTI;
  const LoadableTypeInfo *SwiftRetainablePointerBoxTI = nullptr,
                         *UnknownRetainablePointerBoxTI = nullptr;

  const LoadableTypeInfo *createPrimitive(llvm::Type *T,
                                          Size size, Alignment align);
  const LoadableTypeInfo *createPrimitiveForAlignedPointer(llvm::PointerType *T,
                                                   Size size, Alignment align,
                                                   Alignment pointerAlignment);
  const FixedTypeInfo *createImmovable(llvm::Type *T,
                                       Size size, Alignment align);

  void addForwardDecl(TypeBase *key, llvm::Type *type);

  TypeCacheEntry convertType(CanType T);
  TypeCacheEntry convertAnyNominalType(CanType T, NominalTypeDecl *D);
  const TypeInfo *convertTupleType(TupleType *T);
  const TypeInfo *convertClassType(CanType type, ClassDecl *D);
  const TypeInfo *convertEnumType(TypeBase *key, CanType type, EnumDecl *D);
  const TypeInfo *convertStructType(TypeBase *key, CanType type, StructDecl *D);
  const TypeInfo *convertFunctionType(SILFunctionType *T);
  const TypeInfo *convertBlockStorageType(SILBlockStorageType *T);
  const TypeInfo *convertBoxType(SILBoxType *T);
  const TypeInfo *convertArchetypeType(ArchetypeType *T);
  const TypeInfo *convertInOutType(InOutType *T);
  const TypeInfo *convertExistentialMetatypeType(ExistentialMetatypeType *T);
  const TypeInfo *convertMetatypeType(MetatypeType *T);
  const TypeInfo *convertModuleType(ModuleType *T);
  const TypeInfo *convertProtocolType(ProtocolType *T);
  const TypeInfo *convertProtocolCompositionType(ProtocolCompositionType *T);
  const LoadableTypeInfo *convertBuiltinNativeObject();
  const LoadableTypeInfo *convertBuiltinUnknownObject();
  const LoadableTypeInfo *convertBuiltinBridgeObject();
  const TypeInfo *convertResilientStruct();
  const TypeInfo *convertUnmanagedStorageType(UnmanagedStorageType *T);
  const TypeInfo *convertUnownedStorageType(UnownedStorageType *T);
  const TypeInfo *convertWeakStorageType(WeakStorageType *T);
  
public:
  TypeConverter(IRGenModule &IGM);
  ~TypeConverter();

  TypeCacheEntry getTypeEntry(CanType type);
  const TypeInfo &getCompleteTypeInfo(CanType type);
  const TypeInfo *tryGetCompleteTypeInfo(CanType type);
  const LoadableTypeInfo &getNativeObjectTypeInfo();
  const LoadableTypeInfo &getUnknownObjectTypeInfo();
  const LoadableTypeInfo &getBridgeObjectTypeInfo();
  const LoadableTypeInfo &getRawPointerTypeInfo();
  const LoadableTypeInfo &getTypeMetadataPtrTypeInfo();
  const LoadableTypeInfo &getObjCClassPtrTypeInfo();
  const LoadableTypeInfo &getWitnessTablePtrTypeInfo();
  const LoadableTypeInfo &getEmptyTypeInfo();
  const TypeInfo &getResilientStructTypeInfo();
  const ProtocolInfo &getProtocolInfo(ProtocolDecl *P);
  const LoadableTypeInfo &getOpaqueStorageTypeInfo(Size storageSize,
                                                   Alignment storageAlign);
  const LoadableTypeInfo &getMetatypeTypeInfo(MetatypeRepresentation representation);

  const WeakTypeInfo *createWeakStorageType(llvm::Type *valueType,
                                            ReferenceCounting style);
  const TypeInfo *createUnownedStorageType(llvm::Type *valueType,
                                           ReferenceCounting style);
  const LoadableTypeInfo *createUnmanagedStorageType(llvm::Type *valueType);

  /// Enter a generic context for lowering the parameters of a generic function
  /// type.
  void pushGenericContext(CanGenericSignature signature);
  
  /// Exit a generic context.
  void popGenericContext(CanGenericSignature signature);

  /// Retrieve the generic environment for the current generic context.
  ///
  /// Fails if there is no generic context.
  GenericEnvironment *getGenericEnvironment();

private:
  // Debugging aids.
#ifndef NDEBUG
  bool isExemplarArchetype(ArchetypeType *arch) const;

  LLVM_ATTRIBUTE_DEPRECATED(
    CanType getTypeThatLoweredTo(llvm::Type *t) const LLVM_ATTRIBUTE_USED,
    "only for use within the debugger");
#endif

  ArchetypeType *getExemplarArchetype(ArchetypeType *t);
  CanType getExemplarType(CanType t);
  
  class Types_t {
    llvm::DenseMap<TypeBase*, TypeCacheEntry> IndependentCache;
    llvm::DenseMap<TypeBase*, TypeCacheEntry> DependentCache;
    llvm::DenseMap<TypeBase*, TypeCacheEntry> &getCacheFor(TypeBase *t);

    friend TypeCacheEntry TypeConverter::getTypeEntry(CanType T);
    friend TypeCacheEntry TypeConverter::convertAnyNominalType(CanType Type,
                                                           NominalTypeDecl *D);
    friend void TypeConverter::addForwardDecl(TypeBase*, llvm::Type*);
    friend ArchetypeType *TypeConverter::getExemplarArchetype(ArchetypeType *t);
    friend void TypeConverter::popGenericContext(CanGenericSignature signature);
    
#ifndef NDEBUG
    friend CanType TypeConverter::getTypeThatLoweredTo(llvm::Type *t) const;
    friend bool TypeConverter::isExemplarArchetype(ArchetypeType *arch) const;
#endif
  };
  Types_t Types;
};

/// An RAII interface for entering a generic context for type conversion in
/// a scope.
class GenericContextScope {
  TypeConverter &TC;
  CanGenericSignature sig;
public:
  GenericContextScope(TypeConverter &TC, CanGenericSignature sig)
    : TC(TC), sig(sig)
  {
    TC.pushGenericContext(sig);
  }
  
  GenericContextScope(IRGenModule &IGM, CanGenericSignature sig)
    : GenericContextScope(IGM.Types, sig)
  {}
  
  ~GenericContextScope() {
    TC.popGenericContext(sig);
  }
};

/// Generate code to verify that static type assumptions agree with the runtime.
void emitTypeLayoutVerifier(IRGenFunction &IGF,
                            ArrayRef<CanType> formalTypes);

/// If a type is visibly a singleton aggregate (a tuple with one element, a
/// struct with one field, or an enum with a single payload case), return the
/// type of its field, which it is guaranteed to have identical layout to.
SILType getSingletonAggregateFieldType(IRGenModule &IGM,
                                       SILType t,
                                       ResilienceExpansion expansion);

} // end namespace irgen
} // end namespace swift

#endif
