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
#include "IRGenFunction.h"

namespace swift {
  class GenericSignatureBuilder;
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
  
/// Either a type or a forward-declaration.
using TypeCacheEntry = llvm::PointerUnion<const TypeInfo *, llvm::Type *>;

/// The helper class for generating types.
class TypeConverter {
public:
  IRGenModule &IGM;
private:
  bool CompletelyFragile = false;

  llvm::DenseMap<ProtocolDecl*, const ProtocolInfo*> Protocols;
  const TypeInfo *FirstType;
  
  const ProtocolInfo *FirstProtocol;
  const LoadableTypeInfo *NativeObjectTI = nullptr;
  const LoadableTypeInfo *UnknownObjectTI = nullptr;
  const LoadableTypeInfo *BridgeObjectTI = nullptr;
  const LoadableTypeInfo *RawPointerTI = nullptr;
  const LoadableTypeInfo *WitnessTablePtrTI = nullptr;
  const TypeInfo *TypeMetadataPtrTI = nullptr;
  const TypeInfo *ObjCClassPtrTI = nullptr;
  const LoadableTypeInfo *EmptyTI = nullptr;

  const TypeInfo *AccessibleResilientStructTI = nullptr;
  const TypeInfo *InaccessibleResilientStructTI = nullptr;
  
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
  const TypeInfo *convertResilientStruct(IsABIAccessible_t abiAccessible);
#define REF_STORAGE(Name, ...) \
  const TypeInfo *convert##Name##StorageType(Name##StorageType *T);
#include "swift/AST/ReferenceStorage.def"
  
public:
  TypeConverter(IRGenModule &IGM);
  ~TypeConverter();

  bool isCompletelyFragile() const {
    return CompletelyFragile;
  }

  TypeCacheEntry getTypeEntry(CanType type);
  const TypeInfo &getCompleteTypeInfo(CanType type);
  const LoadableTypeInfo &getNativeObjectTypeInfo();
  const LoadableTypeInfo &getUnknownObjectTypeInfo();
  const LoadableTypeInfo &getBridgeObjectTypeInfo();
  const LoadableTypeInfo &getRawPointerTypeInfo();
  const TypeInfo &getTypeMetadataPtrTypeInfo();
  const TypeInfo &getObjCClassPtrTypeInfo();
  const LoadableTypeInfo &getWitnessTablePtrTypeInfo();
  const LoadableTypeInfo &getEmptyTypeInfo();
  const TypeInfo &getResilientStructTypeInfo(IsABIAccessible_t abiAccessible);
  const ProtocolInfo &getProtocolInfo(ProtocolDecl *P);
  const LoadableTypeInfo &getOpaqueStorageTypeInfo(Size storageSize,
                                                   Alignment storageAlign);
  const TypeInfo &getMetatypeTypeInfo(MetatypeRepresentation representation);

#define REF_STORAGE(Name, ...) \
  const TypeInfo *create##Name##StorageType(llvm::Type *valueType, \
                                            ReferenceCounting style, \
                                            bool isOptional);
#include "swift/AST/ReferenceStorage.def"

  /// Enter a generic context for lowering the parameters of a generic function
  /// type.
  void pushGenericContext(CanGenericSignature signature);
  
  /// Exit a generic context.
  void popGenericContext(CanGenericSignature signature);

  /// Enter a scope where all types are lowered bypassing resilience.
  void pushCompletelyFragile();

  /// Exit a completely fragile scope.
  void popCompletelyFragile();

  /// Retrieve the generic environment for the current generic context.
  ///
  /// Fails if there is no generic context.
  GenericEnvironment *getGenericEnvironment();

private:
  // Debugging aids.
#ifndef NDEBUG
  bool isExemplarArchetype(ArchetypeType *arch) const;
#endif

  ArchetypeType *getExemplarArchetype(ArchetypeType *t);
  CanType getExemplarType(CanType t);
  
  class Types_t {
    llvm::DenseMap<TypeBase*, TypeCacheEntry> IndependentCache;
    llvm::DenseMap<TypeBase*, TypeCacheEntry> DependentCache;
    llvm::DenseMap<TypeBase*, TypeCacheEntry> FragileIndependentCache;
    llvm::DenseMap<TypeBase*, TypeCacheEntry> FragileDependentCache;

  public:
    llvm::DenseMap<TypeBase*, TypeCacheEntry> &getCacheFor(bool isDependent,
                                                           bool completelyFragile);
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

/// An RAII interface for forcing types to be lowered bypassing resilience.
class CompletelyFragileScope {
  bool State;
  TypeConverter &TC;
public:
  explicit CompletelyFragileScope(TypeConverter &TC) : TC(TC) {
    State = TC.isCompletelyFragile();
    if (!State)
      TC.pushCompletelyFragile();
  }

  CompletelyFragileScope(IRGenModule &IGM)
    : CompletelyFragileScope(IGM.Types) {}

  ~CompletelyFragileScope() {
    if (!State)
      TC.popCompletelyFragile();
  }
};

/// If a type is visibly a singleton aggregate (a tuple with one element, a
/// struct with one field, or an enum with a single payload case), return the
/// type of its field, which it is guaranteed to have identical layout to.
SILType getSingletonAggregateFieldType(IRGenModule &IGM,
                                       SILType t,
                                       ResilienceExpansion expansion);

/// An IRGenFunction interface for generating type layout verifiers.
class IRGenTypeVerifierFunction : public IRGenFunction {
private:
  llvm::Constant *VerifierFn;

  struct VerifierArgumentBuffers {
    Address runtimeBuf, staticBuf;
  };
  llvm::DenseMap<llvm::Type *, VerifierArgumentBuffers> VerifierArgBufs;

public:
  IRGenTypeVerifierFunction(IRGenModule &IGM, llvm::Function *f);
  
  void emit(ArrayRef<CanType> typesToVerify);
  
  /// Call a runtime function that verifies that the two LLVM values are
  /// equivalent, logging a detailed error if they differ.
  void verifyValues(llvm::Value *typeMetadata,
                    llvm::Value *runtimeValue,
                    llvm::Value *compilerValue,
                    const llvm::Twine &description);
  
  /// Call a runtime function that verifies that the contents of the two
  /// memory buffers are equivalent, logging a detailed error if they differ.
  void verifyBuffers(llvm::Value *typeMetadata,
                     Address runtimeValue,
                     Address compilerValue,
                     Size size,
                     const llvm::Twine &description);
};
  
} // end namespace irgen
} // end namespace swift

#endif
