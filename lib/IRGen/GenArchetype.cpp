//===--- GenArchetype.cpp - Swift IR Generation for Archetype Types -------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  This file implements IR generation for archetype types in Swift.
//
//===----------------------------------------------------------------------===//

#include "GenArchetype.h"

#include "swift/AST/ASTContext.h"
#include "swift/AST/Types.h"
#include "swift/AST/Decl.h"
#include "swift/AST/IRGenOptions.h"
#include "swift/SIL/SILValue.h"
#include "swift/SIL/TypeLowering.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Module.h"

#include "EnumPayload.h"
#include "Explosion.h"
#include "FixedTypeInfo.h"
#include "GenClass.h"
#include "GenHeap.h"
#include "GenMeta.h"
#include "GenOpaque.h"
#include "GenPoly.h"
#include "GenProto.h"
#include "GenType.h"
#include "HeapTypeInfo.h"
#include "IRGenDebugInfo.h"
#include "IRGenFunction.h"
#include "IRGenModule.h"
#include "Linking.h"
#include "ProtocolInfo.h"
#include "ResilientTypeInfo.h"
#include "TypeInfo.h"
#include "WeakTypeInfo.h"

using namespace swift;
using namespace irgen;

static llvm::Value *emitArchetypeTypeMetadataRef(IRGenFunction &IGF,
                                                 CanArchetypeType archetype) {
  return IGF.getLocalTypeData(archetype, LocalTypeData::forMetatype());
}

namespace {

/// Common type implementation details for all archetypes.
class ArchetypeTypeInfoBase {
protected:
  unsigned NumStoredProtocols;
  ProtocolEntry *StoredProtocolsBuffer;

  ArchetypeTypeInfoBase(void *protocolsBuffer,
                        ArrayRef<ProtocolEntry> protocols)
    : NumStoredProtocols(protocols.size()),
      StoredProtocolsBuffer(reinterpret_cast<ProtocolEntry*>(protocolsBuffer))
  {
    for (unsigned i = 0, e = protocols.size(); i != e; ++i) {
      ::new (&StoredProtocolsBuffer[i]) ProtocolEntry(protocols[i]);
    }
  }

public:
  unsigned getNumStoredProtocols() const {
    return NumStoredProtocols;
  }

  ArrayRef<ProtocolEntry> getStoredProtocols() const {
    return llvm::makeArrayRef(StoredProtocolsBuffer, getNumStoredProtocols());
  }

  /// Return the witness table that's been set for this type.
  llvm::Value *getWitnessTable(IRGenFunction &IGF,
                               CanArchetypeType archetype,
                               unsigned which) const {
    assert(which < getNumStoredProtocols());
    return IGF.getLocalTypeData(archetype,
                          LocalTypeData::forArchetypeProtocolWitness(which));
  }
};

/// A type implementation for an ArchetypeType, otherwise known as a
/// type variable: for example, Self in a protocol declaration, or T
/// in a generic declaration like foo<T>(x : T) -> T.  The critical
/// thing here is that performing an operation involving archetypes
/// is dependent on the witness binding we can see.
class OpaqueArchetypeTypeInfo
  : public ResilientTypeInfo<OpaqueArchetypeTypeInfo>,
    public ArchetypeTypeInfoBase
{
  OpaqueArchetypeTypeInfo(llvm::Type *type,
                          ArrayRef<ProtocolEntry> protocols)
    : ResilientTypeInfo(type),
      ArchetypeTypeInfoBase(this + 1, protocols)
  {}

public:
  static const OpaqueArchetypeTypeInfo *create(llvm::Type *type,
                                         ArrayRef<ProtocolEntry> protocols) {
    void *buffer = operator new(sizeof(OpaqueArchetypeTypeInfo)
                                + protocols.size() * sizeof(ProtocolEntry));
    return ::new (buffer) OpaqueArchetypeTypeInfo(type, protocols);
  }
};

/// A type implementation for a class archetype, that is, an archetype
/// bounded by a class protocol constraint. These archetypes can be
/// represented by a refcounted pointer instead of an opaque value buffer.
/// If ObjC interop is disabled, we can use Swift refcounting entry
/// points, otherwise we have to use the unknown ones.
class ClassArchetypeTypeInfo
  : public HeapTypeInfo<ClassArchetypeTypeInfo>,
    public ArchetypeTypeInfoBase
{
  ReferenceCounting RefCount;

  ClassArchetypeTypeInfo(llvm::PointerType *storageType,
                         Size size, const SpareBitVector &spareBits,
                         Alignment align,
                         ArrayRef<ProtocolEntry> protocols,
                         ReferenceCounting refCount)
    : HeapTypeInfo(storageType, size, spareBits, align),
      ArchetypeTypeInfoBase(this + 1, protocols),
      RefCount(refCount)
  {}

public:
  static const ClassArchetypeTypeInfo *create(llvm::PointerType *storageType,
                                         Size size, const SpareBitVector &spareBits,
                                         Alignment align,
                                         ArrayRef<ProtocolEntry> protocols,
                                         ReferenceCounting refCount) {
    void *buffer = operator new(sizeof(ClassArchetypeTypeInfo)
                                  + protocols.size() * sizeof(ProtocolEntry));
    return ::new (buffer)
      ClassArchetypeTypeInfo(storageType, size, spareBits, align,
                             protocols, refCount);
  }

  ReferenceCounting getReferenceCounting() const {
    return RefCount;
  }
};

} // end anonymous namespace

/// Return the ArchetypeTypeInfoBase information from the TypeInfo for any
/// archetype.
static const ArchetypeTypeInfoBase &
getArchetypeInfo(IRGenFunction &IGF, CanArchetypeType t, const TypeInfo &ti) {
  if (t->requiresClass())
    return ti.as<ClassArchetypeTypeInfo>();
  return ti.as<OpaqueArchetypeTypeInfo>();
}

/// Emit a single protocol witness table reference.
llvm::Value *irgen::emitWitnessTableRef(IRGenFunction &IGF,
                                        CanArchetypeType archetype,
                                        ProtocolDecl *proto) {
  assert(Lowering::TypeConverter::protocolRequiresWitnessTable(proto) &&
         "looking up witness table for protocol that doesn't have one");

  auto &archTI = getArchetypeInfo(IGF, archetype,
                                  IGF.getTypeInfoForLowered(archetype));
  auto wtable = emitImpliedWitnessTableRef(IGF, archTI.getStoredProtocols(),
                                           proto,
    [&](unsigned originIndex) -> llvm::Value* {
      return archTI.getWitnessTable(IGF, archetype, originIndex);
    });
  return wtable;
}

llvm::Value *irgen::emitAssociatedTypeMetadataRef(IRGenFunction &IGF,
                                                  CanArchetypeType origin,
                                               AssociatedTypeDecl *associate) {
  // Find the conformance of the origin to the associated type's protocol.
  llvm::Value *wtable = emitWitnessTableRef(IGF, origin,
                                            associate->getProtocol());

  // Find the origin's type metadata.
  llvm::Value *originMetadata = emitArchetypeTypeMetadataRef(IGF, origin);

  return emitAssociatedTypeMetadataRef(IGF, originMetadata, wtable, associate);
}

llvm::Value *
irgen::emitAssociatedTypeWitnessTableRef(IRGenFunction &IGF,
                                         CanArchetypeType origin,
                                         AssociatedTypeDecl *associate,
                                         llvm::Value *associateMetadata,
                                         ProtocolDecl *associateProtocol) {
  // Find the conformance of the origin to the associated type's protocol.
  llvm::Value *wtable = emitWitnessTableRef(IGF, origin,
                                            associate->getProtocol());

  // Find the origin's type metadata.
  llvm::Value *originMetadata = emitArchetypeTypeMetadataRef(IGF, origin);

  // FIXME: will this ever be an indirect requirement?
  return emitAssociatedTypeWitnessTableRef(IGF, originMetadata, wtable,
                                           associate, associateMetadata,
                                           associateProtocol);
}

const TypeInfo *TypeConverter::convertArchetypeType(ArchetypeType *archetype) {
  assert(isExemplarArchetype(archetype) && "lowering non-exemplary archetype");

  // Compute layouts for the protocols we ascribe to.
  SmallVector<ProtocolEntry, 4> protocols;
  for (auto protocol : archetype->getConformsTo()) {
    const ProtocolInfo &impl = IGM.getProtocolInfo(protocol);
    protocols.push_back(ProtocolEntry(protocol, impl));
  }

  // If the archetype is class-constrained, use a class pointer
  // representation.
  if (archetype->requiresClass()) {
    ReferenceCounting refcount;
    llvm::PointerType *reprTy;

    if (!IGM.ObjCInterop) {
      refcount = ReferenceCounting::Native;
      reprTy = IGM.RefCountedPtrTy;
    } else {
      refcount = ReferenceCounting::Unknown;
      reprTy = IGM.UnknownRefCountedPtrTy;
    }

    // If the archetype has a superclass constraint, it has at least the
    // retain semantics of its superclass, and it can be represented with
    // the supertype's pointer type.
    if (Type super = archetype->getSuperclass()) {
      ClassDecl *superClass = super->getClassOrBoundGenericClass();
      refcount = getReferenceCountingForClass(IGM, superClass);

      auto &superTI = IGM.getTypeInfoForUnlowered(super);
      reprTy = cast<llvm::PointerType>(superTI.StorageType);
    }

    // As a hack, assume class archetypes never have spare bits. There's a
    // corresponding hack in MultiPayloadEnumImplStrategy::completeEnumTypeLayout
    // to ignore spare bits of dependent-typed payloads.
    auto spareBits =
      SpareBitVector::getConstant(IGM.getPointerSize().getValueInBits(), false);

    return ClassArchetypeTypeInfo::create(reprTy,
                                      IGM.getPointerSize(),
                                      spareBits,
                                      IGM.getPointerAlignment(),
                                      protocols, refcount);
  }

  // Otherwise, for now, always use an opaque indirect type.
  llvm::Type *storageType = IGM.OpaquePtrTy->getElementType();
  return OpaqueArchetypeTypeInfo::create(storageType, protocols);
}

static void setMetadataRef(IRGenFunction &IGF,
                           ArchetypeType *archetype,
                           llvm::Value *metadata) {
  assert(metadata->getType() == IGF.IGM.TypeMetadataPtrTy);
  IGF.setUnscopedLocalTypeData(CanType(archetype),
                               LocalTypeData::forMetatype(),
                               metadata);

  // Create a shadow copy of the metadata in an alloca for the debug info.
  StringRef Name = metadata->getName();
  if (!IGF.IGM.Opts.Optimize) {
    auto Alloca = IGF.createAlloca(metadata->getType(),
                                   IGF.IGM.getPointerAlignment(), Name);
    IGF.Builder.CreateAlignedStore(metadata, Alloca.getAddress(),
                                   IGF.IGM.getPointerAlignment().getValue());
    metadata = Alloca.getAddress();
  }

  // Emit debug info for the metadata.
  if (IGF.IGM.DebugInfo)
    IGF.IGM.DebugInfo->emitTypeMetadata(IGF, metadata, Name);
}

static void setWitnessTable(IRGenFunction &IGF,
                            ArchetypeType *archetype,
                            unsigned protocolIndex,
                            llvm::Value *wtable) {
  assert(wtable->getType() == IGF.IGM.WitnessTablePtrTy);
  assert(protocolIndex < archetype->getConformsTo().size());
  IGF.setUnscopedLocalTypeData(CanType(archetype),
             LocalTypeData::forArchetypeProtocolWitness(protocolIndex), wtable);
}

/// Inform IRGenFunction that the given archetype has the given value
/// witness value within this scope.
void IRGenFunction::bindArchetype(ArchetypeType *archetype,
                                  llvm::Value *metadata,
                                  ArrayRef<llvm::Value*> wtables) {
  // Set the metadata pointer.
  bool setNames = !archetype->getOpenedExistentialType();
  if (setNames)
    metadata->setName(archetype->getFullName());
  setMetadataRef(*this, archetype, metadata);

  // Set the protocol witness tables.

  unsigned wtableI = 0;
  for (unsigned i = 0, e = archetype->getConformsTo().size(); i != e; ++i) {
    auto proto = archetype->getConformsTo()[i];
    if (!Lowering::TypeConverter::protocolRequiresWitnessTable(proto))
      continue;
    auto wtable = wtables[wtableI++];
    if (setNames) {
      wtable->setName(Twine(archetype->getFullName()) + "." +
                      proto->getName().str());
    }
    setWitnessTable(*this, archetype, i, wtable);
  }
  assert(wtableI == wtables.size());
}

llvm::Value *irgen::emitDynamicTypeOfOpaqueArchetype(IRGenFunction &IGF,
                                                     Address addr,
                                                     SILType type) {
  auto archetype = type.castTo<ArchetypeType>();

  // Acquire the archetype's static metadata.
  llvm::Value *metadata = emitArchetypeTypeMetadataRef(IGF, archetype);
  return IGF.Builder.CreateCall(IGF.IGM.getGetDynamicTypeFn(),
                                {addr.getAddress(), metadata});
}
