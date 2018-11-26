//===--- GenArchetype.cpp - Swift IR Generation for Archetype Types -------===//
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
//  This file implements IR generation for archetype types in Swift.
//
//===----------------------------------------------------------------------===//

#include "GenArchetype.h"

#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/Types.h"
#include "swift/IRGen/Linking.h"
#include "swift/SIL/SILValue.h"
#include "swift/SIL/TypeLowering.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/IR/Constant.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/raw_ostream.h"

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
#include "MetadataRequest.h"
#include "Outlining.h"
#include "ProtocolInfo.h"
#include "ResilientTypeInfo.h"
#include "TypeInfo.h"
#include "WeakTypeInfo.h"

using namespace swift;
using namespace irgen;

MetadataResponse
irgen::emitArchetypeTypeMetadataRef(IRGenFunction &IGF,
                                    CanArchetypeType archetype,
                                    DynamicMetadataRequest request) {
  // Check for an existing cache entry.
  if (auto response = IGF.tryGetLocalTypeMetadata(archetype, request))
    return response;

  // If that's not present, this must be an associated type.
  assert(!archetype->isPrimary() &&
         "type metadata for primary archetype was not bound in context");

  CanArchetypeType parent(archetype->getParent());
  AssociatedType association(archetype->getAssocType());

  MetadataResponse response =
    emitAssociatedTypeMetadataRef(IGF, parent, association, request);

  setTypeMetadataName(IGF.IGM, response.getMetadata(), archetype);

  IGF.setScopedLocalTypeMetadata(archetype, response);

  return response;
}

namespace {

/// A type implementation for an ArchetypeType, otherwise known as a
/// type variable: for example, Self in a protocol declaration, or T
/// in a generic declaration like foo<T>(x : T) -> T.  The critical
/// thing here is that performing an operation involving archetypes
/// is dependent on the witness binding we can see.
class OpaqueArchetypeTypeInfo
  : public ResilientTypeInfo<OpaqueArchetypeTypeInfo>
{
  OpaqueArchetypeTypeInfo(llvm::Type *type) : ResilientTypeInfo(type) {}

public:
  static const OpaqueArchetypeTypeInfo *create(llvm::Type *type) {
    return new OpaqueArchetypeTypeInfo(type);
  }

  void collectMetadataForOutlining(OutliningMetadataCollector &collector,
                                   SILType T) const override {
    // We'll need formal type metadata for this archetype.
    collector.collectTypeMetadataForLayout(T);
  }
};

/// A type implementation for a class archetype, that is, an archetype
/// bounded by a class protocol constraint. These archetypes can be
/// represented by a refcounted pointer instead of an opaque value buffer.
/// If ObjC interop is disabled, we can use Swift refcounting entry
/// points, otherwise we have to use the unknown ones.
class ClassArchetypeTypeInfo
  : public HeapTypeInfo<ClassArchetypeTypeInfo>
{
  ReferenceCounting RefCount;

  ClassArchetypeTypeInfo(llvm::PointerType *storageType,
                         Size size, const SpareBitVector &spareBits,
                         Alignment align,
                         ReferenceCounting refCount)
    : HeapTypeInfo(storageType, size, spareBits, align),
      RefCount(refCount)
  {}

public:
  static const ClassArchetypeTypeInfo *create(llvm::PointerType *storageType,
                                         Size size, const SpareBitVector &spareBits,
                                         Alignment align,
                                         ReferenceCounting refCount) {
    return new ClassArchetypeTypeInfo(storageType, size, spareBits, align,
                                      refCount);
  }

  ReferenceCounting getReferenceCounting() const {
    return RefCount;
  }
};

class FixedSizeArchetypeTypeInfo
  : public PODSingleScalarTypeInfo<FixedSizeArchetypeTypeInfo, LoadableTypeInfo>
{
  FixedSizeArchetypeTypeInfo(llvm::Type *type, Size size, Alignment align,
                             const SpareBitVector &spareBits)
      : PODSingleScalarTypeInfo(type, size, spareBits, align) {}

public:
  static const FixedSizeArchetypeTypeInfo *
  create(llvm::Type *type, Size size, Alignment align,
         const SpareBitVector &spareBits) {
    return new FixedSizeArchetypeTypeInfo(type, size, align, spareBits);
  }
};

} // end anonymous namespace

/// Emit a single protocol witness table reference.
llvm::Value *irgen::emitArchetypeWitnessTableRef(IRGenFunction &IGF,
                                                 CanArchetypeType archetype,
                                                 ProtocolDecl *protocol) {
  assert(Lowering::TypeConverter::protocolRequiresWitnessTable(protocol) &&
         "looking up witness table for protocol that doesn't have one");

  // The following approach assumes that a protocol will only appear in
  // an archetype's conformsTo array if the archetype is either explicitly
  // constrained to conform to that protocol (in which case we should have
  // a cache entry for it) or there's an associated type declaration with
  // that protocol listed as a direct requirement.

  auto localDataKind =
    LocalTypeDataKind::forAbstractProtocolWitnessTable(protocol);

  // Check immediately for an existing cache entry.
  // TODO: don't give this absolute precedence over other access paths.
  auto wtable = IGF.tryGetLocalTypeData(archetype, localDataKind);
  if (wtable) return wtable;

  // If we don't have an environment, this must be an implied witness table
  // reference.
  // FIXME: eliminate this path when opened types have generic environments.
  auto environment = archetype->getGenericEnvironment();
  if (!environment) {
    assert(archetype->isOpenedExistential() &&
           "non-opened archetype lacking generic environment?");
    SmallVector<ProtocolEntry, 4> entries;
    for (auto p : archetype->getConformsTo()) {
      const ProtocolInfo &impl = IGF.IGM.getProtocolInfo(p);
      entries.push_back(ProtocolEntry(p, impl));
    }

    return emitImpliedWitnessTableRef(IGF, entries, protocol,
        [&](unsigned index) -> llvm::Value* {
      auto localDataKind =
         LocalTypeDataKind::forAbstractProtocolWitnessTable(
                                                  entries[index].getProtocol());
      auto wtable = IGF.tryGetLocalTypeData(archetype, localDataKind);
      assert(wtable &&
             "opened type without local type data for direct conformance?");
      return wtable;
    });
  }

  // Otherwise, ask the generic signature for the environment for the best
  // path to the conformance.
  // TODO: this isn't necessarily optimal if the direct conformance isn't
  // concretely available; we really ought to be comparing the full paths
  // to this conformance from concrete sources.

  auto signature = environment->getGenericSignature()->getCanonicalSignature();
  auto archetypeDepType = archetype->getInterfaceType();

  auto astPath = signature->getConformanceAccessPath(archetypeDepType,
                                                     protocol);

  auto i = astPath.begin(), e = astPath.end();
  assert(i != e && "empty path!");

  // The first entry in the path is a direct requirement of the signature,
  // for which we should always have local type data available.
  CanType rootArchetype =
    environment->mapTypeIntoContext(i->first)->getCanonicalType();
  ProtocolDecl *rootProtocol = i->second;

  // Turn the rest of the path into a MetadataPath.
  auto lastProtocol = rootProtocol;
  MetadataPath path;
  while (++i != e) {
    auto &entry = *i;
    CanType depType = CanType(entry.first);
    ProtocolDecl *requirement = entry.second;

    auto &lastPI = IGF.IGM.getProtocolInfo(lastProtocol);

    // If it's a type parameter, it's self, and this is a base protocol
    // requirement.
    if (isa<GenericTypeParamType>(depType)) {
      assert(depType->isEqual(lastProtocol->getSelfInterfaceType()));
      WitnessIndex index = lastPI.getBaseIndex(requirement);
      path.addInheritedProtocolComponent(index);

    // Otherwise, it's an associated conformance requirement.
    } else {
      AssociatedConformance association(lastProtocol, depType, requirement);
      WitnessIndex index = lastPI.getAssociatedConformanceIndex(association);
      path.addAssociatedConformanceComponent(index);
    }

    lastProtocol = requirement;
  }
  assert(lastProtocol == protocol);

  auto rootWTable = IGF.tryGetLocalTypeData(rootArchetype,
              LocalTypeDataKind::forAbstractProtocolWitnessTable(rootProtocol));
  assert(rootWTable && "root witness table not bound in local context!");

  wtable = path.followFromWitnessTable(IGF, rootArchetype,
                                       ProtocolConformanceRef(rootProtocol),
                                       MetadataResponse::forComplete(rootWTable),
                                       /*request*/ MetadataState::Complete,
                                       nullptr).getMetadata();

  return wtable;
}

MetadataResponse
irgen::emitAssociatedTypeMetadataRef(IRGenFunction &IGF,
                                     CanArchetypeType origin,
                                     AssociatedType association,
                                     DynamicMetadataRequest request) {
  // Find the conformance of the origin to the associated type's protocol.
  llvm::Value *wtable = emitArchetypeWitnessTableRef(IGF, origin,
                                               association.getSourceProtocol());

  // Find the origin's type metadata.
  llvm::Value *originMetadata =
    emitArchetypeTypeMetadataRef(IGF, origin, MetadataState::Abstract)
      .getMetadata();

  return emitAssociatedTypeMetadataRef(IGF, originMetadata, wtable,
                                       association, request);
}

const TypeInfo *TypeConverter::convertArchetypeType(ArchetypeType *archetype) {
  assert(isExemplarArchetype(archetype) && "lowering non-exemplary archetype");

  auto layout = archetype->getLayoutConstraint();

  // If the archetype is class-constrained, use a class pointer
  // representation.
  if (archetype->requiresClass() ||
      (layout && layout->isRefCounted())) {
    auto refcount = getReferenceCountingForType(IGM, CanType(archetype));

    llvm::PointerType *reprTy;

    // If the archetype has a superclass constraint, it has at least the
    // retain semantics of its superclass, and it can be represented with
    // the supertype's pointer type.
    if (auto super = archetype->getSuperclass()) {
      auto &superTI = IGM.getTypeInfoForUnlowered(super);
      reprTy = cast<llvm::PointerType>(superTI.StorageType);
    } else {
      if (refcount == ReferenceCounting::Native) {
        reprTy = IGM.RefCountedPtrTy;
      } else {
        reprTy = IGM.UnknownRefCountedPtrTy;
      }
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
                                      refcount);
  }

  // If the archetype is trivial fixed-size layout-constrained, use a fixed size
  // representation.
  if (layout && layout->isFixedSizeTrivial()) {
    Size size(layout->getTrivialSizeInBytes());
    auto layoutAlignment = layout->getAlignmentInBytes();
    assert(layoutAlignment && "layout constraint alignment should not be 0");
    Alignment align(layoutAlignment);
    auto spareBits =
      SpareBitVector::getConstant(size.getValueInBits(), false);
    // Get an integer type of the required size.
    auto ProperlySizedIntTy = SILType::getBuiltinIntegerType(
        size.getValueInBits(), IGM.getSwiftModule()->getASTContext());
    auto storageType = IGM.getStorageType(ProperlySizedIntTy);
    return FixedSizeArchetypeTypeInfo::create(storageType, size, align,
                                              spareBits);
  }

  // If the archetype is a trivial layout-constrained, use a POD
  // representation. This type is not loadable, but it is known
  // to be a POD.
  if (layout && layout->isAddressOnlyTrivial()) {
    // TODO: Create NonFixedSizeArchetypeTypeInfo and return it.
  }

  // Otherwise, for now, always use an opaque indirect type.
  llvm::Type *storageType = IGM.OpaquePtrTy->getElementType();
  return OpaqueArchetypeTypeInfo::create(storageType);
}

static void setMetadataRef(IRGenFunction &IGF,
                           ArchetypeType *archetype,
                           llvm::Value *metadata,
                           MetadataState metadataState) {
  assert(metadata->getType() == IGF.IGM.TypeMetadataPtrTy);
  IGF.setUnscopedLocalTypeMetadata(CanType(archetype),
                         MetadataResponse::forBounded(metadata, metadataState));
}

static void setWitnessTable(IRGenFunction &IGF,
                            ArchetypeType *archetype,
                            unsigned protocolIndex,
                            llvm::Value *wtable) {
  assert(wtable->getType() == IGF.IGM.WitnessTablePtrTy);
  assert(protocolIndex < archetype->getConformsTo().size());
  auto protocol = archetype->getConformsTo()[protocolIndex];
  IGF.setUnscopedLocalTypeData(CanType(archetype),
                  LocalTypeDataKind::forAbstractProtocolWitnessTable(protocol),
                               wtable);
}

/// Inform IRGenFunction that the given archetype has the given value
/// witness value within this scope.
void IRGenFunction::bindArchetype(ArchetypeType *archetype,
                                  llvm::Value *metadata,
                                  MetadataState metadataState,
                                  ArrayRef<llvm::Value*> wtables) {
  // Set the metadata pointer.
  setTypeMetadataName(IGM, metadata, CanType(archetype));
  setMetadataRef(*this, archetype, metadata, metadataState);

  // Set the protocol witness tables.

  unsigned wtableI = 0;
  for (unsigned i = 0, e = archetype->getConformsTo().size(); i != e; ++i) {
    auto proto = archetype->getConformsTo()[i];
    if (!Lowering::TypeConverter::protocolRequiresWitnessTable(proto))
      continue;
    auto wtable = wtables[wtableI++];
    setProtocolWitnessTableName(IGM, wtable, CanType(archetype), proto);
    setWitnessTable(*this, archetype, i, wtable);
  }
  assert(wtableI == wtables.size());
}

llvm::Value *irgen::emitDynamicTypeOfOpaqueArchetype(IRGenFunction &IGF,
                                                     Address addr,
                                                     SILType type) {
  auto archetype = type.castTo<ArchetypeType>();

  // Acquire the archetype's static metadata.
  llvm::Value *metadata =
    emitArchetypeTypeMetadataRef(IGF, archetype, MetadataState::Complete)
      .getMetadata();
  return IGF.Builder.CreateCall(IGF.IGM.getGetDynamicTypeFn(),
                                {addr.getAddress(), metadata,
                                 llvm::ConstantInt::get(IGF.IGM.Int1Ty, 0)});
}
