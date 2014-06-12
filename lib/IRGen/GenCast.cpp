//===--- GenCast.cpp - Swift IR Generation for dynamic casts --------------===//
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
//  This file implements IR generation for dynamic casts.
//
//===----------------------------------------------------------------------===//

#include "GenCast.h"

#include "GenMeta.h"
#include "GenProto.h"
#include "IRGenFunction.h"
#include "IRGenModule.h"
#include "TypeInfo.h"

#include "swift/Basic/Fallthrough.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/ABI/MetadataValues.h"

using namespace swift;
using namespace irgen;

/// Compute the flags to pass to swift_dynamicCast.
static DynamicCastFlags getDynamicCastFlags(CastConsumptionKind consumptionKind,
                                            CheckedCastMode mode) {
  DynamicCastFlags flags = DynamicCastFlags::Default;

  if (mode == CheckedCastMode::Unconditional)
    flags |= DynamicCastFlags::Unconditional;

  switch (consumptionKind) {
  case CastConsumptionKind::TakeAlways:
    flags |= DynamicCastFlags::DestroyOnFailure;
    SWIFT_FALLTHROUGH;
  case CastConsumptionKind::TakeOnSuccess:
    flags |= DynamicCastFlags::TakeOnSuccess;
    break;
  case CastConsumptionKind::CopyOnSuccess:
    break;
  }

  return flags;
}

/// Emit a checked cast, starting with a value in memory.
llvm::Value *irgen::emitCheckedCast(IRGenFunction &IGF,
                                    Address src,
                                    SILType srcType,
                                    Address dest,
                                    SILType targetType,
                                    CastConsumptionKind consumptionKind,
                                    CheckedCastMode mode) {
  // TODO: attempt to specialize this based on the known types.

  DynamicCastFlags flags = getDynamicCastFlags(consumptionKind, mode);

  // Cast both addresses to opaque pointer type.
  dest = IGF.Builder.CreateBitCast(dest, IGF.IGM.OpaquePtrTy);
  src = IGF.Builder.CreateBitCast(src, IGF.IGM.OpaquePtrTy);

  // Load type metadata for the source's static type and the target type.
  llvm::Value *srcMetadata = IGF.emitTypeMetadataRef(srcType);
  llvm::Value *targetMetadata = IGF.emitTypeMetadataRef(targetType);

  llvm::Value *args[] = {
    dest.getAddress(), src.getAddress(),
    srcMetadata, targetMetadata,
    IGF.IGM.getSize(Size(unsigned(flags)))
  };

  auto call = IGF.Builder.CreateCall(IGF.IGM.getDynamicCastFn(), args);
  call->setDoesNotThrow();

  return call;
}

llvm::Value *irgen::emitSuperToClassArchetypeConversion(IRGenFunction &IGF,
                                                        llvm::Value *super,
                                                        SILType destType,
                                                        CheckedCastMode mode) {
  assert(destType.is<ArchetypeType>() && "expected archetype type");
  assert(destType.castTo<ArchetypeType>()->requiresClass()
         && "expected class archetype type");

  return emitClassDowncast(IGF, super, destType, mode);
}

/// Emit a checked unconditional downcast of a class value.
llvm::Value *irgen::emitClassDowncast(IRGenFunction &IGF, llvm::Value *from,
                                      SILType toType, CheckedCastMode mode) {
  // Emit the value we're casting from.
  if (from->getType() != IGF.IGM.Int8PtrTy)
    from = IGF.Builder.CreateBitCast(from, IGF.IGM.Int8PtrTy);

  // Emit a reference to the metadata and figure out what cast
  // function to use.
  llvm::Value *metadataRef;
  llvm::Constant *castFn;

  // Get the best known type information about the destination type.
  auto destClass = toType.getSwiftRValueType().getClassBound();
  assert(destClass || toType.is<ArchetypeType>());

  // If the destination type is known to have a Swift-compatible
  // implementation, use the most specific entrypoint.
  if (destClass && hasKnownSwiftImplementation(IGF.IGM, destClass)) {
    metadataRef = IGF.emitTypeMetadataRef(toType);

    switch (mode) {
    case CheckedCastMode::Unconditional:
      castFn = IGF.IGM.getDynamicCastClassUnconditionalFn();
      break;
    case CheckedCastMode::Conditional:
      castFn = IGF.IGM.getDynamicCastClassFn();
      break;
    }

  // If the destination type is a foreign class or a non-specific
  // class-bounded archetype, use the most general cast entrypoint.
  } else if (!destClass || destClass->isForeign()) {
    metadataRef = IGF.emitTypeMetadataRef(toType);

    switch (mode) {
    case CheckedCastMode::Unconditional:
      castFn = IGF.IGM.getDynamicCastUnknownClassUnconditionalFn();
      break;
    case CheckedCastMode::Conditional:
      castFn = IGF.IGM.getDynamicCastUnknownClassFn();
      break;
    }

  // Otherwise, use the ObjC-specific entrypoint.
  } else {
    metadataRef = IGF.IGM.getAddrOfObjCClass(destClass, NotForDefinition);

    switch (mode) {
    case CheckedCastMode::Unconditional:
      castFn = IGF.IGM.getDynamicCastObjCClassUnconditionalFn();
      break;
    case CheckedCastMode::Conditional:
      castFn = IGF.IGM.getDynamicCastObjCClassFn();
      break;
    }
  }

  if (metadataRef->getType() != IGF.IGM.Int8PtrTy)
    metadataRef = IGF.Builder.CreateBitCast(metadataRef, IGF.IGM.Int8PtrTy);

  // Call the (unconditional) dynamic cast.
  auto call
    = IGF.Builder.CreateCall2(castFn, from, metadataRef);
  // FIXME: Eventually, we may want to throw.
  call->setDoesNotThrow();

  llvm::Type *subTy = IGF.getTypeInfo(toType).StorageType;
  return IGF.Builder.CreateBitCast(call, subTy);
}

static Address
emitOpaqueDowncast(IRGenFunction &IGF,
                   Address value,
                   llvm::Value *srcMetadata,
                   SILType destType,
                   CheckedCastMode mode) {
  llvm::Value *addr = IGF.Builder.CreateBitCast(value.getAddress(),
                                                IGF.IGM.OpaquePtrTy);

  srcMetadata = IGF.Builder.CreateBitCast(srcMetadata, IGF.IGM.Int8PtrTy);
  llvm::Value *destMetadata = IGF.emitTypeMetadataRef(destType);
  destMetadata = IGF.Builder.CreateBitCast(destMetadata, IGF.IGM.Int8PtrTy);

  llvm::Value *castFn;
  switch (mode) {
  case CheckedCastMode::Unconditional:
    castFn = IGF.IGM.getDynamicCastIndirectUnconditionalFn();
    break;
  case CheckedCastMode::Conditional:
    castFn = IGF.IGM.getDynamicCastIndirectFn();
    break;
  }

  auto *call = IGF.Builder.CreateCall3(castFn, addr, srcMetadata, destMetadata);
  // FIXME: Eventually, we may want to throw.
  call->setDoesNotThrow();

  // Convert the cast address to the destination type.
  auto &destTI = IGF.getTypeInfo(destType);
  llvm::Value *ptr = IGF.Builder.CreateBitCast(call,
                                           destTI.StorageType->getPointerTo());
  return destTI.getAddressForPointer(ptr);
}

/// Emit a checked cast of a metatype.
llvm::Value *irgen::emitMetatypeDowncast(IRGenFunction &IGF,
                                         llvm::Value *metatype,
                                         CanAnyMetatypeType toMetatype,
                                         CheckedCastMode mode) {
  // Pick a runtime entry point and target metadata based on what kind of
  // representation we're casting.
  llvm::Value *castFn;
  llvm::Value *toMetadata;

  switch (toMetatype->getRepresentation()) {
  case MetatypeRepresentation::Thick: {
    // Get the Swift metadata for the type we're checking.
    toMetadata = IGF.emitTypeMetadataRef(toMetatype.getInstanceType());
    switch (mode) {
    case CheckedCastMode::Unconditional:
      castFn = IGF.IGM.getDynamicCastMetatypeUnconditionalFn();
      break;
    case CheckedCastMode::Conditional:
      castFn = IGF.IGM.getDynamicCastMetatypeFn();
      break;
    }
    break;
  }

  case MetatypeRepresentation::ObjC: {
    // Get the ObjC metadata for the type we're checking.
    toMetadata = emitClassHeapMetadataRef(IGF, toMetatype.getInstanceType());
    switch (mode) {
    case CheckedCastMode::Unconditional:
      castFn = IGF.IGM.getDynamicCastObjCClassMetatypeUnconditionalFn();
      break;
    case CheckedCastMode::Conditional:
      castFn = IGF.IGM.getDynamicCastObjCClassMetatypeFn();
      break;
    }
    break;
  }

  case MetatypeRepresentation::Thin:
    llvm_unreachable("not implemented");
  }

  auto call = IGF.Builder.CreateCall2(castFn, metatype, toMetadata);
  call->setDoesNotThrow();
  return call;
}

/// Emit a checked cast of an opaque archetype.
Address irgen::emitOpaqueArchetypeDowncast(IRGenFunction &IGF,
                                           Address value,
                                           SILType srcType,
                                           SILType destType,
                                           CheckedCastMode mode) {
  llvm::Value *srcMetadata = IGF.emitTypeMetadataRef(srcType);
  return emitOpaqueDowncast(IGF, value, srcMetadata, destType, mode);
}

/// Emit a checked unconditional cast of an opaque existential container's
/// contained value.
Address irgen::emitIndirectExistentialDowncast(IRGenFunction &IGF,
                                               Address container,
                                               SILType srcType,
                                               SILType destType,
                                               CheckedCastMode mode) {
  assert(srcType.isExistentialType());

  // Project the value pointer and source type metadata out of the existential
  // container.
  Address value;
  llvm::Value *srcMetadata;
  std::tie(value, srcMetadata)
    = emitIndirectExistentialProjectionWithMetadata(IGF, container, srcType,
                                                    CanArchetypeType());

  return emitOpaqueDowncast(IGF, value, srcMetadata, destType, mode);
}

/// Emit a Protocol* value referencing an ObjC protocol.
llvm::Value *irgen::emitReferenceToObjCProtocol(IRGenFunction &IGF,
                                                ProtocolDecl *proto) {
  assert(proto->isObjC() && "not an objc protocol");

  // Get the address of the global variable the protocol reference gets
  // indirected through.
  llvm::Constant *protocolRefAddr
    = IGF.IGM.getAddrOfObjCProtocolRef(proto, NotForDefinition);

  // Load the protocol reference.
  Address addr(protocolRefAddr, IGF.IGM.getPointerAlignment());
  return IGF.Builder.CreateLoad(addr);
}

/// Emit a checked cast to an Objective-C protocol or protocol composition.
llvm::Value *irgen::emitObjCExistentialDowncast(IRGenFunction &IGF,
                                                llvm::Value *orig,
                                                SILType srcType,
                                                SILType destType,
                                                CheckedCastMode mode) {
  orig = IGF.Builder.CreateBitCast(orig, IGF.IGM.ObjCPtrTy);
  SmallVector<ProtocolDecl*, 4> protos;
  destType.getSwiftRValueType().getAnyExistentialTypeProtocols(protos);

  // Get references to the ObjC Protocol* values for each protocol.
  Address protoRefsBuf = IGF.createAlloca(llvm::ArrayType::get(IGF.IGM.Int8PtrTy,
                                                               protos.size()),
                                          IGF.IGM.getPointerAlignment(),
                                          "objc_protocols");
  protoRefsBuf = IGF.Builder.CreateBitCast(protoRefsBuf,
                                           IGF.IGM.Int8PtrPtrTy);

  unsigned index = 0;
  for (auto proto : protos) {
    Address protoRefSlot = IGF.Builder.CreateConstArrayGEP(protoRefsBuf, index,
                                                     IGF.IGM.getPointerSize());
    auto protoRef = emitReferenceToObjCProtocol(IGF, proto);
    IGF.Builder.CreateStore(protoRef, protoRefSlot);
    ++index;
  }

  // Perform the cast.
  llvm::Value *castFn;
  switch (mode) {
  case CheckedCastMode::Unconditional:
    castFn = IGF.IGM.getDynamicCastObjCProtocolUnconditionalFn();
    break;
  case CheckedCastMode::Conditional:
    castFn = IGF.IGM.getDynamicCastObjCProtocolConditionalFn();
    break;
  }

  return IGF.Builder.CreateCall3(castFn, orig,
                                 IGF.IGM.getSize(Size(protos.size())),
                                 protoRefsBuf.getAddress());
}
