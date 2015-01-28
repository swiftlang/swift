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

#include "Explosion.h"
#include "GenMeta.h"
#include "GenProto.h"
#include "IRGenFunction.h"
#include "IRGenModule.h"
#include "TypeInfo.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Module.h"

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
  if (shouldDestroyOnFailure(consumptionKind))
    flags |= DynamicCastFlags::DestroyOnFailure;
  if (shouldTakeOnSuccess(consumptionKind))
    flags |= DynamicCastFlags::TakeOnSuccess;

  return flags;
}

/// Emit a checked cast, starting with a value in memory.
llvm::Value *irgen::emitCheckedCast(IRGenFunction &IGF,
                                    Address src,
                                    CanType srcType,
                                    Address dest,
                                    CanType targetType,
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

llvm::Value *irgen::emitClassIdenticalCast(IRGenFunction &IGF,
                                           llvm::Value *from,
                                           SILType fromType,
                                           SILType toType,
                                           CheckedCastMode mode) {
  // Emit a reference to the heap metadata for the target type.
  const bool allowConservative = true;

  // If we're allowed to do a conservative check, try to just use the
  // global class symbol.  If the class has been re-allocated, this
  // might not be the heap metadata actually in use, and hence the
  // test might fail; but it's a much faster check.
  // TODO: use ObjC class references
  llvm::Value *targetMetadata;
  if (allowConservative &&
      (targetMetadata = tryEmitConstantHeapMetadataRef(IGF.IGM,
                                          toType.getSwiftRValueType()))) {
    // ok
  } else {
    targetMetadata
      = emitClassHeapMetadataRef(IGF, toType.getSwiftRValueType(),
                                 MetadataValueType::ObjCClass,
                                 /*allowUninitialized*/ allowConservative);
  }

  llvm::Value *objectMetadata =
    emitHeapMetadataRefForHeapObject(IGF, from, fromType);

  objectMetadata = IGF.Builder.CreateBitCast(objectMetadata, targetMetadata->getType());
  llvm::Value *cond = IGF.Builder.CreateICmpEQ(objectMetadata, targetMetadata);
  llvm::Value *nil =
    llvm::ConstantPointerNull::get(cast<llvm::PointerType>(from->getType()));
  llvm::Value *result = IGF.Builder.CreateSelect(cond, from, nil);
  return result;
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
    metadataRef = IGF.emitTypeMetadataRef(toType.getSwiftRValueType());

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
    metadataRef = IGF.emitTypeMetadataRef(toType.getSwiftRValueType());

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
    metadataRef = emitObjCHeapMetadataRef(IGF, destClass);

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

/// Emit a checked cast of a metatype.
void irgen::emitMetatypeDowncast(IRGenFunction &IGF,
                                 llvm::Value *metatype,
                                 CanMetatypeType toMetatype,
                                 CheckedCastMode mode,
                                 Explosion &ex) {
  // Pick a runtime entry point and target metadata based on what kind of
  // representation we're casting.
  llvm::Value *castFn;
  llvm::Value *toMetadata;

  switch (toMetatype->getRepresentation()) {
  case MetatypeRepresentation::Thick: {
    assert(IGF.IGM.ObjCInterop && "should have objc runtime");
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
    assert(IGF.IGM.ObjCInterop && "should have objc runtime");

    // Get the ObjC metadata for the type we're checking.
    toMetadata = emitClassHeapMetadataRef(IGF, toMetatype.getInstanceType(),
                                          MetadataValueType::ObjCClass);
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
  ex.add(call);
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

/// Emit a helper function to look up \c numProtocols witness tables given
/// a type metadata reference.
static llvm::Function *emitExistentialScalarCastFn(IRGenModule &IGM,
                                                   unsigned numProtocols,
                                                   CheckedCastMode mode) {
  // Build the function name.
  llvm::SmallString<32> name;
  {
    llvm::raw_svector_ostream os(name);
    os << "dynamic_cast_existential_";
    os << numProtocols;
    switch (mode) {
    case CheckedCastMode::Unconditional:
      os << "_unconditional";
      break;
    case CheckedCastMode::Conditional:
      os << "_conditional";
      break;
    }
    os.flush();
  }
  
  // See if we already defined this function.
  
  if (auto fn = IGM.Module.getFunction(name))
    return fn;
  
  // Build the function type.
  
  llvm::SmallVector<llvm::Type *, 4> argTys;
  llvm::SmallVector<llvm::Type *, 4> returnTys;
  argTys.push_back(IGM.TypeMetadataPtrTy);
  for (unsigned i = 0; i < numProtocols; ++i) {
    argTys.push_back(IGM.ProtocolDescriptorPtrTy);
    returnTys.push_back(IGM.WitnessTablePtrTy);
  }
  
  llvm::Type *returnTy;
  if (numProtocols == 1)
    returnTy = returnTys.front();
  else
    returnTy = llvm::StructType::get(IGM.getLLVMContext(), returnTys);
  
  auto fnTy = llvm::FunctionType::get(returnTy, argTys, /*vararg*/ false);
  auto fn = llvm::Function::Create(fnTy, llvm::GlobalValue::PrivateLinkage,
                                   llvm::Twine(name), IGM.getModule());
  
  auto IGF = IRGenFunction(IGM, fn);
  Explosion args = IGF.collectParameters();
  
  auto ref = args.claimNext();
  auto failBB = IGF.createBasicBlock("fail");
  auto conformsToProtocol = IGM.getConformsToProtocolFn();
  
  Explosion rets;
  // Look up each protocol conformance we want.
  for (unsigned i = 0; i < numProtocols; ++i) {
    auto proto = args.claimNext();
    auto witness = IGF.Builder.CreateCall2(conformsToProtocol, ref, proto);
    auto isNull = IGF.Builder.CreateICmpEQ(witness,
                     llvm::ConstantPointerNull::get(IGM.WitnessTablePtrTy));
    auto contBB = IGF.createBasicBlock("cont");
    IGF.Builder.CreateCondBr(isNull, failBB, contBB);
    
    IGF.Builder.emitBlock(contBB);
    rets.add(witness);
  }
  
  // If we succeeded, return the witnesses.
  IGF.emitScalarReturn(returnTy, rets);
  
  // If we failed, return nil.
  IGF.Builder.emitBlock(failBB);
  switch (mode) {
  case CheckedCastMode::Conditional: {
    auto null = llvm::ConstantStruct::getNullValue(returnTy);
    IGF.Builder.CreateRet(null);
    break;
  }

  case CheckedCastMode::Unconditional: {
    llvm::Function *trapIntrinsic = llvm::Intrinsic::getDeclaration(&IGM.Module,
                                                    llvm::Intrinsic::ID::trap);
    IGF.Builder.CreateCall(trapIntrinsic);
    IGF.Builder.CreateUnreachable();
    break;
  }
  }
  
  return fn;
}

void irgen::emitMetatypeToObjectDowncast(IRGenFunction &IGF,
                                         llvm::Value *metatypeValue,
                                         CanAnyMetatypeType type,
                                         CheckedCastMode mode,
                                         Explosion &ex) {
  // If ObjC interop is enabled, casting a metatype to AnyObject succeeds
  // if the metatype is for a class.
  auto triviallyFail = [&] {
    ex.add(llvm::ConstantPointerNull::get(IGF.IGM.ObjCPtrTy));
  };
  
  if (!IGF.IGM.ObjCInterop)
    return triviallyFail();
  
  switch (type->getRepresentation()) {
  case MetatypeRepresentation::ObjC:
    // Metatypes that can be represented as ObjC trivially cast to AnyObject.
    ex.add(IGF.Builder.CreateBitCast(metatypeValue, IGF.IGM.ObjCPtrTy));
    return;

  case MetatypeRepresentation::Thin:
    // Metatypes that can be thin would never be classes.
    // TODO: Final class metatypes could in principle be thin.
    assert(!type.getInstanceType()->mayHaveSuperclass()
           && "classes should not have thin metatypes (yet)");
    return triviallyFail();
    
  case MetatypeRepresentation::Thick: {
    auto instanceTy = type.getInstanceType();
    // Is the type obviously a class?
    if (instanceTy->mayHaveSuperclass()) {
      // Get the ObjC metadata for the class.
      auto heapMetadata = emitClassHeapMetadataRefForMetatype(IGF,metatypeValue,
                                                              instanceTy);
      ex.add(IGF.Builder.CreateBitCast(heapMetadata, IGF.IGM.ObjCPtrTy));
      return;
    }
    
    // Is the type obviously not a class?
    if (!isa<ArchetypeType>(instanceTy)
        && !isa<ExistentialMetatypeType>(type))
      return triviallyFail();

    // Ask the runtime whether this is class metadata.
    llvm::Constant *castFn;
    switch (mode) {
    case CheckedCastMode::Conditional:
      castFn = IGF.IGM.getDynamicCastMetatypeToObjectConditionalFn();
      break;
    case CheckedCastMode::Unconditional:
      castFn = IGF.IGM.getDynamicCastMetatypeToObjectUnconditionalFn();
      break;
    }
    
    auto call = IGF.Builder.CreateCall(castFn, metatypeValue);
    ex.add(call);
    return;
  }
  }

}


/// Emit a checked cast to an Objective-C protocol or protocol composition.
void irgen::emitScalarExistentialDowncast(IRGenFunction &IGF,
                                  llvm::Value *value,
                                  SILType srcType,
                                  SILType destType,
                                  CheckedCastMode mode,
                                  Optional<MetatypeRepresentation> metatypeKind,
                                  Explosion &ex) {
  SmallVector<ProtocolDecl*, 4> allProtos;
  destType.getSwiftRValueType().getAnyExistentialTypeProtocols(allProtos);

  // Get references to the ObjC Protocol* values for the objc protocols.
  SmallVector<llvm::Value*, 4> objcProtos;
  bool requiresWitnessTableLookup = false;
  bool requiresClassCheck = false;
  
  for (auto proto : allProtos) {
    if (requiresProtocolWitnessTable(proto))
      requiresWitnessTableLookup = true;
    if (!proto->isObjC())
      continue;
    if (proto->getKnownProtocolKind()
        && *proto->getKnownProtocolKind() == KnownProtocolKind::AnyObject) {
      // Casting an object to AnyObject trivially succeeds.
      continue;
    }
    
    objcProtos.push_back(emitReferenceToObjCProtocol(IGF, proto));
  }
  
  // If we don't have any protocols we really need to check, then trivially
  // succeed.
  if (objcProtos.empty() && !requiresWitnessTableLookup && !requiresClassCheck){
    value = IGF.Builder.CreateBitCast(value, IGF.IGM.ObjCPtrTy);
    ex.add(value);
    return;
  }
  
  // Check the ObjC protocol conformances if there were any.
  llvm::Value *objcCast = nullptr;
  if (!objcProtos.empty()) {
    // Get the ObjC instance or class object to check for these conformances.
    llvm::Value *objcObject;
    if (metatypeKind) {
      switch (*metatypeKind) {
      case MetatypeRepresentation::Thin:
        llvm_unreachable("can't cast to thin metatype");
      case MetatypeRepresentation::Thick: {
        // The metadata might be for a non-class type, which wouldn't have
        // an ObjC class object.
        objcObject = nullptr;
        break;
      }
      case MetatypeRepresentation::ObjC:
        // Metatype is already an ObjC object.
        objcObject = value;
        break;
      }
    } else {
      // Class instance is already an ObjC object.
      objcObject = value;
    }
    if (objcObject)
      objcObject = IGF.Builder.CreateBitCast(objcObject,
                                             IGF.IGM.UnknownRefCountedPtrTy);
    
    // Pick the cast function based on the cast mode and on whether we're
    // casting a Swift metatype or ObjC object.
    llvm::Value *castFn;
    switch (mode) {
    case CheckedCastMode::Unconditional:
      castFn = objcObject
        ? IGF.IGM.getDynamicCastObjCProtocolUnconditionalFn()
        : IGF.IGM.getDynamicCastTypeToObjCProtocolUnconditionalFn();
      break;
    case CheckedCastMode::Conditional:
      castFn = objcObject
        ? IGF.IGM.getDynamicCastObjCProtocolConditionalFn()
        : IGF.IGM.getDynamicCastTypeToObjCProtocolConditionalFn();
      break;
    }
    llvm::Value *objcCastObject = objcObject ? objcObject : value;
    
    Address protoRefsBuf = IGF.createAlloca(
                                        llvm::ArrayType::get(IGF.IGM.Int8PtrTy,
                                                             objcProtos.size()),
                                        IGF.IGM.getPointerAlignment(),
                                        "objc_protocols");
    protoRefsBuf = IGF.Builder.CreateBitCast(protoRefsBuf,
                                             IGF.IGM.Int8PtrPtrTy);

    for (unsigned index : indices(objcProtos)) {
      Address protoRefSlot = IGF.Builder.CreateConstArrayGEP(
                                                     protoRefsBuf, index,
                                                     IGF.IGM.getPointerSize());
      IGF.Builder.CreateStore(objcProtos[index], protoRefSlot);
      ++index;
    }

    
    objcCast = IGF.Builder.CreateCall3(castFn, objcCastObject,
                                       IGF.IGM.getSize(Size(objcProtos.size())),
                                       protoRefsBuf.getAddress());
  }

  // Add the value to the explosion.
  llvm::Type *resultType;
  if (metatypeKind) {
    switch (*metatypeKind) {
    case MetatypeRepresentation::Thin:
      llvm_unreachable("can't cast to thin metatype");
    case MetatypeRepresentation::Thick:
      resultType = IGF.IGM.TypeMetadataPtrTy;
      break;
    case MetatypeRepresentation::ObjC:
      resultType = IGF.IGM.ObjCClassPtrTy;
      break;
    }
  } else {
    auto schema = IGF.getTypeInfo(destType).getSchema();
    resultType = schema[0].getScalarType();
  }
  
  llvm::Value *resultValue;
  if (objcCast)
    resultValue = IGF.Builder.CreateBitCast(objcCast, resultType);
  else
    resultValue = value;
  ex.add(resultValue);
  
  // If we don't need to look up any witness tables, we're done.
  if (!requiresWitnessTableLookup)
    return;
  
  // If we're doing a conditional cast, and the ObjC protocol checks failed,
  // then the cast is done.
  llvm::BasicBlock *origBB = nullptr, *successBB = nullptr, *contBB = nullptr;
  if (!objcProtos.empty()) {
    switch (mode) {
    case CheckedCastMode::Unconditional:
      break;
    case CheckedCastMode::Conditional: {
      origBB = IGF.Builder.GetInsertBlock();
      successBB = IGF.createBasicBlock("success");
      contBB = IGF.createBasicBlock("cont");
      auto isNull = IGF.Builder.CreateICmpEQ(objcCast,
                               llvm::ConstantPointerNull::get(
                                 cast<llvm::PointerType>(objcCast->getType())));
      IGF.Builder.CreateCondBr(isNull, contBB, successBB);
      IGF.Builder.emitBlock(successBB);
    }
    }
  }

  // Get the Swift type metadata for the type.
  llvm::Value *metadataValue;
  if (metatypeKind) {
    switch (*metatypeKind) {
    case MetatypeRepresentation::Thin:
      llvm_unreachable("can't cast to thin metatype");
    case MetatypeRepresentation::Thick:
      // The value is already a native metatype.
      metadataValue = value;
      break;
    case MetatypeRepresentation::ObjC:
      // Get the type metadata from the ObjC class, which may be a wrapper.
      metadataValue = emitObjCMetadataRefForMetadata(IGF, value);
    }
  } else {
    // Get the type metadata for the instance.
    metadataValue = emitDynamicTypeOfHeapObject(IGF, value, srcType);
  }
  
  // Look up witness tables for the protocols that need them.
  SmallVector<llvm::Value*, 4> witnessTableProtos;
  for (auto proto : allProtos) {
    if (!requiresProtocolWitnessTable(proto))
      continue;
    auto descriptor = emitProtocolDescriptorRef(IGF, proto);
    witnessTableProtos.push_back(descriptor);
  }
  
  if (!witnessTableProtos.empty()) {
    auto fn = emitExistentialScalarCastFn(IGF.IGM, witnessTableProtos.size(),
                                          mode);
    llvm::SmallVector<llvm::Value *, 4> args;
    args.push_back(metadataValue);
    for (auto proto : witnessTableProtos)
      args.push_back(proto);
    auto wts = IGF.Builder.CreateCall(fn, args);
    if (witnessTableProtos.size() == 1)
      ex.add(wts);
    else
      for (unsigned i = 0, e = witnessTableProtos.size(); i < e; ++i) {
        auto wt = IGF.Builder.CreateExtractValue(wts, i);
        ex.add(wt);
      }
  }
  
  // If we had conditional ObjC checks, join the failure paths.
  if (contBB) {
    IGF.Builder.CreateBr(contBB);
    IGF.Builder.emitBlock(contBB);
    
    // Return null on the failure path.
    Explosion successEx = std::move(ex);
    ex.reset();
    
    while (!successEx.empty()) {
      auto successVal = successEx.claimNext();
      auto failureVal = llvm::Constant::getNullValue(successVal->getType());
      auto phi = IGF.Builder.CreatePHI(successVal->getType(), 2);
      phi->addIncoming(successVal, successBB);
      phi->addIncoming(failureVal, origBB);
      ex.add(phi);
    }
  }
}
