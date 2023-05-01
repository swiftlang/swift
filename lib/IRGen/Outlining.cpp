//===--- Outlining.cpp - Outlining value operations -----------------------===//
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
//  This file implements IR generation for outlined value operations in Swift.
//
//===----------------------------------------------------------------------===//

#include "Outlining.h"

#include "Explosion.h"
#include "GenOpaque.h"
#include "GenProto.h"
#include "IRGenFunction.h"
#include "IRGenMangler.h"
#include "IRGenModule.h"
#include "LoadableTypeInfo.h"
#include "LocalTypeDataKind.h"
#include "MetadataRequest.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/IRGenOptions.h"
#include "swift/SIL/SILModule.h"

using namespace swift;
using namespace irgen;

void OutliningMetadataCollector::collectTypeMetadataForLayout(SILType type) {
  // If the type has no archetypes, we can emit it from scratch in the callee.
  if (!type.hasArchetype()) {
    return;
  }

  // Substitute opaque types if allowed.
  type =
      IGF.IGM.substOpaqueTypesWithUnderlyingTypes(type, CanGenericSignature());

  auto formalType = type.getASTType();
  auto &ti = IGF.IGM.getTypeInfoForLowered(formalType);

  // We don't need the metadata for fixed size types or types that are not ABI
  // accessible. Outlining will call the value witness of the enclosing type of
  // non ABI accessible field/element types.
  if (isa<FixedTypeInfo>(ti) || !ti.isABIAccessible()) {
    return;
  }

  // If the type is a legal formal type, add it as a formal type.
  // FIXME: does this force us to emit a more expensive metadata than we need
  // to?
  if (formalType->isLegalFormalType()) {
    return collectFormalTypeMetadata(formalType);
  }

  auto key = LocalTypeDataKey(type.getASTType(),
                            LocalTypeDataKind::forRepresentationTypeMetadata());
  if (Values.count(key)) return;

  auto metadata = IGF.emitTypeMetadataRefForLayout(type);
  Values.insert({key, metadata});
}

void OutliningMetadataCollector::collectFormalTypeMetadata(CanType type) {
  // If the type has no archetypes, we can emit it from scratch in the callee.
  assert(type->hasArchetype());

  auto key = LocalTypeDataKey(type, LocalTypeDataKind::forFormalTypeMetadata());
  if (Values.count(key)) return;

  auto metadata = IGF.emitTypeMetadataRef(type);
  Values.insert({key, metadata});
}


void OutliningMetadataCollector::addMetadataArguments(
                                    SmallVectorImpl<llvm::Value*> &args) const {
  for (auto &pair : Values) {
    auto metadata = pair.second;
    assert(metadata->getType() == IGF.IGM.TypeMetadataPtrTy);
    args.push_back(metadata);
  }
}

void OutliningMetadataCollector::addMetadataParameterTypes(
                                 SmallVectorImpl<llvm::Type*> &paramTys) const {
  for (auto &pair : Values) {
    auto *metadata = pair.second;
    paramTys.push_back(metadata->getType());
  }
}

void OutliningMetadataCollector::bindMetadataParameters(IRGenFunction &IGF,
                                                      Explosion &params) const {
  // Note that our parameter IGF intentionally shadows the IGF that this
  // collector was built with.
  for (auto &pair : Values) {
    llvm::Value *arg = params.claimNext();

    auto key = pair.first;
    assert(key.Kind.isAnyTypeMetadata());
    setTypeMetadataName(IGF.IGM, arg, key.Type);
    IGF.setUnscopedLocalTypeData(key, MetadataResponse::forComplete(arg));
  }
}

std::pair<CanType, CanGenericSignature>
irgen::getTypeAndGenericSignatureForManglingOutlineFunction(SILType type) {
  auto loweredType = type.getASTType();
  if (!loweredType->hasArchetype()) return {loweredType, nullptr};

  // Find a non-local, non-opaque archetype in the type and pull out
  // its generic environment.
  // TODO: we ought to be able to usefully minimize this

  GenericEnvironment *env = nullptr;
  loweredType.findIf([&env](CanType t) -> bool {
      if (auto arch = dyn_cast<ArchetypeType>(t)) {
        if (!isa<PrimaryArchetypeType>(arch) &&
            !isa<PackArchetypeType>(arch))
          return false;
        env = arch->getGenericEnvironment();
        return true;
      }
      return false;
    });
  assert(env && "has archetype but no archetype?!");
  return {loweredType->mapTypeOutOfContext()->getCanonicalType(),
          env->getGenericSignature().getCanonicalSignature()};
}

void TypeInfo::callOutlinedCopy(IRGenFunction &IGF, Address dest, Address src,
                                SILType T, IsInitialization_t isInit,
                                IsTake_t isTake) const {
  if (!T.hasLocalArchetype() &&
      !IGF.IGM.getOptions().UseTypeLayoutValueHandling) {
    OutliningMetadataCollector collector(IGF);
    if (T.hasArchetype()) {
      collectMetadataForOutlining(collector, T);
    }
    collector.emitCallToOutlinedCopy(dest, src, T, *this, isInit, isTake);
    return;
  }

  if (!T.hasArchetype()) {
    // Call the outlined copy function (the implementation will call vwt in this
    // case).
    OutliningMetadataCollector collector(IGF);
    collector.emitCallToOutlinedCopy(dest, src, T, *this, isInit, isTake);
    return;
  }

  if (isInit == IsInitialization && isTake == IsTake) {
    return emitInitializeWithTakeCall(IGF, T, dest, src);
  } else if (isInit == IsInitialization && isTake == IsNotTake) {
    return emitInitializeWithCopyCall(IGF, T, dest, src);
  } else if (isInit == IsNotInitialization && isTake == IsTake) {
    return emitAssignWithTakeCall(IGF, T, dest, src);
  } else if (isInit == IsNotInitialization && isTake == IsNotTake) {
    return emitAssignWithCopyCall(IGF, T, dest, src);
  }
  llvm_unreachable("unknown case");
}

void OutliningMetadataCollector::emitCallToOutlinedCopy(
                            Address dest, Address src,
                            SILType T, const TypeInfo &ti, 
                            IsInitialization_t isInit, IsTake_t isTake) const {
  llvm::SmallVector<llvm::Value *, 4> args;
  args.push_back(IGF.Builder.CreateElementBitCast(src, ti.getStorageType())
                            .getAddress());
  args.push_back(IGF.Builder.CreateElementBitCast(dest, ti.getStorageType())
                            .getAddress());
  addMetadataArguments(args);

  llvm::Constant *outlinedFn;
  if (isInit && isTake) {
    outlinedFn =
      IGF.IGM.getOrCreateOutlinedInitializeWithTakeFunction(T, ti, *this);
  } else if (isInit) {
    outlinedFn =
      IGF.IGM.getOrCreateOutlinedInitializeWithCopyFunction(T, ti, *this);
  } else if (isTake) {
    outlinedFn =
      IGF.IGM.getOrCreateOutlinedAssignWithTakeFunction(T, ti, *this);
  } else {
    outlinedFn =
      IGF.IGM.getOrCreateOutlinedAssignWithCopyFunction(T, ti, *this);
  }

  llvm::CallInst *call = IGF.Builder.CreateCall(
      cast<llvm::Function>(outlinedFn)->getFunctionType(), outlinedFn, args);
  call->setCallingConv(IGF.IGM.DefaultCC);
}

static bool needsSpecialOwnershipHandling(SILType t) {
  auto astType = t.getASTType();
  auto ref = dyn_cast<ReferenceStorageType>(astType);
  if (!ref) {
    return false;
  }
  return ref->getOwnership() != ReferenceOwnership::Strong;
}

bool isTypeMetadataForLayoutAccessible(SILModule &M, SILType type);

static bool canUseValueWitnessForValueOp(IRGenModule &IGM, SILType T) {
  if (!IGM.getSILModule().isTypeMetadataForLayoutAccessible(T))
    return false;

  // It is not a good code size trade-off to instantiate a metatype for
  // existentials, and also does not back-deploy gracefully in the case of
  // constrained protocols.
  if (T.getASTType()->isExistentialType())
    return false;

  if (needsSpecialOwnershipHandling(T))
    return false;
  if (T.getASTType()->hasDynamicSelfType())
    return false;
  return true;
}

llvm::Constant *IRGenModule::getOrCreateOutlinedInitializeWithTakeFunction(
                              SILType T, const TypeInfo &ti,
                              const OutliningMetadataCollector &collector) {
  auto manglingBits = getTypeAndGenericSignatureForManglingOutlineFunction(T);
  auto funcName =
    IRGenMangler().mangleOutlinedInitializeWithTakeFunction(manglingBits.first,
                                                           manglingBits.second);

  return getOrCreateOutlinedCopyAddrHelperFunction(
      T, ti, collector, funcName,
      [this](IRGenFunction &IGF, Address dest, Address src, SILType T,
         const TypeInfo &ti) {
        if (!IGF.IGM.getOptions().UseTypeLayoutValueHandling ||
            T.hasArchetype() || !canUseValueWitnessForValueOp(*this, T)) {
          ti.initializeWithTake(IGF, dest, src, T, true);
        } else {
          emitInitializeWithTakeCall(IGF, T, dest, src);
        }
      });
}

llvm::Constant *IRGenModule::getOrCreateOutlinedInitializeWithCopyFunction(
                              SILType T, const TypeInfo &ti,
                              const OutliningMetadataCollector &collector) {
  auto manglingBits = getTypeAndGenericSignatureForManglingOutlineFunction(T);
  auto funcName =
    IRGenMangler().mangleOutlinedInitializeWithCopyFunction(manglingBits.first,
                                                           manglingBits.second);

  return getOrCreateOutlinedCopyAddrHelperFunction(
      T, ti, collector, funcName,
      [this](IRGenFunction &IGF, Address dest, Address src, SILType T,
         const TypeInfo &ti) {
        if (!IGF.IGM.getOptions().UseTypeLayoutValueHandling ||
            T.hasArchetype() || !canUseValueWitnessForValueOp(*this, T)) {
          ti.initializeWithCopy(IGF, dest, src, T, true);
        } else {
          emitInitializeWithCopyCall(IGF, T, dest, src);
        }
      });
}

llvm::Constant *IRGenModule::getOrCreateOutlinedAssignWithTakeFunction(
                              SILType T, const TypeInfo &ti,
                              const OutliningMetadataCollector &collector) {
  auto manglingBits = getTypeAndGenericSignatureForManglingOutlineFunction(T);
  auto funcName =
    IRGenMangler().mangleOutlinedAssignWithTakeFunction(manglingBits.first,
                                                        manglingBits.second);

  return getOrCreateOutlinedCopyAddrHelperFunction(
      T, ti, collector, funcName,
      [this](IRGenFunction &IGF, Address dest, Address src, SILType T,
         const TypeInfo &ti) {
        if (!IGF.IGM.getOptions().UseTypeLayoutValueHandling ||
            T.hasArchetype() || !canUseValueWitnessForValueOp(*this, T)) {
          ti.assignWithTake(IGF, dest, src, T, true);
        } else {
          emitAssignWithTakeCall(IGF, T, dest, src);
        }
      });
}

llvm::Constant *IRGenModule::getOrCreateOutlinedAssignWithCopyFunction(
                              SILType T, const TypeInfo &ti,
                              const OutliningMetadataCollector &collector) {
  auto manglingBits = getTypeAndGenericSignatureForManglingOutlineFunction(T);
  auto funcName =
    IRGenMangler().mangleOutlinedAssignWithCopyFunction(manglingBits.first,
                                                        manglingBits.second);

  return getOrCreateOutlinedCopyAddrHelperFunction(
      T, ti, collector, funcName,
      [this](IRGenFunction &IGF, Address dest, Address src, SILType T,
             const TypeInfo &ti) {
        if (!IGF.IGM.getOptions().UseTypeLayoutValueHandling ||
            T.hasArchetype() || !canUseValueWitnessForValueOp(*this, T)) {
          ti.assignWithCopy(IGF, dest, src, T, true);
        } else {
          emitAssignWithCopyCall(IGF, T, dest, src);
        }
      });
}

llvm::Constant *IRGenModule::getOrCreateOutlinedCopyAddrHelperFunction(
                              SILType T, const TypeInfo &ti,
                              const OutliningMetadataCollector &collector,
                              StringRef funcName,
                              CopyAddrHelperGenerator generator) {
  auto ptrTy = ti.getStorageType()->getPointerTo();

  llvm::SmallVector<llvm::Type *, 4> paramTys;
  paramTys.push_back(ptrTy);
  paramTys.push_back(ptrTy);
  collector.addMetadataParameterTypes(paramTys);

  return getOrCreateHelperFunction(funcName, ptrTy, paramTys,
      [&](IRGenFunction &IGF) {
        auto params = IGF.collectParameters();
        Address src = ti.getAddressForPointer(params.claimNext());
        Address dest = ti.getAddressForPointer(params.claimNext());
        collector.bindMetadataParameters(IGF, params);
        generator(IGF, dest, src, T, ti);
        IGF.Builder.CreateRet(dest.getAddress());
      },
      true /*setIsNoInline*/);
}

void TypeInfo::callOutlinedDestroy(IRGenFunction &IGF,
                                   Address addr, SILType T) const {
  // Short-cut destruction of trivial values.
  if (IGF.IGM.getTypeLowering(T).isTrivial())
    return;

  if (!T.hasLocalArchetype() &&
      !IGF.IGM.getOptions().UseTypeLayoutValueHandling) {
    OutliningMetadataCollector collector(IGF);
    if (T.hasArchetype()) {
      collectMetadataForOutlining(collector, T);
    }
    collector.emitCallToOutlinedDestroy(addr, T, *this);
    return;
  }

  if (!T.hasArchetype()) {
    // Call the outlined copy function (the implementation will call vwt in this
    // case).
    OutliningMetadataCollector collector(IGF);
    collector.emitCallToOutlinedDestroy(addr, T, *this);
    return;
  }

  return emitDestroyCall(IGF, T, addr);
}

void OutliningMetadataCollector::emitCallToOutlinedDestroy(
                      Address addr, SILType T, const TypeInfo &ti) const {
  llvm::SmallVector<llvm::Value *, 4> args;
  args.push_back(IGF.Builder.CreateElementBitCast(addr, ti.getStorageType())
                            .getAddress());
  addMetadataArguments(args);

  auto outlinedFn =
    IGF.IGM.getOrCreateOutlinedDestroyFunction(T, ti, *this);

  llvm::CallInst *call = IGF.Builder.CreateCall(
      cast<llvm::Function>(outlinedFn)->getFunctionType(), outlinedFn, args);
  call->setCallingConv(IGF.IGM.DefaultCC);
}

llvm::Constant *IRGenModule::getOrCreateOutlinedDestroyFunction(
                              SILType T, const TypeInfo &ti,
                              const OutliningMetadataCollector &collector) {
  IRGenMangler mangler;
  auto manglingBits = getTypeAndGenericSignatureForManglingOutlineFunction(T);
  auto funcName = mangler.mangleOutlinedDestroyFunction(manglingBits.first,
                                                        manglingBits.second);

  auto ptrTy = ti.getStorageType()->getPointerTo();
  llvm::SmallVector<llvm::Type *, 4> paramTys;
  paramTys.push_back(ptrTy);
  collector.addMetadataParameterTypes(paramTys);

  return getOrCreateHelperFunction(funcName, ptrTy, paramTys,
      [&](IRGenFunction &IGF) {
        Explosion params = IGF.collectParameters();
        Address addr = ti.getAddressForPointer(params.claimNext());
        collector.bindMetadataParameters(IGF, params);
        if (!IGF.IGM.getOptions().UseTypeLayoutValueHandling ||
            T.hasArchetype() || !canUseValueWitnessForValueOp(*this, T)) {
          ti.destroy(IGF, addr, T, true);
        } else {
          emitDestroyCall(IGF, T, addr);
        }

        IGF.Builder.CreateRet(addr.getAddress());
      },
      true /*setIsNoInline*/);
}

llvm::Constant *IRGenModule::getOrCreateRetainFunction(const TypeInfo &ti,
                                                       SILType t,
                                                       llvm::Type *llvmType,
                                                       Atomicity atomicity) {
  auto *loadableTI = cast<LoadableTypeInfo>(&ti);
  IRGenMangler mangler;
  auto manglingBits =
    getTypeAndGenericSignatureForManglingOutlineFunction(t);
  auto funcName = mangler.mangleOutlinedRetainFunction(manglingBits.first,
                                                       manglingBits.second);
  llvm::Type *argTys[] = {llvmType};
  return getOrCreateHelperFunction(
      funcName, llvmType, argTys,
      [&](IRGenFunction &IGF) {
        auto it = IGF.CurFn->arg_begin();
        Address addr(&*it++, loadableTI->getStorageType(),
                     loadableTI->getFixedAlignment());
        Explosion loaded;
        loadableTI->loadAsTake(IGF, addr, loaded);
        Explosion out;
        loadableTI->copy(IGF, loaded, out, atomicity);
        (void)out.claimAll();
        IGF.Builder.CreateRet(addr.getAddress());
      },
      true /*setIsNoInline*/);
}

llvm::Constant *
IRGenModule::getOrCreateReleaseFunction(const TypeInfo &ti,
                                        SILType t,
                                        llvm::Type *llvmType,
                                        Atomicity atomicity) {
  auto *loadableTI = cast<LoadableTypeInfo>(&ti);
  IRGenMangler mangler;
  auto manglingBits =
    getTypeAndGenericSignatureForManglingOutlineFunction(t);
  auto funcName = mangler.mangleOutlinedReleaseFunction(manglingBits.first,
                                                        manglingBits.second);
  llvm::Type *argTys[] = {llvmType};
  return getOrCreateHelperFunction(
      funcName, llvmType, argTys,
      [&](IRGenFunction &IGF) {
        auto it = IGF.CurFn->arg_begin();
        Address addr(&*it++, loadableTI->getStorageType(),
                     loadableTI->getFixedAlignment());
        Explosion loaded;
        loadableTI->loadAsTake(IGF, addr, loaded);
        loadableTI->consume(IGF, loaded, atomicity, t);
        IGF.Builder.CreateRet(addr.getAddress());
      },
      true /*setIsNoInline*/);
}
