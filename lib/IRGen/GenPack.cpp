//===--- GenPack.cpp - Swift IR Generation For Variadic Generics ----------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  This file implements IR generation for type and value packs in Swift.
//
//===----------------------------------------------------------------------===//

#include "GenPack.h"
#include "swift/AST/Decl.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/IRGenOptions.h"
#include "swift/AST/Types.h"
#include "swift/SIL/SILModule.h"
#include "swift/SIL/SILType.h"
#include "llvm/IR/DerivedTypes.h"

#include "GenType.h"
#include "IRGenFunction.h"
#include "IRGenModule.h"
#include "MetadataRequest.h"

using namespace swift;
using namespace irgen;

static void accumulateSum(IRGenFunction &IGF, llvm::Value *&result,
                          llvm::Value *value) {
  if (result == nullptr) {
    result = value;
    return;
  }

  result = IGF.Builder.CreateAdd(result, value);
}

using PackExplosionCallback = void (CanType eltTy,
                                    unsigned scalarIndex,
                                    llvm::Value *dynamicIndex,
                                    llvm::Value *dynamicLength);

static std::pair<unsigned, llvm::Value *>
visitPackExplosion(IRGenFunction &IGF, CanPackType type,
                   llvm::function_ref<PackExplosionCallback> callback) {
  llvm::Value *result = nullptr;

  // If shape(T) == t and shape(U) == u, the shape expression for a pack
  // {T..., Int, T..., U..., String} becomes 't + t + u + 2'.
  unsigned scalarElements = 0;

  for (auto elt : type.getElementTypes()) {
    if (auto expansionType = dyn_cast<PackExpansionType>(elt)) {
      auto reducedShape = expansionType.getCountType();
      auto *eltCount = IGF.emitPackShapeExpression(reducedShape);
      callback(elt, scalarElements, result, eltCount);
      accumulateSum(IGF, result, eltCount);
      continue;
    }

    callback(elt, scalarElements, result, nullptr);
    ++scalarElements;
  }

  return std::make_pair(scalarElements, result);
}

llvm::Value *IRGenFunction::emitPackShapeExpression(CanType type) {

  type = type->getReducedShape()->getCanonicalType();

  auto kind = LocalTypeDataKind::forPackShapeExpression();

  llvm::Value *result = tryGetLocalTypeData(type, kind);
  if (result != nullptr)
    return result;

  auto pair = visitPackExplosion(
      *this, cast<PackType>(type),
      [&](CanType, unsigned, llvm::Value *, llvm::Value *) {});

  if (pair.first > 0) {
    auto *constant = llvm::ConstantInt::get(IGM.SizeTy, pair.first);
    accumulateSum(*this, pair.second, constant);
  }

  setScopedLocalTypeData(type, kind, pair.second);
  return pair.second;
}

MetadataResponse
irgen::emitPackArchetypeMetadataRef(IRGenFunction &IGF,
                                    CanPackArchetypeType type,
                                    DynamicMetadataRequest request) {
  if (auto result = IGF.tryGetLocalTypeMetadata(type, request))
    return result;

  auto packType = type->getSingletonPackType();
  auto response = emitTypeMetadataPackRef(IGF, packType, request);

  IGF.setScopedLocalTypeMetadata(type, response);
  return response;
}

static Address emitFixedSizeMetadataPackRef(IRGenFunction &IGF,
                                            CanPackType packType,
                                            DynamicMetadataRequest request) {
  assert(!packType->containsPackExpansionType());

  unsigned elementCount = packType->getNumElements();
  auto allocType = llvm::ArrayType::get(
      IGF.IGM.TypeMetadataPtrTy, elementCount);

  auto pack = IGF.createAlloca(allocType, IGF.IGM.getPointerAlignment());
  IGF.Builder.CreateLifetimeStart(pack,
                              IGF.IGM.getPointerSize() * elementCount);

  for (unsigned i : indices(packType->getElementTypes())) {
    Address slot = IGF.Builder.CreateStructGEP(
        pack, i, IGF.IGM.getPointerSize());

    auto metadata = IGF.emitTypeMetadataRef(
        packType.getElementType(i), request).getMetadata();
    IGF.Builder.CreateStore(metadata, slot);
  }

  pack = IGF.Builder.CreateConstArrayGEP(
      pack, 0, IGF.IGM.getPointerSize());

  return pack;
}

static void emitPackExpansionType(IRGenFunction &IGF,
                                  Address pack,
                                  CanPackExpansionType expansionTy,
                                  llvm::Value *dynamicIndex,
                                  llvm::Value *dynamicLength,
                                  DynamicMetadataRequest request) {
  auto *prev = IGF.Builder.GetInsertBlock();
  auto *check = IGF.createBasicBlock("pack-expansion-check");
  auto *loop = IGF.createBasicBlock("pack-expansion-loop");
  auto *rest = IGF.createBasicBlock("pack-expansion-rest");

  IGF.Builder.CreateBr(check);
  IGF.Builder.emitBlock(check);

  // An index into the source metadata pack.
  auto *phi = IGF.Builder.CreatePHI(IGF.IGM.SizeTy, 2);
  phi->addIncoming(llvm::ConstantInt::get(IGF.IGM.SizeTy, 0), prev);

  // If we reach the end, jump to the continuation block.
  auto *cond = IGF.Builder.CreateICmpULT(phi, dynamicLength);
  IGF.Builder.CreateCondBr(cond, loop, rest);

  IGF.Builder.emitBlock(loop);

  auto patternTy = expansionTy.getPatternType();

  // Find all the pack archetypes appearing in the pattern type.
  SmallVector<Type, 2> patternPacks;
  patternTy->getTypeParameterPacks(patternPacks);

  // Get the outer generic signature and environment.
  auto *genericEnv = cast<PackArchetypeType>(expansionTy.getCountType())
      ->getGenericEnvironment();
  auto subMap = genericEnv->getForwardingSubstitutionMap();

  auto genericSig = genericEnv->getGenericSignature().getCanonicalSignature();

  // Create an opened element signature and environment.
  auto elementSig = IGF.IGM.Context.getOpenedElementSignature(genericSig);
  auto *elementEnv = GenericEnvironment::forOpenedElement(
      elementSig, UUID::fromTime(), subMap);

  // Open each pack archetype.
  for (auto patternPackType : patternPacks) {
    // Get the metadata for the pack archetype.
    auto patternPackArchetype = cast<PackArchetypeType>(
        patternPackType->getCanonicalType());
    auto patternPack = IGF.emitTypeMetadataRef(patternPackArchetype, request)
        .getMetadata();

    patternPack = IGF.Builder.CreatePointerCast(
        patternPack, IGF.IGM.TypeMetadataPtrPtrTy);

    Address patternPackAddress(patternPack, IGF.IGM.TypeMetadataPtrTy,
                               IGF.IGM.getPointerAlignment());

    // Load the metadata pack element from the current source index.
    Address fromPtr(
      IGF.Builder.CreateInBoundsGEP(patternPackAddress.getElementType(),
                                    patternPackAddress.getAddress(),
                                    phi),
      patternPackAddress.getElementType(),
      patternPackAddress.getAlignment());
    auto metadata = IGF.Builder.CreateLoad(fromPtr);

    // Bind the metadata pack element to the element archetype.
    auto elementArchetype =
      elementEnv->mapPackTypeIntoElementContext(
          patternPackArchetype->getInterfaceType());

    IGF.setScopedLocalTypeMetadata(
        CanType(elementArchetype),
        MetadataResponse::forComplete(metadata));
  }

  // Replace pack archetypes with element archetypes in the pattern type.
  auto instantiatedPatternTy = elementEnv->mapPackTypeIntoElementContext(
      patternTy->mapTypeOutOfContext())->getCanonicalType();

  // Emit the element metadata.
  auto element = IGF.emitTypeMetadataRef(instantiatedPatternTy, request)
      .getMetadata();

  // Store the element metadata into to the current destination index.
  auto *eltIndex = IGF.Builder.CreateAdd(dynamicIndex, phi);
  Address eltPtr(
      IGF.Builder.CreateInBoundsGEP(pack.getElementType(),
                                    pack.getAddress(),
                                    eltIndex),
      pack.getElementType(),
      pack.getAlignment());

  IGF.Builder.CreateStore(element, eltPtr);

  // Increment our counter.
  auto *next = IGF.Builder.CreateAdd(phi,
                                     llvm::ConstantInt::get(IGF.IGM.SizeTy, 1));

  phi->addIncoming(next, loop);

  // Repeat the loop.
  IGF.Builder.CreateBr(check);

  // Fall through.
  IGF.Builder.emitBlock(rest);
}

StackAddress
irgen::emitTypeMetadataPack(IRGenFunction &IGF,
                            CanPackType packType,
                            DynamicMetadataRequest request) {
  auto *shape = IGF.emitPackShapeExpression(packType);

  if (auto *constantInt = dyn_cast<llvm::ConstantInt>(shape)) {
    assert(packType->getNumElements() == constantInt->getValue());
    return StackAddress(emitFixedSizeMetadataPackRef(IGF, packType, request));
  }

  assert(packType->containsPackExpansionType());
  auto pack = IGF.emitDynamicAlloca(IGF.IGM.TypeMetadataPtrTy, shape,
                                    IGF.IGM.getPointerAlignment(),
                                    /*allowTaskAlloc=*/true);

  auto visitFn =
    [&](CanType eltTy, unsigned staticIndex,
        llvm::Value *dynamicIndex,
        llvm::Value *dynamicLength) {
      if (staticIndex != 0 || dynamicIndex == nullptr) {
        auto *constant = llvm::ConstantInt::get(IGF.IGM.SizeTy, staticIndex);
        accumulateSum(IGF, dynamicIndex, constant);
      }

      if (auto expansionTy = dyn_cast<PackExpansionType>(eltTy)) {
        emitPackExpansionType(IGF, pack.getAddress(), expansionTy,
                              dynamicIndex, dynamicLength, request);
      } else {
        Address eltPtr(
          IGF.Builder.CreateInBoundsGEP(pack.getAddress().getElementType(),
                                        pack.getAddressPointer(),
                                        dynamicIndex),
          pack.getAddress().getElementType(),
          pack.getAlignment());

        auto metadata = IGF.emitTypeMetadataRef(eltTy, request).getMetadata();
        IGF.Builder.CreateStore(metadata, eltPtr);
      }
    };

  visitPackExplosion(IGF, packType, visitFn);

  return pack;
}

MetadataResponse
irgen::emitTypeMetadataPackRef(IRGenFunction &IGF,
                               CanPackType packType,
                               DynamicMetadataRequest request) {
  if (auto result = IGF.tryGetLocalTypeMetadata(packType, request))
    return result;

  if (packType->getNumElements() == 1 &&
      isa<PackExpansionType>(packType.getElementType(0))) {
    if (auto packArchetypeType = dyn_cast<PackArchetypeType>(
            cast<PackExpansionType>(packType.getElementType(0))
                .getPatternType())) {
      if (auto result = IGF.tryGetLocalTypeMetadata(packArchetypeType, request))
        return result;
    }
  }

  auto pack = emitTypeMetadataPack(IGF, packType, request);
  auto *metadata = IGF.Builder.CreateConstArrayGEP(
      pack.getAddress(), 0, IGF.IGM.getPointerSize()).getAddress();

  auto response = MetadataResponse::forComplete(metadata);
  IGF.setScopedLocalTypeMetadata(packType, response);

  return response;
}

void irgen::cleanupTypeMetadataPack(IRGenFunction &IGF,
                                    StackAddress pack,
                                    Optional<unsigned> elementCount) {
  if (pack.getExtraInfo()) {
    IGF.emitDeallocateDynamicAlloca(pack);
  } else {
    IGF.Builder.CreateLifetimeEnd(pack.getAddress(),
                                  IGF.IGM.getPointerSize() * (*elementCount));
  }
}