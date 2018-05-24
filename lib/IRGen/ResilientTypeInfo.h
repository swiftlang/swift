//===--- ResilientTypeInfo.h - Resilient-layout types -----------*- C++ -*-===//
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
//  This file defines a class used for implementing non-class-bound
//  archetypes, and resilient structs and enums. Values of these types are
//  opaque and must be manipulated through value witness function calls.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_RESILIENTTYPEINFO_H
#define SWIFT_IRGEN_RESILIENTTYPEINFO_H

#include "NonFixedTypeInfo.h"

namespace swift {
namespace irgen {

/// An abstract CRTP class designed for types whose values are manipulated
/// indirectly through value witness functions.
///
/// We build upon WitnessSizedTypeInfo, adding the additional structure
/// that the opaque value has the same size as the underlying type with
/// no additional metadata, distinguishing this case from other uses of
/// WitnessSizedTypeInfo, which are existentials (these add conformance
/// tables) and fragile enums with generic payloads (these add tag bits).
///
/// This allows us to make use of array value witness functions, and
/// more importantly, to forward extra inhabitant information from the
/// concrete type. This ensures that enums have the correct layout
/// when accessed at different abstraction levels or from different
/// resilience scopes.
///
template <class Impl>
class ResilientTypeInfo : public WitnessSizedTypeInfo<Impl> {
protected:
  ResilientTypeInfo(llvm::Type *type, IsABIAccessible_t abiAccessible)
    : WitnessSizedTypeInfo<Impl>(type, Alignment(1),
                                 IsNotPOD, IsNotBitwiseTakable,
                                 abiAccessible) {}

public:
  void assignWithCopy(IRGenFunction &IGF, Address dest, Address src, SILType T,
                      bool isOutlined) const override {
    emitAssignWithCopyCall(IGF, T, dest, src);
  }

  void assignArrayWithCopyNoAlias(IRGenFunction &IGF, Address dest, Address src,
                                  llvm::Value *count,
                                  SILType T) const override {
    emitAssignArrayWithCopyNoAliasCall(IGF, T, dest, src, count);
  }

  void assignArrayWithCopyFrontToBack(IRGenFunction &IGF, Address dest,
                                      Address src, llvm::Value *count,
                                      SILType T) const override {
    emitAssignArrayWithCopyFrontToBackCall(IGF, T, dest, src, count);
  }

  void assignArrayWithCopyBackToFront(IRGenFunction &IGF, Address dest,
                                      Address src, llvm::Value *count,
                                      SILType T) const override {
    emitAssignArrayWithCopyBackToFrontCall(IGF, T, dest, src, count);
  }

  void assignWithTake(IRGenFunction &IGF, Address dest, Address src, SILType T,
                      bool isOutlined) const override {
    emitAssignWithTakeCall(IGF, T, dest, src);
  }

  void assignArrayWithTake(IRGenFunction &IGF, Address dest, Address src,
                           llvm::Value *count, SILType T) const override {
    emitAssignArrayWithTakeCall(IGF, T, dest, src, count);
  }

  Address initializeBufferWithCopyOfBuffer(IRGenFunction &IGF,
                                   Address dest, Address src,
                                   SILType T) const override {
    auto addr = emitInitializeBufferWithCopyOfBufferCall(IGF, T, dest, src);
    return this->getAddressForPointer(addr);
  }

  void initializeWithCopy(IRGenFunction &IGF, Address dest, Address src,
                          SILType T, bool isOutlined) const override {
    emitInitializeWithCopyCall(IGF, T, dest, src);
  }

  void initializeArrayWithCopy(IRGenFunction &IGF,
                               Address dest, Address src, llvm::Value *count,
                               SILType T) const override {
    emitInitializeArrayWithCopyCall(IGF, T, dest, src, count);
  }

  void initializeWithTake(IRGenFunction &IGF, Address dest, Address src,
                          SILType T, bool isOutlined) const override {
    emitInitializeWithTakeCall(IGF, T, dest, src);
  }

  void initializeArrayWithTakeNoAlias(IRGenFunction &IGF, Address dest,
                                      Address src, llvm::Value *count,
                                      SILType T) const override {
    emitInitializeArrayWithTakeNoAliasCall(IGF, T, dest, src, count);
  }

  void initializeArrayWithTakeFrontToBack(IRGenFunction &IGF,
                                          Address dest, Address src,
                                          llvm::Value *count,
                                          SILType T) const override {
    emitInitializeArrayWithTakeFrontToBackCall(IGF, T, dest, src, count);
  }

  void initializeArrayWithTakeBackToFront(IRGenFunction &IGF,
                                          Address dest, Address src,
                                          llvm::Value *count,
                                          SILType T) const override {
    emitInitializeArrayWithTakeBackToFrontCall(IGF, T, dest, src, count);
  }

  void destroy(IRGenFunction &IGF, Address addr, SILType T,
               bool isOutlined) const override {
    emitDestroyCall(IGF, T, addr);
  }

  void destroyArray(IRGenFunction &IGF, Address addr, llvm::Value *count,
                    SILType T) const override {
    emitDestroyArrayCall(IGF, T, addr, count);
  }

  bool mayHaveExtraInhabitants(IRGenModule &IGM) const override {
    return true;
  }
  llvm::Value *getExtraInhabitantIndex(IRGenFunction &IGF,
                                       Address src,
                                       SILType T) const override {
    return emitGetExtraInhabitantIndexCall(IGF, T, src);
  }
  void storeExtraInhabitant(IRGenFunction &IGF,
                            llvm::Value *index,
                            Address dest,
                            SILType T) const override {
    emitStoreExtraInhabitantCall(IGF, T, index, dest);
  }

  llvm::Value *getEnumTagSinglePayload(IRGenFunction &IGF,
                                       llvm::Value *numEmptyCases,
                                       Address enumAddr,
                                       SILType T) const override {
    return emitGetEnumTagSinglePayloadCall(IGF, T, numEmptyCases, enumAddr);
  }

  void storeEnumTagSinglePayload(IRGenFunction &IGF, llvm::Value *whichCase,
                                 llvm::Value *numEmptyCases, Address enumAddr,
                                 SILType T) const override {
    emitStoreEnumTagSinglePayloadCall(IGF, T, whichCase, numEmptyCases, enumAddr);
  }

  void collectMetadataForOutlining(OutliningMetadataCollector &collector,
                                   SILType T) const override {
    collector.collectTypeMetadataForLayout(T);
  }
};

}
}

#endif
