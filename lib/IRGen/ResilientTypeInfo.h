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
  ResilientTypeInfo(llvm::Type *type)
    : WitnessSizedTypeInfo<Impl>(type, Alignment(1),
                                 IsNotPOD, IsNotBitwiseTakable) {}

public:
  void assignWithCopy(IRGenFunction &IGF, Address dest, Address src,
                      SILType T) const override {
    emitAssignWithCopyCall(IGF, T, dest, src);
  }

  void assignWithTake(IRGenFunction &IGF, Address dest, Address src,
                      SILType T) const override {
    emitAssignWithTakeCall(IGF, T, dest, src);
  }

  Address allocateBuffer(IRGenFunction &IGF, Address buffer,
                         SILType T) const override {
    auto addr = emitAllocateBufferCall(IGF, T, buffer);
    return this->getAddressForPointer(addr);
  }

  Address projectBuffer(IRGenFunction &IGF, Address buffer,
                        SILType T) const override {
    auto addr = emitProjectBufferCall(IGF, T, buffer);
    return this->getAddressForPointer(addr);
  }

  void destroyBuffer(IRGenFunction &IGF, Address buffer,
                     SILType T) const override {
    emitDestroyBufferCall(IGF, T, buffer);
  }

  void deallocateBuffer(IRGenFunction &IGF, Address buffer,
                        SILType T) const override {
    emitDeallocateBufferCall(IGF, T, buffer);
  }

  Address initializeBufferWithCopyOfBuffer(IRGenFunction &IGF,
                                   Address dest, Address src,
                                   SILType T) const override {
    auto addr = emitInitializeBufferWithCopyOfBufferCall(IGF, T, dest, src);
    return this->getAddressForPointer(addr);
  }

  Address initializeBufferWithTakeOfBuffer(IRGenFunction &IGF,
                                   Address dest, Address src,
                                   SILType T) const override {
    auto addr = emitInitializeBufferWithTakeOfBufferCall(IGF, T, dest, src);
    return this->getAddressForPointer(addr);
  }

  Address initializeBufferWithCopy(IRGenFunction &IGF,
                                   Address dest, Address src,
                                   SILType T) const override {
    auto addr = emitInitializeBufferWithCopyCall(IGF, T, dest, src);
    return this->getAddressForPointer(addr);
  }

  Address initializeBufferWithTake(IRGenFunction &IGF,
                                   Address dest, Address src,
                                   SILType T) const override {
    auto addr = emitInitializeBufferWithTakeCall(IGF, T, dest, src);
    return this->getAddressForPointer(addr);
  }

  void initializeWithCopy(IRGenFunction &IGF,
                        Address dest, Address src, SILType T) const override {
    emitInitializeWithCopyCall(IGF, T, dest, src);
  }

  void initializeArrayWithCopy(IRGenFunction &IGF,
                               Address dest, Address src, llvm::Value *count,
                               SILType T) const override {
    emitInitializeArrayWithCopyCall(IGF, T, dest, src, count);
  }

  void initializeWithTake(IRGenFunction &IGF,
                        Address dest, Address src, SILType T) const override {
    emitInitializeWithTakeCall(IGF, T, dest, src);
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

  void destroy(IRGenFunction &IGF, Address addr, SILType T) const override {
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

  void initializeMetadata(IRGenFunction &IGF,
                          llvm::Value *metadata,
                          llvm::Value *vwtable,
                          SILType T) const override {
    // Resilient value types and archetypes always refer to an existing type.
    // A witness table should never be independently initialized for one.
    llvm_unreachable("initializing value witness table for opaque type?!");
  }

};

}
}

#endif
