//===--- NonFixedTypeInfo.h - Non-fixed-layout types ------------*- C++ -*-===//
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
//  This file defines classes that are useful for implementing types
//  that do not have a fixed representation and cannot be laid out
//  statically.
//
//  These classes are useful only for creating TypeInfo
//  implementations; unlike the similarly-named FixedTypeInfo, they
//  do not provide a supplemental API.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_NONFIXEDTYPEINFO_H
#define SWIFT_IRGEN_NONFIXEDTYPEINFO_H

#include "Address.h"
#include "GenOpaque.h"
#include "IndirectTypeInfo.h"

namespace swift {
namespace irgen {

/// Emits the generic implementation for getEnumTagSinglePayload.
llvm::Value *emitGetEnumTagSinglePayload(IRGenFunction &IGF,
                                         llvm::Value *numEmptyCases,
                                         Address enumAddr, SILType T);

/// Emits the generic implementation for storeEnumTagSinglePayload.
void emitStoreEnumTagSinglePayload(IRGenFunction &IGF, llvm::Value *whichCase,
                                   llvm::Value *numEmptyCases, Address enumAddr,
                                   SILType T);

/// An abstract CRTP class designed for types whose storage size,
/// alignment, and stride need to be fetched from the value witness
/// table for the type.
template <class Impl>
class WitnessSizedTypeInfo : public IndirectTypeInfo<Impl, TypeInfo> {
private:
  using super = IndirectTypeInfo<Impl, TypeInfo>;

protected:
  const Impl &asImpl() const { return static_cast<const Impl &>(*this); }

  WitnessSizedTypeInfo(llvm::Type *type, Alignment align, IsPOD_t pod,
                       IsBitwiseTakable_t bt, IsABIAccessible_t abi)
    : super(type, align, pod, bt, IsNotFixedSize, abi,
            SpecialTypeInfoKind::None) {}

  /// Bit-cast the given pointer to the right type and assume it as an
  /// address of this type.
  Address getAsBitCastAddress(IRGenFunction &IGF, llvm::Value *addr) const {
    addr = IGF.Builder.CreateBitCast(addr,
                                     this->getStorageType()->getPointerTo());
    return this->getAddressForPointer(addr);
  }

public:
  // This is useful for metaprogramming.
  static bool isFixed() { return false; }

  StackAddress allocateStack(IRGenFunction &IGF, SILType T,
                             const llvm::Twine &name) const override {
    // Allocate memory on the stack.
    auto alloca = IGF.emitDynamicAlloca(T, name);
    IGF.Builder.CreateLifetimeStart(alloca.getAddressPointer());
    return alloca.withAddress(
             getAsBitCastAddress(IGF, alloca.getAddressPointer()));
  }

  void deallocateStack(IRGenFunction &IGF, StackAddress stackAddress,
                       SILType T) const override {
    IGF.Builder.CreateLifetimeEnd(stackAddress.getAddress().getAddress());
    IGF.emitDeallocateDynamicAlloca(stackAddress);
  }

  void destroyStack(IRGenFunction &IGF, StackAddress stackAddress, SILType T,
                    bool isOutlined) const override {
    emitDestroyCall(IGF, T, stackAddress.getAddress());
    deallocateStack(IGF, stackAddress, T);
  }

  llvm::Value *getValueWitnessTable(IRGenFunction &IGF, SILType T) const {
    return IGF.emitValueWitnessTableRef(T);
  }

  llvm::Value *getSize(IRGenFunction &IGF, SILType T) const override {
    return emitLoadOfSize(IGF, T);
  }

  llvm::Value *getAlignmentMask(IRGenFunction &IGF, SILType T) const override {
    return emitLoadOfAlignmentMask(IGF, T);
  }

  llvm::Value *getStride(IRGenFunction &IGF, SILType T) const override {
    return emitLoadOfStride(IGF, T);
  }

  llvm::Value *getIsPOD(IRGenFunction &IGF, SILType T) const override {
    return emitLoadOfIsPOD(IGF, T);
  }

  llvm::Value *getIsBitwiseTakable(IRGenFunction &IGF, SILType T) const override {
    return emitLoadOfIsBitwiseTakable(IGF, T);
  }

  llvm::Value *isDynamicallyPackedInline(IRGenFunction &IGF,
                                         SILType T) const override {
    return emitLoadOfIsInline(IGF, T);
  }

  /// FIXME: Dynamic extra inhabitant lookup.
  bool mayHaveExtraInhabitants(IRGenModule &) const override { return false; }
  llvm::Value *getExtraInhabitantIndex(IRGenFunction &IGF,
                                       Address src, SILType T,
                                       bool isOutlined) const override {
    llvm_unreachable("dynamic extra inhabitants not supported");
  }
  void storeExtraInhabitant(IRGenFunction &IGF,
                            llvm::Value *index,
                            Address dest, SILType T,
                            bool isOutlined) const override {
    llvm_unreachable("dynamic extra inhabitants not supported");
  }

  llvm::Constant *getStaticSize(IRGenModule &IGM) const override {
    return nullptr;
  }
  llvm::Constant *getStaticAlignmentMask(IRGenModule &IGM) const override {
    return nullptr;
  }
  llvm::Constant *getStaticStride(IRGenModule &IGM) const override {
    return nullptr;
  }

  llvm::Value *getEnumTagSinglePayload(IRGenFunction &IGF,
                                       llvm::Value *numEmptyCases,
                                       Address enumAddr,
                                       SILType T) const override {
    return emitGetEnumTagSinglePayload(IGF, numEmptyCases, enumAddr, T);
  }

  void storeEnumTagSinglePayload(IRGenFunction &IGF, llvm::Value *whichCase,
                                 llvm::Value *numEmptyCases, Address enumAddr,
                                 SILType T) const override {
    emitStoreEnumTagSinglePayload(IGF, whichCase, numEmptyCases, enumAddr, T);
  }
};
}
}

#endif
