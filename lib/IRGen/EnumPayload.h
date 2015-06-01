//===--- EnumPayload.h - Payload management for 'enum' Types ------* C++ *-===//
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

#ifndef SWIFT_IRGEN_ENUMPAYLOAD_H
#define SWIFT_IRGEN_ENUMPAYLOAD_H

#include "IRGenModule.h"
#include "Explosion.h"
#include "TypeInfo.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/ADT/Fixnum.h"
#include "llvm/ADT/PointerUnion.h"
#include <utility>

namespace swift {
namespace irgen {
  
/// A description of how to represent an enum payload as a value.
/// A payload can either use a generic word-chunked representation, or attempt
/// to follow the explosion schema of one of its payload types.
class EnumPayloadSchema {
  const llvm::PointerUnion<ExplosionSchema *, llvm::Fixnum<31>> Value;

public:
  EnumPayloadSchema() : Value((ExplosionSchema *)nullptr) {}

  explicit operator bool() {
    return Value.getOpaqueValue() != nullptr;
  }

  explicit EnumPayloadSchema(unsigned bits)
    : Value(llvm::Fixnum<31>(bits)) {}

  EnumPayloadSchema(ExplosionSchema &s)
    : Value(&s) {}

  static EnumPayloadSchema withBitSize(unsigned bits) {
    return EnumPayloadSchema(bits);
  }
  
  ExplosionSchema *getSchema() const {
    return Value.dyn_cast<ExplosionSchema*>();
  }
  
  /// Invoke a functor for each element type in the schema.
  template<typename TypeFn /* void(llvm::Type *schemaType) */>
  void forEachType(IRGenModule &IGM, TypeFn &&fn) const {
    // Follow an explosion schema if we have one.
    if (auto *explosion = Value.dyn_cast<ExplosionSchema *>()) {
      for (auto &element : *explosion) {
        auto type = element.getScalarType();
        assert(IGM.DataLayout.getTypeSizeInBits(type)
                 == IGM.DataLayout.getTypeAllocSizeInBits(type)
               && "enum payload schema elements should use full alloc size");
        fn(element.getScalarType());
      }
      return;
    }
    
    // Otherwise, chunk into pointer-sized integer values by default.
    unsigned bitSize = Value.get<llvm::Fixnum<31>>();
    unsigned pointerSize = IGM.getPointerSize().getValueInBits();
    
    while (bitSize >= pointerSize) {
      fn(IGM.SizeTy);
      bitSize -= pointerSize;
    }
    if (bitSize > 0)
      fn(llvm::IntegerType::get(IGM.getLLVMContext(), bitSize));
  }
};

/// Is a switch default destination unreachable?
enum IsUnreachable_t: bool {
  IsNotUnreachable = false,
  IsUnreachable = true,
};

using SwitchDefaultDest
  = llvm::PointerIntPair<llvm::BasicBlock*, 1, IsUnreachable_t>;

/// An enum payload value. The payload is represented as an explosion of
/// integers and pointers that together represent the bit pattern of
/// the payload.
class EnumPayload {
public:
  /// A value, or the type of a zero value in the payload.
  using LazyValue = llvm::PointerUnion<llvm::Value *, llvm::Type *>;
  
  mutable SmallVector<LazyValue, 2> PayloadValues;
  mutable llvm::Type *StorageType = nullptr;
  
  EnumPayload() = default;

  /// Generate a "zero" enum payload.
  static EnumPayload zero(IRGenModule &IGM,
                          EnumPayloadSchema schema);

  /// Generate an enum payload containing the given bit pattern.
  static EnumPayload fromBitPattern(IRGenModule &IGM,
                                    APInt bitPattern,
                                    EnumPayloadSchema schema);
  
  /// Insert a value into the enum payload.
  ///
  /// The current payload value at the given offset is assumed to be zero.
  void insertValue(IRGenFunction &IGF,
                   llvm::Value *value, unsigned bitOffset);
  
  /// Extract a value from the enum payload.
  llvm::Value *extractValue(IRGenFunction &IGF,
                            llvm::Type *type, unsigned bitOffset) const;
  
  /// Take an enum payload out of an explosion.
  static EnumPayload fromExplosion(IRGenModule &IGM,
                                   Explosion &in,
                                   EnumPayloadSchema schema);
  
  /// Add the payload to an explosion.
  void explode(IRGenModule &IGM, Explosion &out) const;
  
  /// Pack into another enum payload.
  void packIntoEnumPayload(IRGenFunction &IGF,
                           EnumPayload &dest,
                           unsigned bitOffset) const;
  
  /// Unpack from another enum payload.
  static EnumPayload unpackFromEnumPayload(IRGenFunction &IGF,
                                           const EnumPayload &src,
                                           unsigned bitOffset,
                                           EnumPayloadSchema schema);
  
  /// Load an enum payload from memory.
  static EnumPayload load(IRGenFunction &IGF, Address address,
                          EnumPayloadSchema schema);
  
  /// Store an enum payload to memory.
  void store(IRGenFunction &IGF, Address address) const;

  /// Emit a switch over specific bit patterns for the payload.
  /// The value will be tested as if AND-ed against the given mask.
  void emitSwitch(IRGenFunction &IGF,
                  APInt mask,
                  ArrayRef<std::pair<APInt, llvm::BasicBlock*>> cases,
                  SwitchDefaultDest dflt) const;
  
  /// Emit an equality comparison operation that payload & mask == value.
  llvm::Value *emitCompare(IRGenFunction &IGF,
                           APInt mask,
                           APInt value) const;
  
  /// Apply an AND mask to the payload.
  void emitApplyAndMask(IRGenFunction &IGF, APInt mask);
  
  /// Apply an OR mask to the payload.
  void emitApplyOrMask(IRGenFunction &IGF, APInt mask);
  
  /// Gather bits from an enum payload based on a spare bit mask.
  llvm::Value *emitGatherSpareBits(IRGenFunction &IGF,
                                   const SpareBitVector &spareBits,
                                   unsigned firstBitOffset,
                                   unsigned bitWidth) const;
};
  
}
}

#endif
