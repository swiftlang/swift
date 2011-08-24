//===--- GenOneOf.cpp - Swift IR Generation For 'oneof' Types -------------===//
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
//  This file implements IR generation for algebraic data types (ADTs,
//  or 'oneof' types) in Swift.  This includes creating the IR type as
//  well as emitting the basic access operations.
//
//  The current scheme is that all such types with are represented
//  with an initial word indicating the variant, followed by a union
//  of all the possibilities.  This is obviously completely acceptable
//  to everyone and will not benefit from further refinement.
//
//  As a completely unimportant premature optimization, we do emit
//  types with only a single variant as simple structs wrapping that
//  variant.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/Types.h"
#include "swift/AST/Decl.h"
#include "llvm/DerivedTypes.h"

#include "GenType.h"
#include "IRGenFunction.h"
#include "IRGenModule.h"
#include "LValue.h"
#include "RValue.h"

using namespace swift;
using namespace irgen;

namespace {
  class OneofTypeInfo : public TypeInfo {
  public:
    OneofTypeInfo(llvm::Type *T, Size S, Alignment A) : TypeInfo(T, S, A) {}

    RValueSchema getSchema() const {
      llvm::StructType *Struct = cast<llvm::StructType>(getStorageType());
      return RValueSchema::forAggregate(Struct, StorageAlignment);
    }

    RValue load(IRGenFunction &IGF, const LValue &LV) const {
      // FIXME
      return RValue();
    }

    void store(IRGenFunction &CGF, const RValue &RV, const LValue &LV) const {
      // FIXME
    }
  };

  /// A TypeInfo implementation for singleton oneofs.
  class SingletonOneofTypeInfo : public TypeInfo {
    static LValue getSingletonLValue(IRGenFunction &IGF, const LValue &LV) {
      llvm::Value *SingletonAddr =
        IGF.Builder.CreateStructGEP(LV.getAddress(), 0);
      return LValue::forAddress(SingletonAddr, LV.getAlignment());
    }

  public:
    /// The type info of the singleton member, or null if it carries no data.
    const TypeInfo *Singleton;

    SingletonOneofTypeInfo(llvm::Type *T, Size S, Alignment A)
      : TypeInfo(T, S, A), Singleton(nullptr) {}

    RValueSchema getSchema() const {
      assert(isComplete());
      if (!Singleton) return RValueSchema::forScalars();
      return Singleton->getSchema();
    }

    RValue load(IRGenFunction &IGF, const LValue &LV) const {
      assert(isComplete());
      if (!Singleton) return RValue::forScalars();
      return Singleton->load(IGF, getSingletonLValue(IGF, LV));
    }

    void store(IRGenFunction &IGF, const RValue &RV, const LValue &LV) const {
      assert(isComplete());
      if (!Singleton) return;
      Singleton->store(IGF, RV, getSingletonLValue(IGF, LV));
    }
  };
}

const TypeInfo *
TypeConverter::convertOneOfType(IRGenModule &IGM, OneOfType *T) {
  // Every oneof type is a unique type.
  llvm::StructType *Converted
    = llvm::StructType::create(IGM.getLLVMContext(), "oneof");

  // We don't need a discriminator if this is a singleton ADT.
  if (T->hasSingleElement()) {
    SingletonOneofTypeInfo *TInfo =
      new SingletonOneofTypeInfo(Converted, Size(0), Alignment(0));
    assert(!IGM.Types.Converted.count(T));
    IGM.Types.Converted.insert(std::make_pair(T, TInfo));

    Type Ty = T->getElement(0)->ArgumentType;

    llvm::Type *StorageType;
    if (Ty.isNull()) {
      StorageType = IGM.Int8Ty;
      TInfo->StorageAlignment = Alignment(1);
      TInfo->Singleton = nullptr;
    } else {
      const TypeInfo &EltInfo = getFragileTypeInfo(IGM, Ty);
      assert(EltInfo.isComplete());
      StorageType = EltInfo.StorageType;
      TInfo->StorageSize = EltInfo.StorageSize;
      TInfo->StorageAlignment = EltInfo.StorageAlignment;
      TInfo->Singleton = &EltInfo;
    }

    llvm::Type *Body[] = { StorageType };
    Converted->setBody(Body);

    return TInfo;
  }

  OneofTypeInfo *TInfo = new OneofTypeInfo(Converted, Size(0), Alignment(0));

  // They can be recursive -- not structurally, but by anything
  // indirected.
  assert(!IGM.Types.Converted.count(T));
  IGM.Types.Converted.insert(std::make_pair(T, TInfo));

  // Otherwise, we need a discriminator.
  llvm::Type *DiscriminatorType;
  Size DiscriminatorSize;
  if (T->Elements.size() <= (1 << 8)) {
    DiscriminatorType = IGM.Int8Ty;
    DiscriminatorSize = Size(1);
  } else if (T->Elements.size() <= (1 << 16)) {
    DiscriminatorType = IGM.Int16Ty;
    DiscriminatorSize = Size(2);
  } else {
    DiscriminatorType = IGM.Int32Ty;
    DiscriminatorSize = Size(4);
  }

  SmallVector<llvm::Type*, 2> Body;
  Body.push_back(DiscriminatorType);

  Size PayloadSize = Size(0);
  Alignment StorageAlignment = Alignment(1);

  // Figure out how much storage we need for the union.
  for (unsigned I = 0, E = T->Elements.size(); I != E; ++I) {
    // Ignore variants that carry no data.
    Type Ty = T->getElement(I)->ArgumentType;
    if (Ty.isNull()) continue;

    // Compute layout for the type, and ignore variants with
    // zero-size data.
    const TypeInfo &EltInfo = getFragileTypeInfo(IGM, Ty);
    assert(EltInfo.isComplete());
    if (EltInfo.StorageSize.isZero()) continue;

    // The required payload size is the amount of padding needed to
    // get up to the element's alignment, plus the actual size.
    Size EltPayloadSize = EltInfo.StorageSize;
    if (EltInfo.StorageAlignment.getValue() > DiscriminatorSize.getValue())
      EltPayloadSize += Size(EltInfo.StorageAlignment.getValue()
                               - DiscriminatorSize.getValue());

    PayloadSize = std::max(PayloadSize, EltPayloadSize);
    StorageAlignment = std::max(StorageAlignment, EltInfo.StorageAlignment);
  }

  // If there's any payload at all, add in the payload array.
  if (PayloadSize) {
    Body.push_back(llvm::ArrayType::get(IGM.Int8Ty, PayloadSize.getValue()));
  }

  Size StorageSize = DiscriminatorSize + PayloadSize;
  // Should we round TypeSize up to a multiple of the alignment?

  Converted->setBody(Body);
  TInfo->StorageSize = StorageSize;
  TInfo->StorageAlignment = StorageAlignment;
  return TInfo;
}
