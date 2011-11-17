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
#include "swift/AST/Expr.h"
#include "llvm/DerivedTypes.h"

#include "GenType.h"
#include "IRGenFunction.h"
#include "IRGenModule.h"
#include "LValue.h"
#include "RValue.h"

using namespace swift;
using namespace irgen;

namespace {
  /// An abstract base class for TypeInfo implementations of oneof types.
  class OneofTypeInfo : public TypeInfo {
  public:
    OneofTypeInfo(llvm::StructType *T, Size S, Alignment A)
      : TypeInfo(T, S, A) {}

    llvm::StructType *getStorageType() const {
      return cast<llvm::StructType>(TypeInfo::getStorageType());
    }

    llvm::IntegerType *getDiscriminatorType() const {
      llvm::StructType *Struct = getStorageType();
      return cast<llvm::IntegerType>(Struct->getElementType(0));
    }
  };

  /// A TypeInfo implementation which uses an aggregate.
  class AggregateOneofTypeInfo : public OneofTypeInfo {
  public:
    AggregateOneofTypeInfo(llvm::StructType *T, Size S, Alignment A)
      : OneofTypeInfo(T, S, A) {}

    RValueSchema getSchema() const {
      return RValueSchema::forAggregate(getStorageType(), StorageAlignment);
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
  class SingletonOneofTypeInfo : public OneofTypeInfo {
    static LValue getSingletonLValue(IRGenFunction &IGF, const LValue &LV) {
      llvm::Value *SingletonAddr =
        IGF.Builder.CreateStructGEP(LV.getAddress(), 0);
      return LValue::forAddress(SingletonAddr, LV.getAlignment());
    }

  public:
    /// The type info of the singleton member, or null if it carries no data.
    const TypeInfo *Singleton;

    SingletonOneofTypeInfo(llvm::StructType *T, Size S, Alignment A)
      : OneofTypeInfo(T, S, A), Singleton(nullptr) {}

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

  /// A TypeInfo implementation for oneofs with no payload.
  class EnumTypeInfo : public OneofTypeInfo {
  public:
    EnumTypeInfo(llvm::StructType *T, Size S, Alignment A)
      : OneofTypeInfo(T, S, A) {}

    RValueSchema getSchema() const {
      assert(isComplete());
      return RValueSchema::forScalars(getDiscriminatorType());
    }

    RValue load(IRGenFunction &IGF, const LValue &LV) const {
      llvm::Value *OneofAddr = LV.getAddress();
      llvm::Value *Addr = IGF.Builder.CreateStructGEP(OneofAddr, 0);
      llvm::Value *V = IGF.Builder.CreateLoad(Addr,
                                              LV.getAlignment(),
                                              OneofAddr->getName() + ".load");
      return RValue::forScalars(V);
    }

    void store(IRGenFunction &IGF, const RValue &RV, const LValue &LV) const {
      assert(RV.isScalar(1));
      llvm::Value *OneofAddr = LV.getAddress();
      llvm::Value *Addr = IGF.Builder.CreateStructGEP(OneofAddr, 0);
      IGF.Builder.CreateStore(RV.getScalars()[0], Addr, LV.getAlignment());
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

  // Otherwise, we need a discriminator.
  llvm::Type *DiscriminatorType;
  Size DiscriminatorSize;
  if (T->Elements.size() == 2) {
    DiscriminatorType = IGM.Int1Ty;
    DiscriminatorSize = Size(1);
  } else if (T->Elements.size() <= (1 << 8)) {
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

  Size StorageSize = DiscriminatorSize + PayloadSize;

  OneofTypeInfo *TInfo;

  // If there's no payload at all, use the enum TypeInfo.
  if (!PayloadSize) {
    TInfo = new EnumTypeInfo(Converted, StorageSize, StorageAlignment);

  // Otherwise, add the payload array to the storage.
  } else {
    Body.push_back(llvm::ArrayType::get(IGM.Int8Ty, PayloadSize.getValue()));

    // TODO: don't always use an aggregate representation.
    TInfo = new AggregateOneofTypeInfo(Converted, StorageSize,
                                       StorageAlignment);
  }

  Converted->setBody(Body);
  return TInfo;
}

LValue IRGenFunction::emitLookThroughOneofLValue(LookThroughOneofExpr *E) {
  LValue oneofLV = emitLValue(E->getSubExpr());
  llvm::Value *addr = Builder.CreateStructGEP(oneofLV.getAddress(), 0);
  return LValue::forAddress(addr, oneofLV.getAlignment());
}

RValue IRGenFunction::emitLookThroughOneofRValue(LookThroughOneofExpr *E) {
  RValue oneofRV = emitRValue(E->getSubExpr());
  if (oneofRV.isScalar()) return oneofRV;

  llvm::Value *addr = Builder.CreateStructGEP(oneofRV.getAggregateAddress(), 0);
  return RValue::forAggregate(addr);
}
