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
#include "IRGenModule.h"
#include "LValue.h"
#include "RValue.h"

using namespace swift;
using namespace irgen;

namespace {
  class OneOfTypeInfo : public TypeInfo {
  public:
    OneOfTypeInfo(llvm::Type *T, Size S, Alignment A) : TypeInfo(T, S, A) {}

    RValue load(IRGenFunction &IGF, const LValue &LV) const {
      // FIXME
      return RValue();
    }

    void store(IRGenFunction &CGF, const RValue &RV, const LValue &LV) const {
      // FIXME
    }
  };
}

const TypeInfo *
TypeConverter::convertOneOfType(IRGenModule &IGM, OneOfType *T) {
  // Every oneof type is a unique type.
  llvm::StructType *Converted
    = llvm::StructType::create(IGM.getLLVMContext(), "oneof");

  OneOfTypeInfo *TInfo = new OneOfTypeInfo(Converted, Size(0), Alignment(0));

  // They can be recursive -- not structurally, but by anything
  // indirected.
  assert(!IGM.Types.Converted.count(T));
  IGM.Types.Converted.insert(std::make_pair(T, TInfo));

  // We don't need a discriminator if this is a singleton ADT.
  if (T->hasSingleElement()) {
    Type Ty = T->getElement(0)->ArgumentType;

    llvm::Type *Type;
    if (Ty.isNull()) {
      Type = IGM.Int8Ty;
      TInfo->TypeAlignment = Alignment(1);
    } else {
      const TypeInfo &EltInfo = getFragileTypeInfo(IGM, Ty);
      assert(EltInfo.isComplete());
      Type = EltInfo.Type;
      TInfo->TypeSize = EltInfo.TypeSize;
      TInfo->TypeAlignment = EltInfo.TypeAlignment;
    }

    llvm::Type *Body[] = { Type };
    Converted->setBody(Body);

    return TInfo;
  }

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
  Alignment TypeAlignment = Alignment(1);

  // Figure out how much storage we need for the union.
  for (unsigned I = 0, E = T->Elements.size(); I != E; ++I) {
    // Ignore variants that carry no data.
    Type Ty = T->getElement(I)->ArgumentType;
    if (Ty.isNull()) continue;

    // Compute layout for the type, and ignore variants with
    // zero-size data.
    const TypeInfo &EltInfo = getFragileTypeInfo(IGM, Ty);
    assert(EltInfo.isComplete());
    if (EltInfo.TypeSize.isZero()) continue;

    // The required payload size is the amount of padding needed to
    // get up to the element's alignment, plus the actual size.
    Size EltPayloadSize = EltInfo.TypeSize;
    if (EltInfo.TypeAlignment.getValue() > DiscriminatorSize.getValue())
      EltPayloadSize +=
        Size(EltInfo.TypeAlignment.getValue() - DiscriminatorSize.getValue());

    PayloadSize = std::max(PayloadSize, EltPayloadSize);
    TypeAlignment = std::max(TypeAlignment, EltInfo.TypeAlignment);
  }

  // If there's any payload at all, add in the payload array.
  if (PayloadSize) {
    Body.push_back(llvm::ArrayType::get(IGM.Int8Ty, PayloadSize.getValue()));
  }

  Size TypeSize = DiscriminatorSize + PayloadSize;
  // Should we round TypeSize up to a multiple of the alignment?

  Converted->setBody(Body);
  TInfo->TypeSize = TypeSize;
  TInfo->TypeAlignment = TypeAlignment;
  return TInfo;
}
