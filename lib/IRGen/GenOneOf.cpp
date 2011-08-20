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

using namespace swift;
using namespace irgen;

const TypeInfo *
TypeConverter::convertOneOfType(IRGenModule &IGM, OneOfType *T) {
  // Every oneof type is a unique type.
  llvm::StructType *Converted
    = llvm::StructType::create(IGM.getLLVMContext(), "oneof");

  TypeInfo *TInfo = new TypeInfo(Converted, 0, 0);

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
      TInfo->AlignmentInBytes = 1;
    } else {
      const TypeInfo &EltInfo = getFragileTypeInfo(IGM, Ty);
      assert(EltInfo.isComplete());
      Type = EltInfo.Type;
      TInfo->SizeInBytes = EltInfo.SizeInBytes;
      TInfo->AlignmentInBytes = EltInfo.AlignmentInBytes;
    }

    llvm::Type *Body[] = { Type };
    Converted->setBody(Body);

    return TInfo;
  }

  // Otherwise, we need a discriminator.
  llvm::Type *DiscriminatorType;
  uint64_t DiscriminatorSizeInBytes;
  if (T->Elements.size() <= (1 << 8)) {
    DiscriminatorType = IGM.Int8Ty;
    DiscriminatorSizeInBytes = 1;
  } else if (T->Elements.size() <= (1 << 16)) {
    DiscriminatorType = IGM.Int16Ty;
    DiscriminatorSizeInBytes = 2;
  } else {
    DiscriminatorType = IGM.Int32Ty;
    DiscriminatorSizeInBytes = 4;
  }

  SmallVector<llvm::Type*, 2> Body;
  Body.push_back(DiscriminatorType);

  unsigned PayloadSizeInBytes = 0;
  unsigned AlignmentInBytes = 1;

  // Figure out how much storage we need for the union.
  for (unsigned I = 0, E = T->Elements.size(); I != E; ++I) {
    // Ignore variants that carry no data.
    Type Ty = T->getElement(I)->ArgumentType;
    if (Ty.isNull()) continue;

    // Compute layout for the type, and ignore variants with
    // zero-size data.
    const TypeInfo &EltInfo = getFragileTypeInfo(IGM, Ty);
    assert(EltInfo.isComplete());
    if (EltInfo.SizeInBytes == 0) continue;

    // The required payload size is the amount of padding needed to
    // get up to the element's alignment, plus the actual size.
    unsigned EltPayloadSizeInBytes = EltInfo.SizeInBytes;
    if (EltInfo.AlignmentInBytes > DiscriminatorSizeInBytes)
      EltPayloadSizeInBytes +=
        (EltInfo.AlignmentInBytes - DiscriminatorSizeInBytes);

    PayloadSizeInBytes = std::max(PayloadSizeInBytes, EltPayloadSizeInBytes);
    AlignmentInBytes = std::max(AlignmentInBytes, EltInfo.AlignmentInBytes);
  }

  // If there's any payload at all, add in the payload array.
  if (PayloadSizeInBytes) {
    Body.push_back(llvm::ArrayType::get(IGM.Int8Ty, PayloadSizeInBytes));
  }

  unsigned SizeInBytes = DiscriminatorSizeInBytes + PayloadSizeInBytes;

  // Should we round SizeInBytes up to a multiple of the alignment?

  Converted->setBody(Body);
  TInfo->SizeInBytes = SizeInBytes;
  TInfo->AlignmentInBytes = AlignmentInBytes;
  return TInfo;
}
