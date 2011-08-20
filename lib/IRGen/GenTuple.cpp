//===--- GenTuple.cpp - Swift IR Generation For Tuple Types ---------------===//
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
//  This file implements IR generation for tuple types in Swift.  This
//  includes creating the IR type as  well as emitting the primitive access
//  operations.
//
//  Currently we do no optimization of tuples.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/Types.h"
#include "swift/AST/Decl.h"
#include "llvm/DerivedTypes.h"
#include "llvm/Target/TargetData.h"

#include "GenType.h"
#include "IRGenModule.h"

using namespace swift;
using namespace irgen;

const TypeInfo *
TypeConverter::convertTupleType(IRGenModule &IGM, TupleType *T) {
  SmallVector<llvm::Type*, 8> Elts;
  Elts.reserve(T->Fields.size());

  unsigned CurFieldNumber = 0;
  uint64_t TupleSize = 0;
  unsigned TupleAlignment = 0;
  
  // TODO: rearrange the tuple for optimal packing.
  for (const TupleTypeElt &Field : T->Fields) {
    const TypeInfo &FieldInfo = getFragileTypeInfo(IGM, Field.Ty);
    assert(FieldInfo.AlignmentInBytes);

    // Ignore zero-sized fields.
    if (FieldInfo.SizeInBytes == 0) continue;

    TupleAlignment = std::max(TupleAlignment, FieldInfo.AlignmentInBytes);

    // If the current tuple size isn't a multiple of the tuple
    // alignment, and the field's required alignment is more than its
    // IR preferred alignment, we need padding.
    if (unsigned OffsetFromAlignment = TupleSize % TupleAlignment) {
      unsigned FieldIRAlignment =
        IGM.TargetData.getABITypeAlignment(FieldInfo.Type);
      assert(FieldIRAlignment <= FieldInfo.AlignmentInBytes);
      if (FieldIRAlignment != FieldInfo.AlignmentInBytes) {
        unsigned PaddingRequired = TupleAlignment - OffsetFromAlignment;
        Elts.push_back(llvm::ArrayType::get(IGM.Int8Ty, PaddingRequired));
        CurFieldNumber++;

        TupleSize += PaddingRequired;
      }
    }

    Elts.push_back(FieldInfo.Type);
    CurFieldNumber++;
    TupleSize += FieldInfo.SizeInBytes;
    // TODO: remember the field number somewhere so that we can do
    // access into this tuple properly.
  }

  // If the tuple requires no storage at all, just use i8.  Most
  // clients will just ignore zero-size types, but those that care can
  // have a sensible one-byte type.
  if (TupleSize == 0) {
    return new TypeInfo(IGM.Int8Ty, 0, 1);
  }

  // Otherwise, build a new, structural type.
  llvm::StructType *Converted
    = llvm::StructType::get(IGM.getLLVMContext(), Elts);

  return new TypeInfo(Converted, TupleSize, TupleAlignment);
}
