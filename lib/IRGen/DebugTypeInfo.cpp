//===--- DebugTypeInfo.h - Type Info for Debugging --------------*- C++ -*-===//
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
// This file defines the data structure that holds all the debug info
// we want to emit for types.
//
//===----------------------------------------------------------------------===//

#include "DebugTypeInfo.h"
#include "IRGen.h"
#include "FixedTypeInfo.h"

using namespace swift;
using namespace irgen;

DebugTypeInfo::DebugTypeInfo(Type Ty, uint64_t Size, uint64_t Align)
  : Ty(Ty), SizeInBytes(Size), AlignmentInBytes(Align) {
}

DebugTypeInfo::DebugTypeInfo(Type Ty, Size Size, Alignment Align)
  : Ty(Ty), SizeInBytes(Size.getValue()), AlignmentInBytes(Align.getValue()) {
}

DebugTypeInfo::DebugTypeInfo(Type Ty, const TypeInfo &Info)
  : Ty(Ty) {
  if (Info.isFixedSize()) {
    const FixedTypeInfo &FixTy = *cast<const FixedTypeInfo>(&Info);
    SizeInBytes = FixTy.getFixedSize().getValue();
    AlignmentInBytes = FixTy.getBestKnownAlignment().getValue();
  }
}

DebugTypeInfo::DebugTypeInfo(const ValueDecl &Decl, const TypeInfo &Info)
  : Ty(Decl.getType()) {
  // Same as above.
  if (Info.isFixedSize()) {
    const FixedTypeInfo &FixTy = *cast<const FixedTypeInfo>(&Info);
    SizeInBytes = FixTy.getFixedSize().getValue();
    AlignmentInBytes = FixTy.getBestKnownAlignment().getValue();
  }
}

DebugTypeInfo::DebugTypeInfo(const ValueDecl &Decl, Size Size, Alignment Align)
  : Ty(Decl.getType()),
    SizeInBytes(Size.getValue()),
    AlignmentInBytes(Align.getValue()) {
}


bool DebugTypeInfo::operator==(DebugTypeInfo T) const {
  bool TypesEqual;
  if (Ty.getPointer() == T.Ty.getPointer())
    TypesEqual = true;
  else {
    // nullptr.
    if (Ty.isNull() || T.Ty.isNull())
      TypesEqual = false;

    // Tombstone.
    auto Tombstone =
      llvm::DenseMapInfo<swift::Type>::getTombstoneKey().getPointer();
    if ((Ty.getPointer() == Tombstone) || (T.Ty.getPointer() == Tombstone))
      TypesEqual = false;

    // Pointers are safe, do the real comparison.
    if (TypesEqual)
      TypesEqual = Ty->isSpelledLike(T.Ty.getPointer());
  }

  return TypesEqual
    && SizeInBytes == T.SizeInBytes
    && AlignmentInBytes == T.AlignmentInBytes;
}

bool DebugTypeInfo::operator!=(DebugTypeInfo T) const {
  return !operator==(T);
}
