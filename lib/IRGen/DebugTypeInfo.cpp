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
  : DeclOrType(Ty.getPointer()), SizeInBytes(Size), AlignInBytes(Align) {
}

DebugTypeInfo::DebugTypeInfo(Type Ty, Size Size, Alignment Align)
  : DeclOrType(Ty.getPointer()),
    SizeInBytes(Size.getValue()), AlignInBytes(Align.getValue()) {
}

DebugTypeInfo::DebugTypeInfo(Type Ty, const TypeInfo &Info)
  : DeclOrType(Ty.getPointer()) {
  if (Info.isFixedSize()) {
    const FixedTypeInfo &FixTy = *cast<const FixedTypeInfo>(&Info);
    SizeInBytes = FixTy.getFixedSize().getValue();
    AlignInBytes = FixTy.getBestKnownAlignment().getValue();
  } else {
    SizeInBytes = 0;
    AlignInBytes = Info.getBestKnownAlignment().getValue();
  }
  assert(AlignInBytes != 0);
}

DebugTypeInfo::DebugTypeInfo(ValueDecl *Decl, const TypeInfo &Info)
  : DeclOrType(Decl) {
  // Same as above.
  if (Info.isFixedSize()) {
    const FixedTypeInfo &FixTy = *cast<const FixedTypeInfo>(&Info);
    SizeInBytes = FixTy.getFixedSize().getValue();
    AlignInBytes = FixTy.getBestKnownAlignment().getValue();
  } else {
    SizeInBytes = 0;
    AlignInBytes = Info.getBestKnownAlignment().getValue();
  }
  assert(AlignInBytes != 0);
}

DebugTypeInfo::DebugTypeInfo(ValueDecl *Decl, Size Size, Alignment Align)
  : DeclOrType(Decl),
    SizeInBytes(Size.getValue()),
    AlignInBytes(Align.getValue()) {
  assert(AlignInBytes != 0);
}

static bool typesEqual(Type A, Type B) {
  if (A.getPointer() == B.getPointer())
    return true;

  // nullptr.
  if (A.isNull() || B.isNull())
    return false;

  // Tombstone.
  auto Tombstone =
    llvm::DenseMapInfo<swift::Type>::getTombstoneKey().getPointer();
  if ((A.getPointer() == Tombstone) || (B.getPointer() == Tombstone))
    return false;

  // Pointers are safe, do the real comparison.
  return A->isSpelledLike(B.getPointer());
}

bool DebugTypeInfo::operator==(DebugTypeInfo T) const {
  return typesEqual(getType(), T.getType())
    && SizeInBytes == T.SizeInBytes
    && AlignInBytes == T.AlignInBytes;
}

bool DebugTypeInfo::operator!=(DebugTypeInfo T) const {
  return !operator==(T);
}
