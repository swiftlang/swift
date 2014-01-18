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
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"

using namespace swift;
using namespace irgen;

DebugTypeInfo::DebugTypeInfo(Type Ty,
                             uint64_t SizeInBytes,
                             uint32_t AlignInBytes,
                             DeclContext *DC)
  : DeclOrType(Ty.getPointer()),
    DeclCtx(DC),
    StorageType(nullptr),
    size(SizeInBytes),
    align(AlignInBytes),
    DebugScope(nullptr) {
  assert(align.getValue() != 0);
}

DebugTypeInfo::DebugTypeInfo(Type Ty, Size size, Alignment align, DeclContext *DC)
  : DeclOrType(Ty.getPointer()),
    DeclCtx(DC),
    StorageType(nullptr),
    size(size),
    align(align),
    DebugScope(nullptr) {
  assert(align.getValue() != 0);
}

static void
initFromTypeInfo(Size &size, Alignment &align, llvm::Type *&StorageType,
                 const TypeInfo &Info) {
  StorageType = Info.getStorageType();
  if (Info.isFixedSize()) {
    const FixedTypeInfo &FixTy = *cast<const FixedTypeInfo>(&Info);
    size = FixTy.getFixedSize();
    align = FixTy.getBestKnownAlignment();
  } else {
    size = Size(0);
    align = Info.getBestKnownAlignment();
  }
  assert(align.getValue() != 0);
}

DebugTypeInfo::DebugTypeInfo(Type Ty, const TypeInfo &Info, DeclContext *DC)
  : DeclOrType(Ty.getPointer()),
    DeclCtx(DC),
    DebugScope(nullptr) {
  initFromTypeInfo(size, align, StorageType, Info);
}

DebugTypeInfo::DebugTypeInfo(ValueDecl *Decl, const TypeInfo &Info,
                             SILDebugScope *DS)
  : DeclOrType(Decl),
    DeclCtx(nullptr),
    DebugScope(DS) {
  initFromTypeInfo(size, align, StorageType, Info);
}

DebugTypeInfo::DebugTypeInfo(ValueDecl *Decl, Size size, Alignment align,
                             SILDebugScope *DS)
  : DeclOrType(Decl),
    DeclCtx(nullptr),
    StorageType(nullptr),
    size(size),
    align(align),
    DebugScope(DS)  {
  assert(align.getValue() != 0);
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
    && size == T.size
    && align == T.align;
}

bool DebugTypeInfo::operator!=(DebugTypeInfo T) const {
  return !operator==(T);
}

void DebugTypeInfo::dump() const {
  llvm::errs()<<"[Size "<<size.getValue()<<" Alignment "<<align.getValue()<<"] ";
  if (getDecl())
    getDecl()->dump(llvm::errs());
  else
    getType()->dump();
}
