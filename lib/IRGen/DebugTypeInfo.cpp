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

DebugTypeInfo::DebugTypeInfo(swift::Type Ty,
                             llvm::Type *StorageTy,
                             uint64_t SizeInBytes,
                             uint32_t AlignInBytes,
                             DeclContext *DC)
  : DeclOrContext(DC),
    Type(Ty.getPointer()),
    StorageType(StorageTy),
    size(SizeInBytes),
    align(AlignInBytes) {
  assert(StorageType && "StorageType is a nullptr");
  assert(align.getValue() != 0);
}

DebugTypeInfo::DebugTypeInfo(swift::Type Ty, llvm::Type *StorageTy,
                             Size size, Alignment align,
                             DeclContext *DC)
  : DeclOrContext(DC),
    Type(Ty.getPointer()),
    StorageType(StorageTy),
    size(size),
    align(align) {
  assert(StorageType && "StorageType is a nullptr");
  assert(align.getValue() != 0);
}

static void
initFromTypeInfo(Size &size, Alignment &align, llvm::Type *&StorageType,
                 const TypeInfo &Info) {
  StorageType = Info.getStorageType();
  if (Info.isFixedSize()) {
    const FixedTypeInfo &FixTy = *cast<const FixedTypeInfo>(&Info);
    size = FixTy.getFixedSize();
  } else {
    // FIXME: Handle NonFixedTypeInfo here or assert that we won't
    // encounter one.
    size = Size(0);
  }
  align = Info.getBestKnownAlignment();
  assert(align.getValue() != 0);
  assert(StorageType && "StorageType is a nullptr");
}

DebugTypeInfo::DebugTypeInfo(swift::Type Ty, const TypeInfo &Info,
                             DeclContext *DC)
  : DeclOrContext(DC),
    Type(Ty.getPointer()) {
  initFromTypeInfo(size, align, StorageType, Info);
}

DebugTypeInfo::DebugTypeInfo(ValueDecl *Decl, const TypeInfo &Info)
  : DeclOrContext(Decl) {
  // Use the sugared version of the type, if there is one.
  if (auto AliasDecl = dyn_cast<TypeAliasDecl>(Decl))
    Type = AliasDecl->getAliasType();
  else
    Type = Decl->getType().getPointer();

  initFromTypeInfo(size, align, StorageType, Info);
}

DebugTypeInfo::DebugTypeInfo(ValueDecl *Decl, llvm::Type *StorageTy,
                             Size size, Alignment align)
  : DeclOrContext(Decl),
    StorageType(StorageTy),
    size(size),
    align(align) {
  // Use the sugared version of the type, if there is one.
  if (auto AliasDecl = dyn_cast<TypeAliasDecl>(Decl))
    Type = AliasDecl->getAliasType();
  else
    Type = Decl->getType().getPointer();

  assert(StorageType && "StorageType is a nullptr");
  assert(align.getValue() != 0);
}

DebugTypeInfo::DebugTypeInfo(ValueDecl *Decl, swift::Type Ty,
                             const TypeInfo &Info)
  : DeclOrContext(Decl) {
  // Prefer the original, potentially sugared version of the type if
  // the type hasn't been mucked with by an optimization pass.
  if (Decl->getType().getCanonicalTypeOrNull() == Ty.getCanonicalTypeOrNull())
    Type = Decl->getType().getPointer();
  else
    Type = Ty.getPointer();

  initFromTypeInfo(size, align, StorageType, Info);
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
  llvm::errs() << "[Size " << size.getValue()
               << " Alignment " << align.getValue()<<"] ";
  if (getDecl())
    getDecl()->dump(llvm::errs());
  else
    getType()->dump();
  if (StorageType) {
    llvm::errs() << "StorageType=";
    StorageType->dump();
  }
}
