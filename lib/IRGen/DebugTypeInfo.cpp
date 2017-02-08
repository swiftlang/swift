//===--- DebugTypeInfo.cpp - Type Info for Debugging ----------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines the data structure that holds all the debug info
// we want to emit for types.
//
//===----------------------------------------------------------------------===//

#include "DebugTypeInfo.h"
#include "FixedTypeInfo.h"
#include "IRGen.h"
#include "swift/SIL/SILGlobalVariable.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"

using namespace swift;
using namespace irgen;

DebugTypeInfo::DebugTypeInfo(DeclContext *DC, swift::Type Ty,
                             llvm::Type *StorageTy, Size size, Alignment align)
    : DeclCtx(DC), Type(Ty.getPointer()), StorageType(StorageTy), size(size),
      align(align) {
  assert((!isArchetype() || (isArchetype() && DC)) &&
         "archetype without a declcontext");
  assert(StorageType && "StorageType is a nullptr");
  assert(align.getValue() != 0);
}

DebugTypeInfo DebugTypeInfo::getFromTypeInfo(DeclContext *DC, swift::Type Ty,
                                             const TypeInfo &Info) {
  Size size;
  if (Info.isFixedSize()) {
    const FixedTypeInfo &FixTy = *cast<const FixedTypeInfo>(&Info);
    size = FixTy.getFixedSize();
  } else {
    // FIXME: Handle NonFixedTypeInfo here or assert that we won't
    // encounter one.
    size = Size(0);
  }
  return DebugTypeInfo(DC, Ty.getPointer(), Info.getStorageType(), size,
                       Info.getBestKnownAlignment());
}

DebugTypeInfo DebugTypeInfo::getLocalVariable(DeclContext *DeclCtx,
                                              VarDecl *Decl, swift::Type Ty,
                                              const TypeInfo &Info,
                                              bool Unwrap) {

  auto DeclType = Ty;
  if (DeclCtx)
    DeclType = (Decl->hasType()
                    ? Decl->getType()
                    : DeclCtx->mapTypeIntoContext(Decl->getInterfaceType()));
  auto RealType = Ty;
  if (Unwrap) {
    DeclType = DeclType->getInOutObjectType();
    RealType = RealType->getInOutObjectType();
  }

  // DynamicSelfType is also sugar as far as debug info is concerned.
  auto DeclSelfType = DeclType;
  if (auto DynSelfTy = DeclType->getAs<DynamicSelfType>())
    DeclSelfType = DynSelfTy->getSelfType();

  // Prefer the original, potentially sugared version of the type if
  // the type hasn't been mucked with by an optimization pass.
  auto *Type = DeclSelfType->isEqual(RealType) ? DeclType.getPointer()
                                               : RealType.getPointer();
  return getFromTypeInfo(DeclCtx, Type, Info);
}

DebugTypeInfo DebugTypeInfo::getMetadata(swift::Type Ty, llvm::Type *StorageTy,
                                         Size size, Alignment align) {
  DebugTypeInfo DbgTy = {nullptr, Ty.getPointer(), StorageTy, size, align};
  assert(!DbgTy.isArchetype() && "type metadata cannot contain an archetype");
  return DbgTy;
}

DebugTypeInfo DebugTypeInfo::getGlobal(SILGlobalVariable *GV,
                                       llvm::Type *StorageTy, Size size,
                                       Alignment align) {
  // Prefer the original, potentially sugared version of the type if
  // the type hasn't been mucked with by an optimization pass.
  auto LowTy = GV->getLoweredType().getSwiftType();
  auto *Type = LowTy.getPointer();
  if (auto *Decl = GV->getDecl()) {
    auto DeclType =
        (Decl->hasType() ? Decl->getType()
                         : Decl->getDeclContext()->mapTypeIntoContext(
                               Decl->getInterfaceType()));
    if (DeclType->isEqual(LowTy))
      Type = DeclType.getPointer();
  }
  DebugTypeInfo DbgTy = {nullptr, Type, StorageTy, size, align};
  assert(StorageTy && "StorageType is a nullptr");
  assert(!DbgTy.isArchetype() &&
         "type of a global var cannot contain an archetype");
  assert(align.getValue() != 0);
  return DbgTy;
}

DebugTypeInfo DebugTypeInfo::getObjCClass(ClassDecl *theClass,
                                          llvm::Type *StorageType, Size size,
                                          Alignment align) {
  DebugTypeInfo DbgTy(nullptr, theClass->getInterfaceType().getPointer(),
                      StorageType, size, align);
  assert(!DbgTy.isArchetype() &&
         "type of an objc class cannot contain an archetype");
  return DbgTy;
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
  return typesEqual(getType(), T.getType()) && size == T.size &&
         align == T.align;
}

bool DebugTypeInfo::operator!=(DebugTypeInfo T) const { return !operator==(T); }

TypeDecl *DebugTypeInfo::getDecl() const {
  if (auto *N = dyn_cast<NominalType>(Type))
    return N->getDecl();
  if (auto *TA = dyn_cast<NameAliasType>(Type))
    return TA->getDecl();
  if (auto *UBG = dyn_cast<UnboundGenericType>(Type))
    return UBG->getDecl();
  if (auto *BG = dyn_cast<BoundGenericType>(Type))
    return BG->getDecl();
  return nullptr;
}

#if !defined(NDEBUG) || defined(LLVM_ENABLE_DUMP)
LLVM_DUMP_METHOD void DebugTypeInfo::dump() const {
  llvm::errs() << "[Size " << size.getValue() << " Alignment "
               << align.getValue() << "] ";

  getType()->dump();
  if (StorageType) {
    llvm::errs() << "StorageType=";
    StorageType->dump();
  }
}
#endif
