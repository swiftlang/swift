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
#include "swift/SIL/SILGlobalVariable.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"

using namespace swift;
using namespace irgen;

DebugTypeInfo::DebugTypeInfo(DeclContext *DC, GenericEnvironment *GE,
                             swift::Type Ty, llvm::Type *StorageTy, Size size,
                             Alignment align, bool HasDefaultAlignment)
    : DeclCtx(DC), GenericEnv(GE), Type(Ty.getPointer()),
      StorageType(StorageTy), size(size), align(align),
      DefaultAlignment(HasDefaultAlignment) {
  assert(StorageType && "StorageType is a nullptr");
  assert(align.getValue() != 0);
}

/// Determine whether this type has a custom @_alignment attribute.
static bool hasDefaultAlignment(swift::Type Ty) {
  if (auto CanTy = Ty->getCanonicalType())
    if (auto *TyDecl = CanTy.getNominalOrBoundGenericNominal())
      if (TyDecl->getAttrs().getAttribute<AlignmentAttr>())
        return false;
  return true;
}

DebugTypeInfo DebugTypeInfo::getFromTypeInfo(DeclContext *DC,
                                             GenericEnvironment *GE,
                                             swift::Type Ty,
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
  return DebugTypeInfo(DC, GE, Ty.getPointer(), Info.getStorageType(), size,
                       Info.getBestKnownAlignment(), hasDefaultAlignment(Ty));
}

DebugTypeInfo DebugTypeInfo::getLocalVariable(DeclContext *DC,
                                              GenericEnvironment *GE,
                                              VarDecl *Decl, swift::Type Ty,
                                              const TypeInfo &Info) {

  auto DeclType = Decl->getInterfaceType();
  auto RealType = Ty;

  // DynamicSelfType is also sugar as far as debug info is concerned.
  auto Sugared = DeclType;
  if (auto DynSelfTy = DeclType->getAs<DynamicSelfType>())
    Sugared = DynSelfTy->getSelfType();

  // Prefer the original, potentially sugared version of the type if
  // the type hasn't been mucked with by an optimization pass.
  auto *Type = Sugared->isEqual(RealType) ? DeclType.getPointer()
                                          : RealType.getPointer();
  return getFromTypeInfo(DC, GE, Type, Info);
}

DebugTypeInfo DebugTypeInfo::getMetadata(swift::Type Ty, llvm::Type *StorageTy,
                                         Size size, Alignment align) {
  DebugTypeInfo DbgTy(nullptr, nullptr, Ty.getPointer(), StorageTy, size,
                      align, true);
  assert(!DbgTy.isArchetype() && "type metadata cannot contain an archetype");
  return DbgTy;
}

DebugTypeInfo DebugTypeInfo::getGlobal(SILGlobalVariable *GV,
                                       llvm::Type *StorageTy, Size size,
                                       Alignment align) {
  // Prefer the original, potentially sugared version of the type if
  // the type hasn't been mucked with by an optimization pass.
  DeclContext *DC = nullptr;
  GenericEnvironment *GE = nullptr;
  auto LowTy = GV->getLoweredType().getASTType();
  auto *Type = LowTy.getPointer();
  if (auto *Decl = GV->getDecl()) {
    DC = Decl->getDeclContext();
    GE = DC->getGenericEnvironmentOfContext();
    auto DeclType = Decl->getType();
    if (DeclType->isEqual(LowTy))
      Type = DeclType.getPointer();
  }
  DebugTypeInfo DbgTy(DC, GE, Type, StorageTy, size, align,
                      hasDefaultAlignment(Type));
  assert(StorageTy && "StorageType is a nullptr");
  assert(!DbgTy.isArchetype() &&
         "type of global variable cannot be an archetype");
  assert(align.getValue() != 0);
  return DbgTy;
}

DebugTypeInfo DebugTypeInfo::getObjCClass(ClassDecl *theClass,
                                          llvm::Type *StorageType, Size size,
                                          Alignment align) {
  DebugTypeInfo DbgTy(nullptr, nullptr,
                      theClass->getInterfaceType().getPointer(), StorageType,
                      size, align, true);
  assert(!DbgTy.isArchetype() && "type of objc class cannot be an archetype");
  return DbgTy;
}

bool DebugTypeInfo::operator==(DebugTypeInfo T) const {
  return (getType() == T.getType() &&
          size == T.size &&
          align == T.align);
}

bool DebugTypeInfo::operator!=(DebugTypeInfo T) const { return !operator==(T); }

TypeDecl *DebugTypeInfo::getDecl() const {
  if (auto *N = dyn_cast<NominalType>(Type))
    return N->getDecl();
  if (auto *BTA = dyn_cast<NameAliasType>(Type))
    return BTA->getDecl();
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
