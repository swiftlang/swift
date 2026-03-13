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
#include "IRGenModule.h"
#include "swift/Basic/Assertions.h"
#include "swift/SIL/SILGlobalVariable.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"

using namespace swift;
using namespace irgen;

DebugTypeInfo::DebugTypeInfo(swift::Type Ty, Alignment Align,
                             bool HasDefaultAlignment, bool IsMetadata,
                             bool IsFixedBuffer,
                             std::optional<uint32_t> NumExtraInhabitants)
    : Type(Ty.getPointer()), NumExtraInhabitants(NumExtraInhabitants),
      Align(Align), DefaultAlignment(HasDefaultAlignment),
      IsMetadataType(IsMetadata), IsFixedBuffer(IsFixedBuffer) {
  assert(Align.getValue() != 0);
}

/// Determine whether this type has an attribute specifying a custom alignment.
static bool hasDefaultAlignment(swift::Type Ty) {
  if (auto CanTy = Ty->getCanonicalType())
    if (auto *TyDecl = CanTy.getNominalOrBoundGenericNominal())
      if (TyDecl->getAttrs().getAttribute<AlignmentAttr>()
          || TyDecl->getAttrs().getAttribute<RawLayoutAttr>())
        return false;
  return true;
}

DebugTypeInfo DebugTypeInfo::getFromTypeInfo(swift::Type Ty, const TypeInfo &TI,
                                             IRGenModule &IGM) {
  std::optional<uint32_t> NumExtraInhabitants;
  if (TI.isFixedSize()) {
    const FixedTypeInfo &FixTy = *cast<const FixedTypeInfo>(&TI);
    NumExtraInhabitants = FixTy.getFixedExtraInhabitantCount(IGM);
  }
  assert(TI.getStorageType() && "StorageType is a nullptr");
  return DebugTypeInfo(Ty.getPointer(), TI.getBestKnownAlignment(),
                       ::hasDefaultAlignment(Ty),
                       /* IsMetadataType = */ false,
                       /* IsFixedBuffer = */ false, NumExtraInhabitants);
}

DebugTypeInfo DebugTypeInfo::getLocalVariable(VarDecl *Decl, swift::Type Ty,
                                              const TypeInfo &Info,
                                              IRGenModule &IGM) {

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
  return getFromTypeInfo(Type, Info, IGM);
}

DebugTypeInfo DebugTypeInfo::getGlobalMetadata(swift::Type Ty, Size size,
                                               Alignment align) {
  DebugTypeInfo DbgTy(Ty.getPointer(), align,
                      /* HasDefaultAlignment = */ true,
                      /* IsMetadataType = */ false);
  assert(!DbgTy.isContextArchetype() &&
         "type metadata cannot contain an archetype");
  return DbgTy;
}

DebugTypeInfo DebugTypeInfo::getTypeMetadata(swift::Type Ty, Size size,
                                             Alignment align) {
  DebugTypeInfo DbgTy(Ty.getPointer(), align,
                      /* HasDefaultAlignment = */ true,
                      /* IsMetadataType = */ true);
  assert(!DbgTy.isContextArchetype() &&
         "type metadata cannot contain an archetype");
  return DbgTy;
}

DebugTypeInfo DebugTypeInfo::getForwardDecl(swift::Type Ty) {
  DebugTypeInfo DbgTy(Ty.getPointer());
  DbgTy.IsForwardDecl = true;
  return DbgTy;
}

static TypeBase *getTypeForGlobal(SILGlobalVariable *GV, IRGenModule &IGM) {
  // Prefer the original, potentially sugared version of the type if
  // the type hasn't been mucked with by an optimization pass.
  auto LowTy = GV->getLoweredType().getASTType();
  auto *Type = LowTy.getPointer();
  if (auto *Decl = GV->getDecl()) {
    auto DeclType = Decl->getTypeInContext();
    if (DeclType->isEqual(LowTy))
      Type = DeclType.getPointer();
  }
  // If this global variable contains an opaque type, replace it with its
  // underlying type.
  Type = IGM.substOpaqueTypesWithUnderlyingTypes(Type).getPointer();
  return Type;
}

DebugTypeInfo DebugTypeInfo::getGlobal(SILGlobalVariable *GV,
                                       IRGenModule &IGM) {
  auto *Type = getTypeForGlobal(GV, IGM);
  auto &TI = IGM.getTypeInfoForUnlowered(Type);
  DebugTypeInfo DbgTy = getFromTypeInfo(Type, TI, IGM);
  assert(!DbgTy.isContextArchetype() &&
         "type of global variable cannot be an archetype");
  return DbgTy;
}

DebugTypeInfo DebugTypeInfo::getGlobalFixedBuffer(SILGlobalVariable *GV,
                                                  Alignment Align,
                                                  IRGenModule &IGM) {
  auto *Type = getTypeForGlobal(GV, IGM);
  DebugTypeInfo DbgTy(Type, Align, ::hasDefaultAlignment(Type),
                      /* IsMetadataType = */ false, /* IsFixedBuffer = */ true);
  assert(!DbgTy.isContextArchetype() &&
         "type of global variable cannot be an archetype");
  return DbgTy;
}

DebugTypeInfo DebugTypeInfo::getObjCClass(ClassDecl *theClass, Size SizeInBytes,
                                          Alignment align) {
  DebugTypeInfo DbgTy(theClass->getInterfaceType().getPointer(), align,
                      /* HasDefaultAlignment = */ true,
                      /* IsMetadataType = */ false);
  assert(!DbgTy.isContextArchetype() &&
         "type of objc class cannot be an archetype");
  return DbgTy;
}

DebugTypeInfo DebugTypeInfo::getErrorResult(swift::Type Ty,
                                            IRGenModule &IGM) {
  auto &TI = IGM.getTypeInfoForUnlowered(Ty);
  DebugTypeInfo DbgTy = getFromTypeInfo(Ty, TI, IGM);
  return DbgTy;
}

bool DebugTypeInfo::operator==(DebugTypeInfo T) const {
  return getType() == T.getType() &&
         Align == T.Align;
}

bool DebugTypeInfo::operator!=(DebugTypeInfo T) const { return !operator==(T); }

TypeDecl *DebugTypeInfo::getDecl() const {
  if (auto *N = dyn_cast<NominalType>(Type))
    return N->getDecl();
  if (auto *BTA = dyn_cast<TypeAliasType>(Type))
    return BTA->getDecl();
  if (auto *UBG = dyn_cast<UnboundGenericType>(Type))
    return UBG->getDecl();
  if (auto *BG = dyn_cast<BoundGenericType>(Type))
    return BG->getDecl();
  if (auto *E = dyn_cast<ExistentialType>(Type))
    return E->getConstraintType()->getAnyNominal();
  return nullptr;
}

#if !defined(NDEBUG) || defined(LLVM_ENABLE_DUMP)
LLVM_DUMP_METHOD void DebugTypeInfo::dump() const {
  llvm::errs() << "[";
  if (isForwardDecl())
    llvm::errs() << "forward ";
  llvm::errs() << "Alignment " << Align.getValue() << "] ";
  if (auto *Type = getType())
    Type->dump(llvm::errs());
}
#endif

std::optional<CompletedDebugTypeInfo>
CompletedDebugTypeInfo::getFromTypeInfo(swift::Type Ty, const TypeInfo &Info,
                                        IRGenModule &IGM,
                                        std::optional<Size::int_type> Size) {
  if (!Ty || Ty->hasTypeParameter())
    return {};
  auto *StorageType = IGM.getStorageTypeForUnlowered(Ty);
  std::optional<uint64_t> SizeInBits;
  if (StorageType->isSized())
    SizeInBits = IGM.DataLayout.getTypeSizeInBits(StorageType);
  else if (Info.isFixedSize()) {
    const FixedTypeInfo &FixTy = *cast<const FixedTypeInfo>(&Info);
    Size::int_type Size = FixTy.getFixedSize().getValue() * 8;
    SizeInBits = Size;
  } else if (Size) {
    SizeInBits = *Size * 8;
  }

  return CompletedDebugTypeInfo::get(
      DebugTypeInfo::getFromTypeInfo(Ty, Info, IGM), SizeInBits);
}
