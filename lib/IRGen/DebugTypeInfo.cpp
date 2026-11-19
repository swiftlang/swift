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
#include "swift/AST/TypeWalker.h"
#include "swift/SIL/SILGlobalVariable.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"

using namespace swift;
using namespace irgen;

DebugTypeInfo::DebugTypeInfo(swift::Type Ty, std::optional<Alignment> Align,
                             bool IsMetadata, bool IsFixedBuffer,
                             std::optional<uint32_t> NumExtraInhabitants)
    : Type(Ty.getPointer()), NumExtraInhabitants(NumExtraInhabitants),
      Align(Align), IsMetadataType(IsMetadata), IsFixedBuffer(IsFixedBuffer) {
  assert((!Align || Align->getValue() != 0) && "zero alignment");
}

DebugTypeInfo DebugTypeInfo::getFromTypeInfo(swift::Type Ty, const TypeInfo &TI,
                                             IRGenModule &IGM) {
  std::optional<uint32_t> NumExtraInhabitants;
  std::optional<Alignment> Align;
  if (TI.isFixedSize()) {
    const FixedTypeInfo &FixTy = *cast<const FixedTypeInfo>(&TI);
    NumExtraInhabitants = FixTy.getFixedExtraInhabitantCount(IGM);
    Align = FixTy.getFixedAlignment();
  }
  assert(TI.getStorageType() && "StorageType is a nullptr");
  return DebugTypeInfo(Ty.getPointer(), Align,
                       /* IsMetadataType = */ false,
                       /* IsFixedBuffer = */ false, NumExtraInhabitants);
}

namespace {
/// Maps each canonical type that occurs within a type to its original,
/// potentially sugared type.
class OriginalTypeCollector : public TypeWalker {
  /// Maps each canonical type to its original type, or to null if it occurs
  /// with more than one original type or with one that can't be used.
  llvm::SmallDenseMap<CanType, TypeBase *, 8> OriginalTypes;
  /// The canonical types of the types being visited, outermost first.
  SmallVector<CanType, 8> Stack;
  llvm::SmallPtrSet<TypeAliasType *, 4> VisitedAliases;

public:
  /// Returns the original type of \p CanTy, or null if \p CanTy doesn't occur,
  /// occurs with more than one original type, or with one that can't be used.
  TypeBase *getOriginalType(CanType CanTy) const {
    return OriginalTypes.lookup(CanTy);
  }

  Action walkToTypePre(Type Ty) override {
    CanType CanTy = Ty->getCanonicalType();
    // A type with the same canonical type as its parent, such as the
    // underlying type of a typealias, is part of the same occurrence. The
    // outermost type wins.
    if (Stack.empty() || Stack.back() != CanTy) {
      TypeBase *Original = Ty->hasTypeParameter() ? nullptr : Ty.getPointer();
      auto [It, Inserted] = OriginalTypes.try_emplace(CanTy, Original);
      if (!Inserted && It->second != Original)
        It->second = nullptr;
    }
    Stack.push_back(CanTy);

    if (auto *Alias = dyn_cast<TypeAliasType>(Ty.getPointer()))
      if (VisitedAliases.insert(Alias).second)
        Type(Alias->getSinglyDesugaredType()).walk(*this);
    return Action::Continue;
  }

  Action walkToTypePost(Type Ty) override {
    Stack.pop_back();
    return Action::Continue;
  }
};
} // namespace

/// Reapply the sugar of \p Sugared to the parts of \p Lowered that are
/// unchanged. For example, given
///
///     typealias MyInt = Int
///
/// SIL type lowering turns `MyInt.Type` into `@thin Int.Type`, which this
/// function turns back into `@thin MyInt.Type`.
///
/// Each subtree of \p Lowered whose canonical type occurs in \p Sugared is
/// replaced by its original type there, so the result is canonically equal to
/// \p Lowered.
///
/// Note that this does not work for canonical types that occur in \p Sugared
/// with more than one original type, like `Int` in `(MyInt, Int)`, or for
/// typealiases whose underlying type is itself changed by lowering, like
/// `typealias Meta = Int.Type`. They are left unsubstituted, though the sugar
/// inside such a typealias is still restored.
static Type resugar(CanType Lowered, Type Sugared) {
  OriginalTypeCollector Collector;
  Sugared.walk(Collector);

  return Type(Lowered).transformRec([&](TypeBase *Ty) -> std::optional<Type> {
    if (auto *Original = Collector.getOriginalType(Ty->getCanonicalType()))
      return Type(Original);
    return std::nullopt;
  });
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
  // the type hasn't been changed by type lowering or an optimization pass.
  if (Sugared->isEqual(RealType)) {
    if (DeclType->hasTypeParameter())
      DeclType = Decl->getTypeInContext();
    return getFromTypeInfo(DeclType, Info, IGM);
  }

  // Otherwise keep the sugar of the parts that weren't changed. Since Sugared
  // is an interface type, this leaves out sugar that depends on generic
  // parameters, like `BoxA<T>`: LLDB can't resolve a generic typealias whose
  // arguments are bound in the current frame.
  return getFromTypeInfo(resugar(RealType->getCanonicalType(), Sugared), Info,
                         IGM);
}

DebugTypeInfo DebugTypeInfo::getGlobalMetadata(swift::Type Ty, Size size,
                                               Alignment align) {
  DebugTypeInfo DbgTy(Ty.getPointer(), align,
                      /* IsMetadataType = */ false);
  assert(!DbgTy.isContextArchetype() &&
         "type metadata cannot contain an archetype");
  return DbgTy;
}

DebugTypeInfo DebugTypeInfo::getTypeMetadata(swift::Type Ty, Size size,
                                             Alignment align) {
  DebugTypeInfo DbgTy(Ty.getPointer(), align,
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
  DebugTypeInfo DbgTy(Type, Align,
                      /* IsMetadataType = */ false, /* IsFixedBuffer = */ true);
  assert(!DbgTy.isContextArchetype() &&
         "type of global variable cannot be an archetype");
  return DbgTy;
}

DebugTypeInfo DebugTypeInfo::getObjCClass(ClassDecl *theClass, Size SizeInBytes,
                                          Alignment align) {
  DebugTypeInfo DbgTy(theClass->getInterfaceType().getPointer(), align,
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
  if (auto *PPT = dyn_cast<ParameterizedProtocolType>(Type))
    return PPT->getProtocol();
  if (auto *E = dyn_cast<ExistentialType>(Type))
    return E->getConstraintType()->getAnyNominal();
  return nullptr;
}

#if !defined(NDEBUG) || defined(LLVM_ENABLE_DUMP)
LLVM_DUMP_METHOD void DebugTypeInfo::dump() const {
  llvm::errs() << "[";
  if (isForwardDecl())
    llvm::errs() << "forward ";
  if (Align)
    llvm::errs() << "Alignment " << Align->getValue() << "] ";
  else
    llvm::errs() << "Alignment unknown] ";
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
  std::optional<uint64_t> SizeInBits;

  auto *StorageType = IGM.getStorageTypeForUnlowered(Ty);
  // Prefer the storage type where possible. This is more accurate in case of
  // Int1 where we see the exact number of used bits.
  // If the type does NOT have a fixed size, then ignore the storage type as it
  // ithe storage type is just a pointer.
  if (StorageType->isSized() && Info.isFixedSize())
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
