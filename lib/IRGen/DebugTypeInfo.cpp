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

/// Reapply the sugar of \p Sugared to the parts of \p Lowered that are
/// unchanged, and return a type canonically equal to \p Lowered. For example,
/// given
///
///     typealias MyInt = Int
///
/// SIL type lowering turns `MyInt.Type` into `@thin Int.Type`, which this
/// function turns back into `@thin MyInt.Type`. Sugar that mentions generic
/// parameters, like `BoxA<T>` or a typealias nested in a generic type, is left
/// out, because LLDB can't yet resolve a typealias applied to the generic
/// parameters of the current frame.
static Type resugar(CanType Lowered, Type Sugared) {
  // Both types are walked in parallel, so sugar is only reapplied where it was
  // written. Looking it up by canonical type would be wrong, because
  // specialization and opaque type substitution introduce new occurrences of a
  // canonical type: specializing `(T, MyInt)` with `T == Int` yields
  // `(Int, Int)`, whose first element must not become `MyInt`.
  //
  // Sugar that can't be used may still contain sugar that can: a typealias
  // whose underlying type lowering changes, like `typealias MetaT = MyInt.Type`
  // for `@thin Int.Type`, still contains `MyInt`.
  for (;;) {
    if (Sugared->getCanonicalType() == Lowered && !Sugared->hasTypeParameter())
      return Sugared;
    auto *Sugar = dyn_cast<SugarType>(Sugared.getPointer());
    if (!Sugar)
      break;
    Sugared = Sugar->getSinglyDesugaredType();
  }

  // The outermost type was changed, or it mentions generic parameters, but its
  // components may be unchanged.
  ASTContext &Ctx = Lowered->getASTContext();

  if (auto LoweredTuple = dyn_cast<TupleType>(Lowered)) {
    auto *SugaredTuple = dyn_cast<TupleType>(Sugared.getPointer());
    if (!SugaredTuple)
      return Lowered;
    // Substitution can replace a pack expansion with any number of elements,
    // so only the elements before the first and after the last pack expansion
    // are known to line up.
    unsigned NumLowered = LoweredTuple->getNumElements();
    unsigned NumSugared = SugaredTuple->getNumElements();
    unsigned Prefix = NumSugared, Suffix = 0;
    for (unsigned I = 0; I != NumSugared; ++I) {
      if (SugaredTuple->getElementType(I)->is<PackExpansionType>()) {
        Prefix = std::min(Prefix, I);
        Suffix = NumSugared - I - 1;
      }
    }
    if (NumLowered < Prefix + Suffix)
      return Lowered;
    SmallVector<TupleTypeElt, 4> Elts(LoweredTuple->getElements());
    auto resugarElt = [&](unsigned LoweredIdx, unsigned SugaredIdx) {
      Elts[LoweredIdx] = Elts[LoweredIdx].getWithType(
          resugar(LoweredTuple.getElementType(LoweredIdx),
                  SugaredTuple->getElementType(SugaredIdx)));
    };
    for (unsigned I = 0; I != Prefix; ++I)
      resugarElt(I, I);
    for (unsigned I = 1; I <= Suffix; ++I)
      resugarElt(NumLowered - I, NumSugared - I);
    return TupleType::get(Elts, Ctx);
  }

  if (auto LoweredMeta = dyn_cast<AnyMetatypeType>(Lowered)) {
    auto *SugaredMeta = dyn_cast<AnyMetatypeType>(Sugared.getPointer());
    if (!SugaredMeta || SugaredMeta->getKind() != LoweredMeta->getKind())
      return Lowered;
    Type Instance =
        resugar(LoweredMeta.getInstanceType(), SugaredMeta->getInstanceType());
    std::optional<MetatypeRepresentation> Repr;
    if (LoweredMeta->hasRepresentation())
      Repr = LoweredMeta->getRepresentation();
    if (isa<MetatypeType>(LoweredMeta))
      return MetatypeType::get(Instance, Repr);
    return ExistentialMetatypeType::get(Instance, Repr);
  }

  if (auto LoweredNominal = dyn_cast<NominalType>(Lowered)) {
    auto *SugaredNominal = dyn_cast<NominalType>(Sugared.getPointer());
    if (!SugaredNominal ||
        SugaredNominal->getDecl() != LoweredNominal->getDecl() ||
        bool(SugaredNominal->getParent()) !=
            bool(LoweredNominal->getParent()) ||
        SugaredNominal->getGenericArgs().size() !=
            LoweredNominal->getGenericArgs().size())
      return Lowered;
    Type Parent;
    if (auto LoweredParent = LoweredNominal.getParent())
      Parent = resugar(LoweredParent, SugaredNominal->getParent());
    SmallVector<Type, 2> Args;
    for (auto [LoweredArg, SugaredArg] : llvm::zip_equal(
             LoweredNominal.getGenericArgs(), SugaredNominal->getGenericArgs()))
      Args.push_back(resugar(LoweredArg, SugaredArg));
    return NominalType::get(LoweredNominal->getDecl(), Parent, Args, Ctx,
                            LoweredNominal->getScopeArgs());
  }

  if (auto LoweredStorage = dyn_cast<ReferenceStorageType>(Lowered)) {
    auto *SugaredStorage = dyn_cast<ReferenceStorageType>(Sugared.getPointer());
    if (!SugaredStorage ||
        SugaredStorage->getKind() != LoweredStorage->getKind())
      return Lowered;
    Type Referent = resugar(LoweredStorage.getReferentType(),
                            SugaredStorage->getReferentType());
    return ReferenceStorageType::get(Referent, LoweredStorage->getOwnership(),
                                     Ctx);
  }

  // AST function types are left alone by lowering, but generic arguments and
  // metatypes can still contain them.
  if (auto LoweredFn = dyn_cast<FunctionType>(Lowered)) {
    auto *SugaredFn = dyn_cast<FunctionType>(Sugared.getPointer());
    if (!SugaredFn || SugaredFn->getNumParams() != LoweredFn->getNumParams())
      return Lowered;
    SmallVector<AnyFunctionType::Param, 4> Params;
    for (auto [LoweredParam, SugaredParam] :
         llvm::zip_equal(LoweredFn.getParams(), SugaredFn->getParams()))
      Params.push_back(LoweredParam.withType(
          resugar(LoweredParam.getPlainType(), SugaredParam.getPlainType())));
    Type Result = resugar(LoweredFn.getResult(), SugaredFn->getResult());
    return FunctionType::get(Params, LoweredFn->getYields(), Result,
                             LoweredFn->getExtInfo());
  }

  // Other types keep their lowered form. SIL function types only contain
  // canonical types, and the debug info only records constrained existentials
  // by their mangled names, which contain no sugar.
  return Lowered;
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

  // Otherwise keep the sugar of the parts that weren't changed, if there is
  // debug info for variables to show it.
  CanType Lowered = RealType->getCanonicalType();
  if (IGM.getOptions().DebugInfoLevel <= IRGenDebugInfoLevel::LineTables)
    return getFromTypeInfo(Lowered, Info, IGM);
  swift::Type Resugared = resugar(Lowered, Sugared);
  ASSERT(Resugared->getCanonicalType() == Lowered &&
         "resugaring changed the lowered type");
  return getFromTypeInfo(Resugared, Info, IGM);
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
