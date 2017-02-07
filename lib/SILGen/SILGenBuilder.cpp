//===--- SILGenBuilder.cpp ------------------------------------------------===//
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

#include "SILGenBuilder.h"
#include "SILGenFunction.h"

using namespace swift;
using namespace Lowering;

//===----------------------------------------------------------------------===//
//                              Utility Methods
//===----------------------------------------------------------------------===//

SILGenModule &SILGenBuilder::getSILGenModule() const { return gen.SGM; }

//===----------------------------------------------------------------------===//
//                                Constructors
//===----------------------------------------------------------------------===//

SILGenBuilder::SILGenBuilder(SILGenFunction &gen)
    : SILBuilder(gen.F), gen(gen) {}

SILGenBuilder::SILGenBuilder(SILGenFunction &gen, SILBasicBlock *insertBB)
    : SILBuilder(insertBB), gen(gen) {}

SILGenBuilder::SILGenBuilder(SILGenFunction &gen, SILBasicBlock *insertBB,
                             SmallVectorImpl<SILInstruction *> *insertedInsts)
    : SILBuilder(insertBB, insertedInsts), gen(gen) {}

SILGenBuilder::SILGenBuilder(SILGenFunction &gen, SILBasicBlock *insertBB,
                             SILBasicBlock::iterator insertInst)
    : SILBuilder(insertBB, insertInst), gen(gen) {}

//===----------------------------------------------------------------------===//
//                            Instruction Emission
//===----------------------------------------------------------------------===//

MetatypeInst *SILGenBuilder::createMetatype(SILLocation loc, SILType metatype) {
  auto theMetatype = metatype.castTo<MetatypeType>();
  // Getting a nontrivial metatype requires forcing any conformances necessary
  // to instantiate the type.
  switch (theMetatype->getRepresentation()) {
  case MetatypeRepresentation::Thin:
    break;
  case MetatypeRepresentation::Thick:
  case MetatypeRepresentation::ObjC: {
    // Walk the type recursively to look for substitutions we may need.
    theMetatype.getInstanceType().findIf([&](Type t) -> bool {
      if (!t->getAnyNominal())
        return false;

      auto subs =
          t->gatherAllSubstitutions(getSILGenModule().SwiftModule, nullptr);
      getSILGenModule().useConformancesFromSubstitutions(subs);
      return false;
    });

    break;
  }
  }

  return SILBuilder::createMetatype(loc, metatype);
}

ApplyInst *SILGenBuilder::createApply(SILLocation Loc, SILValue Fn,
                                      SILType SubstFnTy, SILType Result,
                                      SubstitutionList Subs,
                                      ArrayRef<SILValue> Args) {
  getSILGenModule().useConformancesFromSubstitutions(Subs);
  return SILBuilder::createApply(Loc, Fn, SubstFnTy, Result, Subs, Args, false);
}

TryApplyInst *SILGenBuilder::createTryApply(SILLocation loc, SILValue Fn,
                                            SILType substFnTy,
                                            SubstitutionList subs,
                                            ArrayRef<SILValue> args,
                                            SILBasicBlock *normalBB,
                                            SILBasicBlock *errorBB) {
  getSILGenModule().useConformancesFromSubstitutions(subs);
  return SILBuilder::createTryApply(loc, Fn, substFnTy, subs, args, normalBB,
                                    errorBB);
}

PartialApplyInst *SILGenBuilder::createPartialApply(
    SILLocation Loc, SILValue Fn, SILType SubstFnTy,
    SubstitutionList Subs, ArrayRef<SILValue> Args, SILType ClosureTy) {
  getSILGenModule().useConformancesFromSubstitutions(Subs);
  return SILBuilder::createPartialApply(Loc, Fn, SubstFnTy, Subs, Args,
                                        ClosureTy);
}

BuiltinInst *SILGenBuilder::createBuiltin(SILLocation Loc, Identifier Name,
                                          SILType ResultTy,
                                          SubstitutionList Subs,
                                          ArrayRef<SILValue> Args) {
  getSILGenModule().useConformancesFromSubstitutions(Subs);
  return SILBuilder::createBuiltin(Loc, Name, ResultTy, Subs, Args);
}

InitExistentialAddrInst *SILGenBuilder::createInitExistentialAddr(
    SILLocation Loc, SILValue Existential, CanType FormalConcreteType,
    SILType LoweredConcreteType,
    ArrayRef<ProtocolConformanceRef> Conformances) {
  for (auto conformance : Conformances)
    getSILGenModule().useConformance(conformance);

  return SILBuilder::createInitExistentialAddr(
      Loc, Existential, FormalConcreteType, LoweredConcreteType, Conformances);
}

InitExistentialMetatypeInst *SILGenBuilder::createInitExistentialMetatype(
    SILLocation loc, SILValue metatype, SILType existentialType,
    ArrayRef<ProtocolConformanceRef> conformances) {
  for (auto conformance : conformances)
    getSILGenModule().useConformance(conformance);

  return SILBuilder::createInitExistentialMetatype(
      loc, metatype, existentialType, conformances);
}

InitExistentialRefInst *SILGenBuilder::createInitExistentialRef(
    SILLocation Loc, SILType ExistentialType, CanType FormalConcreteType,
    SILValue Concrete, ArrayRef<ProtocolConformanceRef> Conformances) {
  for (auto conformance : Conformances)
    getSILGenModule().useConformance(conformance);

  return SILBuilder::createInitExistentialRef(
      Loc, ExistentialType, FormalConcreteType, Concrete, Conformances);
}

AllocExistentialBoxInst *SILGenBuilder::createAllocExistentialBox(
    SILLocation Loc, SILType ExistentialType, CanType ConcreteType,
    ArrayRef<ProtocolConformanceRef> Conformances) {
  for (auto conformance : Conformances)
    getSILGenModule().useConformance(conformance);

  return SILBuilder::createAllocExistentialBox(Loc, ExistentialType,
                                               ConcreteType, Conformances);
}

ManagedValue SILGenBuilder::createStructExtract(SILLocation Loc,
                                                ManagedValue Base,
                                                VarDecl *Decl) {
  ManagedValue BorrowedBase = gen.emitManagedBeginBorrow(Loc, Base.getValue());
  SILValue StructExtract =
      SILBuilder::createStructExtract(Loc, BorrowedBase.getValue(), Decl);
  return ManagedValue::forUnmanaged(StructExtract);
}

ManagedValue SILGenBuilder::createCopyValue(SILLocation Loc,
                                            ManagedValue OriginalValue) {
  SILValue Result = SILBuilder::createCopyValue(Loc, OriginalValue.getValue());
  return gen.emitManagedRValueWithCleanup(Result);
}

ManagedValue SILGenBuilder::createCopyUnownedValue(SILLocation Loc,
                                                   ManagedValue OriginalValue) {
  auto UnownedType = OriginalValue.getType().castTo<UnownedStorageType>();
  assert(UnownedType->isLoadable(ResilienceExpansion::Maximal));
  (void)UnownedType;

  SILValue Result =
      SILBuilder::createCopyUnownedValue(Loc, OriginalValue.getValue());
  return gen.emitManagedRValueWithCleanup(Result);
}

ManagedValue
SILGenBuilder::createUnsafeCopyUnownedValue(SILLocation Loc,
                                            ManagedValue OriginalValue) {
  auto UnmanagedType = OriginalValue.getType().getAs<UnmanagedStorageType>();
  SILValue Result = SILBuilder::createUnmanagedToRef(
      Loc, OriginalValue.getValue(),
      SILType::getPrimitiveObjectType(UnmanagedType.getReferentType()));
  SILBuilder::createUnmanagedRetainValue(Loc, Result);
  return gen.emitManagedRValueWithCleanup(Result);
}

ManagedValue SILGenBuilder::createOwnedPHIArgument(SILType Type) {
  SILPHIArgument *Arg =
      getInsertionBB()->createPHIArgument(Type, ValueOwnershipKind::Owned);
  return gen.emitManagedRValueWithCleanup(Arg);
}
