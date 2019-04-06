//===--- SILGenSILBuilder.cpp ---------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "SILGenSILBuilder.h"
#include "SILGenFunction.h"

using namespace swift;
using namespace Lowering;

//===----------------------------------------------------------------------===//
//                              Utility Methods
//===----------------------------------------------------------------------===//

SILGenModule &SILGenSILBuilder::getSILGenModule() const { return SGF.SGM; }

//===----------------------------------------------------------------------===//
//                                Constructors
//===----------------------------------------------------------------------===//

SILGenSILBuilder::SILGenSILBuilder(SILGenFunction &SGF)
    : SILBuilder(SGF.F), SGF(SGF) {}

SILGenSILBuilder::SILGenSILBuilder(
    SILGenFunction &SGF, SILBasicBlock *insertBB,
    SmallVectorImpl<SILInstruction *> *insertedInsts)
    : SILBuilder(insertBB, insertedInsts), SGF(SGF) {}

SILGenSILBuilder::SILGenSILBuilder(SILGenFunction &SGF, SILBasicBlock *insertBB,
                                   SILBasicBlock::iterator insertInst)
    : SILBuilder(insertBB, insertInst), SGF(SGF) {}

//===----------------------------------------------------------------------===//
//              Instruction Emission + Conformance Endowed APIs
//===----------------------------------------------------------------------===//
//
// This section contains wrappers around SILBuilder SILValue APIs that add extra
// conformances. These are the only places where we should be accessing
// SILBuilder APIs directly.
//

MetatypeInst *SILGenSILBuilder::createMetatype(SILLocation loc,
                                               SILType metatype) {
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
      auto *decl = t->getAnyNominal();
      if (!decl)
        return false;

      if (isa<ProtocolDecl>(decl))
        return false;

      auto *genericSig = decl->getGenericSignature();
      if (!genericSig)
        return false;

      auto subMap =
          t->getContextSubstitutionMap(getSILGenModule().SwiftModule, decl);
      getSILGenModule().useConformancesFromSubstitutions(subMap);
      return false;
    });

    break;
  }
  }

  return SILBuilder::createMetatype(loc, metatype);
}

ApplyInst *SILGenSILBuilder::createApply(SILLocation loc, SILValue fn,
                                         SILType substFnTy, SILType result,
                                         SubstitutionMap subs,
                                         ArrayRef<SILValue> args) {
  getSILGenModule().useConformancesFromSubstitutions(subs);
  return SILBuilder::createApply(loc, fn, subs, args, false);
}

TryApplyInst *SILGenSILBuilder::createTryApply(
    SILLocation loc, SILValue fn, SILType substFnTy, SubstitutionMap subs,
    ArrayRef<SILValue> args, SILBasicBlock *normalBB, SILBasicBlock *errorBB) {
  getSILGenModule().useConformancesFromSubstitutions(subs);
  return SILBuilder::createTryApply(loc, fn, subs, args, normalBB, errorBB);
}

BeginApplyInst *SILGenSILBuilder::createBeginApply(SILLocation loc, SILValue fn,
                                                   SubstitutionMap subs,
                                                   ArrayRef<SILValue> args) {
  getSILGenModule().useConformancesFromSubstitutions(subs);
  return SILBuilder::createBeginApply(loc, fn, subs, args, false);
}

PartialApplyInst *SILGenSILBuilder::createPartialApply(
    SILLocation loc, SILValue fn, SILType substFnTy, SubstitutionMap subs,
    ArrayRef<SILValue> args, SILType closureTy) {
  getSILGenModule().useConformancesFromSubstitutions(subs);
  return SILBuilder::createPartialApply(
      loc, fn, subs, args,
      closureTy.getAs<SILFunctionType>()->getCalleeConvention());
}

BuiltinInst *SILGenSILBuilder::createBuiltin(SILLocation loc, Identifier name,
                                             SILType resultTy,
                                             SubstitutionMap subs,
                                             ArrayRef<SILValue> args) {
  getSILGenModule().useConformancesFromSubstitutions(subs);
  return SILBuilder::createBuiltin(loc, name, resultTy, subs, args);
}

InitExistentialAddrInst *SILGenSILBuilder::createInitExistentialAddr(
    SILLocation loc, SILValue existential, CanType formalConcreteType,
    SILType loweredConcreteType,
    ArrayRef<ProtocolConformanceRef> conformances) {
  for (auto conformance : conformances)
    getSILGenModule().useConformance(conformance);

  return SILBuilder::createInitExistentialAddr(
      loc, existential, formalConcreteType, loweredConcreteType, conformances);
}

InitExistentialValueInst *SILGenSILBuilder::createInitExistentialValue(
    SILLocation Loc, SILType ExistentialType, CanType FormalConcreteType,
    SILValue Concrete, ArrayRef<ProtocolConformanceRef> Conformances) {
  for (auto conformance : Conformances)
    getSILGenModule().useConformance(conformance);

  return SILBuilder::createInitExistentialValue(
      Loc, ExistentialType, FormalConcreteType, Concrete, Conformances);
}

InitExistentialMetatypeInst *SILGenSILBuilder::createInitExistentialMetatype(
    SILLocation loc, SILValue metatype, SILType existentialType,
    ArrayRef<ProtocolConformanceRef> conformances) {
  for (auto conformance : conformances)
    getSILGenModule().useConformance(conformance);

  return SILBuilder::createInitExistentialMetatype(
      loc, metatype, existentialType, conformances);
}

InitExistentialRefInst *SILGenSILBuilder::createInitExistentialRef(
    SILLocation loc, SILType existentialType, CanType formalConcreteType,
    SILValue concreteValue, ArrayRef<ProtocolConformanceRef> conformances) {
  for (auto conformance : conformances)
    getSILGenModule().useConformance(conformance);

  return SILBuilder::createInitExistentialRef(
      loc, existentialType, formalConcreteType, concreteValue, conformances);
}

AllocExistentialBoxInst *SILGenSILBuilder::createAllocExistentialBox(
    SILLocation loc, SILType existentialType, CanType concreteType,
    ArrayRef<ProtocolConformanceRef> conformances) {
  for (auto conformance : conformances)
    getSILGenModule().useConformance(conformance);

  return SILBuilder::createAllocExistentialBox(loc, existentialType,
                                               concreteType, conformances);
}
