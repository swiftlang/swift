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
//                                Constructors
//===----------------------------------------------------------------------===//

SILGenBuilder::SILGenBuilder(SILGenFunction &gen)
    : SILBuilder(gen.F), SGM(gen.SGM) {}

SILGenBuilder::SILGenBuilder(SILGenFunction &gen, SILBasicBlock *insertBB)
    : SILBuilder(insertBB), SGM(gen.SGM) {}

SILGenBuilder::SILGenBuilder(SILGenFunction &gen, SILBasicBlock *insertBB,
                             SmallVectorImpl<SILInstruction *> *insertedInsts)
    : SILBuilder(insertBB, insertedInsts), SGM(gen.SGM) {}

SILGenBuilder::SILGenBuilder(SILGenFunction &gen, SILBasicBlock *insertBB,
                             SILBasicBlock::iterator insertInst)
    : SILBuilder(insertBB, insertInst), SGM(gen.SGM) {}

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

      auto subs = t->gatherAllSubstitutions(SGM.SwiftModule, nullptr);
      SGM.useConformancesFromSubstitutions(subs);
      return false;
    });

    break;
  }
  }

  return SILBuilder::createMetatype(loc, metatype);
}

ApplyInst *SILGenBuilder::createApply(SILLocation Loc, SILValue Fn,
                                      SILType SubstFnTy, SILType Result,
                                      ArrayRef<Substitution> Subs,
                                      ArrayRef<SILValue> Args) {
  SGM.useConformancesFromSubstitutions(Subs);
  return SILBuilder::createApply(Loc, Fn, SubstFnTy, Result, Subs, Args, false);
}

TryApplyInst *SILGenBuilder::createTryApply(SILLocation loc, SILValue Fn,
                                            SILType substFnTy,
                                            ArrayRef<Substitution> subs,
                                            ArrayRef<SILValue> args,
                                            SILBasicBlock *normalBB,
                                            SILBasicBlock *errorBB) {
  SGM.useConformancesFromSubstitutions(subs);
  return SILBuilder::createTryApply(loc, Fn, substFnTy, subs, args, normalBB,
                                    errorBB);
}

PartialApplyInst *SILGenBuilder::createPartialApply(
    SILLocation Loc, SILValue Fn, SILType SubstFnTy,
    ArrayRef<Substitution> Subs, ArrayRef<SILValue> Args, SILType ClosureTy) {
  SGM.useConformancesFromSubstitutions(Subs);
  return SILBuilder::createPartialApply(Loc, Fn, SubstFnTy, Subs, Args,
                                        ClosureTy);
}

BuiltinInst *SILGenBuilder::createBuiltin(SILLocation Loc, Identifier Name,
                                          SILType ResultTy,
                                          ArrayRef<Substitution> Subs,
                                          ArrayRef<SILValue> Args) {
  SGM.useConformancesFromSubstitutions(Subs);
  return SILBuilder::createBuiltin(Loc, Name, ResultTy, Subs, Args);
}

InitExistentialAddrInst *SILGenBuilder::createInitExistentialAddr(
    SILLocation Loc, SILValue Existential, CanType FormalConcreteType,
    SILType LoweredConcreteType,
    ArrayRef<ProtocolConformanceRef> Conformances) {
  for (auto conformance : Conformances)
    SGM.useConformance(conformance);

  return SILBuilder::createInitExistentialAddr(
      Loc, Existential, FormalConcreteType, LoweredConcreteType, Conformances);
}

InitExistentialMetatypeInst *SILGenBuilder::createInitExistentialMetatype(
    SILLocation loc, SILValue metatype, SILType existentialType,
    ArrayRef<ProtocolConformanceRef> conformances) {
  for (auto conformance : conformances)
    SGM.useConformance(conformance);

  return SILBuilder::createInitExistentialMetatype(
      loc, metatype, existentialType, conformances);
}

InitExistentialRefInst *SILGenBuilder::createInitExistentialRef(
    SILLocation Loc, SILType ExistentialType, CanType FormalConcreteType,
    SILValue Concrete, ArrayRef<ProtocolConformanceRef> Conformances) {
  for (auto conformance : Conformances)
    SGM.useConformance(conformance);

  return SILBuilder::createInitExistentialRef(
      Loc, ExistentialType, FormalConcreteType, Concrete, Conformances);
}

AllocExistentialBoxInst *SILGenBuilder::createAllocExistentialBox(
    SILLocation Loc, SILType ExistentialType, CanType ConcreteType,
    ArrayRef<ProtocolConformanceRef> Conformances) {
  for (auto conformance : Conformances)
    SGM.useConformance(conformance);

  return SILBuilder::createAllocExistentialBox(Loc, ExistentialType,
                                               ConcreteType, Conformances);
}
