//==- SILOpenedArchetypeTracker.cpp - Track opened archetypes  ---*- C++ -*-==//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/SIL/SILOpenedArchetypesTracker.h"

using namespace swift;

// Register archetypes opened by a given instruction.
// Can be used to incrementally populate the mapping, e.g.
// if it is done when performing a scan of all instructions
// inside a function.
void SILOpenedArchetypesTracker::registerOpenedArchetypes(
    const SILInstruction *I) {
  assert((!I->getParent() || I->getFunction() == &F) &&
         "Instruction does not belong to a proper SILFunction");
  auto Archetype = getOpenedArchetypeOf(I);
  if (Archetype)
    addOpenedArchetypeDef(Archetype, I);
}

// Register opened archetypes whose definitions are referenced by
// the typedef operands of this instruction.
void SILOpenedArchetypesTracker::registerUsedOpenedArchetypes(
    const SILInstruction *I) {
  assert((!I->getParent() || I->getFunction() == &F) &&
         "Instruction does not belong to a proper SILFunction");
  for (auto &Op : I->getOpenedArchetypeOperands()) {
    auto OpenedArchetypeDef = Op.get();
    assert(isa<SILInstruction>(OpenedArchetypeDef) &&
           "typedef operand should refer to a SILInstruction");
    addOpenedArchetypeDef(
        getOpenedArchetypeOf(cast<SILInstruction>(OpenedArchetypeDef)),
        OpenedArchetypeDef);
  }
}

// Unregister archetypes opened by a given instruction.
// Should be only called when this instruction is to be removed.
void SILOpenedArchetypesTracker::unregisterOpenedArchetypes(
    const SILInstruction *I) {
  assert(I->getFunction() == &F &&
         "Instruction does not belong to a proper SILFunction");
  auto Archetype = getOpenedArchetypeOf(I);
  if (Archetype)
    removeOpenedArchetypeDef(Archetype, I);
}

void SILOpenedArchetypesTracker::handleDeleteNotification(
    swift::ValueBase *Value) {
  if (auto I = dyn_cast<SILInstruction>(Value))
    if (I->getFunction() == &F)
      unregisterOpenedArchetypes(I);
}

/// Find an opened archetype defined by an instruction.
/// \returns The found archetype or empty type otherwise.
CanType swift::getOpenedArchetypeOf(const SILInstruction *I) {
  if (isa<OpenExistentialAddrInst>(I) || isa<OpenExistentialRefInst>(I) ||
      isa<OpenExistentialBoxInst>(I) || isa<OpenExistentialMetatypeInst>(I)) {
    auto Ty = getOpenedArchetypeOf(I->getType().getSwiftRValueType());
    assert(Ty->isOpenedExistential() && "Type should be an opened archetype");
    return Ty;
  }

  return CanType();
}


bool hasAtMostOneOpenedArchetype(CanType Ty) {
  int NumOpenedArchetypes = 0;
  Ty.visit([&](Type t) {
    if (t->isOpenedExistential()) {
      NumOpenedArchetypes++;
    }
    return;
  });
  return NumOpenedArchetypes <= 1;
}

/// Find an opened archetype represented by this type.
/// It is assumed by this method that the type contains
/// at most one opened archetype.
/// Typically, it would be called from a type visitor.
/// It checks only the type itself, but does not try to
/// recursively check any children of this type, because
/// this is the task of the type visitor invoking it.
/// \returns The found archetype or empty type otherwise.
CanType swift::getOpenedArchetypeOf(CanType Ty) {
  if (!Ty)
    return CanType();
  assert(hasAtMostOneOpenedArchetype(Ty) &&
         "Type should contain at most one opened archetype");
  while (auto MetaTy = dyn_cast<AnyMetatypeType>(Ty)) {
    Ty = MetaTy->getInstanceType().getCanonicalTypeOrNull();
  }
  if (Ty->isOpenedExistential())
    return Ty;
  return CanType();
}

SILValue SILOpenedArchetypesState::getOpenedArchetypeDef(Type Ty) const {
  if (!Ty)
    return SILValue();
  auto CanTy = Ty.getCanonicalTypeOrNull();
  // First perform a quick check.
  for (auto &Op : OpenedArchetypeOperands) {
    auto Def = Op.get();
    if (getOpenedArchetypeOf(cast<SILInstruction>(Def)) == CanTy)
      return Def;
  }
  // Then use a regular lookup.
  if (OpenedArchetypesTracker)
    return OpenedArchetypesTracker->getOpenedArchetypeDef(Ty);
  return SILValue();
}
