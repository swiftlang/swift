//===- SILOpenedArchetypeTracker.h - Track opened archetypes  ---*- C++ -*-===//
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

#ifndef SWIFT_SIL_SILOPENEDARCHETYPESTRACKER_H
#define SWIFT_SIL_SILOPENEDARCHETYPESTRACKER_H

#include "swift/SIL/Notifications.h"
#include "swift/SIL/SILModule.h"
#include "swift/SIL/SILFunction.h"

namespace swift {

/// SILOpenedArchetypesTracker is a helper class that can be used to create
/// and maintain a mapping from opened archetypes to instructions
/// defining them, e.g. open_existential_ref, open_existential_addr,
/// open_existential_metatype.
///
/// This information is useful for representing and maintaining the
/// dependencies of instructions on opened archetypes they are using.
class SILOpenedArchetypesTracker : public DeleteNotificationHandler {
public:
  typedef llvm::DenseMap<Type, SILValue> OpenedArchetypeDefsMap;
  // Re-use pre-populated map if available.
  SILOpenedArchetypesTracker(const SILFunction &F,
                             SILOpenedArchetypesTracker &Tracker)
      : F(F), OpenedArchetypeDefs(Tracker.OpenedArchetypeDefs) { }

  // Re-use pre-populated map if available.
  SILOpenedArchetypesTracker(const SILFunction &F,
                             OpenedArchetypeDefsMap &OpenedArchetypeDefs)
      : F(F), OpenedArchetypeDefs(OpenedArchetypeDefs) { }

  // Use its own local map if no pre-populated map is provided.
  SILOpenedArchetypesTracker(const SILFunction &F)
      : F(F), OpenedArchetypeDefs(LocalOpenedArchetypeDefs) { }


  const SILFunction &getFunction() { return F; }

  void addOpenedArchetypeDef(Type archetype, SILValue Def) {
    assert(!getOpenedArchetypeDef(archetype) &&
           "There can be only one definition of an opened archetype");
    OpenedArchetypeDefs[archetype] = Def;
  }

  void removeOpenedArchetypeDef(Type archetype, SILValue Def) {
    auto FoundDef = getOpenedArchetypeDef(archetype);
    assert(FoundDef &&
           "Opened archetype definition is not registered in SILFunction");
    if (FoundDef == Def)
      OpenedArchetypeDefs.erase(archetype);
  }

  SILValue getOpenedArchetypeDef(Type archetype) const {
    return OpenedArchetypeDefs.lookup(archetype);
  }

  const OpenedArchetypeDefsMap &getOpenedArchetypeDefs() {
    return OpenedArchetypeDefs;
  }

  // Register archetypes opened by a given instruction.
  // Can be used to incrementally populate the mapping, e.g.
  // if it is done when performing a scan of all instructions
  // inside a function.
  void registerOpenedArchetypes(const SILInstruction *I) {
    assert((!I->getParent() || I->getFunction() == &F) &&
           "Instruction does not belong to a proper SILFunction");
    if (isa<OpenExistentialAddrInst>(I) || isa<OpenExistentialRefInst>(I) ||
        isa<OpenExistentialBoxInst>(I)) {
      auto Ty = I->getType().getSwiftRValueType();
      assert(Ty->isOpenedExistential() && "Type should be an opened archetype");
      addOpenedArchetypeDef(Ty, I);
    } else if (isa<OpenExistentialMetatypeInst>(I)) {
      SILType InstanceTy = I->getType();
      while (isa<AnyMetatypeType>(InstanceTy.getSwiftRValueType())) {
        InstanceTy = InstanceTy.getMetatypeInstanceType(I->getModule());
      }
      auto Ty = InstanceTy.getSwiftRValueType();
      assert(Ty->isOpenedExistential() && "Type should be an opened archetype");
      addOpenedArchetypeDef(Ty, I);
    }
  }

  // Unregister archetypes opened by a given instruction.
  // Should be only called when this instruction is to be removed.
  void unregisterOpenedArchetypes(const SILInstruction *I) {
    assert(I->getFunction() == &F &&
           "Instruction does not belong to a proper SILFunction");
    if (isa<OpenExistentialAddrInst>(I) || isa<OpenExistentialRefInst>(I) ||
        isa<OpenExistentialBoxInst>(I)) {
      auto Ty = I->getType().getSwiftRValueType();
      assert(Ty->isOpenedExistential() && "Type should be an opened archetype");
      removeOpenedArchetypeDef(Ty, I);
    } else if (isa<OpenExistentialMetatypeInst>(I)) {
      SILType InstanceTy = I->getType();
      while (isa<AnyMetatypeType>(InstanceTy.getSwiftRValueType())) {
        InstanceTy = InstanceTy.getMetatypeInstanceType(I->getModule());
      }
      auto Ty = InstanceTy.getSwiftRValueType();
      assert(Ty->isOpenedExistential() && "Type should be an opened archetype");
      removeOpenedArchetypeDef(Ty, I);
    }
  }

  // Pre-populate the map by scanning the whole function for any
  // instructions creating new opened archetypes.
  void collectOpenedArchetypesOfFunction() {
    for (auto FI = F.begin(), FE = F.end(); FI != FE; ++FI) {
      for (auto I = FI->begin(), E = FI->end(); I != E; ++I) {
        registerOpenedArchetypes(&*I);
      }
    }
  }

  // Handling of instruction removal notifications.
  bool needsNotifications() { return true; }

  void handleDeleteNotification(swift::ValueBase *Value) {
    if (auto I = dyn_cast<SILInstruction>(Value))
      if (I->getFunction() == &F)
        unregisterOpenedArchetypes(I);
  }

  virtual ~SILOpenedArchetypesTracker() {
    // Unregister the handler.
    F.getModule().removeDeleteNotificationHandler(this);
  }

private:
  const SILFunction &F;
  /// Mapping from opened archetypes to their definitions
  OpenedArchetypeDefsMap &OpenedArchetypeDefs;
  // Local map to be used if no other map was provided in the
  // constructor.
  OpenedArchetypeDefsMap LocalOpenedArchetypeDefs;
};

} // end swift namespace
#endif
