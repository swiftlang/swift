//===--- SILOpenedArchetypesTracker.h - Track opened archetypes -*- C++ -*-===//
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

#ifndef SWIFT_SIL_SILOPENEDARCHETYPESTRACKER_H
#define SWIFT_SIL_SILOPENEDARCHETYPESTRACKER_H

#include "swift/SIL/Notifications.h"
#include "swift/SIL/SILModule.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILUndef.h"

namespace swift {

/// SILOpenedArchetypesTracker is a helper class that can be used to create
/// and maintain a mapping from opened archetypes to instructions
/// defining them, e.g. open_existential_ref, open_existential_addr,
/// open_existential_metatype.
///
/// This information is useful for representing and maintaining the
/// dependencies of instructions on opened archetypes they are using.
///
/// The intended clients of this class are SILGen, SIL deserializers, etc.
class SILOpenedArchetypesTracker : public DeleteNotificationHandler {
public:
  typedef llvm::DenseMap<ArchetypeType *, SILValue> OpenedArchetypeDefsMap;

  SILOpenedArchetypesTracker(SILOpenedArchetypesTracker &Tracker)
      : SILOpenedArchetypesTracker(Tracker.F, Tracker) {}

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


  const SILFunction &getFunction() const { return F; }

  // Register a definition of a given opened archetype.
  void addOpenedArchetypeDef(ArchetypeType *archetype, SILValue Def);

  void removeOpenedArchetypeDef(ArchetypeType *archetype, SILValue Def) {
    auto FoundDef = getOpenedArchetypeDef(archetype);
    assert(FoundDef &&
           "Opened archetype definition is not registered in SILFunction");
    if (FoundDef == Def)
      OpenedArchetypeDefs.erase(archetype);
  }

  // Return the SILValue defining a given archetype.
  // If the defining value is not known, return an empty SILValue.
  SILValue getOpenedArchetypeDef(ArchetypeType *archetype) const {
    return OpenedArchetypeDefs.lookup(archetype);
  }

  const OpenedArchetypeDefsMap &getOpenedArchetypeDefs() const {
    return OpenedArchetypeDefs;
  }

  // Register archetypes opened by a given instruction.
  // Can be used to incrementally populate the mapping, e.g.
  // if it is done when performing a scan of all instructions
  // inside a function.
  void registerOpenedArchetypes(const SILInstruction *I);

  // Register opened archetypes whose definitions are referenced by
  // the typedef operands of this instruction.
  void registerUsedOpenedArchetypes(const SILInstruction *I);

  // Register opened archetypes referenced by this type, if they
  // are not registered yet. Create placeholders representing forward
  // definitions of these opened archetypes.
  void registerUsedOpenedArchetypes(Type Ty);

  // Unregister archetypes opened by a given instruction.
  // Should be only called when this instruction is to be removed.
  void unregisterOpenedArchetypes(const SILInstruction *I);

  // Returns true of some of the forward opened archetype definitions
  // are unresolved.
  bool hasUnresolvedOpenedArchetypeDefinitions();

  // Handling of instruction removal notifications.
  bool needsNotifications() { return true; }

  // Handle notifications about removals of instructions.
  void handleDeleteNotification(swift::ValueBase *Value);

  virtual ~SILOpenedArchetypesTracker() {
    // Unregister the handler.
    F.getModule().removeDeleteNotificationHandler(this);
  }

private:
  // Never copy
  SILOpenedArchetypesTracker &operator = (const SILOpenedArchetypesTracker &) = delete;
  /// The function whose opened archetypes are being tracked.
  /// Used only for verification purposes.
  const SILFunction &F;

  /// Mapping from opened archetypes to their definitions.
  OpenedArchetypeDefsMap &OpenedArchetypeDefs;
  /// Local map to be used if no other map was provided in the
  /// constructor.
  OpenedArchetypeDefsMap LocalOpenedArchetypeDefs;
};

// A state object containing information about opened archetypes.
// This information can be used by constructors of SILInstructions,
// their create methods, etc.
// The object can be configured to use different sources for providing
// archetypes, but none of those archetype sets can be modified through
// this object, which makes it essentially immutable.
class SILOpenedArchetypesState {
  // A set of opened archetypes operands for quick lookup.
  // It usually provides opened archetypes operands of the
  // instruction being currently processed.
  ArrayRef<Operand> OpenedArchetypeOperands;
  // A non-modifiable mapping provided by the tracker.
  const SILOpenedArchetypesTracker *OpenedArchetypesTracker;
public:
  SILOpenedArchetypesState(const SILOpenedArchetypesTracker *Tracker = nullptr)
      : OpenedArchetypesTracker(Tracker) {}

  SILOpenedArchetypesState(const SILOpenedArchetypesTracker &Tracker)
      : OpenedArchetypesTracker(&Tracker) { }

  void setOpenedArchetypesTracker(const SILOpenedArchetypesTracker *Tracker) {
    OpenedArchetypesTracker = Tracker;
  }

  void addOpenedArchetypeOperands(ArrayRef<Operand> Operands) {
    OpenedArchetypeOperands = Operands;
  }

  const SILOpenedArchetypesTracker *getOpenedArchetypesTracker() const {
    return OpenedArchetypesTracker;
  }

  /// Lookup the instruction defining an opened archetype by first 
  /// performing a quick lookup in the opened archetypes operands
  /// and then in the opened archetypes tracker.
  SILValue getOpenedArchetypeDef(ArchetypeType *archetypeTy) const;
};

/// Find an opened archetype defined by an instruction.
/// \returns The found archetype or empty type otherwise.
ArchetypeType *getOpenedArchetypeOf(const SILInstruction *I);

/// Find an opened archetype represented by this type.
/// It is assumed by this method that the type contains
/// at most one opened archetype.
/// Typically, it would be called from a type visitor.
/// It checks only the type itself, but does not try to
/// recursively check any children of this type, because
/// this is the task of the type visitor invoking it.
/// \returns The found archetype or empty type otherwise.
ArchetypeType *getOpenedArchetypeOf(Type Ty);

} // end swift namespace
#endif
