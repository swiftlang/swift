//===--- SILInstructionWorklist.h -------------------------------*- C++ -*-===//
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
///
/// \file
///
/// When visiting the instructions in a function/basic block, one often modifies
/// the list of instructions to be visited.  That fact introduces complexity in
/// the form of ensuring that no effort is wasted moving items down in the list
/// when an item is removed from it and also in the form of ensuring that no
/// instructions which have been modified/deleted are visited.
///
/// The SILInstructionWorklist manages that complexity.  It is responsible for
/// ensuring that removing an instruction is not unnecessarily expensive and
/// that only valid instructions are removed from the list.
///
//===----------------------------------------------------------------------===//

#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILValue.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/SmallVector.h"

namespace swift {

/// Manages a list of instructions awaiting visitation.
class SILInstructionWorklist final {
  llvm::SmallVector<SILInstruction *, 256> worklist;
  llvm::DenseMap<SILInstruction *, unsigned> worklistMap;
  StringRef loggingName;

  void operator=(const SILInstructionWorklist &rhs) = delete;
  SILInstructionWorklist(const SILInstructionWorklist &worklist) = delete;

public:
  SILInstructionWorklist(const char *loggingName = "InstructionWorklist")
      : loggingName(loggingName) {}

  /// Returns true if the worklist is empty.
  bool isEmpty() const { return worklist.empty(); }

  /// Add the specified instruction to the worklist if it isn't already in it.
  void add(SILInstruction *instruction);

  /// If the given ValueBase is a SILInstruction add it to the worklist.
  void addValue(ValueBase *value) {
    if (auto *instruction = value->getDefiningInstruction())
      add(instruction);
  }

  /// Add the given list of instructions in reverse order to the worklist. This
  /// routine assumes that the worklist is empty and the given list has no
  /// duplicates.
  void addInitialGroup(ArrayRef<SILInstruction *> list);

  // If instruction is in the worklist, remove it.
  void remove(SILInstruction *instruction) {
    auto iterator = worklistMap.find(instruction);
    if (iterator == worklistMap.end())
      return; // Not in worklist.

    // Don't bother moving everything down, just null out the slot. We will
    // check before we process any instruction if it is null.
    worklist[iterator->second] = nullptr;
    worklistMap.erase(iterator);
  }

  /// Remove the top element from the worklist.
  SILInstruction *removeOne() {
    SILInstruction *instruction = worklist.pop_back_val();
    worklistMap.erase(instruction);
    return instruction;
  }

  /// When an instruction has been simplified, add all of its users to the
  /// worklist, since additional simplifications of its users may have been
  /// exposed.
  void addUsersToWorklist(ValueBase *instruction) {
    for (auto *use : instruction->getUses())
      add(use->getUser());
  }

  void addUsersToWorklist(SILValue value) {
    for (auto *use : value->getUses())
      add(use->getUser());
  }

  /// When an instruction has been simplified, add all of its users to the
  /// worklist, since additional simplifications of its users may have been
  /// exposed.
  void addUsersOfAllResultsToWorklist(SILInstruction *instruction) {
    for (auto result : instruction->getResults()) {
      addUsersToWorklist(result);
    }
  }

  /// Check that the worklist is empty and nuke the backing store for the map if
  /// it is large.
  void zap() {
    assert(worklistMap.empty() && "Worklist empty, but the map is not?");

    // Do an explicit clear, this shrinks the map if needed.
    worklistMap.clear();
  }
};

} // end namespace swift
