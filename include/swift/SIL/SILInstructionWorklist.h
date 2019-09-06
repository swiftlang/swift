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

#include "swift/Basic/BlotSetVector.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILValue.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/SmallVector.h"

namespace swift {

class SILInstructionWorklistBase {
  const char *loggingName;

protected:
  SILInstructionWorklistBase(const char *loggingName)
      : loggingName(loggingName){};
  void withDebugStream(
      std::function<void(llvm::raw_ostream &stream, const char *loggingName)>
          perform);
};

/// Manages a list of instructions awaiting visitation.
template <typename VectorT = std::vector<SILInstruction *>,
          typename MapT = llvm::DenseMap<SILInstruction *, unsigned>>
class SILInstructionWorklist : SILInstructionWorklistBase {
  BlotSetVector<SILInstruction *, VectorT, MapT> worklist;

  void operator=(const SILInstructionWorklist &rhs) = delete;
  SILInstructionWorklist(const SILInstructionWorklist &worklist) = delete;

public:
  SILInstructionWorklist(const char *loggingName = "InstructionWorklist")
      : SILInstructionWorklistBase(loggingName) {}

  /// Returns true if the worklist is empty.
  bool isEmpty() const { return worklist.empty(); }

  /// Add the specified instruction to the worklist if it isn't already in it.
  void add(SILInstruction *instruction) {
    if (worklist.insert(instruction).second) {
      withDebugStream([&](llvm::raw_ostream &stream, StringRef loggingName) {
        stream << loggingName << ": ADD: " << *instruction << '\n';
      });
    }
  }

  /// If the given SILValue is a SILInstruction add it to the worklist.
  void addValue(SILValue value) {
    if (auto *instruction = value->getDefiningInstruction()) {
      add(instruction);
    }
  }

  /// Add the given list of instructions in reverse order to the worklist. This
  /// routine assumes that the worklist is empty and the given list has no
  /// duplicates.
  void addInitialGroup(ArrayRef<SILInstruction *> list) {
    assert(worklist.empty() && "worklist must be empty to add initial group");
    worklist.reserve(list.size() + 16);
    withDebugStream([&](llvm::raw_ostream &stream, StringRef loggingName) {
      stream << loggingName << ": ADDING: " << list.size()
             << " instrs to worklist\n";
    });
    while (!list.empty()) {
      SILInstruction *instruction = list.back();
      list = list.drop_back();
      worklist.insert(instruction);
    }
  }

  // If instruction is in the worklist, remove it.
  void erase(SILInstruction *instruction) { worklist.erase(instruction); }

  /// Remove the top element from the worklist.
  SILInstruction *pop_back_val() {
    return worklist.pop_back_val().getValueOr(nullptr);
  }

  /// When an instruction has been simplified, add all of its users to the
  /// worklist, since additional simplifications of its users may have been
  /// exposed.
  void addUsersToWorklist(SILValue value) {
    for (auto *use : value->getUses()) {
      add(use->getUser());
    }
  }

  /// When an instruction has been simplified, add all of its users to the
  /// worklist, since additional simplifications of its users may have been
  /// exposed.
  void addUsersOfAllResultsToWorklist(SILInstruction *instruction) {
    for (auto result : instruction->getResults()) {
      addUsersToWorklist(&*result);
    }
  }

  /// Check that the worklist is empty and nuke the backing store if it is
  /// large.
  void resetChecked() {
    assert(worklist.empty() && "Vector empty, but the map is not?");

    // Do an explicit clear, shrinking the storage if needed.
    worklist.clear();
  }
};

// TODO: This name is somewhat unpleasant.  Once the type is templated over its
//       storage and renamed BlottableWorklist, this type will be
//       SmallBlottableWorklist which is more reasonable.
template <unsigned N>
class SmallSILInstructionWorklist final
    : public SILInstructionWorklist<
          llvm::SmallVector<Optional<SILInstruction *>, N>,
          // TODO: A DenseMap rather than a SmallDenseMap is used here to avoid
          //       running into an upstream problem with the handling of grow()
          //       when it results in rehashing and tombstone removal:
          //
          //       https://reviews.llvm.org/D56455
          llvm::DenseMap<SILInstruction *, unsigned>> {
public:
  using VectorT = llvm::SmallVector<Optional<SILInstruction *>, N>;
  using MapT = llvm::DenseMap<SILInstruction *, unsigned>;
  SmallSILInstructionWorklist(const char *loggingName = "InstructionWorklist")
      : SILInstructionWorklist<VectorT, MapT>(loggingName) {}
};

} // end namespace swift
