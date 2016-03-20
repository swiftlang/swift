//===--- InstructionUtils.h - Utilities for SIL instructions ----*- C++ -*-===//
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

#ifndef SWIFT_SIL_INSTRUCTIONUTILS_H
#define SWIFT_SIL_INSTRUCTIONUTILS_H

#include "swift/SIL/SILInstruction.h"
#include "llvm/ADT/SetVector.h"

namespace swift {

/// Strip off casts/indexing insts/address projections from V until there is
/// nothing left to strip.
SILValue getUnderlyingObject(SILValue V);

SILValue stripSinglePredecessorArgs(SILValue V);

/// Return the underlying SILValue after stripping off all casts from the
/// current SILValue.
SILValue stripCasts(SILValue V);

/// Return the underlying SILValue after stripping off all upcasts from the
/// current SILValue.
SILValue stripUpCasts(SILValue V);

/// Return the underlying SILValue after stripping off all
/// upcasts and downcasts.
SILValue stripClassCasts(SILValue V);

/// Return the underlying SILValue after stripping off all address projection
/// instructions.
SILValue stripAddressProjections(SILValue V);

/// Return the underlying SILValue after stripping off all address projection
/// instructions which have a single operand.
SILValue stripUnaryAddressProjections(SILValue V);

/// Return the underlying SILValue after stripping off all aggregate projection
/// instructions.
///
/// An aggregate projection instruction is either a struct_extract or a
/// tuple_extract instruction.
SILValue stripValueProjections(SILValue V);

/// Return the underlying SILValue after stripping off all indexing
/// instructions.
///
/// An indexing inst is either index_addr or index_raw_pointer.
SILValue stripIndexingInsts(SILValue V);

/// Returns the underlying value after stripping off a builtin expect
/// intrinsic call.
SILValue stripExpectIntrinsic(SILValue V);

/// A RAII struct that removes instructions from their parent block but does not
/// delete them until the RAII struct is torn down.
class DelayedInstructionDestroyer {
  /// The module that we used to allocate/deallocate memory.
  SILModule &Mod;

  // TODO: We /could/ use an inst list here perhaps. Then we could do quick
  // transfering of instructions via splice.
  llvm::SmallSetVector<SILInstruction *, 32> Insts;

public:
  DelayedInstructionDestroyer(SILModule &Mod) : Mod(Mod), Insts() {}
  DelayedInstructionDestroyer(const DelayedInstructionDestroyer &) = delete;
  DelayedInstructionDestroyer(DelayedInstructionDestroyer &&) = delete;
  ~DelayedInstructionDestroyer();

  /// Clean up \p I by removing it from its parent iplist and destroying I. Do
  /// not deallocate I's memory though.
  void remove(SILInstruction *I);

  /// \returns true if \p I is an instruction that was removed from its parent
  /// and that will be deleted.
  bool count(SILInstruction *I) {
    return Insts.count(I);
  }
};

} // end namespace swift

#endif
