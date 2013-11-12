//===--- Local.h - Local SIL transformations. -------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SILPASSES_UTILS_LOCAL_H
#define SWIFT_SILPASSES_UTILS_LOCAL_H

#include "swift/SIL/SILInstruction.h"

namespace swift {

  /// \brief If the given instruction is dead, delete it along with its dead
  /// operands.
  ///
  /// \param I The instruction to be deleted.
  /// \param Force If Force is set, don't check if the top level instruction is
  ///        considered dead - delete it regardless.
  /// \return Returns true if any instructions were deleted.
  bool recursivelyDeleteTriviallyDeadInstructions(SILInstruction *I,
                                                  bool Force = false);

  /// \brief Perform a fast local check to see if the instruction is dead,,
  /// assuming all uses of the instruction are dead.
  ///
  /// This routine only examines the state of the instruction at hand.
  bool isInstructionTriviallyDeadWithoutUses(SILInstruction *I);

  /// \brief Perform a fast local check to see if the instruction is dead.
  ///
  /// This routine only examines the state of the instruction at hand.
  bool isInstructionTriviallyDead(SILInstruction *I);

  /// \brief Try to simplify the specified instruction, performing local
  /// analysis of the operands of the instruction, without looking at its uses
  /// (e.g. constant folding).  If a simpler result can be found, it is
  /// returned, otherwise a null SILValue is returned.
  ///
  SILValue simplifyInstruction(SILInstruction *I);

} // end namespace swift

#endif
