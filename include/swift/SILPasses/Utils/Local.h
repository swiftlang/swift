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
  class DominanceInfo;

  /// \brief For each of the given instructions, if they are dead delete them
  /// along with their dead operands.
  ///
  /// \param I The instruction to be deleted.
  /// \param Force If Force is set, don't check if the top level instructions
  ///        are considered dead - delete them regardless.
  /// \param C a callback called whenever an instruction is deleted.
  /// \return Returns true if any instructions were deleted.
  bool
  recursivelyDeleteTriviallyDeadInstructions(
    ArrayRef<SILInstruction*> I, bool Force = false,
    std::function<void(SILInstruction *)> C = [](SILInstruction *){});

  /// \brief If the given instruction is dead, delete it along with its dead
  /// operands.
  ///
  /// \param I The instruction to be deleted.
  /// \param Force If Force is set, don't check if the top level instruction is
  ///        considered dead - delete it regardless.
  /// \param C a callback called whenever an instruction is deleted.
  /// \return Returns true if any instructions were deleted.
  bool
  recursivelyDeleteTriviallyDeadInstructions(
    SILInstruction *I,
    bool Force = false,
    std::function<void(SILInstruction *)> C = [](SILInstruction *){});

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

  /// \brief Recursively erase all of the uses of the instruction (but not the
  /// instruction itself) and delete instructions that will become trivially
  /// dead when this instruction is removed.
  void eraseUsesOfInstruction(SILInstruction *Inst);

  /// Does the passed in BuiltinFunctionRefInst have any side effects?
  bool isSideEffectFree(BuiltinFunctionRefInst *FR);

  /// Does the passed in BuiltinFunctionRefInst touch memory at all?
  bool isReadNone(BuiltinFunctionRefInst *FR);

  /// Does the passed in FunctionRefInst touch memory at all?
  bool isReadNone(FunctionRefInst *FR);

  // Rewrite a call, which may previously have been a dynmaic dispath, to a
  // known function reference.
  void replaceWithSpecializedFunction(ApplyInst *AI, SILFunction *NewF);

  /// \brief Return true if the substitution map contains a
  /// substitution that is an unbound generic type.
  bool hasUnboundGenericTypes(TypeSubstitutionMap &SubsMap);

  /// Return true if the substitution list contains a substitution
  /// that is an unbound generic.
  bool hasUnboundGenericTypes(ArrayRef<Substitution> Subs);

  /// Return true if the substitution list contains a substitution
  /// that is any existential type.
  bool hasAnyExistentialTypes(ArrayRef<Substitution> Subs);

  /// \brief Move an ApplyInst's FuncRef so that it dominates the call site.
  void placeFuncRef(ApplyInst *AI, DominanceInfo *DT);

  /// \brief Add an argument, \p val, to the branch-edge that is pointing into
  /// block \p Dest. Return a new instruction and do not erase the old
  /// instruction.
  TermInst *addArgumentToBranch(SILValue Val, SILBasicBlock *Dest,
                                TermInst *Branch);

  /// \brief Get the linkage to be used for specializations of a function with
  /// the given linkage.
  SILLinkage getSpecializedLinkage(SILLinkage L);

} // end namespace swift

#endif
