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

  /// The kind of array operation identified by looking at the semantics attribute
  /// of the called function.
  enum class ArrayCallKind {
    kNone = 0,
    kCheckSubscript,
    kCheckIndex,
    kGetCount,
    kGetCapacity,
    kGetElement,
    kGetElementAddress,
    kMakeMutable,
    kMutateUnknown,
    kArrayInit
  };

  /// Wrapper around array semantic calls.
  class ArraySemanticsCall {
    ApplyInst *SemanticsCall;

  public:
    /// Match array semantic calls.
    ArraySemanticsCall(ValueBase *V, StringRef SemanticStr,
                       bool MatchPartialName);

    /// Match any array semantics call.
    ArraySemanticsCall(ValueBase *V) : ArraySemanticsCall(V, "array.", true) {}

    /// Match a specific array semantic call.
    ArraySemanticsCall(ValueBase *V, StringRef SemanticStr)
        : ArraySemanticsCall(V, SemanticStr, false) {}

    /// Can we hoist this call.
    bool canHoist(SILInstruction *To, DominanceInfo *DT);

    /// Determine which kind of array semantics call this is.
    ArrayCallKind getKind();

    /// Get the self argument.
    SILValue getSelf();

    /// Get the index for operations that have one.
    SILValue getIndex();

    /// Remove instruction by replacing it with a retain_value of the array
    /// argument.
    void replaceByRetainValue();

    /// Remove the instruction. This is to be used for calls that receive self
    /// by reference (and hence need no matching retain).
    void remove() { SemanticsCall->eraseFromParent(); }

    /// Hoist the call to the insert point.
    void hoist(SILInstruction *InsertBefore, DominanceInfo *DT) {
      hoistOrCopy(InsertBefore, DT, false);
    }

    /// Copy the call to the insert point and return the newly created call.
    ApplyInst *copyTo(SILInstruction *InsertBefore, DominanceInfo *DT) {
      return hoistOrCopy(InsertBefore, DT, true);
    }

    /// Get the semantics call as an ApplyInst.
    operator ApplyInst *() { return SemanticsCall; }

    /// Is this an semantics call.
    operator bool() { return SemanticsCall != nullptr; }

  protected:
    /// Hoist or copy the call to the insert point. If LeaveOriginal is true the
    /// call is copied to the insert point. Returns the copied call.
    ApplyInst *hoistOrCopy(SILInstruction *InsertBefore, DominanceInfo *DT,
                           bool LeaveOriginal);

  };

} // end namespace swift

#endif
