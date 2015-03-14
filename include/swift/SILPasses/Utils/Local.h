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
#include "swift/SIL/SILBuilder.h"
#include "llvm/ADT/SmallPtrSet.h"

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

  /// Returns true if debug values propagate liveness.
  ///
  /// TODO: Once all passes have been audited to handle debug values correctly
  /// in their white lists, this will no longer be necessary and should be
  /// removed.
  bool debugValuesPropagateLiveness();

  /// \brief Perform a fast local check to see if the instruction is dead.
  ///
  /// This routine only examines the state of the instruction at hand.
  bool isInstructionTriviallyDead(SILInstruction *I);

  /// \brief Recursively erase all of the uses of the instruction (but not the
  /// instruction itself) and delete instructions that will become trivially
  /// dead when this instruction is removed.
  void eraseUsesOfInstruction(SILInstruction *Inst);

  // Rewrite a call, which may previously have been a dynmaic dispath, to a
  // known function reference.
  void replaceWithSpecializedFunction(ApplyInst *AI, SILFunction *NewF);

  /// \brief Return true if the substitution map contains a
  /// substitution that is an unbound generic type.
  bool hasUnboundGenericTypes(TypeSubstitutionMap &SubsMap);

  /// Return true if the substitution list contains a substitution
  /// that is an unbound generic.
  bool hasUnboundGenericTypes(ArrayRef<Substitution> Subs);

  /// \brief Move an ApplyInst's FuncRef so that it dominates the call site.
  void placeFuncRef(ApplyInst *AI, DominanceInfo *DT);

  /// \brief Add an argument, \p val, to the branch-edge that is pointing into
  /// block \p Dest. Return a new instruction and do not erase the old
  /// instruction.
  TermInst *addArgumentToBranch(SILValue Val, SILBasicBlock *Dest,
                                TermInst *Branch);

  /// Handle the mechanical aspects of removing an unreachable block.
  void removeDeadBlock(SILBasicBlock *BB);

  /// Remove all instructions in the body of \p BB in safe manner by using
  /// undef.
  void clearBlockBody(SILBasicBlock *BB);

  /// \brief Get the linkage to be used for specializations of a function with
  /// the given linkage.
  SILLinkage getSpecializedLinkage(SILLinkage L);

  /// Tries to optimize a given apply instruction if it is a concatenation of
  /// string literals. Returns a new instruction if optimization was possible.
  SILInstruction *tryToConcatenateStrings(ApplyInst *AI, SILBuilder &B);

  /// If Closure is a partial_apply or thin_to_thick_function with only local
  /// ref count users and a set of post-dominating releases:
  ///
  /// 1. Remove all ref count operations and the closure.
  /// 2. Add each one of the last release locations insert releases for the
  ///    captured args if we have a partial_apply.
  ///
  /// In the future this should be extended to be less conservative with users.
  bool tryDeleteDeadClosure(SILInstruction *Closure);

  /// This helper class represents the lifetime of a single
  /// SILValue. The value itself is held and the lifetime endpoints of
  /// that value are computed.
  class LifetimeTracker {
    SILValue TheValue;

    llvm::SmallPtrSet<SILInstruction *, 4> Endpoints;

    bool LifetimeComputed = false;

  public:
    LifetimeTracker(SILValue Value) : TheValue(Value) { }

    using EndpointRange =
      Range<llvm::SmallPtrSetImpl<SILInstruction *>::iterator>;

    SILValue getStart() { return TheValue; }

    EndpointRange getEndpoints() {
      if (!LifetimeComputed)
        computeLifetime();

      return EndpointRange(Endpoints.begin(), Endpoints.end());
    }

  private:
    void computeLifetime();
  };

  /// \brief This is a helper class used to optimize casts.
  class CastOptimizer {
    // Callback to be called when uses of an instruction should be replaced.
    std::function<void (SILInstruction *I, ValueBase *V)> ReplaceInstUsesAction;

    // Callback to call when an instruction needs to be erased.
    std::function<void (SILInstruction *)> EraseInstAction;

    // Callback to call after an optimization was performed based on the fact
    // that a cast will succeed.
    std::function<void ()> WillSucceedAction;

    // Callback to call after an optimization was performed based on the fact
    // that a cast will fail.
    std::function<void ()> WillFailAction;

  public:
    CastOptimizer(std::function<void (SILInstruction *I, ValueBase *V)> ReplaceInstUsesAction,
                  std::function<void (SILInstruction *)> EraseAction = [](SILInstruction*){},
                  std::function<void ()> WillSucceedAction = [](){},
                  std::function<void ()> WillFailAction = [](){})
  : ReplaceInstUsesAction(ReplaceInstUsesAction),
    EraseInstAction(EraseAction),
    WillSucceedAction(WillSucceedAction),
    WillFailAction(WillFailAction) {}

    /// Simplify checked_cast_br. It may change the control flow.
    SILInstruction *
    simplifyCheckedCastBranchInst(CheckedCastBranchInst *Inst);

    /// Simplify checked_cast_addr_br. It may change the control flow.
    SILInstruction *
    simplifyCheckedCastAddrBranchInst(CheckedCastAddrBranchInst *Inst);

    /// Optimize checked_cast_br. This cannot change the control flow.
    SILInstruction *
    optimizeCheckedCastBranchInst(CheckedCastBranchInst *Inst);

    /// Optimize checked_cast_addr__br. This cannot change the control flow.
    SILInstruction *
    optimizeCheckedCastAddrBranchInst(CheckedCastAddrBranchInst *Inst);

    /// Optimize unconditional_checked_cast.
    /// This cannot change the control flow.
    SILInstruction *
    optimizeUnconditionalCheckedCastInst(UnconditionalCheckedCastInst *Inst);

    /// Optimize unconditional_checked_cast_addr.
    /// This cannot change the control flow.
    SILInstruction *
    optimizeUnconditionalCheckedCastAddrInst(UnconditionalCheckedCastAddrInst *Inst);
  };

} // end namespace swift

#endif
