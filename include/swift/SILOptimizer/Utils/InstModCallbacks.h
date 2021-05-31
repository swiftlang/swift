//===--- InstModCallbacks.h - intruction modification callbacks -*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
///
/// InstModCallbacks: callbacks for instruction modification.
///
/// Callbacks are generally problematic because a pass cannot anticipate the
/// state that SIL will be in when lower-level utilties invoke the
/// callback. This creates implicit coupling across the layers of SIL utilities.
///
/// Alternatives:
///
/// For an Analyses that caches SILInstruction pointers, check
/// SILInstruction::isDeleted() upon retrieval, and use the PassManager's
/// analysis invalidation mechanism to clear the pointers at the end of each
/// pass. The pointers remain valid in the "deleted" state until the end the
/// pass.
///
/// For iterating over instructions during instruction creation and deletion,
/// use an UpdatingInstructionIterator provided by the InstructionDeleter
/// object:
///
///   for (SILInstruction *inst : deleter.updatingRange(bb)) ...
///
/// Make sure the pass uses the same deleter object for all deletion within the
/// iterator scope.
///
/// To defer instruction deletion so that deletions happen in bulk at a
/// convenient point in the pass, use InstructionDeleter::trackIfDead() and
/// cleanupDeadInstructions().
///
/// To determine whether multiple layers of utilities made any change to the
/// SIL, have each utility report whether it made a change.
///
/// For uses that don't fall into the categories above, restructure the pass so
/// that low-level operations on individual instructions don't require
/// callbacks. The SILCombine worklist is currently the main client of
/// callbacks. It's possible to work around this by running more complex
/// utilities as a separate SILCombine subpass in between draining the worklist
/// so those utilities do not require callbacks.
///
//===----------------------------------------------------------------------===//

#include "swift/SIL/SILInstruction.h"
#include <functional>

#ifndef SWIFT_SILOPTIMIZER_UTILS_INSTMODCALLBACKS_H
#define SWIFT_SILOPTIMIZER_UTILS_INSTMODCALLBACKS_H

namespace swift {

/// A structure containing callbacks that are called when an instruction is
/// removed or added.
///
/// PERFORMANCE NOTES: This code can be used in loops, so we want to make sure
/// to not have overhead when the user does not specify a callback. To do that
/// instead of defining a "default" std::function, we represent the "default"
/// functions as nullptr. Then, in the helper function trampoline that actually
/// gets called, we check if we have a nullptr and if we do, we perform the
/// default operation inline. What is nice about this from a perf perspective is
/// that in a loop this property should predict well since you have a single
/// branch that is going to go the same way everytime.
struct InstModCallbacks {
  /// A function that is called to notify that a new function was created.
  ///
  /// Default implementation is a no-op, but we still mark madeChange.
  std::function<void(SILInstruction *newlyCreatedInst)> createdNewInstFunc;

  /// A function sets the value in \p use to be \p newValue.
  ///
  /// Default implementation just calls use->set(newValue).
  ///
  /// NOTE: It is assumed that this operation will never invalidate instruction
  /// iterators.
  ///
  /// This can have compile-time implications and should be avoided
  /// whenever possible in favor of more structured optimization passes.
  std::function<void(Operand *use, SILValue newValue)> setUseValueFunc;

  /// A function that takes in an instruction and deletes the inst.
  ///
  /// This is used to invalidate dangling instruction pointers. The SIL will be
  /// invalid when it is invoked. The callback is only allowed to inspect the
  /// inline fields of \p instToDelete and iterate over the results. It is not
  /// allowed to dereference operands or iterate uses.
  ///
  /// See comments for notifyWillBeDeletedFunc.
  ///
  /// The default implementation is:
  ///
  ///   instToDelete->eraseFromParent();
  ///
  /// The reason this callback is reponsible for deleting the instruction is to
  /// interoperate more easily with
  /// CanonicalizeInstruction::killInstruction(). This allows updates to choose
  /// whether to happen before or after deleting the instruction and possibly
  /// keep it around as a zombie object. All implementations must at least
  /// immediately remove all references to the instruction, including the parent
  /// block list.
  ///
  /// TODO: Now that instructions deletion can be delayed via
  /// SILModule::scheduleForDeletion(); there's no longer a good use case for
  /// calling eraseFromParent() within this callback. Rewrite all clients
  /// without doing the instruction deletion within the callback.
  std::function<void(SILInstruction *instToDelete)> deleteInstFunc;

  /// If non-null, called before a salient instruction is deleted or has its
  /// references dropped. If null, no-op.
  ///
  /// This can be used to respond to dead instructions that will be deleted in
  /// the future. Unlike deleteInstFunc, the SIL will be in a valid
  /// state. However, arbitrary SIL transformations may happen between this
  /// invocation and actual instruction deletion.
  ///
  /// This callback is not guaranteed to be called for every deleted
  /// instruction. It cannot be used to invalidate dangling pointers. It is only
  /// called for "salient" instructions that likely create additional
  /// optimization opportunities when deleted. If a dead def-use chain is
  /// deleted, notification only occurs for the initial def.
  ///
  /// This is used in rare circumstances to update an optimization worklist. It
  /// should be avoided whenever possible in favor of more structured
  /// optimization passes.
  std::function<void(SILInstruction *instThatWillBeDeleted)>
      notifyWillBeDeletedFunc;

  /// A boolean that tracks if any of our callbacks were ever called.
  bool wereAnyCallbacksInvoked = false;

  InstModCallbacks() = default;
  ~InstModCallbacks() = default;
  InstModCallbacks(const InstModCallbacks &) = default;

  /// Return a copy of self with deleteInstFunc set to \p newDeleteInstFunc.
  LLVM_ATTRIBUTE_UNUSED InstModCallbacks
  onDelete(decltype(deleteInstFunc) newDeleteInstFunc) const {
    InstModCallbacks result = *this;
    result.deleteInstFunc = newDeleteInstFunc;
    return result;
  }

  /// Return a copy of self with createdNewInstFunc set to \p
  /// newCreatedNewInstFunc.
  LLVM_ATTRIBUTE_UNUSED InstModCallbacks
  onCreateNewInst(decltype(createdNewInstFunc) newCreatedNewInstFunc) const {
    InstModCallbacks result = *this;
    result.createdNewInstFunc = newCreatedNewInstFunc;
    return result;
  }

  /// Return a copy of self with setUseValueFunc set to \p newSetUseValueFunc.
  LLVM_ATTRIBUTE_UNUSED InstModCallbacks
  onSetUseValue(decltype(setUseValueFunc) newSetUseValueFunc) const {
    InstModCallbacks result = *this;
    result.setUseValueFunc = newSetUseValueFunc;
    return result;
  }

  /// Return a copy of self with notifyWillBeDeletedFunc set to \p
  /// newNotifyWillBeDeletedFunc.
  LLVM_ATTRIBUTE_UNUSED
  InstModCallbacks onNotifyWillBeDeleted(
      decltype(notifyWillBeDeletedFunc) newNotifyWillBeDeletedFunc) const {
    InstModCallbacks result = *this;
    result.notifyWillBeDeletedFunc = newNotifyWillBeDeletedFunc;
    return result;
  }

  void deleteInst(SILInstruction *instToDelete,
                  bool notifyWhenDeleting = true) {
    wereAnyCallbacksInvoked = true;
    if (notifyWhenDeleting && notifyWillBeDeletedFunc)
      notifyWillBeDeletedFunc(instToDelete);
    if (deleteInstFunc)
      return deleteInstFunc(instToDelete);
    instToDelete->eraseFromParent();
  }

  void createdNewInst(SILInstruction *newlyCreatedInst) {
    wereAnyCallbacksInvoked = true;
    if (createdNewInstFunc)
      createdNewInstFunc(newlyCreatedInst);
  }

  void setUseValue(Operand *use, SILValue newValue) {
    wereAnyCallbacksInvoked = true;
    if (setUseValueFunc)
      return setUseValueFunc(use, newValue);
    use->set(newValue);
  }

  /// Notify via our callbacks that an instruction will be deleted/have its
  /// operands dropped.
  ///
  /// DISCUSSION: Since we do not delete instructions in any specific order, we
  /// drop all references of the instructions before we call deleteInst. Thus
  /// one can not in deleteInst look at operands. Certain parts of the optimizer
  /// rely on this ability, so we preserve it.
  void notifyWillBeDeleted(SILInstruction *instThatWillBeDeleted) {
    wereAnyCallbacksInvoked = true;
    if (notifyWillBeDeletedFunc)
      return notifyWillBeDeletedFunc(instThatWillBeDeleted);
  }

  void replaceValueUsesWith(SILValue oldValue, SILValue newValue) {
    wereAnyCallbacksInvoked = true;

    // If setUseValueFunc is not set, just call RAUW directly. RAUW in this case
    // is equivalent to what we do below. We just enable better
    // performance. This ensures that the default InstModCallback is really
    // fast.
    if (!setUseValueFunc)
      return oldValue->replaceAllUsesWith(newValue);

    while (!oldValue->use_empty()) {
      auto *use = *oldValue->use_begin();
      setUseValue(use, newValue);
    }
  }

  void eraseAndRAUWSingleValueInst(SingleValueInstruction *oldInst,
                                   SILValue newValue) {
    wereAnyCallbacksInvoked = true;
    replaceValueUsesWith(oldInst, newValue);
    deleteInst(oldInst);
  }

  bool hadCallbackInvocation() const { return wereAnyCallbacksInvoked; }

  /// Set \p wereAnyCallbacksInvoked to false. Useful if one wants to reuse an
  /// InstModCallback in between iterations.
  void resetHadCallbackInvocation() { wereAnyCallbacksInvoked = false; }
};

} // end namespace swift

#endif
