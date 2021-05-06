//===--- InstOptUtils.h - SILOptimizer instruction utilities ----*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
///
/// Utilities used by the SILOptimizer for analyzing and transforming
/// SILInstructions.
///
/// SIL/InstUtils.h provides essential SILInstruction utilities.
///
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SILOPTIMIZER_UTILS_INSTOPTUTILS_H
#define SWIFT_SILOPTIMIZER_UTILS_INSTOPTUTILS_H

#include "swift/SIL/BasicBlockUtils.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SILOptimizer/Analysis/ARCAnalysis.h"
#include "swift/SILOptimizer/Analysis/ClassHierarchyAnalysis.h"
#include "swift/SILOptimizer/Analysis/EpilogueARCAnalysis.h"
#include "swift/SILOptimizer/Analysis/SimplifyInstruction.h"
#include "llvm/ADT/SmallPtrSet.h"
#include <functional>
#include <utility>

namespace swift {

class DominanceInfo;
template <class T> class NullablePtr;

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
class InstModCallbacks {
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
  std::function<void(Operand *use, SILValue newValue)> setUseValueFunc;

  /// A function that takes in an instruction and deletes the inst.
  ///
  /// Default implementation is instToDelete->eraseFromParent();
  ///
  /// NOTE: The reason why we have deleteInstFunc and notifyWillBeDeletedFunc is
  /// InstModCallback supports 2 stage deletion where a callee passed
  /// InstModCallback is allowed to drop all references to the instruction
  /// before calling deleteInstFunc. In contrast, notifyWillBeDeletedFunc
  /// assumes that the IR is in a good form before being called so that the
  /// caller can via the callback gather state about the instruction that will
  /// be deleted. As an example, see InstructionDeleter::deleteInstruction() in
  /// InstOptUtils.cpp.
  std::function<void(SILInstruction *instToDelete)> deleteInstFunc;

  /// If non-null, called before an instruction is deleted or has its references
  /// dropped. If null, no-op.
  ///
  /// NOTE: The reason why we have deleteInstFunc and notifyWillBeDeletedFunc is
  /// InstModCallback supports 2 stage deletion where a callee passed
  /// InstModCallback is allowed to drop all references to the instruction
  /// before calling deleteInstFunc. In contrast, notifyWillBeDeletedFunc
  /// assumes that the IR is in a good form before being called so that the
  /// caller can via the callback gather state about the instruction that will
  /// be deleted. As an example, see InstructionDeleter::deleteInstruction() in
  /// InstOptUtils.cpp.
  ///
  /// NOTE: This is called in InstModCallback::deleteInst() if one does not use
  /// a default bool argument to disable the notification. In general that
  /// should only be done when one is writing custom handling and is performing
  /// the notification ones self. It is assumed that the notification will be
  /// called with a valid instruction.
  std::function<void(SILInstruction *instThatWillBeDeleted)>
      notifyWillBeDeletedFunc;

  /// A boolean that tracks if any of our callbacks were ever called.
  bool wereAnyCallbacksInvoked = false;

public:
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

/// Transform a Use Range (Operand*) into a User Range (SILInstruction *)
using UserTransform = std::function<SILInstruction *(Operand *)>;
using ValueBaseUserRange =
    TransformRange<iterator_range<ValueBase::use_iterator>, UserTransform>;

template <typename Range>
inline TransformRange<Range, UserTransform> makeUserRange(Range range) {
  auto toUser = [](Operand *operand) { return operand->getUser(); };
  return makeTransformRange(range, UserTransform(toUser));
}

/// Transform a use_iterator range (Operand*) into an llvm::iterator_range
/// of users (SILInstruction *)
inline iterator_range<llvm::mapped_iterator<ValueBase::use_iterator, UserTransform>>
makeUserIteratorRange(iterator_range<ValueBase::use_iterator> useRange) {
  auto toUser = [](Operand *operand) { return operand->getUser(); };
  return llvm::map_range(useRange, UserTransform(toUser));
}

using DeadInstructionSet = llvm::SmallSetVector<SILInstruction *, 8>;

/// Create a retain of \p Ptr before the \p InsertPt.
NullablePtr<SILInstruction> createIncrementBefore(SILValue ptr,
                                                  SILInstruction *insertpt);

/// Create a release of \p Ptr before the \p InsertPt.
NullablePtr<SILInstruction> createDecrementBefore(SILValue ptr,
                                                  SILInstruction *insertpt);

/// Get the insertion point after \p val.
Optional<SILBasicBlock::iterator> getInsertAfterPoint(SILValue val);

/// Return the number of @inout arguments passed to the given apply site.
unsigned getNumInOutArguments(FullApplySite applySite);

/// For each of the given instructions, if they are dead delete them
/// along with their dead operands. Note this utility must be phased out and
/// replaced by \c eliminateDeadInstruction  and
/// \c InstructionDeleter utilities.
///
/// \param inst The ArrayRef of instructions to be deleted.
/// \param force If Force is set, don't check if the top level instructions
///        are considered dead - delete them regardless.
/// \param callbacks The inst mod callbacks used to delete instructions.
void recursivelyDeleteTriviallyDeadInstructions(
    ArrayRef<SILInstruction *> inst, bool force = false,
    InstModCallbacks callbacks = InstModCallbacks());

/// If the given instruction is dead, delete it along with its dead
/// operands. Note this utility must be phased out and replaced by
/// \c eliminateDeadInstruction and
/// \c InstructionDeleter utilities.
///
/// \param inst The instruction to be deleted.
/// \param force If Force is set, don't check if the top level instruction is
///        considered dead - delete it regardless.
/// \param callbacks InstModCallback used to delete instructions.
void recursivelyDeleteTriviallyDeadInstructions(
    SILInstruction *inst, bool force = false,
    InstModCallbacks callbacks = InstModCallbacks());

/// Perform a fast local check to see if the instruction is dead.
///
/// This routine only examines the state of the instruction at hand.
bool isInstructionTriviallyDead(SILInstruction *inst);

/// Return true if this is a release instruction that's not going to
/// free the object.
bool isIntermediateRelease(SILInstruction *inst, EpilogueARCFunctionInfo *erfi);

/// Recursively collect all the uses and transitive uses of the
/// instruction.
void collectUsesOfValue(SILValue V,
                        llvm::SmallPtrSetImpl<SILInstruction *> &Insts);

/// Recursively erase all of the uses of the instruction (but not the
/// instruction itself)
void eraseUsesOfInstruction(SILInstruction *inst,
                            InstModCallbacks callbacks = InstModCallbacks());

/// Recursively erase all of the uses of the value (but not the
/// value itself)
void eraseUsesOfValue(SILValue value);

/// Gets the concrete value which is stored in an existential box.
/// Returns %value in following pattern:
///
///    %existentialBox = alloc_existential_box $Error, $ConcreteError
///    %a = project_existential_box $ConcreteError in %existentialBox : $Error
///    store %value to %a : $*ConcreteError
///
/// Returns an invalid SILValue in case there are multiple stores or any unknown
/// users of \p existentialBox.
/// The \p ignoreUser is ignored in the user list of \p existentialBox.
SILValue
getConcreteValueOfExistentialBox(AllocExistentialBoxInst *existentialBox,
                                 SILInstruction *ignoreUser);

/// Gets the concrete value which is stored in an existential box, which itself
/// is stored in \p addr.
/// Returns %value in following pattern:
///
///    %b = alloc_existential_box $Error, $ConcreteError
///    %a = project_existential_box $ConcreteError in %b : $Error
///    store %value to %a : $*ConcreteError
///    %addr = alloc_stack $Error
///    store %b to %addr : $*Error
///
/// Returns an invalid SILValue in case there are multiple stores or any unknown
/// users of \p addr or the existential box.
/// The \p ignoreUser is ignored in the user list of \p addr.
SILValue getConcreteValueOfExistentialBoxAddr(SILValue addr,
                                              SILInstruction *ignoreUser);

/// Cast a value into the expected, ABI compatible type if necessary.
/// This may happen e.g. when:
/// - a type of the return value is a subclass of the expected return type.
/// - actual return type and expected return type differ in optionality.
/// - both types are tuple-types and some of the elements need to be casted.
///
/// \p usePoints is required when \p value has guaranteed ownership. It must be
/// the last users of the returned, casted value. A usePoint cannot be a
/// BranchInst (a phi is never the last guaranteed user). \p builder's current
/// insertion point must dominate all \p usePoints.
std::pair<SILValue, bool /* changedCFG */>
castValueToABICompatibleType(SILBuilder *builder, SILLocation Loc,
                             SILValue value, SILType srcTy, SILType destTy,
                             ArrayRef<SILInstruction *> usePoints);
/// Peek through trivial Enum initialization, typically for pointless
/// Optionals.
///
/// The returned InitEnumDataAddr dominates the given
/// UncheckedTakeEnumDataAddrInst.
InitEnumDataAddrInst *
findInitAddressForTrivialEnum(UncheckedTakeEnumDataAddrInst *utedai);

/// Returns a project_box if it is the next instruction after \p ABI and
/// and has \p ABI as operand. Otherwise it creates a new project_box right
/// after \p ABI and returns it.
ProjectBoxInst *getOrCreateProjectBox(AllocBoxInst *abi, unsigned index);

/// Return true if any call inside the given function may bind dynamic
/// 'Self' to a generic argument of the callee.
bool mayBindDynamicSelf(SILFunction *f);

/// Check whether the \p addr is an address of a tail-allocated array element.
bool isAddressOfArrayElement(SILValue addr);

/// Move an ApplyInst's FuncRef so that it dominates the call site.
void placeFuncRef(ApplyInst *ai, DominanceInfo *dt);

/// Add an argument, \p val, to the branch-edge that is pointing into
/// block \p Dest. Return a new instruction and do not erase the old
/// instruction.
TermInst *addArgumentToBranch(SILValue val, SILBasicBlock *dest,
                              TermInst *branch);

/// Get the linkage to be used for specializations of a function with
/// the given linkage.
SILLinkage getSpecializedLinkage(SILFunction *f, SILLinkage linkage);

/// Tries to perform jump-threading on all checked_cast_br instruction in
/// function \p Fn.
bool tryCheckedCastBrJumpThreading(
    SILFunction *fn, DominanceInfo *dt,
    SmallVectorImpl<SILBasicBlock *> &blocksForWorklist);

/// A utility for deleting one or more instructions belonging to a function, and
/// cleaning up any dead code resulting from deleting those instructions. Use
/// this utility instead of
/// \c recursivelyDeleteTriviallyDeadInstruction.
class InstructionDeleter {
private:
  /// A set vector of instructions that are found to be dead. The ordering of
  /// instructions in this set is important as when a dead instruction is
  /// removed, new instructions will be generated to fix the lifetime of the
  /// instruction's operands. This has to be deterministic.
  SmallSetVector<SILInstruction *, 8> deadInstructions;

  /// Callbacks used when adding/deleting instructions.
  InstModCallbacks instModCallbacks;

public:
  InstructionDeleter() : deadInstructions(), instModCallbacks() {}
  InstructionDeleter(InstModCallbacks inputCallbacks)
      : deadInstructions(), instModCallbacks(inputCallbacks) {}

  /// If the instruction \p inst is dead, record it so that it can be cleaned
  /// up.
  void trackIfDead(SILInstruction *inst);

  /// If the instruction \p inst is dead, delete it immediately and record
  /// its operands so that they can be cleaned up later.
  void deleteIfDead(SILInstruction *inst);

  /// Delete the instruction \p inst and record instructions that may become
  /// dead because of the removal of \c inst. This function will add necessary
  /// ownership instructions to fix the lifetimes of the operands of \c inst to
  /// compensate for its deletion. This function will not clean up dead code
  /// resulting from the instruction's removal. To do so, invoke the method \c
  /// cleanupDeadCode of this instance, once the SIL of the contaning function
  /// is made consistent.
  ///
  /// \pre the function containing \c inst must be using ownership SIL.
  /// \pre the instruction to be deleted must not have any use other than
  /// incidental uses.
  ///
  /// \p callback is called on each deleted instruction before deleting any
  /// instructions. This way, the SIL is valid in the callback. However, the
  /// callback cannot be used to update instruction iterators since other
  /// instructions to be deleted remain in the instruction list.
  void forceDeleteAndFixLifetimes(SILInstruction *inst);

  /// Delete the instruction \p inst and record instructions that may become
  /// dead because of the removal of \c inst. If in ownership SIL, use the
  /// \c forceDeleteAndFixLifetimes function instead, unless under special
  /// circumstances where the client must handle fixing lifetimes of the
  /// operands of the deleted instructions. This function will not fix the
  /// lifetimes of the operands of \c inst once it is deleted. This function
  /// will not clean up dead code resulting from the instruction's removal. To
  /// do so, invoke the method \c cleanupDeadCode of this instance, once the SIL
  /// of the contaning function is made consistent.
  ///
  /// \pre the instruction to be deleted must not have any use other than
  /// incidental uses.
  void forceDelete(SILInstruction *inst);

  /// Force track this instruction as dead. Used to enable the deletion of a
  /// bunch of instructions at the same time.
  void forceTrackAsDead(SILInstruction *inst);

  /// Clean up dead instructions that are tracked by this instance and all
  /// instructions that transitively become dead.
  ///
  /// \pre the function contaning dead instructions must be consistent (i.e., no
  /// under or over releases). Note that if \c forceDelete call leaves the
  /// function body in an inconsistent state, it needs to be made consistent
  /// before this method is invoked.
  void cleanUpDeadInstructions();

  /// Recursively visit users of \c inst  (including \c inst)and delete
  /// instructions that are dead (including \c inst).
  void recursivelyDeleteUsersIfDead(SILInstruction *inst);

  /// Recursively visit users of \c inst  (including \c inst)and force delete
  /// them. Also, destroy the consumed operands of the deleted instructions
  /// whenever necessary.
  void recursivelyForceDeleteUsersAndFixLifetimes(SILInstruction *inst);

private:
  void deleteInstruction(SILInstruction *inst, bool fixOperandLifetimes);
};

/// If \c inst is dead, delete it and recursively eliminate all code that
/// becomes dead because of that. If more than one instruction must
/// be checked/deleted use the \c InstructionDeleter utility.
///
/// This function will add necessary compensation code to fix the lifetimes of
/// the operands of the deleted instructions.
///
/// \pre the SIL function containing the instruction is assumed to be
/// consistent, i.e., does not have under or over releases.
///
/// \p callbacks is used to delete each instruction. However, the callback
/// cannot be used to update instruction iterators since other instructions to
/// be deleted remain in the instruction list. If set to nullptr, we use the
/// default instruction modification callback structure.
void eliminateDeadInstruction(SILInstruction *inst,
                              InstModCallbacks callbacks = InstModCallbacks());

/// Get all consumed arguments of a partial_apply.
///
/// These are basically all arguments, except inout arguments and arguments
/// of trivial type.
/// If \p includeTrivialAddrArgs is true, also trivial address-type arguments
/// are included.
void getConsumedPartialApplyArgs(PartialApplyInst *pai,
                                 SmallVectorImpl<Operand *> &argOperands,
                                 bool includeTrivialAddrArgs);

/// Emit destroy operation for \p operand, and call appropriate functions from
/// \p callbacks for newly created instructions and deleted instructions.
void emitDestroyOperation(SILBuilder &builder, SILLocation loc,
                          SILValue operand, InstModCallbacks callbacks);

/// Collect all (transitive) users of \p inst which just copy or destroy \p
/// inst.
///
/// In other words: all users which do not prevent \p inst from being considered
/// as "dead".
/// Returns true, if there are no other users beside those collected in \p
/// destroys, i.e. if \p inst can be considered as "dead".
bool collectDestroys(SingleValueInstruction *inst,
                     SmallVectorImpl<SILInstruction *> &destroys);

/// If Closure is a partial_apply or thin_to_thick_function with only local
/// ref count users and a set of post-dominating releases:
///
/// 1. Remove all ref count operations and the closure.
/// 2. At each one of the last release locations insert releases for the
///    captured args if we have a partial_apply (except \p needKeepArgsAlive is
///    false).
///
/// In the future this should be extended to be less conservative with users.
bool tryDeleteDeadClosure(SingleValueInstruction *closure,
                          InstModCallbacks callbacks = InstModCallbacks(),
                          bool needKeepArgsAlive = true);

/// Given a SILValue argument to a partial apply \p Arg and the associated
/// parameter info for that argument, perform the necessary cleanups to Arg when
/// one is attempting to delete the partial apply.
void releasePartialApplyCapturedArg(
    SILBuilder &builder, SILLocation loc, SILValue arg,
    SILParameterInfo paramInfo,
    InstModCallbacks callbacks = InstModCallbacks());

void deallocPartialApplyCapturedArg(
    SILBuilder &builder, SILLocation loc, SILValue arg,
    SILParameterInfo paramInfo);

/// Insert destroys of captured arguments of partial_apply [stack].
void insertDestroyOfCapturedArguments(
    PartialApplyInst *pai, SILBuilder &builder,
    llvm::function_ref<bool(SILValue)> shouldInsertDestroy =
        [](SILValue arg) -> bool { return true; });

void insertDeallocOfCapturedArguments(
    PartialApplyInst *pai, SILBuilder &builder);

/// This iterator 'looks through' one level of builtin expect users exposing all
/// users of the looked through builtin expect instruction i.e it presents a
/// view that shows all users as if there were no builtin expect instructions
/// interposed.
class IgnoreExpectUseIterator
    : public std::iterator<std::forward_iterator_tag, Operand *, ptrdiff_t> {
  ValueBaseUseIterator origUseChain;
  ValueBaseUseIterator currentIter;

  static BuiltinInst *isExpect(Operand *use) {
    if (auto *bi = dyn_cast<BuiltinInst>(use->getUser()))
      if (bi->getIntrinsicInfo().ID == llvm::Intrinsic::expect)
        return bi;
    return nullptr;
  }

  // Advance through expect users to their users until we encounter a user that
  // is not an expect.
  void advanceThroughExpects() {
    while (currentIter == origUseChain
           && currentIter != ValueBaseUseIterator(nullptr)) {
      auto *Expect = isExpect(*currentIter);
      if (!Expect)
        return;
      currentIter = Expect->use_begin();
      // Expect with no users advance to next item in original use chain.
      if (currentIter == Expect->use_end())
        currentIter = ++origUseChain;
    }
  }

public:
  IgnoreExpectUseIterator(ValueBase *value)
      : origUseChain(value->use_begin()), currentIter(value->use_begin()) {
    advanceThroughExpects();
  }

  IgnoreExpectUseIterator() = default;

  Operand *operator*() const { return *currentIter; }
  Operand *operator->() const { return *currentIter; }
  SILInstruction *getUser() const { return currentIter->getUser(); }

  IgnoreExpectUseIterator &operator++() {
    assert(**this && "increment past end()!");
    if (origUseChain == currentIter) {
      // Use chain of the original value.
      ++origUseChain;
      ++currentIter;
      // Ignore expects.
      advanceThroughExpects();
    } else {
      // Use chain of an expect.
      ++currentIter;
      if (currentIter == ValueBaseUseIterator(nullptr)) {
        // At the end of the use chain of an expect.
        currentIter = ++origUseChain;
        advanceThroughExpects();
      }
    }
    return *this;
  }

  IgnoreExpectUseIterator operator++(int unused) {
    IgnoreExpectUseIterator copy = *this;
    ++*this;
    return copy;
  }
  friend bool operator==(IgnoreExpectUseIterator lhs,
                         IgnoreExpectUseIterator rhs) {
    return lhs.currentIter == rhs.currentIter;
  }
  friend bool operator!=(IgnoreExpectUseIterator lhs,
                         IgnoreExpectUseIterator rhs) {
    return !(lhs == rhs);
  }
};

inline iterator_range<IgnoreExpectUseIterator>
ignore_expect_uses(ValueBase *value) {
  return make_range(IgnoreExpectUseIterator(value), IgnoreExpectUseIterator());
}

/// Run simplifyInstruction() on all of the instruction I's users if they only
/// have one result (since simplifyInstruction assumes that). Replace all uses
/// of the user with its simplification of we succeed. Returns true if we
/// succeed and false otherwise.
///
/// An example of how this is useful is in cases where one is splitting up an
/// aggregate and reforming it, the reformed aggregate may have extract
/// operations from it. These can be simplified and removed.
bool simplifyUsers(SingleValueInstruction *inst);

///  True if a type can be expanded
/// without a significant increase to code size.
bool shouldExpand(SILModule &module, SILType ty);

/// Check if the value of value is computed by means of a simple initialization.
/// Store the actual SILValue into \p Val and the reversed list of instructions
/// initializing it in \p Insns.
/// The check is performed by recursively walking the computation of the
/// SIL value being analyzed.
bool analyzeStaticInitializer(SILValue value,
                              SmallVectorImpl<SILInstruction *> &insns);

/// Returns true if the below operation will succeed.
bool canReplaceLoadSequence(SILInstruction *inst);

/// Replace load sequence which may contain
/// a chain of struct_element_addr followed by a load.
/// The sequence is traversed inside out, i.e.
/// starting with the innermost struct_element_addr
void replaceLoadSequence(SILInstruction *inst, SILValue value);

/// Do we have enough information to determine all callees that could
/// be reached by calling the function represented by Decl?
bool calleesAreStaticallyKnowable(SILModule &module, SILDeclRef decl);

/// Do we have enough information to determine all callees that could
/// be reached by calling the function represented by Decl?
bool calleesAreStaticallyKnowable(SILModule &module, ValueDecl *vd);

// Attempt to get the instance for , whose static type is the same as
// its exact dynamic type, returning a null SILValue() if we cannot find it.
// The information that a static type is the same as the exact dynamic,
// can be derived e.g.:
// - from a constructor or
// - from a successful outcome of a checked_cast_br [exact] instruction.
SILValue getInstanceWithExactDynamicType(SILValue instance,
                                         ClassHierarchyAnalysis *cha);

/// Try to determine the exact dynamic type of an object.
/// returns the exact dynamic type of the object, or an empty type if the exact
/// type could not be determined.
SILType getExactDynamicType(SILValue instance, ClassHierarchyAnalysis *cha,
                            bool forUnderlyingObject = false);

/// Try to statically determine the exact dynamic type of the underlying object.
/// returns the exact dynamic type of the underlying object, or an empty SILType
/// if the exact type could not be determined.
SILType getExactDynamicTypeOfUnderlyingObject(SILValue instance,
                                              ClassHierarchyAnalysis *cha);

// Move only data structure that is the result of findLocalApplySite.
///
/// NOTE: Generally it is not suggested to have move only types that contain
/// small vectors. Since our small vectors contain one element or a std::vector
/// like data structure , this is ok since we will either just copy the single
/// element when we do the move or perform a move of the vector type.
struct LLVM_LIBRARY_VISIBILITY FindLocalApplySitesResult {
  /// Contains the list of local non fully applied partial apply sites that we
  /// found.
  SmallVector<ApplySite, 1> partialApplySites;

  /// Contains the list of full apply sites that we found.
  SmallVector<FullApplySite, 1> fullApplySites;

  /// Set to true if the function_ref escapes into a use that our analysis does
  /// not understand. Set to false if we found a use that had an actual
  /// escape. Set to None if we did not find any call sites, but also didn't
  /// find any "escaping uses" as well.
  ///
  /// The none case is so that we can distinguish in between saying that a value
  /// did escape and saying that we did not find any conservative information.
  bool escapes;

  FindLocalApplySitesResult() = default;
  FindLocalApplySitesResult(const FindLocalApplySitesResult &) = delete;
  FindLocalApplySitesResult &
  operator=(const FindLocalApplySitesResult &) = delete;
  FindLocalApplySitesResult(FindLocalApplySitesResult &&) = default;
  FindLocalApplySitesResult &operator=(FindLocalApplySitesResult &&) = default;
  ~FindLocalApplySitesResult() = default;

  /// Treat this function ref as escaping only if we found an actual user we
  /// didn't understand. Do not treat it as escaping if we did not find any
  /// users at all.
  bool isEscaping() const { return escapes; }
};

/// Returns .some(FindLocalApplySitesResult) if we found any interesting
/// information for the given function_ref. Otherwise, returns None.
///
/// We consider "interesting information" to mean inclusively that:
///
/// 1. We discovered that the function_ref never escapes.
/// 2. We were able to find either a partial apply or a full apply site.
Optional<FindLocalApplySitesResult>
findLocalApplySites(FunctionRefBaseInst *fri);

/// Gets the base implementation of a method.
AbstractFunctionDecl *getBaseMethod(AbstractFunctionDecl *FD);

bool tryOptimizeApplyOfPartialApply(
    PartialApplyInst *pai, SILBuilderContext &builderCtxt,
    InstModCallbacks callbacks = InstModCallbacks());

/// Clone this full apply site, replacing the callee with \p newCallee while
/// doing so.
///
/// The current full apply site is used as an insertion point, so the caller
/// must clean up this full apply site.
FullApplySite cloneFullApplySiteReplacingCallee(FullApplySite applySite,
                                                SILValue newCallee,
                                                SILBuilderContext &builderCtx);

/// This is a low level routine that makes all uses of \p svi uses of \p
/// newValue (ignoring end scope markers) and then deletes \p svi and all end
/// scope markers. Then returns the next inst to process.
SILBasicBlock::iterator replaceAllUsesAndErase(SingleValueInstruction *svi,
                                               SILValue newValue,
                                               InstModCallbacks &callbacks);

/// This API is equivalent to performing \p use->set(\p newValue) except that:
///
/// 1. If the user of \p use is an end scope, this API no-opts. This API is only
///    used in contexts where we are rewriting uses and are not interesting in
///    end scope instructions since we are moving uses from one scope to another
///    scope.
///
/// 2. If the user of \p use is not an end scope, but is a lifetime ending use
///    of \p use->get(), we insert a destroy_value|end_borrow as appropriate on
///    \p use->get() to ensure \p use->get()'s lifetime is still ended. We
///    assume that if \p use->getUser() is lifetime ending, that our caller has
///    ensured that we can end \p newValue's lifetime.
SILBasicBlock::iterator replaceSingleUse(Operand *use, SILValue newValue,
                                         InstModCallbacks &callbacks);

/// Creates a copy of \p value and inserts additional control equivalent copy
/// and destroy at leaking blocks to adjust ownership and make available for use
/// at \p inBlock.
SILValue
makeCopiedValueAvailable(SILValue value, SILBasicBlock *inBlock);

/// Given a newly created @owned value \p value without any uses, this utility
/// inserts control equivalent copy and destroy at leaking blocks to adjust
/// ownership and make \p value available for use at \p inBlock.
///
/// inBlock must be the only point at which \p value will be consumed. If this
/// consuming point is within a loop, this will create and return a copy of \p
/// value inside \p inBlock.
SILValue
makeNewValueAvailable(SILValue value, SILBasicBlock *inBlock);

/// Given an ssa value \p value, create destroy_values at leaking blocks
///
/// Warning: This does not properly cleanup an OSSA lifetime with a consuming
/// use blocks inside a loop relative to \p value. The client must create
/// separate copies for any uses within the loop.
void endLifetimeAtLeakingBlocks(SILValue value,
                                ArrayRef<SILBasicBlock *> userBBs);

/// Given a forwarding instruction, eliminate it if all of its users are debug
/// instructions and ownership uses.
bool tryEliminateOnlyOwnershipUsedForwardingInst(
    SingleValueInstruction *forwardingInst, InstModCallbacks &callbacks);

} // end namespace swift

#endif
