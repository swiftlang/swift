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

/// Transform a Use Range (Operand*) into a User Range (SILInstruction*)
using UserTransform = std::function<SILInstruction *(Operand *)>;
using ValueBaseUserRange =
    TransformRange<iterator_range<ValueBase::use_iterator>, UserTransform>;

inline ValueBaseUserRange
makeUserRange(iterator_range<ValueBase::use_iterator> range) {
  auto toUser = [](Operand *operand) { return operand->getUser(); };
  return makeTransformRange(range, UserTransform(toUser));
}

using DeadInstructionSet = llvm::SmallSetVector<SILInstruction *, 8>;

/// Create a retain of \p Ptr before the \p InsertPt.
NullablePtr<SILInstruction> createIncrementBefore(SILValue ptr,
                                                  SILInstruction *insertpt);

/// Create a release of \p Ptr before the \p InsertPt.
NullablePtr<SILInstruction> createDecrementBefore(SILValue ptr,
                                                  SILInstruction *insertpt);

/// A utility for deleting one or more instructions belonging to a function, and
/// cleaning up any dead code resulting from deleting those instructions. Use
/// this utility instead of
/// \c recursivelyDeleteTriviallyDeadInstruction.
class InstructionDeleter {
private:
  // A set vector of instructions that are found to be dead. The ordering
  // of instructions in this set is important as when a dead instruction is
  // removed, new instructions will be generated to fix the lifetime of the
  // instruction's operands. This has to be deterministic.
  SmallSetVector<SILInstruction *, 8> deadInstructions;

  void deleteInstruction(SILInstruction *inst,
                         llvm::function_ref<void(SILInstruction *)> callback,
                         bool fixOperandLifetimes);

public:
  InstructionDeleter() {}

  /// If the instruction \p inst is dead, record it so that it can be cleaned
  /// up.
  void trackIfDead(SILInstruction *inst);

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
  /// \param callback a callback called whenever an instruction
  /// is deleted.
  void forceDeleteAndFixLifetimes(
      SILInstruction *inst,
      llvm::function_ref<void(SILInstruction *)> callback =
          [](SILInstruction *) {});

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
  ///
  /// \param callback a callback called whenever an instruction
  /// is deleted.
  void forceDelete(
      SILInstruction *inst,
      llvm::function_ref<void(SILInstruction *)> callback =
          [](SILInstruction *) {});

  /// Clean up dead instructions that are tracked by this instance and all
  /// instructions that transitively become dead.
  ///
  /// \pre the function contaning dead instructions must be consistent (i.e., no
  /// under or over releases). Note that if \c forceDelete call leaves the
  /// function body in an inconsistent state, it needs to be made consistent
  /// before this method is invoked.
  ///
  /// \param callback a callback called whenever an instruction is deleted.
  void
  cleanUpDeadInstructions(llvm::function_ref<void(SILInstruction *)> callback =
                              [](SILInstruction *) {});
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
/// \param callback a callback called whenever an instruction is deleted.
void eliminateDeadInstruction(
    SILInstruction *inst, llvm::function_ref<void(SILInstruction *)> callback =
                              [](SILInstruction *) {});

/// For each of the given instructions, if they are dead delete them
/// along with their dead operands. Note this utility must be phased out and
/// replaced by \c eliminateDeadInstruction  and
/// \c InstructionDeleter utilities.
///
/// \param inst The ArrayRef of instructions to be deleted.
/// \param force If Force is set, don't check if the top level instructions
///        are considered dead - delete them regardless.
/// \param callback a callback called whenever an instruction is deleted.
void recursivelyDeleteTriviallyDeadInstructions(
    ArrayRef<SILInstruction *> inst, bool force = false,
    llvm::function_ref<void(SILInstruction *)> callback = [](SILInstruction *) {
    });

/// If the given instruction is dead, delete it along with its dead
/// operands. Note this utility must be phased out and replaced by
/// \c eliminateDeadInstruction and
/// \c InstructionDeleter utilities.
///
/// \param inst The instruction to be deleted.
/// \param force If Force is set, don't check if the top level instruction is
///        considered dead - delete it regardless.
/// \param callback a callback called whenever an instruction is deleted.
void recursivelyDeleteTriviallyDeadInstructions(
    SILInstruction *inst, bool force = false,
    llvm::function_ref<void(SILInstruction *)> callback = [](SILInstruction *) {
    });

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
void eraseUsesOfInstruction(
    SILInstruction *inst, llvm::function_ref<void(SILInstruction *)> callback =
                              [](SILInstruction *) {});

/// Recursively erase all of the uses of the value (but not the
/// value itself)
void eraseUsesOfValue(SILValue value);

FullApplySite findApplyFromDevirtualizedResult(SILValue value);

/// Cast a value into the expected, ABI compatible type if necessary.
/// This may happen e.g. when:
/// - a type of the return value is a subclass of the expected return type.
/// - actual return type and expected return type differ in optionality.
/// - both types are tuple-types and some of the elements need to be casted.
SILValue castValueToABICompatibleType(SILBuilder *builder, SILLocation Loc,
                                      SILValue value, SILType srcTy,
                                      SILType destTy);
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

/// Tries to optimize a given apply instruction if it is a concatenation of
/// string literals. Returns a new instruction if optimization was possible.
SingleValueInstruction *tryToConcatenateStrings(ApplyInst *ai,
                                                SILBuilder &builder);

/// Tries to perform jump-threading on all checked_cast_br instruction in
/// function \p Fn.
bool tryCheckedCastBrJumpThreading(
    SILFunction *fn, DominanceInfo *dt,
    SmallVectorImpl<SILBasicBlock *> &blocksForWorklist);

/// A structure containing callbacks that are called when an instruction is
/// removed or added.
struct InstModCallbacks {
  std::function<void(SILInstruction *)> deleteInst = [](SILInstruction *inst) {
    inst->eraseFromParent();
  };
  std::function<void(SILInstruction *)> createdNewInst = [](SILInstruction *) {
  };
  std::function<void(SILValue, SILValue)> replaceValueUsesWith =
      [](SILValue oldValue, SILValue newValue) {
        oldValue->replaceAllUsesWith(newValue);
      };

  InstModCallbacks(decltype(deleteInst) deleteInst,
                   decltype(createdNewInst) createdNewInst,
                   decltype(replaceValueUsesWith) replaceValueUsesWith)
      : deleteInst(deleteInst), createdNewInst(createdNewInst),
        replaceValueUsesWith(replaceValueUsesWith) {}
  InstModCallbacks() = default;
  ~InstModCallbacks() = default;
  InstModCallbacks(const InstModCallbacks &) = default;
  InstModCallbacks(InstModCallbacks &&) = default;
};

/// If Closure is a partial_apply or thin_to_thick_function with only local
/// ref count users and a set of post-dominating releases:
///
/// 1. Remove all ref count operations and the closure.
/// 2. Add each one of the last release locations insert releases for the
///    captured args if we have a partial_apply.
///
/// In the future this should be extended to be less conservative with users.
bool tryDeleteDeadClosure(SingleValueInstruction *closure,
                          InstModCallbacks callbacks = InstModCallbacks());

/// Given a SILValue argument to a partial apply \p Arg and the associated
/// parameter info for that argument, perform the necessary cleanups to Arg when
/// one is attempting to delete the partial apply.
void releasePartialApplyCapturedArg(
    SILBuilder &builder, SILLocation loc, SILValue arg,
    SILParameterInfo paramInfo,
    InstModCallbacks callbacks = InstModCallbacks());

/// Insert destroys of captured arguments of partial_apply [stack].
void insertDestroyOfCapturedArguments(
    PartialApplyInst *pai, SILBuilder &builder,
    llvm::function_ref<bool(SILValue)> shouldInsertDestroy =
        [](SILValue arg) -> bool { return true; });

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
bool calleesAreStaticallyKnowable(SILModule &module, AbstractFunctionDecl *afd);

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

SILInstruction *
tryOptimizeApplyOfPartialApply(PartialApplyInst *pai, SILBuilder &builder,
                               InstModCallbacks callbacks = InstModCallbacks());

} // end namespace swift

#endif
