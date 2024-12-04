//===--- InstructionUtils.h - Utilities for SIL instructions ----*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_INSTRUCTIONUTILS_H
#define SWIFT_SIL_INSTRUCTIONUTILS_H

#include "swift/SIL/InstWrappers.h"
#include "swift/SIL/RuntimeEffect.h"
#include "swift/SIL/SILModule.h"

namespace swift {

//===----------------------------------------------------------------------===//
//                         SSA Use-Def Helpers
//===----------------------------------------------------------------------===//

/// Strip off casts/indexing insts/address projections from V until there is
/// nothing left to strip.
SILValue getUnderlyingObject(SILValue V);

SILValue stripSinglePredecessorArgs(SILValue V);

/// Return the underlying SILValue after stripping off all casts from the
/// current SILValue.
SILValue stripCasts(SILValue V);

/// Return the underlying SILValue after stripping off all casts (but
/// mark_dependence) from the current SILValue.
SILValue stripCastsWithoutMarkDependence(SILValue V);

/// Return the underlying SILValue after looking through all copy_value and
/// begin_borrow instructions.
SILValue lookThroughOwnershipInsts(SILValue v);

/// Reverse of lookThroughOwnershipInsts.
///
/// Return true if \p visitor returned true for all uses.
bool visitNonOwnershipUses(SILValue value,
                           function_ref<bool(Operand *)> visitor);

/// Return the underlying SILValue after looking through all copy_value
/// instructions.
SILValue lookThroughCopyValueInsts(SILValue v);

/// Return the underlying SILValue after stripping off all upcasts from the
/// current SILValue.
SILValue stripUpCasts(SILValue V);

/// Return the underlying SILValue after stripping off all
/// upcasts and downcasts.
SILValue stripClassCasts(SILValue V);

/// Return the underlying SILValue after stripping off all address projection
/// instructions.
///
/// FIXME: Today address projections are referring to the result of the
/// projection and doesn't consider the operand. Should we change this?
SILValue stripAddressProjections(SILValue V);

/// Look through any projections that transform an address -> an address.
SILValue lookThroughAddressToAddressProjections(SILValue v);

/// Look through address and value projections
SILValue lookThroughAddressAndValueProjections(SILValue V);

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

/// If V is a begin_borrow, strip off the begin_borrow and return. Otherwise,
/// ust return V.
SILValue stripBorrow(SILValue V);

//===----------------------------------------------------------------------===//
//                         Instruction Properties
//===----------------------------------------------------------------------===//

/// Return a non-null SingleValueInstruction if the given instruction merely
/// copies or moves the value of its first operand, possibly changing its type
/// or ownership state, but otherwise having no effect.
///
/// The returned instruction may have additional "incidental" operands;
/// mark_dependence for example.
///
/// This is useful for checking all users of a value to verify that the value is
/// only used in recognizable patterns without otherwise "escaping". These are
/// instructions that the use-visitor can recurse into. Note that the value's
/// type may be changed by a cast.
SingleValueInstruction *getSingleValueCopyOrCast(SILInstruction *I);

// Return true if this instruction begins a SIL-level scope. If so, it must have
// a single result. That result must have an isEndOfScopeMarker direct use on
// all reachable paths. This instruction along with its scope-ending
// instructions are considered a single operation. They must be inserted and
// deleted together.
bool isBeginScopeMarker(SILInstruction *user);

/// Return true if this instruction terminates a SIL-level scope. Scope end
/// instructions do not produce a result. Their single operand must be an
/// isBeginScopeMarker and cannot be 'undef'.
bool isEndOfScopeMarker(SILInstruction *user);

/// Return true if the given instruction has no effect on it's operand values
/// and produces no result. These are typically end-of scope markers.
///
/// This is useful for checking all users of a value to verify that the value is
/// only used in recognizable patterns without otherwise "escaping".
bool isIncidentalUse(SILInstruction *user);

/// Return true if the given `user` instruction modifies the value's refcount
/// without propagating the value or having any other effect aside from
/// potentially destroying the value itself (and executing associated cleanups).
///
/// This is useful for checking all users of a value to verify that the value is
/// only used in recognizable patterns without otherwise "escaping".
bool onlyAffectsRefCount(SILInstruction *user);

/// Returns true if the given user instruction checks the ref count of a
/// pointer.
bool mayCheckRefCount(SILInstruction *User);

/// Return true when the instruction represents added instrumentation for
/// run-time sanitizers.
bool isSanitizerInstrumentation(SILInstruction *Instruction);

/// Return true when the instruction represents added instrumentation for
/// run-time sanitizers or code coverage.
bool isInstrumentation(SILInstruction *Instruction);

/// Check that this is a partial apply of a reabstraction thunk and return the
/// argument of the partial apply if it is.
SILValue isPartialApplyOfReabstractionThunk(PartialApplyInst *PAI);

/// Returns true if \p PAI is only used by an assign_by_wrapper instruction as
/// init or set function.
bool onlyUsedByAssignByWrapper(PartialApplyInst *PAI);

/// Returns true if \p PAI is only used by an \c assign_or_init
/// instruction as init or set function.
bool onlyUsedByAssignOrInit(PartialApplyInst *PAI);

/// Returns the runtime effects of \p inst.
///
/// Predicts which runtime calls are called in the generated code for `inst`.
/// This is sometimes a conservative approximation, i.e. more runtime effects
/// are reported than actually happen.
/// If the runtime effects can be associated with a type, this type is returned
/// in `impactType`. That's useful for diagnostics.
RuntimeEffect getRuntimeEffect(SILInstruction *inst, SILType &impactType);

/// If V is a function closure, return the reaching set of partial_apply's.
void findClosuresForFunctionValue(SILValue V,
                                  TinyPtrVector<PartialApplyInst *> &results);

/// Given a polymorphic builtin \p bi that may be generic and thus have in/out
/// params, stash all of the information needed for either specializing while
/// inlining or propagating the type in constant propagation.
///
/// NOTE: If we perform this transformation, our builtin will no longer have any
/// substitutions since we only substitute to concrete static overloads.
struct PolymorphicBuiltinSpecializedOverloadInfo {
  const BuiltinInfo *builtinInfo;
  Identifier staticOverloadIdentifier;
  SmallVector<SILType, 8> argTypes;
  SILType resultType;
  bool hasOutParam;

private:
  bool isInitialized;

public:
  PolymorphicBuiltinSpecializedOverloadInfo()
      : builtinInfo(nullptr), staticOverloadIdentifier(), argTypes(),
        resultType(), hasOutParam(false), isInitialized(false) {}

  /// Returns true if we were able to map the polymorphic builtin to a static
  /// overload. False otherwise.
  ///
  /// NOTE: This does not mean that the static overload actually exists.
  bool init(BuiltinInst *bi);

  bool doesOverloadExist() const {
    CanBuiltinType builtinType = argTypes.front().getAs<BuiltinType>();
    return canBuiltinBeOverloadedForType(builtinInfo->ID, builtinType);
  }

private:
  bool init(SILFunction *fn, BuiltinValueKind builtinKind,
            ArrayRef<SILType> oldOperandTypes, SILType oldResultType);
};

/// Given a polymorphic builtin \p bi, analyze its types and create a builtin
/// for the static overload that the builtin corresponds to. If \p bi is not a
/// polymorphic builtin or does not have any available overload for these types,
/// return SILValue().
SILValue getStaticOverloadForSpecializedPolymorphicBuiltin(BuiltinInst *bi);

/// Visit the exploded leaf elements of a tuple type that contains potentially
/// a tree of tuples.
///
/// If visitor returns false, we stop processing early. We return true if we
/// visited all of the tuple elements without the visitor returing false.
bool visitExplodedTupleType(SILType type,
                            llvm::function_ref<bool(SILType)> callback);

/// Visit the exploded leaf elements of a tuple type that contains potentially
/// a tree of tuples.
///
/// If visitor returns false, we stop processing early. We return true if we
/// visited all of the tuple elements without the visitor returing false.
bool visitExplodedTupleValue(SILValue value,
                             llvm::function_ref<SILValue(SILValue, std::optional<unsigned>)> callback);

std::pair<SILFunction *, SILWitnessTable *>
lookUpFunctionInWitnessTable(WitnessMethodInst *wmi, SILModule::LinkingMode linkingMode);

/// True if a type can be expanded without a significant increase to code size.
///
/// False if expanding a type is invalid. For example, expanding a
/// struct-with-deinit drops the deinit.
bool shouldExpand(SILModule &module, SILType ty);

} // end namespace swift

#endif
