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

#include "swift/SIL/SILInstruction.h"

namespace swift {

/// Strip off casts/indexing insts/address projections from V until there is
/// nothing left to strip.
SILValue getUnderlyingObject(SILValue V);

/// Strip off indexing and address projections.
///
/// This is similar to getUnderlyingObject, except that it does not strip any
/// object-to-address projections, like ref_element_addr. In other words, the
/// result is always an address value.
SILValue getUnderlyingAddressRoot(SILValue V);

SILValue getUnderlyingObjectStopAtMarkDependence(SILValue V);

SILValue stripSinglePredecessorArgs(SILValue V);

/// Return the underlying SILValue after stripping off all casts from the
/// current SILValue.
SILValue stripCasts(SILValue V);

/// Return the underlying SILValue after stripping off all casts (but
/// mark_dependence) from the current SILValue.
SILValue stripCastsWithoutMarkDependence(SILValue V);

/// Return the underlying SILValue after stripping off all upcasts from the
/// current SILValue.
SILValue stripUpCasts(SILValue V);

/// Return the underlying SILValue after stripping off all
/// upcasts and downcasts.
SILValue stripClassCasts(SILValue V);

/// Return the underlying SILValue after stripping off non-projection address
/// casts. The result will still be an address--this does not look through
/// pointer-to-address.
SILValue stripAddressAccess(SILValue V);

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

/// If V is a begin_borrow, strip off the begin_borrow and return. Otherwise,
/// ust return V.
SILValue stripBorrow(SILValue V);

/// Return a non-null SingleValueInstruction if the given instruction merely
/// copies the value of its first operand, possibly changing its type or
/// ownership state, but otherwise having no effect.
///
/// The returned instruction may have additional "incidental" operands;
/// mark_dependence for example.
///
/// This is useful for checking all users of a value to verify that the value is
/// only used in recognizable patterns without otherwise "escaping". These are
/// instructions that the use-visitor can recurse into. Note that the value's
/// type may be changed by a cast.
SingleValueInstruction *getSingleValueCopyOrCast(SILInstruction *I);

/// Return true if this instruction terminates a SIL-level scope. Scope end
/// instructions do not produce a result.
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

/// Return true when the instruction represents added instrumentation for
/// run-time sanitizers.
bool isSanitizerInstrumentation(SILInstruction *Instruction);

/// If V is a convert_function or convert_escape_to_noescape return its operand
/// recursively.
SILValue stripConvertFunctions(SILValue V);

/// Check that this is a partial apply of a reabstraction thunk and return the
/// argument of the partial apply if it is.
SILValue isPartialApplyOfReabstractionThunk(PartialApplyInst *PAI);

/// If V is a function closure, return the reaching set of partial_apply's.
void findClosuresForFunctionValue(SILValue V,
                                  TinyPtrVector<PartialApplyInst *> &results);

/// A utility class for evaluating whether a newly parsed or deserialized
/// function has qualified or unqualified ownership.
///
/// The reason that we are using this is that we would like to avoid needing to
/// add code to the SILParser or to the Serializer to support this temporary
/// staging concept of a function having qualified or unqualified
/// ownership. Once SemanticARC is complete, SILFunctions will always have
/// qualified ownership, so the notion of an unqualified ownership function will
/// no longer exist.
///
/// Thus we note that there are three sets of instructions in SIL from an
/// ownership perspective:
///
///    a. ownership qualified instructions
///    b. ownership unqualified instructions
///    c. instructions that do not have ownership semantics (think literals,
///       geps, etc).
///
/// The set of functions can be split into ownership qualified and ownership
/// unqualified using the rules that:
///
///    a. a function can never contain both ownership qualified and ownership
///       unqualified instructions.
///    b. a function that contains only instructions without ownership semantics
///       is considered ownership qualified.
///
/// Thus we can know when parsing/serializing what category of function we have
/// and set the bit appropriately.
class FunctionOwnershipEvaluator {
  NullablePtr<SILFunction> F;
  bool HasOwnershipQualifiedInstruction = false;

public:
  FunctionOwnershipEvaluator() {}
  FunctionOwnershipEvaluator(SILFunction *F) : F(F) {}
  void reset(SILFunction *NewF) {
    F = NewF;
    HasOwnershipQualifiedInstruction = false;
  }
  bool evaluate(SILInstruction *I);
};

} // end namespace swift

#endif
