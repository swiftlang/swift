//===--- ArraySemantic.h - Wrapper around array semantic calls. -*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SILOPTIMIZER_ANALYSIS_ARRAYSEMANTIC_H
#define SWIFT_SILOPTIMIZER_ANALYSIS_ARRAYSEMANTIC_H

#include "swift/SIL/SILInstruction.h"

namespace swift {

class DominanceInfo;

/// The kind of array operation identified by looking at the semantics attribute
/// of the called function.
enum class ArrayCallKind {
  kNone = 0,
  kArrayPropsIsNativeTypeChecked,
  kCheckSubscript,
  kCheckIndex,
  kGetCount,
  kGetCapacity,
  kGetElement,
  kGetArrayOwner,
  kGetElementAddress,
  kMakeMutable,
  kMutateUnknown,
  kReserveCapacityForAppend,
  kWithUnsafeMutableBufferPointer,
  kAppendContentsOf,
  kAppendElement,
  // The following two semantic function kinds return the result @owned
  // instead of operating on self passed as parameter. If you are adding
  // a function, and it has a self parameter, make sure that it is defined
  // before this comment.
  kArrayInit,
  kArrayUninitialized
};

/// Wrapper around array semantic calls.
class ArraySemanticsCall {
  ApplyInst *SemanticsCall;

  void initialize(ApplyInst *apply, StringRef semanticString,
                  bool matchPartialName);

public:
  /// Match calls with any array semantic.
  template <class NodeTy>
  ArraySemanticsCall(NodeTy node)
    : ArraySemanticsCall(node, "array.", /*allow partial*/ true) {}

  /// Match calls with a specific array semantic.
  template <class NodeTy>
  ArraySemanticsCall(NodeTy node, StringRef semanticName)
    : ArraySemanticsCall(node, semanticName, /*allow partial*/ false) {}

  /// Match array semantic calls.
  ArraySemanticsCall(ApplyInst *apply, StringRef SemanticStr,
                     bool MatchPartialName);

  /// Match array semantic calls.
  ArraySemanticsCall(SILInstruction *I, StringRef semanticName,
                     bool matchPartialName);

  /// Match array semantic calls.
  ArraySemanticsCall(SILValue V, StringRef semanticName,
                     bool matchPartialName);

  /// Can we hoist this call.
  bool canHoist(SILInstruction *To, DominanceInfo *DT) const;

  /// Determine which kind of array semantics call this is.
  ArrayCallKind getKind() const;

  /// Does this semantic call has a self argument.
  ///
  /// For example, kArrayInit and kArrayUninitialized don't.
  bool hasSelf() const;

  /// Does this instruction have guaranteed self.
  ///
  /// Once +0 self is enabled, this can be removed in favor of just hasSelf()
  /// since all of these methods will have guaranteed self always.
  bool hasGuaranteedSelf() const;

  /// Get the self argument.
  SILValue getSelf() const;

  /// Get the self argument operand.
  Operand &getSelfOperand() const;

  /// Returns true if this array.get_element call returns the element
  /// as a direct result (and not as an indirect result).
  bool hasGetElementDirectResult() const;

  /// Returns the wasNativeTypeChecked argument of this
  /// array.get_element call.
  SILValue getTypeCheckedArgument() const;

  /// Returns the matchingSubscriptCheck argument of this
  /// array.get_element call.
  SILValue getSubscriptCheckArgument() const;

  /// Get the index for operations that have one.
  SILValue getIndex() const;

  /// Get the index as a constant if possible.
  Optional<int64_t> getConstantIndex() const;

  /// Get the array.props.isNativeTypeChecked argument.
  SILValue getArrayPropertyIsNativeTypeChecked() const;

  /// Get the count used for this array initialization.
  ///
  /// Returns SILValue() if this is not an array initialization call or the call
  /// can't be parsed.
  SILValue getInitializationCount() const;

  /// Get the array value returned by an array initialization call.
  ///
  /// Returns SILValue() if this is not an array initialization call.
  SILValue getArrayValue() const;

  /// Get the array element storage pointer returned by an array initialization
  /// call.
  ///
  /// Returns SILValue() if this is not an array initialization call or the call
  /// can't be parsed.
  SILValue getArrayElementStoragePointer() const;

  /// Remove the semantics call replacing it by a release of any @owned
  /// parameter.
  void removeCall();

  /// Replace a call to get_element by a value.
  ///
  /// Preconditions:
  /// The value \p V must dominate this get_element call.
  /// This must be a get_element call.
  ///
  /// Returns true on success, false otherwise.
  bool replaceByValue(SILValue V);

  /// Replace a call to append(contentsOf: ) with a series of
  /// append(element: ) calls.
  bool replaceByAppendingValues(SILModule &M, SILFunction *AppendFn,
                                SILFunction *ReserveFn,
                                const llvm::SmallVectorImpl<SILValue> &Vals,
                                SubstitutionMap Subs);

  /// Hoist the call to the insert point.
  void hoist(SILInstruction *InsertBefore, DominanceInfo *DT) {
    hoistOrCopy(InsertBefore, DT, false);
  }

  /// Copy the call to the insert point and return the newly created call.
  ApplyInst *copyTo(SILInstruction *InsertBefore, DominanceInfo *DT) {
    return hoistOrCopy(InsertBefore, DT, true);
  }

  /// Get the semantics call as an ApplyInst.
  operator ApplyInst *() const { return SemanticsCall; }

  SILValue getCallResult() const { return SemanticsCall; }

  /// Is this a semantics call.
  operator bool() const { return SemanticsCall != nullptr; }

  /// Is this a call which is not used to mutate the array.
  bool doesNotChangeArray() const;

  /// Could this array be backed by an NSArray.
  bool mayHaveBridgedObjectElementType() const;
  
  /// Can this function be inlined by the early inliner.
  bool canInlineEarly() const;

protected:
  /// Validate the signature of this call.
  bool isValidSignature();

  /// Hoist or copy the call to the insert point. If LeaveOriginal is true the
  /// call is copied to the insert point. Returns the copied call.
  ApplyInst *hoistOrCopy(SILInstruction *InsertBefore, DominanceInfo *DT,
                         bool LeaveOriginal);
};

} // end namespace swift
#endif
