//===--- ArraySemantic.h - Wrapper around array semantic calls. -*- C++ -*-===//
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

#ifndef SWIFT_SILANALYSIS_ARRAYSEMANTIC_H
#define SWIFT_SILANALYSIS_ARRAYSEMANTIC_H

#include "swift/SIL/SILInstruction.h"

namespace swift {

class DominanceInfo;
class CallGraph;

/// The kind of array operation identified by looking at the semantics attribute
/// of the called function.
enum class ArrayCallKind {
  kNone = 0,
  kArrayPropsIsNative,
  kArrayPropsNeedsTypeCheck,
  kCheckSubscript,
  kCheckIndex,
  kGetCount,
  kGetCapacity,
  kGetElement,
  kGetArrayOwner,
  kGetElementAddress,
  kMakeMutable,
  kMutateUnknown,
  // The following two semantic function kinds return the result @owned
  // instead of operating on self passed as parameter.
  kArrayInit,
  kArrayUninitialized
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

  /// Get the index for operations that have one.
  SILValue getIndex() const;

  /// Get the array.props.isNative argument.
  SILValue getArrayPropertyIsNative() const;

  /// Get the array.props.needsElementTypeCheck argument.
  SILValue getArrayPropertyNeedsTypeCheck() const;

  /// Remove the semantics call replacing it by a release of any @owned
  /// parameter.
  ///
  /// Updates the passed in callgraph if non-null.
  void removeCall(CallGraph *CG);

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

  /// Is this an semantics call.
  operator bool() const { return SemanticsCall != nullptr; }

protected:
  /// Validate the signature of this call.
  bool isValidSignature();

  /// Hoist or copy the call to the insert point. If LeaveOriginal is true the
  /// call is copied to the insert point. Returns the copied call.
  ApplyInst *hoistOrCopy(SILInstruction *InsertBefore, DominanceInfo *DT,
                         bool LeaveOriginal);
};

} // End namespace swift.
#endif
