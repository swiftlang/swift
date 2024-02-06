//===--- VariableNameUtils.h ----------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
///
/// Utilities for inferring the name of a value.
///
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SILOPTIMIZER_UTILS_VARIABLENAMEUTILS_H
#define SWIFT_SILOPTIMIZER_UTILS_VARIABLENAMEUTILS_H

#include "swift/SIL/ApplySite.h"
#include "swift/SIL/DebugUtils.h"
#include "swift/SIL/MemAccessUtils.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/StackList.h"

namespace swift {

class VariableNameInferrer {
  /// The stacklist that we use to process from use->
  StackList<PointerUnion<SILInstruction *, SILValue>> variableNamePath;

  /// The root value of our string.
  ///
  /// If set, a diagnostic should do a 'root' is defined here error.
  SILValue rootValue;

  /// The final string we computed.
  SmallString<64> &resultingString;

public:
  VariableNameInferrer(SILFunction *fn, SmallString<64> &resultingString)
      : variableNamePath(fn), resultingString(resultingString) {}

  /// Attempts to infer a name from just uses of \p searchValue.
  ///
  /// Returns true if we found a name.
  bool tryInferNameFromUses(SILValue searchValue) {
    auto *use = getAnyDebugUse(searchValue);
    if (!use)
      return false;

    auto debugVar = DebugVarCarryingInst(use->getUser());
    if (!debugVar)
      return false;

    assert(debugVar.getKind() == DebugVarCarryingInst::Kind::DebugValue);
    resultingString += debugVar.getName();
    return true;
  }

  /// See if \p searchValue or one of its defs (walking use->def) has a name
  /// that we can use.
  ///
  /// \p failInsteadOfEmittingUnknown set to true if we should return false
  /// rather than emitting unknown (and always succeeding).
  ///
  /// \returns true if we inferred anything. Returns false otherwise.
  bool inferByWalkingUsesToDefs(SILValue searchValue) {
    // Look up our root value while adding to the variable name path list.
    auto rootValue = findDebugInfoProvidingValue(searchValue);
    if (!rootValue) {
      // If we do not pattern match successfully, just set resulting string to
      // unknown and return early.
      resultingString += "unknown";
      return true;
    }

    drainVariableNamePath();
    return true;
  }

  /// Infers the value that provides the debug info. This can be something like
  /// an alloc_stack that provides the information directly or a value that has
  /// a debug_value as a user.
  ///
  /// \returns SILValue() if we did not find anything.
  SILValue inferByWalkingUsesToDefsReturningRoot(SILValue searchValue) {
    // Look up our root value while adding to the variable name path list.
    auto rootValue = findDebugInfoProvidingValue(searchValue);
    if (!rootValue) {
      // If we do not pattern match successfully, return SILValue() early.
      return SILValue();
    }

    drainVariableNamePath();
    return rootValue;
  }

  StringRef getName() const { return resultingString; }

private:
  void drainVariableNamePath();

  /// Finds the SILValue that either provides the direct debug information or
  /// that has a debug_value user that provides the name of the value.
  SILValue findDebugInfoProvidingValue(SILValue searchValue);

  /// Given an initialized once allocation inst without a ValueDecl or a
  /// DebugVariable provided name, attempt to find a root value from its
  /// initialization.
  SILValue getRootValueForTemporaryAllocation(AllocationInst *allocInst);
};

} // namespace swift

#endif
