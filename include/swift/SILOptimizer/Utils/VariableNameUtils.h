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

#include "swift/Basic/Defer.h"
#include "swift/Basic/OptionSet.h"
#include "swift/SIL/ApplySite.h"
#include "swift/SIL/DebugUtils.h"
#include "swift/SIL/MemAccessUtils.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/StackList.h"

namespace swift {

class VariableNameInferrer {
public:
  enum class Flag {
    /// If set then we should look through get and set accessors and infer their
    /// name from self.
    ///
    /// DISCUSSION: This may not be the correct semantics for all name inference
    /// since we may want to consider computed properties to be tied to self.
    InferSelfThroughAllAccessors = 0x1,
  };

  using Options = OptionSet<Flag>;

private:
  /// A two phase stack data structure. The first phase only allows for two
  /// operations:
  ///
  /// 1. pushing elements onto the stack.
  /// 2. pushing/popping a "snapshot" of the stack (see description below).
  ///
  /// The second phase only allows for the stack to be drained.
  ///
  /// DISCUSSION: The snapshot operation stashes the current size of the
  /// variable name path array when the snapshot operation occurs. If one pops
  /// the snapshot, the data structure sets its current insertion point to be
  /// the old stack point effectively popping off all of the elements of the
  /// stack until the last snapshot. This is useful when working with things
  /// like phis where one wants to speculatively push items onto this stack
  /// while discovering if one has an actual interesting value from the phi. If
  /// one fails to find something interesting, then one can just pop the
  /// snapshot and go process the next phi incoming value.
  template <typename T, unsigned SmallSize>
  class VariableNamePathArray {
    SmallVector<T, SmallSize> data;

    unsigned lastSnapShotIndex = 0;
    unsigned insertionPointIndex = 0;

  public:
    VariableNamePathArray() : data() {}

    ArrayRef<T> getData() const {
      assert(insertionPointIndex <= data.size());
      return ArrayRef<T>(data).take_front(insertionPointIndex);
    }

    void print(llvm::raw_ostream &os) const {
      os << "LastSnapShotIndex: " << lastSnapShotIndex << '\n';
      os << "InsertionPointIndex: " << insertionPointIndex << '\n';
    }

    SWIFT_DEBUG_DUMP { print(llvm::dbgs()); }

    /// Pushes a snapshot and returns the old index.
    unsigned pushSnapShot() & {
      // After we run with a snapshot, we want:
      //
      // 1. lastSnapShotIndex to return to its value before the continuation
      // ran.
      // 2. The insertion point index becomes lastSnapShotIndex.
      unsigned oldSnapShotIndex = lastSnapShotIndex;
      lastSnapShotIndex = insertionPointIndex;
      return oldSnapShotIndex;
    }

    void popSnapShot(unsigned oldIndex) & {
      insertionPointIndex = lastSnapShotIndex;
      lastSnapShotIndex = oldIndex;
    }

    void returnSnapShot(unsigned oldIndex) & { lastSnapShotIndex = oldIndex; }

    void push_back(const T &newValue) & {
      SWIFT_DEFER { assert(insertionPointIndex <= data.size()); };
      if (insertionPointIndex == data.size()) {
        data.push_back(newValue);
        ++insertionPointIndex;
        return;
      }

      data[insertionPointIndex] = newValue;
      ++insertionPointIndex;
    }

    [[nodiscard]] T pop_back_val() & {
      SWIFT_DEFER { assert(insertionPointIndex <= data.size()); };
      assert(!lastSnapShotIndex &&
             "Can only pop while lastSnapShotIndex is not set");
      --insertionPointIndex;
      return data[insertionPointIndex];
    }

    bool empty() const { return !insertionPointIndex; }
  };

  /// ASTContext for forming identifiers when we need to.
  ASTContext &astContext;

  /// The stacklist that we use to print out variable names.
  ///
  /// Has to be a small vector since we push/pop the last segment start. This
  /// lets us speculate when processing phis.
  VariableNamePathArray<StringRef, 4> variableNamePath;

  /// The root value of our string.
  ///
  /// If set, a diagnostic should do a 'root' is defined here error.
  SILValue rootValue;

  /// The final string we computed.
  SmallString<64> &resultingString;

  /// Options that control how we do our walk.
  ///
  /// Example: In certain cases we may want to impute self as a name for
  /// computed getters/setters and in other cases we may not want to.
  Options options;

public:
  VariableNameInferrer(SILFunction *fn, SmallString<64> &resultingString)
      : astContext(fn->getASTContext()), variableNamePath(),
        resultingString(resultingString) {}

  VariableNameInferrer(SILFunction *fn, Options options,
                       SmallString<64> &resultingString)
      : astContext(fn->getASTContext()), variableNamePath(),
        resultingString(resultingString), options(options) {}

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

  SWIFT_DEBUG_DUMP { llvm::dbgs() << getName() << '\n'; }

  /// Given a specific SILValue, construct a VariableNameInferrer and use it to
  /// attempt to infer an identifier for the value.
  static std::optional<Identifier> inferName(SILValue value);

  /// Given a specific SILValue, construct a VariableNameInferrer and use it to
  /// attempt to infer an identifier for the value and a named value.
  static std::optional<std::pair<Identifier, SILValue>>
  inferNameAndRoot(SILValue value);

  /// Given a specific decl \p d, come up with a name for it.
  ///
  /// This is used internally for translating all decls to names. This is
  /// exposed in case someone wants to wrap VariableNameUtils and needs to use
  /// the same internal mapping that VariableNameUtils uses.
  static StringRef getNameFromDecl(Decl *d);

private:
  void drainVariableNamePath();

  /// Finds the SILValue that either provides the direct debug information or
  /// that has a debug_value user that provides the name of the value.
  SILValue findDebugInfoProvidingValue(SILValue searchValue);

  /// Do not call this directly. Used just to improve logging for
  /// findDebugInfoProvidingValue.
  SILValue findDebugInfoProvidingValueHelper(SILValue searchValue,
                                             ValueSet &visitedValues);

  /// A special helper for handling phi values. Do not call this directly.
  SILValue findDebugInfoProvidingValuePhiArg(SILValue incomingValue,
                                             ValueSet &visitedValues);

  /// Given an initialized once allocation inst without a ValueDecl or a
  /// DebugVariable provided name, attempt to find a root value from its
  /// initialization.
  SILValue getRootValueForTemporaryAllocation(AllocationInst *allocInst);

  StringRef getStringRefForIndex(unsigned index) const {
    llvm::SmallString<64> indexString;
    {
      llvm::raw_svector_ostream stream(indexString);
      stream << index;
    }
    return astContext.getIdentifier(indexString).str();
  }
};

} // namespace swift

#endif
