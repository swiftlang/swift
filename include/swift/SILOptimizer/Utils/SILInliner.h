//===--- SILInliner.h - Inlines SIL functions -------------------*- C++ -*-===//
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
//
// This file defines the SILInliner class, used for inlining SIL functions into
// function application sites
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_SILINLINER_H
#define SWIFT_SIL_SILINLINER_H

#include "swift/AST/SubstitutionMap.h"
#include "swift/SIL/ApplySite.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILOpenedArchetypesTracker.h"
#include "llvm/ADT/DenseMap.h"
#include <functional>

namespace swift {

class SILOptFunctionBuilder;

// For now Free is 0 and Expensive is 1. This can be changed in the future by
// adding more categories.
enum class InlineCost : unsigned {
  Free = 0,
  Expensive = 1
};

/// Return the 'cost' of one instruction. Instructions that are expected to
/// disappear at the LLVM IR level are assigned a cost of 'Free'.
InlineCost instructionInlineCost(SILInstruction &I);

class SILInliner {
public:
  enum class InlineKind { MandatoryInline, PerformanceInline };

  using DeletionFuncTy = std::function<void(SILInstruction *)>;

private:
  SILOptFunctionBuilder &FuncBuilder;
  InlineKind IKind;
  SubstitutionMap ApplySubs;
  SILOpenedArchetypesTracker &OpenedArchetypesTracker;

  DeletionFuncTy DeletionCallback;

public:
  SILInliner(SILOptFunctionBuilder &FuncBuilder, InlineKind IKind,
             SubstitutionMap ApplySubs,
             SILOpenedArchetypesTracker &OpenedArchetypesTracker)
      : FuncBuilder(FuncBuilder), IKind(IKind), ApplySubs(ApplySubs),
        OpenedArchetypesTracker(OpenedArchetypesTracker) {}

  /// Returns true if we are able to inline \arg AI.
  ///
  /// *NOTE* This must be checked before attempting to inline \arg AI. If one
  /// attempts to inline \arg AI and this returns false, an assert will fire.
  static bool canInlineApplySite(FullApplySite apply);

  /// Allow the client to track instructions before they are deleted. The
  /// registered callback is called from
  /// recursivelyDeleteTriviallyDeadInstructions.
  ///
  /// (This is safer than the SILModule deletion callback because the
  /// instruction is still in a valid form and its operands can be inspected.)
  void setDeletionCallback(DeletionFuncTy f) { DeletionCallback = f; }

  /// Inline a callee function at the given apply site with the given
  /// arguments. Delete the apply and any dead arguments. Return a valid
  /// iterator to the first inlined instruction (or first instruction after the
  /// call for an empty function).
  ///
  /// This may split basic blocks and delete instructions.
  ///
  /// This only performs one step of inlining: it does not recursively
  /// inline functions called by the callee.
  ///
  /// *NOTE*: This attempts to perform inlining unconditionally and thus asserts
  /// if inlining will fail. All users /must/ check that a function is allowed
  /// to be inlined using SILInliner::canInlineApplySite before calling this
  /// function.
  SILBasicBlock::iterator inlineFunction(SILFunction *calleeFunction,
                                         FullApplySite apply,
                                         ArrayRef<SILValue> appliedArgs);
};

} // end namespace swift

#endif
