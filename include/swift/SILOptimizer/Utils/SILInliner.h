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

#include "llvm/ADT/DenseMap.h"
#include "swift/SIL/TypeSubstCloner.h"
#include <functional>

namespace swift {

// For now Free is 0 and Expensive is 1. This can be changed in the future by
// adding more categories.
enum class InlineCost : unsigned {
  Free = 0,
  Expensive = 1
};

/// Return the 'cost' of one instruction. Instructions that are expected to
/// disappear at the LLVM IR level are assigned a cost of 'Free'.
InlineCost instructionInlineCost(SILInstruction &I);

class SILInliner : public TypeSubstCloner<SILInliner> {
public:
  friend class SILVisitor<SILInliner>;
  friend class SILCloner<SILInliner>;
  
  enum class InlineKind {
    MandatoryInline,
    PerformanceInline
  };

  SILInliner(SILFunction &To, SILFunction &From, InlineKind IKind,
             SubstitutionList ApplySubs,
             SILOpenedArchetypesTracker &OpenedArchetypesTracker,
             CloneCollector::CallbackType Callback = nullptr)
      : TypeSubstCloner<SILInliner>(To, From, ApplySubs,
                                    OpenedArchetypesTracker, true),
        IKind(IKind), CalleeEntryBB(nullptr), CallSiteScope(nullptr),
        Callback(Callback) {
  }

  /// inlineFunction - This method inlines a callee function, assuming that it
  /// is called with the given arguments, into the caller at a given instruction
  /// (as specified by a basic block iterator), assuming that the instruction
  /// corresponds semantically to an application of the function. It only
  /// performs one step of inlining: it does not recursively inline functions
  /// called by the callee.
  ///
  /// Returns true on success or false if it is unable to inline the function
  /// (for any reason). If successful, I now points to the first inlined
  /// instruction, or the next instruction after the removed instruction in the
  /// original function, in case the inlined function is completely trivial
  bool inlineFunction(FullApplySite AI, ArrayRef<SILValue> Args);

private:
  void visitDebugValueInst(DebugValueInst *Inst);
  void visitDebugValueAddrInst(DebugValueAddrInst *Inst);

  const SILDebugScope *getOrCreateInlineScope(const SILDebugScope *DS);

  void postProcess(SILInstruction *Orig, SILInstruction *Cloned) {
    // Call client-supplied callback function.
    if (Callback)
      Callback(Orig, Cloned);

    // We just updated the debug scope information. Intentionally
    // don't call SILClonerWithScopes<SILInliner>::postProcess().
    SILCloner<SILInliner>::postProcess(Orig, Cloned);
  }

  SILLocation remapLocation(SILLocation InLoc) {
    // For performance inlining return the original location.
    if (IKind == InlineKind::PerformanceInline)
      return InLoc;
    // Inlined location wraps the call site that is being inlined, regardless
    // of the input location.
    return Loc.hasValue() ? Loc.getValue() :
      MandatoryInlinedLocation::getMandatoryInlinedLocation((Decl*)nullptr);
  }

  const SILDebugScope *remapScope(const SILDebugScope *DS) {
    if (IKind == InlineKind::MandatoryInline)
      // Transparent functions are absorbed into the call
      // site. No soup, err, debugging for you!
      return CallSiteScope;
    else
      // Create an inlined version of the scope.
      return getOrCreateInlineScope(DS);
  }

  InlineKind IKind;
  
  SILBasicBlock *CalleeEntryBB;

  /// \brief The location representing the inlined instructions.
  ///
  /// This location wraps the call site AST node that is being inlined.
  /// Alternatively, it can be the SIL file location of the call site (in case
  /// of SIL-to-SIL transformations).
  Optional<SILLocation> Loc;
  const SILDebugScope *CallSiteScope;
  SILFunction *CalleeFunction;
  llvm::SmallDenseMap<const SILDebugScope *,
                      const SILDebugScope *> InlinedScopeCache;
  CloneCollector::CallbackType Callback;
};

} // end namespace swift

#endif
