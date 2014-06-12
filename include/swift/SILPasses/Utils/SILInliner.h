//===--- SILInliner.h - Inlines SIL functions --------------------*- C++ -*-==//
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
//
// This file defines the SILInliner class, used for inlining SIL functions into
// function application sites
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_SILINLINER_H
#define SWIFT_SIL_SILINLINER_H

#include "llvm/ADT/DenseMap.h"
#include "swift/SIL/SILCloner.h"

namespace swift {

/// Helper that computes an integral cost metric for use with inlining/cloning
/// passes that want to compare against a threshold.
unsigned getFunctionCost(SILFunction *F, SILFunction *Callee, unsigned Cutoff);

class SILInliner : public SILCloner<SILInliner> {
public:
  friend class SILVisitor<SILInliner>;
  friend class SILCloner<SILInliner>;
  
  enum class InlineKind {
    MandatoryInline,
    PerformanceInline
  };

  explicit SILInliner(SILFunction &F, InlineKind IKind)
    : SILCloner<SILInliner>(F), IKind(IKind),
    CalleeEntryBB(nullptr), CallSiteScope(nullptr) {
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
  bool inlineFunction(ApplyInst *AI,
                      SILFunction *CalleeFunction,
                      ArrayRef<SILValue> Args);

private:
  void visitSILBasicBlock(SILBasicBlock* BB);
  
  void visitDebugValueInst(DebugValueInst *Inst);
  void visitDebugValueAddrInst(DebugValueAddrInst *Inst);

  
  SILDebugScope *getOrCreateInlineScope(SILInstruction *Orig);
  void postProcess(SILInstruction *Orig, SILInstruction *Cloned) {
    if (IKind == InlineKind::MandatoryInline)
      // Transparent functions are inheriting the location of the call
      // site. No soup, err, debugging for you!
      Cloned->setDebugScope(CallSiteScope);
    else
      // Create an inlined version of the scope.
      Cloned->setDebugScope(getOrCreateInlineScope(Orig));

    SILCloner<SILInliner>::postProcess(Orig, Cloned);
  }

  SILLocation remapLocation(SILLocation InLoc) {
    // Inlined location wraps the call site that is being inlined, regardless
    // of the input location.
    return Loc.hasValue() ? Loc.getValue() : InLoc;
  }

  InlineKind IKind;
  
  SILBasicBlock *CalleeEntryBB;

  /// \brief The location representing the inlined instructions.
  ///
  /// This location wrapps the call site AST node that is being inlined.
  /// Alternatively, it can be the SIL file location of the call site (in case
  /// of SIL-to-SIL transformations).
  Optional<SILLocation> Loc;
  SILDebugScope *CallSiteScope;
  SILFunction *CalleeFunction;
  llvm::SmallDenseMap<SILDebugScope *, SILDebugScope *> InlinedScopeCache;
};

} // end namespace swift

#endif
