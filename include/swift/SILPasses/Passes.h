//===-------- Passes.h - Swift Compiler SIL Pass Entrypoints ----*- C++ -*-===//
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
//  This file declares the main entrypoints to SIL passes.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SILPASSES_PASSES_H
#define SWIFT_SILPASSES_PASSES_H

namespace swift {

  class SILModule;

  /// performSILDefiniteInitialization - Perform definitive initialization
  /// analysis, applying flow sensitive analysis to the SILGen generated code
  /// to determine whether unadorned assignment operations are actually
  /// assignment or if they are initializations.
  void performSILDefiniteInitialization(SILModule *M);

  /// performSILPredictableMemoryOptimizations - Perform predictable memory
  /// optimizations, including promoting operations to SSA form so that later
  /// analysis can depend on SSA use-def chains.
  void performSILPredictableMemoryOptimizations(SILModule *M);

  /// performSILAllocBoxToStackPromotion - Promote alloc_box into stack
  /// allocations.
  void performSILAllocBoxToStackPromotion(SILModule *M);

  /// \brief Fold instructions with constant operands. Diagnose overflows when
  /// possible.
  void performSILConstantPropagation(SILModule *M);

  /// \brief Detect and remove unreachable code. Diagnose provably unreachable
  /// user code.
  void performSILDeadCodeElimination(SILModule *M);

  /// \brief Combine instructions to form fewer, simple instructions via a
  /// simple worklist driven algorithm.
  void performSILCombine(SILModule *M);

  /// \brief Perform constant subexpression elimination.
  void performSILCSE(SILModule *M);

  /// \brief Simplify the CFG of SIL functions.
  void performSimplifyCFG(SILModule *M);

  /// \brief Link a SILFunction declaration to the actual definition in the
  /// serialized modules.
  void performSILLinking(SILModule *M);

  /// \brief Optimize away shadow variables for any inout arguments that don't
  /// escape.
  void performInOutDeshadowing(SILModule *M);

  /// \brief Inline functions marked transparent. Diagnose attempts to
  /// circularly inline
  void performSILMandatoryInlining(SILModule *M);

  /// \brief Promote closure captures from [inout] to by-value.
  void performSILCapturePromotion(SILModule *M);

  /// \brief Analyze the SIL module for correcntess and generate user
  /// diagnostics if any.
  void emitSILDataflowDiagnostics(SILModule *M);

  /// \brief Cleanup instructions/builtin calls not suitable for IRGen.
  void performSILCleanup(SILModule *M);

} // end namespace swift

#endif
