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
  class SILOptions;
  class SILTransform;

  /// \brief Run all the SIL diagnostic passes on \p M.
  ///
  /// \returns true if the diagnostic passes produced an error
  bool runSILDiagnosticPasses(SILModule &M, const SILOptions &Options);

  /// \brief Run all the SIL performance optimization passes on \p M.
  void runSILOptimizationPasses(SILModule &M, const SILOptions &Options);

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

  /// \brief Specialize generic functions by cloning them and replacing the
  /// abstract type with the concrete type.
  bool performSILSpecialization(SILModule *M);

  /// \brief Devirtualize virtual function calls into direct
  /// function calls.
  bool performSILDevirtualization(SILModule *M);

  /// \brief Link a SILFunction declaration to the actual definition in the
  /// serialized modules.
  ///
  /// \param M the SILModule on which to operate
  /// \param LinkAll when true, always link. For testing purposes.
  void performSILLinking(SILModule *M, bool LinkAll = false);

  /// \brief Optimize away shadow variables for any inout arguments that don't
  /// escape.
  void performInOutDeshadowing(SILModule *M);

  /// \brief Inline functions marked transparent. Diagnose attempts to
  /// circularly inline
  void performSILMandatoryInlining(SILModule *M);

  /// \brief Perform Mem2Reg.
  void performSILMem2Reg(SILModule *M);

  /// \brief Promote closure captures from [inout] to by-value.
  void performSILCapturePromotion(SILModule *M);

  /// \brief Analyze the SIL module for correcntess and generate user
  /// diagnostics if any.
  void emitSILDataflowDiagnostics(SILModule *M);

  /// \brief Cleanup instructions/builtin calls not suitable for IRGen.
  void performSILCleanup(SILModule *M);

  /// \brief Perform SIL Inlining for Performance.
  void performSILPerformanceInlining(SILModule *M,
                                     unsigned inlineCostThreshold);

  /// \brief Perform SIL code motion optimizations.
  void performSILCodeMotion(SILModule *M);

  /// \brief Replace aggregate instructions with scalar instructions.
  void performSILLowerAggregateInstrs(SILModule *M);

  /// \brief Replace aggregate allocations with scalar allocations.
  void performSILSROA(SILModule *M);

  /// \brief Remove redundant ARC memory operations.
  void performSILARCOpts(SILModule *M);

  /// \brief Eliminate debug info instructions.
  void performSILStripDebugInfo(SILModule *M);

  /// \brief Eliminate alloc_ref with well behaved destructors that have no uses
  /// with side effects.
  void performSILAllocRefElimination(SILModule *M);

  /// \brief Count the number of instructions in the module for all
  /// instructions.
  void performSILInstCount(SILModule *M);

  // Diagnostics transformations.
  SILTransform *createCapturePromotion();
  SILTransform *createInOutDeshadowing();
  SILTransform *createDefiniteInitialization();
  SILTransform *createPredictableMemoryOptimizations();
  SILTransform *createConstantPropagation();
  SILTransform *createDCE();
  SILTransform *createMandatoryInlining();
  SILTransform *createSILCleanup();
  SILTransform *createEmitDFDiagnostics();

  // Performance transformations.
  SILTransform *createSILCombine();
  SILTransform *createDeadFunctionElimination();
  SILTransform *createLowerAggregate();
  SILTransform *createSROA();
  SILTransform *createMem2Reg();
  SILTransform *createCSE();
  SILTransform *createCodeMotion();
  SILTransform *createPerfInliner(int threshold);
  SILTransform *createGenericSpecializer();
  SILTransform *createSILARCOpts();
  SILTransform *createSimplifyCFG();
  SILTransform *createDevirtualization();
  SILTransform *createStackPromotion();
  SILTransform *createSILAllocRefElimination();

  // Utilities
  SILTransform *createStripDebug();
  SILTransform *createSILInstCount();
} // end namespace swift

#endif
