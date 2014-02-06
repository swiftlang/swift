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

  /// \brief Detect and remove unreachable code. Diagnose provably unreachable
  /// user code.
  void performSILDeadCodeElimination(SILModule *M);

  /// \brief Link a SILFunction declaration to the actual definition in the
  /// serialized modules.
  ///
  /// \param M the SILModule on which to operate
  /// \param LinkAll when true, always link. For testing purposes.
  void performSILLinking(SILModule *M, bool LinkAll = false);

  /// \brief Cleanup instructions/builtin calls not suitable for IRGen.
  void performSILCleanup(SILModule *M);

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
  SILTransform *createPerfInliner(unsigned threshold);
  SILTransform *createGenericSpecializer();
  SILTransform *createSILARCOpts();
  SILTransform *createSimplifyCFG();
  SILTransform *createDevirtualization();
  SILTransform *createAllocBoxToStack();
  SILTransform *createSILAllocRefElimination();

  // Utilities
  SILTransform *createStripDebug();
  SILTransform *createSILInstCount();
  SILTransform *createSILAAEvaluator();
} // end namespace swift

#endif
