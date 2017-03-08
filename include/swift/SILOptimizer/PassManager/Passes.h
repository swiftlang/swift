//===--- Passes.h - Swift Compiler SIL Pass Entrypoints ---------*- C++ -*-===//
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
//  This file declares the main entrypoints to SIL passes.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SILOPTIMIZER_PASSMANAGER_PASSES_H
#define SWIFT_SILOPTIMIZER_PASSMANAGER_PASSES_H

#include "swift/SIL/SILModule.h"

namespace swift {
  class SILOptions;
  class SILTransform;

  namespace irgen {
    class IRGenModule;
  }

  /// \brief Run all the SIL diagnostic passes on \p M.
  ///
  /// \returns true if the diagnostic passes produced an error
  bool runSILDiagnosticPasses(SILModule &M);

  /// \brief Run all the SIL performance optimization passes on \p M.
  void runSILOptimizationPasses(SILModule &M);

  /// \brief Run all SIL passes for -Onone on module \p M.
  void runSILPassesForOnone(SILModule &M);

  /// \brief Run the SIL ownership eliminator pass on \p M.
  bool runSILOwnershipEliminatorPass(SILModule &M);

  void runSILOptimizationPassesWithFileSpecification(SILModule &Module,
                                                     StringRef FileName);

  /// \brief Detect and remove unreachable code. Diagnose provably unreachable
  /// user code.
  void performSILDiagnoseUnreachable(SILModule *M);

  /// \brief Remove dead functions from \p M.
  void performSILDeadFunctionElimination(SILModule *M);

  /// \brief Link a SILFunction declaration to the actual definition in the
  /// serialized modules.
  ///
  /// \param M the SILModule on which to operate
  /// \param LinkAll when true, always link. For testing purposes.
  void performSILLinking(SILModule *M, bool LinkAll = false);

  /// \brief Convert SIL to a lowered form suitable for IRGen.
  void runSILLoweringPasses(SILModule &M);

  /// \brief Perform SIL Inst Count on M.
  void performSILInstCount(SILModule *M);

  /// \brief Identifiers for all passes. Used to procedurally create passes from
  /// lists of passes.
  enum class PassKind {
#define PASS(ID, NAME, DESCRIPTION) ID,
#define PASS_RANGE(ID, START, END) ID##_First = START, ID##_Last = END,
#include "Passes.def"
    invalidPassKind
  };

  PassKind PassKindFromString(StringRef ID);
  StringRef PassKindName(PassKind Kind);
  StringRef PassKindID(PassKind Kind);

#define PASS(ID, NAME, DESCRIPTION) SILTransform *create##ID();
#define IRGEN_PASS(ID, NAME, DESCRIPTION)
#include "Passes.def"

} // end namespace swift

#endif
