//===--- DiagnosticHelper.h - Diagnostic Helper -----------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file exposes helper class to emit diagnostics from swift-frontend.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_FRONTEND_DIAGNOSTIC_HELPER_H
#define SWIFT_FRONTEND_DIAGNOSTIC_HELPER_H

#include "swift/Basic/LLVM.h"
#include "llvm/Support/raw_ostream.h"

namespace swift {
class CompilerInstance;
class CompilerInvocation;

class DiagnosticHelper {
private:
  class Implementation;
  Implementation &Impl;

public:
  /// Create a DiagnosticHelper class to emit diagnostics from frontend actions.
  /// OS is the stream to print diagnostics.
  static DiagnosticHelper create(CompilerInstance &instance,
                                 const CompilerInvocation &invocation,
                                 llvm::raw_pwrite_stream &OS = llvm::errs());

  /// Set up the diagnostic consumers that depend on supplementary output paths
  /// (serialized diagnostics, JSON fix-its). Must be called once after the
  /// CompilerInvocation has been parsed.
  void initDiagnosticConsumers();

  /// Set if printing output should be suppressed.
  void setSuppressOutput(bool suppressOutput);

  /// Helper function to emit fatal error.
  void diagnoseFatalError(const char *reason, bool shouldCrash);

  DiagnosticHelper(const DiagnosticHelper &) = delete;
  DiagnosticHelper(DiagnosticHelper &&) = delete;
  DiagnosticHelper &operator=(const DiagnosticHelper &) = delete;
  DiagnosticHelper &operator=(DiagnosticHelper &&) = delete;
  ~DiagnosticHelper();

private:
  DiagnosticHelper(CompilerInstance &instance,
                   const CompilerInvocation &invocation,
                   llvm::raw_pwrite_stream &OS);
};

} // namespace swift

#endif
