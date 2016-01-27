//===--- DiagnosticVerifier.h - Diagnostic Verifier (-verify) ---*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file exposes support for the diagnostic verifier, which is used to
// implement -verify mode in the compiler.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_FRONTEND_DIAGNOSTIC_VERIFIER_H
#define SWIFT_FRONTEND_DIAGNOSTIC_VERIFIER_H

#include "swift/Basic/LLVM.h"

namespace swift {
  class SourceManager;

  /// enableDiagnosticVerifier - Set up the specified source manager so that
  /// diagnostics are captured instead of being printed.
  void enableDiagnosticVerifier(SourceManager &SM);

  /// verifyDiagnostics - Verify that captured diagnostics meet with the
  /// expectations of the source files corresponding to the specified BufferIDs
  /// and tear down our support for capturing and verifying diagnostics.
  ///
  /// This returns true if there are any mismatches found.
  ///
  bool verifyDiagnostics(SourceManager &SM, ArrayRef<unsigned> BufferIDs);
}

#endif
