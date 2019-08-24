//===--- DiagnosticOptions.h ------------------------------------*- C++ -*-===//
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

#ifndef SWIFT_BASIC_DIAGNOSTICOPTIONS_H
#define SWIFT_BASIC_DIAGNOSTICOPTIONS_H

#include "llvm/ADT/Hashing.h"

namespace swift {

/// Options for controlling diagnostics.
class DiagnosticOptions {
public:
  /// Indicates whether textual diagnostics should use color.
  bool UseColor = false;

  /// Indicates whether the diagnostics produced during compilation should be
  /// checked against expected diagnostics, indicated by markers in the
  /// input source file.
  enum {
    NoVerify,
    Verify,
    VerifyAndApplyFixes
  } VerifyMode = NoVerify;

  /// Indicates whether to allow diagnostics for \c <unknown> locations if
  /// \c VerifyMode is not \c NoVerify.
  bool VerifyIgnoreUnknown = false;

  /// Indicates whether diagnostic passes should be skipped.
  bool SkipDiagnosticPasses = false;

  /// Keep emitting subsequent diagnostics after a fatal error.
  bool ShowDiagnosticsAfterFatalError = false;

  /// When emitting fixits as code edits, apply all fixits from diagnostics
  /// without any filtering.
  bool FixitCodeForAllDiagnostics = false;

  /// Suppress all warnings
  bool SuppressWarnings = false;

  /// Treat all warnings as errors
  bool WarningsAsErrors = false;

  // When printing diagnostics, include the diagnostic name at the end
  bool PrintDiagnosticNames = false;

  /// Return a hash code of any components from these options that should
  /// contribute to a Swift Bridging PCH hash.
  llvm::hash_code getPCHHashComponents() const {
    // Nothing here that contributes anything significant when emitting the PCH.
    return llvm::hash_value(0);
  }
};

} // end namespace swift

#endif // SWIFT_BASIC_DIAGNOSTICOPTIONS_H
