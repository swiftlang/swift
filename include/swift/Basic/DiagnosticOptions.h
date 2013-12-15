//===--- DiagnosticOptions.h ------------------------------------*- C++ -*-===//
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

#ifndef SWIFT_BASIC_DIAGNOSTICOPTIONS_H
#define SWIFT_BASIC_DIAGNOSTICOPTIONS_H

namespace swift {

/// Options for controlling diagnostics.
class DiagnosticOptions {
public:
  /// Indicates whether the diagnostics produced during compilation should be
  /// checked against expectated diagnostics, indicated by markers in the
  /// input source file.
  bool VerifyDiagnostics;

  /// Indicates whether diagnostic passes should be skipped.
  bool SkipDiagnosticPasses;
};

}

#endif
