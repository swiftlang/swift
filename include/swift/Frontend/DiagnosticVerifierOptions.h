//===-- Frontend/DiagnosticVerifierOptions.h -=====--------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_FRONTEND_DIAGNOSTICVERIFIEROPTIONS_H
#define SWIFT_FRONTEND_DIAGNOSTICVERIFIEROPTIONS_H

#include <string>
#include <vector>

namespace swift {

/// Options for verifying diagnostics produced during compilation against
/// diagnostic expectations expressed using special markers in the input
/// source file.
struct DiagnosticVerifierOptions {
  /// Ignore diagnostics with an invalid source location.
  bool IgnoreUnknown = false;

  /// If there were any verification errors, such as differences between
  /// expected and actual diagnostics, fix the expectations in place.
  bool ApplyFixes = false;

  /// Additional paths to non-source files which will have diagnostics emitted
  /// in them, and which should be scanned for expectations by the diagnostic
  /// verifier.
  std::vector<std::string> AdditionalFilePaths;

  /// A list of prefixes that are appended to expected- that the diagnostic
  /// verifier should check for diagnostics.
  ///
  /// For example, if one placed the phrase "NAME", the verifier will check for:
  /// expected-$NAME{error,note,warning,remark} as well as the normal expected-
  /// prefixes.
  std::vector<std::string> AdditionalPrefixes;
};

} // end namespace swift

#endif /* SWIFT_FRONTEND_DIAGNOSTICVERIFIEROPTIONS_H */
