//===--- InstrumenterSupport.cpp - Instrumenter Support -------------------===//
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
//  This file implements the supporting functions for writing instrumenters of
//  the Swift AST.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_DIAGNOSTIC_SUPPRESSION_H
#define SWIFT_AST_DIAGNOSTIC_SUPPRESSION_H

#include <vector>

namespace swift {

class DiagnosticConsumer;
class DiagnosticEngine;

/// RAII class that suppresses diagnostics by temporarily disabling all of
/// the diagnostic consumers.
class DiagnosticSuppression {
  DiagnosticEngine &diags;
  std::vector<DiagnosticConsumer *> consumers;

  DiagnosticSuppression(const DiagnosticSuppression &) = delete;
  DiagnosticSuppression &operator=(const DiagnosticSuppression &) = delete;

public:
  explicit DiagnosticSuppression(DiagnosticEngine &diags);
  ~DiagnosticSuppression();
  static bool isEnabled(const DiagnosticEngine &diags);
};

}
#endif // SWIFT_AST_DIAGNOSTIC_SUPPRESSION_H
