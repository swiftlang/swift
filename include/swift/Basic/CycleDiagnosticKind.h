//===--- CycleDiagnosticKind.h - Cycle Diagnostic Kind ----------*- C++ -*-===//
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
// This file defines the CycleDiagnosticKind enum.
//
//===----------------------------------------------------------------------===//

#ifndef CycleDiagnosticKind_h
#define CycleDiagnosticKind_h

namespace swift {

/// How to diagnose cycles when they are encountered during evaluation.
enum class CycleDiagnosticKind {
  /// Don't diagnose cycles at all.
  NoDiagnose,
  /// Diagnose cycles as full-fledged errors.
  FullDiagnose,
  /// Diagnose cycles via debugging dumps.
  DebugDiagnose,
};

} // end namespace swift

#endif /* CycleDiagnosticKind_h */
