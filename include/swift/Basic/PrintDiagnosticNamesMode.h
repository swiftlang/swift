//===--- PrintDiagnosticNamesMode.h -----------------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_BASIC_PRINTDIAGNOSTICNAMESMODE_H
#define SWIFT_BASIC_PRINTDIAGNOSTICNAMESMODE_H

namespace swift {

/// What diagnostic name will be printed alongside the diagnostic message.
enum class PrintDiagnosticNamesMode {
  /// No diagnostic name will be printed.
  None,

  /// The identifier of a diagnostic (DiagID) will be used. Corresponds to the
  /// `-debug-diagnostic-names` option in the frontend.
  Identifier,

  /// The associated group name (DiagGroupID) will be used. Corresponds to the
  /// `-print-diagnostic-groups` option in the frontend.
  Group
};

} // end namespace swift

#endif // SWIFT_BASIC_PRINTDIAGNOSTICNAMESMODE_H
