//===--- SARIFDiagnosticConsumer.h - SARIF Diagnostics ----------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// A DiagnosticConsumer that writes diagnostics to a file in the Static Analysis
// Results Interchange Format (SARIF).
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_FRONTEND_SARIFDIAGNOSTICCONSUMER_H
#define SWIFT_FRONTEND_SARIFDIAGNOSTICCONSUMER_H

#include "swift/Basic/LLVM.h"
#include <memory>

namespace swift {

class DiagnosticConsumer;

namespace sarif_diagnostics {

/// Create a DiagnosticConsumer that serializes diagnostics to a file in SARIF
/// format.
///
/// \param outputPath the file path to write the diagnostics to.
///
/// \returns A new diagnostic consumer that serializes diagnostics.
std::unique_ptr<DiagnosticConsumer>
createConsumer(StringRef outputPath, bool emitMacroExpansionFiles);

} // namespace sarif_diagnostics
} // namespace swift

#endif // SWIFT_FRONTEND_SARIFDIAGNOSTICCONSUMER_H
