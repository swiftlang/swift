//===--- SARIFDiagnosticConsumer.h - Export Diagnostics as SARIF -*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  This file defines the SARIFDiagnosticConsumer class, which exports
//  diagnostics to SARIF v2.1.0 JSON format.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SARIFDIAGNOSTICCONSUMER_H
#define SWIFT_SARIFDIAGNOSTICCONSUMER_H

#if SWIFT_BUILD_SWIFT_SYNTAX

#include <memory>

namespace llvm {
class StringRef;
}

namespace swift {

class DiagnosticConsumer;
class CompilerInvocation;

namespace sarif_diagnostics {
/// Create a DiagnosticConsumer that exports diagnostics to SARIF format.
///
/// \param outputPath the file path to write the SARIF JSON to.
/// \param invocation the compiler invocation containing parsed configuration.
///
/// \returns A new diagnostic consumer that exports diagnostics to SARIF.
std::unique_ptr<DiagnosticConsumer>
createConsumer(llvm::StringRef outputPath,
               const CompilerInvocation &invocation);
} // namespace sarif_diagnostics
} // namespace swift

#endif // SWIFT_BUILD_SWIFT_SYNTAX

#endif
