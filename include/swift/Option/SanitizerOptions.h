//===--- SanitizerOptions.h - Helpers related to sanitizers -----*- C++ -*-===//
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

#ifndef SWIFT_OPTIONS_SANITIZER_OPTIONS_H
#define SWIFT_OPTIONS_SANITIZER_OPTIONS_H

#include "swift/Basic/Sanitizers.h"
#include "swift/Basic/OptionSet.h"
#include "llvm/TargetParser/Triple.h"
#include "llvm/Option/Arg.h"
#include "llvm/Option/ArgList.h"
// FIXME: This include is just for llvm::SanitizerCoverageOptions. We should
// split the header upstream so we don't include so much.
#include "llvm/Transforms/Instrumentation.h"

namespace swift {
class DiagnosticEngine;

/// Parses a -sanitize= argument's values.
///
/// \param Diag If non null, the argument is used to diagnose invalid values.
/// \param sanitizerRuntimeLibExists Function which checks for existence of a
//         sanitizer dylib with a given name.
/// \return Returns a SanitizerKind.
OptionSet<SanitizerKind> parseSanitizerArgValues(
    const llvm::opt::ArgList &Args, const llvm::opt::Arg *A,
    const llvm::Triple &Triple, DiagnosticEngine &Diag,
    llvm::function_ref<bool(llvm::StringRef, bool)> sanitizerRuntimeLibExists);

OptionSet<SanitizerKind> parseSanitizerRecoverArgValues(
    const llvm::opt::Arg *A, const OptionSet<SanitizerKind> &enabledSanitizers,
    DiagnosticEngine &Diags, bool emitWarnings);

/// Parses a -sanitize-coverage= argument's value.
llvm::SanitizerCoverageOptions parseSanitizerCoverageArgValue(
        const llvm::opt::Arg *A,
        const llvm::Triple &Triple,
        DiagnosticEngine &Diag,
        OptionSet<SanitizerKind> sanitizers);

/// Parse -sanitize-address-use-odr-indicator's value.
bool parseSanitizerAddressUseODRIndicator(
    const llvm::opt::Arg *A, const OptionSet<SanitizerKind> &enabledSanitizers,
    DiagnosticEngine &Diags);

/// Parse -sanitize-stable-abi's value
bool parseSanitizerUseStableABI(
    const llvm::opt::Arg *A, const OptionSet<SanitizerKind> &enabledSanitizers,
    DiagnosticEngine &Diags);

/// Returns the active sanitizers as a comma-separated list.
std::string getSanitizerList(const OptionSet<SanitizerKind> &Set);
}
#endif // SWIFT_OPTIONS_SANITIZER_OPTIONS_H
