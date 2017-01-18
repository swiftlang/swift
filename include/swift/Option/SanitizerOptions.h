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
#include "llvm/ADT/Triple.h"
#include "llvm/Option/Arg.h"
// FIXME: This include is just for llvm::SanitizerCoverageOptions. We should
// split the header upstream so we don't include so much.
#include "llvm/Transforms/Instrumentation.h"

namespace swift {
class DiagnosticEngine;

/// \brief Parses a -sanitize= argument's values.
///
/// \param Diag If non null, the argument is used to diagnose invalid values.
/// \return Returns a SanitizerKind.
SanitizerKind parseSanitizerArgValues(const llvm::opt::Arg *A,
                                      const llvm::Triple &Triple,
                                      DiagnosticEngine &Diag);

/// \brief Parses a -sanitize-coverage= argument's value.
llvm::SanitizerCoverageOptions
parseSanitizerCoverageArgValue(const llvm::opt::Arg *A,
                               const llvm::Triple &Triple,
                               DiagnosticEngine &Diag, SanitizerKind sanitizer);
}
#endif // SWIFT_OPTIONS_SANITIZER_OPTIONS_H
