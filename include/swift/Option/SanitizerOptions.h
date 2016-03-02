//===--- SanitizerOptions.h - Helpers related to sanitizers -----*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_OPTIONS_SANITIZER_OPTIONS_H
#define SWIFT_OPTIONS_SANITIZER_OPTIONS_H

#include "swift/Basic/Sanitizers.h"
#include "llvm/ADT/Triple.h"
#include "llvm/Option/Arg.h"

namespace swift {
class DiagnosticEngine;

/// \brief Parses a -sanitize= argument's values.
///
/// \param Diag If non null, the argument is used to diagnose invalid values.
/// \return Returns a SanitizerKind.
SanitizerKind parseSanitizerArgValues(const llvm::opt::Arg *A,
                                      const llvm::Triple &Triple,
                                      DiagnosticEngine &Diag);
}
#endif // SWIFT_OPTIONS_SANITIZER_OPTIONS_H
