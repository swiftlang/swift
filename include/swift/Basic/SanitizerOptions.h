//===----- SanitizerOptions.h - Helpers related to sanitizers --*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===---------------------------------------------------------------------===//

#ifndef SWIFT_BASIC_SANITIZER_OPTIONS_H
#define SWIFT_BASIC_SANITIZER_OPTIONS_H

#include "llvm/Option/Arg.h"

namespace swift {
class DiagnosticEngine;

enum class SanitizerKind : unsigned {
  None = 0,
  Address,
};

/// \brief Parses a -sanitize= argument's values.
///
/// \param Diag If non null, the argument is used to diagnose invalid values.
/// \return Returns a SanitizerKind.
SanitizerKind parseSanitizerArgValues(const llvm::opt::Arg *A,
                                      DiagnosticEngine *Diag = nullptr);
}
#endif // SWIFT_BASIC_SANITIZER_OPTIONS_H