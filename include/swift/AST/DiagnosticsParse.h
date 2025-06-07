//===--- DiagnosticsParse.h - Diagnostic Definitions ------------*- C++ -*-===//
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
/// \file
/// This file defines diagnostics for lexing and parsing.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_DIAGNOSTICSPARSE_H
#define SWIFT_DIAGNOSTICSPARSE_H

#include "swift/AST/DiagnosticsCommon.h"

namespace swift {
  namespace diag {
  // Declare common diagnostics objects with their appropriate types.
#define DIAG(KIND, ID, Group, Options, Text, Signature)                      \
    extern detail::DiagWithArguments<void Signature>::type ID;
#define FIXIT(ID, Text, Signature)                                           \
    extern detail::StructuredFixItWithArguments<void Signature>::type ID;
#include "DiagnosticsParse.def"
  }
}

#endif
