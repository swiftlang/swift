//===--- SanitizerOptions.cpp - Swift Sanitizer options ------------------===//
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
//
// \file
// This file implements the parsing of sanitizer arguments.
//
//===----------------------------------------------------------------------===//

#include "swift/Option/SanitizerOptions.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/DiagnosticsFrontend.h"
using namespace swift;

static StringRef toStringRef(const SanitizerKind kind) {
  switch (kind) {
  case SanitizerKind::Address:
    return "address";
  case SanitizerKind::None:
    llvm_unreachable("Getting a name for SanitizerKind::None");
  }
  llvm_unreachable("Unsupported sanitizer");
}

SanitizerKind swift::parseSanitizerArgValues(const llvm::opt::Arg *A,
                                      const llvm::Triple &Triple,
                                      DiagnosticEngine &Diags) {
  SanitizerKind kind = SanitizerKind::None;

  // Find the sanitizer kind.
  // TODO: Add support for dealing with multiple sanitizers.
  for (int i = 0, n = A->getNumValues(); i != n; ++i) {
    const char *Value = A->getValue(i);
    if (StringRef(Value).equals("address")) {
      kind = SanitizerKind::Address;
    } else {
      Diags.diagnose(SourceLoc(), diag::error_unsupported_option_argument,
        A->getOption().getName(), A->getValue(i));
      return kind;
    }
  }

  // Check if the target is supported for this sanitizer.
  if (!Triple.isOSDarwin() && kind != SanitizerKind::None) {
    SmallVector<char, 128> buffer;
    Diags.diagnose(SourceLoc(), diag::error_unsupported_opt_for_target,
      (A->getOption().getName() + toStringRef(kind)).toStringRef(buffer),
      Triple.getTriple());
  }

  return kind;
}
