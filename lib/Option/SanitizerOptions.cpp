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
#include "swift/Basic/Platform.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/DiagnosticsFrontend.h"
#include "llvm/ADT/StringSwitch.h"
using namespace swift;

static StringRef toStringRef(const SanitizerKind kind) {
  switch (kind) {
  case SanitizerKind::Address:
    return "address";
  case SanitizerKind::Thread:
    return "thread";
  case SanitizerKind::None:
    llvm_unreachable("Getting a name for SanitizerKind::None");
  }
  llvm_unreachable("Unsupported sanitizer");
}

void swift::parseSanitizerArgValues(const llvm::opt::Arg *A,
                                      DiagnosticEngine &Diags) {
  SanitizerKind kind = SanitizerKind::None;

  for (int i = 0, n = A->getNumValues(); i != n; ++i) {
    kind =
    llvm::StringSwitch<SanitizerKind>(A->getValue(i))
      .Case("address", SanitizerKind::Address)
      .Case("thread", SanitizerKind::Thread)
      .Default(SanitizerKind::None);

    if (kind == SanitizerKind::None) {
      Diags.diagnose(SourceLoc(), diag::error_unsupported_option_argument,
        A->getOption().getPrefixedName(), A->getValue(i));
      return;
    }

    // SWIFT 2.3 ONLY
    // Tell the user that Address Sanitizer and Thread Sanitizer are not
    // available on the Swift 2.3 toolchain.
    SmallString<128> b;
    Diags.diagnose(SourceLoc(), diag::error_unsupported_on_2_3_toolchain,
      (A->getOption().getPrefixedName() + toStringRef(kind)).toStringRef(b));
  }
}
