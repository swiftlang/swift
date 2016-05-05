//===--- SanitizerOptions.cpp - Swift Sanitizer options -------------------===//
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

SanitizerKind swift::parseSanitizerArgValues(const llvm::opt::Arg *A,
                                      const llvm::Triple &Triple,
                                      DiagnosticEngine &Diags) {
  SanitizerKind kind = SanitizerKind::None;

  // Find the sanitizer kind.
  SanitizerKind pKind = SanitizerKind::None;
  for (int i = 0, n = A->getNumValues(); i != n; ++i) {
    kind =
    llvm::StringSwitch<SanitizerKind>(A->getValue(i))
      .Case("address", SanitizerKind::Address)
      .Case("thread", SanitizerKind::Thread)
      .Default(SanitizerKind::None);

    if (kind == SanitizerKind::None) {
      Diags.diagnose(SourceLoc(), diag::error_unsupported_option_argument,
        A->getOption().getPrefixedName(), A->getValue(i));
      return kind;
    }

    // Currently, more than one sanitizer cannot be enabled at the same time.
    if (pKind != SanitizerKind::None && pKind != kind) {
      SmallString<128> pb;
      SmallString<128> b;
      Diags.diagnose(SourceLoc(), diag::error_argument_not_allowed_with,
        (A->getOption().getPrefixedName() + toStringRef(pKind)).toStringRef(pb),
        (A->getOption().getPrefixedName() + toStringRef(kind)).toStringRef(b));
    }
    pKind = kind;
  }

  if (kind == SanitizerKind::None)
    return kind;

  // Check if the target is supported for this sanitizer.
  // None of the sanitizers work on Linux right now.
  if (!Triple.isOSDarwin()) {
    SmallString<128> b;
    Diags.diagnose(SourceLoc(), diag::error_unsupported_opt_for_target,
      (A->getOption().getPrefixedName() + toStringRef(kind)).toStringRef(b),
      Triple.getTriple());
  }
  // Thread Sanitizer only works on OS X and the simulators. It's only supported
  // on 64 bit architectures.
  if (kind == SanitizerKind::Thread &&
      (!(Triple.isMacOSX() || tripleIsAnySimulator(Triple)) ||
       !Triple.isArch64Bit())) {
    SmallString<128> b;
    Diags.diagnose(SourceLoc(), diag::error_unsupported_opt_for_target,
      (A->getOption().getPrefixedName() + toStringRef(kind)).toStringRef(b),
      Triple.getTriple());
  }

  return kind;
}
