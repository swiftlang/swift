//===--- SanitizerOptions.cpp - Swift Sanitizer options -------------------===//
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
  case SanitizerKind::Optional:
    return "optional";
  case SanitizerKind::None:
    llvm_unreachable("Getting a name for SanitizerKind::None");
  }
  llvm_unreachable("Unsupported sanitizer");
}

llvm::SanitizerCoverageOptions swift::parseSanitizerCoverageArgValue(
    const llvm::opt::Arg *A, const llvm::Triple &Triple,
    DiagnosticEngine &Diags, SanitizerKind sanitizer) {

  llvm::SanitizerCoverageOptions opts;
  // The coverage names here follow the names used by clang's
  // ``-fsanitize-coverage=`` flag.
  for (int i = 0, n = A->getNumValues(); i != n; ++i) {
    if (opts.CoverageType == llvm::SanitizerCoverageOptions::SCK_None) {
      opts.CoverageType =
          llvm::StringSwitch<llvm::SanitizerCoverageOptions::Type>(
              A->getValue(i))
              .Case("func", llvm::SanitizerCoverageOptions::SCK_Function)
              .Case("bb", llvm::SanitizerCoverageOptions::SCK_BB)
              .Case("edge", llvm::SanitizerCoverageOptions::SCK_Edge)
              .Default(llvm::SanitizerCoverageOptions::SCK_None);
      if (opts.CoverageType != llvm::SanitizerCoverageOptions::SCK_None)
        continue;
    }

    if (StringRef(A->getValue(i)) == "indirect-calls") {
      opts.IndirectCalls = true;
      continue;
    } else if (StringRef(A->getValue(i)) == "trace-bb") {
      opts.TraceBB = true;
      continue;
    } else if (StringRef(A->getValue(i)) == "trace-cmp") {
      opts.TraceCmp = true;
      continue;
    } else if (StringRef(A->getValue(i)) == "8bit-counters") {
      opts.Use8bitCounters = true;
      continue;
    }

    // Argument is not supported.
    Diags.diagnose(SourceLoc(), diag::error_unsupported_option_argument,
                   A->getOption().getPrefixedName(), A->getValue(i));
    return llvm::SanitizerCoverageOptions();
  }

  if (opts.CoverageType == llvm::SanitizerCoverageOptions::SCK_None) {
    Diags.diagnose(SourceLoc(), diag::error_option_missing_required_argument,
                   A->getSpelling(), "\"func\", \"bb\", \"edge\"");
    return llvm::SanitizerCoverageOptions();
  }

  // Running the sanitizer coverage pass will add undefined symbols to
  // functions in compiler-rt's "sanitizer_common". "sanitizer_common" isn't
  // shipped as a separate library we can link with. However those are defined
  // in the various sanitizer runtime libraries so we require that we are
  // doing a sanitized build so we pick up the required functions during
  // linking.
  if (opts.CoverageType != llvm::SanitizerCoverageOptions::SCK_None &&
      sanitizer == SanitizerKind::None) {
    Diags.diagnose(SourceLoc(), diag::error_option_requires_sanitizer,
                   A->getSpelling());
    return llvm::SanitizerCoverageOptions();
  }
  return opts;
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
      .Case("optional", SanitizerKind::Optional)
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
