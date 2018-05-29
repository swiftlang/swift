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
#include "swift/Basic/OptionSet.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/DiagnosticsDriver.h"
#include "swift/AST/DiagnosticsFrontend.h"
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/ADT/Triple.h"

using namespace swift;

static StringRef toStringRef(const SanitizerKind kind) {
  switch (kind) {
  case SanitizerKind::Address:
    return "address";
  case SanitizerKind::Thread:
    return "thread";
  case SanitizerKind::Fuzzer:
    return "fuzzer";
  }
  llvm_unreachable("Unsupported sanitizer");
}

static const char* toFileName(const SanitizerKind kind) {
  switch (kind) {
  case SanitizerKind::Address:
    return "asan";
  case SanitizerKind::Thread:
    return "tsan";
  case SanitizerKind::Fuzzer:
    return "fuzzer";
  }
  llvm_unreachable("Unsupported sanitizer");
}

llvm::SanitizerCoverageOptions swift::parseSanitizerCoverageArgValue(
    const llvm::opt::Arg *A, const llvm::Triple &Triple,
    DiagnosticEngine &Diags, OptionSet<SanitizerKind> sanitizers) {

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
    } else if (StringRef(A->getValue(i)) == "trace-pc") {
      opts.TracePC = true;
      continue;
    } else if (StringRef(A->getValue(i)) == "trace-pc-guard") {
      opts.TracePCGuard = true;
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
      !sanitizers) {
    Diags.diagnose(SourceLoc(), diag::error_option_requires_sanitizer,
                   A->getSpelling());
    return llvm::SanitizerCoverageOptions();
  }
  return opts;
}

OptionSet<SanitizerKind> swift::parseSanitizerArgValues(
    const llvm::opt::ArgList &Args,
    const llvm::opt::Arg *A,
    const llvm::Triple &Triple,
    DiagnosticEngine &Diags,
    llvm::function_ref<bool(llvm::StringRef, bool)> sanitizerRuntimeLibExists) {
  OptionSet<SanitizerKind> sanitizerSet;

  // Find the sanitizer kind.
  for (int i = 0, n = A->getNumValues(); i != n; ++i) {
    auto kind = llvm::StringSwitch<Optional<SanitizerKind>>(A->getValue(i))
        .Case("address", SanitizerKind::Address)
        .Case("thread", SanitizerKind::Thread)
        .Case("fuzzer", SanitizerKind::Fuzzer)
        .Default(None);
    bool isShared = kind && *kind != SanitizerKind::Fuzzer;
    if (!kind) {
      Diags.diagnose(SourceLoc(), diag::error_unsupported_option_argument,
          A->getOption().getPrefixedName(), A->getValue(i));
    } else {
      // Support is determined by existance of the sanitizer library.
      bool sanitizerSupported =
          sanitizerRuntimeLibExists(toFileName(*kind), isShared);

      // TSan is explicitly not supported for 32 bits.
      if (*kind == SanitizerKind::Thread && !Triple.isArch64Bit())
        sanitizerSupported = false;

      if (!sanitizerSupported) {
        SmallString<128> b;
        Diags.diagnose(SourceLoc(), diag::error_unsupported_opt_for_target,
                       (A->getOption().getPrefixedName() + toStringRef(*kind))
                           .toStringRef(b),
                       Triple.getTriple());
      } else {
        sanitizerSet |= *kind;
      }
    }
  }

  // Check that we're one of the known supported targets for sanitizers.
  if (!(Triple.isOSDarwin() || Triple.isOSLinux() || Triple.isOSWindows())) {
    SmallString<128> b;
    Diags.diagnose(SourceLoc(), diag::error_unsupported_opt_for_target,
      (A->getOption().getPrefixedName() +
          StringRef(A->getAsString(Args))).toStringRef(b),
      Triple.getTriple());
  }

  // Address and thread sanitizers can not be enabled concurrently.
  if ((sanitizerSet & SanitizerKind::Thread)
        && (sanitizerSet & SanitizerKind::Address)) {
    SmallString<128> b1;
    SmallString<128> b2;
    Diags.diagnose(SourceLoc(), diag::error_argument_not_allowed_with,
        (A->getOption().getPrefixedName()
            + toStringRef(SanitizerKind::Address)).toStringRef(b1),
        (A->getOption().getPrefixedName()
            + toStringRef(SanitizerKind::Thread)).toStringRef(b2));
  }

  return sanitizerSet;
}
