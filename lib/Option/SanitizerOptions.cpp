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
#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/DiagnosticsDriver.h"
#include "swift/AST/DiagnosticsFrontend.h"
#include "swift/Basic/OptionSet.h"
#include "swift/Basic/Platform.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/TargetParser/Triple.h"
#include <optional>

using namespace swift;

static StringRef toStringRef(const SanitizerKind kind) {
  switch (kind) {
    #define SANITIZER(_, kind, name, file) \
        case SanitizerKind::kind: return #name;
    #include "swift/Basic/Sanitizers.def"
  }
  llvm_unreachable("Unknown sanitizer");
}

static StringRef toFileName(const SanitizerKind kind) {
  switch (kind) {
    #define SANITIZER(_, kind, name, file) \
        case SanitizerKind::kind: return #file;
    #include "swift/Basic/Sanitizers.def"
  }
  llvm_unreachable("Unknown sanitizer");
}

static std::optional<SanitizerKind> parse(const char *arg) {
  return llvm::StringSwitch<std::optional<SanitizerKind>>(arg)
#define SANITIZER(_, kind, name, file) .Case(#name, SanitizerKind::kind)
#include "swift/Basic/Sanitizers.def"
      .Default(std::nullopt);
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
    } else if (StringRef(A->getValue(i)) == "inline-8bit-counters") {
      opts.Inline8bitCounters = true;
      continue;
    } else if (StringRef(A->getValue(i)) == "pc-table") {
      opts.PCTable = true;
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
  for (const char *arg : A->getValues()) {
    std::optional<SanitizerKind> optKind = parse(arg);

    // Unrecognized sanitizer option
    if (!optKind.has_value()) {
      Diags.diagnose(SourceLoc(), diag::error_unsupported_option_argument,
          A->getOption().getPrefixedName(), arg);
      continue;
    }
    SanitizerKind kind = optKind.value();

    // Support is determined by existence of the sanitizer library.
    auto fileName = toFileName(kind);
    bool isShared = (kind != SanitizerKind::Fuzzer);
    bool sanitizerSupported = sanitizerRuntimeLibExists(fileName, isShared);

    // TSan is explicitly not supported for 32 bits.
    if (kind == SanitizerKind::Thread && !Triple.isArch64Bit())
      sanitizerSupported = false;

    if (!sanitizerSupported) {
      SmallString<128> b;
      Diags.diagnose(SourceLoc(), diag::error_unsupported_opt_for_target,
                      (A->getOption().getPrefixedName() + toStringRef(kind))
                          .toStringRef(b),
                      Triple.getTriple());
    } else {
      sanitizerSet |= kind;
    }
  }

  // Check that we're one of the known supported targets for sanitizers.
  if (!(Triple.isOSDarwin() || Triple.isOSLinux() || Triple.isOSWindows() || Triple.isOSWASI() || Triple.isOSFreeBSD())) {
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

  // Scudo must be run standalone
  if (sanitizerSet.contains(SanitizerKind::Scudo) &&
      !sanitizerSet.containsOnly(SanitizerKind::Scudo)) {
    auto diagnoseSanitizerKind = [&Diags, A, &sanitizerSet](SanitizerKind kind) {
      // Don't diagnose Scudo, but diagnose anything else
      if (kind != SanitizerKind::Scudo && sanitizerSet.contains(kind)) {
        SmallString<128> b1;
        SmallString<128> b2;
        Diags.diagnose(SourceLoc(), diag::error_argument_not_allowed_with,
            (A->getOption().getPrefixedName()
             + toStringRef(SanitizerKind::Scudo)).toStringRef(b1),
            (A->getOption().getPrefixedName()
             + toStringRef(kind)).toStringRef(b2));
      }
    };

#define SANITIZER(enm, kind, name, file) \
    diagnoseSanitizerKind(SanitizerKind::kind);
#include "swift/Basic/Sanitizers.def"

  }
  return sanitizerSet;
}

OptionSet<SanitizerKind> swift::parseSanitizerRecoverArgValues(
    const llvm::opt::Arg *A, const OptionSet<SanitizerKind> &enabledSanitizers,
    DiagnosticEngine &Diags, bool emitWarnings) {
  OptionSet<SanitizerKind> sanitizerRecoverSet;

  // Find the sanitizer kind.
  for (const char *arg : A->getValues()) {
    std::optional<SanitizerKind> optKind = parse(arg);

    // Unrecognized sanitizer option
    if (!optKind.has_value()) {
      Diags.diagnose(SourceLoc(), diag::error_unsupported_option_argument,
                     A->getOption().getPrefixedName(), arg);
      continue;
    }
    SanitizerKind kind = optKind.value();

    // Only support ASan for now.
    if (kind != SanitizerKind::Address) {
      Diags.diagnose(SourceLoc(), diag::error_unsupported_option_argument,
                     A->getOption().getPrefixedName(), arg);
      continue;
    }

    // Check that the sanitizer is enabled.
    if (!(enabledSanitizers & kind)) {
      SmallString<128> b;
      if (emitWarnings) {
        Diags.diagnose(SourceLoc(),
                       diag::warning_option_requires_specific_sanitizer,
                       (A->getOption().getPrefixedName() + toStringRef(kind))
                           .toStringRef(b),
                       toStringRef(kind));
      }
      continue;
    }

    sanitizerRecoverSet |= kind;
  }

  return sanitizerRecoverSet;
}

// Note this implementation cannot be inlined at its use site because it calls
// `toStringRef(const SanitizerKind).`
bool swift::parseSanitizerAddressUseODRIndicator(
    const llvm::opt::Arg *A, const OptionSet<SanitizerKind> &enabledSanitizers,
    DiagnosticEngine &Diags) {
  // Warn if ASan isn't enabled.
  if (!(enabledSanitizers & SanitizerKind::Address)) {
    Diags.diagnose(
        SourceLoc(), diag::warning_option_requires_specific_sanitizer,
        A->getOption().getPrefixedName(), toStringRef(SanitizerKind::Address));
    return false;
  }

  return true;
}

bool swift::parseSanitizerUseStableABI(
    const llvm::opt::Arg *A, const OptionSet<SanitizerKind> &enabledSanitizers,
    DiagnosticEngine &Diags) {
  // Warn if ASan isn't enabled.
  if (!(enabledSanitizers & SanitizerKind::Address)) {
    Diags.diagnose(
        SourceLoc(), diag::warning_option_requires_specific_sanitizer,
        A->getOption().getPrefixedName(), toStringRef(SanitizerKind::Address));
    return false;
  }

  return true;
}

std::string swift::getSanitizerList(const OptionSet<SanitizerKind> &Set) {
  std::string list;
  #define SANITIZER(_, kind, name, file) \
      if (Set & SanitizerKind::kind) list += #name ",";
  #include "swift/Basic/Sanitizers.def"

  if (!list.empty())
    list.pop_back(); // Remove last comma

  return list;
}
