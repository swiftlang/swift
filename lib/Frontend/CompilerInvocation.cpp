//===-- CompilerInvocation.cpp - CompilerInvocation methods ---------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/Frontend/Frontend.h"

#include "swift/Driver/Options.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/Triple.h"
#include "llvm/Option/Arg.h"
#include "llvm/Option/ArgList.h"
#include "llvm/Option/Option.h"
#include "llvm/Support/Path.h"

using namespace swift;
using namespace swift::driver;
using namespace llvm::opt;

swift::CompilerInvocation::CompilerInvocation() {
  TargetOpts.Triple = llvm::sys::getDefaultTargetTriple();
}

void CompilerInvocation::setMainExecutablePath(StringRef Path) {
  llvm::SmallString<128> LibPath(Path);
  llvm::sys::path::remove_filename(LibPath); // Remove /swift
  llvm::sys::path::remove_filename(LibPath); // Remove /bin
  llvm::sys::path::append(LibPath, "lib", "swift");
  setRuntimeIncludePath(LibPath.str());
}

static bool ParseFrontendArgs(FrontendOptions &Opts, ArgList &Args,
                              DiagnosticEngine &Diags) {
  using namespace options;

  if (const Arg *A = Args.getLastArg(OPT_o)) {
    Opts.OutputFilename = A->getValue();
  }

  if (const Arg *A = Args.getLastArg(OPT_module_name)) {
    Opts.ModuleName = A->getValue();
  }

  if (const Arg *A = Args.getLastArg(OPT_serialize_diagnostics)) {
    Opts.SerializedDiagnosticsPath = A->getValue();
  }

  if (Args.hasArg(OPT_emit_verbose_sil)) {
    Opts.EmitVerboseSIL = true;
  }

  if (Args.hasArg(OPT_delayed_function_body_parsing)) {
    Opts.DelayedFunctionBodyParsing = true;
  }

  if (Args.hasArg(OPT_print_stats)) {
    Opts.PrintStats = true;
  }

  if (const Arg *A = Args.getLastArg(OPT_help, OPT__help_hidden)) {
    if (A->getOption().matches(OPT_help)) {
      Opts.PrintHelp = true;
    } else if (A->getOption().matches(OPT__help_hidden)) {
      Opts.PrintHelpHidden = true;
    } else {
      llvm_unreachable("Unknown help option parsed");
    }
  }

  for (const Arg *A : make_range(Args.filtered_begin(OPT_INPUT),
                                 Args.filtered_end())) {
    Opts.InputFilenames.push_back(A->getValue());
  }

  if (const Arg *A = Args.getLastArg(OPT_module_source_list)) {
    Opts.ModuleSourceListPath = A->getValue();
  }

  return false;
}

static bool ParseLangArgs(LangOptions &Opts, ArgList &Args,
                          DiagnosticEngine &Diags) {
  using namespace options;

  if (Args.hasArg(OPT_use_malloc)) {
    Opts.UseMalloc = true;
  }

  if (Args.hasArg(OPT_debug_constraints)) {
    Opts.DebugConstraintSolver = true;
  }

  if (const Arg *A = Args.getLastArg(OPT_debug_constraints_attempt)) {
    unsigned attempt;
    if (StringRef(A->getValue()).getAsInteger(10, attempt)) {
      Diags.diagnose(SourceLoc(), diag::error_invalid_arg_value,
                     A->getAsString(Args), A->getValue());
      return true;
    }

    Opts.DebugConstraintSolverAttempt = attempt;
  }

  if (Args.hasArg(OPT_emit_sil_protocol_witness_tables)) {
    Opts.EmitSILProtocolWitnessTables = true;
  }

  return false;
}

static bool ParseClangImporterArgs(ClangImporterOptions &Opts, ArgList &Args,
                                   DiagnosticEngine &Diags) {
  using namespace options;

  if (const Arg *A = Args.getLastArg(OPT_module_cache_path)) {
    Opts.ModuleCachePath = A->getValue();
  }

  for (const Arg *A : make_range(Args.filtered_begin(OPT_Xcc),
                                 Args.filtered_end())) {
    Opts.ExtraArgs.push_back(A->getValue());
  }

  return false;
}

static bool ParseSearchPathArgs(SearchPathOptions &Opts, ArgList &Args,
                                DiagnosticEngine &Diags) {
  using namespace options;

  for (const Arg *A : make_range(Args.filtered_begin(OPT_I),
                                 Args.filtered_end())) {
    Opts.ImportSearchPaths.push_back(A->getValue());
  }

  for (const Arg *A : make_range(Args.filtered_begin(OPT_F),
                                 Args.filtered_end())) {
    Opts.FrameworkSearchPaths.push_back(A->getValue());
  }

  if (const Arg *A = Args.getLastArg(OPT_sdk)) {
    Opts.SDKPath = A->getValue();
  }

  // Opts.RuntimeIncludePath is set by calls to
  // CompilerInvocation::setRuntimeIncludePath().
  // TODO: improve the handling of RuntimeIncludePath.

  return false;
}

static bool ParseTargetArgs(TargetOptions &Opts, ArgList &Args,
                            DiagnosticEngine &Diags) {
  using namespace options;

  if (const Arg *A = Args.getLastArg(OPT_target)) {
    Opts.Triple = llvm::Triple::normalize(A->getValue());
  }

  return false;
}

static bool ParseDiagnosticArgs(DiagnosticOptions &Opts, ArgList &Args,
                                DiagnosticEngine &Diags) {
  using namespace options;

  if (Args.hasArg(OPT_verify)) {
    Opts.VerifyDiagnostics = true;
  }

  if (Args.hasArg(OPT_disable_diagnostic_passes)) {
    Opts.SkipDiagnosticPasses = true;
  }

  return false;
}

bool CompilerInvocation::parseArgs(ArrayRef<const char *> Args,
                                   DiagnosticEngine &Diags) {
  using namespace driver::options;

  if (Args.empty())
    return false;

  // Parse frontend command line options using Swift's option table.
  std::unique_ptr<llvm::opt::InputArgList> ParsedArgs;
  std::unique_ptr<llvm::opt::OptTable> Table = createDriverOptTable();
  unsigned MissingIndex;
  unsigned MissingCount;
  ParsedArgs.reset(
      Table->ParseArgs(Args.begin(), Args.end(), MissingIndex, MissingCount,
                       driver::options::FrontendOption));
  if (MissingCount) {
    Diags.diagnose(SourceLoc(), diag::error_missing_arg_value,
                   ParsedArgs->getArgString(MissingIndex), MissingCount);
    return true;
  }

  if (ParsedArgs->hasArg(OPT_UNKNOWN)) {
    for (const Arg *A : make_range(ParsedArgs->filtered_begin(OPT_UNKNOWN),
                                   ParsedArgs->filtered_end())) {
      Diags.diagnose(SourceLoc(), diag::error_unknown_arg,
                     A->getAsString(*ParsedArgs));
    }
    return true;
  }

  if (ParseFrontendArgs(FrontendOpts, *ParsedArgs, Diags)) {
    return true;
  }

  if (ParseLangArgs(LangOpts, *ParsedArgs, Diags)) {
    return true;
  }

  if (ParseClangImporterArgs(ClangImporterOpts, *ParsedArgs, Diags)) {
    return true;
  }

  if (ParseSearchPathArgs(SearchPathOpts, *ParsedArgs, Diags)) {
    return true;
  }

  if (ParseTargetArgs(TargetOpts, *ParsedArgs, Diags)) {
    return true;
  }

  if (ParseDiagnosticArgs(DiagnosticOpts, *ParsedArgs, Diags)) {
    return true;
  }

  for (auto InputArg : *ParsedArgs) {
    switch (InputArg->getOption().getID()) {
    case OPT_parse_as_library:
      setInputKind(SourceFileKind::Library);
      break;

    case OPT_parse_stdlib:
      setParseStdlib();
      break;

    case OPT_l:
      addLinkLibrary(InputArg->getValue(), LibraryKind::Library);
      break;

    case OPT_framework:
      addLinkLibrary(InputArg->getValue(), LibraryKind::Framework);
      break;
    }
  }

  return false;
}
