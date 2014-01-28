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

#include "swift/AST/DiagnosticsFrontend.h"
#include "swift/Subsystems.h"
#include "swift/Driver/Options.h"
#include "swift/Driver/Util.h"
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
  IRGenOpts.Triple = llvm::sys::getDefaultTargetTriple();
}

void CompilerInvocation::setMainExecutablePath(StringRef Path) {
  llvm::SmallString<128> LibPath(Path);
  llvm::sys::path::remove_filename(LibPath); // Remove /swift
  llvm::sys::path::remove_filename(LibPath); // Remove /bin
  llvm::sys::path::append(LibPath, "lib", "swift");
  setRuntimeIncludePath(LibPath.str());
}

void CompilerInvocation::setRuntimeIncludePath(StringRef Path) {
  llvm::SmallString<128> LibPath(Path);
  SearchPathOpts.RuntimeIncludePath = LibPath.str();
  updateRuntimeImportPath();
}

void CompilerInvocation::updateRuntimeImportPath() {
  llvm::SmallString<128> LibPath(SearchPathOpts.RuntimeIncludePath);

  llvm::Triple Triple(IRGenOpts.Triple);
  llvm::sys::path::append(LibPath, getPlatformNameForTriple(Triple));

  SearchPathOpts.RuntimeImportPath = LibPath.str();
}

void CompilerInvocation::setTargetTriple(StringRef Triple) {
  IRGenOpts.Triple = Triple.str();
  updateRuntimeImportPath();
}

static bool ParseFrontendArgs(FrontendOptions &Opts, ArgList &Args,
                              DiagnosticEngine &Diags) {
  using namespace options;

  if (Args.hasArg(OPT_emit_verbose_sil)) {
    Opts.EmitVerboseSIL = true;
  }

  if (Args.hasArg(OPT_delayed_function_body_parsing)) {
    Opts.DelayedFunctionBodyParsing = true;
  }

  if (Args.hasArg(OPT_print_stats)) {
    Opts.PrintStats = true;
  }

  if (const Arg *A = Args.getLastArg(OPT_help, OPT_help_hidden)) {
    if (A->getOption().matches(OPT_help)) {
      Opts.PrintHelp = true;
    } else if (A->getOption().matches(OPT_help_hidden)) {
      Opts.PrintHelpHidden = true;
    } else {
      llvm_unreachable("Unknown help option parsed");
    }
  }

  for (const Arg *A : make_range(Args.filtered_begin(OPT_INPUT,
                                                     OPT_primary_file),
                                 Args.filtered_end())) {
    if (A->getOption().matches(OPT_INPUT)) {
      Opts.InputFilenames.push_back(A->getValue());
    } else if (A->getOption().matches(OPT_primary_file)) {
      Opts.PrimaryInput = SelectedInput(Opts.InputFilenames.size());
      Opts.InputFilenames.push_back(A->getValue());
    } else {
      llvm_unreachable("Unknown input-related argument!");
    }
  }

  if (Args.hasArg(OPT_parse_stdlib)) {
    Opts.ParseStdlib = true;
  }

  if (const Arg *A = Args.getLastArg(OPT__DASH_DASH)) {
    for (unsigned i = 0, e = A->getNumValues(); i != e; ++i) {
      Opts.ImmediateArgv.push_back(A->getValue(i));
    }
  }

  // Determine what the user has asked the frontend to do.
  FrontendOptions::ActionType Action;
  if (const Arg *A = Args.getLastArg(OPT_modes_Group)) {
    Option Opt = A->getOption();
    if (Opt.matches(OPT_c)) {
      Action = FrontendOptions::EmitObject;
    } else if (Opt.matches(OPT_S)) {
      Action = FrontendOptions::EmitAssembly;
    } else if (Opt.matches(OPT_emit_ir)) {
      Action = FrontendOptions::EmitIR;
    } else if (Opt.matches(OPT_emit_bc)) {
      Action = FrontendOptions::EmitBC;
    } else if (Opt.matches(OPT_emit_sil)) {
      Action = FrontendOptions::EmitSIL;
    } else if (Opt.matches(OPT_emit_silgen)) {
      Action = FrontendOptions::EmitSILGen;
    } else if (Opt.matches(OPT_parse)) {
      Action = FrontendOptions::Parse;
    } else if (Opt.matches(OPT_dump_parse)) {
      Action = FrontendOptions::DumpParse;

      // -dump-parse explicitly disables type-checking
      Opts.ParseOnly = true;
    } else if (Opt.matches(OPT_dump_ast)) {
      Action = FrontendOptions::DumpAST;
    } else if (Opt.matches(OPT_print_ast)) {
      Action = FrontendOptions::PrintAST;
    } else if (Opt.matches(OPT_repl)) {
      Action = FrontendOptions::REPL;
    } else if (Opt.matches(OPT_i)) {
      Action = FrontendOptions::Immediate;
    } else {
      llvm_unreachable("Unhandled mode option");
    }
  } else {
    // We don't have a mode, so determine a default.
    // TODO: add check for EmitModuleOnly, once we support -emit-module.
    if (Opts.InputFilenames.empty()) {
      // We don't have any input files, so default to the REPL.
      Action = FrontendOptions::REPL;
    } else if (Args.hasArg(OPT_emit_module, OPT_emit_module_path)) {
      // We've been told to emit a module, but have no other mode indicators.
      // As a result, put the frontend into EmitModuleOnly mode.
      // (Setting up module output will be handled below.)
      Action = FrontendOptions::EmitModuleOnly;
    } else {
      // In the absence of any other mode indicators, emit an object file.
      Action = FrontendOptions::EmitObject;
    }
  }
  Opts.RequestedAction = Action;

  if (Opts.RequestedAction == FrontendOptions::REPL &&
      !Opts.InputFilenames.empty()) {
    // TODO: emit diagnostic
    llvm::errs() << "error: REPL mode requires no input files\n";
    return true;
  } else if (Opts.RequestedAction == FrontendOptions::Immediate &&
             Opts.PrimaryInput.hasValue()) {
    // TODO: emit diagnostic
    llvm::errs() << "error: immediate mode is incompatible with -primary-file"
                 << '\n';
    return true;
  }

  bool TreatAsSIL = Args.hasArg(OPT_parse_sil);
  if (!TreatAsSIL && Opts.InputFilenames.size() == 1) {
    // If we have exactly one input filename, and its extension is "sil",
    // treat the input as SIL.
    StringRef Input(Opts.InputFilenames[0]);
    TreatAsSIL = llvm::sys::path::extension(Input).endswith(SIL_EXTENSION);
  }

  if (TreatAsSIL)
    Opts.InputKind = SourceFileKind::SIL;
  else if (Args.hasArg(OPT_parse_as_library))
    Opts.InputKind = SourceFileKind::Library;
  else if (Action == FrontendOptions::REPL)
    Opts.InputKind = SourceFileKind::REPL;
  else
    Opts.InputKind = SourceFileKind::Main;

  if (const Arg *A = Args.getLastArg(OPT_o)) {
    Opts.OutputFilename = A->getValue();
  }

  bool UserSpecifiedModuleName = false;
  {
    const Arg *A = Args.getLastArg(OPT_module_name);
    std::string ModuleName;
    if (A) {
      ModuleName = A->getValue();
      UserSpecifiedModuleName = true;
    } else {
      // The user did not specify a module name, so determine a default fallback
      // based on other options.

      // Note: this code path will only be taken when running the frontend
      // directly; the driver should always pass -module-name when invoking the
      // frontend.
      if (Opts.RequestedAction == FrontendOptions::REPL) {
        // Default to a module named "REPL" if we're in REPL mode.
        ModuleName = "REPL";
      } else {
        StringRef OutputFilename(Opts.OutputFilename);
        if (OutputFilename.empty() || OutputFilename == "-" ||
            llvm::sys::fs::is_directory(OutputFilename)) {
          ModuleName = Opts.InputFilenames[0];
        } else {
          ModuleName = OutputFilename;
        }

        ModuleName = llvm::sys::path::stem(ModuleName);
      }
    }

    if (!Lexer::isIdentifier(ModuleName)) {
      if (!Opts.actionHasOutput() ||
          (Opts.InputKind == SourceFileKind::Main &&
           Opts.InputFilenames.size() == 1)) {
        ModuleName = "main";
      } else {
        Diags.diagnose(SourceLoc(), diag::bad_module_name,
                       ModuleName, A == nullptr);
        ModuleName = "__bad__";
      }
    }

    Opts.ModuleName = ModuleName;
  }

  if (Opts.OutputFilename.empty() ||
      llvm::sys::fs::is_directory(Opts.OutputFilename)) {
    // No output filename was specified, or an output directory was specified.
    // Determine the correct output filename.

    // Note: this should typically only be used when invoking the frontend
    // directly, as the driver will always pass -o with an appropriate filename
    // if output is required for the requested action.

    StringRef Suffix;
    switch (Opts.RequestedAction) {
    case FrontendOptions::Parse:
    case FrontendOptions::DumpParse:
    case FrontendOptions::DumpAST:
    case FrontendOptions::PrintAST:
      // Textual modes.
      Opts.OutputFilename = "-";
      break;

    case FrontendOptions::EmitSILGen:
    case FrontendOptions::EmitSIL: {
      if (Opts.OutputFilename.empty())
        Opts.OutputFilename = "-";
      else
        Suffix = SIL_EXTENSION;
      break;
    }

    case FrontendOptions::EmitModuleOnly:
      Suffix = SERIALIZED_MODULE_EXTENSION;
      break;

    case FrontendOptions::Immediate:
    case FrontendOptions::REPL:
      // These modes have no frontend-generated output.
      Opts.OutputFilename = "";
      break;

    case FrontendOptions::EmitAssembly: {
      if (Opts.OutputFilename.empty())
        Opts.OutputFilename = "-";
      else
        Suffix = "s";
      break;
    }

    case FrontendOptions::EmitIR: {
      if (Opts.OutputFilename.empty())
        Opts.OutputFilename = "-";
      else
        Suffix = "ll";
      break;
    }

    case FrontendOptions::EmitBC: {
      if (Opts.OutputFilename.empty())
        Opts.OutputFilename = "-";
      else
        Suffix = "bc";
      break;
    }

    case FrontendOptions::EmitObject:
      Suffix = "o";
      break;
    }

    if (!Suffix.empty()) {
      // We need to deduce a file name.

      // First, if we're reading from stdin and we don't have a directory,
      // output to stdout.
      if (Opts.InputFilenames.size() == 1 && Opts.InputFilenames[0] == "-" &&
          Opts.OutputFilename.empty())
        Opts.OutputFilename = "-";
      else {
        // We have a suffix, so determine an appropriate name.
        llvm::SmallString<128> Path(Opts.OutputFilename);
        
        StringRef BaseName;
        if (Opts.PrimaryInput.hasValue() && Opts.PrimaryInput->isFilename()) {
          unsigned Index = Opts.PrimaryInput->Index;
          BaseName = llvm::sys::path::stem(Opts.InputFilenames[Index]);
        } else if (!UserSpecifiedModuleName &&
                   Opts.InputFilenames.size() == 1) {
          BaseName = llvm::sys::path::stem(Opts.InputFilenames[0]);
        } else {
          BaseName = Opts.ModuleName;
        }

        llvm::sys::path::append(Path, BaseName);
        llvm::sys::path::replace_extension(Path, Suffix);

        Opts.OutputFilename = Path.c_str();
      }
    }

    if (Opts.OutputFilename.empty()) {
      if (Opts.RequestedAction != FrontendOptions::REPL &&
          Opts.RequestedAction != FrontendOptions::Immediate) {
        // TODO: emit diagnostic
        llvm::errs() << "error: an output filename was not specified for a mode which requires an output filename\n";
        return true;
      }
    } else if (Opts.OutputFilename != "-" &&
        llvm::sys::fs::is_directory(Opts.OutputFilename)) {
      // TODO: emit diagnostic
      llvm::errs() << "error: the implicit output file '" << Opts.OutputFilename
                   << "' is a directory; explicitly specify a filename using -o"
                   << '\n';
      return true;
    }
  }

  auto determineOutputFilename = [&] (const char *Extension) -> std::string {
    StringRef OriginalPath;
    if (!Opts.OutputFilename.empty() && Opts.OutputFilename != "-")
      // Put the serialized diagnostics file next to the output file.
      OriginalPath = Opts.OutputFilename;
    else if (Opts.PrimaryInput.hasValue() && Opts.PrimaryInput->isFilename())
      // We have a primary input, so use that as the basis for the name of the
      // serialized diagnostics file.
      OriginalPath = llvm::sys::path::filename(
        Opts.InputFilenames[Opts.PrimaryInput->Index]);
    else
      // We don't have any better indication of name, so fall back on the
      // module name.
      OriginalPath = Opts.ModuleName;

    llvm::SmallString<128> Path(OriginalPath);
    llvm::sys::path::replace_extension(Path, Extension);
    return Path.str();
  };

  if (const Arg *A = Args.getLastArg(OPT_serialize_diagnostics_path)) {
    // Claim -serialize-diagnostics, if present.
    Args.ClaimAllArgs(OPT_serialize_diagnostics);
    Opts.SerializedDiagnosticsPath = A->getValue();
  } else if (Args.hasArg(OPT_serialize_diagnostics)) {
    // -serialize-diagnostics has been passed without
    // -serialize-diagnostics-path, so determine a path based on other inputs.
    static const char *const DiagnosticsFilePathExtension = "dia";
    Opts.SerializedDiagnosticsPath =
      determineOutputFilename(DiagnosticsFilePathExtension);
  }

  if (const Arg *A = Args.getLastArg(OPT_emit_module_path)) {
    // Claim -emit-module, if present.
    Args.ClaimAllArgs(OPT_emit_module);
    Opts.ModuleOutputPath = A->getValue();
  } else if (Args.hasArg(OPT_emit_module)) {
    // -emit-module has been passed without -emit-module-path, so determine
    // a path based on other inputs.
    if (Opts.RequestedAction == FrontendOptions::EmitModuleOnly &&
        !Opts.OutputFilename.empty())
      // We're in EmitModuleOnly mode, so use OutputFilename if we have one.
      Opts.ModuleOutputPath = Opts.OutputFilename;
    else
      // We're either not in EmitModuleOnly mode, or we don't have an
      // OutputFilename, so determine an output path based on other inputs.
      Opts.ModuleOutputPath =
        determineOutputFilename(SERIALIZED_MODULE_EXTENSION);
  }

  if (const Arg *A = Args.getLastArg(OPT_module_link_name)) {
    Opts.ModuleLinkName = A->getValue();
  }

  return false;
}

static bool ParseLangArgs(LangOptions &Opts, ArgList &Args,
                          DiagnosticEngine &Diags) {
  using namespace options;

  if (Args.hasArg(OPT_use_malloc)) {
    Opts.UseMalloc = true;
  }

  if (Args.hasArg(OPT_enable_experimental_patterns)) {
    Opts.EnableExperimentalPatterns = true;
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
  // setRuntimeIncludePath() or setMainExecutablePath().
  // Opts.RuntimeImportPath is set by calls to
  // setRuntimeIncludePath() or setMainExecutablePath() and 
  // updated by calls to setTargetTriple() or parseArgs().
  // Assumes exactly one of setMainExecutablePath() or setRuntimeIncludePath() 
  // is called before setTargetTriple() and parseArgs().
  // TODO: improve the handling of RuntimeIncludePath.

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

// Lifted from the clang driver.
static void PrintArg(raw_ostream &OS, const char *Arg, bool Quote) {
  const bool Escape = std::strpbrk(Arg, "\"\\$ ");

  if (!Quote && !Escape) {
    OS << Arg;
    return;
  }

  // Quote and escape. This isn't really complete, but good enough.
  OS << '"';
  while (const char c = *Arg++) {
    if (c == '"' || c == '\\' || c == '$')
      OS << '\\';
    OS << c;
  }
  OS << '"';
}

void CompilerInvocation::buildDWARFDebugFlags(std::string &Output,
                                              const ArrayRef<const char*> &Args,
                                              StringRef SDKPath) {
  llvm::raw_string_ostream OS(Output);
  interleave(Args,
             [&](const char *Argument) { PrintArg(OS, Argument, false); },
             [&] { OS << " "; });

  // Inject the SDK path if it is missing.
  for (auto A : Args) {
    if (StringRef(A).startswith("-sdk"))
      return;
  }
  OS << " -sdk ";
  PrintArg(OS, SDKPath.data(), false);
}

static bool ParseIRGenArgs(IRGenOptions &Opts, ArgList &Args,
                           DiagnosticEngine &Diags,
                           const FrontendOptions &FrontendOpts,
                           StringRef SDKPath) {
  using namespace options;

  if (Args.hasArg(OPT_g)) {
    Opts.DebugInfo = true;
    ArgStringList RenderedArgs;
    for (auto A : Args)
      A->render(Args, RenderedArgs);
    CompilerInvocation::buildDWARFDebugFlags(Opts.DWARFDebugFlags,
                                             RenderedArgs, SDKPath);
  }

  for (const Arg *A : make_range(Args.filtered_begin(OPT_l, OPT_framework),
                                 Args.filtered_end())) {
    LibraryKind Kind;
    if (A->getOption().matches(OPT_l)) {
      Kind = LibraryKind::Library;
    } else if (A->getOption().matches(OPT_framework)) {
      Kind = LibraryKind::Framework;
    } else {
      llvm_unreachable("Unknown LinkLibrary option kind");
    }

    Opts.LinkLibraries.push_back(LinkLibrary(A->getValue(), Kind));
  }

  if (const Arg *A = Args.getLastArg(OPT_O_Group)) {
    if (A->getOption().matches(OPT_O0)) {
      Opts.OptLevel = 0;
    }
    else {
      unsigned OptLevel;
      if (StringRef(A->getValue()).getAsInteger(10, OptLevel) || OptLevel > 3) {
        Diags.diagnose(SourceLoc(), diag::error_invalid_arg_value,
                       A->getAsString(Args), A->getValue());
        return true;
      }

      Opts.OptLevel = OptLevel;
    }
  } else {
    Opts.OptLevel = 0;
  }

  if (Args.hasArg(OPT_disable_all_runtime_checks)) {
    Opts.DisableAllRuntimeChecks = true;
  }

  if (Args.hasArg(OPT_disable_llvm_optzns)) {
    Opts.DisableLLVMOptzns = true;
  }

  if (Args.hasArg(OPT_disable_llvm_arc_opts)) {
    Opts.DisableLLVMARCOpts = true;
  }

  if (Args.hasArg(OPT_enable_dynamic_value_type_layout)) {
    Opts.EnableDynamicValueTypeLayout = true;
  }

  if (const Arg *A = Args.getLastArg(OPT_target)) {
    Opts.Triple = llvm::Triple::normalize(A->getValue());
  }

  // TODO: investigate whether these should be removed, in favor of definitions
  // in other classes.
  if (!FrontendOpts.InputFilenames.empty())
    Opts.MainInputFilename = FrontendOpts.InputFilenames[0];
  Opts.OutputFilename = FrontendOpts.OutputFilename;

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
  
  if (ParseIRGenArgs(IRGenOpts, *ParsedArgs, Diags, FrontendOpts,
                     getSDKPath())) {
    return true;
  }

  // Target options may have changed this path.
  updateRuntimeImportPath();

  if (ParseDiagnosticArgs(DiagnosticOpts, *ParsedArgs, Diags)) {
    return true;
  }

  return false;
}
