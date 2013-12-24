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

void CompilerInvocation::setRuntimeIncludePath(StringRef Path) {
  llvm::SmallString<128> LibPath(Path);
  SearchPathOpts.RuntimeIncludePath = LibPath.str();
  updateRuntimeImportPath();
}

void CompilerInvocation::updateRuntimeImportPath() {
  llvm::SmallString<128> LibPath(SearchPathOpts.RuntimeIncludePath);

  llvm::Triple Triple(TargetOpts.Triple);
  if (Triple.isiOS())
    if (Triple.getArch() == llvm::Triple::ArchType::x86)
      llvm::sys::path::append(LibPath, "iphonesimulator");
    else 
      llvm::sys::path::append(LibPath, "iphoneos");
  else if (Triple.isMacOSX()) 
    llvm::sys::path::append(LibPath, "macosx");  

  SearchPathOpts.RuntimeImportPath = LibPath.str();
}

void CompilerInvocation::setTargetTriple(StringRef Triple) {
  TargetOpts.Triple = Triple.str();
  updateRuntimeImportPath();
}

// TODO: remove InputKind param once we support storing it in FrontendOptions
static bool ParseFrontendArgs(FrontendOptions &Opts, ArgList &Args,
                              DiagnosticEngine &Diags,
                              SourceFileKind InputKind) {
  using namespace options;

  if (const Arg *A = Args.getLastArg(OPT_o)) {
    Opts.OutputFilename = A->getValue();
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

  if (const Arg *A = Args.getLastArg(OPT_help, OPT_help_hidden)) {
    if (A->getOption().matches(OPT_help)) {
      Opts.PrintHelp = true;
    } else if (A->getOption().matches(OPT_help_hidden)) {
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
    } else {
      // In the absence of any other mode indicators, parse the inputs.
      Action = FrontendOptions::Parse;
    }
  }
  Opts.RequestedAction = Action;

  {
    const Arg *A = Args.getLastArg(OPT_module_name);
    std::string ModuleName;
    if (A) {
      ModuleName = A->getValue();
    } else {
      // The user did not specify a module name, so determine a default fallback
      // based on other options.
      if (Opts.RequestedAction == FrontendOptions::REPL) {
        // Default to a module named "REPL" if we're in REPL mode.
        ModuleName = "REPL";
      } else {
        StringRef OutputFilename(Opts.OutputFilename);
        if (OutputFilename.empty() || OutputFilename == "-") {
          ModuleName = Opts.InputFilenames[0];
        } else {
          ModuleName = OutputFilename;
        }

        ModuleName = llvm::sys::path::stem(ModuleName);
      }
    }

    if (!Lexer::isIdentifier(ModuleName)) {
      if (!Opts.actionHasOutput() ||
          (InputKind == SourceFileKind::Main &&
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

static bool ParseIRGenArgs(IRGenOptions &Opts, ArgList &Args,
                           DiagnosticEngine &Diags,
                           const FrontendOptions &FrontendOpts) {
  using namespace options;

  if (Args.hasArg(OPT_g)) {
    Opts.DebugInfo = true;
    llvm::raw_string_ostream Flags(Opts.DWARFDebugFlags);
    interleave(Args,
               [&](const Arg *Argument) {
                 Flags << Argument->getAsString(Args);
               },
               [&] { Flags << " "; });
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

  if (Args.hasArg(OPT_disable_llvm_optzns)) {
    Opts.DisableLLVMOptzns = true;
  }

  if (Args.hasArg(OPT_enable_dynamic_value_type_layout)) {
    Opts.EnableDynamicValueTypeLayout = true;
  }

  // TODO: investigate whether these should be removed, in favor of definitions
  // in other classes.
  if (!FrontendOpts.InputFilenames.empty())
    Opts.MainInputFilename = FrontendOpts.InputFilenames[0];
  Opts.OutputFilename = FrontendOpts.OutputFilename;

  if (const Arg *A = Args.getLastArg(OPT_target)) {
    Opts.Triple = A->getValue();
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

  for (auto InputArg : *ParsedArgs) {
    switch (InputArg->getOption().getID()) {
    case OPT_parse_as_library:
      setInputKind(SourceFileKind::Library);
      break;

    case OPT_parse_stdlib:
      setParseStdlib();
      break;
    }
  }

  if (ParseFrontendArgs(FrontendOpts, *ParsedArgs, Diags, InputKind)) {
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
  
  if (ParseIRGenArgs(IRGenOpts, *ParsedArgs, Diags, FrontendOpts)) {
    return true;
  }

  // Target options may have changed this path.
  updateRuntimeImportPath();

  if (ParseDiagnosticArgs(DiagnosticOpts, *ParsedArgs, Diags)) {
    return true;
  }

  return false;
}
