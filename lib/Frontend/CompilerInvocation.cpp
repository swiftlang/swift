//===--- CompilerInvocation.cpp - CompilerInvocation methods --------------===//
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

#include "swift/Frontend/Frontend.h"

#if __APPLE__
# include "AppleHostVersionDetection.h"
#endif

#include "swift/Strings.h"
#include "swift/AST/DiagnosticsFrontend.h"
#include "swift/Basic/Platform.h"
#include "swift/Option/Options.h"
#include "swift/Option/SanitizerOptions.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/Triple.h"
#include "llvm/Option/Arg.h"
#include "llvm/Option/ArgList.h"
#include "llvm/Option/Option.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/LineIterator.h"
#include "llvm/Support/Path.h"

using namespace swift;
using namespace llvm::opt;

swift::CompilerInvocation::CompilerInvocation() {
  setTargetTriple(llvm::sys::getDefaultTargetTriple());
}

void CompilerInvocation::setMainExecutablePath(StringRef Path) {
  llvm::SmallString<128> LibPath(Path);
  llvm::sys::path::remove_filename(LibPath); // Remove /swift
  llvm::sys::path::remove_filename(LibPath); // Remove /bin
  llvm::sys::path::append(LibPath, "lib", "swift");
  setRuntimeResourcePath(LibPath.str());
}

static void updateRuntimeLibraryPath(SearchPathOptions &SearchPathOpts,
                                     llvm::Triple &Triple) {
  llvm::SmallString<128> LibPath(SearchPathOpts.RuntimeResourcePath);

  llvm::sys::path::append(LibPath, getPlatformNameForTriple(Triple));
  SearchPathOpts.RuntimeLibraryPath = LibPath.str();

  llvm::sys::path::append(LibPath, swift::getMajorArchitectureName(Triple));
  SearchPathOpts.RuntimeLibraryImportPath = LibPath.str();
}

void CompilerInvocation::setRuntimeResourcePath(StringRef Path) {
  SearchPathOpts.RuntimeResourcePath = Path;
  updateRuntimeLibraryPath(SearchPathOpts, LangOpts.Target);
}

void CompilerInvocation::setTargetTriple(StringRef Triple) {
  LangOpts.setTarget(llvm::Triple(Triple));
  updateRuntimeLibraryPath(SearchPathOpts, LangOpts.Target);
}

SourceFileKind CompilerInvocation::getSourceFileKind() const {
  switch (getInputKind()) {
  case InputFileKind::IFK_Swift:
    return SourceFileKind::Main;
  case InputFileKind::IFK_Swift_Library:
    return SourceFileKind::Library;
  case InputFileKind::IFK_Swift_REPL:
    return SourceFileKind::REPL;
  case InputFileKind::IFK_SIL:
    return SourceFileKind::SIL;
  case InputFileKind::IFK_None:
  case InputFileKind::IFK_LLVM_IR:
    llvm_unreachable("Trying to convert from unsupported InputFileKind");
  }

  llvm_unreachable("Unhandled InputFileKind in switch.");
}

// This is a separate function so that it shows up in stack traces.
LLVM_ATTRIBUTE_NOINLINE
static void debugFailWithAssertion() {
  // This assertion should always fail, per the user's request, and should
  // not be converted to llvm_unreachable.
  assert(0 && "This is an assertion!");
}

// This is a separate function so that it shows up in stack traces.
LLVM_ATTRIBUTE_NOINLINE
static void debugFailWithCrash() {
  LLVM_BUILTIN_TRAP;
}

/// Try to read a file list file.
///
/// Returns false on error.
static bool readFileList(DiagnosticEngine &diags,
                         std::vector<std::string> &inputFiles,
                         const llvm::opt::Arg *filelistPath,
                         const llvm::opt::Arg *primaryFileArg = nullptr,
                         unsigned *primaryFileIndex = nullptr) {
  assert((primaryFileArg == nullptr) || (primaryFileIndex != nullptr) &&
         "did not provide argument for primary file index");

  llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>> buffer =
      llvm::MemoryBuffer::getFile(filelistPath->getValue());
  if (!buffer) {
    diags.diagnose(SourceLoc(), diag::cannot_open_file,
                   filelistPath->getValue(), buffer.getError().message());
    return false;
  }

  bool foundPrimaryFile = false;
  if (primaryFileIndex) *primaryFileIndex = 0;

  for (StringRef line : make_range(llvm::line_iterator(*buffer.get()), {})) {
    inputFiles.push_back(line);

    if (foundPrimaryFile || primaryFileArg == nullptr)
      continue;
    if (line == primaryFileArg->getValue())
      foundPrimaryFile = true;
    else
      ++*primaryFileIndex;
  }

  if (primaryFileArg && !foundPrimaryFile) {
    diags.diagnose(SourceLoc(), diag::error_primary_file_not_found,
                   primaryFileArg->getValue(), filelistPath->getValue());
    return false;
  }

  return true;
}

static bool ParseFrontendArgs(FrontendOptions &Opts, ArgList &Args,
                              DiagnosticEngine &Diags) {
  using namespace options;

  if (const Arg *A = Args.getLastArg(OPT_debug_crash_Group)) {
    Option Opt = A->getOption();
    if (Opt.matches(OPT_debug_assert_immediately)) {
      debugFailWithAssertion();
    } else if (Opt.matches(OPT_debug_crash_immediately)) {
      debugFailWithCrash();
    } else if (Opt.matches(OPT_debug_assert_after_parse)) {
      // Set in FrontendOptions
      Opts.CrashMode = FrontendOptions::DebugCrashMode::AssertAfterParse;
    } else if (Opt.matches(OPT_debug_crash_after_parse)) {
      // Set in FrontendOptions
      Opts.CrashMode = FrontendOptions::DebugCrashMode::CrashAfterParse;
    } else {
      llvm_unreachable("Unknown debug_crash_Group option!");
    }
  }

  if (const Arg *A = Args.getLastArg(OPT_dump_api_path)) {
    Opts.DumpAPIPath = A->getValue();
  }

  if (const Arg *A = Args.getLastArg(OPT_group_info_path)) {
    Opts.GroupInfoPath = A->getValue();
  }

  Opts.EmitVerboseSIL |= Args.hasArg(OPT_emit_verbose_sil);
  Opts.EmitSortedSIL |= Args.hasArg(OPT_emit_sorted_sil);

  Opts.DelayedFunctionBodyParsing |= Args.hasArg(OPT_delayed_function_body_parsing);
  Opts.EnableTesting |= Args.hasArg(OPT_enable_testing);
  Opts.EnableResilience |= Args.hasArg(OPT_enable_resilience);

  Opts.PrintStats |= Args.hasArg(OPT_print_stats);
  Opts.PrintClangStats |= Args.hasArg(OPT_print_clang_stats);
#if defined(NDEBUG) && !defined(LLVM_ENABLE_STATS)
  if (Opts.PrintStats || Opts.PrintClangStats)
    Diags.diagnose(SourceLoc(), diag::stats_disabled);
#endif

  Opts.DebugTimeFunctionBodies |= Args.hasArg(OPT_debug_time_function_bodies);
  Opts.DebugTimeExpressionTypeChecking |=
    Args.hasArg(OPT_debug_time_expression_type_checking);
  Opts.DebugTimeCompilation |= Args.hasArg(OPT_debug_time_compilation);
  if (const Arg *A = Args.getLastArg(OPT_stats_output_dir)) {
    Opts.StatsOutputDir = A->getValue();
  }

  Opts.ValidateTBDAgainstIR |= Args.hasArg(OPT_validate_tbd_against_ir);

  if (const Arg *A = Args.getLastArg(OPT_warn_long_function_bodies)) {
    unsigned attempt;
    if (StringRef(A->getValue()).getAsInteger(10, attempt)) {
      Diags.diagnose(SourceLoc(), diag::error_invalid_arg_value,
                     A->getAsString(Args), A->getValue());
    } else {
      Opts.WarnLongFunctionBodies = attempt;
    }
  }

  Opts.PlaygroundTransform |= Args.hasArg(OPT_playground);
  if (Args.hasArg(OPT_disable_playground_transform))
    Opts.PlaygroundTransform = false;
  Opts.PlaygroundHighPerformance |=
    Args.hasArg(OPT_playground_high_performance);

  // This can be enabled independently of the playground transform.
  Opts.PCMacro |= Args.hasArg(OPT_pc_macro);

  if (const Arg *A = Args.getLastArg(OPT_help, OPT_help_hidden)) {
    if (A->getOption().matches(OPT_help)) {
      Opts.PrintHelp = true;
    } else if (A->getOption().matches(OPT_help_hidden)) {
      Opts.PrintHelpHidden = true;
    } else {
      llvm_unreachable("Unknown help option parsed");
    }
  }

  if (const Arg *A = Args.getLastArg(OPT_filelist)) {
    const Arg *primaryFileArg = Args.getLastArg(OPT_primary_file);
    unsigned primaryFileIndex = 0;
    if (readFileList(Diags, Opts.InputFilenames, A,
                     primaryFileArg, &primaryFileIndex)) {
      if (primaryFileArg)
        Opts.PrimaryInput = SelectedInput(primaryFileIndex);
      assert(!Args.hasArg(OPT_INPUT) && "mixing -filelist with inputs");
    }
  } else {
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
  }

  Opts.ParseStdlib |= Args.hasArg(OPT_parse_stdlib);

  // Determine what the user has asked the frontend to do.
  FrontendOptions::ActionType &Action = Opts.RequestedAction;
  if (const Arg *A = Args.getLastArg(OPT_modes_Group)) {
    Option Opt = A->getOption();
    if (Opt.matches(OPT_emit_object)) {
      Action = FrontendOptions::EmitObject;
    } else if (Opt.matches(OPT_emit_assembly)) {
      Action = FrontendOptions::EmitAssembly;
    } else if (Opt.matches(OPT_emit_ir)) {
      Action = FrontendOptions::EmitIR;
    } else if (Opt.matches(OPT_emit_bc)) {
      Action = FrontendOptions::EmitBC;
    } else if (Opt.matches(OPT_emit_sil)) {
      Action = FrontendOptions::EmitSIL;
    } else if (Opt.matches(OPT_emit_silgen)) {
      Action = FrontendOptions::EmitSILGen;
    } else if (Opt.matches(OPT_emit_sib)) {
      Action = FrontendOptions::EmitSIB;
    } else if (Opt.matches(OPT_emit_sibgen)) {
      Action = FrontendOptions::EmitSIBGen;
    } else if (Opt.matches(OPT_emit_pch)) {
      Action = FrontendOptions::EmitPCH;
    } else if (Opt.matches(OPT_emit_imported_modules)) {
      Action = FrontendOptions::EmitImportedModules;
    } else if (Opt.matches(OPT_emit_tbd)) {
      Action = FrontendOptions::EmitTBD;
    } else if (Opt.matches(OPT_parse)) {
      Action = FrontendOptions::Parse;
    } else if (Opt.matches(OPT_typecheck)) {
      Action = FrontendOptions::Typecheck;
    } else if (Opt.matches(OPT_dump_parse)) {
      Action = FrontendOptions::DumpParse;
    } else if (Opt.matches(OPT_dump_ast)) {
      Action = FrontendOptions::DumpAST;
    } else if (Opt.matches(OPT_dump_scope_maps)) {
      Action = FrontendOptions::DumpScopeMaps;

      StringRef value = A->getValue();
      if (value == "expanded") {
        // Note: fully expanded the scope map.
      } else {
        // Parse a comma-separated list of line:column for lookups to
        // perform (and dump the result of).
        SmallVector<StringRef, 4> locations;
        value.split(locations, ',');

        bool invalid = false;
        for (auto location : locations) {
          auto lineColumnStr = location.split(':');
          unsigned line, column;
          if (lineColumnStr.first.getAsInteger(10, line) ||
              lineColumnStr.second.getAsInteger(10, column)) {
            Diags.diagnose(SourceLoc(), diag::error_invalid_source_location_str,
                           location);
            invalid = true;
            continue;
          }

          Opts.DumpScopeMapLocations.push_back({line, column});
        }

        if (!invalid && Opts.DumpScopeMapLocations.empty())
          Diags.diagnose(SourceLoc(), diag::error_no_source_location_scope_map);
      }
    } else if (Opt.matches(OPT_dump_type_refinement_contexts)) {
      Action = FrontendOptions::DumpTypeRefinementContexts;
    } else if (Opt.matches(OPT_dump_interface_hash)) {
      Action = FrontendOptions::DumpInterfaceHash;
    } else if (Opt.matches(OPT_print_ast)) {
      Action = FrontendOptions::PrintAST;
    } else if (Opt.matches(OPT_repl) ||
               Opt.matches(OPT_deprecated_integrated_repl)) {
      Action = FrontendOptions::REPL;
    } else if (Opt.matches(OPT_interpret)) {
      Action = FrontendOptions::Immediate;
    } else {
      llvm_unreachable("Unhandled mode option");
    }
  } else {
    // We don't have a mode, so determine a default.
    if (Args.hasArg(OPT_emit_module, OPT_emit_module_path)) {
      // We've been told to emit a module, but have no other mode indicators.
      // As a result, put the frontend into EmitModuleOnly mode.
      // (Setting up module output will be handled below.)
      Action = FrontendOptions::EmitModuleOnly;
    }
  }

  if (Opts.RequestedAction == FrontendOptions::Immediate &&
      Opts.PrimaryInput.hasValue()) {
    Diags.diagnose(SourceLoc(), diag::error_immediate_mode_primary_file);
    return true;
  }

  bool TreatAsSIL = Args.hasArg(OPT_parse_sil);
  if (!TreatAsSIL && Opts.InputFilenames.size() == 1) {
    // If we have exactly one input filename, and its extension is "sil",
    // treat the input as SIL.
    StringRef Input(Opts.InputFilenames[0]);
    TreatAsSIL = llvm::sys::path::extension(Input).endswith(SIL_EXTENSION);
  } else if (!TreatAsSIL && Opts.PrimaryInput.hasValue() &&
             Opts.PrimaryInput->isFilename()) {
    // If we have a primary input and it's a filename with extension "sil",
    // treat the input as SIL.
    StringRef Input(Opts.InputFilenames[Opts.PrimaryInput->Index]);
    TreatAsSIL = llvm::sys::path::extension(Input).endswith(SIL_EXTENSION);
  }

  // If we have exactly one input filename, and its extension is "bc" or "ll",
  // treat the input as LLVM_IR.
  bool TreatAsLLVM = false;
  if (Opts.InputFilenames.size() == 1) {
    StringRef Input(Opts.InputFilenames[0]);
    TreatAsLLVM =
      llvm::sys::path::extension(Input).endswith(LLVM_BC_EXTENSION) ||
      llvm::sys::path::extension(Input).endswith(LLVM_IR_EXTENSION);
  }

  if (Opts.RequestedAction == FrontendOptions::REPL) {
    if (!Opts.InputFilenames.empty()) {
      Diags.diagnose(SourceLoc(), diag::error_repl_requires_no_input_files);
      return true;
    }
  } else if (TreatAsSIL && Opts.PrimaryInput.hasValue()) {
    // If we have the SIL as our primary input, we can waive the one file
    // requirement as long as all the other inputs are SIBs.
    if (Opts.PrimaryInput.hasValue()) {
      for (unsigned i = 0, e = Opts.InputFilenames.size(); i != e; ++i) {
        if (i == Opts.PrimaryInput->Index)
          continue;

        StringRef File(Opts.InputFilenames[i]);
        if (!llvm::sys::path::extension(File).endswith(SIB_EXTENSION)) {
          Diags.diagnose(SourceLoc(),
                         diag::error_mode_requires_one_sil_multi_sib);
          return true;
        }
      }
    }
  } else if (TreatAsSIL) {
    if (Opts.InputFilenames.size() != 1) {
      Diags.diagnose(SourceLoc(), diag::error_mode_requires_one_input_file);
      return true;
    }
  } else if (Opts.RequestedAction != FrontendOptions::NoneAction) {
    if (Opts.InputFilenames.empty()) {
      Diags.diagnose(SourceLoc(), diag::error_mode_requires_an_input_file);
      return true;
    }
  }

  if (Opts.RequestedAction == FrontendOptions::Immediate) {
    assert(!Opts.InputFilenames.empty());
    Opts.ImmediateArgv.push_back(Opts.InputFilenames[0]); // argv[0]
    if (const Arg *A = Args.getLastArg(OPT__DASH_DASH)) {
      for (unsigned i = 0, e = A->getNumValues(); i != e; ++i) {
        Opts.ImmediateArgv.push_back(A->getValue(i));
      }
    }
  }

  if (TreatAsSIL)
    Opts.InputKind = InputFileKind::IFK_SIL;
  else if (TreatAsLLVM)
    Opts.InputKind = InputFileKind::IFK_LLVM_IR;
  else if (Args.hasArg(OPT_parse_as_library))
    Opts.InputKind = InputFileKind::IFK_Swift_Library;
  else if (Action == FrontendOptions::REPL)
    Opts.InputKind = InputFileKind::IFK_Swift_REPL;
  else
    Opts.InputKind = InputFileKind::IFK_Swift;

  if (const Arg *A = Args.getLastArg(OPT_output_filelist)) {
    readFileList(Diags, Opts.OutputFilenames, A);
    assert(!Args.hasArg(OPT_o) && "don't use -o with -output-filelist");
  } else {
    Opts.OutputFilenames = Args.getAllArgValues(OPT_o);
  }

  bool UserSpecifiedModuleName = false;
  {
    const Arg *A = Args.getLastArg(OPT_module_name);
    StringRef ModuleName = Opts.ModuleName;
    if (A) {
      ModuleName = A->getValue();
      UserSpecifiedModuleName = true;
    } else if (ModuleName.empty()) {
      // The user did not specify a module name, so determine a default fallback
      // based on other options.

      // Note: this code path will only be taken when running the frontend
      // directly; the driver should always pass -module-name when invoking the
      // frontend.
      if (Opts.RequestedAction == FrontendOptions::REPL) {
        // Default to a module named "REPL" if we're in REPL mode.
        ModuleName = "REPL";
      } else if (!Opts.InputFilenames.empty()) {
        StringRef OutputFilename = Opts.getSingleOutputFilename();
        if (OutputFilename.empty() || OutputFilename == "-" ||
            llvm::sys::fs::is_directory(OutputFilename)) {
          ModuleName = Opts.InputFilenames[0];
        } else {
          ModuleName = OutputFilename;
        }

        ModuleName = llvm::sys::path::stem(ModuleName);
      }
    }

    if (!Lexer::isIdentifier(ModuleName) ||
        (ModuleName == STDLIB_NAME && !Opts.ParseStdlib)) {
      if (!Opts.actionHasOutput() ||
          (Opts.InputKind == InputFileKind::IFK_Swift &&
           Opts.InputFilenames.size() == 1)) {
        ModuleName = "main";
      } else {
        auto DID = (ModuleName == STDLIB_NAME) ? diag::error_stdlib_module_name
                                               : diag::error_bad_module_name;
        Diags.diagnose(SourceLoc(), DID, ModuleName, A == nullptr);
        ModuleName = "__bad__";
      }
    }

    Opts.ModuleName = ModuleName;
  }

  if (Opts.OutputFilenames.empty() ||
      llvm::sys::fs::is_directory(Opts.getSingleOutputFilename())) {
    // No output filename was specified, or an output directory was specified.
    // Determine the correct output filename.

    // Note: this should typically only be used when invoking the frontend
    // directly, as the driver will always pass -o with an appropriate filename
    // if output is required for the requested action.

    StringRef Suffix;
    switch (Opts.RequestedAction) {
    case FrontendOptions::NoneAction:
      break;

    case FrontendOptions::Parse:
    case FrontendOptions::Typecheck:
    case FrontendOptions::DumpParse:
    case FrontendOptions::DumpInterfaceHash:
    case FrontendOptions::DumpAST:
    case FrontendOptions::PrintAST:
    case FrontendOptions::DumpScopeMaps:
    case FrontendOptions::DumpTypeRefinementContexts:
      // Textual modes.
      Opts.setSingleOutputFilename("-");
      break;

    case FrontendOptions::EmitPCH:
      Suffix = PCH_EXTENSION;
      break;

    case FrontendOptions::EmitSILGen:
    case FrontendOptions::EmitSIL: {
      if (Opts.OutputFilenames.empty())
        Opts.setSingleOutputFilename("-");
      else
        Suffix = SIL_EXTENSION;
      break;
    }

    case FrontendOptions::EmitSIBGen:
    case FrontendOptions::EmitSIB:
      Suffix = SIB_EXTENSION;
      break;

    case FrontendOptions::EmitModuleOnly:
      Suffix = SERIALIZED_MODULE_EXTENSION;
      break;

    case FrontendOptions::Immediate:
    case FrontendOptions::REPL:
      // These modes have no frontend-generated output.
      Opts.OutputFilenames.clear();
      break;

    case FrontendOptions::EmitAssembly: {
      if (Opts.OutputFilenames.empty())
        Opts.setSingleOutputFilename("-");
      else
        Suffix = "s";
      break;
    }

    case FrontendOptions::EmitIR: {
      if (Opts.OutputFilenames.empty())
        Opts.setSingleOutputFilename("-");
      else
        Suffix = "ll";
      break;
    }

    case FrontendOptions::EmitBC: {
      Suffix = "bc";
      break;
    }

    case FrontendOptions::EmitObject:
      Suffix = "o";
      break;

    case FrontendOptions::EmitImportedModules:
      if (Opts.OutputFilenames.empty())
        Opts.setSingleOutputFilename("-");
      else
        Suffix = "importedmodules";
      break;

    case FrontendOptions::EmitTBD:
      if (Opts.OutputFilenames.empty())
        Opts.setSingleOutputFilename("-");
      else
        Suffix = "tbd";
      break;
    }

    if (!Suffix.empty()) {
      // We need to deduce a file name.

      // First, if we're reading from stdin and we don't have a directory,
      // output to stdout.
      if (Opts.InputFilenames.size() == 1 && Opts.InputFilenames[0] == "-" &&
          Opts.OutputFilenames.empty())
        Opts.setSingleOutputFilename("-");
      else {
        // We have a suffix, so determine an appropriate name.
        llvm::SmallString<128> Path(Opts.getSingleOutputFilename());

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

        Opts.setSingleOutputFilename(Path.str());
      }
    }

    if (Opts.OutputFilenames.empty()) {
      if (Opts.RequestedAction != FrontendOptions::REPL &&
          Opts.RequestedAction != FrontendOptions::Immediate &&
          Opts.RequestedAction != FrontendOptions::NoneAction) {
        Diags.diagnose(SourceLoc(), diag::error_no_output_filename_specified);
        return true;
      }
    } else if (Opts.getSingleOutputFilename() != "-" &&
        llvm::sys::fs::is_directory(Opts.getSingleOutputFilename())) {
      Diags.diagnose(SourceLoc(), diag::error_implicit_output_file_is_directory,
                     Opts.getSingleOutputFilename());
      return true;
    }
  }

  auto determineOutputFilename = [&](std::string &output,
                                     OptSpecifier optWithoutPath,
                                     OptSpecifier optWithPath,
                                     const char *extension,
                                     bool useMainOutput) {
    if (const Arg *A = Args.getLastArg(optWithPath)) {
      Args.ClaimAllArgs(optWithoutPath);
      output = A->getValue();
      return;
    }

    if (!Args.hasArg(optWithoutPath))
      return;

    if (useMainOutput && !Opts.OutputFilenames.empty()) {
      output = Opts.getSingleOutputFilename();
      return;
    }

    if (!output.empty())
      return;

    StringRef OriginalPath;
    if (!Opts.OutputFilenames.empty() && Opts.getSingleOutputFilename() != "-")
      // Put the serialized diagnostics file next to the output file.
      OriginalPath = Opts.getSingleOutputFilename();
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
    llvm::sys::path::replace_extension(Path, extension);
    output = Path.str();
  };

  determineOutputFilename(Opts.DependenciesFilePath,
                          OPT_emit_dependencies,
                          OPT_emit_dependencies_path,
                          "d", false);
  determineOutputFilename(Opts.ReferenceDependenciesFilePath,
                          OPT_emit_reference_dependencies,
                          OPT_emit_reference_dependencies_path,
                          "swiftdeps", false);
  determineOutputFilename(Opts.SerializedDiagnosticsPath,
                          OPT_serialize_diagnostics,
                          OPT_serialize_diagnostics_path,
                          "dia", false);
  determineOutputFilename(Opts.ObjCHeaderOutputPath,
                          OPT_emit_objc_header,
                          OPT_emit_objc_header_path,
                          "h", false);

  if (const Arg *A = Args.getLastArg(OPT_emit_fixits_path)) {
    Opts.FixitsOutputPath = A->getValue();
  }

  bool IsSIB =
    Opts.RequestedAction == FrontendOptions::EmitSIB ||
    Opts.RequestedAction == FrontendOptions::EmitSIBGen;
  bool canUseMainOutputForModule =
    Opts.RequestedAction == FrontendOptions::EmitModuleOnly || IsSIB;
  auto ext = IsSIB ? SIB_EXTENSION : SERIALIZED_MODULE_EXTENSION;
  auto sibOpt = Opts.RequestedAction == FrontendOptions::EmitSIB ?
    OPT_emit_sib : OPT_emit_sibgen;
  determineOutputFilename(Opts.ModuleOutputPath,
                          IsSIB ? sibOpt : OPT_emit_module,
                          OPT_emit_module_path,
                          ext,
                          canUseMainOutputForModule);

  determineOutputFilename(Opts.ModuleDocOutputPath,
                          OPT_emit_module_doc,
                          OPT_emit_module_doc_path,
                          SERIALIZED_MODULE_DOC_EXTENSION,
                          false);

  if (!Opts.DependenciesFilePath.empty()) {
    switch (Opts.RequestedAction) {
    case FrontendOptions::NoneAction:
    case FrontendOptions::DumpParse:
    case FrontendOptions::DumpInterfaceHash:
    case FrontendOptions::DumpAST:
    case FrontendOptions::PrintAST:
    case FrontendOptions::DumpScopeMaps:
    case FrontendOptions::DumpTypeRefinementContexts:
    case FrontendOptions::Immediate:
    case FrontendOptions::REPL:
      Diags.diagnose(SourceLoc(), diag::error_mode_cannot_emit_dependencies);
      return true;
    case FrontendOptions::Parse:
    case FrontendOptions::Typecheck:
    case FrontendOptions::EmitModuleOnly:
    case FrontendOptions::EmitPCH:
    case FrontendOptions::EmitSILGen:
    case FrontendOptions::EmitSIL:
    case FrontendOptions::EmitSIBGen:
    case FrontendOptions::EmitSIB:
    case FrontendOptions::EmitIR:
    case FrontendOptions::EmitBC:
    case FrontendOptions::EmitAssembly:
    case FrontendOptions::EmitObject:
    case FrontendOptions::EmitImportedModules:
    case FrontendOptions::EmitTBD:
      break;
    }
  }

  if (!Opts.ObjCHeaderOutputPath.empty()) {
    switch (Opts.RequestedAction) {
    case FrontendOptions::NoneAction:
    case FrontendOptions::DumpParse:
    case FrontendOptions::DumpInterfaceHash:
    case FrontendOptions::DumpAST:
    case FrontendOptions::PrintAST:
    case FrontendOptions::EmitPCH:
    case FrontendOptions::DumpScopeMaps:
    case FrontendOptions::DumpTypeRefinementContexts:
    case FrontendOptions::Immediate:
    case FrontendOptions::REPL:
      Diags.diagnose(SourceLoc(), diag::error_mode_cannot_emit_header);
      return true;
    case FrontendOptions::Parse:
    case FrontendOptions::Typecheck:
    case FrontendOptions::EmitModuleOnly:
    case FrontendOptions::EmitSILGen:
    case FrontendOptions::EmitSIL:
    case FrontendOptions::EmitSIBGen:
    case FrontendOptions::EmitSIB:
    case FrontendOptions::EmitIR:
    case FrontendOptions::EmitBC:
    case FrontendOptions::EmitAssembly:
    case FrontendOptions::EmitObject:
    case FrontendOptions::EmitImportedModules:
    case FrontendOptions::EmitTBD:
      break;
    }
  }

  if (!Opts.ModuleOutputPath.empty() ||
      !Opts.ModuleDocOutputPath.empty()) {
    switch (Opts.RequestedAction) {
    case FrontendOptions::NoneAction:
    case FrontendOptions::Parse:
    case FrontendOptions::Typecheck:
    case FrontendOptions::DumpParse:
    case FrontendOptions::DumpInterfaceHash:
    case FrontendOptions::DumpAST:
    case FrontendOptions::PrintAST:
    case FrontendOptions::EmitPCH:
    case FrontendOptions::DumpScopeMaps:
    case FrontendOptions::DumpTypeRefinementContexts:
    case FrontendOptions::EmitSILGen:
    case FrontendOptions::Immediate:
    case FrontendOptions::REPL:
      if (!Opts.ModuleOutputPath.empty())
        Diags.diagnose(SourceLoc(), diag::error_mode_cannot_emit_module);
      else
        Diags.diagnose(SourceLoc(), diag::error_mode_cannot_emit_module_doc);
      return true;
    case FrontendOptions::EmitModuleOnly:
    case FrontendOptions::EmitSIL:
    case FrontendOptions::EmitSIBGen:
    case FrontendOptions::EmitSIB:
    case FrontendOptions::EmitIR:
    case FrontendOptions::EmitBC:
    case FrontendOptions::EmitAssembly:
    case FrontendOptions::EmitObject:
    case FrontendOptions::EmitImportedModules:
    case FrontendOptions::EmitTBD:
      break;
    }
  }

  if (const Arg *A = Args.getLastArg(OPT_module_link_name)) {
    Opts.ModuleLinkName = A->getValue();
  }

  Opts.AlwaysSerializeDebuggingOptions |=
      Args.hasArg(OPT_serialize_debugging_options);
  Opts.EnableSourceImport |= Args.hasArg(OPT_enable_source_import);
  Opts.ImportUnderlyingModule |= Args.hasArg(OPT_import_underlying_module);
  Opts.SILSerializeAll |= Args.hasArg(OPT_sil_serialize_all);
  Opts.EnableSerializationNestedTypeLookupTable &=
      !Args.hasArg(OPT_disable_serialization_nested_type_lookup_table);

  if (const Arg *A = Args.getLastArgNoClaim(OPT_import_objc_header)) {
    Opts.ImplicitObjCHeaderPath = A->getValue();
    Opts.SerializeBridgingHeader |=
      !Opts.PrimaryInput && !Opts.ModuleOutputPath.empty();
  }

  for (const Arg *A : make_range(Args.filtered_begin(OPT_import_module),
                                 Args.filtered_end())) {
    Opts.ImplicitImportModuleNames.push_back(A->getValue());
  }

  for (const Arg *A : make_range(Args.filtered_begin(OPT_Xllvm),
                                 Args.filtered_end())) {
    Opts.LLVMArgs.push_back(A->getValue());
  }

  return false;
}

static void diagnoseSwiftVersion(Optional<version::Version> &vers, Arg *verArg,
                                 ArgList &Args, DiagnosticEngine &diags) {
  // General invalid version error
  diags.diagnose(SourceLoc(), diag::error_invalid_arg_value,
                 verArg->getAsString(Args), verArg->getValue());

  // Check for an unneeded minor version, otherwise just list valid versions
  if (vers.hasValue() && !vers.getValue().empty() &&
      vers.getValue().asMajorVersion().getEffectiveLanguageVersion()) {
    diags.diagnose(SourceLoc(), diag::note_swift_version_major,
                   vers.getValue()[0]);
  } else {
    // Note valid versions instead
    auto validVers = version::Version::getValidEffectiveVersions();
    auto versStr =
        "'" + llvm::join(validVers.begin(), validVers.end(), "', '") + "'";
    diags.diagnose(SourceLoc(), diag::note_valid_swift_versions, versStr);
  }
}

static bool ParseLangArgs(LangOptions &Opts, ArgList &Args,
                          DiagnosticEngine &Diags,
                          const FrontendOptions &FrontendOpts) {
  using namespace options;

  if (auto A = Args.getLastArg(OPT_swift_version)) {
    auto vers = version::Version::parseVersionString(
      A->getValue(), SourceLoc(), &Diags);
    bool isValid = false;
    if (vers.hasValue()) {
      if (auto effectiveVers = vers.getValue().getEffectiveLanguageVersion()) {
        Opts.EffectiveLanguageVersion = effectiveVers.getValue();
        isValid = true;
      }
    }
    if (!isValid)
      diagnoseSwiftVersion(vers, A, Args, Diags);
  }

  Opts.AttachCommentsToDecls |= Args.hasArg(OPT_dump_api_path);

  Opts.UseMalloc |= Args.hasArg(OPT_use_malloc);

  Opts.DiagnosticsEditorMode |= Args.hasArg(OPT_diagnostics_editor_mode);

  Opts.EnableExperimentalPropertyBehaviors |=
    Args.hasArg(OPT_enable_experimental_property_behaviors);

  Opts.EnableClassResilience |=
    Args.hasArg(OPT_enable_class_resilience);

  Opts.DisableAvailabilityChecking |=
      Args.hasArg(OPT_disable_availability_checking);

  Opts.DisableTsanInoutInstrumentation |=
      Args.hasArg(OPT_disable_tsan_inout_instrumentation);

  Opts.VisualOsanAccessInstrumentation |=
    Args.hasArg(OPT_visual_osan_unwrap_instrumentation);

  if (FrontendOpts.InputKind == InputFileKind::IFK_SIL)
    Opts.DisableAvailabilityChecking = true;
  
  if (auto A = Args.getLastArg(OPT_enable_access_control,
                               OPT_disable_access_control)) {
    Opts.EnableAccessControl
      = A->getOption().matches(OPT_enable_access_control);
  }

  Opts.DisableTypoCorrection |= Args.hasArg(OPT_disable_typo_correction);

  Opts.CodeCompleteInitsInPostfixExpr |=
      Args.hasArg(OPT_code_complete_inits_in_postfix_expr);

  if (auto A = Args.getLastArg(OPT_enable_target_os_checking,
                               OPT_disable_target_os_checking)) {
    Opts.EnableTargetOSChecking
      = A->getOption().matches(OPT_enable_target_os_checking);
  }
  
  Opts.EnableASTScopeLookup |= Args.hasArg(OPT_enable_astscope_lookup);
  Opts.DebugConstraintSolver |= Args.hasArg(OPT_debug_constraints);
  Opts.EnableConstraintPropagation |= Args.hasArg(OPT_propagate_constraints);
  Opts.IterativeTypeChecker |= Args.hasArg(OPT_iterative_type_checker);
  Opts.DebugGenericSignatures |= Args.hasArg(OPT_debug_generic_signatures);

  Opts.DebuggerSupport |= Args.hasArg(OPT_debugger_support);
  if (Opts.DebuggerSupport)
    Opts.EnableDollarIdentifiers = true;
  Opts.Playground |= Args.hasArg(OPT_playground);
  Opts.InferImportAsMember |= Args.hasArg(OPT_enable_infer_import_as_member);

  Opts.EnableThrowWithoutTry |= Args.hasArg(OPT_enable_throw_without_try);

  if (auto A = Args.getLastArg(OPT_enable_objc_attr_requires_foundation_module,
                               OPT_disable_objc_attr_requires_foundation_module)) {
    Opts.EnableObjCAttrRequiresFoundation
      = A->getOption().matches(OPT_enable_objc_attr_requires_foundation_module);
  }

  if (auto A = Args.getLastArg(OPT_enable_testable_attr_requires_testable_module,
                               OPT_disable_testable_attr_requires_testable_module)) {
    Opts.EnableTestableAttrRequiresTestableModule
      = A->getOption().matches(OPT_enable_testable_attr_requires_testable_module);
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
  
  if (const Arg *A = Args.getLastArg(OPT_debug_forbid_typecheck_prefix)) {
    Opts.DebugForbidTypecheckPrefix = A->getValue();
  }

  if (const Arg *A = Args.getLastArg(OPT_solver_memory_threshold)) {
    unsigned threshold;
    if (StringRef(A->getValue()).getAsInteger(10, threshold)) {
      Diags.diagnose(SourceLoc(), diag::error_invalid_arg_value,
                     A->getAsString(Args), A->getValue());
      return true;
    }
    
    Opts.SolverMemoryThreshold = threshold;
  }
  
  for (const Arg *A : make_range(Args.filtered_begin(OPT_D),
                                 Args.filtered_end())) {
    Opts.addCustomConditionalCompilationFlag(A->getValue());
  }

  Opts.EnableAppExtensionRestrictions |= Args.hasArg(OPT_enable_app_extension);
  Opts.WarnSwift3ObjCInference |= Args.hasArg(OPT_warn_swift3_objc_inference);

  Opts.EnableSwift3ObjCInference =
    Args.hasFlag(OPT_enable_swift3_objc_inference,
                 OPT_disable_swift3_objc_inference,
                 Opts.isSwiftVersion3());

  llvm::Triple Target = Opts.Target;
  StringRef TargetArg;
  if (const Arg *A = Args.getLastArg(OPT_target)) {
    Target = llvm::Triple(A->getValue());
    TargetArg = A->getValue();
  }
#if __APPLE__
  else if (FrontendOpts.actionIsImmediate()) {
    clang::VersionTuple currentOSVersion = inferAppleHostOSVersion();
    if (currentOSVersion.getMajor() != 0) {
      llvm::Triple::OSType currentOS = Target.getOS();
      if (currentOS == llvm::Triple::Darwin)
        currentOS = llvm::Triple::MacOSX;

      SmallString<16> newOSBuf;
      llvm::raw_svector_ostream newOS(newOSBuf);
      newOS << llvm::Triple::getOSTypeName(currentOS) << currentOSVersion;
      Target.setOSName(newOS.str());
    }
  }
#endif

  Opts.EnableObjCInterop = Target.isOSDarwin();
  if (auto A = Args.getLastArg(OPT_enable_objc_interop,
                               OPT_disable_objc_interop)) {
    Opts.EnableObjCInterop
      = A->getOption().matches(OPT_enable_objc_interop);
  }

  Opts.EnableSILOpaqueValues |= Args.hasArg(OPT_enable_sil_opaque_values);

  // Must be processed after any other language options that could affect
  // platform conditions.
  bool UnsupportedOS, UnsupportedArch;
  std::tie(UnsupportedOS, UnsupportedArch) = Opts.setTarget(Target);

  SmallVector<StringRef, 3> TargetComponents;
  TargetArg.split(TargetComponents, "-");

  if (UnsupportedArch) {
    auto TargetArgArch = TargetComponents.size() ? TargetComponents[0] : "";
    Diags.diagnose(SourceLoc(), diag::error_unsupported_target_arch, TargetArgArch);
  }

  if (UnsupportedOS) {
    auto TargetArgOS = TargetComponents.size() > 2 ? TargetComponents[2] : "";
    Diags.diagnose(SourceLoc(), diag::error_unsupported_target_os, TargetArgOS);
  }

  return UnsupportedOS || UnsupportedArch;
}

static bool ParseClangImporterArgs(ClangImporterOptions &Opts,
                                   ArgList &Args,
                                   DiagnosticEngine &Diags,
                                   StringRef workingDirectory) {
  using namespace options;

  if (const Arg *A = Args.getLastArg(OPT_module_cache_path)) {
    Opts.ModuleCachePath = A->getValue();
  }

  if (const Arg *A = Args.getLastArg(OPT_target_cpu))
    Opts.TargetCPU = A->getValue();

  for (const Arg *A : make_range(Args.filtered_begin(OPT_Xcc),
                                 Args.filtered_end())) {
    Opts.ExtraArgs.push_back(A->getValue());
  }

  if (!workingDirectory.empty()) {
    // Provide a working directory to Clang as well if there are any -Xcc
    // options, in case some of them are search-related. But do it at the
    // beginning, so that an explicit -Xcc -working-directory will win.
    Opts.ExtraArgs.insert(Opts.ExtraArgs.begin(), {
      "-working-directory", workingDirectory
    });
  }

  Opts.InferImportAsMember |= Args.hasArg(OPT_enable_infer_import_as_member);
  Opts.DumpClangDiagnostics |= Args.hasArg(OPT_dump_clang_diagnostics);

  if (Args.hasArg(OPT_embed_bitcode))
    Opts.Mode = ClangImporterOptions::Modes::EmbedBitcode;
  if (auto *A = Args.getLastArg(OPT_import_objc_header))
    Opts.BridgingHeader = A->getValue();
  Opts.DisableSwiftBridgeAttr |= Args.hasArg(OPT_disable_swift_bridge_attr);

  Opts.DisableModulesValidateSystemHeaders |= Args.hasArg(OPT_disable_modules_validate_system_headers);

  Opts.DisableAdapterModules |= Args.hasArg(OPT_emit_imported_modules);

  if (const Arg *A = Args.getLastArg(OPT_pch_output_dir)) {
    Opts.PrecompiledHeaderOutputDir = A->getValue();
  }

  return false;
}

static bool ParseSearchPathArgs(SearchPathOptions &Opts,
                                ArgList &Args,
                                DiagnosticEngine &Diags,
                                StringRef workingDirectory) {
  using namespace options;
  namespace path = llvm::sys::path;

  auto resolveSearchPath =
      [workingDirectory](StringRef searchPath) -> std::string {
    if (workingDirectory.empty() || path::is_absolute(searchPath))
      return searchPath;
    SmallString<64> fullPath{workingDirectory};
    path::append(fullPath, searchPath);
    return fullPath.str();
  };

  for (const Arg *A : make_range(Args.filtered_begin(OPT_I),
                                 Args.filtered_end())) {
    Opts.ImportSearchPaths.push_back(resolveSearchPath(A->getValue()));
  }

  for (const Arg *A : make_range(Args.filtered_begin(OPT_F, OPT_Fsystem),
                                 Args.filtered_end())) {
    Opts.FrameworkSearchPaths.push_back({resolveSearchPath(A->getValue()),
                           /*isSystem=*/A->getOption().getID() == OPT_Fsystem});
  }

  for (const Arg *A : make_range(Args.filtered_begin(OPT_L),
                                 Args.filtered_end())) {
    Opts.LibrarySearchPaths.push_back(resolveSearchPath(A->getValue()));
  }

  if (const Arg *A = Args.getLastArg(OPT_sdk))
    Opts.SDKPath = A->getValue();

  if (const Arg *A = Args.getLastArg(OPT_resource_dir))
    Opts.RuntimeResourcePath = A->getValue();

  Opts.SkipRuntimeLibraryImportPath |= Args.hasArg(OPT_nostdimport);

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

  if (Args.hasArg(OPT_verify))
    Opts.VerifyMode = DiagnosticOptions::Verify;
  if (Args.hasArg(OPT_verify_apply_fixes))
    Opts.VerifyMode = DiagnosticOptions::VerifyAndApplyFixes;
  Opts.VerifyIgnoreUnknown |= Args.hasArg(OPT_verify_ignore_unknown);
  Opts.SkipDiagnosticPasses |= Args.hasArg(OPT_disable_diagnostic_passes);
  Opts.ShowDiagnosticsAfterFatalError |=
    Args.hasArg(OPT_show_diagnostics_after_fatal);
  Opts.UseColor |= Args.hasArg(OPT_color_diagnostics);
  Opts.FixitCodeForAllDiagnostics |= Args.hasArg(OPT_fixit_all);
  Opts.SuppressWarnings |= Args.hasArg(OPT_suppress_warnings);
  Opts.WarningsAsErrors |= Args.hasArg(OPT_warnings_as_errors);

  assert(!(Opts.WarningsAsErrors && Opts.SuppressWarnings) &&
         "conflicting arguments; should have been caught by driver");

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

/// Parse -enforce-exclusivity=... options
void parseExclusivityEnforcementOptions(const llvm::opt::Arg *A,
                                        SILOptions &Opts,
                                        DiagnosticEngine &Diags) {
  StringRef Argument = A->getValue();
  if (Argument == "unchecked") {
    // This option is analogous to the -Ounchecked optimization setting.
    // It will disable dynamic checking but still diagnose statically.
    Opts.EnforceExclusivityStatic = true;
    Opts.EnforceExclusivityDynamic = false;
  } else if (Argument == "checked") {
    Opts.EnforceExclusivityStatic = true;
    Opts.EnforceExclusivityDynamic = true;
  } else if (Argument == "dynamic-only") {
    // This option is intended for staging purposes. The intent is that
    // it will eventually be removed.
    Opts.EnforceExclusivityStatic = false;
    Opts.EnforceExclusivityDynamic = true;
  } else if (Argument == "none") {
    // This option is for staging purposes.
    Opts.EnforceExclusivityStatic = false;
    Opts.EnforceExclusivityDynamic = false;
  } else {
    Diags.diagnose(SourceLoc(), diag::error_unsupported_option_argument,
        A->getOption().getPrefixedName(), A->getValue());
  }
}

static bool ParseSILArgs(SILOptions &Opts, ArgList &Args,
                         IRGenOptions &IRGenOpts,
                         FrontendOptions &FEOpts,
                         DiagnosticEngine &Diags,
                         const llvm::Triple &Triple) {
  using namespace options;

  if (const Arg *A = Args.getLastArg(OPT_sil_inline_threshold)) {
    if (StringRef(A->getValue()).getAsInteger(10, Opts.InlineThreshold)) {
      Diags.diagnose(SourceLoc(), diag::error_invalid_arg_value,
                     A->getAsString(Args), A->getValue());
      return true;
    }
  }
  if (const Arg *A = Args.getLastArg(OPT_num_threads)) {
    if (StringRef(A->getValue()).getAsInteger(10, Opts.NumThreads)) {
      Diags.diagnose(SourceLoc(), diag::error_invalid_arg_value,
                     A->getAsString(Args), A->getValue());
      return true;
    }
  }
  
  if (const Arg *A = Args.getLastArg(OPT_disable_sil_linking,
                                     OPT_sil_link_all)) {
    if (A->getOption().matches(OPT_disable_sil_linking))
      Opts.LinkMode = SILOptions::LinkNone;
    else if (A->getOption().matches(OPT_sil_link_all))
      Opts.LinkMode = SILOptions::LinkAll;
    else
      llvm_unreachable("Unknown SIL linking option!");
  }

  if (Args.hasArg(OPT_sil_merge_partial_modules))
    Opts.MergePartialModules = true;

  // Parse the optimization level.
  if (const Arg *A = Args.getLastArg(OPT_O_Group)) {
    if (A->getOption().matches(OPT_Onone)) {
      IRGenOpts.Optimize = false;
      Opts.Optimization = SILOptions::SILOptMode::None;
    } else if (A->getOption().matches(OPT_Ounchecked)) {
      // Turn on optimizations and remove all runtime checks.
      IRGenOpts.Optimize = true;
      Opts.Optimization = SILOptions::SILOptMode::OptimizeUnchecked;
      // Removal of cond_fail (overflow on binary operations).
      Opts.RemoveRuntimeAsserts = true;
      Opts.AssertConfig = SILOptions::Unchecked;
    } else if (A->getOption().matches(OPT_Oplayground)) {
      // For now -Oplayground is equivalent to -Onone.
      IRGenOpts.Optimize = false;
      Opts.Optimization = SILOptions::SILOptMode::None;
    } else {
      assert(A->getOption().matches(OPT_O));
      IRGenOpts.Optimize = true;
      Opts.Optimization = SILOptions::SILOptMode::Optimize;
    }
  }

  if (Args.getLastArg(OPT_AssumeSingleThreaded)) {
    Opts.AssumeSingleThreaded = true;
  }

  // Parse the assert configuration identifier.
  if (const Arg *A = Args.getLastArg(OPT_AssertConfig)) {
    StringRef Configuration = A->getValue();
    if (Configuration == "DisableReplacement") {
      Opts.AssertConfig = SILOptions::DisableReplacement;
    } else if (Configuration == "Debug") {
      Opts.AssertConfig = SILOptions::Debug;
    } else if (Configuration == "Release") {
      Opts.AssertConfig = SILOptions::Release;
    } else if (Configuration == "Unchecked") {
      Opts.AssertConfig = SILOptions::Unchecked;
    } else {
      Diags.diagnose(SourceLoc(), diag::error_invalid_arg_value,
                     A->getAsString(Args), A->getValue());
      return true;
    }
  } else if (FEOpts.ParseStdlib) {
    // Disable assertion configuration replacement when we build the standard
    // library.
    Opts.AssertConfig = SILOptions::DisableReplacement;
  } else if (Opts.AssertConfig == SILOptions::Debug) {
    // Set the assert configuration according to the optimization level if it
    // has not been set by the -Ounchecked flag.
    Opts.AssertConfig =
        IRGenOpts.Optimize ? SILOptions::Release : SILOptions::Debug;
  }

  // -Ounchecked might also set removal of runtime asserts (cond_fail).
  Opts.RemoveRuntimeAsserts |= Args.hasArg(OPT_remove_runtime_asserts);

  Opts.EnableARCOptimizations |= !Args.hasArg(OPT_disable_arc_opts);
  Opts.DisableSILPerfOptimizations |= Args.hasArg(OPT_disable_sil_perf_optzns);
  Opts.VerifyAll |= Args.hasArg(OPT_sil_verify_all);
  Opts.DebugSerialization |= Args.hasArg(OPT_sil_debug_serialization);
  Opts.EmitVerboseSIL |= Args.hasArg(OPT_emit_verbose_sil);
  Opts.PrintInstCounts |= Args.hasArg(OPT_print_inst_counts);
  if (const Arg *A = Args.getLastArg(OPT_external_pass_pipeline_filename))
    Opts.ExternalPassPipelineFilename = A->getValue();

  Opts.GenerateProfile |= Args.hasArg(OPT_profile_generate);
  Opts.EmitProfileCoverageMapping |= Args.hasArg(OPT_profile_coverage_mapping);
  Opts.EnableGuaranteedClosureContexts |=
    Args.hasArg(OPT_enable_guaranteed_closure_contexts);
  Opts.DisableSILPartialApply |=
    Args.hasArg(OPT_disable_sil_partial_apply);
  Opts.EnableSILOwnership |= Args.hasArg(OPT_enable_sil_ownership);
  Opts.AssumeUnqualifiedOwnershipWhenParsing
    |= Args.hasArg(OPT_assume_parsing_unqualified_ownership_sil);

  if (Args.hasArg(OPT_debug_on_sil)) {
    // Derive the name of the SIL file for debugging from
    // the regular outputfile.
    StringRef BaseName = FEOpts.getSingleOutputFilename();
    // If there are no or multiple outputfiles, derive the name
    // from the module name.
    if (BaseName.empty())
      BaseName = FEOpts.ModuleName;
    Opts.SILOutputFileNameForDebugging = BaseName.str();
  }

  if (const Arg *A = Args.getLastArg(options::OPT_sanitize_EQ)) {
    Opts.Sanitize = parseSanitizerArgValues(A, Triple, Diags);
    IRGenOpts.Sanitize = Opts.Sanitize;
  }

  if (const Arg *A = Args.getLastArg(options::OPT_enforce_exclusivity_EQ)) {
    parseExclusivityEnforcementOptions(A, Opts, Diags);
  }

  /// Should we use the copy-on-write implementation of opaque existentials.
  /// FIXME: Use during bootstraping this feature. Remove later.
  Opts.UseCOWExistentials |= Args.hasArg(OPT_enable_cow_existentials);

  return false;
}

void CompilerInvocation::buildDWARFDebugFlags(std::string &Output,
                                              const ArrayRef<const char*> &Args,
                                              StringRef SDKPath,
                                              StringRef ResourceDir) {
  llvm::raw_string_ostream OS(Output);
  interleave(Args,
             [&](const char *Argument) { PrintArg(OS, Argument, false); },
             [&] { OS << " "; });

  // Inject the SDK path and resource dir if they are nonempty and missing.
  bool haveSDKPath = SDKPath.empty();
  bool haveResourceDir = ResourceDir.empty();
  for (auto A : Args) {
    StringRef Arg(A);
    // FIXME: this should distinguish between key and value.
    if (!haveSDKPath && Arg.equals("-sdk"))
      haveSDKPath = true;
    if (!haveResourceDir && Arg.equals("-resource-dir"))
      haveResourceDir = true;
  }
  if (!haveSDKPath) {
    OS << " -sdk ";
    PrintArg(OS, SDKPath.data(), false);
  }
  if (!haveResourceDir) {
    OS << " -resource-dir ";
    PrintArg(OS, ResourceDir.data(), false);
  }
}

static bool ParseIRGenArgs(IRGenOptions &Opts, ArgList &Args,
                           DiagnosticEngine &Diags,
                           const FrontendOptions &FrontendOpts,
                           const SILOptions &SILOpts,
                           StringRef SDKPath,
                           StringRef ResourceDir,
                           const llvm::Triple &Triple) {
  using namespace options;

  if (!SILOpts.SILOutputFileNameForDebugging.empty()) {
      Opts.DebugInfoKind = IRGenDebugInfoKind::LineTables;
  } else if (const Arg *A = Args.getLastArg(OPT_g_Group)) {
    if (A->getOption().matches(OPT_g))
      Opts.DebugInfoKind = IRGenDebugInfoKind::Normal;
    else if (A->getOption().matches(options::OPT_gline_tables_only))
      Opts.DebugInfoKind = IRGenDebugInfoKind::LineTables;
    else if (A->getOption().matches(options::OPT_gdwarf_types))
      Opts.DebugInfoKind = IRGenDebugInfoKind::DwarfTypes;
    else
      assert(A->getOption().matches(options::OPT_gnone) &&
             "unknown -g<kind> option");

    if (Opts.DebugInfoKind > IRGenDebugInfoKind::LineTables) {
      ArgStringList RenderedArgs;
      for (auto A : Args)
        A->render(Args, RenderedArgs);
      CompilerInvocation::buildDWARFDebugFlags(Opts.DWARFDebugFlags,
                                               RenderedArgs, SDKPath,
                                               ResourceDir);
      // TODO: Should we support -fdebug-compilation-dir?
      llvm::SmallString<256> cwd;
      llvm::sys::fs::current_path(cwd);
      Opts.DebugCompilationDir = cwd.str();
    }
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

  if (auto valueNames = Args.getLastArg(OPT_disable_llvm_value_names,
                                        OPT_enable_llvm_value_names)) {
    Opts.HasValueNamesSetting = true;
    Opts.ValueNames =
      valueNames->getOption().matches(OPT_enable_llvm_value_names);
  }

  Opts.DisableLLVMOptzns |= Args.hasArg(OPT_disable_llvm_optzns);
  Opts.DisableLLVMARCOpts |= Args.hasArg(OPT_disable_llvm_arc_opts);
  Opts.DisableLLVMSLPVectorizer |= Args.hasArg(OPT_disable_llvm_slp_vectorizer);
  if (Args.hasArg(OPT_disable_llvm_verify))
    Opts.Verify = false;

  Opts.EmitStackPromotionChecks |= Args.hasArg(OPT_stack_promotion_checks);
  if (const Arg *A = Args.getLastArg(OPT_stack_promotion_limit)) {
    unsigned limit;
    if (StringRef(A->getValue()).getAsInteger(10, limit)) {
      Diags.diagnose(SourceLoc(), diag::error_invalid_arg_value,
                     A->getAsString(Args), A->getValue());
      return true;
    }
    Opts.StackPromotionSizeLimit = limit;
  }

  if (Args.hasArg(OPT_autolink_force_load))
    Opts.ForceLoadSymbolName = Args.getLastArgValue(OPT_module_link_name);

  // TODO: investigate whether these should be removed, in favor of definitions
  // in other classes.
  if (!SILOpts.SILOutputFileNameForDebugging.empty()) {
    Opts.MainInputFilename = SILOpts.SILOutputFileNameForDebugging;
  } else if (FrontendOpts.PrimaryInput && FrontendOpts.PrimaryInput->isFilename()) {
    unsigned Index = FrontendOpts.PrimaryInput->Index;
    Opts.MainInputFilename = FrontendOpts.InputFilenames[Index];
  } else if (FrontendOpts.InputFilenames.size() == 1) {
    Opts.MainInputFilename = FrontendOpts.InputFilenames.front();
  }
  Opts.OutputFilenames = FrontendOpts.OutputFilenames;
  Opts.ModuleName = FrontendOpts.ModuleName;

  if (Args.hasArg(OPT_use_jit))
    Opts.UseJIT = true;
  
  for (const Arg *A : make_range(Args.filtered_begin(OPT_verify_type_layout),
                                 Args.filtered_end())) {
    Opts.VerifyTypeLayoutNames.push_back(A->getValue());
  }

  for (const Arg *A : make_range(Args.filtered_begin(
                                   OPT_disable_autolink_framework),
                                 Args.filtered_end())) {
    Opts.DisableAutolinkFrameworks.push_back(A->getValue());
  }

  Opts.GenerateProfile |= Args.hasArg(OPT_profile_generate);
  Opts.PrintInlineTree |= Args.hasArg(OPT_print_llvm_inline_tree);

  Opts.UseSwiftCall = Args.hasArg(OPT_enable_swiftcall);

  // This is set to true by default.
  Opts.UseIncrementalLLVMCodeGen &=
    !Args.hasArg(OPT_disable_incremental_llvm_codegeneration);

  if (Args.hasArg(OPT_embed_bitcode))
    Opts.EmbedMode = IRGenEmbedMode::EmbedBitcode;
  else if (Args.hasArg(OPT_embed_bitcode_marker))
    Opts.EmbedMode = IRGenEmbedMode::EmbedMarker;

  if (Opts.EmbedMode == IRGenEmbedMode::EmbedBitcode) {
    // Keep track of backend options so we can embed them in a separate data
    // section and use them when building from the bitcode. This can be removed
    // when all the backend options are recorded in the IR.
    for (ArgList::const_iterator A = Args.begin(), AE = Args.end();
         A != AE; ++ A) {
      // Do not encode output and input.
      if ((*A)->getOption().getID() == options::OPT_o ||
          (*A)->getOption().getID() == options::OPT_INPUT ||
          (*A)->getOption().getID() == options::OPT_primary_file ||
          (*A)->getOption().getID() == options::OPT_embed_bitcode)
        continue;
      ArgStringList ASL;
      (*A)->render(Args, ASL);
      for (ArgStringList::iterator it = ASL.begin(), ie = ASL.end();
          it != ie; ++ it) {
        StringRef ArgStr(*it);
        Opts.CmdArgs.insert(Opts.CmdArgs.end(), ArgStr.begin(), ArgStr.end());
        // using \00 to terminate to avoid problem decoding.
        Opts.CmdArgs.push_back('\0');
      }
    }
  }

  if (const Arg *A = Args.getLastArg(options::OPT_sanitize_coverage_EQ)) {
    Opts.SanitizeCoverage =
        parseSanitizerCoverageArgValue(A, Triple, Diags, Opts.Sanitize);
  }

  if (Args.hasArg(OPT_disable_reflection_metadata)) {
    Opts.EnableReflectionMetadata = false;
    Opts.EnableReflectionNames = false;
  }

  if (Args.hasArg(OPT_disable_reflection_names)) {
    Opts.EnableReflectionNames = false;
  }

  for (const auto &Lib : Args.getAllArgValues(options::OPT_autolink_library))
    Opts.LinkLibraries.push_back(LinkLibrary(Lib, LibraryKind::Library));

  return false;
}

bool CompilerInvocation::parseArgs(ArrayRef<const char *> Args,
                                   DiagnosticEngine &Diags,
                                   StringRef workingDirectory) {
  using namespace options;

  if (Args.empty())
    return false;

  // Parse frontend command line options using Swift's option table.
  unsigned MissingIndex;
  unsigned MissingCount;
  std::unique_ptr<llvm::opt::OptTable> Table = createSwiftOptTable();
  llvm::opt::InputArgList ParsedArgs =
      Table->ParseArgs(Args, MissingIndex, MissingCount, FrontendOption);
  if (MissingCount) {
    Diags.diagnose(SourceLoc(), diag::error_missing_arg_value,
                   ParsedArgs.getArgString(MissingIndex), MissingCount);
    return true;
  }

  if (ParsedArgs.hasArg(OPT_UNKNOWN)) {
    for (const Arg *A : make_range(ParsedArgs.filtered_begin(OPT_UNKNOWN),
                                   ParsedArgs.filtered_end())) {
      Diags.diagnose(SourceLoc(), diag::error_unknown_arg,
                     A->getAsString(ParsedArgs));
    }
    return true;
  }

  if (ParseFrontendArgs(FrontendOpts, ParsedArgs, Diags)) {
    return true;
  }

  if (ParseLangArgs(LangOpts, ParsedArgs, Diags, FrontendOpts)) {
    return true;
  }

  if (ParseClangImporterArgs(ClangImporterOpts, ParsedArgs, Diags,
                             workingDirectory)) {
    return true;
  }

  if (ParseSearchPathArgs(SearchPathOpts, ParsedArgs, Diags,
                          workingDirectory)) {
    return true;
  }

  if (ParseSILArgs(SILOpts, ParsedArgs, IRGenOpts, FrontendOpts, Diags,
                   LangOpts.Target)) {
    return true;
  }

  if (ParseIRGenArgs(IRGenOpts, ParsedArgs, Diags, FrontendOpts, SILOpts,
                     getSDKPath(), SearchPathOpts.RuntimeResourcePath,
                     LangOpts.Target)) {
    return true;
  }

  if (ParseDiagnosticArgs(DiagnosticOpts, ParsedArgs, Diags)) {
    return true;
  }

  updateRuntimeLibraryPath(SearchPathOpts, LangOpts.Target);

  return false;
}


serialization::Status
CompilerInvocation::loadFromSerializedAST(StringRef data) {
  serialization::ExtendedValidationInfo extendedInfo;
  serialization::ValidationInfo info =
      serialization::validateSerializedAST(data, &extendedInfo);

  if (info.status != serialization::Status::Valid)
    return info.status;

  setTargetTriple(info.targetTriple);
  if (!extendedInfo.getSDKPath().empty())
    setSDKPath(extendedInfo.getSDKPath());

  auto &extraClangArgs = getClangImporterOptions().ExtraArgs;
  extraClangArgs.insert(extraClangArgs.end(),
                        extendedInfo.getExtraClangImporterOptions().begin(),
                        extendedInfo.getExtraClangImporterOptions().end());
  return info.status;
}
