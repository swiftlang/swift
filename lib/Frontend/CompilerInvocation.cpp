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
#include "swift/AST/DiagnosticsFrontend.h"
#include "swift/Basic/Platform.h"
#include "swift/Option/Options.h"
#include "swift/Option/SanitizerOptions.h"
#include "swift/Strings.h"
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

namespace swift {

/// Implement argument semantics in a way that will make it easier to have
/// >1 primary file (or even a primary file list) in the future without
/// breaking anything today.
///
/// Semantics today:
/// If input files are on command line, primary files on command line are also
/// input files; they are not repeated without -primary-file. If input files are
/// in a file list, the primary files on the command line are repeated in the
/// file list. Thus, if there are any primary files, it is illegal to have both
/// (non-primary) input files and a file list. Finally, the order of input files
/// must match the order given on the command line or the file list.
///
/// Side note:
/// since each input file will cause a lot of work for the compiler, this code
/// is biased towards clarity and not optimized.
/// In the near future, it will be possible to put primary files in the
/// filelist, or to have a separate filelist for primaries. The organization
/// here anticipates that evolution.

class ArgsToFrontendInputsConverter {
  DiagnosticEngine &Diags;
  const ArgList &Args;
  FrontendInputs &Inputs;

  Arg const *const FilelistPathArg;

  std::unique_ptr<llvm::MemoryBuffer> FilelistBuffer;

  llvm::StringMap<unsigned> FileIndices;
  std::vector<StringRef> PrimaryFiles;

  StringRef filelistPath() { return FilelistPathArg->getValue(); }

  void addPrimary(StringRef file) { PrimaryFiles.push_back(file); }

  void addInput(StringRef file) {
    FileIndices.insert({file, Inputs.inputFilenameCount()});
    Inputs.addInputFilename(file);
  }

  bool arePrimariesOnCommandLineAlsoAppearingInFilelist() {
    return FilelistPathArg != nullptr;
  }

  enum class Whence {
    PrimaryFromCommandLine,
    SecondaryFromCommandLine,
    SecondaryFromFileList
  };

  void addFile(StringRef file, Whence whence) {
    switch (whence) {
    case Whence::PrimaryFromCommandLine:
      addPrimary(file);
      if (!arePrimariesOnCommandLineAlsoAppearingInFilelist())
        addInput(file);
      break;
    case Whence::SecondaryFromCommandLine:
    case Whence::SecondaryFromFileList:
      addInput(file);
      break;
    }
  }

public:
  ArgsToFrontendInputsConverter(DiagnosticEngine &Diags, const ArgList &Args,
                                FrontendInputs &Inputs)
      : Diags(Diags), Args(Args), Inputs(Inputs),
        FilelistPathArg(Args.getLastArg(options::OPT_filelist)) {}

  bool convert() {
    if (enforceFilelistExclusion())
      return true;
    getFilesFromCommandLine();
    if (getFilesFromFilelist())
      return true;
    return setPrimaryFiles();
  }

private:
  bool enforceFilelistExclusion() {
    if (Args.hasArg(options::OPT_INPUT) && FilelistPathArg != nullptr) {
      Diags.diagnose(SourceLoc(),
                     diag::error_cannot_have_input_files_with_file_list);
      return true;
    }
    return false;
  }
  void getFilesFromCommandLine() {
    for (const Arg *A :
         Args.filtered(options::OPT_INPUT, options::OPT_primary_file)) {
      StringRef file = A->getValue();
      if (A->getOption().matches(options::OPT_INPUT))
        addFile(file, Whence::SecondaryFromCommandLine);
      else if (A->getOption().matches(options::OPT_primary_file))
        addFile(file, Whence::PrimaryFromCommandLine);
      else
        llvm_unreachable("Unknown input-related argument!");
    }
  }

  bool getFilesFromFilelist() {
    if (FilelistPathArg == nullptr)
      return false;
    llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>> filelistBufferOrError =
        llvm::MemoryBuffer::getFile(filelistPath());
    if (!filelistBufferOrError) {
      Diags.diagnose(SourceLoc(), diag::cannot_open_file, filelistPath(),
                     filelistBufferOrError.getError().message());
      return true;
    }
    // Keep buffer alive because code passes around StringRefs.
    FilelistBuffer = std::move(*filelistBufferOrError);
    for (auto file : llvm::make_range(llvm::line_iterator(*FilelistBuffer),
                                      llvm::line_iterator())) {
      addFile(file, Whence::SecondaryFromFileList);
    }
    return false;
  }

  bool setPrimaryFiles() {
    for (StringRef primaryFile : PrimaryFiles) {
      const auto iterator = FileIndices.find(primaryFile);
      // Catch "swiftc -frontend -c -filelist foo -primary-file
      // some-file-not-in-foo".
      if (iterator == FileIndices.end()) {
        assert(FilelistPathArg != nullptr &&
               "Missing primary with no filelist");
        Diags.diagnose(SourceLoc(), diag::error_primary_file_not_found,
                       primaryFile, filelistPath());
        return true;
      }
      Inputs.addPrimaryInputFilename(iterator->second);
    }
    return false;
  }
};
} // namespace swift

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

  if (const Arg *A = Args.getLastArg(OPT_index_store_path)) {
    Opts.IndexStorePath = A->getValue();
  }
  Opts.IndexSystemModules |= Args.hasArg(OPT_index_system_modules);

  Opts.EmitVerboseSIL |= Args.hasArg(OPT_emit_verbose_sil);
  Opts.EmitSortedSIL |= Args.hasArg(OPT_emit_sorted_sil);

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
    if (Args.getLastArg(OPT_trace_stats_events)) {
      Opts.TraceStats = true;
    }
  }

  if (const Arg *A = Args.getLastArg(OPT_validate_tbd_against_ir_EQ)) {
    using Mode = FrontendOptions::TBDValidationMode;
    StringRef value = A->getValue();
    if (value == "none") {
      Opts.ValidateTBDAgainstIR = Mode::None;
    } else if (value == "missing") {
      Opts.ValidateTBDAgainstIR = Mode::MissingFromTBD;
    } else if (value == "all") {
      Opts.ValidateTBDAgainstIR = Mode::All;
    } else {
      Diags.diagnose(SourceLoc(), diag::error_unsupported_option_argument,
                     A->getOption().getPrefixedName(), value);
    }
  }

  if (const Arg *A = Args.getLastArg(OPT_tbd_install_name)) {
    Opts.TBDInstallName = A->getValue();
  }

  if (const Arg *A = Args.getLastArg(OPT_warn_long_function_bodies)) {
    unsigned attempt;
    if (StringRef(A->getValue()).getAsInteger(10, attempt)) {
      Diags.diagnose(SourceLoc(), diag::error_invalid_arg_value,
                     A->getAsString(Args), A->getValue());
    } else {
      Opts.WarnLongFunctionBodies = attempt;
    }
  }

  if (const Arg *A = Args.getLastArg(OPT_warn_long_expression_type_checking)) {
    unsigned attempt;
    if (StringRef(A->getValue()).getAsInteger(10, attempt)) {
      Diags.diagnose(SourceLoc(), diag::error_invalid_arg_value,
                     A->getAsString(Args), A->getValue());
    } else {
      Opts.WarnLongExpressionTypeChecking = attempt;
    }
  }

  if (const Arg *A = Args.getLastArg(OPT_solver_expression_time_threshold_EQ)) {
    unsigned attempt;
    if (StringRef(A->getValue()).getAsInteger(10, attempt)) {
      Diags.diagnose(SourceLoc(), diag::error_invalid_arg_value,
                     A->getAsString(Args), A->getValue());
    } else {
      Opts.SolverExpressionTimeThreshold = attempt;
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

  if (ArgsToFrontendInputsConverter(Diags, Args, Opts.Inputs).convert())
    return true;

  Opts.ParseStdlib |= Args.hasArg(OPT_parse_stdlib);

  if (const Arg *A = Args.getLastArg(OPT_verify_generic_signatures)) {
    Opts.VerifyGenericSignaturesInModule = A->getValue();
  }

  // Determine what the user has asked the frontend to do.
  FrontendOptions::ActionType &Action = Opts.RequestedAction;
  if (const Arg *A = Args.getLastArg(OPT_modes_Group)) {
    Option Opt = A->getOption();
    if (Opt.matches(OPT_emit_object)) {
      Action = FrontendOptions::ActionType::EmitObject;
    } else if (Opt.matches(OPT_emit_assembly)) {
      Action = FrontendOptions::ActionType::EmitAssembly;
    } else if (Opt.matches(OPT_emit_ir)) {
      Action = FrontendOptions::ActionType::EmitIR;
    } else if (Opt.matches(OPT_emit_bc)) {
      Action = FrontendOptions::ActionType::EmitBC;
    } else if (Opt.matches(OPT_emit_sil)) {
      Action = FrontendOptions::ActionType::EmitSIL;
    } else if (Opt.matches(OPT_emit_silgen)) {
      Action = FrontendOptions::ActionType::EmitSILGen;
    } else if (Opt.matches(OPT_emit_sib)) {
      Action = FrontendOptions::ActionType::EmitSIB;
    } else if (Opt.matches(OPT_emit_sibgen)) {
      Action = FrontendOptions::ActionType::EmitSIBGen;
    } else if (Opt.matches(OPT_emit_pch)) {
      Action = FrontendOptions::ActionType::EmitPCH;
    } else if (Opt.matches(OPT_emit_imported_modules)) {
      Action = FrontendOptions::ActionType::EmitImportedModules;
    } else if (Opt.matches(OPT_parse)) {
      Action = FrontendOptions::ActionType::Parse;
    } else if (Opt.matches(OPT_typecheck)) {
      Action = FrontendOptions::ActionType::Typecheck;
    } else if (Opt.matches(OPT_dump_parse)) {
      Action = FrontendOptions::ActionType::DumpParse;
    } else if (Opt.matches(OPT_dump_ast)) {
      Action = FrontendOptions::ActionType::DumpAST;
    } else if (Opt.matches(OPT_emit_syntax)) {
      Action = FrontendOptions::ActionType::EmitSyntax;
    } else if (Opt.matches(OPT_merge_modules)) {
      Action = FrontendOptions::ActionType::MergeModules;
    } else if (Opt.matches(OPT_dump_scope_maps)) {
      Action = FrontendOptions::ActionType::DumpScopeMaps;

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
      Action = FrontendOptions::ActionType::DumpTypeRefinementContexts;
    } else if (Opt.matches(OPT_dump_interface_hash)) {
      Action = FrontendOptions::ActionType::DumpInterfaceHash;
    } else if (Opt.matches(OPT_print_ast)) {
      Action = FrontendOptions::ActionType::PrintAST;
    } else if (Opt.matches(OPT_repl) ||
               Opt.matches(OPT_deprecated_integrated_repl)) {
      Action = FrontendOptions::ActionType::REPL;
    } else if (Opt.matches(OPT_interpret)) {
      Action = FrontendOptions::ActionType::Immediate;
    } else {
      llvm_unreachable("Unhandled mode option");
    }
  } else {
    // We don't have a mode, so determine a default.
    if (Args.hasArg(OPT_emit_module, OPT_emit_module_path)) {
      // We've been told to emit a module, but have no other mode indicators.
      // As a result, put the frontend into EmitModuleOnly mode.
      // (Setting up module output will be handled below.)
      Action = FrontendOptions::ActionType::EmitModuleOnly;
    }
  }

  if (Opts.RequestedAction == FrontendOptions::ActionType::Immediate &&
      Opts.Inputs.havePrimaryInputs()) {
    Diags.diagnose(SourceLoc(), diag::error_immediate_mode_primary_file);
    return true;
  }

  bool TreatAsSIL =
      Args.hasArg(OPT_parse_sil) || Opts.Inputs.shouldTreatAsSIL();

  bool TreatAsLLVM = Opts.Inputs.shouldTreatAsLLVM();

  if (Opts.Inputs.verifyInputs(
          Diags, TreatAsSIL,
          Opts.RequestedAction == FrontendOptions::ActionType::REPL,
          Opts.RequestedAction == FrontendOptions::ActionType::NoneAction)) {
    return true;
  }

  if (Opts.RequestedAction == FrontendOptions::ActionType::Immediate) {
    Opts.ImmediateArgv.push_back(
        Opts.Inputs.getFilenameOfFirstInput()); // argv[0]
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
  else if (Action == FrontendOptions::ActionType::REPL)
    Opts.InputKind = InputFileKind::IFK_Swift_REPL;
  else
    Opts.InputKind = InputFileKind::IFK_Swift;

  Opts.setOutputFileList(Diags, Args);

  Opts.setModuleName(Diags, Args);

  if (Opts.OutputFilenames.empty() ||
      llvm::sys::fs::is_directory(Opts.getSingleOutputFilename())) {
    // No output filename was specified, or an output directory was specified.
    // Determine the correct output filename.

    // Note: this should typically only be used when invoking the frontend
    // directly, as the driver will always pass -o with an appropriate filename
    // if output is required for the requested action.

    StringRef Suffix;
    switch (Opts.RequestedAction) {
    case FrontendOptions::ActionType::NoneAction:
      break;

    case FrontendOptions::ActionType::Parse:
    case FrontendOptions::ActionType::Typecheck:
    case FrontendOptions::ActionType::DumpParse:
    case FrontendOptions::ActionType::DumpInterfaceHash:
    case FrontendOptions::ActionType::DumpAST:
    case FrontendOptions::ActionType::EmitSyntax:
    case FrontendOptions::ActionType::PrintAST:
    case FrontendOptions::ActionType::DumpScopeMaps:
    case FrontendOptions::ActionType::DumpTypeRefinementContexts:
      // Textual modes.
      Opts.setOutputFilenameToStdout();
      break;

    case FrontendOptions::ActionType::EmitPCH:
      Suffix = PCH_EXTENSION;
      break;

    case FrontendOptions::ActionType::EmitSILGen:
    case FrontendOptions::ActionType::EmitSIL: {
      if (Opts.OutputFilenames.empty())
        Opts.setOutputFilenameToStdout();
      else
        Suffix = SIL_EXTENSION;
      break;
    }

    case FrontendOptions::ActionType::EmitSIBGen:
    case FrontendOptions::ActionType::EmitSIB:
      Suffix = SIB_EXTENSION;
      break;

    case FrontendOptions::ActionType::MergeModules:
    case FrontendOptions::ActionType::EmitModuleOnly:
      Suffix = SERIALIZED_MODULE_EXTENSION;
      break;

    case FrontendOptions::ActionType::Immediate:
    case FrontendOptions::ActionType::REPL:
      // These modes have no frontend-generated output.
      Opts.OutputFilenames.clear();
      break;

    case FrontendOptions::ActionType::EmitAssembly: {
      if (Opts.OutputFilenames.empty())
        Opts.setOutputFilenameToStdout();
      else
        Suffix = "s";
      break;
    }

    case FrontendOptions::ActionType::EmitIR: {
      if (Opts.OutputFilenames.empty())
        Opts.setOutputFilenameToStdout();
      else
        Suffix = "ll";
      break;
    }

    case FrontendOptions::ActionType::EmitBC: {
      Suffix = "bc";
      break;
    }

    case FrontendOptions::ActionType::EmitObject:
      Suffix = "o";
      break;

    case FrontendOptions::ActionType::EmitImportedModules:
      if (Opts.OutputFilenames.empty())
        Opts.setOutputFilenameToStdout();
      else
        Suffix = "importedmodules";
      break;
    }

    if (!Suffix.empty()) {
      // We need to deduce a file name.

      // First, if we're reading from stdin and we don't have a directory,
      // output to stdout.
      if (Opts.Inputs.isReadingFromStdin() && Opts.OutputFilenames.empty())
        Opts.setOutputFilenameToStdout();
      else {
        // We have a suffix, so determine an appropriate name.
        StringRef BaseName =
            Opts.Inputs.baseNameOfOutput(Args, Opts.ModuleName);
        llvm::SmallString<128> Path(Opts.getSingleOutputFilename());
        llvm::sys::path::append(Path, BaseName);
        llvm::sys::path::replace_extension(Path, Suffix);

        Opts.setSingleOutputFilename(Path.str());
      }
    }

    if (Opts.OutputFilenames.empty()) {
      if (Opts.RequestedAction != FrontendOptions::ActionType::REPL &&
          Opts.RequestedAction != FrontendOptions::ActionType::Immediate &&
          Opts.RequestedAction != FrontendOptions::ActionType::NoneAction) {
        Diags.diagnose(SourceLoc(), diag::error_no_output_filename_specified);
        return true;
      }
    } else if (Opts.isOutputFileDirectory()) {
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

    llvm::SmallString<128> Path(Opts.originalPath());
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
  determineOutputFilename(Opts.LoadedModuleTracePath,
                          OPT_emit_loaded_module_trace,
                          OPT_emit_loaded_module_trace_path,
                          "trace.json", false);

  determineOutputFilename(Opts.TBDPath, OPT_emit_tbd, OPT_emit_tbd_path, "tbd",
                          false);

  if (const Arg *A = Args.getLastArg(OPT_emit_fixits_path)) {
    Opts.FixitsOutputPath = A->getValue();
  }

  bool IsSIB = Opts.RequestedAction == FrontendOptions::ActionType::EmitSIB ||
               Opts.RequestedAction == FrontendOptions::ActionType::EmitSIBGen;
  bool canUseMainOutputForModule =
      Opts.RequestedAction == FrontendOptions::ActionType::MergeModules ||
      Opts.RequestedAction == FrontendOptions::ActionType::EmitModuleOnly ||
      IsSIB;
  auto ext = IsSIB ? SIB_EXTENSION : SERIALIZED_MODULE_EXTENSION;
  auto sibOpt = Opts.RequestedAction == FrontendOptions::ActionType::EmitSIB
                    ? OPT_emit_sib
                    : OPT_emit_sibgen;
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
    case FrontendOptions::ActionType::NoneAction:
    case FrontendOptions::ActionType::DumpParse:
    case FrontendOptions::ActionType::DumpInterfaceHash:
    case FrontendOptions::ActionType::DumpAST:
    case FrontendOptions::ActionType::EmitSyntax:
    case FrontendOptions::ActionType::PrintAST:
    case FrontendOptions::ActionType::DumpScopeMaps:
    case FrontendOptions::ActionType::DumpTypeRefinementContexts:
    case FrontendOptions::ActionType::Immediate:
    case FrontendOptions::ActionType::REPL:
      Diags.diagnose(SourceLoc(), diag::error_mode_cannot_emit_dependencies);
      return true;
    case FrontendOptions::ActionType::Parse:
    case FrontendOptions::ActionType::Typecheck:
    case FrontendOptions::ActionType::MergeModules:
    case FrontendOptions::ActionType::EmitModuleOnly:
    case FrontendOptions::ActionType::EmitPCH:
    case FrontendOptions::ActionType::EmitSILGen:
    case FrontendOptions::ActionType::EmitSIL:
    case FrontendOptions::ActionType::EmitSIBGen:
    case FrontendOptions::ActionType::EmitSIB:
    case FrontendOptions::ActionType::EmitIR:
    case FrontendOptions::ActionType::EmitBC:
    case FrontendOptions::ActionType::EmitAssembly:
    case FrontendOptions::ActionType::EmitObject:
    case FrontendOptions::ActionType::EmitImportedModules:
      break;
    }
  }

  if (!Opts.ObjCHeaderOutputPath.empty()) {
    switch (Opts.RequestedAction) {
    case FrontendOptions::ActionType::NoneAction:
    case FrontendOptions::ActionType::DumpParse:
    case FrontendOptions::ActionType::DumpInterfaceHash:
    case FrontendOptions::ActionType::DumpAST:
    case FrontendOptions::ActionType::EmitSyntax:
    case FrontendOptions::ActionType::PrintAST:
    case FrontendOptions::ActionType::EmitPCH:
    case FrontendOptions::ActionType::DumpScopeMaps:
    case FrontendOptions::ActionType::DumpTypeRefinementContexts:
    case FrontendOptions::ActionType::Immediate:
    case FrontendOptions::ActionType::REPL:
      Diags.diagnose(SourceLoc(), diag::error_mode_cannot_emit_header);
      return true;
    case FrontendOptions::ActionType::Parse:
    case FrontendOptions::ActionType::Typecheck:
    case FrontendOptions::ActionType::MergeModules:
    case FrontendOptions::ActionType::EmitModuleOnly:
    case FrontendOptions::ActionType::EmitSILGen:
    case FrontendOptions::ActionType::EmitSIL:
    case FrontendOptions::ActionType::EmitSIBGen:
    case FrontendOptions::ActionType::EmitSIB:
    case FrontendOptions::ActionType::EmitIR:
    case FrontendOptions::ActionType::EmitBC:
    case FrontendOptions::ActionType::EmitAssembly:
    case FrontendOptions::ActionType::EmitObject:
    case FrontendOptions::ActionType::EmitImportedModules:
      break;
    }
  }

  if (!Opts.LoadedModuleTracePath.empty()) {
    switch (Opts.RequestedAction) {
    case FrontendOptions::ActionType::NoneAction:
    case FrontendOptions::ActionType::Parse:
    case FrontendOptions::ActionType::DumpParse:
    case FrontendOptions::ActionType::DumpInterfaceHash:
    case FrontendOptions::ActionType::DumpAST:
    case FrontendOptions::ActionType::EmitSyntax:
    case FrontendOptions::ActionType::PrintAST:
    case FrontendOptions::ActionType::DumpScopeMaps:
    case FrontendOptions::ActionType::DumpTypeRefinementContexts:
    case FrontendOptions::ActionType::Immediate:
    case FrontendOptions::ActionType::REPL:
      Diags.diagnose(SourceLoc(),
                     diag::error_mode_cannot_emit_loaded_module_trace);
      return true;
    case FrontendOptions::ActionType::Typecheck:
    case FrontendOptions::ActionType::MergeModules:
    case FrontendOptions::ActionType::EmitModuleOnly:
    case FrontendOptions::ActionType::EmitPCH:
    case FrontendOptions::ActionType::EmitSILGen:
    case FrontendOptions::ActionType::EmitSIL:
    case FrontendOptions::ActionType::EmitSIBGen:
    case FrontendOptions::ActionType::EmitSIB:
    case FrontendOptions::ActionType::EmitIR:
    case FrontendOptions::ActionType::EmitBC:
    case FrontendOptions::ActionType::EmitAssembly:
    case FrontendOptions::ActionType::EmitObject:
    case FrontendOptions::ActionType::EmitImportedModules:
      break;
    }
  }

  if (!Opts.ModuleOutputPath.empty() ||
      !Opts.ModuleDocOutputPath.empty()) {
    switch (Opts.RequestedAction) {
    case FrontendOptions::ActionType::NoneAction:
    case FrontendOptions::ActionType::Parse:
    case FrontendOptions::ActionType::Typecheck:
    case FrontendOptions::ActionType::DumpParse:
    case FrontendOptions::ActionType::DumpInterfaceHash:
    case FrontendOptions::ActionType::DumpAST:
    case FrontendOptions::ActionType::EmitSyntax:
    case FrontendOptions::ActionType::PrintAST:
    case FrontendOptions::ActionType::EmitPCH:
    case FrontendOptions::ActionType::DumpScopeMaps:
    case FrontendOptions::ActionType::DumpTypeRefinementContexts:
    case FrontendOptions::ActionType::EmitSILGen:
    case FrontendOptions::ActionType::Immediate:
    case FrontendOptions::ActionType::REPL:
      if (!Opts.ModuleOutputPath.empty())
        Diags.diagnose(SourceLoc(), diag::error_mode_cannot_emit_module);
      else
        Diags.diagnose(SourceLoc(), diag::error_mode_cannot_emit_module_doc);
      return true;
    case FrontendOptions::ActionType::MergeModules:
    case FrontendOptions::ActionType::EmitModuleOnly:
    case FrontendOptions::ActionType::EmitSIL:
    case FrontendOptions::ActionType::EmitSIBGen:
    case FrontendOptions::ActionType::EmitSIB:
    case FrontendOptions::ActionType::EmitIR:
    case FrontendOptions::ActionType::EmitBC:
    case FrontendOptions::ActionType::EmitAssembly:
    case FrontendOptions::ActionType::EmitObject:
    case FrontendOptions::ActionType::EmitImportedModules:
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
  Opts.EnableSerializationNestedTypeLookupTable &=
      !Args.hasArg(OPT_disable_serialization_nested_type_lookup_table);

  if (const Arg *A = Args.getLastArgNoClaim(OPT_import_objc_header)) {
    Opts.ImplicitObjCHeaderPath = A->getValue();
    Opts.SerializeBridgingHeader |=
        !Opts.Inputs.havePrimaryInputs() && !Opts.ModuleOutputPath.empty();
  }

  for (const Arg *A : Args.filtered(OPT_import_module)) {
    Opts.ImplicitImportModuleNames.push_back(A->getValue());
  }

  for (const Arg *A : Args.filtered(OPT_Xllvm)) {
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

/// \brief Create a new Regex instance out of the string value in \p RpassArg.
/// It returns a pointer to the newly generated Regex instance.
static std::shared_ptr<llvm::Regex>
generateOptimizationRemarkRegex(DiagnosticEngine &Diags, ArgList &Args,
                                Arg *RpassArg) {
  StringRef Val = RpassArg->getValue();
  std::string RegexError;
  std::shared_ptr<llvm::Regex> Pattern = std::make_shared<llvm::Regex>(Val);
  if (!Pattern->isValid(RegexError)) {
    Diags.diagnose(SourceLoc(), diag::error_optimization_remark_pattern,
                   RegexError, RpassArg->getAsString(Args));
    Pattern.reset();
  }
  return Pattern;
}

static bool ParseLangArgs(LangOptions &Opts, ArgList &Args,
                          DiagnosticEngine &Diags,
                          const FrontendOptions &FrontendOpts) {
  using namespace options;

  /// FIXME: Remove this flag when void subscripts are implemented.
  /// This is used to guard preemptive testing for the fix-it.
  if (Args.hasArg(OPT_fix_string_substring_conversion)) {
    Opts.FixStringToSubstringConversions = true;
  }

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

  Opts.DiagnosticsEditorMode |= Args.hasArg(OPT_diagnostics_editor_mode,
                                            OPT_serialize_diagnostics_path);

  Opts.EnableExperimentalPropertyBehaviors |=
    Args.hasArg(OPT_enable_experimental_property_behaviors);

  Opts.EnableClassResilience |=
    Args.hasArg(OPT_enable_class_resilience);

  if (auto A = Args.getLastArg(OPT_enable_deserialization_recovery,
                               OPT_disable_deserialization_recovery)) {
    Opts.EnableDeserializationRecovery
      = A->getOption().matches(OPT_enable_deserialization_recovery);
  }

  Opts.DisableAvailabilityChecking |=
      Args.hasArg(OPT_disable_availability_checking);

  Opts.DisableTsanInoutInstrumentation |=
      Args.hasArg(OPT_disable_tsan_inout_instrumentation);

  if (FrontendOpts.InputKind == InputFileKind::IFK_SIL)
    Opts.DisableAvailabilityChecking = true;
  
  if (auto A = Args.getLastArg(OPT_enable_access_control,
                               OPT_disable_access_control)) {
    Opts.EnableAccessControl
      = A->getOption().matches(OPT_enable_access_control);
  }

  if (auto A = Args.getLastArg(OPT_disable_typo_correction,
                               OPT_typo_correction_limit)) {
    if (A->getOption().matches(OPT_disable_typo_correction))
      Opts.TypoCorrectionLimit = 0;
    else {
      unsigned limit;
      if (StringRef(A->getValue()).getAsInteger(10, limit)) {
        Diags.diagnose(SourceLoc(), diag::error_invalid_arg_value,
                       A->getAsString(Args), A->getValue());
        return true;
      }

      Opts.TypoCorrectionLimit = limit;
    }
  }

  Opts.CodeCompleteInitsInPostfixExpr |=
      Args.hasArg(OPT_code_complete_inits_in_postfix_expr);

  Opts.CodeCompleteCallPatternHeuristics |=
      Args.hasArg(OPT_code_complete_call_pattern_heuristics);

  if (auto A = Args.getLastArg(OPT_enable_target_os_checking,
                               OPT_disable_target_os_checking)) {
    Opts.EnableTargetOSChecking
      = A->getOption().matches(OPT_enable_target_os_checking);
  }
  
  Opts.EnableASTScopeLookup |= Args.hasArg(OPT_enable_astscope_lookup);
  Opts.DebugConstraintSolver |= Args.hasArg(OPT_debug_constraints);
  Opts.IterativeTypeChecker |= Args.hasArg(OPT_iterative_type_checker);
  Opts.NamedLazyMemberLoading &= !Args.hasArg(OPT_disable_named_lazy_member_loading);
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

  if (const Arg *A = Args.getLastArg(OPT_solver_shrink_unsolved_threshold)) {
    unsigned threshold;
    if (StringRef(A->getValue()).getAsInteger(10, threshold)) {
      Diags.diagnose(SourceLoc(), diag::error_invalid_arg_value,
                     A->getAsString(Args), A->getValue());
      return true;
    }

    Opts.SolverShrinkUnsolvedThreshold = threshold;
  }

  if (const Arg *A = Args.getLastArg(OPT_value_recursion_threshold)) {
    unsigned threshold;
    if (StringRef(A->getValue()).getAsInteger(10, threshold)) {
      Diags.diagnose(SourceLoc(), diag::error_invalid_arg_value,
                     A->getAsString(Args), A->getValue());
      return true;
    }

    Opts.MaxCircularityDepth = threshold;
  }
  
  for (const Arg *A : Args.filtered(OPT_D)) {
    Opts.addCustomConditionalCompilationFlag(A->getValue());
  }

  Opts.EnableAppExtensionRestrictions |= Args.hasArg(OPT_enable_app_extension);

  Opts.EnableSwift3ObjCInference =
    Args.hasFlag(OPT_enable_swift3_objc_inference,
                 OPT_disable_swift3_objc_inference,
                 Opts.isSwiftVersion3());

  if (Opts.EnableSwift3ObjCInference) {
    if (const Arg *A = Args.getLastArg(
                                   OPT_warn_swift3_objc_inference_minimal,
                                   OPT_warn_swift3_objc_inference_complete)) {
      if (A->getOption().getID() == OPT_warn_swift3_objc_inference_minimal)
        Opts.WarnSwift3ObjCInference = Swift3ObjCInferenceWarnings::Minimal;
      else
        Opts.WarnSwift3ObjCInference = Swift3ObjCInferenceWarnings::Complete;
    }
  }

  Opts.EnableNSKeyedArchiverDiagnostics =
      Args.hasFlag(OPT_enable_nskeyedarchiver_diagnostics,
                   OPT_disable_nskeyedarchiver_diagnostics,
                   Opts.EnableNSKeyedArchiverDiagnostics);

  if (Arg *A = Args.getLastArg(OPT_Rpass_EQ))
    Opts.OptimizationRemarkPassedPattern =
        generateOptimizationRemarkRegex(Diags, Args, A);
  if (Arg *A = Args.getLastArg(OPT_Rpass_missed_EQ))
    Opts.OptimizationRemarkMissedPattern =
        generateOptimizationRemarkRegex(Diags, Args, A);

  llvm::Triple Target = Opts.Target;
  StringRef TargetArg;
  if (const Arg *A = Args.getLastArg(OPT_target)) {
    Target = llvm::Triple(A->getValue());
    TargetArg = A->getValue();
  }

  Opts.EnableObjCInterop =
      Args.hasFlag(OPT_enable_objc_interop, OPT_disable_objc_interop,
                   Target.isOSDarwin());
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

  if (const Arg *A = Args.getLastArg(OPT_index_store_path))
    Opts.IndexStorePath = A->getValue();

  for (const Arg *A : Args.filtered(OPT_Xcc)) {
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
    Opts.PCHDisableValidation |= Args.hasArg(OPT_pch_disable_validation);
  }

  Opts.DebuggerSupport |= Args.hasArg(OPT_debugger_support);
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

  for (const Arg *A : Args.filtered(OPT_I)) {
    Opts.ImportSearchPaths.push_back(resolveSearchPath(A->getValue()));
  }

  for (const Arg *A : Args.filtered(OPT_F, OPT_Fsystem)) {
    Opts.FrameworkSearchPaths.push_back({resolveSearchPath(A->getValue()),
                           /*isSystem=*/A->getOption().getID() == OPT_Fsystem});
  }

  for (const Arg *A : Args.filtered(OPT_L)) {
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
static void PrintArg(raw_ostream &OS, const char *Arg, StringRef TempDir) {
  const bool Escape = std::strpbrk(Arg, "\"\\$ ");

  if (StringRef(Arg).startswith(TempDir)) {
    // Don't write temporary file names in the debug info. This would prevent
    // incremental llvm compilation because we would generate different IR on
    // every compiler invocation.
    Arg = "<temporary-file>";
  }

  if (!Escape) {
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
  if (Opts.shouldOptimize() && Opts.EnforceExclusivityDynamic) {
    Diags.diagnose(SourceLoc(),
                   diag::warning_argument_not_supported_with_optimization,
                   A->getOption().getPrefixedName() + A->getValue());
  }
}

static bool ParseSILArgs(SILOptions &Opts, ArgList &Args,
                         IRGenOptions &IRGenOpts,
                         FrontendOptions &FEOpts,
                         DiagnosticEngine &Diags,
                         const llvm::Triple &Triple,
                         ClangImporterOptions &ClangOpts) {
  using namespace options;

  if (const Arg *A = Args.getLastArg(OPT_sil_inline_threshold)) {
    if (StringRef(A->getValue()).getAsInteger(10, Opts.InlineThreshold)) {
      Diags.diagnose(SourceLoc(), diag::error_invalid_arg_value,
                     A->getAsString(Args), A->getValue());
      return true;
    }
  }
  if (const Arg *A = Args.getLastArg(OPT_sil_inline_caller_benefit_reduction_factor)) {
    if (StringRef(A->getValue()).getAsInteger(10, Opts.CallerBaseBenefitReductionFactor)) {
      Diags.diagnose(SourceLoc(), diag::error_invalid_arg_value,
                     A->getAsString(Args), A->getValue());
      return true;
    }
  }
  if (const Arg *A = Args.getLastArg(OPT_sil_unroll_threshold)) {
    if (StringRef(A->getValue()).getAsInteger(10, Opts.UnrollThreshold)) {
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
  // Default to Onone settings if no option is passed.
  Opts.OptMode = OptimizationMode::NoOptimization;
  if (const Arg *A = Args.getLastArg(OPT_O_Group)) {
    if (A->getOption().matches(OPT_Onone)) {
      // Already set.
    } else if (A->getOption().matches(OPT_Ounchecked)) {
      // Turn on optimizations and remove all runtime checks.
      Opts.OptMode = OptimizationMode::ForSpeed;
      // Removal of cond_fail (overflow on binary operations).
      Opts.RemoveRuntimeAsserts = true;
      Opts.AssertConfig = SILOptions::Unchecked;
    } else if (A->getOption().matches(OPT_Oplayground)) {
      // For now -Oplayground is equivalent to -Onone.
      Opts.OptMode = OptimizationMode::NoOptimization;
    } else if (A->getOption().matches(OPT_Osize)) {
      Opts.OptMode = OptimizationMode::ForSize;
    } else {
      assert(A->getOption().matches(OPT_O));
      Opts.OptMode = OptimizationMode::ForSpeed;
    }

    if (Opts.shouldOptimize()) {
      ClangOpts.Optimization = "-Os";
    }
  }
  IRGenOpts.OptMode = Opts.OptMode;

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
        (IRGenOpts.shouldOptimize() ? SILOptions::Release : SILOptions::Debug);
  }

  // -Ounchecked might also set removal of runtime asserts (cond_fail).
  Opts.RemoveRuntimeAsserts |= Args.hasArg(OPT_RemoveRuntimeAsserts);

  Opts.EnableARCOptimizations |= !Args.hasArg(OPT_disable_arc_opts);
  Opts.DisableSILPerfOptimizations |= Args.hasArg(OPT_disable_sil_perf_optzns);
  Opts.VerifyAll |= Args.hasArg(OPT_sil_verify_all);
  Opts.DebugSerialization |= Args.hasArg(OPT_sil_debug_serialization);
  Opts.EmitVerboseSIL |= Args.hasArg(OPT_emit_verbose_sil);
  Opts.PrintInstCounts |= Args.hasArg(OPT_print_inst_counts);
  if (const Arg *A = Args.getLastArg(OPT_external_pass_pipeline_filename))
    Opts.ExternalPassPipelineFilename = A->getValue();

  Opts.GenerateProfile |= Args.hasArg(OPT_profile_generate);
  const Arg *ProfileUse = Args.getLastArg(OPT_profile_use);
  Opts.UseProfile = ProfileUse ? ProfileUse->getValue() : "";

  Opts.EmitProfileCoverageMapping |= Args.hasArg(OPT_profile_coverage_mapping);
  Opts.DisableSILPartialApply |=
    Args.hasArg(OPT_disable_sil_partial_apply);
  Opts.EnableSILOwnership |= Args.hasArg(OPT_enable_sil_ownership);
  Opts.AssumeUnqualifiedOwnershipWhenParsing
    |= Args.hasArg(OPT_assume_parsing_unqualified_ownership_sil);
  Opts.EnableMandatorySemanticARCOpts |=
      !Args.hasArg(OPT_disable_mandatory_semantic_arc_opts);
  Opts.EnableLargeLoadableTypes |= Args.hasArg(OPT_enable_large_loadable_types);
  Opts.EnableGuaranteedNormalArguments |=
      Args.hasArg(OPT_enable_guaranteed_normal_arguments);

  if (const Arg *A = Args.getLastArg(OPT_save_optimization_record_path))
    Opts.OptRecordFile = A->getValue();

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
    Opts.Sanitizers = parseSanitizerArgValues(
        Args, A, Triple, Diags,
        /* sanitizerRuntimeLibExists= */[](StringRef libName) {

          // The driver has checked the existence of the library
          // already.
          return true;
        });
    IRGenOpts.Sanitizers = Opts.Sanitizers;
  }

  if (Opts.shouldOptimize())
    Opts.EnforceExclusivityDynamic = false;
  if (const Arg *A = Args.getLastArg(options::OPT_enforce_exclusivity_EQ)) {
    parseExclusivityEnforcementOptions(A, Opts, Diags);
  }

  return false;
}

void CompilerInvocation::buildDWARFDebugFlags(std::string &Output,
                                              const ArrayRef<const char*> &Args,
                                              StringRef SDKPath,
                                              StringRef ResourceDir) {
  // This isn't guaranteed to be the same temp directory as what the driver
  // uses, but it's highly likely.
  llvm::SmallString<128> TDir;
  llvm::sys::path::system_temp_directory(true, TDir);

  llvm::raw_string_ostream OS(Output);
  interleave(Args,
             [&](const char *Argument) { PrintArg(OS, Argument, TDir.str()); },
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
    PrintArg(OS, SDKPath.data(), TDir.str());
  }
  if (!haveResourceDir) {
    OS << " -resource-dir ";
    PrintArg(OS, ResourceDir.data(), TDir.str());
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

  for (const Arg *A : Args.filtered(OPT_Xcc)) {
    StringRef Opt = A->getValue();
    if (Opt.startswith("-D") || Opt.startswith("-U"))
      Opts.ClangDefines.push_back(Opt);
  }

  for (const Arg *A : Args.filtered(OPT_l, OPT_framework)) {
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
  } else if (const Optional<StringRef> filename =
                 FrontendOpts.Inputs.getOptionalUniquePrimaryInputFilename()) {
    Opts.MainInputFilename = filename.getValue();
  } else if (FrontendOpts.Inputs.haveUniqueInputFilename()) {
    Opts.MainInputFilename = FrontendOpts.Inputs.getFilenameOfFirstInput();
  }
  Opts.OutputFilenames = FrontendOpts.OutputFilenames;
  Opts.ModuleName = FrontendOpts.ModuleName;

  if (Args.hasArg(OPT_use_jit))
    Opts.UseJIT = true;
  
  for (const Arg *A : Args.filtered(OPT_verify_type_layout)) {
    Opts.VerifyTypeLayoutNames.push_back(A->getValue());
  }

  for (const Arg *A : Args.filtered(OPT_disable_autolink_framework)) {
    Opts.DisableAutolinkFrameworks.push_back(A->getValue());
  }

  Opts.GenerateProfile |= Args.hasArg(OPT_profile_generate);
  const Arg *ProfileUse = Args.getLastArg(OPT_profile_use);
  Opts.UseProfile = ProfileUse ? ProfileUse->getValue() : "";

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
    for (const Arg *A : Args) {
      // Do not encode output and input.
      if (A->getOption().getID() == options::OPT_o ||
          A->getOption().getID() == options::OPT_INPUT ||
          A->getOption().getID() == options::OPT_primary_file ||
          A->getOption().getID() == options::OPT_embed_bitcode)
        continue;
      ArgStringList ASL;
      A->render(Args, ASL);
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
        parseSanitizerCoverageArgValue(A, Triple, Diags, Opts.Sanitizers);
  } else if (Opts.Sanitizers & SanitizerKind::Fuzzer) {

    // Automatically set coverage flags, unless coverage type was explicitly
    // requested.
    Opts.SanitizeCoverage.IndirectCalls = true;
    Opts.SanitizeCoverage.TraceCmp = true;
    Opts.SanitizeCoverage.TracePCGuard = true;
    Opts.SanitizeCoverage.CoverageType = llvm::SanitizerCoverageOptions::SCK_Edge;
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

bool ParseMigratorArgs(MigratorOptions &Opts, llvm::Triple &Triple,
                       StringRef ResourcePath, ArgList &Args,
                       DiagnosticEngine &Diags) {
  using namespace options;

  Opts.KeepObjcVisibility |= Args.hasArg(OPT_migrate_keep_objc_visibility);
  Opts.DumpUsr = Args.hasArg(OPT_dump_usr);

  if (Args.hasArg(OPT_disable_migrator_fixits)) {
    Opts.EnableMigratorFixits = false;
  }

  if (auto RemapFilePath = Args.getLastArg(OPT_emit_remap_file_path)) {
    Opts.EmitRemapFilePath = RemapFilePath->getValue();
  }

  if (auto MigratedFilePath = Args.getLastArg(OPT_emit_migrated_file_path)) {
    Opts.EmitMigratedFilePath = MigratedFilePath->getValue();
  }

  if (auto Dumpster = Args.getLastArg(OPT_dump_migration_states_dir)) {
    Opts.DumpMigrationStatesDir = Dumpster->getValue();
  }

  if (auto DataPath = Args.getLastArg(OPT_api_diff_data_file)) {
    Opts.APIDigesterDataStorePaths.push_back(DataPath->getValue());
  } else {
    bool Supported = true;
    llvm::SmallString<128> dataPath(ResourcePath);
    llvm::sys::path::append(dataPath, "migrator");
    if (Triple.isMacOSX())
      llvm::sys::path::append(dataPath, "macos.json");
    else if (Triple.isiOS())
      llvm::sys::path::append(dataPath, "ios.json");
    else if (Triple.isTvOS())
      llvm::sys::path::append(dataPath, "tvos.json");
    else if (Triple.isWatchOS())
      llvm::sys::path::append(dataPath, "watchos.json");
    else
      Supported = false;
    if (Supported) {
      llvm::SmallString<128> authoredDataPath(ResourcePath);
      llvm::sys::path::append(authoredDataPath, "migrator");
      llvm::sys::path::append(authoredDataPath, "overlay.json");
      // Add authored list first to take higher priority.
      Opts.APIDigesterDataStorePaths.push_back(authoredDataPath.str());
      Opts.APIDigesterDataStorePaths.push_back(dataPath.str());
    }
  }

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
    for (const Arg *A : ParsedArgs.filtered(OPT_UNKNOWN)) {
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
                   LangOpts.Target, ClangImporterOpts)) {
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

  if (ParseMigratorArgs(MigratorOpts, LangOpts.Target,
                        SearchPathOpts.RuntimeResourcePath, ParsedArgs, Diags)) {
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

llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>>
CompilerInvocation::setUpInputForSILTool(
    StringRef InputFilename, StringRef ModuleNameArg,
    bool alwaysSetModuleToMain,
    serialization::ExtendedValidationInfo &extendedInfo) {
  // Load the input file.
  llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>> FileBufOrErr =
      llvm::MemoryBuffer::getFileOrSTDIN(InputFilename);
  if (!FileBufOrErr) {
    return FileBufOrErr;
  }

  // If it looks like we have an AST, set the source file kind to SIL and the
  // name of the module to the file's name.
  addInputBuffer(FileBufOrErr.get().get());

  auto result = serialization::validateSerializedAST(
      FileBufOrErr.get()->getBuffer(), &extendedInfo);
  bool HasSerializedAST = result.status == serialization::Status::Valid;

  if (HasSerializedAST) {
    const StringRef Stem = !ModuleNameArg.empty()
                               ? ModuleNameArg
                               : llvm::sys::path::stem(InputFilename);
    setModuleName(Stem);
    setInputKind(InputFileKind::IFK_Swift_Library);
  } else {
    const StringRef Name = (alwaysSetModuleToMain || ModuleNameArg.empty())
                               ? "main"
                               : ModuleNameArg;
    setModuleName(Name);
    setInputKind(InputFileKind::IFK_SIL);
  }
  return FileBufOrErr;
}
