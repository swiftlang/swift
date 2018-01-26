//===--- ArgsToFrontendOptionsConverter -------------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/Frontend/ArgsToFrontendOptionsConverter.h"

#include "swift/AST/DiagnosticsFrontend.h"
#include "swift/Basic/Platform.h"
#include "swift/Frontend/ArgsToFrontendInputsConverter.h"
#include "swift/Frontend/Frontend.h"
#include "swift/Option/Options.h"
#include "swift/Option/SanitizerOptions.h"
#include "swift/Parse/Lexer.h"
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

// This is a separate function so that it shows up in stack traces.
LLVM_ATTRIBUTE_NOINLINE
static void debugFailWithAssertion() {
  // This assertion should always fail, per the user's request, and should
  // not be converted to llvm_unreachable.
  assert(0 && "This is an assertion!");
}

// This is a separate function so that it shows up in stack traces.
LLVM_ATTRIBUTE_NOINLINE
static void debugFailWithCrash() { LLVM_BUILTIN_TRAP; }

bool ArgsToFrontendOptionsConverter::convert() {
  using namespace options;

  handleDebugCrashGroupArguments();

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

  computePrintStatsOptions();
  computeDebugTimeOptions();
  computeTBDOptions();

  setUnsignedIntegerArgument(OPT_warn_long_function_bodies, 10,
                             Opts.WarnLongFunctionBodies);
  setUnsignedIntegerArgument(OPT_warn_long_expression_type_checking, 10,
                             Opts.WarnLongExpressionTypeChecking);
  setUnsignedIntegerArgument(OPT_solver_expression_time_threshold_EQ, 10,
                             Opts.SolverExpressionTimeThreshold);

  computePlaygroundOptions();

  // This can be enabled independently of the playground transform.
  Opts.PCMacro |= Args.hasArg(OPT_pc_macro);

  computeHelpOptions();
  if (ArgsToFrontendInputsConverter(Diags, Args, Opts.Inputs).convert())
    return true;

  Opts.ParseStdlib |= Args.hasArg(OPT_parse_stdlib);

  if (const Arg *A = Args.getLastArg(OPT_verify_generic_signatures)) {
    Opts.VerifyGenericSignaturesInModule = A->getValue();
  }

  computeDumpScopeMapLocations();
  Opts.RequestedAction = determineRequestedAction();

  if (Opts.RequestedAction == FrontendOptions::ActionType::Immediate &&
      Opts.Inputs.hasPrimaryInputs()) {
    Diags.diagnose(SourceLoc(), diag::error_immediate_mode_primary_file);
    return true;
  }

  if (setUpForSILOrLLVM())
    return true;

  if (computeModuleName())
    return true;

  if (computeOutputFilenames())
    return true;
  determineSupplementaryOutputFilenames();

  if (checkForUnusedOutputPaths())
    return true;

  if (const Arg *A = Args.getLastArg(OPT_module_link_name)) {
    Opts.ModuleLinkName = A->getValue();
  }

  Opts.AlwaysSerializeDebuggingOptions |=
      Args.hasArg(OPT_serialize_debugging_options);
  Opts.EnableSourceImport |= Args.hasArg(OPT_enable_source_import);
  Opts.ImportUnderlyingModule |= Args.hasArg(OPT_import_underlying_module);
  Opts.EnableSerializationNestedTypeLookupTable &=
      !Args.hasArg(OPT_disable_serialization_nested_type_lookup_table);

  computeImportObjCHeaderOptions();
  computeImplicitImportModuleNames();
  computeLLVMArgs();

  return false;
}

void ArgsToFrontendOptionsConverter::handleDebugCrashGroupArguments() {
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
}

void ArgsToFrontendOptionsConverter::computePrintStatsOptions() {
  using namespace options;
  Opts.PrintStats |= Args.hasArg(OPT_print_stats);
  Opts.PrintClangStats |= Args.hasArg(OPT_print_clang_stats);
#if defined(NDEBUG) && !defined(LLVM_ENABLE_STATS)
  if (Opts.PrintStats || Opts.PrintClangStats)
    Diags.diagnose(SourceLoc(), diag::stats_disabled);
#endif
}

void ArgsToFrontendOptionsConverter::computeDebugTimeOptions() {
  using namespace options;
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
}

void ArgsToFrontendOptionsConverter::computeTBDOptions() {
  using namespace options;
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
}

void ArgsToFrontendOptionsConverter::setUnsignedIntegerArgument(
    options::ID optionID, unsigned max, unsigned &valueToSet) {
  if (const Arg *A = Args.getLastArg(optionID)) {
    unsigned attempt;
    if (StringRef(A->getValue()).getAsInteger(max, attempt)) {
      Diags.diagnose(SourceLoc(), diag::error_invalid_arg_value,
                     A->getAsString(Args), A->getValue());
    } else {
      valueToSet = attempt;
    }
  }
}

void ArgsToFrontendOptionsConverter::computePlaygroundOptions() {
  using namespace options;
  Opts.PlaygroundTransform |= Args.hasArg(OPT_playground);
  if (Args.hasArg(OPT_disable_playground_transform))
    Opts.PlaygroundTransform = false;
  Opts.PlaygroundHighPerformance |=
      Args.hasArg(OPT_playground_high_performance);
}

void ArgsToFrontendOptionsConverter::computeHelpOptions() {
  using namespace options;
  if (const Arg *A = Args.getLastArg(OPT_help, OPT_help_hidden)) {
    if (A->getOption().matches(OPT_help)) {
      Opts.PrintHelp = true;
    } else if (A->getOption().matches(OPT_help_hidden)) {
      Opts.PrintHelpHidden = true;
    } else {
      llvm_unreachable("Unknown help option parsed");
    }
  }
}

void ArgsToFrontendOptionsConverter::computeDumpScopeMapLocations() {
  using namespace options;
  const Arg *A = Args.getLastArg(OPT_modes_Group);
  if (!A || !A->getOption().matches(OPT_dump_scope_maps))
    return;
  StringRef value = A->getValue();
  if (value == "expanded") {
    // Note: fully expanded the scope map.
    return;
  }
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

FrontendOptions::ActionType
ArgsToFrontendOptionsConverter::determineRequestedAction() const {
  using namespace options;
  const Arg *A = Args.getLastArg(OPT_modes_Group);
  if (!A) {
    // We don't have a mode, so determine a default.
    if (Args.hasArg(OPT_emit_module, OPT_emit_module_path)) {
      // We've been told to emit a module, but have no other mode indicators.
      // As a result, put the frontend into EmitModuleOnly mode.
      // (Setting up module output will be handled below.)
      return FrontendOptions::ActionType::EmitModuleOnly;
    }
    return FrontendOptions::ActionType::NoneAction;
  }
  Option Opt = A->getOption();
  if (Opt.matches(OPT_emit_object))
    return FrontendOptions::ActionType::EmitObject;
  if (Opt.matches(OPT_emit_assembly))
    return FrontendOptions::ActionType::EmitAssembly;
  if (Opt.matches(OPT_emit_ir))
    return FrontendOptions::ActionType::EmitIR;
  if (Opt.matches(OPT_emit_bc))
    return FrontendOptions::ActionType::EmitBC;
  if (Opt.matches(OPT_emit_sil))
    return FrontendOptions::ActionType::EmitSIL;
  if (Opt.matches(OPT_emit_silgen))
    return FrontendOptions::ActionType::EmitSILGen;
  if (Opt.matches(OPT_emit_sib))
    return FrontendOptions::ActionType::EmitSIB;
  if (Opt.matches(OPT_emit_sibgen))
    return FrontendOptions::ActionType::EmitSIBGen;
  if (Opt.matches(OPT_emit_pch))
    return FrontendOptions::ActionType::EmitPCH;
  if (Opt.matches(OPT_emit_imported_modules))
    return FrontendOptions::ActionType::EmitImportedModules;
  if (Opt.matches(OPT_parse))
    return FrontendOptions::ActionType::Parse;
  if (Opt.matches(OPT_typecheck))
    return FrontendOptions::ActionType::Typecheck;
  if (Opt.matches(OPT_dump_parse))
    return FrontendOptions::ActionType::DumpParse;
  if (Opt.matches(OPT_dump_ast))
    return FrontendOptions::ActionType::DumpAST;
  if (Opt.matches(OPT_emit_syntax))
    return FrontendOptions::ActionType::EmitSyntax;
  if (Opt.matches(OPT_merge_modules))
    return FrontendOptions::ActionType::MergeModules;
  if (Opt.matches(OPT_dump_scope_maps))
    return FrontendOptions::ActionType::DumpScopeMaps;
  if (Opt.matches(OPT_dump_type_refinement_contexts))
    return FrontendOptions::ActionType::DumpTypeRefinementContexts;
  if (Opt.matches(OPT_dump_interface_hash))
    return FrontendOptions::ActionType::DumpInterfaceHash;
  if (Opt.matches(OPT_print_ast))
    return FrontendOptions::ActionType::PrintAST;

  if (Opt.matches(OPT_repl) || Opt.matches(OPT_deprecated_integrated_repl))
    return FrontendOptions::ActionType::REPL;
  if (Opt.matches(OPT_interpret))
    return FrontendOptions::ActionType::Immediate;

  llvm_unreachable("Unhandled mode option");
}

bool ArgsToFrontendOptionsConverter::setUpForSILOrLLVM() {
  using namespace options;
  bool treatAsSIL =
      Args.hasArg(OPT_parse_sil) || Opts.Inputs.shouldTreatAsSIL();
  bool treatAsLLVM = Opts.Inputs.shouldTreatAsLLVM();

  if (Opts.Inputs.verifyInputs(
          Diags, treatAsSIL,
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

  if (treatAsSIL)
    Opts.InputKind = InputFileKind::IFK_SIL;
  else if (treatAsLLVM)
    Opts.InputKind = InputFileKind::IFK_LLVM_IR;
  else if (Args.hasArg(OPT_parse_as_library))
    Opts.InputKind = InputFileKind::IFK_Swift_Library;
  else if (Opts.RequestedAction == FrontendOptions::ActionType::REPL)
    Opts.InputKind = InputFileKind::IFK_Swift_REPL;
  else
    Opts.InputKind = InputFileKind::IFK_Swift;

  return false;
}

bool ArgsToFrontendOptionsConverter::computeModuleName() {
  const Arg *A = Args.getLastArg(options::OPT_module_name);
  if (A) {
    Opts.ModuleName = A->getValue();
  } else if (Opts.ModuleName.empty()) {
    // The user did not specify a module name, so determine a default fallback
    // based on other options.

    // Note: this code path will only be taken when running the frontend
    // directly; the driver should always pass -module-name when invoking the
    // frontend.
    if (computeFallbackModuleName())
      return true;
  }

  if (Lexer::isIdentifier(Opts.ModuleName) &&
      (Opts.ModuleName != STDLIB_NAME || Opts.ParseStdlib)) {
    return false;
  }
  if (!FrontendOptions::needsProperModuleName(Opts.RequestedAction) ||
      Opts.isCompilingExactlyOneSwiftFile()) {
    Opts.ModuleName = "main";
    return false;
  }
  auto DID = (Opts.ModuleName == STDLIB_NAME) ? diag::error_stdlib_module_name
                                              : diag::error_bad_module_name;
  Diags.diagnose(SourceLoc(), DID, Opts.ModuleName, A == nullptr);
  Opts.ModuleName = "__bad__";
  return false; // FIXME: Must continue to run to pass the tests, but should not
  // have to.
}

bool ArgsToFrontendOptionsConverter::computeFallbackModuleName() {
  if (Opts.RequestedAction == FrontendOptions::ActionType::REPL) {
    // Default to a module named "REPL" if we're in REPL mode.
    Opts.ModuleName = "REPL";
    return false;
  }
  // In order to pass some tests, must leave ModuleName empty.
  if (!Opts.Inputs.hasInputs()) {
    Opts.ModuleName = StringRef();
    // FIXME: This is a bug that should not happen, but does in tests.
    // The compiler should bail out earlier, where "no frontend action was
    // selected".
    return false;
  }
  ArrayRef<std::string> outputFilenames =
      getOutputFilenamesFromCommandLineOrFilelist();

  bool isOutputAUniqueOrdinaryFile =
      outputFilenames.size() == 1 && outputFilenames[0] != "-" &&
      !llvm::sys::fs::is_directory(outputFilenames[0]);
  std::string nameToStem = isOutputAUniqueOrdinaryFile
                               ? outputFilenames[0]
                               : Opts.Inputs.getFilenameOfFirstInput().str();
  Opts.ModuleName = llvm::sys::path::stem(nameToStem);
  return false;
}

bool ArgsToFrontendOptionsConverter::computeOutputFilenames() {
  assert(Opts.OutputFilenames.empty() &&
         "Output filename should not be set at this point");
  if (!FrontendOptions::doesActionProduceOutput(Opts.RequestedAction)) {
    return false;
  }
  ArrayRef<std::string> outputFilenamesFromCommandLineOrFilelist =
      getOutputFilenamesFromCommandLineOrFilelist();

  if (outputFilenamesFromCommandLineOrFilelist.size() > 1) {
    // WMO, threaded with N files (also someday batch mode).
    Opts.OutputFilenames = outputFilenamesFromCommandLineOrFilelist;
    return false;
  }

  if (outputFilenamesFromCommandLineOrFilelist.empty()) {
    // When the Frontend is invoked without going through the driver
    // (e.g. for testing), it is convenient to derive output filenames from
    // input.
    return deriveOutputFilenameFromInputFile();
  }

  StringRef outputFilename = outputFilenamesFromCommandLineOrFilelist[0];
  if (!llvm::sys::fs::is_directory(outputFilename)) {
    // Could be -primary-file (1), or -wmo (non-threaded w/ N (input) files)
    Opts.OutputFilenames = outputFilenamesFromCommandLineOrFilelist;
    return false;
  }
  // Only used for testing & when invoking frontend directly.
  return deriveOutputFilenameForDirectory(outputFilename);
}

bool ArgsToFrontendOptionsConverter::deriveOutputFilenameFromInputFile() {
  if (Opts.Inputs.isReadingFromStdin() ||
      FrontendOptions::doesActionProduceTextualOutput(Opts.RequestedAction)) {
    Opts.setOutputFilenameToStdout();
    return false;
  }
  std::string baseName = determineBaseNameOfOutput();
  if (baseName.empty()) {
    if (Opts.RequestedAction != FrontendOptions::ActionType::REPL &&
        Opts.RequestedAction != FrontendOptions::ActionType::Immediate &&
        Opts.RequestedAction != FrontendOptions::ActionType::NoneAction) {
      Diags.diagnose(SourceLoc(), diag::error_no_output_filename_specified);
      return true;
    }
    return false;
  }
  deriveOutputFilenameFromParts("", baseName);
  return false;
}

bool ArgsToFrontendOptionsConverter::deriveOutputFilenameForDirectory(
    StringRef outputDir) {

  std::string baseName = determineBaseNameOfOutput();
  if (baseName.empty()) {
    Diags.diagnose(SourceLoc(), diag::error_implicit_output_file_is_directory,
                   outputDir);
    return true;
  }
  deriveOutputFilenameFromParts(outputDir, baseName);
  return false;
}

void ArgsToFrontendOptionsConverter::deriveOutputFilenameFromParts(
    StringRef dir, StringRef base) {
  assert(!base.empty());
  llvm::SmallString<128> path(dir);
  llvm::sys::path::append(path, base);
  StringRef suffix = FrontendOptions::suffixForPrincipalOutputFileForAction(
      Opts.RequestedAction);
  llvm::sys::path::replace_extension(path, suffix);
  Opts.OutputFilenames.push_back(path.str());
}

std::string ArgsToFrontendOptionsConverter::determineBaseNameOfOutput() const {
  std::string nameToStem;
  if (Opts.Inputs.hasPrimaryInputs()) {
    nameToStem = Opts.Inputs.getRequiredUniquePrimaryInput().file();
  } else if (auto UserSpecifiedModuleName =
                 Args.getLastArg(options::OPT_module_name)) {
    nameToStem = UserSpecifiedModuleName->getValue();
  } else if (Opts.Inputs.hasSingleInput()) {
    nameToStem = Opts.Inputs.getFilenameOfFirstInput();
  } else
    nameToStem = "";

  return llvm::sys::path::stem(nameToStem).str();
}

ArrayRef<std::string>
ArgsToFrontendOptionsConverter::getOutputFilenamesFromCommandLineOrFilelist() {
  if (cachedOutputFilenamesFromCommandLineOrFilelist) {
    return *cachedOutputFilenamesFromCommandLineOrFilelist;
  }

  if (const Arg *A = Args.getLastArg(options::OPT_output_filelist)) {
    assert(!Args.hasArg(options::OPT_o) &&
           "don't use -o with -output-filelist");
    cachedOutputFilenamesFromCommandLineOrFilelist.emplace(
        readOutputFileList(A->getValue()));
  } else {
    cachedOutputFilenamesFromCommandLineOrFilelist.emplace(
        Args.getAllArgValues(options::OPT_o));
  }
  return *cachedOutputFilenamesFromCommandLineOrFilelist;
}

/// Try to read an output file list file.
std::vector<std::string> ArgsToFrontendOptionsConverter::readOutputFileList(
    const StringRef filelistPath) const {
  llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>> buffer =
      llvm::MemoryBuffer::getFile(filelistPath);
  if (!buffer) {
    Diags.diagnose(SourceLoc(), diag::cannot_open_file, filelistPath,
                   buffer.getError().message());
  }
  std::vector<std::string> outputFiles;
  for (StringRef line : make_range(llvm::line_iterator(*buffer.get()), {})) {
    outputFiles.push_back(line.str());
  }
  return outputFiles;
}

void ArgsToFrontendOptionsConverter::determineSupplementaryOutputFilenames() {
  using namespace options;
  auto determineOutputFilename =
      [&](std::string &output, OptSpecifier optWithoutPath,
          OptSpecifier optWithPath, const char *extension, bool useMainOutput) {
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

        llvm::SmallString<128> path(Opts.originalPath());
        llvm::sys::path::replace_extension(path, extension);
        output = path.str();
      };

  determineOutputFilename(Opts.DependenciesFilePath, OPT_emit_dependencies,
                          OPT_emit_dependencies_path, "d", false);
  determineOutputFilename(
      Opts.ReferenceDependenciesFilePath, OPT_emit_reference_dependencies,
      OPT_emit_reference_dependencies_path, "swiftdeps", false);
  determineOutputFilename(Opts.SerializedDiagnosticsPath,
                          OPT_serialize_diagnostics,
                          OPT_serialize_diagnostics_path, "dia", false);
  determineOutputFilename(Opts.ObjCHeaderOutputPath, OPT_emit_objc_header,
                          OPT_emit_objc_header_path, "h", false);
  determineOutputFilename(
      Opts.LoadedModuleTracePath, OPT_emit_loaded_module_trace,
      OPT_emit_loaded_module_trace_path, "trace.json", false);

  determineOutputFilename(Opts.TBDPath, OPT_emit_tbd, OPT_emit_tbd_path, "tbd",
                          false);

  if (const Arg *A = Args.getLastArg(OPT_emit_fixits_path)) {
    Opts.FixitsOutputPath = A->getValue();
  }

  bool isSIB = Opts.RequestedAction == FrontendOptions::ActionType::EmitSIB ||
               Opts.RequestedAction == FrontendOptions::ActionType::EmitSIBGen;
  bool canUseMainOutputForModule =
      Opts.RequestedAction == FrontendOptions::ActionType::MergeModules ||
      Opts.RequestedAction == FrontendOptions::ActionType::EmitModuleOnly ||
      isSIB;
  auto ext = isSIB ? SIB_EXTENSION : SERIALIZED_MODULE_EXTENSION;
  auto sibOpt = Opts.RequestedAction == FrontendOptions::ActionType::EmitSIB
                    ? OPT_emit_sib
                    : OPT_emit_sibgen;
  determineOutputFilename(Opts.ModuleOutputPath,
                          isSIB ? sibOpt : OPT_emit_module,
                          OPT_emit_module_path, ext, canUseMainOutputForModule);

  determineOutputFilename(Opts.ModuleDocOutputPath, OPT_emit_module_doc,
                          OPT_emit_module_doc_path,
                          SERIALIZED_MODULE_DOC_EXTENSION, false);
}

bool ArgsToFrontendOptionsConverter::checkForUnusedOutputPaths() const {
  if (Opts.hasUnusedDependenciesFilePath()) {
    Diags.diagnose(SourceLoc(), diag::error_mode_cannot_emit_dependencies);
    return true;
  }
  if (Opts.hasUnusedObjCHeaderOutputPath()) {
    Diags.diagnose(SourceLoc(), diag::error_mode_cannot_emit_header);
    return true;
  }
  if (Opts.hasUnusedLoadedModuleTracePath()) {
    Diags.diagnose(SourceLoc(),
                   diag::error_mode_cannot_emit_loaded_module_trace);
    return true;
  }
  if (Opts.hasUnusedModuleOutputPath()) {
    Diags.diagnose(SourceLoc(), diag::error_mode_cannot_emit_module);
    return true;
  }
  if (Opts.hasUnusedModuleDocOutputPath()) {
    Diags.diagnose(SourceLoc(), diag::error_mode_cannot_emit_module_doc);
    return true;
  }
  return false;
}

void ArgsToFrontendOptionsConverter::computeImportObjCHeaderOptions() {
  using namespace options;
  if (const Arg *A = Args.getLastArgNoClaim(OPT_import_objc_header)) {
    Opts.ImplicitObjCHeaderPath = A->getValue();
    Opts.SerializeBridgingHeader |=
        !Opts.Inputs.hasPrimaryInputs() && !Opts.ModuleOutputPath.empty();
  }
}
void ArgsToFrontendOptionsConverter::computeImplicitImportModuleNames() {
  using namespace options;
  for (const Arg *A : Args.filtered(OPT_import_module)) {
    Opts.ImplicitImportModuleNames.push_back(A->getValue());
  }
}
void ArgsToFrontendOptionsConverter::computeLLVMArgs() {
  using namespace options;
  for (const Arg *A : Args.filtered(OPT_Xllvm)) {
    Opts.LLVMArgs.push_back(A->getValue());
  }
}
