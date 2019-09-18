//===--- ArgsToFrontendOptionsConverter -----------------------------------===//
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

#include "ArgsToFrontendOptionsConverter.h"

#include "ArgsToFrontendInputsConverter.h"
#include "ArgsToFrontendOutputsConverter.h"
#include "swift/AST/DiagnosticsFrontend.h"
#include "swift/Basic/Platform.h"
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

bool ArgsToFrontendOptionsConverter::convert(
    SmallVectorImpl<std::unique_ptr<llvm::MemoryBuffer>> *buffers) {
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
  if (const Arg *A = Args.getLastArg(OPT_prebuilt_module_cache_path)) {
    Opts.PrebuiltModuleCachePath = A->getValue();
  }

  Opts.IndexSystemModules |= Args.hasArg(OPT_index_system_modules);

  Opts.EmitVerboseSIL |= Args.hasArg(OPT_emit_verbose_sil);
  Opts.EmitSortedSIL |= Args.hasArg(OPT_emit_sorted_sil);

  Opts.EnableTesting |= Args.hasArg(OPT_enable_testing);
  Opts.EnablePrivateImports |= Args.hasArg(OPT_enable_private_imports);
  Opts.EnableLibraryEvolution |= Args.hasArg(OPT_enable_library_evolution);

  // FIXME: Remove this flag
  Opts.EnableLibraryEvolution |= Args.hasArg(OPT_enable_resilience);

  Opts.EnableImplicitDynamic |= Args.hasArg(OPT_enable_implicit_dynamic);

  Opts.TrackSystemDeps |= Args.hasArg(OPT_track_system_dependencies);

  Opts.SerializeModuleInterfaceDependencyHashes |=
    Args.hasArg(OPT_serialize_module_interface_dependency_hashes);

  Opts.RemarkOnRebuildFromModuleInterface |=
    Args.hasArg(OPT_Rmodule_interface_rebuild);

  computePrintStatsOptions();
  computeDebugTimeOptions();
  computeTBDOptions();

  setUnsignedIntegerArgument(OPT_warn_long_function_bodies, 10,
                             Opts.WarnLongFunctionBodies);
  setUnsignedIntegerArgument(OPT_warn_long_expression_type_checking, 10,
                             Opts.WarnLongExpressionTypeChecking);
  setUnsignedIntegerArgument(OPT_solver_expression_time_threshold_EQ, 10,
                             Opts.SolverExpressionTimeThreshold);
  setUnsignedIntegerArgument(OPT_switch_checking_invocation_threshold_EQ, 10,
                             Opts.SwitchCheckingInvocationThreshold);

  Opts.CheckOnoneSupportCompleteness = Args.hasArg(OPT_check_onone_completeness);

  Opts.DebuggerTestingTransform = Args.hasArg(OPT_debugger_testing_transform);

  computePlaygroundOptions();

  // This can be enabled independently of the playground transform.
  Opts.PCMacro |= Args.hasArg(OPT_pc_macro);

  Opts.ParseStdlib |= Args.hasArg(OPT_parse_stdlib);

  computeHelpOptions();

  if (const Arg *A = Args.getLastArg(OPT_verify_generic_signatures)) {
    Opts.VerifyGenericSignaturesInModule = A->getValue();
  }

  computeDumpScopeMapLocations();

  Optional<FrontendInputsAndOutputs> inputsAndOutputs =
      ArgsToFrontendInputsConverter(Diags, Args).convert(buffers);

  // None here means error, not just "no inputs". Propagage unconditionally.
  if (!inputsAndOutputs)
    return true;

  // InputsAndOutputs can only get set up once; if it was set already when we
  // entered this function, we should not set it again (and should assert this
  // is not being done). Further, the computeMainAndSupplementaryOutputFilenames
  // call below needs to only happen when there was a new InputsAndOutputs,
  // since it clobbers the existing one rather than adding to it.
  bool HaveNewInputsAndOutputs = false;
  if (Opts.InputsAndOutputs.hasInputs()) {
    assert(!inputsAndOutputs->hasInputs());
  } else {
    HaveNewInputsAndOutputs = true;
    Opts.InputsAndOutputs = std::move(inputsAndOutputs).getValue();
  }

  if (Opts.RequestedAction == FrontendOptions::ActionType::NoneAction) {
    Opts.RequestedAction = determineRequestedAction(Args);
  }

  if (Opts.RequestedAction == FrontendOptions::ActionType::Immediate &&
      Opts.InputsAndOutputs.hasPrimaryInputs()) {
    Diags.diagnose(SourceLoc(), diag::error_immediate_mode_primary_file);
    return true;
  }

  if (setUpInputKindAndImmediateArgs())
    return true;

  if (computeModuleName())
    return true;

  if (HaveNewInputsAndOutputs &&
      computeMainAndSupplementaryOutputFilenames())
    return true;

  if (checkUnusedSupplementaryOutputPaths())
    return true;

  if (const Arg *A = Args.getLastArg(OPT_module_link_name))
    Opts.ModuleLinkName = A->getValue();

  if (const Arg *A = Args.getLastArg(OPT_serialize_debugging_options,
                                     OPT_no_serialize_debugging_options)) {
    Opts.SerializeOptionsForDebugging =
        A->getOption().matches(OPT_serialize_debugging_options);
  }

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
    if (Args.getLastArg(OPT_profile_stats_events)) {
      Opts.ProfileEvents = true;
    }
    if (Args.getLastArg(OPT_profile_stats_entities)) {
      Opts.ProfileEntities = true;
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
}

void ArgsToFrontendOptionsConverter::setUnsignedIntegerArgument(
    options::ID optionID, unsigned radix, unsigned &valueToSet) {
  if (const Arg *A = Args.getLastArg(optionID)) {
    unsigned attempt;
    if (StringRef(A->getValue()).getAsInteger(radix, attempt)) {
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
ArgsToFrontendOptionsConverter::determineRequestedAction(const ArgList &args) {
  using namespace options;
  const Arg *A = args.getLastArg(OPT_modes_Group);
  if (!A) {
    // We don't have a mode, so determine a default.
    if (args.hasArg(OPT_emit_module, OPT_emit_module_path)) {
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
  if (Opt.matches(OPT_resolve_imports))
    return FrontendOptions::ActionType::ResolveImports;
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
  if (Opt.matches(OPT_dump_type_info))
    return FrontendOptions::ActionType::DumpTypeInfo;
  if (Opt.matches(OPT_print_ast))
    return FrontendOptions::ActionType::PrintAST;

  if (Opt.matches(OPT_repl) || Opt.matches(OPT_deprecated_integrated_repl))
    return FrontendOptions::ActionType::REPL;
  if (Opt.matches(OPT_interpret))
    return FrontendOptions::ActionType::Immediate;
  if (Opt.matches(OPT_compile_module_from_interface))
    return FrontendOptions::ActionType::CompileModuleFromInterface;

  llvm_unreachable("Unhandled mode option");
}

bool ArgsToFrontendOptionsConverter::setUpInputKindAndImmediateArgs() {
  using namespace options;
  bool treatAsSIL =
      Args.hasArg(OPT_parse_sil) || Opts.InputsAndOutputs.shouldTreatAsSIL();

  if (Opts.InputsAndOutputs.verifyInputs(
          Diags, treatAsSIL,
          Opts.RequestedAction == FrontendOptions::ActionType::REPL,
          Opts.RequestedAction == FrontendOptions::ActionType::NoneAction)) {
    return true;
  }
  if (Opts.RequestedAction == FrontendOptions::ActionType::Immediate) {
    Opts.ImmediateArgv.push_back(
        Opts.InputsAndOutputs.getFilenameOfFirstInput()); // argv[0]
    if (const Arg *A = Args.getLastArg(OPT__DASH_DASH)) {
      for (unsigned i = 0, e = A->getNumValues(); i != e; ++i) {
        Opts.ImmediateArgv.push_back(A->getValue(i));
      }
    }
  }

  if (treatAsSIL)
    Opts.InputKind = InputFileKind::SIL;
  else if (Opts.InputsAndOutputs.shouldTreatAsLLVM())
    Opts.InputKind = InputFileKind::LLVM;
  else if (Opts.InputsAndOutputs.shouldTreatAsModuleInterface())
    Opts.InputKind = InputFileKind::SwiftModuleInterface;
  else if (Args.hasArg(OPT_parse_as_library))
    Opts.InputKind = InputFileKind::SwiftLibrary;
  else if (Opts.RequestedAction == FrontendOptions::ActionType::REPL)
    Opts.InputKind = InputFileKind::SwiftREPL;
  else
    Opts.InputKind = InputFileKind::Swift;

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
  if (!Opts.InputsAndOutputs.hasInputs()) {
    Opts.ModuleName = StringRef();
    // FIXME: This is a bug that should not happen, but does in tests.
    // The compiler should bail out earlier, where "no frontend action was
    // selected".
    return false;
  }
  Optional<std::vector<std::string>> outputFilenames =
      OutputFilesComputer::getOutputFilenamesFromCommandLineOrFilelist(Args,
                                                                       Diags);

  std::string nameToStem =
      outputFilenames && outputFilenames->size() == 1 &&
              outputFilenames->front() != "-" &&
              !llvm::sys::fs::is_directory(outputFilenames->front())
          ? outputFilenames->front()
          : Opts.InputsAndOutputs.getFilenameOfFirstInput();

  Opts.ModuleName = llvm::sys::path::stem(nameToStem);
  return false;
}

bool ArgsToFrontendOptionsConverter::
    computeMainAndSupplementaryOutputFilenames() {
  std::vector<std::string> mainOutputs;
  std::vector<SupplementaryOutputPaths> supplementaryOutputs;
  const bool hadError = ArgsToFrontendOutputsConverter(
                            Args, Opts.ModuleName, Opts.InputsAndOutputs, Diags)
                            .convert(mainOutputs, supplementaryOutputs);
  if (hadError)
    return true;
  Opts.InputsAndOutputs.setMainAndSupplementaryOutputs(mainOutputs,
                                                       supplementaryOutputs);
  return false;
}

bool ArgsToFrontendOptionsConverter::checkUnusedSupplementaryOutputPaths()
    const {
  if (!FrontendOptions::canActionEmitDependencies(Opts.RequestedAction) &&
      Opts.InputsAndOutputs.hasDependenciesPath()) {
    Diags.diagnose(SourceLoc(), diag::error_mode_cannot_emit_dependencies);
    return true;
  }
  if (!FrontendOptions::canActionEmitReferenceDependencies(Opts.RequestedAction)
      && Opts.InputsAndOutputs.hasReferenceDependenciesPath()) {
    Diags.diagnose(SourceLoc(),
                   diag::error_mode_cannot_emit_reference_dependencies);
    return true;
  }
  if (!FrontendOptions::canActionEmitObjCHeader(Opts.RequestedAction) &&
      Opts.InputsAndOutputs.hasObjCHeaderOutputPath()) {
    Diags.diagnose(SourceLoc(), diag::error_mode_cannot_emit_header);
    return true;
  }
  if (!FrontendOptions::canActionEmitLoadedModuleTrace(Opts.RequestedAction) &&
      Opts.InputsAndOutputs.hasLoadedModuleTracePath()) {
    Diags.diagnose(SourceLoc(),
                   diag::error_mode_cannot_emit_loaded_module_trace);
    return true;
  }
  if (!FrontendOptions::canActionEmitModule(Opts.RequestedAction) &&
      Opts.InputsAndOutputs.hasModuleOutputPath()) {
    Diags.diagnose(SourceLoc(), diag::error_mode_cannot_emit_module);
    return true;
  }
  if (!FrontendOptions::canActionEmitModuleDoc(Opts.RequestedAction) &&
      Opts.InputsAndOutputs.hasModuleDocOutputPath()) {
    Diags.diagnose(SourceLoc(), diag::error_mode_cannot_emit_module_doc);
    return true;
  }
  if (!FrontendOptions::canActionEmitInterface(Opts.RequestedAction) &&
      Opts.InputsAndOutputs.hasModuleInterfaceOutputPath()) {
    Diags.diagnose(SourceLoc(), diag::error_mode_cannot_emit_interface);
    return true;
  }
  return false;
}

void ArgsToFrontendOptionsConverter::computeImportObjCHeaderOptions() {
  using namespace options;
  if (const Arg *A = Args.getLastArgNoClaim(OPT_import_objc_header)) {
    Opts.ImplicitObjCHeaderPath = A->getValue();
    Opts.SerializeBridgingHeader |= !Opts.InputsAndOutputs.hasPrimaryInputs();
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
