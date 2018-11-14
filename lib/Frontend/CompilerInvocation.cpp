//===--- CompilerInvocation.cpp - CompilerInvocation methods --------------===//
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

#include "swift/Frontend/Frontend.h"

#include "ArgsToFrontendOptionsConverter.h"
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
  setTargetTriple(llvm::Triple(Triple));
}

void CompilerInvocation::setTargetTriple(const llvm::Triple &Triple) {
  LangOpts.setTarget(Triple);
  updateRuntimeLibraryPath(SearchPathOpts, LangOpts.Target);
}

SourceFileKind CompilerInvocation::getSourceFileKind() const {
  switch (getInputKind()) {
  case InputFileKind::Swift:
    return SourceFileKind::Main;
  case InputFileKind::SwiftLibrary:
    return SourceFileKind::Library;
  case InputFileKind::SwiftREPL:
    return SourceFileKind::REPL;
  case InputFileKind::SwiftModuleInterface:
    return SourceFileKind::Interface;
  case InputFileKind::SIL:
    return SourceFileKind::SIL;
  case InputFileKind::None:
  case InputFileKind::LLVM:
    llvm_unreachable("Trying to convert from unsupported InputFileKind");
  }

  llvm_unreachable("Unhandled InputFileKind in switch.");
}

static bool ParseFrontendArgs(
    FrontendOptions &opts, ArgList &args, DiagnosticEngine &diags,
    SmallVectorImpl<std::unique_ptr<llvm::MemoryBuffer>> *buffers) {
  ArgsToFrontendOptionsConverter converter(diags, args, opts);
  return converter.convert(buffers);
}

static void diagnoseSwiftVersion(Optional<version::Version> &vers, Arg *verArg,
                                 ArgList &Args, DiagnosticEngine &diags) {
  // General invalid version error
  diags.diagnose(SourceLoc(), diag::error_invalid_arg_value,
                 verArg->getAsString(Args), verArg->getValue());

  // Note valid versions.
  auto validVers = version::Version::getValidEffectiveVersions();
  auto versStr = "'" + llvm::join(validVers, "', '") + "'";
  diags.diagnose(SourceLoc(), diag::note_valid_swift_versions, versStr);
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

// Lifted from the clang driver.
static void PrintArg(raw_ostream &OS, const char *Arg, StringRef TempDir) {
  const bool Escape = std::strpbrk(Arg, "\"\\$ ");

  if (!TempDir.empty() && StringRef(Arg).startswith(TempDir)) {
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

/// Save a copy of any flags marked as ParseableInterfaceOption, if running
/// in a mode that is going to emit a .swiftinterface file.
static void SaveParseableInterfaceArgs(ParseableInterfaceOptions &Opts,
                                       FrontendOptions &FOpts,
                                       ArgList &Args, DiagnosticEngine &Diags) {
  if (!FOpts.InputsAndOutputs.hasParseableInterfaceOutputPath())
    return;
  ArgStringList RenderedArgs;
  for (auto A : Args) {
    if (A->getOption().hasFlag(options::ParseableInterfaceOption))
      A->render(Args, RenderedArgs);
  }
  llvm::raw_string_ostream OS(Opts.ParseableInterfaceFlags);
  interleave(RenderedArgs,
             [&](const char *Argument) { PrintArg(OS, Argument, StringRef()); },
             [&] { OS << " "; });
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

  Opts.EnableExperimentalStaticAssert |=
    Args.hasArg(OPT_enable_experimental_static_assert);

  Opts.EnableExperimentalPropertyBehaviors |=
    Args.hasArg(OPT_enable_experimental_property_behaviors);

  Opts.EnableOperatorDesignatedTypes |=
      Args.hasArg(OPT_enable_operator_designated_types);

  // Always enable operator designated types for the standard library.
  Opts.EnableOperatorDesignatedTypes |= FrontendOpts.ParseStdlib;

  Opts.SolverEnableOperatorDesignatedTypes |=
      Args.hasArg(OPT_solver_enable_operator_designated_types);

  if (auto A = Args.getLastArg(OPT_enable_deserialization_recovery,
                               OPT_disable_deserialization_recovery)) {
    Opts.EnableDeserializationRecovery
      = A->getOption().matches(OPT_enable_deserialization_recovery);
  }

  Opts.DisableAvailabilityChecking |=
      Args.hasArg(OPT_disable_availability_checking);

  Opts.DisableTsanInoutInstrumentation |=
      Args.hasArg(OPT_disable_tsan_inout_instrumentation);

  if (FrontendOpts.InputKind == InputFileKind::SIL)
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
  Opts.NamedLazyMemberLoading &= !Args.hasArg(OPT_disable_named_lazy_member_loading);
  Opts.DebugGenericSignatures |= Args.hasArg(OPT_debug_generic_signatures);

  if (Args.hasArg(OPT_verify_syntax_tree)) {
    Opts.BuildSyntaxTree = true;
    Opts.VerifySyntaxTree = true;
  }
  
  if (Args.hasArg(OPT_enable_experimental_dependencies))
    Opts.EnableExperimentalDependencies = true;

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

  if (Args.getLastArg(OPT_debug_cycles)) {
    Opts.EvaluatorCycleDiagnostics = CycleDiagnosticKind::DebugDiagnose;
  }

  if (const Arg *A = Args.getLastArg(OPT_output_request_graphviz)) {
    Opts.RequestEvaluatorGraphVizPath = A->getValue();
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

  if (Args.getLastArg(OPT_solver_disable_shrink))
    Opts.SolverDisableShrink = true;

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
                 OPT_disable_swift3_objc_inference, false);

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

  Opts.WarnImplicitOverrides =
    Args.hasArg(OPT_warn_implicit_overrides);

  Opts.EnableNSKeyedArchiverDiagnostics =
      Args.hasFlag(OPT_enable_nskeyedarchiver_diagnostics,
                   OPT_disable_nskeyedarchiver_diagnostics,
                   Opts.EnableNSKeyedArchiverDiagnostics);

  Opts.EnableNonFrozenEnumExhaustivityDiagnostics =
    Args.hasFlag(OPT_enable_nonfrozen_enum_exhaustivity_diagnostics,
                 OPT_disable_nonfrozen_enum_exhaustivity_diagnostics,
                 Opts.isSwiftVersionAtLeast(5));

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

#if SWIFT_DARWIN_ENABLE_STABLE_ABI_BIT
  Opts.UseDarwinPreStableABIBit = false;
#else
  Opts.UseDarwinPreStableABIBit = true;
#endif

  Opts.DisableConstraintSolverPerformanceHacks |=
      Args.hasArg(OPT_disable_constraint_solver_performance_hacks);

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

  for (auto A : Args.getAllArgValues(OPT_debug_prefix_map)) {
    // Forward -debug-prefix-map arguments from Swift to Clang as
    // -fdebug-prefix-map. This is required to ensure DIFiles created there,
    // like "<swift-imported-modules>", have their paths remapped properly.
    // (Note, however, that Clang's usage of std::map means that the remapping
    // may not be applied in the same order, which can matter if one mapping is
    // a prefix of another.)
    Opts.ExtraArgs.push_back("-fdebug-prefix-map=" + A);
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

  for (const Arg *A : Args.filtered(OPT_vfsoverlay)) {
    Opts.VFSOverlayFiles.push_back(resolveSearchPath(A->getValue()));
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
  if (Args.hasArg(OPT_sil_existential_specializer)) {
    Opts.ExistentialSpecializer = true;
  }
  if (const Arg *A = Args.getLastArg(OPT_num_threads)) {
    if (StringRef(A->getValue()).getAsInteger(10, Opts.NumThreads)) {
      Diags.diagnose(SourceLoc(), diag::error_invalid_arg_value,
                     A->getAsString(Args), A->getValue());
      return true;
    }
    if (environmentVariableRequestedMaximumDeterminism()) {
      Opts.NumThreads = 1;
      Diags.diagnose(SourceLoc(), diag::remark_max_determinism_overriding,
                     "-num-threads");
    }
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
      Args.hasArg(OPT_enable_mandatory_semantic_arc_opts);
  Opts.EnableLargeLoadableTypes |= Args.hasArg(OPT_enable_large_loadable_types);

  if (const Arg *A = Args.getLastArg(OPT_save_optimization_record_path))
    Opts.OptRecordFile = A->getValue();

  if (Args.hasArg(OPT_debug_on_sil)) {
    // Derive the name of the SIL file for debugging from
    // the regular outputfile.
    std::string BaseName = FEOpts.InputsAndOutputs.getSingleOutputFilename();
    // If there are no or multiple outputfiles, derive the name
    // from the module name.
    if (BaseName.empty())
      BaseName = FEOpts.ModuleName;
    Opts.SILOutputFileNameForDebugging = BaseName;
  }

  if (const Arg *A = Args.getLastArg(options::OPT_sanitize_EQ)) {
    Opts.Sanitizers = parseSanitizerArgValues(
        Args, A, Triple, Diags,
        /* sanitizerRuntimeLibExists= */[](StringRef libName, bool shared) {

          // The driver has checked the existence of the library
          // already.
          return true;
        });
    IRGenOpts.Sanitizers = Opts.Sanitizers;
  }

  if (auto A = Args.getLastArg(OPT_enable_verify_exclusivity,
                               OPT_disable_verify_exclusivity)) {
    Opts.VerifyExclusivity
      = A->getOption().matches(OPT_enable_verify_exclusivity);
  }
  // If runtime asserts are disabled in general, also disable runtime
  // exclusivity checks unless explicitly requested.
  if (Opts.RemoveRuntimeAsserts)
    Opts.EnforceExclusivityDynamic = false;

  if (const Arg *A = Args.getLastArg(options::OPT_enforce_exclusivity_EQ)) {
    parseExclusivityEnforcementOptions(A, Opts, Diags);
  }

  return false;
}

void CompilerInvocation::buildDebugFlags(std::string &Output,
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

static bool ParseTBDGenArgs(TBDGenOptions &Opts, ArgList &Args,
                            DiagnosticEngine &Diags,
                            CompilerInvocation &Invocation) {
  using namespace options;

  Opts.HasMultipleIGMs = Invocation.getSILOptions().hasMultipleIGMs();

  if (const Arg *A = Args.getLastArg(OPT_module_link_name)) {
    Opts.ModuleLinkName = A->getValue();
  }

  if (const Arg *A = Args.getLastArg(OPT_tbd_install_name)) {
    Opts.InstallName = A->getValue();
  }

  if (const Arg *A = Args.getLastArg(OPT_tbd_compatibility_version)) {
    if (auto vers = version::Version::parseVersionString(
          A->getValue(), SourceLoc(), &Diags)) {
      Opts.CompatibilityVersion = *vers;
    }
  }
  if (const Arg *A = Args.getLastArg(OPT_tbd_current_version)) {
    if (auto vers = version::Version::parseVersionString(
          A->getValue(), SourceLoc(), &Diags)) {
      Opts.CurrentVersion = *vers;
    }
  }
  return false;
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
      Opts.DebugInfoLevel = IRGenDebugInfoLevel::LineTables;
  } else if (const Arg *A = Args.getLastArg(OPT_g_Group)) {
    if (A->getOption().matches(OPT_g))
      Opts.DebugInfoLevel = IRGenDebugInfoLevel::Normal;
    else if (A->getOption().matches(options::OPT_gline_tables_only))
      Opts.DebugInfoLevel = IRGenDebugInfoLevel::LineTables;
    else if (A->getOption().matches(options::OPT_gdwarf_types))
      Opts.DebugInfoLevel = IRGenDebugInfoLevel::DwarfTypes;
    else
      assert(A->getOption().matches(options::OPT_gnone) &&
             "unknown -g<kind> option");

    if (Opts.DebugInfoLevel > IRGenDebugInfoLevel::LineTables) {
      if (Args.hasArg(options::OPT_debug_info_store_invocation)) {
        ArgStringList RenderedArgs;
        for (auto A : Args)
          A->render(Args, RenderedArgs);
        CompilerInvocation::buildDebugFlags(Opts.DebugFlags,
                                            RenderedArgs, SDKPath,
                                            ResourceDir);
      }
      // TODO: Should we support -fdebug-compilation-dir?
      llvm::SmallString<256> cwd;
      llvm::sys::fs::current_path(cwd);
      Opts.DebugCompilationDir = cwd.str();
    }
  }

  if (const Arg *A = Args.getLastArg(options::OPT_debug_info_format)) {
    if (A->containsValue("dwarf"))
      Opts.DebugInfoFormat = IRGenDebugInfoFormat::DWARF;
    else if (A->containsValue("codeview"))
      Opts.DebugInfoFormat = IRGenDebugInfoFormat::CodeView;
    else
      Diags.diagnose(SourceLoc(), diag::error_invalid_arg_value,
                     A->getAsString(Args), A->getValue());
  } else if (Opts.DebugInfoLevel > IRGenDebugInfoLevel::None) {
    // If -g was specified but not -debug-info-format, DWARF is assumed.
    Opts.DebugInfoFormat = IRGenDebugInfoFormat::DWARF;
  }
  if (Args.hasArg(options::OPT_debug_info_format) &&
      !Args.hasArg(options::OPT_g_Group)) {
    const Arg *debugFormatArg = Args.getLastArg(options::OPT_debug_info_format);
    Diags.diagnose(SourceLoc(), diag::error_option_missing_required_argument,
                   debugFormatArg->getAsString(Args), "-g");
  }
  if (Opts.DebugInfoFormat == IRGenDebugInfoFormat::CodeView &&
      (Opts.DebugInfoLevel == IRGenDebugInfoLevel::LineTables ||
       Opts.DebugInfoLevel == IRGenDebugInfoLevel::DwarfTypes)) {
    const Arg *debugFormatArg = Args.getLastArg(options::OPT_debug_info_format);
    Diags.diagnose(SourceLoc(), diag::error_argument_not_allowed_with,
                   debugFormatArg->getAsString(Args),
                   Opts.DebugInfoLevel == IRGenDebugInfoLevel::LineTables
                     ? "-gline-tables-only"
                     : "-gdwarf_types");
  }

  for (auto A : Args.getAllArgValues(options::OPT_debug_prefix_map)) {
    auto SplitMap = StringRef(A).split('=');
    Opts.DebugPrefixMap.addMapping(SplitMap.first, SplitMap.second);
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
  Opts.DisableSwiftSpecificLLVMOptzns |=
      Args.hasArg(OPT_disable_swift_specific_llvm_optzns);
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

  Opts.EnableDynamicReplacementChaining |=
      Args.hasArg(OPT_enable_dynamic_replacement_chaining);

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

  if (Args.hasArg(OPT_enable_class_resilience)) {
    Opts.EnableClassResilience = true;
  }

  if (Args.hasArg(OPT_enable_resilience_bypass)) {
    Opts.EnableResilienceBypass = true;
  }

  // PE/COFF cannot deal with the cross-module reference to the metadata parent
  // (e.g. NativeObject).  Force the lazy initialization of the VWT always.
  Opts.LazyInitializeClassMetadata = Triple.isOSBinFormatCOFF();

  if (const Arg *A = Args.getLastArg(OPT_read_type_info_path_EQ)) {
    Opts.ReadTypeInfoPath = A->getValue();
  }

  for (const auto &Lib : Args.getAllArgValues(options::OPT_autolink_library))
    Opts.LinkLibraries.push_back(LinkLibrary(Lib, LibraryKind::Library));

  if (const Arg *A = Args.getLastArg(OPT_type_info_dump_filter_EQ)) {
    StringRef mode(A->getValue());
    if (mode == "all")
      Opts.TypeInfoFilter = IRGenOptions::TypeInfoDumpFilter::All;
    else if (mode == "resilient")
      Opts.TypeInfoFilter = IRGenOptions::TypeInfoDumpFilter::Resilient;
    else if (mode == "fragile")
      Opts.TypeInfoFilter = IRGenOptions::TypeInfoDumpFilter::Fragile;
    else {
      Diags.diagnose(SourceLoc(), diag::error_invalid_arg_value,
                     A->getAsString(Args), A->getValue());
    }
  }

  return false;
}

static std::string getScriptFileName(StringRef name) {
  return (Twine(name) + "4" + ".json").str();
}

static bool ParseMigratorArgs(MigratorOptions &Opts,
                              LangOptions &LangOpts,
                              const FrontendOptions &FrontendOpts,
                              StringRef ResourcePath, const ArgList &Args,
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
    auto &Triple = LangOpts.Target;

    llvm::SmallString<128> basePath;
    if (auto DataDir = Args.getLastArg(OPT_api_diff_data_dir)) {
      basePath = DataDir->getValue();
    } else {
      basePath = ResourcePath;
      llvm::sys::path::append(basePath, "migrator");
    }

    bool Supported = true;
    llvm::SmallString<128> dataPath(basePath);

    if (Triple.isMacOSX())
      llvm::sys::path::append(dataPath, getScriptFileName("macos"));
    else if (Triple.isiOS())
      llvm::sys::path::append(dataPath, getScriptFileName("ios"));
    else if (Triple.isTvOS())
      llvm::sys::path::append(dataPath, getScriptFileName("tvos"));
    else if (Triple.isWatchOS())
      llvm::sys::path::append(dataPath, getScriptFileName("watchos"));
    else
      Supported = false;
    if (Supported) {
      llvm::SmallString<128> authoredDataPath(basePath);
      llvm::sys::path::append(authoredDataPath, getScriptFileName("overlay"));
      // Add authored list first to take higher priority.
      Opts.APIDigesterDataStorePaths.push_back(authoredDataPath.str());
      Opts.APIDigesterDataStorePaths.push_back(dataPath.str());
    }
  }

  if (Opts.shouldRunMigrator()) {
    assert(!FrontendOpts.InputsAndOutputs.isWholeModule());
    // FIXME: In order to support batch mode properly, the migrator would have
    // to support having one remap file path and one migrated file path per
    // primary input. The easiest way to do this would be to move processing of
    // these paths into FrontendOptions, like other supplementary outputs, and
    // to call migrator::updateCodeAndEmitRemapIfNeeded once for each primary
    // file.
    //
    // Supporting WMO would be similar, but WMO is set up to only produce one
    // supplementary output for the whole compilation instead of one per input,
    // so it's probably not worth it.
    FrontendOpts.InputsAndOutputs.assertMustNotBeMoreThanOnePrimaryInput();

    // Always disable typo-correction in the migrator.
    LangOpts.TypoCorrectionLimit = 0;
  }

  return false;
}

bool CompilerInvocation::parseArgs(
    ArrayRef<const char *> Args,
    DiagnosticEngine &Diags,
    SmallVectorImpl<std::unique_ptr<llvm::MemoryBuffer>>
        *ConfigurationFileBuffers,
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

  if (ParseFrontendArgs(FrontendOpts, ParsedArgs, Diags,
                        ConfigurationFileBuffers)) {
    return true;
  }

  SaveParseableInterfaceArgs(ParseableInterfaceOpts, FrontendOpts,
                             ParsedArgs, Diags);

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

  if (ParseTBDGenArgs(TBDGenOpts, ParsedArgs, Diags, *this)) {
    return true;
  }

  if (ParseDiagnosticArgs(DiagnosticOpts, ParsedArgs, Diags)) {
    return true;
  }

  if (ParseMigratorArgs(MigratorOpts, LangOpts, FrontendOpts,
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

  LangOpts.EffectiveLanguageVersion = info.compatibilityVersion;
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
    StringRef inputFilename, StringRef moduleNameArg,
    bool alwaysSetModuleToMain, bool bePrimary,
    serialization::ExtendedValidationInfo &extendedInfo) {
  // Load the input file.
  llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>> fileBufOrErr =
      llvm::MemoryBuffer::getFileOrSTDIN(inputFilename);
  if (!fileBufOrErr) {
    return fileBufOrErr;
  }

  // If it looks like we have an AST, set the source file kind to SIL and the
  // name of the module to the file's name.
  getFrontendOptions().InputsAndOutputs.addInput(
      InputFile(inputFilename, bePrimary, fileBufOrErr.get().get()));

  auto result = serialization::validateSerializedAST(
      fileBufOrErr.get()->getBuffer(), &extendedInfo);
  bool hasSerializedAST = result.status == serialization::Status::Valid;

  if (hasSerializedAST) {
    const StringRef stem = !moduleNameArg.empty()
                               ? moduleNameArg
                               : llvm::sys::path::stem(inputFilename);
    setModuleName(stem);
    setInputKind(InputFileKind::SwiftLibrary);
  } else {
    const StringRef name = (alwaysSetModuleToMain || moduleNameArg.empty())
                               ? "main"
                               : moduleNameArg;
    setModuleName(name);
    setInputKind(InputFileKind::SIL);
  }
  return fileBufOrErr;
}
