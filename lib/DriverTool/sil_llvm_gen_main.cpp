//===--- sil_llvm_gen_main.cpp --------------------------------------------===//
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
///
/// \file
///
/// This is a tool for reading sil files and running IRGen passes upon them. It
/// is not meant to be used to run llvm optimizations on llvm-ir.
///
//===----------------------------------------------------------------------===//

#include "swift/AST/DiagnosticsFrontend.h"
#include "swift/AST/IRGenRequests.h"
#include "swift/AST/SILOptions.h"
#include "swift/Basic/Assertions.h"
#include "swift/Basic/LLVMInitialize.h"
#include "swift/Basic/QuotedString.h"
#include "swift/Frontend/DiagnosticVerifier.h"
#include "swift/Frontend/Frontend.h"
#include "swift/Frontend/PrintingDiagnosticConsumer.h"
#include "swift/IRGen/IRGenPublic.h"
#include "swift/IRGen/IRGenSILPasses.h"
#include "swift/SILOptimizer/Analysis/Analysis.h"
#include "swift/SILOptimizer/PassManager/PassManager.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/Serialization/SerializationOptions.h"
#include "swift/Serialization/SerializedModuleLoader.h"
#include "swift/Serialization/SerializedSILLoader.h"
#include "swift/Strings.h"
#include "swift/Subsystems.h"
#include "llvm/ADT/ScopeExit.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/ManagedStatic.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/Signals.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/VirtualOutputBackends.h"
#include <cstdio>
using namespace swift;

namespace {
enum class SILOptStrictConcurrency {
  None = 0,
  Complete,
  Targeted,
  Minimal,
};
}

static std::optional<StrictConcurrency>
convertSILOptToRawStrictConcurrencyLevel(SILOptStrictConcurrency level) {
  switch (level) {
  case SILOptStrictConcurrency::None:
    return {};
  case SILOptStrictConcurrency::Complete:
    return StrictConcurrency::Complete;
  case SILOptStrictConcurrency::Targeted:
    return StrictConcurrency::Targeted;
  case SILOptStrictConcurrency::Minimal:
    return StrictConcurrency::Minimal;
  }
}

struct SILLLVMGenOptions {
  llvm::cl::opt<std::string>
    InputFilename = llvm::cl::opt<std::string>(llvm::cl::desc("input file"),
                                                  llvm::cl::init("-"),
                                                  llvm::cl::Positional);

  llvm::cl::opt<std::string>
    OutputFilename = llvm::cl::opt<std::string>("o", llvm::cl::init("-"), llvm::cl::desc("output filename"));

  llvm::cl::list<std::string>
    ImportPaths = llvm::cl::list<std::string>("I",
                  llvm::cl::desc("add a directory to the import search path"));

  llvm::cl::list<std::string>
    FrameworkPaths = llvm::cl::list<std::string>(
      "F", llvm::cl::desc("add a directory to the framework search path"));

  llvm::cl::list<std::string> VFSOverlays = llvm::cl::list<std::string>(
      "vfsoverlay", llvm::cl::desc("add a VFS overlay"));

  llvm::cl::opt<std::string>
    ModuleName = llvm::cl::opt<std::string>("module-name",
                 llvm::cl::desc("The name of the module if processing"
                                " a module. Necessary for processing "
                                "stdin."));

  llvm::cl::opt<bool> StrictImplicitModuleContext = llvm::cl::opt<bool>(
      "strict-implicit-module-context",
      llvm::cl::desc("Enable the strict forwarding of compilation "
                     "context to downstream implicit module dependencies"));

  llvm::cl::opt<bool> DisableImplicitModules =
      llvm::cl::opt<bool>("disable-implicit-swift-modules",
                          llvm::cl::desc("Disable implicit swift modules."));

  llvm::cl::opt<std::string> ExplicitSwiftModuleMapPath =
      llvm::cl::opt<std::string>(
          "explicit-swift-module-map-file",
          llvm::cl::desc("Explict swift module map file path"));

  llvm::cl::opt<std::string>
    ResourceDir = llvm::cl::opt<std::string>(
      "resource-dir",
      llvm::cl::desc("The directory that holds the compiler resource files"));

  llvm::cl::opt<std::string>
    SDKPath = llvm::cl::opt<std::string>("sdk", llvm::cl::desc("The path to the SDK for use with the clang "
                                                               "importer."),
              llvm::cl::init(""));

  llvm::cl::opt<std::string>
    Target = llvm::cl::opt<std::string>("target",
                                           llvm::cl::desc("target triple"));

  llvm::cl::opt<bool>
    PrintStats = llvm::cl::opt<bool>("print-stats", llvm::cl::desc("Print various statistics"));

  llvm::cl::opt<std::string>
    ModuleCachePath = llvm::cl::opt<std::string>("module-cache-path",
                      llvm::cl::desc("Clang module cache path"));

  llvm::cl::opt<bool>
    PerformWMO = llvm::cl::opt<bool>("wmo", llvm::cl::desc("Enable whole-module optimizations"));

  llvm::cl::opt<IRGenOutputKind> OutputKind = llvm::cl::opt<IRGenOutputKind>(
      "output-kind", llvm::cl::desc("Type of output to produce"),
      llvm::cl::values(
          clEnumValN(IRGenOutputKind::LLVMAssemblyBeforeOptimization, "llvm-as",
                     "Emit llvm assembly before optimization"),
          clEnumValN(IRGenOutputKind::LLVMAssemblyAfterOptimization,
                     "llvm-as-opt", "Emit llvm assembly after optimization"),
          clEnumValN(IRGenOutputKind::LLVMBitcode, "llvm-bc",
                     "Emit llvm bitcode"),
          clEnumValN(IRGenOutputKind::NativeAssembly, "as",
                     "Emit native assembly"),
          clEnumValN(IRGenOutputKind::ObjectFile, "object",
                     "Emit an object file")),
      llvm::cl::init(IRGenOutputKind::ObjectFile));

  llvm::cl::opt<bool>
    DisableLegacyTypeInfo = llvm::cl::opt<bool>("disable-legacy-type-info",
          llvm::cl::desc("Don't try to load backward deployment layouts"));

  llvm::cl::opt<std::string> SwiftVersionString = llvm::cl::opt<std::string>(
      "swift-version",
      llvm::cl::desc(
          "The swift version to assume AST declarations correspond to"));

  llvm::cl::opt<bool> EnableExperimentalConcurrency = llvm::cl::opt<bool>(
      "enable-experimental-concurrency",
      llvm::cl::desc("Enable experimental concurrency model."));

  llvm::cl::opt<llvm::cl::boolOrDefault> EnableExperimentalMoveOnly =
      llvm::cl::opt<llvm::cl::boolOrDefault>(
          "enable-experimental-move-only", llvm::cl::init(llvm::cl::BOU_UNSET),
          llvm::cl::desc("Enable experimental move-only semantics."));

  llvm::cl::list<std::string> ExperimentalFeatures =
      llvm::cl::list<std::string>(
          "enable-experimental-feature",
          llvm::cl::desc("Enable the given experimental feature."));

  llvm::cl::list<std::string> UpcomingFeatures = llvm::cl::list<std::string>(
      "enable-upcoming-feature",
      llvm::cl::desc("Enable the given upcoming feature."));

  llvm::cl::opt<bool> EnableCxxInterop = llvm::cl::opt<bool>(
      "enable-experimental-cxx-interop", llvm::cl::desc("Enable C++ interop."),
      llvm::cl::init(false));

  llvm::cl::opt<bool> EnableObjCInterop = llvm::cl::opt<bool>(
      "enable-objc-interop",
      llvm::cl::desc("Enable Objective-C interoperability."));

  llvm::cl::opt<bool> DisableObjCInterop = llvm::cl::opt<bool>(
      "disable-objc-interop",
      llvm::cl::desc("Disable Objective-C interoperability."));

  // Strict Concurrency
  llvm::cl::opt<SILOptStrictConcurrency> StrictConcurrencyLevel =
      llvm::cl::opt<SILOptStrictConcurrency>(
          "strict-concurrency", llvm::cl::desc("strict concurrency level"),
          llvm::cl::init(SILOptStrictConcurrency::None),
          llvm::cl::values(
              clEnumValN(SILOptStrictConcurrency::Complete, "complete",
                         "Enable complete strict concurrency"),
              clEnumValN(SILOptStrictConcurrency::Targeted, "targeted",
                         "Enable targeted strict concurrency"),
              clEnumValN(SILOptStrictConcurrency::Minimal, "minimal",
                         "Enable minimal strict concurrency"),
              clEnumValN(SILOptStrictConcurrency::None, "disabled",
                         "Strict concurrency disabled")));
};

static std::optional<bool> toOptionalBool(llvm::cl::boolOrDefault defaultable) {
  switch (defaultable) {
  case llvm::cl::BOU_TRUE:
    return true;
  case llvm::cl::BOU_FALSE:
    return false;
  case llvm::cl::BOU_UNSET:
    return std::nullopt;
  }
  llvm_unreachable("Bad case for llvm::cl::boolOrDefault!");
}

namespace {
class LLVMOptionFilter {
  /// Options with -Xllvm removed. We let through -Xllvm options so that LLVM's
  /// processing can find it.
  SmallVector<const char *, 32> filteredOptions;

  /// Options with the -Xllvm prefix. We have that here so we can process them
  /// separately.
  SmallVector<const char *, 32> llvmOptions;

public:
  LLVMOptionFilter(ArrayRef<const char *> argv) {
    for (unsigned i = 0; i < argv.size(); ++i) {
      // Not a -Xllvm option... just let it through.
      if (StringRef(argv[i]) != "-Xllvm") {
        filteredOptions.push_back(argv[i]);
        continue;
      }

      assert(i + 1 < argv.size() && "-Xllvm without a corresponding option");
      ++i;
      filteredOptions.push_back(argv[i]);
      llvmOptions.push_back(argv[i]);
    }
  }

  ArrayRef<const char *> getFilteredOptions() const { return filteredOptions; }
  ArrayRef<const char *> getLLVMOptions() const { return llvmOptions; }
};
} // namespace

int sil_llvm_gen_main(ArrayRef<const char *> argv, void *MainAddr) {
  INITIALIZE_LLVM();

  llvm::setBugReportMsg(SWIFT_CRASH_BUG_REPORT_MESSAGE "\n");
  llvm::EnablePrettyStackTraceOnSigInfoForThisThread();

  SILLLVMGenOptions options;
  LLVMOptionFilter filteredOptions(argv);

  CompilerInvocation Invocation;

  Invocation.setMainExecutablePath(llvm::sys::fs::getMainExecutable(argv[0], MainAddr));
  copy(filteredOptions.getLLVMOptions(),
       std::back_inserter(Invocation.getFrontendOptions().LLVMArgs));

  llvm::cl::ParseCommandLineOptions(filteredOptions.getFilteredOptions().size(),
                                    filteredOptions.getFilteredOptions().data(),
                                    "Swift LLVM IR Generator\n");

  if (options.PrintStats)
    llvm::EnableStatistics();

  // Give the context the list of search paths to use for modules.
  std::vector<SearchPathOptions::SearchPath> ImportPaths;
  for (const auto &path : options.ImportPaths) {
    ImportPaths.push_back({path, /*isSystem=*/false});
  }
  Invocation.setImportSearchPaths(ImportPaths);
  std::vector<SearchPathOptions::SearchPath> FramePaths;
  for (const auto &path : options.FrameworkPaths) {
    FramePaths.push_back({path, /*isSystem=*/false});
  }
  Invocation.setFrameworkSearchPaths(FramePaths);

  Invocation.setVFSOverlays(options.VFSOverlays);

  // Set the SDK path and target if given.
  if (options.SDKPath.getNumOccurrences() == 0) {
    const char *SDKROOT = getenv("SDKROOT");
    if (SDKROOT)
      options.SDKPath = SDKROOT;
  }
  if (!options.SDKPath.empty())
    Invocation.setSDKPath(options.SDKPath);
  if (!options.Target.empty())
    Invocation.setTargetTriple(options.Target);
  if (!options.ResourceDir.empty())
    Invocation.setRuntimeResourcePath(options.ResourceDir);

  Invocation.getFrontendOptions().StrictImplicitModuleContext =
      options.StrictImplicitModuleContext;

  Invocation.getFrontendOptions().DisableImplicitModules =
      options.DisableImplicitModules;
  Invocation.getSearchPathOptions().ExplicitSwiftModuleMapPath =
      options.ExplicitSwiftModuleMapPath;

  // Set the module cache path. If not passed in we use the default swift module
  // cache.
  Invocation.getClangImporterOptions().ModuleCachePath = options.ModuleCachePath;
  Invocation.setParseStdlib();

  // Setup the language options
  if (options.SwiftVersionString.size()) {
    auto vers = VersionParser::parseVersionString(options.SwiftVersionString,
                                                  SourceLoc(), nullptr);
    bool isValid = false;
    if (vers.has_value()) {
      if (auto effectiveVers = vers.value().getEffectiveLanguageVersion()) {
        Invocation.getLangOptions().EffectiveLanguageVersion =
            effectiveVers.value();
        isValid = true;
      }
    }
    if (!isValid) {
      llvm::errs() << "error: invalid swift version "
                   << options.SwiftVersionString << '\n';
      exit(-1);
    }
  }
  Invocation.getLangOptions().DisableAvailabilityChecking = true;
  Invocation.getLangOptions().EnableAccessControl = false;
  Invocation.getLangOptions().EnableObjCAttrRequiresFoundation = false;
  Invocation.getLangOptions().EnableDeserializationSafety = false;
  Invocation.getLangOptions().EnableExperimentalConcurrency =
      options.EnableExperimentalConcurrency;
  std::optional<bool> enableExperimentalMoveOnly =
      toOptionalBool(options.EnableExperimentalMoveOnly);
  if (enableExperimentalMoveOnly && *enableExperimentalMoveOnly) {
    // FIXME: drop addition of Feature::MoveOnly once its queries are gone.
    Invocation.getLangOptions().enableFeature(Feature::MoveOnly);
    Invocation.getLangOptions().enableFeature(Feature::NoImplicitCopy);
    Invocation.getLangOptions().enableFeature(
        Feature::OldOwnershipOperatorSpellings);
  }

  for (auto &featureName : options.UpcomingFeatures) {
    auto feature = Feature::getUpcomingFeature(featureName);
    if (!feature) {
      llvm::errs() << "error: unknown upcoming feature "
                   << QuotedString(featureName) << "\n";
      exit(-1);
    }

    if (auto firstVersion = feature->getLanguageVersion()) {
      if (Invocation.getLangOptions().isSwiftVersionAtLeast(*firstVersion)) {
        llvm::errs() << "error: upcoming feature " << QuotedString(featureName)
                     << " is already enabled as of Swift version "
                     << *firstVersion << '\n';
        exit(-1);
      }
    }
    Invocation.getLangOptions().enableFeature(*feature);
  }

  for (auto &featureName : options.ExperimentalFeatures) {
    if (auto feature = Feature::getExperimentalFeature(featureName)) {
      Invocation.getLangOptions().enableFeature(*feature);
    } else {
      llvm::errs() << "error: unknown experimental feature "
                   << QuotedString(featureName) << "\n";
      exit(-1);
    }
  }

  // Enable strict concurrency if we have the feature specified or if it was
  // specified via a command line option to sil-opt.
  if (Invocation.getLangOptions().hasFeature(Feature::StrictConcurrency)) {
    Invocation.getLangOptions().StrictConcurrencyLevel =
        StrictConcurrency::Complete;
  } else if (auto level = convertSILOptToRawStrictConcurrencyLevel(
                 options.StrictConcurrencyLevel)) {
    // If strict concurrency was enabled from the cmdline so the feature flag as
    // well.
    if (*level == StrictConcurrency::Complete)
      Invocation.getLangOptions().enableFeature(Feature::StrictConcurrency);
    Invocation.getLangOptions().StrictConcurrencyLevel = *level;
  }

  // If we have strict concurrency set as a feature and were told to turn off
  // region-based isolation... do so now.
  if (Invocation.getLangOptions().hasFeature(Feature::StrictConcurrency)) {
    Invocation.getLangOptions().enableFeature(Feature::RegionBasedIsolation);
  }

  Invocation.getLangOptions().EnableObjCInterop =
      options.EnableObjCInterop    ? true
      : options.DisableObjCInterop ? false
                                   : llvm::Triple(options.Target).isOSDarwin();

  Invocation.getLangOptions().EnableCXXInterop = options.EnableCxxInterop;
  Invocation.computeCXXStdlibOptions();

  // Setup the IRGen Options.
  IRGenOptions &Opts = Invocation.getIRGenOptions();
  Opts.OutputKind = options.OutputKind;
  Opts.DisableLegacyTypeInfo = options.DisableLegacyTypeInfo;
  Invocation.computeAArch64TBIOptions();

  serialization::ExtendedValidationInfo extendedInfo;
  llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>> FileBufOrErr =
      Invocation.setUpInputForSILTool(options.InputFilename, options.ModuleName,
                                      /*alwaysSetModuleToMain*/ false,
                                      /*bePrimary*/ !options.PerformWMO, extendedInfo);
  if (!FileBufOrErr) {
    fprintf(stderr, "Error! Failed to open file: %s\n", options.InputFilename.c_str());
    exit(-1);
  }

  CompilerInstance CI;
  PrintingDiagnosticConsumer PrintDiags;
  CI.addDiagnosticConsumer(&PrintDiags);

  std::string InstanceSetupError;
  if (CI.setup(Invocation, InstanceSetupError)) {
    llvm::errs() << InstanceSetupError << '\n';
    return 1;
  }

  llvm::vfs::OnDiskOutputBackend Backend;
  auto outFile = Backend.createFile(options.OutputFilename);
  if (!outFile) {
    CI.getDiags().diagnose(SourceLoc(), diag::error_opening_output,
                           options.OutputFilename, toString(outFile.takeError()));
    return 1;
  }
  auto closeFile = llvm::make_scope_exit([&]() {
    if (auto E = outFile->keep()) {
      CI.getDiags().diagnose(SourceLoc(), diag::error_closing_output,
                             options.OutputFilename, toString(std::move(E)));
    }
  });

  auto *mod = CI.getMainModule();
  assert(mod->getFiles().size() == 1);

  const auto &TBDOpts = Invocation.getTBDGenOptions();
  const auto &SILOpts = Invocation.getSILOptions();
  auto &SILTypes = CI.getSILTypes();
  auto moduleName = CI.getMainModule()->getName().str();
  const PrimarySpecificPaths PSPs(options.OutputFilename, options.InputFilename);

  auto getDescriptor = [&]() -> IRGenDescriptor {
    if (options.PerformWMO) {
      return IRGenDescriptor::forWholeModule(
          mod, Opts, TBDOpts, SILOpts, SILTypes,
          /*SILMod*/ nullptr, moduleName, PSPs);
    }

    return IRGenDescriptor::forFile(
        mod->getFiles()[0], Opts, TBDOpts, SILOpts, SILTypes,
        /*SILMod*/ nullptr, moduleName, PSPs, /*discriminator*/ "");
  };

  auto &eval = CI.getASTContext().evaluator;
  auto desc = getDescriptor();
  desc.out = &outFile->getOS();

  if (options.OutputKind == IRGenOutputKind::LLVMAssemblyBeforeOptimization) {
    auto generatedMod = evaluateOrFatal(eval, IRGenRequest{desc});
    if (!generatedMod)
      return 1;

    generatedMod.getModule()->print(*outFile, nullptr);
    return 0;
  }

  auto generatedMod = evaluateOrFatal(eval, OptimizedIRRequest{desc});
  if (!generatedMod)
    return 1;

  return compileAndWriteLLVM(generatedMod.getModule(),
                             generatedMod.getTargetMachine(), Opts,
                             CI.getStatsReporter(), CI.getDiags(), *outFile);
}
