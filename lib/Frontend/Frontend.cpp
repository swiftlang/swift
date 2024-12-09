//===--- Frontend.cpp - frontend utility methods --------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file contains utility methods for parsing and performing semantic
// on modules.
//
//===----------------------------------------------------------------------===//

#include "swift/Frontend/Frontend.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/DiagnosticsFrontend.h"
#include "swift/AST/DiagnosticsSema.h"
#include "swift/AST/FileSystem.h"
#include "swift/AST/Module.h"
#include "swift/AST/ModuleDependencies.h"
#include "swift/AST/PluginLoader.h"
#include "swift/AST/TypeCheckRequests.h"
#include "swift/Basic/Assertions.h"
#include "swift/Basic/FileTypes.h"
#include "swift/Basic/Platform.h"
#include "swift/Basic/SourceManager.h"
#include "swift/Basic/Statistic.h"
#include "swift/Frontend/CachingUtils.h"
#include "swift/Frontend/CompileJobCacheKey.h"
#include "swift/Frontend/ModuleInterfaceLoader.h"
#include "swift/Parse/Lexer.h"
#include "swift/SIL/SILModule.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/Utils/Generics.h"
#include "swift/Serialization/SerializationOptions.h"
#include "swift/Serialization/SerializedModuleLoader.h"
#include "swift/Serialization/ScanningLoaders.h"
#include "swift/DependencyScan/ModuleDependencyScanner.h"
#include "swift/Strings.h"
#include "swift/Subsystems.h"
#include "clang/AST/ASTContext.h"
#include "llvm/ADT/Hashing.h"
#include "llvm/ADT/IntrusiveRefCntPtr.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/TargetParser/Triple.h"
#include "llvm/CAS/ActionCache.h"
#include "llvm/CAS/BuiltinUnifiedCASDatabases.h"
#include "llvm/CAS/CASFileSystem.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Error.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/Process.h"
#include "llvm/Support/VirtualOutputBackends.h"
#include "llvm/Support/ThreadPool.h"
#include <llvm/ADT/StringExtras.h>

using namespace swift;

CompilerInstance::CompilerInstance() = default;
CompilerInstance::~CompilerInstance() = default;

std::string CompilerInvocation::getPCHHash() const {
  using llvm::hash_combine;

  auto Code = hash_combine(LangOpts.getPCHHashComponents(),
                           FrontendOpts.getPCHHashComponents(),
                           ClangImporterOpts.getPCHHashComponents(),
                           SearchPathOpts.getPCHHashComponents(),
                           DiagnosticOpts.getPCHHashComponents(),
                           SILOpts.getPCHHashComponents(),
                           IRGenOpts.getPCHHashComponents(),
                           CASOpts.getPCHHashComponents());

  return llvm::toString(llvm::APInt(64, Code), 36, /*Signed=*/false);
}

std::string CompilerInvocation::getModuleScanningHash() const {
  using llvm::hash_combine;

  auto Code = hash_combine(LangOpts.getModuleScanningHashComponents(),
                           FrontendOpts.getModuleScanningHashComponents(),
                           ClangImporterOpts.getModuleScanningHashComponents(),
                           SearchPathOpts.getModuleScanningHashComponents(),
                           DiagnosticOpts.getModuleScanningHashComponents(),
                           SILOpts.getModuleScanningHashComponents(),
                           IRGenOpts.getModuleScanningHashComponents(),
                           CASOpts.getModuleScanningHashComponents());

  return llvm::toString(llvm::APInt(64, Code), 36, /*Signed=*/false);
}

const PrimarySpecificPaths &
CompilerInvocation::getPrimarySpecificPathsForAtMostOnePrimary() const {
  return getFrontendOptions().getPrimarySpecificPathsForAtMostOnePrimary();
}

const PrimarySpecificPaths &
CompilerInvocation::getPrimarySpecificPathsForPrimary(
    StringRef filename) const {
  return getFrontendOptions().getPrimarySpecificPathsForPrimary(filename);
}

const PrimarySpecificPaths &
CompilerInvocation::getPrimarySpecificPathsForSourceFile(
    const SourceFile &SF) const {
  return getPrimarySpecificPathsForPrimary(SF.getFilename());
}

std::string CompilerInvocation::getOutputFilenameForAtMostOnePrimary() const {
  return getPrimarySpecificPathsForAtMostOnePrimary().OutputFilename;
}
std::string
CompilerInvocation::getMainInputFilenameForDebugInfoForAtMostOnePrimary()
    const {
  return getPrimarySpecificPathsForAtMostOnePrimary()
      .MainInputFilenameForDebugInfo;
}
std::string
CompilerInvocation::getClangHeaderOutputPathForAtMostOnePrimary() const {
  return getPrimarySpecificPathsForAtMostOnePrimary()
      .SupplementaryOutputs.ClangHeaderOutputPath;
}
std::string CompilerInvocation::getModuleOutputPathForAtMostOnePrimary() const {
  return getPrimarySpecificPathsForAtMostOnePrimary()
      .SupplementaryOutputs.ModuleOutputPath;
}
std::string CompilerInvocation::getReferenceDependenciesFilePathForPrimary(
    StringRef filename) const {
  return getPrimarySpecificPathsForPrimary(filename)
      .SupplementaryOutputs.ReferenceDependenciesFilePath;
}
std::string CompilerInvocation::getConstValuesFilePathForPrimary(
    StringRef filename) const {
  return getPrimarySpecificPathsForPrimary(filename)
      .SupplementaryOutputs.ConstValuesOutputPath;
}
std::string
CompilerInvocation::getSerializedDiagnosticsPathForAtMostOnePrimary() const {
  return getPrimarySpecificPathsForAtMostOnePrimary()
      .SupplementaryOutputs.SerializedDiagnosticsPath;
}
std::string CompilerInvocation::getTBDPathForWholeModule() const {
  assert(getFrontendOptions().InputsAndOutputs.isWholeModule() &&
         "TBDPath only makes sense when the whole module can be seen");
  return getPrimarySpecificPathsForAtMostOnePrimary()
      .SupplementaryOutputs.TBDPath;
}

std::string
CompilerInvocation::getModuleInterfaceOutputPathForWholeModule() const {
  assert(getFrontendOptions().InputsAndOutputs.isWholeModule() &&
         "ModuleInterfaceOutputPath only makes sense when the whole module "
         "can be seen");
  return getPrimarySpecificPathsForAtMostOnePrimary()
      .SupplementaryOutputs.ModuleInterfaceOutputPath;
}

std::string
CompilerInvocation::getPrivateModuleInterfaceOutputPathForWholeModule() const {
  assert(getFrontendOptions().InputsAndOutputs.isWholeModule() &&
         "PrivateModuleInterfaceOutputPath only makes sense when the whole "
         "module can be seen");
  return getPrimarySpecificPathsForAtMostOnePrimary()
      .SupplementaryOutputs.PrivateModuleInterfaceOutputPath;
}

std::string
CompilerInvocation::getPackageModuleInterfaceOutputPathForWholeModule() const {
  assert(getFrontendOptions().InputsAndOutputs.isWholeModule() &&
         "PackageModuleInterfaceOutputPath only makes sense when the whole "
         "module can be seen");
  return getPrimarySpecificPathsForAtMostOnePrimary()
      .SupplementaryOutputs.PackageModuleInterfaceOutputPath;
}

std::string CompilerInvocation::getAPIDescriptorPathForWholeModule() const {
  assert(
      getFrontendOptions().InputsAndOutputs.isWholeModule() &&
      "APIDescriptorPath only makes sense when the whole module can be seen");
  return getPrimarySpecificPathsForAtMostOnePrimary()
      .SupplementaryOutputs.APIDescriptorOutputPath;
}

SerializationOptions CompilerInvocation::computeSerializationOptions(
    const SupplementaryOutputPaths &outs, const ModuleDecl *module) const {
  const FrontendOptions &opts = getFrontendOptions();

  SerializationOptions serializationOpts;
  serializationOpts.OutputPath = outs.ModuleOutputPath;
  serializationOpts.DocOutputPath = outs.ModuleDocOutputPath;
  serializationOpts.SourceInfoOutputPath = outs.ModuleSourceInfoOutputPath;
  serializationOpts.GroupInfoPath = opts.GroupInfoPath.c_str();
  if (opts.SerializeBridgingHeader && !outs.ModuleOutputPath.empty())
    serializationOpts.ImportedHeader = opts.ImplicitObjCHeaderPath;
  serializationOpts.ModuleLinkName = opts.ModuleLinkName;
  serializationOpts.UserModuleVersion = opts.UserModuleVersion;
  serializationOpts.AllowableClients = opts.AllowableClients;
  serializationOpts.SerializeDebugInfoSIL = opts.SerializeDebugInfoSIL;

  serializationOpts.PublicDependentLibraries =
      getIRGenOptions().PublicLinkLibraries;
  serializationOpts.SDKName = getLangOptions().SDKName;
  serializationOpts.SDKVersion = swift::getSDKBuildVersion(
                                          getSearchPathOptions().getSDKPath());
  serializationOpts.ABIDescriptorPath = outs.ABIDescriptorOutputPath.c_str();
  serializationOpts.emptyABIDescriptor = opts.emptyABIDescriptor;

  if (!getIRGenOptions().ForceLoadSymbolName.empty())
    serializationOpts.AutolinkForceLoad = true;

  // Options contain information about the developer's computer,
  // so only serialize them if the module isn't going to be shipped to
  // the public.
  serializationOpts.SerializeOptionsForDebugging =
      opts.SerializeOptionsForDebugging.value_or(
          !module->isExternallyConsumed());

  serializationOpts.PathObfuscator = opts.serializedPathObfuscator;
  if (serializationOpts.SerializeOptionsForDebugging &&
      opts.DebugPrefixSerializedDebuggingOptions) {
    serializationOpts.DebuggingOptionsPrefixMap =
        getIRGenOptions().DebugPrefixMap;
    auto &remapper = serializationOpts.DebuggingOptionsPrefixMap;
    auto remapClangPaths = [&remapper](StringRef path) {
      return remapper.remapPath(path);
    };
    serializationOpts.ExtraClangOptions =
        getClangImporterOptions().getRemappedExtraArgs(remapClangPaths);
  } else {
    serializationOpts.ExtraClangOptions = getClangImporterOptions().ExtraArgs;
  }
  if (LangOpts.ClangTarget &&
      !getClangImporterOptions().DirectClangCC1ModuleBuild) {
    serializationOpts.ExtraClangOptions.push_back("--target=" +
                                                  LangOpts.ClangTarget->str());
  }
  if (LangOpts.EnableAppExtensionRestrictions) {
    serializationOpts.ExtraClangOptions.push_back("-fapplication-extension");
  }

  serializationOpts.PluginSearchOptions =
      getSearchPathOptions().PluginSearchOpts;

  serializationOpts.DisableCrossModuleIncrementalInfo =
      opts.DisableCrossModuleIncrementalBuild;

  serializationOpts.StaticLibrary = opts.Static;

  serializationOpts.HermeticSealAtLink = opts.HermeticSealAtLink;

  serializationOpts.EmbeddedSwiftModule =
      LangOpts.hasFeature(Feature::Embedded);

  serializationOpts.IsOSSA = getSILOptions().EnableOSSAModules;

  serializationOpts.SkipNonExportableDecls =
      getLangOptions().SkipNonExportableDecls;

  serializationOpts.ExplicitModuleBuild = FrontendOpts.DisableImplicitModules;

  serializationOpts.EnableSerializationRemarks =
      getLangOptions().EnableModuleSerializationRemarks;

  return serializationOpts;
}

Lowering::TypeConverter &CompilerInstance::getSILTypes() {
  if (auto *tc = TheSILTypes.get())
    return *tc;

  auto *tc = new Lowering::TypeConverter(
      *getMainModule(),
      /*loweredAddresses=*/!Context->SILOpts.EnableSILOpaqueValues);
  TheSILTypes.reset(tc);
  return *tc;
}

void CompilerInstance::recordPrimaryInputBuffer(unsigned BufID) {
  PrimaryBufferIDs.insert(BufID);
}

bool CompilerInstance::setUpASTContextIfNeeded() {
  if (FrontendOptions::doesActionBuildModuleFromInterface(
          Invocation.getFrontendOptions().RequestedAction) &&
      !Invocation.getFrontendOptions().ExplicitInterfaceBuild) {
    // Compiling a module interface from source uses its own CompilerInstance
    // with options read from the input file. Don't bother setting up an
    // ASTContext at this level.
    return false;
  }

  // For the time being, we only need to record dependencies in batch mode
  // and single file builds.
  Invocation.getLangOptions().RecordRequestReferences
    = !isWholeModuleCompilation();

  Context.reset(ASTContext::get(
      Invocation.getLangOptions(), Invocation.getTypeCheckerOptions(),
      Invocation.getSILOptions(), Invocation.getSearchPathOptions(),
      Invocation.getClangImporterOptions(), Invocation.getSymbolGraphOptions(),
      Invocation.getCASOptions(), Invocation.getSerializationOptions(),
      SourceMgr, Diagnostics, OutputBackend));
  if (!Invocation.getFrontendOptions().ModuleAliasMap.empty())
    Context->setModuleAliases(Invocation.getFrontendOptions().ModuleAliasMap);

  registerParseRequestFunctions(Context->evaluator);
  registerTypeCheckerRequestFunctions(Context->evaluator);
  registerClangImporterRequestFunctions(Context->evaluator);
  registerConstExtractRequestFunctions(Context->evaluator);
  registerSILGenRequestFunctions(Context->evaluator);
  registerSILOptimizerRequestFunctions(Context->evaluator);
  registerTBDGenRequestFunctions(Context->evaluator);
  registerIRGenRequestFunctions(Context->evaluator);
  
  // Migrator, indexing and typo correction need some IDE requests.
  // The integrated REPL needs IDE requests for completion.
  if (Invocation.getMigratorOptions().shouldRunMigrator() ||
      !Invocation.getFrontendOptions().IndexStorePath.empty() ||
      Invocation.getLangOptions().TypoCorrectionLimit ||
      Invocation.getFrontendOptions().RequestedAction ==
          FrontendOptions::ActionType::REPL) {
    registerIDERequestFunctions(Context->evaluator);
  }

  registerIRGenSILTransforms(*Context);

  if (Invocation.getFrontendOptions().RequestedAction ==
        FrontendOptions::ActionType::MergeModules ||
      Invocation.getLangOptions().DebuggerSupport)
    Invocation.getLangOptions().EnableDeserializationSafety = false;

  if (setUpModuleLoaders())
    return true;
  if (setUpPluginLoader())
    return true;

  return false;
}

void CompilerInstance::setupStatsReporter() {
  const auto &Invoke = getInvocation();
  const std::string &StatsOutputDir =
      Invoke.getFrontendOptions().StatsOutputDir;
  if (StatsOutputDir.empty())
    return;

  auto silOptModeArgStr = [](OptimizationMode mode) -> StringRef {
    switch (mode) {
    case OptimizationMode::ForSpeed:
      return "O";
    case OptimizationMode::ForSize:
      return "Osize";
    default:
      return "Onone";
    }
  };

  auto getClangSourceManager = [](ASTContext &Ctx) -> clang::SourceManager * {
    if (auto *clangImporter = static_cast<ClangImporter *>(
            Ctx.getClangModuleLoader())) {
      return &clangImporter->getClangASTContext().getSourceManager();
    }
    return nullptr;
  };

  const auto &FEOpts = Invoke.getFrontendOptions();
  const auto &LangOpts = Invoke.getLangOptions();
  const auto &SILOpts = Invoke.getSILOptions();
  const std::string &OutFile =
      FEOpts.InputsAndOutputs.lastInputProducingOutput().outputFilename();
  auto Reporter = std::make_unique<UnifiedStatsReporter>(
      "swift-frontend",
      FEOpts.ModuleName,
      FEOpts.InputsAndOutputs.getStatsFileMangledInputName(),
      LangOpts.Target.normalize(),
      llvm::sys::path::extension(OutFile),
      silOptModeArgStr(SILOpts.OptMode),
      StatsOutputDir,
      &getSourceMgr(),
      getClangSourceManager(getASTContext()),
      Invoke.getFrontendOptions().FineGrainedTimers,
      Invoke.getFrontendOptions().TraceStats,
      Invoke.getFrontendOptions().ProfileEvents,
      Invoke.getFrontendOptions().ProfileEntities,
      Invoke.getFrontendOptions().PrintZeroStats);
  // Hand the stats reporter down to the ASTContext so the rest of the compiler
  // can use it.
  getASTContext().setStatsReporter(Reporter.get());
  Diagnostics.setStatsReporter(Reporter.get());
  Stats = std::move(Reporter);
}

bool CompilerInstance::setupDiagnosticVerifierIfNeeded() {
  auto &diagOpts = Invocation.getDiagnosticOptions();
  bool hadError = false;

  if (diagOpts.VerifyMode != DiagnosticOptions::NoVerify) {
    DiagVerifier = std::make_unique<DiagnosticVerifier>(
        SourceMgr, InputSourceCodeBufferIDs,
        diagOpts.VerifyMode == DiagnosticOptions::VerifyAndApplyFixes,
        diagOpts.VerifyIgnoreUnknown, diagOpts.UseColor,
        diagOpts.AdditionalDiagnosticVerifierPrefixes);
    for (const auto &filename : diagOpts.AdditionalVerifierFiles) {
      auto result = getFileSystem().getBufferForFile(filename);
      if (!result) {
        Diagnostics.diagnose(SourceLoc(), diag::error_open_input_file,
                             filename, result.getError().message());
        hadError |= true;
        continue;
      }

      auto bufferID = SourceMgr.addNewSourceBuffer(std::move(result.get()));
      DiagVerifier->appendAdditionalBufferID(bufferID);
    }

    addDiagnosticConsumer(DiagVerifier.get());
  }

  return hadError;
}

void CompilerInstance::setupDependencyTrackerIfNeeded() {
  assert(!Context && "Must be called before the ASTContext is created");

  const auto &Invocation = getInvocation();
  const auto &opts = Invocation.getFrontendOptions();

  // Note that we may track dependencies even when we don't need to write them
  // directly; in particular, -track-system-dependencies affects how module
  // interfaces get loaded, and so we need to be consistently tracking system
  // dependencies throughout the compiler.
  auto collectionMode = opts.IntermoduleDependencyTracking;
  if (!collectionMode) {
    // If we have an output path specified, but no other tracking options,
    // default to non-system dependency tracking.
    if (opts.InputsAndOutputs.hasDependencyTrackerPath() ||
        !opts.IndexStorePath.empty()) {
      collectionMode = IntermoduleDepTrackingMode::ExcludeSystem;
    }
  }
  if (!collectionMode)
    return;

  DepTracker = std::make_unique<DependencyTracker>(*collectionMode);
}

bool CompilerInstance::setupCASIfNeeded(ArrayRef<const char *> Args) {
  if (!getInvocation().requiresCAS())
    return false;

  const auto &Opts = getInvocation().getCASOptions();
  auto MaybeDB= Opts.CASOpts.getOrCreateDatabases();
  if (!MaybeDB) {
    Diagnostics.diagnose(SourceLoc(), diag::error_cas,
                         toString(MaybeDB.takeError()));
    return true;
  }
  std::tie(CAS, ResultCache) = *MaybeDB;

  // create baseline key.
  auto BaseKey = createCompileJobBaseCacheKey(*CAS, Args);
  if (!BaseKey) {
    Diagnostics.diagnose(SourceLoc(), diag::error_cas,
                         toString(BaseKey.takeError()));
    return true;
  }
  CompileJobBaseKey = *BaseKey;
  return false;
}

void CompilerInstance::setupOutputBackend() {
  // Skip if output backend is not setup, default to OnDiskOutputBackend.
  if (OutputBackend)
    return;

  OutputBackend =
      llvm::makeIntrusiveRefCnt<llvm::vfs::OnDiskOutputBackend>();

  // Mirror the output into CAS.
  if (supportCaching()) {
    auto &InAndOuts = Invocation.getFrontendOptions().InputsAndOutputs;
    CASOutputBackend = createSwiftCachingOutputBackend(
        *CAS, *ResultCache, *CompileJobBaseKey, InAndOuts,
        Invocation.getFrontendOptions(),
        Invocation.getFrontendOptions().RequestedAction);

    if (Invocation.getIRGenOptions().UseCASBackend) {
      auto OutputFiles = InAndOuts.copyOutputFilenames();
      std::unordered_set<std::string> OutputFileSet(
          std::make_move_iterator(OutputFiles.begin()),
          std::make_move_iterator(OutputFiles.end()));
      // Filter the object file output if MCCAS is enabled, we do not want to
      // store the object file itself, but store the MCCAS CASID instead.
      auto FilterBackend = llvm::vfs::makeFilteringOutputBackend(
          CASOutputBackend,
          [&, OutputFileSet](StringRef Path,
                             std::optional<llvm::vfs::OutputConfig> Config) {
            if (InAndOuts.getPrincipalOutputType() != file_types::ID::TY_Object)
              return true;
            return !(OutputFileSet.find(Path.str()) != OutputFileSet.end());
          });
      OutputBackend =
          llvm::vfs::makeMirroringOutputBackend(OutputBackend, FilterBackend);
      return;
    }

    OutputBackend =
        llvm::vfs::makeMirroringOutputBackend(OutputBackend, CASOutputBackend);
  }

  // Setup verification backend.
  // Create a mirroring outputbackend to produce hash for output files.
  // We cannot skip disk here since swift compiler is expecting to read back
  // some output file in later stages.
  if (Invocation.getFrontendOptions().DeterministicCheck) {
    HashBackend = llvm::makeIntrusiveRefCnt<HashBackendTy>();
    OutputBackend =
        llvm::vfs::makeMirroringOutputBackend(OutputBackend, HashBackend);
  }
}

void CompilerInstance::setupCachingDiagnosticsProcessorIfNeeded() {
  if (!supportCaching())
    return;

  // Only setup if using CAS.
  CDP = std::make_unique<CachingDiagnosticsProcessor>(*this);
  CDP->startDiagnosticCapture();
}

bool CompilerInstance::setup(const CompilerInvocation &Invoke,
                             std::string &Error, ArrayRef<const char *> Args) {
  Invocation = Invoke;

  if (setupCASIfNeeded(Args)) {
    Error = "Setting up CAS failed";
    return true;
  }

  setupDependencyTrackerIfNeeded();
  setupOutputBackend();

  // If initializing the overlay file system fails there's no sense in
  // continuing because the compiler will read the wrong files.
  if (setUpVirtualFileSystemOverlays()) {
    Error = "Setting up virtual file system overlays failed";
    return true;
  }
  setUpLLVMArguments();
  setUpDiagnosticOptions();

  assert(Lexer::isIdentifier(Invocation.getModuleName()));

  if (setUpInputs()) {
    Error = "Setting up inputs failed";
    return true;
  }

  if (setUpASTContextIfNeeded()) {
    Error = "Setting up ASTContext failed";
    return true;
  }

  if (hasASTContext()) {
    setupStatsReporter();
  }

  if (setupDiagnosticVerifierIfNeeded()) {
    Error = "Setting up diagnostics verifier failed";
    return true;
  }

  // Setup caching diagnostics processor. It should be setup after all other
  // DiagConsumers are added.
  setupCachingDiagnosticsProcessorIfNeeded();

  // Dump module search paths if -Rmodule-loading is on.
  const auto &LangOpts = Invocation.getLangOptions();
  if (LangOpts.EnableModuleLoadingRemarks) {
    Invocation.getSearchPathOptions().dump(LangOpts.Target.isOSDarwin());
  }

  if (LangOpts.OpenSourcesAsVolatile) {
    this->getSourceMgr().setOpenSourcesAsVolatile();
  }

  // If we expect an implicit stdlib import, load in the standard library. If we
  // either fail to find it or encounter an error while loading it, bail early. Continuing will at best
  // trigger a bunch of other errors due to the stdlib being missing, or at
  // worst crash downstream as many call sites don't currently handle a missing
  // stdlib.
  if (loadStdlibIfNeeded()) {
    Error = "Loading the standard library failed";
    return true;
  }

  return false;
}

bool CompilerInstance::setupForReplay(const CompilerInvocation &Invoke,
                                      std::string &Error,
                                      ArrayRef<const char *> Args) {
  // This is the fast path for setup an instance for replay but cannot run
  // regular compilation.
  Invocation = Invoke;

  if (setupCASIfNeeded(Args)) {
    Error = "Setting up CAS failed";
    return true;
  }

  setupOutputBackend();
  setupCachingDiagnosticsProcessorIfNeeded();
  return false;
}

bool CompilerInstance::setUpVirtualFileSystemOverlays() {
  const auto &CASOpts = getInvocation().getCASOptions();
  if (CASOpts.EnableCaching && !CASOpts.HasImmutableFileSystem &&
      FrontendOptions::supportCompilationCaching(
          Invocation.getFrontendOptions().RequestedAction)) {
    Diagnostics.diagnose(SourceLoc(), diag::error_caching_no_cas_fs);
    return true;
  }

  if (Invocation.getCASOptions().requireCASFS()) {
    if (!CASOpts.CASFSRootIDs.empty() || !CASOpts.ClangIncludeTrees.empty() ||
        !CASOpts.ClangIncludeTreeFileList.empty()) {
      // Set up CASFS as BaseFS.
      auto FS = createCASFileSystem(*CAS, CASOpts.CASFSRootIDs,
                                    CASOpts.ClangIncludeTrees,
                                    CASOpts.ClangIncludeTreeFileList);
      if (!FS) {
        Diagnostics.diagnose(SourceLoc(), diag::error_cas,
                             toString(FS.takeError()));
        return true;
      }
      SourceMgr.setFileSystem(std::move(*FS));
    }

    // If we need to load any files from CAS, try load it now and overlay it.
    llvm::IntrusiveRefCntPtr<llvm::vfs::InMemoryFileSystem> MemFS =
        new llvm::vfs::InMemoryFileSystem();
    const auto &ClangOpts = getInvocation().getClangImporterOptions();

    if (!CASOpts.BridgingHeaderPCHCacheKey.empty()) {
      if (auto loadedBuffer = loadCachedCompileResultFromCacheKey(
              getObjectStore(), getActionCache(), Diagnostics,
              CASOpts.BridgingHeaderPCHCacheKey, file_types::ID::TY_PCH,
              ClangOpts.BridgingHeader))
        MemFS->addFile(Invocation.getClangImporterOptions().BridgingHeader, 0,
                       std::move(loadedBuffer));
      else
        Diagnostics.diagnose(
            SourceLoc(), diag::error_load_input_from_cas,
            Invocation.getClangImporterOptions().BridgingHeader);
    }
    if (!CASOpts.InputFileKey.empty()) {
      if (Invocation.getFrontendOptions()
              .InputsAndOutputs.getAllInputs()
              .size() != 1)
        Diagnostics.diagnose(SourceLoc(),
                             diag::error_wrong_input_num_for_input_file_key);
      else {
        auto InputPath = Invocation.getFrontendOptions()
                             .InputsAndOutputs.getFilenameOfFirstInput();
        auto Type = file_types::lookupTypeFromFilename(
            llvm::sys::path::filename(InputPath));
        if (auto loadedBuffer = loadCachedCompileResultFromCacheKey(
                getObjectStore(), getActionCache(), Diagnostics,
                CASOpts.InputFileKey, Type, InputPath))
          MemFS->addFile(InputPath, 0, std::move(loadedBuffer));
        else
          Diagnostics.diagnose(SourceLoc(), diag::error_load_input_from_cas,
                               InputPath);
      }
    }
    llvm::IntrusiveRefCntPtr<llvm::vfs::OverlayFileSystem> OverlayVFS =
        new llvm::vfs::OverlayFileSystem(SourceMgr.getFileSystem());
    OverlayVFS->pushOverlay(MemFS);
    SourceMgr.setFileSystem(std::move(OverlayVFS));
  }

  auto ExpectedOverlay =
      Invocation.getSearchPathOptions().makeOverlayFileSystem(
          SourceMgr.getFileSystem());
  if (!ExpectedOverlay) {
    llvm::handleAllErrors(
        ExpectedOverlay.takeError(), [&](const llvm::FileError &FE) {
          if (FE.convertToErrorCode() == std::errc::no_such_file_or_directory) {
            Diagnostics.diagnose(SourceLoc(), diag::cannot_open_file,
                                 FE.getFileName(), FE.messageWithoutFileInfo());
          } else {
            Diagnostics.diagnose(SourceLoc(), diag::invalid_vfs_overlay_file,
                                 FE.getFileName());
          }
        });
    return true;
  }

  SourceMgr.setFileSystem(*ExpectedOverlay);
  return false;
}

void CompilerInstance::setUpLLVMArguments() {
  // Dependency scanning has no need for LLVM options, and
  // must not use `llvm::cl::` utilities operating on global state
  // since dependency scanning is multi-threaded.
  if (Invocation.getFrontendOptions().RequestedAction !=
      FrontendOptions::ActionType::ScanDependencies) {
    // Honor -Xllvm.
    if (!Invocation.getFrontendOptions().LLVMArgs.empty()) {
      llvm::SmallVector<const char *, 4> Args;
      Args.push_back("swift (LLVM option parsing)");
      for (unsigned i = 0, e = Invocation.getFrontendOptions().LLVMArgs.size();
           i != e; ++i)
        Args.push_back(Invocation.getFrontendOptions().LLVMArgs[i].c_str());
      Args.push_back(nullptr);
      llvm::cl::ParseCommandLineOptions(Args.size()-1, Args.data());
    }
  }
}

void CompilerInstance::setUpDiagnosticOptions() {
  if (Invocation.getDiagnosticOptions().ShowDiagnosticsAfterFatalError) {
    Diagnostics.setShowDiagnosticsAfterFatalError();
  }
  if (Invocation.getDiagnosticOptions().SuppressWarnings) {
    Diagnostics.setSuppressWarnings(true);
  }
  if (Invocation.getDiagnosticOptions().SuppressRemarks) {
    Diagnostics.setSuppressRemarks(true);
  }
  Diagnostics.setWarningsAsErrorsRules(
      Invocation.getDiagnosticOptions().WarningsAsErrorsRules);
  Diagnostics.setPrintDiagnosticNamesMode(
      Invocation.getDiagnosticOptions().PrintDiagnosticNames);
  Diagnostics.setDiagnosticDocumentationPath(
      Invocation.getDiagnosticOptions().DiagnosticDocumentationPath);
  Diagnostics.setLanguageVersion(
      Invocation.getLangOptions().EffectiveLanguageVersion);
  if (!Invocation.getDiagnosticOptions().LocalizationCode.empty()) {
    Diagnostics.setLocalization(
        Invocation.getDiagnosticOptions().LocalizationCode,
        Invocation.getDiagnosticOptions().LocalizationPath);
  }
}

// The ordering of ModuleLoaders is important!
//
// 1. SourceLoader: This is a hack and only the compiler's tests are using it,
//    to avoid writing repetitive code involving generating modules/interfaces.
//    Ideally, we'd get rid of it.
// 2. MemoryBufferSerializedModuleLoader: This is used by LLDB, because it might
//    already have the module available in memory.
// 3. ExplicitSwiftModuleLoader: Loads a serialized module if it can, provided
//    this modules was specified as an explicit input to the compiler.
// 4. ModuleInterfaceLoader: Tries to find an up-to-date swiftmodule. If it
//    succeeds, it issues a particular "error" (see
//    [NOTE: ModuleInterfaceLoader-defer-to-ImplicitSerializedModuleLoader]),
//    which is interpreted by the overarching loader as a command to use the
//    ImplicitSerializedModuleLoader. If we failed to find a .swiftmodule,
//    this falls back to using an interface. Actual errors lead to diagnostics.
// 5. ImplicitSerializedModuleLoader: Loads a serialized module if it can.
//    Used for implicit loading of modules from the compiler's search paths.
// 6. ClangImporter: This must come after all the Swift module loaders because
//    in the presence of overlays and mixed-source frameworks, we want to prefer
//    the overlay or framework module over the underlying Clang module.
bool CompilerInstance::setUpModuleLoaders() {
  if (hasSourceImport()) {
    bool enableLibraryEvolution =
      Invocation.getFrontendOptions().EnableLibraryEvolution;
    Context->addModuleLoader(SourceLoader::create(*Context,
                                                  enableLibraryEvolution,
                                                  getDependencyTracker()));
  }
  auto MLM = Invocation.getSearchPathOptions().ModuleLoadMode;
  auto IgnoreSourceInfoFile =
    Invocation.getFrontendOptions().IgnoreSwiftSourceInfo;
  if (Invocation.getLangOptions().EnableMemoryBufferImporter) {
    auto MemoryBufferLoader = MemoryBufferSerializedModuleLoader::create(
        *Context, getDependencyTracker(), MLM, IgnoreSourceInfoFile);
    this->MemoryBufferLoader = MemoryBufferLoader.get();
    Context->addModuleLoader(std::move(MemoryBufferLoader));
  }

  // If using `-explicit-swift-module-map-file`, create the explicit loader
  // before creating `ClangImporter` because the entries in the map influence
  // the Clang flags. The loader is added to the context below.
  std::unique_ptr<SerializedModuleLoaderBase> ESML = nullptr;
  bool ExplicitModuleBuild =
      Invocation.getFrontendOptions().DisableImplicitModules;
  if (ExplicitModuleBuild ||
      !Invocation.getSearchPathOptions().ExplicitSwiftModuleMapPath.empty() ||
      !Invocation.getSearchPathOptions().ExplicitSwiftModuleInputs.empty()) {
    if (Invocation.getCASOptions().EnableCaching)
      ESML = ExplicitCASModuleLoader::create(
          *Context, getObjectStore(), getActionCache(), getDependencyTracker(),
          MLM, Invocation.getSearchPathOptions().ExplicitSwiftModuleMapPath,
          Invocation.getSearchPathOptions().ExplicitSwiftModuleInputs,
          IgnoreSourceInfoFile);
    else
      ESML = ExplicitSwiftModuleLoader::create(
          *Context, getDependencyTracker(), MLM,
          Invocation.getSearchPathOptions().ExplicitSwiftModuleMapPath,
          Invocation.getSearchPathOptions().ExplicitSwiftModuleInputs,
          IgnoreSourceInfoFile);
  }

  // Wire up the Clang importer. If the user has specified an SDK, use it.
  // Otherwise, we just keep it around as our interface to Clang's ABI
  // knowledge.
  std::unique_ptr<ClangImporter> clangImporter =
    ClangImporter::create(*Context, Invocation.getPCHHash(),
                          getDependencyTracker());
  if (!clangImporter) {
    Diagnostics.diagnose(SourceLoc(), diag::error_clang_importer_create_fail);
    return true;
  }

  // Configure ModuleInterfaceChecker for the ASTContext.
  auto CacheFromInvocation = getInvocation().getClangModuleCachePath();
  auto const &Clang = clangImporter->getClangInstance();
  std::string ModuleCachePath = CacheFromInvocation.empty()
                                    ? getModuleCachePathFromClang(Clang)
                                    : CacheFromInvocation.str();
  auto &FEOpts = Invocation.getFrontendOptions();
  ModuleInterfaceLoaderOptions LoaderOpts(FEOpts);
  Context->addModuleInterfaceChecker(
      std::make_unique<ModuleInterfaceCheckerImpl>(
          *Context, ModuleCachePath, FEOpts.PrebuiltModuleCachePath,
          FEOpts.BackupModuleInterfaceDir, LoaderOpts,
          RequireOSSAModules_t(Invocation.getSILOptions())));

  // Install an explicit module loader if it was created earlier.
  if (ESML) {
    this->DefaultSerializedLoader = ESML.get();
    Context->addModuleLoader(std::move(ESML));
  }

  if (!ExplicitModuleBuild) {
    if (MLM != ModuleLoadingMode::OnlySerialized) {
      // We only need ModuleInterfaceLoader for implicit modules.
      auto PIML = ModuleInterfaceLoader::create(
          *Context, *static_cast<ModuleInterfaceCheckerImpl*>(Context
            ->getModuleInterfaceChecker()), getDependencyTracker(), MLM,
          FEOpts.PreferInterfaceForModules, IgnoreSourceInfoFile);
      Context->addModuleLoader(std::move(PIML), false, false, true);
    }
    std::unique_ptr<ImplicitSerializedModuleLoader> ISML =
    ImplicitSerializedModuleLoader::create(*Context, getDependencyTracker(), MLM,
                                   IgnoreSourceInfoFile);
    this->DefaultSerializedLoader = ISML.get();
    Context->addModuleLoader(std::move(ISML));
  }

  Context->addModuleLoader(std::move(clangImporter), /*isClang*/ true);

  // When scanning for dependencies, we must add the scanner placeholder loader in order to
  // handle ASTContext operations such as canImportModule
  if (Invocation.getFrontendOptions().RequestedAction ==
      FrontendOptions::ActionType::ScanDependencies) {
    auto ClangModuleCachePath = getModuleCachePathFromClang(
        Context->getClangModuleLoader()->getClangInstance());
    auto &FEOpts = Invocation.getFrontendOptions();
    ModuleInterfaceLoaderOptions LoaderOpts(FEOpts);
    InterfaceSubContextDelegateImpl ASTDelegate(
        Context->SourceMgr, &Context->Diags, Context->SearchPathOpts,
        Context->LangOpts, Context->ClangImporterOpts, Context->CASOpts,
        LoaderOpts,
        /*buildModuleCacheDirIfAbsent*/ false, ClangModuleCachePath,
        FEOpts.PrebuiltModuleCachePath, FEOpts.BackupModuleInterfaceDir,
        FEOpts.SerializeModuleInterfaceDependencyHashes,
        FEOpts.shouldTrackSystemDependencies(),
        RequireOSSAModules_t(Invocation.getSILOptions()));
    auto mainModuleName = Context->getIdentifier(FEOpts.ModuleName);
    std::unique_ptr<PlaceholderSwiftModuleScanner> PSMS =
        std::make_unique<PlaceholderSwiftModuleScanner>(
            *Context, MLM, mainModuleName,
            Context->SearchPathOpts.PlaceholderDependencyModuleMap, ASTDelegate,
            getInvocation().getFrontendOptions().ExplicitModulesOutputPath);
    Context->addModuleLoader(std::move(PSMS));
  }

  return false;
}

bool CompilerInstance::setUpPluginLoader() {
  /// FIXME: If Invocation has 'PluginRegistry', we can set it. But should we?
  auto loader = std::make_unique<PluginLoader>(
      *Context, getDependencyTracker(),
      Invocation.getFrontendOptions().DisableSandbox);
  Context->setPluginLoader(std::move(loader));
  return false;
}

std::optional<unsigned> CompilerInstance::setUpIDEInspectionTargetBuffer() {
  std::optional<unsigned> ideInspectionTargetBufferID;
  auto ideInspectionTarget = Invocation.getIDEInspectionTarget();
  if (ideInspectionTarget.first) {
    auto memBuf = ideInspectionTarget.first;
    // CompilerInvocation doesn't own the buffers, copy to a new buffer.
    ideInspectionTargetBufferID = SourceMgr.addMemBufferCopy(memBuf);
    InputSourceCodeBufferIDs.push_back(*ideInspectionTargetBufferID);
    SourceMgr.setIDEInspectionTarget(*ideInspectionTargetBufferID,
                                    ideInspectionTarget.second);
  }
  return ideInspectionTargetBufferID;
}

SourceFile *CompilerInstance::getIDEInspectionFile() const {
  auto *mod = getMainModule();
  auto &eval = mod->getASTContext().evaluator;
  return evaluateOrDefault(eval, IDEInspectionFileRequest{mod}, nullptr);
}

static inline bool isPCHFilenameExtension(StringRef path) {
  return llvm::sys::path::extension(path)
    .ends_with(file_types::getExtension(file_types::TY_PCH));
}

std::string CompilerInstance::getBridgingHeaderPath() const {
  const FrontendOptions &opts = Invocation.getFrontendOptions();
  if (!isPCHFilenameExtension(opts.ImplicitObjCHeaderPath))
    return opts.ImplicitObjCHeaderPath;

  auto clangImporter =
      static_cast<ClangImporter *>(getASTContext().getClangModuleLoader());

  // No clang importer created. Report error?
  if (!clangImporter)
    return std::string();

  return clangImporter->getOriginalSourceFile(opts.ImplicitObjCHeaderPath);
}

bool CompilerInstance::setUpInputs() {
  // There is no input file when building PCM using ClangIncludeTree.
  if (Invocation.getFrontendOptions().RequestedAction ==
          FrontendOptions::ActionType::EmitPCM &&
      Invocation.getClangImporterOptions().HasClangIncludeTreeRoot)
    return false;

  // Adds to InputSourceCodeBufferIDs, so may need to happen before the
  // per-input setup.
  const std::optional<unsigned> ideInspectionTargetBufferID =
      setUpIDEInspectionTargetBuffer();

  const auto &Inputs =
      Invocation.getFrontendOptions().InputsAndOutputs.getAllInputs();
  const bool shouldRecover = Invocation.getFrontendOptions()
                                 .InputsAndOutputs.shouldRecoverMissingInputs();

  bool hasFailed = false;
  for (const InputFile &input : Inputs) {
    bool failed = false;
    std::optional<unsigned> bufferID =
        getRecordedBufferID(input, shouldRecover, failed);
    hasFailed |= failed;

    if (!bufferID.has_value() || !input.isPrimary())
      continue;

    recordPrimaryInputBuffer(*bufferID);
  }
  if (hasFailed)
    return true;

  // Set the primary file to the IDE inspection point if one exists.
  if (ideInspectionTargetBufferID.has_value() &&
      !isPrimaryInput(*ideInspectionTargetBufferID)) {
    assert(PrimaryBufferIDs.empty() && "re-setting PrimaryBufferID");
    recordPrimaryInputBuffer(*ideInspectionTargetBufferID);
  }

  return false;
}

std::optional<unsigned>
CompilerInstance::getRecordedBufferID(const InputFile &input,
                                      const bool shouldRecover, bool &failed) {
  if (!input.getBuffer()) {
    if (std::optional<unsigned> existingBufferID =
            SourceMgr.getIDForBufferIdentifier(input.getFileName())) {
      return existingBufferID;
    }
  }
  auto buffers = getInputBuffersIfPresent(input);

  // Recover by dummy buffer if requested.
  if (!buffers.has_value() && shouldRecover &&
      input.getType() == file_types::TY_Swift) {
    buffers = ModuleBuffers(llvm::MemoryBuffer::getMemBuffer(
        "// missing file\n", input.getFileName()));
  }

  if (!buffers.has_value()) {
    failed = true;
    return std::nullopt;
  }

  // FIXME: The fact that this test happens twice, for some cases,
  // suggests that setupInputs could use another round of refactoring.
  if (serialization::isSerializedAST(buffers->ModuleBuffer->getBuffer())) {
    PartialModules.push_back(std::move(*buffers));
    return std::nullopt;
  }
  assert(buffers->ModuleDocBuffer.get() == nullptr);
  assert(buffers->ModuleSourceInfoBuffer.get() == nullptr);
  // Transfer ownership of the MemoryBuffer to the SourceMgr.
  unsigned bufferID = SourceMgr.addNewSourceBuffer(std::move(buffers->ModuleBuffer));

  InputSourceCodeBufferIDs.push_back(bufferID);
  return bufferID;
}

std::optional<ModuleBuffers>
CompilerInstance::getInputBuffersIfPresent(const InputFile &input) {
  if (auto b = input.getBuffer()) {
    return ModuleBuffers(llvm::MemoryBuffer::getMemBufferCopy(b->getBuffer(),
                                                              b->getBufferIdentifier()));
  }
  // FIXME: Working with filenames is fragile, maybe use the real path
  // or have some kind of FileManager.
  using FileOrError = llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>>;
  // Avoid memory-mapping when the compiler is run for IDE inspection,
  // since that would prevent the user from saving the file.
  FileOrError inputFileOrErr =
    swift::vfs::getFileOrSTDIN(getFileSystem(), input.getFileName(),
                              /*FileSize*/-1,
                              /*RequiresNullTerminator*/true,
                              /*IsVolatile*/getInvocation().isIDEInspection(),
      /*Bad File Descriptor Retry*/getInvocation().getFrontendOptions()
                               .BadFileDescriptorRetryCount);
  if (!inputFileOrErr) {
    Diagnostics.diagnose(SourceLoc(), diag::error_open_input_file,
                         input.getFileName(),
                         inputFileOrErr.getError().message());
    return std::nullopt;
  }
  if (!serialization::isSerializedAST((*inputFileOrErr)->getBuffer()))
    return ModuleBuffers(std::move(*inputFileOrErr));

  auto swiftdoc = openModuleDoc(input);
  auto sourceinfo = openModuleSourceInfo(input);
  return ModuleBuffers(std::move(*inputFileOrErr),
                       swiftdoc.has_value() ? std::move(swiftdoc.value()) : nullptr,
                       sourceinfo.has_value() ? std::move(sourceinfo.value()) : nullptr);
}

std::optional<std::unique_ptr<llvm::MemoryBuffer>>
CompilerInstance::openModuleSourceInfo(const InputFile &input) {
  llvm::SmallString<128> pathWithoutProjectDir(input.getFileName());
  llvm::sys::path::replace_extension(pathWithoutProjectDir,
                  file_types::getExtension(file_types::TY_SwiftSourceInfoFile));
  llvm::SmallString<128> pathWithProjectDir = pathWithoutProjectDir.str();
  StringRef fileName = llvm::sys::path::filename(pathWithoutProjectDir);
  llvm::sys::path::remove_filename(pathWithProjectDir);
  llvm::sys::path::append(pathWithProjectDir, "Project");
  llvm::sys::path::append(pathWithProjectDir, fileName);
  if (auto sourceInfoFileOrErr = swift::vfs::getFileOrSTDIN(getFileSystem(),
                                                            pathWithProjectDir))
    return std::move(*sourceInfoFileOrErr);
  if (auto sourceInfoFileOrErr = swift::vfs::getFileOrSTDIN(getFileSystem(),
                                                            pathWithoutProjectDir))
    return std::move(*sourceInfoFileOrErr);
  return std::nullopt;
}

std::optional<std::unique_ptr<llvm::MemoryBuffer>>
CompilerInstance::openModuleDoc(const InputFile &input) {
  llvm::SmallString<128> moduleDocFilePath(input.getFileName());
  llvm::sys::path::replace_extension(
      moduleDocFilePath,
      file_types::getExtension(file_types::TY_SwiftModuleDocFile));
  using FileOrError = llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>>;
  FileOrError moduleDocFileOrErr =
      swift::vfs::getFileOrSTDIN(getFileSystem(), moduleDocFilePath);
  if (moduleDocFileOrErr)
    return std::move(*moduleDocFileOrErr);

  if (moduleDocFileOrErr.getError() == std::errc::no_such_file_or_directory)
    return std::unique_ptr<llvm::MemoryBuffer>();

  Diagnostics.diagnose(SourceLoc(), diag::error_open_input_file,
                       moduleDocFilePath,
                       moduleDocFileOrErr.getError().message());
  return std::nullopt;
}

/// Enable Swift concurrency on a per-target basis
static bool shouldImportConcurrencyByDefault(const llvm::Triple &target) {
  if (target.isOSDarwin())
    return true;
  if (target.isOSWindows())
    return true;
  if (target.isOSLinux())
    return true;
#if SWIFT_IMPLICIT_CONCURRENCY_IMPORT
  if (target.isOSWASI())
    return true;
  if (target.isOSOpenBSD())
    return true;
  if (target.isOSFreeBSD())
    return true;
#endif
  return false;
}

bool CompilerInvocation::shouldImportSwiftConcurrency() const {
  return shouldImportConcurrencyByDefault(getLangOptions().Target) &&
      !getLangOptions().DisableImplicitConcurrencyModuleImport &&
      getFrontendOptions().InputMode !=
        FrontendOptions::ParseInputMode::SwiftModuleInterface;
}

bool CompilerInvocation::shouldImportSwiftStringProcessing() const {
  return getLangOptions().EnableExperimentalStringProcessing &&
      !getLangOptions().DisableImplicitStringProcessingModuleImport &&
      getFrontendOptions().InputMode !=
        FrontendOptions::ParseInputMode::SwiftModuleInterface;
}

/// Enable Swift backtracing on a per-target basis
static bool shouldImportSwiftBacktracingByDefault(const llvm::Triple &target) {
  if (target.isOSDarwin() || target.isOSWindows() || target.isOSLinux())
    return true;
  return false;
}

bool CompilerInvocation::shouldImportSwiftBacktracing() const {
  return shouldImportSwiftBacktracingByDefault(getLangOptions().Target) &&
    !getLangOptions().DisableImplicitBacktracingModuleImport &&
    getFrontendOptions().InputMode !=
      FrontendOptions::ParseInputMode::SwiftModuleInterface;
}

bool CompilerInvocation::shouldImportCxx() const {
  // C++ Interop is disabled
  if (!getLangOptions().EnableCXXInterop)
    return false;
  // Avoid C++ stdlib when building Swift stdlib
  if (getImplicitStdlibKind() == ImplicitStdlibKind::Builtin)
    return false;
  // Avoid importing Cxx when building Cxx itself
  if (getFrontendOptions().ModuleName == CXX_MODULE_NAME)
    return false;
  // Cxx cannot be imported when Library evolution is enabled
  if (getFrontendOptions().EnableLibraryEvolution)
    return false;
  // Implicit import of Cxx is disabled
  if (getLangOptions().DisableImplicitCxxModuleImport)
    return false;

  return true;
}

/// Implicitly import the SwiftOnoneSupport module in non-optimized
/// builds. This allows for use of popular specialized functions
/// from the standard library, which makes the non-optimized builds
/// execute much faster.
bool CompilerInvocation::shouldImportSwiftONoneSupport() const {
  if (getImplicitStdlibKind() != ImplicitStdlibKind::Stdlib)
    return false;
  if (getSILOptions().shouldOptimize())
    return false;
  if (LangOpts.hasFeature(Feature::Embedded))
    return false;

  // If we are not executing an action that has a dependency on
  // SwiftOnoneSupport, don't load it.
  //
  // FIXME: Knowledge of SwiftOnoneSupport loading in the Frontend is a layering
  // violation. However, SIL currently does not have a way to express this
  // dependency itself for the benefit of autolinking.  In the mean time, we
  // will be conservative and say that actions like -emit-silgen and
  // -emit-sibgen - that don't really involve the optimizer - have a
  // strict dependency on SwiftOnoneSupport.
  //
  // This optimization is disabled by -track-system-dependencies to preserve
  // the explicit dependency.
  const auto &options = getFrontendOptions();
  return options.shouldTrackSystemDependencies() ||
         FrontendOptions::doesActionGenerateSIL(options.RequestedAction);
}

void CompilerInstance::verifyImplicitConcurrencyImport() {
  if (Invocation.shouldImportSwiftConcurrency() &&
      !canImportSwiftConcurrency()) {
    Diagnostics.diagnose(SourceLoc(),
                         diag::warn_implicit_concurrency_import_failed);
  }
}

bool CompilerInstance::canImportSwiftConcurrency() const {
  ImportPath::Module::Builder builder(
      getASTContext().getIdentifier(SWIFT_CONCURRENCY_NAME));
  auto modulePath = builder.get();
  return getASTContext().testImportModule(modulePath);
}

bool CompilerInstance::canImportSwiftConcurrencyShims() const {
  ImportPath::Module::Builder builder(
      getASTContext().getIdentifier(SWIFT_CONCURRENCY_SHIMS_NAME));
  auto modulePath = builder.get();
  return getASTContext().testImportModule(modulePath);
}

void CompilerInstance::verifyImplicitStringProcessingImport() {
  if (Invocation.shouldImportSwiftStringProcessing() &&
      !canImportSwiftStringProcessing()) {
    Diagnostics.diagnose(SourceLoc(),
                         diag::warn_implicit_string_processing_import_failed);
  }
}

bool CompilerInstance::canImportSwiftStringProcessing() const {
  ImportPath::Module::Builder builder(
      getASTContext().getIdentifier(SWIFT_STRING_PROCESSING_NAME));
  auto modulePath = builder.get();
  return getASTContext().testImportModule(modulePath);
}

void CompilerInstance::verifyImplicitBacktracingImport() {
  if (Invocation.shouldImportSwiftBacktracing() &&
      !canImportSwiftBacktracing()) {
    Diagnostics.diagnose(SourceLoc(),
                         diag::warn_implicit_backtracing_import_failed);
  }
}

bool CompilerInstance::canImportSwiftBacktracing() const {
  ImportPath::Module::Builder builder(
      getASTContext().getIdentifier(SWIFT_BACKTRACING_NAME));
  auto modulePath = builder.get();
  return getASTContext().testImportModule(modulePath);
}

bool CompilerInstance::canImportCxx() const {
  ImportPath::Module::Builder builder(
      getASTContext().getIdentifier(CXX_MODULE_NAME));
  auto modulePath = builder.get();
  return getASTContext().testImportModule(modulePath);
}

bool CompilerInstance::canImportCxxShim() const {
  ImportPath::Module::Builder builder(
      getASTContext().getIdentifier(CXX_SHIM_NAME));
  auto modulePath = builder.get();
  // Currently, Swift interfaces are not to expose their
  // C++ dependencies. Which means that when scanning them we should not
  // bring in such dependencies, including CxxShims.
  return getASTContext().testImportModule(modulePath) &&
         !Invocation.getFrontendOptions()
              .InputsAndOutputs.hasModuleInterfaceOutputPath() &&
         !Invocation.getFrontendOptions()
              .DependencyScanningSubInvocation;
}

bool CompilerInstance::supportCaching() const {
  if (!Invocation.getCASOptions().EnableCaching)
    return false;

  return FrontendOptions::supportCompilationCaching(
      Invocation.getFrontendOptions().RequestedAction);
}

bool CompilerInstance::downgradeInterfaceVerificationErrors() const {
  auto &FrontendOpts = Invocation.getFrontendOptions();
  if (Context->blockListConfig.hasBlockListAction(FrontendOpts.ModuleName,
                                             BlockListKeyKind::ModuleName,
                        BlockListAction::DowngradeInterfaceVerificationFailure)) {
    Context->Diags.diagnose(SourceLoc(), diag::interface_block_listed_broken,
                            FrontendOpts.ModuleName);
    return true;
  }
  return FrontendOpts.DowngradeInterfaceVerificationError;
}

ImplicitImportInfo CompilerInstance::getImplicitImportInfo() const {
  auto &frontendOpts = Invocation.getFrontendOptions();

  ImplicitImportInfo imports;
  imports.StdlibKind = Invocation.getImplicitStdlibKind();

  auto pushImport = [&](StringRef moduleStr,
                        ImportOptions options = ImportOptions()) {
    ImportPath::Builder importPath(Context->getIdentifier(moduleStr));
    UnloadedImportedModule import(importPath.copyTo(*Context),
                                  /*isScoped=*/false);
    imports.AdditionalUnloadedImports.emplace_back(
        import, SourceLoc(), options);
  };

  for (auto &moduleStrAndTestable : frontendOpts.getImplicitImportModuleNames()) {
    pushImport(moduleStrAndTestable.first,
               moduleStrAndTestable.second ? ImportFlags::Testable
                                           : ImportOptions());
  }

  if (Invocation.shouldImportSwiftONoneSupport()) {
    pushImport(SWIFT_ONONE_SUPPORT);
  }

  // FIXME: The canImport check is required for compatibility
  // with older SDKs. Longer term solution is to have the driver make
  // the decision on the implicit import: rdar://76996377
  if (Invocation.shouldImportSwiftConcurrency()) {
    switch (imports.StdlibKind) {
    case ImplicitStdlibKind::Builtin:
    case ImplicitStdlibKind::None:
      break;

    case ImplicitStdlibKind::Stdlib:
      if (canImportSwiftConcurrency())
        pushImport(SWIFT_CONCURRENCY_NAME);
      if (canImportSwiftConcurrencyShims())
        pushImport(SWIFT_CONCURRENCY_SHIMS_NAME);
      break;
    }
  }

  if (Invocation.shouldImportSwiftStringProcessing()) {
    switch (imports.StdlibKind) {
    case ImplicitStdlibKind::Builtin:
    case ImplicitStdlibKind::None:
      break;

    case ImplicitStdlibKind::Stdlib:
      if (canImportSwiftStringProcessing())
        pushImport(SWIFT_STRING_PROCESSING_NAME);
      break;
    }
  }

  if (Invocation.shouldImportSwiftBacktracing()) {
    switch (imports.StdlibKind) {
    case ImplicitStdlibKind::Builtin:
    case ImplicitStdlibKind::None:
      break;

    case ImplicitStdlibKind::Stdlib:
      if (canImportSwiftBacktracing())
        pushImport(SWIFT_BACKTRACING_NAME);
      break;
    }
  }

  if (Invocation.getLangOptions().EnableCXXInterop) {
    if (Invocation.shouldImportCxx() && canImportCxx())
      pushImport(CXX_MODULE_NAME);
    if (canImportCxxShim())
      pushImport(CXX_SHIM_NAME, {ImportFlags::ImplementationOnly});
  }

  imports.ShouldImportUnderlyingModule = frontendOpts.ImportUnderlyingModule;
  imports.BridgingHeaderPath = frontendOpts.ImplicitObjCHeaderPath;
  return imports;
}

static std::optional<SourceFileKind>
tryMatchInputModeToSourceFileKind(FrontendOptions::ParseInputMode mode) {
  switch (mode) {
  case FrontendOptions::ParseInputMode::SwiftLibrary:
      // A Swift file in -parse-as-library mode is a library file.
    return SourceFileKind::Library;
  case FrontendOptions::ParseInputMode::SIL:
      // A Swift file in -parse-sil mode is a SIL file.
    return SourceFileKind::SIL;
  case FrontendOptions::ParseInputMode::SwiftModuleInterface:
    return SourceFileKind::Interface;
  case FrontendOptions::ParseInputMode::Swift:
    return SourceFileKind::Main;
  }
  llvm::outs() << (unsigned)mode;
  llvm_unreachable("Unhandled input parsing mode!");
}

SourceFile *
CompilerInstance::computeMainSourceFileForModule(ModuleDecl *mod) const {
  // Swift libraries cannot have a 'main'.
  const auto &FOpts = getInvocation().getFrontendOptions();
  const auto &Inputs = FOpts.InputsAndOutputs.getAllInputs();
  if (FOpts.InputMode == FrontendOptions::ParseInputMode::SwiftLibrary) {
    return nullptr;
  }

  // Try to pull out a file called 'main.swift'.
  auto MainInputIter =
      std::find_if(Inputs.begin(), Inputs.end(), [](const InputFile &input) {
        return input.getType() == file_types::TY_Swift &&
               llvm::sys::path::filename(input.getFileName()) == "main.swift";
      });

  std::optional<unsigned> MainBufferID = std::nullopt;
  if (MainInputIter != Inputs.end()) {
    MainBufferID =
        getSourceMgr().getIDForBufferIdentifier(MainInputIter->getFileName());
  } else if (InputSourceCodeBufferIDs.size() == 1) {
    // Barring that, just nominate a single Swift file as the main file.
    MainBufferID.emplace(InputSourceCodeBufferIDs.front());
  }

  if (!MainBufferID.has_value()) {
    return nullptr;
  }

  auto SFK = tryMatchInputModeToSourceFileKind(FOpts.InputMode);
  if (!SFK.has_value()) {
    return nullptr;
  }

  return createSourceFileForMainModule(mod, *SFK,
                                       *MainBufferID, /*isMainBuffer*/true);
}

bool CompilerInstance::createFilesForMainModule(
    ModuleDecl *mod, SmallVectorImpl<FileUnit *> &files) const {
  // Try to pull out the main source file, if any. This ensures that it
  // is at the start of the list of files.
  std::optional<unsigned> MainBufferID = std::nullopt;
  if (SourceFile *mainSourceFile = computeMainSourceFileForModule(mod)) {
    MainBufferID = mainSourceFile->getBufferID();
    files.push_back(mainSourceFile);
  }

  // If we have partial modules to load, do so now, bailing if any failed to
  // load.
  if (!PartialModules.empty()) {
    if (loadPartialModulesAndImplicitImports(mod, files))
      return true;
  }

  // Finally add the library files.
  // FIXME: This is the only demand point for InputSourceCodeBufferIDs. We
  // should compute this list of source files lazily.
  for (auto BufferID : InputSourceCodeBufferIDs) {
    // Skip the main buffer, we've already handled it.
    if (BufferID == MainBufferID)
      continue;

    auto *libraryFile =
        createSourceFileForMainModule(mod, SourceFileKind::Library, BufferID);
    files.push_back(libraryFile);
  }
  return false;
}

ModuleDecl *CompilerInstance::getMainModule() const {
  if (MainModule)
    return MainModule;

  Identifier ID = Context->getIdentifier(Invocation.getModuleName());
  MainModule = ModuleDecl::createMainModule(
      *Context, ID, getImplicitImportInfo(),
      [&](ModuleDecl *MainModule, auto addFile) {
    if (Invocation.getFrontendOptions().EnableTesting)
      MainModule->setTestingEnabled();
    if (Invocation.getFrontendOptions().EnablePrivateImports)
      MainModule->setPrivateImportsEnabled();
    if (Invocation.getFrontendOptions().EnableImplicitDynamic)
      MainModule->setImplicitDynamicEnabled();
    if (Invocation.getLangOptions().BypassResilienceChecks)
      MainModule->setBypassResilience();
    if (!Invocation.getFrontendOptions().ModuleABIName.empty()) {
      MainModule->setABIName(getASTContext().getIdentifier(
          Invocation.getFrontendOptions().ModuleABIName));
    }
    if (!Invocation.getLangOptions().PackageName.empty()) {
      auto pkgName = Invocation.getLangOptions().PackageName;
      MainModule->setPackageName(getASTContext().getIdentifier(pkgName));
    }
    if (!Invocation.getFrontendOptions().ExportAsName.empty()) {
      MainModule->setExportAsName(getASTContext().getIdentifier(
          Invocation.getFrontendOptions().ExportAsName));
    }
    if (!Invocation.getFrontendOptions().PublicModuleName.empty()) {
      MainModule->setPublicModuleName(getASTContext().getIdentifier(
          Invocation.getFrontendOptions().PublicModuleName));
    }
    if (Invocation.getFrontendOptions().EnableLibraryEvolution)
      MainModule->setResilienceStrategy(ResilienceStrategy::Resilient);
    if (Invocation.getLangOptions().isSwiftVersionAtLeast(6))
      MainModule->setIsConcurrencyChecked(true);
    if (Invocation.getLangOptions().EnableCXXInterop &&
        Invocation.getLangOptions()
            .RequireCxxInteropToImportCxxInteropModule)
      MainModule->setHasCxxInteroperability();
    if (Invocation.getLangOptions().EnableCXXInterop)
      MainModule->setCXXStdlibKind(Invocation.getLangOptions().CXXStdlib);
    if (Invocation.getLangOptions().AllowNonResilientAccess)
      MainModule->setAllowNonResilientAccess();
    if (Invocation.getSILOptions().EnableSerializePackage)
      MainModule->setSerializePackageEnabled();

    if (!Invocation.getFrontendOptions()
             .SwiftInterfaceCompilerVersion.empty()) {
      auto compilerVersion =
          Invocation.getFrontendOptions().SwiftInterfaceCompilerVersion;
      MainModule->setSwiftInterfaceCompilerVersion(compilerVersion);
    }

    // Register the main module with the AST context.
    Context->addLoadedModule(MainModule);
    Context->MainModule = MainModule;

    // Create and add the module's files.
    SmallVector<FileUnit *, 16> files;
    if (!createFilesForMainModule(MainModule, files)) {
      for (auto *file : files)
        addFile(file);
    } else {
      // If we failed to load a partial module, mark the main module as having
      // "failed to load", as it will contain no files. Note that we don't try
      // to add any of the successfully loaded partial modules. This ensures
      // that we don't encounter cases where we try to resolve a cross-reference
      // into a partial module that failed to load.
      MainModule->setFailedToLoad();
    }
  });
  return MainModule;
}

void CompilerInstance::setMainModule(ModuleDecl *newMod) {
  assert(newMod->isMainModule());
  MainModule = newMod;
  Context->addLoadedModule(newMod);
  Context->MainModule = newMod;
}

bool CompilerInstance::performParseAndResolveImportsOnly() {
  FrontendStatsTracer tracer(getStatsReporter(), "parse-and-resolve-imports");

  auto *mainModule = getMainModule();

  // Load access notes.
  if (!Invocation.getFrontendOptions().AccessNotesPath.empty()) {
    auto accessNotesPath = Invocation.getFrontendOptions().AccessNotesPath;

    auto bufferOrError =
        swift::vfs::getFileOrSTDIN(getFileSystem(), accessNotesPath);
    if (bufferOrError) {
      int sourceID =
          SourceMgr.addNewSourceBuffer(std::move(bufferOrError.get()));
      auto buffer =
          SourceMgr.getLLVMSourceMgr().getMemoryBuffer(sourceID);

      if (auto accessNotesFile = AccessNotesFile::load(*Context, buffer))
        mainModule->getAccessNotes() = *accessNotesFile;
    }
    else {
      Diagnostics.diagnose(SourceLoc(), diag::access_notes_file_io_error,
                           accessNotesPath, bufferOrError.getError().message());
    }
  }

  // Resolve imports for all the source files in the module.
  performImportResolution(mainModule);

  bindExtensions(*mainModule);
  return Context->hadError();
}

void CompilerInstance::performSema() {
  performParseAndResolveImportsOnly();

  FrontendStatsTracer tracer(getStatsReporter(), "perform-sema");

  forEachFileToTypeCheck([&](SourceFile &SF) {
    performTypeChecking(SF);
    return false;
  });

  finishTypeChecking();
}

bool CompilerInstance::loadStdlibIfNeeded() {
  if (!FrontendOptions::doesActionRequireSwiftStandardLibrary(
          Invocation.getFrontendOptions().RequestedAction)) {
    return false;
  }
  // If we aren't expecting an implicit stdlib import, there's nothing to do.
  if (getImplicitImportInfo().StdlibKind != ImplicitStdlibKind::Stdlib)
    return false;

  FrontendStatsTracer tracer(getStatsReporter(), "load-stdlib");
  ModuleDecl *M = Context->getStdlibModule(/*loadIfAbsent*/ true);

  if (!M) {
    Diagnostics.diagnose(SourceLoc(), diag::error_stdlib_not_found,
                         Invocation.getTargetTriple());
    return true;
  }

  verifyImplicitConcurrencyImport();
  verifyImplicitStringProcessingImport();

  // If we failed to load, we should have already diagnosed.
  if (M->failedToLoad()) {
    assert(Diagnostics.hadAnyError() &&
           "stdlib module failed to load but nothing was diagnosed?");
    return true;
  }
  return false;
}

bool CompilerInstance::loadPartialModulesAndImplicitImports(
    ModuleDecl *mod, SmallVectorImpl<FileUnit *> &partialModules) const {
  assert(DefaultSerializedLoader && "Expected module loader in Compiler Instance");
  FrontendStatsTracer tracer(getStatsReporter(),
                             "load-partial-modules-and-implicit-imports");
  // Force loading implicit imports. This is currently needed to allow
  // deserialization to resolve cross references into bridging headers.
  // FIXME: Once deserialization loads all the modules it needs for cross
  // references, this can be removed.
  (void)mod->getImplicitImports();

  // Load in the partial modules.
  bool hadLoadError = false;
  for (auto &PM : PartialModules) {
    assert(PM.ModuleBuffer);
    auto *file = DefaultSerializedLoader->loadAST(
        *mod, /*diagLoc=*/SourceLoc(), /*moduleInterfacePath*/ "",
        /*moduleInterfaceSourcePath=*/"", std::move(PM.ModuleBuffer),
        std::move(PM.ModuleDocBuffer), std::move(PM.ModuleSourceInfoBuffer),
        /*isFramework*/ false);
    if (file) {
      partialModules.push_back(file);
    } else {
      hadLoadError = true;
    }
  }
  return hadLoadError;
}

bool CompilerInstance::forEachFileToTypeCheck(
    llvm::function_ref<bool(SourceFile &)> fn) {
  if (isWholeModuleCompilation()) {
    for (auto fileName : getMainModule()->getFiles()) {
      auto *SF = dyn_cast<SourceFile>(fileName);
      if (!SF) {
        continue;
      }
      if (fn(*SF))
        return true;
    }
  } else {
    for (auto *SF : getPrimarySourceFiles()) {
      if (fn(*SF))
        return true;
    }
  }
  return false;
}

bool CompilerInstance::forEachSourceFile(
    llvm::function_ref<bool(SourceFile &)> fn) {
  for (auto fileName : getMainModule()->getFiles()) {
    auto *SF = dyn_cast<SourceFile>(fileName);
    if (!SF) {
      continue;
    }
    if (fn(*SF))
      return true;
    ;
  }

  return false;
}

void CompilerInstance::finishTypeChecking() {
  forEachFileToTypeCheck([](SourceFile &SF) {
    performWholeModuleTypeChecking(SF);
    return false;
  });

  forEachSourceFile([](SourceFile &SF) {
    loadDerivativeConfigurations(SF);
    return false;
  });
}

SourceFile::ParsingOptions
CompilerInstance::getSourceFileParsingOptions(bool forPrimary) const {
  using ActionType = FrontendOptions::ActionType;
  using ParsingFlags = SourceFile::ParsingFlags;

  const auto &frontendOpts = Invocation.getFrontendOptions();
  const auto action = frontendOpts.RequestedAction;

  auto opts = SourceFile::getDefaultParsingOptions(getASTContext().LangOpts);
  if (FrontendOptions::shouldActionOnlyParse(action)) {
    // Generally in a parse-only invocation, we want to disable #if evaluation.
    // However, there are a couple of modes where we need to know which clauses
    // are active.
    if (action != ActionType::EmitImportedModules &&
        action != ActionType::ScanDependencies) {
      opts |= ParsingFlags::DisablePoundIfEvaluation;
    }
  }

  const auto &typeOpts = getASTContext().TypeCheckerOpts;
  const auto isEffectivelyPrimary = forPrimary || isWholeModuleCompilation();
  if (isEffectivelyPrimary) {
    // Disable delayed body parsing for primaries and in WMO, unless
    // forcefully skipping function bodies
    if (typeOpts.SkipFunctionBodies == FunctionBodySkipping::None)
      opts |= ParsingFlags::DisableDelayedBodies;
  } else {
    // Suppress parse warnings for non-primaries, as they'll get parsed multiple
    // times.
    opts |= ParsingFlags::SuppressWarnings;
  }

  // Turn off new parser round-trip and diagnostics checking for
  //   - secondary files
  //     - Only want to verify on primary files, no point checking more than
  //       once
  //   - IDE inspection
  //     - We don't want to pay the cost of verification for simple IDE
  //       functionality (eg. completion and cursor info)
  //   - dependency scanning
  //     - Same as IDE inspection, this is meant to be a very fast operation.
  //       Don't slow it down
  //   - skipped function bodies
  //     - Swift parser doesn't support function body skipping yet, so this
  //       would result in verification failures when bodies have errors
  if (!isEffectivelyPrimary || SourceMgr.hasIDEInspectionTargetBuffer() ||
      frontendOpts.RequestedAction == ActionType::ScanDependencies ||
      typeOpts.SkipFunctionBodies != FunctionBodySkipping::None) {
    opts -= ParsingFlags::RoundTrip;
    opts -= ParsingFlags::ValidateNewParserDiagnostics;
  }

  // Enable interface hash computation for primaries or emit-module-separately,
  // but not in WMO, as it's only currently needed for incremental mode.
  if (forPrimary ||
      typeOpts.SkipFunctionBodies ==
          FunctionBodySkipping::NonInlinableWithoutTypes ||
      frontendOpts.ReuseFrontendForMultipleCompilations) {
    opts |= ParsingFlags::EnableInterfaceHash;
  }
  const auto &LangOpts = Invocation.getLangOptions();
  if (action == ActionType::Immediate &&
      LangOpts.hasFeature(Feature::LazyImmediate)) {
    opts -= ParsingFlags::DisableDelayedBodies;
    opts -= ParsingFlags::ValidateNewParserDiagnostics;
  }
  return opts;
}

SourceFile *CompilerInstance::createSourceFileForMainModule(
    ModuleDecl *mod, SourceFileKind fileKind, unsigned bufferID,
    bool isMainBuffer) const {
  auto isPrimary = isPrimaryInput(bufferID);
  auto opts = getSourceFileParsingOptions(isPrimary);

  auto *inputFile = new (*Context)
      SourceFile(*mod, fileKind, bufferID, opts, isPrimary);

  return inputFile;
}

void CompilerInstance::freeASTContext() {
  TheSILTypes.reset();
  Context.reset();
  MainModule = nullptr;
  DefaultSerializedLoader = nullptr;
  MemoryBufferLoader = nullptr;
  PrimaryBufferIDs.clear();
}

/// Perform "stable" optimizations that are invariant across compiler versions.
static bool performMandatorySILPasses(CompilerInvocation &Invocation,
                                      SILModule *SM) {
  // Don't run diagnostic passes at all when merging modules.
  if (Invocation.getFrontendOptions().RequestedAction ==
      FrontendOptions::ActionType::MergeModules) {
    return false;
  }
  if (Invocation.getDiagnosticOptions().SkipDiagnosticPasses) {
    // Even if we are not supposed to run the diagnostic passes, we still need
    // to run the ownership evaluator.
    return runSILOwnershipEliminatorPass(*SM);
  }
  return runSILDiagnosticPasses(*SM);
}

/// Perform SIL optimization passes if optimizations haven't been disabled.
/// These may change across compiler versions.
static void performSILOptimizations(CompilerInvocation &Invocation,
                                    SILModule *SM) {
  FrontendStatsTracer tracer(SM->getASTContext().Stats,
                             "SIL optimization");
  if (Invocation.getFrontendOptions().RequestedAction ==
      FrontendOptions::ActionType::MergeModules ||
      !Invocation.getSILOptions().shouldOptimize()) {
    runSILPassesForOnone(*SM);
    return;
  }
  StringRef CustomPipelinePath =
  Invocation.getSILOptions().ExternalPassPipelineFilename;
  if (!CustomPipelinePath.empty()) {
    runSILOptimizationPassesWithFileSpecification(*SM, CustomPipelinePath);
  } else {
    runSILOptimizationPasses(*SM);
  }
  // When building SwiftOnoneSupport.o verify all expected ABI symbols.
  if (Invocation.getFrontendOptions().CheckOnoneSupportCompleteness
       // TODO: handle non-ObjC based stdlib builds, e.g. on linux.
      && Invocation.getLangOptions().EnableObjCInterop
      && Invocation.getFrontendOptions().RequestedAction
             == FrontendOptions::ActionType::EmitObject) {
    checkCompletenessOfPrespecializations(*SM);
  }
}

static void countStatsPostSILOpt(UnifiedStatsReporter &Stats,
                                 const SILModule& Module) {
  auto &C = Stats.getFrontendCounters();
  // FIXME: calculate these in constant time, via the dense maps.
  C.NumSILOptFunctions += Module.getFunctionList().size();
  C.NumSILOptVtables += Module.getVTables().size();
  C.NumSILOptWitnessTables += Module.getWitnessTableList().size();
  C.NumSILOptDefaultWitnessTables += Module.getDefaultWitnessTableList().size();
  C.NumSILOptGlobalVariables += Module.getSILGlobalList().size();
}

bool CompilerInstance::performSILProcessing(SILModule *silModule) {
  if (performMandatorySILPasses(Invocation, silModule) &&
      !Invocation.getFrontendOptions().AllowModuleWithCompilerErrors)
    return true;

  {
    FrontendStatsTracer tracer(silModule->getASTContext().Stats,
                               "SIL verification, pre-optimization");
    silModule->verify();
  }

  performSILOptimizations(Invocation, silModule);

  if (auto *stats = getStatsReporter())
    countStatsPostSILOpt(*stats, *silModule);

  {
    FrontendStatsTracer tracer(silModule->getASTContext().Stats,
                               "SIL verification, post-optimization");
    silModule->verify();
  }

  performSILInstCountIfNeeded(silModule);
  return false;
}

bool CompilerInstance::isCancellationRequested() const {
  auto flag = getASTContext().CancellationFlag;
  return flag && flag->load(std::memory_order_relaxed);
}

const PrimarySpecificPaths &
CompilerInstance::getPrimarySpecificPathsForWholeModuleOptimizationMode()
    const {
  return getPrimarySpecificPathsForAtMostOnePrimary();
}
const PrimarySpecificPaths &
CompilerInstance::getPrimarySpecificPathsForAtMostOnePrimary() const {
  return Invocation.getPrimarySpecificPathsForAtMostOnePrimary();
}
const PrimarySpecificPaths &
CompilerInstance::getPrimarySpecificPathsForPrimary(StringRef filename) const {
  return Invocation.getPrimarySpecificPathsForPrimary(filename);
}
const PrimarySpecificPaths &
CompilerInstance::getPrimarySpecificPathsForSourceFile(
    const SourceFile &SF) const {
  return Invocation.getPrimarySpecificPathsForSourceFile(SF);
}
