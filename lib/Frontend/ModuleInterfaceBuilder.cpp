//===----- ModuleInterfaceBuilder.cpp - Compiles .swiftinterface files ----===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "textual-module-interface"

#include "ModuleInterfaceBuilder.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/DiagnosticsFrontend.h"
#include "swift/AST/DiagnosticsSema.h"
#include "swift/AST/FileSystem.h"
#include "swift/AST/Module.h"
#include "llvm/ADT/StringSet.h"
#include "swift/Basic/Defer.h"
#include "swift/Frontend/Frontend.h"
#include "swift/Frontend/ModuleInterfaceSupport.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/Serialization/SerializationOptions.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Lex/PreprocessorOptions.h"
#include "llvm/ADT/Hashing.h"
#include "llvm/ADT/StringSet.h"
#include "llvm/Support/xxhash.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/CrashRecoveryContext.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/Errc.h"
#include "llvm/Support/Regex.h"
#include "llvm/Support/StringSaver.h"

using namespace swift;
using FileDependency = SerializationOptions::FileDependency;
namespace path = llvm::sys::path;

/// If the file dependency in \p FullDepPath is inside the \p Base directory,
/// this returns its path relative to \p Base. Otherwise it returns None.
static Optional<StringRef> getRelativeDepPath(StringRef DepPath,
                                              StringRef Base) {
  // If Base is the root directory, or DepPath does not start with Base, bail.
  if (Base.size() <= 1 || !DepPath.startswith(Base)) {
    return None;
  }

  assert(DepPath.size() > Base.size() &&
      "should never depend on a directory");

  // Is the DepName something like ${Base}/foo.h"?
  if (path::is_separator(DepPath[Base.size()]))
    return DepPath.substr(Base.size() + 1);

  // Is the DepName something like "${Base}foo.h", where Base
  // itself contains a trailing slash?
  if (path::is_separator(Base.back()))
    return DepPath.substr(Base.size());

  // We have something next to Base, like "Base.h", that's somehow
  // become a dependency.
  return None;
}

void ModuleInterfaceBuilder::configureSubInvocationInputsAndOutputs(
    StringRef OutPath) {
  auto &SubFEOpts = subInvocation.getFrontendOptions();
  SubFEOpts.RequestedAction = FrontendOptions::ActionType::EmitModuleOnly;
  SubFEOpts.InputsAndOutputs.addPrimaryInputFile(interfacePath);
  SupplementaryOutputPaths SOPs;
  SOPs.ModuleOutputPath = OutPath.str();

  // Pick a primary output path that will cause problems to use.
  StringRef MainOut = "/<unused>";
  SubFEOpts.InputsAndOutputs
  .setMainAndSupplementaryOutputs({MainOut}, {SOPs});
}

void ModuleInterfaceBuilder::configureSubInvocation(
    const SearchPathOptions &SearchPathOpts,
    const LangOptions &LangOpts,
    ClangModuleLoader *ClangLoader) {
  // Start with a SubInvocation that copies various state from our
  // invoking ASTContext.
  subInvocation.setImportSearchPaths(SearchPathOpts.ImportSearchPaths);
  subInvocation.setFrameworkSearchPaths(SearchPathOpts.FrameworkSearchPaths);
  subInvocation.setSDKPath(SearchPathOpts.SDKPath);
  subInvocation.setInputKind(InputFileKind::SwiftModuleInterface);
  subInvocation.setRuntimeResourcePath(SearchPathOpts.RuntimeResourcePath);
  subInvocation.setTargetTriple(LangOpts.Target);

  subInvocation.setModuleName(moduleName);
  subInvocation.setClangModuleCachePath(moduleCachePath);
  subInvocation.getFrontendOptions().PrebuiltModuleCachePath =
  prebuiltCachePath;
  subInvocation.getFrontendOptions().TrackSystemDeps = trackSystemDependencies;

  // Respect the detailed-record preprocessor setting of the parent context.
  // This, and the "raw" clang module format it implicitly enables, are
  // required by sourcekitd.
  if (ClangLoader) {
    auto &Opts = ClangLoader->getClangInstance().getPreprocessorOpts();
    if (Opts.DetailedRecord) {
      subInvocation.getClangImporterOptions().DetailedPreprocessingRecord = true;
    }
  }

  // Inhibit warnings from the SubInvocation since we are assuming the user
  // is not in a position to fix them.
  subInvocation.getDiagnosticOptions().SuppressWarnings = true;

  // Inherit this setting down so that it can affect error diagnostics (mostly
  // by making them non-fatal).
  subInvocation.getLangOptions().DebuggerSupport = LangOpts.DebuggerSupport;

  // Disable this; deinitializers always get printed with `@objc` even in
  // modules that don't import Foundation.
  subInvocation.getLangOptions().EnableObjCAttrRequiresFoundation = false;

  // Tell the subinvocation to serialize dependency hashes if asked to do so.
  auto &frontendOpts = subInvocation.getFrontendOptions();
  frontendOpts.SerializeModuleInterfaceDependencyHashes =
  serializeDependencyHashes;

  // Tell the subinvocation to remark on rebuilds from an interface if asked
  // to do so.
  frontendOpts.RemarkOnRebuildFromModuleInterface =
  remarkOnRebuildFromInterface;
}

bool ModuleInterfaceBuilder::extractSwiftInterfaceVersionAndArgs(
    swift::version::Version &Vers, llvm::StringSaver &SubArgSaver,
    SmallVectorImpl<const char *> &SubArgs) {
  auto FileOrError = swift::vfs::getFileOrSTDIN(fs, interfacePath);
  if (!FileOrError) {
    diags.diagnose(diagnosticLoc, diag::error_open_input_file,
                   interfacePath, FileOrError.getError().message());
    return true;
  }
  auto SB = FileOrError.get()->getBuffer();
  auto VersRe = getSwiftInterfaceFormatVersionRegex();
  auto FlagRe = getSwiftInterfaceModuleFlagsRegex();
  SmallVector<StringRef, 1> VersMatches, FlagMatches;
  if (!VersRe.match(SB, &VersMatches)) {
    diags.diagnose(diagnosticLoc,
                   diag::error_extracting_version_from_module_interface);
    return true;
  }
  if (!FlagRe.match(SB, &FlagMatches)) {
    diags.diagnose(diagnosticLoc,
                   diag::error_extracting_flags_from_module_interface);
    return true;
  }
  assert(VersMatches.size() == 2);
  assert(FlagMatches.size() == 2);
  Vers = swift::version::Version(VersMatches[1], SourceLoc(), &diags);
  llvm::cl::TokenizeGNUCommandLine(FlagMatches[1], SubArgSaver, SubArgs);
  return false;
}

bool ModuleInterfaceBuilder::collectDepsForSerialization(
    CompilerInstance &SubInstance, SmallVectorImpl<FileDependency> &Deps,
    bool IsHashBased) {
  auto &Opts = SubInstance.getASTContext().SearchPathOpts;
  SmallString<128> SDKPath(Opts.SDKPath);
  path::native(SDKPath);
  SmallString<128> ResourcePath(Opts.RuntimeResourcePath);
  path::native(ResourcePath);

  auto DTDeps = SubInstance.getDependencyTracker()->getDependencies();
  SmallVector<StringRef, 16> InitialDepNames(DTDeps.begin(), DTDeps.end());
  InitialDepNames.push_back(interfacePath);
  InitialDepNames.insert(InitialDepNames.end(),
                         extraDependencies.begin(), extraDependencies.end());
  llvm::StringSet<> AllDepNames;
  SmallString<128> Scratch;

  for (const auto &InitialDepName : InitialDepNames) {
    path::native(InitialDepName, Scratch);
    StringRef DepName = Scratch.str();

    assert(moduleCachePath.empty() || !DepName.startswith(moduleCachePath));

    // Serialize the paths of dependencies in the SDK relative to it.
    Optional<StringRef> SDKRelativePath = getRelativeDepPath(DepName, SDKPath);
    StringRef DepNameToStore = SDKRelativePath.getValueOr(DepName);
    bool IsSDKRelative = SDKRelativePath.hasValue();

    // Forwarding modules add the underlying prebuilt module to their
    // dependency list -- don't serialize that.
    if (!prebuiltCachePath.empty() && DepName.startswith(prebuiltCachePath))
      continue;

    if (AllDepNames.insert(DepName).second && dependencyTracker) {
      dependencyTracker->addDependency(DepName, /*isSystem*/IsSDKRelative);
    }

    // Don't serialize compiler-relative deps so the cache is relocatable.
    if (DepName.startswith(ResourcePath))
      continue;

    auto Status = fs.status(DepName);
    if (!Status)
      return true;

    /// Lazily load the dependency buffer if we need it. If we're not
    /// dealing with a hash-based dependencies, and if the dependency is
    /// not a .swiftmodule, we can avoid opening the buffer.
    std::unique_ptr<llvm::MemoryBuffer> DepBuf = nullptr;
    auto getDepBuf = [&]() -> llvm::MemoryBuffer * {
      if (DepBuf) return DepBuf.get();
      if (auto Buf = fs.getBufferForFile(DepName, /*FileSize=*/-1,
                                         /*RequiresNullTerminator=*/false)) {
        DepBuf = std::move(Buf.get());
        return DepBuf.get();
      }
      return nullptr;
    };

    if (IsHashBased) {
      auto buf = getDepBuf();
      if (!buf) return true;
      uint64_t hash = xxHash64(buf->getBuffer());
      Deps.push_back(
                     FileDependency::hashBased(DepNameToStore, IsSDKRelative,
                                               Status->getSize(), hash));
    } else {
      uint64_t mtime =
      Status->getLastModificationTime().time_since_epoch().count();
      Deps.push_back(
                     FileDependency::modTimeBased(DepNameToStore, IsSDKRelative,
                                                  Status->getSize(), mtime));
    }
  }
  return false;
}

bool ModuleInterfaceBuilder::buildSwiftModule(
    StringRef OutPath, bool ShouldSerializeDeps,
    std::unique_ptr<llvm::MemoryBuffer> *ModuleBuffer) {
  bool SubError = false;
  bool RunSuccess = llvm::CrashRecoveryContext().RunSafelyOnThread([&] {
    // Note that we don't assume cachePath is the same as the Clang
    // module cache path at this point.
    if (!moduleCachePath.empty())
      (void)llvm::sys::fs::create_directories(moduleCachePath);

    configureSubInvocationInputsAndOutputs(OutPath);

    FrontendOptions &FEOpts = subInvocation.getFrontendOptions();
    const auto &InputInfo = FEOpts.InputsAndOutputs.firstInput();
    StringRef InPath = InputInfo.file();
    const auto &OutputInfo =
    InputInfo.getPrimarySpecificPaths().SupplementaryOutputs;
    StringRef OutPath = OutputInfo.ModuleOutputPath;

    llvm::BumpPtrAllocator SubArgsAlloc;
    llvm::StringSaver SubArgSaver(SubArgsAlloc);
    SmallVector<const char *, 16> SubArgs;
    swift::version::Version Vers;
    if (extractSwiftInterfaceVersionAndArgs(Vers, SubArgSaver, SubArgs)) {
      SubError = true;
      return;
    }

    // For now: we support anything with the same "major version" and assume
    // minor versions might be interesting for debugging, or special-casing a
    // compatible field variant.
    if (Vers.asMajorVersion() != InterfaceFormatVersion.asMajorVersion()) {
      diags.diagnose(diagnosticLoc,
                     diag::unsupported_version_of_module_interface,
                     interfacePath, Vers);
      SubError = true;
      return;
    }

    SmallString<32> ExpectedModuleName = subInvocation.getModuleName();
    if (subInvocation.parseArgs(SubArgs, diags)) {
      SubError = true;
      return;
    }

    if (subInvocation.getModuleName() != ExpectedModuleName) {
      auto DiagKind = diag::serialization_name_mismatch;
      if (subInvocation.getLangOptions().DebuggerSupport)
        DiagKind = diag::serialization_name_mismatch_repl;
      diags.diagnose(diagnosticLoc, DiagKind, subInvocation.getModuleName(),
                     ExpectedModuleName);
      SubError = true;
      return;
    }

    // Build the .swiftmodule; this is a _very_ abridged version of the logic
    // in performCompile in libFrontendTool, specialized, to just the one
    // module-serialization task we're trying to do here.
    LLVM_DEBUG(llvm::dbgs() << "Setting up instance to compile "
               << InPath << " to " << OutPath << "\n");
    CompilerInstance SubInstance;
    SubInstance.getSourceMgr().setFileSystem(&fs);

    ForwardingDiagnosticConsumer FDC(diags);
    SubInstance.addDiagnosticConsumer(&FDC);

    SubInstance.createDependencyTracker(FEOpts.TrackSystemDeps);

    SWIFT_DEFER {
      // Make sure to emit a generic top-level error if a module fails to
      // load. This is not only good for users; it also makes sure that we've
      // emitted an error in the parent diagnostic engine, which is what
      // determines whether the process exits with a proper failure status.
      if (SubInstance.getASTContext().hadError()) {
        diags.diagnose(diagnosticLoc, diag::serialization_load_failed,
                       moduleName);
      }
    };

    if (SubInstance.setup(subInvocation)) {
      SubError = true;
      return;
    }

    LLVM_DEBUG(llvm::dbgs() << "Performing sema\n");
    SubInstance.performSema();
    if (SubInstance.getASTContext().hadError()) {
      LLVM_DEBUG(llvm::dbgs() << "encountered errors\n");
      SubError = true;
      return;
    }

    SILOptions &SILOpts = subInvocation.getSILOptions();
    auto Mod = SubInstance.getMainModule();
    auto &TC = SubInstance.getSILTypes();
    auto SILMod = performSILGeneration(Mod, TC, SILOpts);
    if (!SILMod) {
      LLVM_DEBUG(llvm::dbgs() << "SILGen did not produce a module\n");
      SubError = true;
      return;
    }

    // Setup the callbacks for serialization, which can occur during the
    // optimization pipeline.
    SerializationOptions SerializationOpts;
    std::string OutPathStr = OutPath;
    SerializationOpts.OutputPath = OutPathStr.c_str();
    SerializationOpts.ModuleLinkName = FEOpts.ModuleLinkName;

    // Record any non-SDK module interface files for the debug info.
    StringRef SDKPath = SubInstance.getASTContext().SearchPathOpts.SDKPath;
    if (!getRelativeDepPath(InPath, SDKPath))
      SerializationOpts.ModuleInterface = InPath;

    SmallVector<FileDependency, 16> Deps;
    bool serializeHashes = FEOpts.SerializeModuleInterfaceDependencyHashes;
    if (collectDepsForSerialization(SubInstance, Deps, serializeHashes)) {
      SubError = true;
      return;
    }
    if (ShouldSerializeDeps)
      SerializationOpts.Dependencies = Deps;
    SILMod->setSerializeSILAction([&]() {
      // We don't want to serialize module docs in the cache -- they
      // will be serialized beside the interface file.
      serializeToBuffers(Mod, SerializationOpts, ModuleBuffer,
                         /*ModuleDocBuffer*/nullptr,
                         /*SourceInfoBuffer*/nullptr,
                         SILMod.get());
    });

    LLVM_DEBUG(llvm::dbgs() << "Running SIL processing passes\n");
    if (SubInstance.performSILProcessing(SILMod.get())) {
      LLVM_DEBUG(llvm::dbgs() << "encountered errors\n");
      SubError = true;
      return;
    }

    SubError = SubInstance.getDiags().hadAnyError();
  });
  return !RunSuccess || SubError;
}
