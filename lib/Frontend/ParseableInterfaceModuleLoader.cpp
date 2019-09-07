//===-- ParseableInterfaceModuleLoader.cpp - Loads .swiftinterface files --===//
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
#include "swift/Frontend/ParseableInterfaceModuleLoader.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/DiagnosticsFrontend.h"
#include "swift/AST/DiagnosticsSema.h"
#include "swift/AST/FileSystem.h"
#include "swift/AST/Module.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/Basic/Defer.h"
#include "swift/Basic/Lazy.h"
#include "swift/Basic/Platform.h"
#include "swift/Basic/STLExtras.h"
#include "swift/Frontend/Frontend.h"
#include "swift/Frontend/ParseableInterfaceSupport.h"
#include "swift/Frontend/PrintingDiagnosticConsumer.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/Serialization/SerializationOptions.h"
#include "swift/Serialization/Validation.h"
#include "clang/Basic/Module.h"
#include "clang/Lex/Preprocessor.h"
#include "clang/Lex/PreprocessorOptions.h"
#include "clang/Lex/HeaderSearch.h"
#include "clang/Frontend/CompilerInstance.h"
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
#include "llvm/Support/YAMLTraits.h"

using namespace swift;
using FileDependency = SerializationOptions::FileDependency;

/// Extract the specified-or-defaulted -module-cache-path that winds up in
/// the clang importer, for reuse as the .swiftmodule cache path when
/// building a ParseableInterfaceModuleLoader.
std::string
swift::getModuleCachePathFromClang(const clang::CompilerInstance &Clang) {
  if (!Clang.hasPreprocessor())
    return "";
  std::string SpecificModuleCachePath = Clang.getPreprocessor()
    .getHeaderSearchInfo()
    .getModuleCachePath();

  // The returned-from-clang module cache path includes a suffix directory
  // that is specific to the clang version and invocation; we want the
  // directory above that.
  return llvm::sys::path::parent_path(SpecificModuleCachePath);
}

#pragma mark - Forwarding Modules

namespace {

/// Describes a "forwarding module", that is, a .swiftmodule that's actually
/// a YAML file inside, pointing to a the original .swiftmodule but describing
/// a different dependency resolution strategy.
struct ForwardingModule {
  /// The path to the original .swiftmodule in the prebuilt cache.
  std::string underlyingModulePath;

  /// Describes a set of file-based dependencies with their size and
  /// modification time stored. This is slightly different from
  /// \c SerializationOptions::FileDependency, because this type needs to be
  /// serializable to and from YAML.
  struct Dependency {
    std::string path;
    uint64_t size;
    uint64_t lastModificationTime;
    bool isSDKRelative;
  };
  std::vector<Dependency> dependencies;
  unsigned version = 1;

  ForwardingModule() = default;
  ForwardingModule(StringRef underlyingModulePath)
  : underlyingModulePath(underlyingModulePath) {}

  /// Loads the contents of the forwarding module whose contents lie in
  /// the provided buffer, and returns a new \c ForwardingModule, or an error
  /// if the YAML could not be parsed.
  static llvm::ErrorOr<ForwardingModule> load(const llvm::MemoryBuffer &buf);

  /// Adds a given dependency to the dependencies list.
  void addDependency(StringRef path, bool isSDKRelative, uint64_t size,
                     uint64_t modTime) {
    dependencies.push_back({path.str(), size, modTime, isSDKRelative});
  }
};

} // end anonymous namespace

#pragma mark - YAML Serialization

namespace llvm {
  namespace yaml {
    template <>
    struct MappingTraits<ForwardingModule::Dependency> {
      static void mapping(IO &io, ForwardingModule::Dependency &dep) {
        io.mapRequired("mtime", dep.lastModificationTime);
        io.mapRequired("path", dep.path);
        io.mapRequired("size", dep.size);
        io.mapOptional("sdk_relative", dep.isSDKRelative, /*default*/false);
      }
    };

    template <>
    struct SequenceElementTraits<ForwardingModule::Dependency> {
      static const bool flow = false;
    };

    template <>
    struct MappingTraits<ForwardingModule> {
      static void mapping(IO &io, ForwardingModule &module) {
        io.mapRequired("path", module.underlyingModulePath);
        io.mapRequired("dependencies", module.dependencies);
        io.mapRequired("version", module.version);
      }
    };
  }
} // end namespace llvm

llvm::ErrorOr<ForwardingModule>
ForwardingModule::load(const llvm::MemoryBuffer &buf) {
  llvm::yaml::Input yamlIn(buf.getBuffer());
  ForwardingModule fwd;
  yamlIn >> fwd;
  if (yamlIn.error())
    return yamlIn.error();

  // We only currently support Version 1 of the forwarding module format.
  if (fwd.version != 1)
    return std::make_error_code(std::errc::not_supported);
  return std::move(fwd);
}

#pragma mark - Module Discovery

namespace {

/// The result of a search for a module either alongside an interface, in the
/// module cache, or in the prebuilt module cache.
class DiscoveredModule {
  /// The kind of module we've found.
  enum class Kind {
    /// A module that's either alongside the swiftinterface or in the
    /// module cache.
    Normal,

    /// A module that resides in the prebuilt cache, and has hash-based
    /// dependencies.
    Prebuilt,

    /// A 'forwarded' module. This is a module in the prebuilt cache, but whose
    /// dependencies live in a forwarding module.
    /// \sa ForwardingModule.
    Forwarded
  };

  /// The kind of module that's been discovered.
  const Kind kind;

  DiscoveredModule(StringRef path, Kind kind,
    std::unique_ptr<llvm::MemoryBuffer> moduleBuffer)
    : kind(kind), moduleBuffer(std::move(moduleBuffer)), path(path) {}

public:
  /// The contents of the .swiftmodule, if we've read it while validating
  /// dependencies.
  std::unique_ptr<llvm::MemoryBuffer> moduleBuffer;

  /// The path to the discovered serialized .swiftmodule on disk.
  const std::string path;

  /// Creates a \c Normal discovered module.
  static DiscoveredModule normal(StringRef path,
      std::unique_ptr<llvm::MemoryBuffer> moduleBuffer) {
    return { path, Kind::Normal, std::move(moduleBuffer) };
  }

  /// Creates a \c Prebuilt discovered module.
  static DiscoveredModule prebuilt(
      StringRef path, std::unique_ptr<llvm::MemoryBuffer> moduleBuffer) {
    return { path, Kind::Prebuilt, std::move(moduleBuffer) };
  }

  /// Creates a \c Forwarded discovered module, whose dependencies have been
  /// externally validated by a \c ForwardingModule.
  static DiscoveredModule forwarded(
      StringRef path, std::unique_ptr<llvm::MemoryBuffer> moduleBuffer) {
    return { path, Kind::Forwarded, std::move(moduleBuffer) };
  }

  bool isNormal() const { return kind == Kind::Normal; }
  bool isPrebuilt() const { return kind == Kind::Prebuilt; }
  bool isForwarded() const { return kind == Kind::Forwarded; }
};

} // end anonymous namespace

#pragma mark - Common utilities

namespace path = llvm::sys::path;

static bool serializedASTLooksValid(const llvm::MemoryBuffer &buf) {
  auto VI = serialization::validateSerializedAST(buf.getBuffer());
  return VI.status == serialization::Status::Valid;
}

static std::unique_ptr<llvm::MemoryBuffer> getBufferOfDependency(
  llvm::vfs::FileSystem &fs, StringRef depPath) {
  auto depBuf = fs.getBufferForFile(depPath, /*FileSize=*/-1,
                                    /*RequiresNullTerminator=*/false);
  if (!depBuf) {
    return nullptr;
  }
  return std::move(depBuf.get());
}

static Optional<llvm::vfs::Status> getStatusOfDependency(
  llvm::vfs::FileSystem &fs, StringRef depPath) {
  auto status = fs.status(depPath);
  if (!status) {
    return None;
  }
  return status.get();
}

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

#pragma mark - Module Building

/// Builds a parseable module interface into a .swiftmodule at the provided
/// output path.
/// \note Needs to be in the swift namespace so CompilerInvocation can see it.
class swift::ParseableInterfaceBuilder {
  llvm::vfs::FileSystem &fs;
  DiagnosticEngine &diags;
  const StringRef interfacePath;
  const StringRef moduleName;
  const StringRef moduleCachePath;
  const StringRef prebuiltCachePath;
  const bool serializeDependencyHashes;
  const bool trackSystemDependencies;
  const bool remarkOnRebuildFromInterface;
  const SourceLoc diagnosticLoc;
  DependencyTracker *const dependencyTracker;
  CompilerInvocation subInvocation;
  SmallVector<StringRef, 3> extraDependencies;

  void configureSubInvocationInputsAndOutputs(StringRef OutPath) {
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

  void configureSubInvocation(const SearchPathOptions &SearchPathOpts,
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

  bool extractSwiftInterfaceVersionAndArgs(
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

  /// Populate the provided \p Deps with \c FileDependency entries for all
  /// dependencies \p SubInstance's DependencyTracker recorded while compiling
  /// the module, excepting .swiftmodules in \p moduleCachePath or
  /// \p prebuiltCachePath. Those have _their_ dependencies added instead, both
  /// to avoid having to do recursive scanning when rechecking this dependency
  /// in future and to make the module caches relocatable.
  bool collectDepsForSerialization(CompilerInstance &SubInstance,
                                   SmallVectorImpl<FileDependency> &Deps,
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

      auto Status = getStatusOfDependency(fs, DepName);
      if (!Status)
        return true;

      /// Lazily load the dependency buffer if we need it. If we're not
      /// dealing with a hash-based dependencies, and if the dependency is
      /// not a .swiftmodule, we can avoid opening the buffer.
      std::unique_ptr<llvm::MemoryBuffer> DepBuf = nullptr;
      auto getDepBuf = [&]() -> llvm::MemoryBuffer * {
        if (DepBuf) return DepBuf.get();
        if (auto Buf = getBufferOfDependency(fs, DepName)) {
          DepBuf = std::move(Buf);
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

public:
  ParseableInterfaceBuilder(SourceManager &sourceMgr, DiagnosticEngine &diags,
                            const SearchPathOptions &searchPathOpts,
                            const LangOptions &langOpts,
                            ClangModuleLoader *clangImporter,
                            StringRef interfacePath,
                            StringRef moduleName,
                            StringRef moduleCachePath,
                            StringRef prebuiltCachePath,
                            bool serializeDependencyHashes = false,
                            bool trackSystemDependencies = false,
                            bool remarkOnRebuildFromInterface = false,
                            SourceLoc diagnosticLoc = SourceLoc(),
                            DependencyTracker *tracker = nullptr)
    : fs(*sourceMgr.getFileSystem()), diags(diags),
      interfacePath(interfacePath), moduleName(moduleName),
      moduleCachePath(moduleCachePath), prebuiltCachePath(prebuiltCachePath),
      serializeDependencyHashes(serializeDependencyHashes),
      trackSystemDependencies(trackSystemDependencies),
      remarkOnRebuildFromInterface(remarkOnRebuildFromInterface),
      diagnosticLoc(diagnosticLoc), dependencyTracker(tracker) {
    configureSubInvocation(searchPathOpts, langOpts, clangImporter);
  }

  const CompilerInvocation &getSubInvocation() const {
    return subInvocation;
  }

  /// Ensures the requested file name is added as a dependency of the resulting
  /// module.
  void addExtraDependency(StringRef path) {
    extraDependencies.push_back(path);
  }

  bool buildSwiftModule(StringRef OutPath, bool ShouldSerializeDeps,
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

      // Record any non-SDK parseable interface files for the debug info.
      StringRef SDKPath = SubInstance.getASTContext().SearchPathOpts.SDKPath;
      if (!getRelativeDepPath(InPath, SDKPath))
        SerializationOpts.ParseableInterface = InPath;

      SmallVector<FileDependency, 16> Deps;
      if (collectDepsForSerialization(SubInstance, Deps,
            FEOpts.SerializeModuleInterfaceDependencyHashes)) {
        SubError = true;
        return;
      }
      if (ShouldSerializeDeps)
        SerializationOpts.Dependencies = Deps;
      SILMod->setSerializeSILAction([&]() {
        // We don't want to serialize module docs in the cache -- they
        // will be serialized beside the interface file.
        serializeToBuffers(Mod, SerializationOpts, ModuleBuffer,
                           /*ModuleDocBuffer*/nullptr, SILMod.get());
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
};

#pragma mark - Module Loading

namespace {

/// Keeps track of the various reasons the module interface loader needed to
/// fall back and rebuild a module from its interface.
struct ModuleRebuildInfo {
  enum class ModuleKind {
    Normal,
    Cached,
    Forwarding,
    Prebuilt
  };
  struct OutOfDateModule {
    std::string path;
    Optional<serialization::Status> serializationStatus;
    ModuleKind kind;
    SmallVector<std::string, 10> outOfDateDependencies;
    SmallVector<std::string, 10> missingDependencies;
  };
  SmallVector<OutOfDateModule, 3> outOfDateModules;

  OutOfDateModule &getOrInsertOutOfDateModule(StringRef path) {
    for (auto &mod : outOfDateModules) {
      if (mod.path == path) return mod;
    }
    outOfDateModules.push_back({path, None, ModuleKind::Normal, {}, {}});
    return outOfDateModules.back();
  }

  /// Sets the kind of a module that failed to load.
  void setModuleKind(StringRef path, ModuleKind kind) {
    getOrInsertOutOfDateModule(path).kind = kind;
  }

  /// Sets the serialization status of the module at \c path. If this is
  /// anything other than \c Valid, a note will be added stating why the module
  /// was invalid.
  void setSerializationStatus(StringRef path, serialization::Status status) {
    getOrInsertOutOfDateModule(path).serializationStatus = status;
  }

  /// Registers an out-of-date dependency at \c depPath for the module
  /// at \c modulePath.
  void addOutOfDateDependency(StringRef modulePath, StringRef depPath) {
    getOrInsertOutOfDateModule(modulePath)
      .outOfDateDependencies.push_back(depPath);
  }

  /// Registers a missing dependency at \c depPath for the module
  /// at \c modulePath.
  void addMissingDependency(StringRef modulePath, StringRef depPath) {
    getOrInsertOutOfDateModule(modulePath)
      .missingDependencies.push_back(depPath);
  }

  /// Determines if we saw the given module path and registered is as out of
  /// date.
  bool sawOutOfDateModule(StringRef modulePath) {
    for (auto &mod : outOfDateModules)
      if (mod.path == modulePath)
        return true;
    return false;
  }

  const char *invalidModuleReason(serialization::Status status) {
    using namespace serialization;
    switch (status) {
    case Status::FormatTooOld:
      return "compiled with an older version of the compiler";
    case Status::FormatTooNew:
      return "compiled with a newer version of the compiler";
    case Status::Malformed:
      return "malformed";
    case Status::TargetIncompatible:
      return "compiled for a different target platform";
    case Status::TargetTooNew:
      return "target platform newer than current platform";
    default: return nullptr;
    }
  }

  /// Emits a diagnostic for all out-of-date compiled or forwarding modules
  /// encountered while trying to load a module.
  void diagnose(ASTContext &ctx, SourceLoc loc, StringRef moduleName,
                 StringRef interfacePath) {
    ctx.Diags.diagnose(loc, diag::rebuilding_module_from_interface,
                       moduleName, interfacePath);

    // We may have found multiple failing modules, that failed for different
    // reasons. Emit a note for each of them.
    for (auto &mod : outOfDateModules) {
      ctx.Diags.diagnose(loc, diag::out_of_date_module_here,
                         (unsigned)mod.kind, mod.path);

      // Diagnose any out-of-date dependencies in this module.
      for (auto &dep : mod.outOfDateDependencies) {
        ctx.Diags.diagnose(loc, diag::module_interface_dependency_out_of_date,
                           dep);
      }

      // Diagnose any missing dependencies in this module.
      for (auto &dep : mod.missingDependencies) {
        ctx.Diags.diagnose(loc, diag::module_interface_dependency_missing, dep);
      }

      // If there was a compiled module that wasn't able to be read, diagnose
      // the reason we couldn't read it.
      if (auto status = mod.serializationStatus) {
        if (auto reason = invalidModuleReason(*status)) {
          ctx.Diags.diagnose(loc, diag::compiled_module_invalid_reason,
              mod.path, reason);
        } else {
          ctx.Diags.diagnose(loc, diag::compiled_module_invalid, mod.path);
        }
      }
    }
  }
};

/// Handles the details of loading parseable interfaces as modules, and will
/// do the necessary lookup to determine if we should be loading from the
/// normal cache, the prebuilt cache, a module adjacent to the interface, or
/// a module that we'll build from a parseable interface.
class ParseableInterfaceModuleLoaderImpl {
  using AccessPathElem = std::pair<Identifier, SourceLoc>;
  friend class swift::ParseableInterfaceModuleLoader;
  ASTContext &ctx;
  llvm::vfs::FileSystem &fs;
  DiagnosticEngine &diags;
  ModuleRebuildInfo rebuildInfo;
  const StringRef modulePath;
  const std::string interfacePath;
  const StringRef moduleName;
  const StringRef prebuiltCacheDir;
  const StringRef cacheDir;
  const SourceLoc diagnosticLoc;
  DependencyTracker *const dependencyTracker;
  const ModuleLoadingMode loadMode;
  const bool remarkOnRebuildFromInterface;

  ParseableInterfaceModuleLoaderImpl(
    ASTContext &ctx, StringRef modulePath, StringRef interfacePath,
    StringRef moduleName, StringRef cacheDir, StringRef prebuiltCacheDir,
    SourceLoc diagLoc, bool remarkOnRebuildFromInterface,
    DependencyTracker *dependencyTracker = nullptr,
    ModuleLoadingMode loadMode = ModuleLoadingMode::PreferSerialized)
  : ctx(ctx), fs(*ctx.SourceMgr.getFileSystem()), diags(ctx.Diags),
    modulePath(modulePath), interfacePath(interfacePath),
    moduleName(moduleName), prebuiltCacheDir(prebuiltCacheDir),
    cacheDir(cacheDir), diagnosticLoc(diagLoc),
    dependencyTracker(dependencyTracker), loadMode(loadMode),
    remarkOnRebuildFromInterface(remarkOnRebuildFromInterface) {}

  /// Construct a cache key for the .swiftmodule being generated. There is a
  /// balance to be struck here between things that go in the cache key and
  /// things that go in the "up to date" check of the cache entry. We want to
  /// avoid fighting over a single cache entry too much when (say) running
  /// different compiler versions on the same machine or different inputs
  /// that happen to have the same short module name, so we will disambiguate
  /// those in the key. But we want to invalidate and rebuild a cache entry
  /// -- rather than making a new one and potentially filling up the cache
  /// with dead entries -- when other factors change, such as the contents of
  /// the .swiftinterface input or its dependencies.
  std::string getCacheHash(const CompilerInvocation &SubInvocation) {
    // Start with the compiler version (which will be either tag names or revs).
    // Explicitly don't pass in the "effective" language version -- this would
    // mean modules built in different -swift-version modes would rebuild their
    // dependencies.
    llvm::hash_code H = hash_value(swift::version::getSwiftFullVersion());

    // Simplest representation of input "identity" (not content) is just a
    // pathname, and probably all we can get from the VFS in this regard
    // anyways.
    H = hash_combine(H, interfacePath);

    // Include the target CPU architecture. In practice, .swiftinterface files
    // will be in architecture-specific subdirectories and would have
    // architecture-specific pieces #if'd out. However, it doesn't hurt to
    // include it, and it guards against mistakenly reusing cached modules
    // across architectures.
    H = hash_combine(H, SubInvocation.getLangOptions().Target.getArchName());

    // The SDK path is going to affect how this module is imported, so include
    // it.
    H = hash_combine(H, SubInvocation.getSDKPath());

    // Whether or not we're tracking system dependencies affects the
    // invalidation behavior of this cache item.
    H = hash_combine(H, SubInvocation.getFrontendOptions().TrackSystemDeps);

    return llvm::APInt(64, H).toString(36, /*Signed=*/false);
  }

  /// Calculate an output filename in \p SubInvocation's cache path that
  /// includes a hash of relevant key data.
  void computeCachedOutputPath(const CompilerInvocation &SubInvocation,
                               llvm::SmallString<256> &OutPath) {
    OutPath = SubInvocation.getClangModuleCachePath();
    llvm::sys::path::append(OutPath, SubInvocation.getModuleName());
    OutPath.append("-");
    OutPath.append(getCacheHash(SubInvocation));
    OutPath.append(".");
    auto OutExt = file_types::getExtension(file_types::TY_SwiftModuleFile);
    OutPath.append(OutExt);
  }

  /// Constructs the full path of the dependency \p dep by prepending the SDK
  /// path if necessary.
  StringRef getFullDependencyPath(const FileDependency &dep,
                                  SmallVectorImpl<char> &scratch) const {
    if (!dep.isSDKRelative())
      return dep.getPath();

    path::native(ctx.SearchPathOpts.SDKPath, scratch);
    llvm::sys::path::append(scratch, dep.getPath());
    return StringRef(scratch.data(), scratch.size());
  }

  enum class DependencyStatus {
    UpToDate,
    OutOfDate,
    Missing
  };

  // Checks that a dependency read from the cached module is up to date compared
  // to the interface file it represents.
  DependencyStatus checkDependency(StringRef modulePath,
                                   const FileDependency &dep,
                                   StringRef fullPath) {
    auto status = getStatusOfDependency(fs, fullPath);
    if (!status)
      return DependencyStatus::Missing;

    // If the sizes differ, then we know the file has changed.
    if (status->getSize() != dep.getSize())
      return DependencyStatus::OutOfDate;

    // Otherwise, if this dependency is verified by modification time, check
    // it vs. the modification time of the file.
    if (dep.isModificationTimeBased()) {
      uint64_t mtime =
        status->getLastModificationTime().time_since_epoch().count();
      return mtime == dep.getModificationTime() ?
          DependencyStatus::UpToDate :
          DependencyStatus::OutOfDate;
    }

    // Slow path: if the dependency is verified by content hash, check it vs.
    // the hash of the file.
    auto buf = getBufferOfDependency(fs, fullPath);
    if (!buf)
      return DependencyStatus::Missing;

    return xxHash64(buf->getBuffer()) == dep.getContentHash() ?
        DependencyStatus::UpToDate :
        DependencyStatus::OutOfDate;
  }

  // Check if all the provided file dependencies are up-to-date compared to
  // what's currently on disk.
  bool dependenciesAreUpToDate(StringRef modulePath,
                               ArrayRef<FileDependency> deps,
                               bool skipSystemDependencies) {
    SmallString<128> SDKRelativeBuffer;
    for (auto &in : deps) {
      if (skipSystemDependencies && in.isSDKRelative() &&
          in.isModificationTimeBased()) {
        continue;
      }
      StringRef fullPath = getFullDependencyPath(in, SDKRelativeBuffer);
      switch (checkDependency(modulePath, in, fullPath)) {
      case DependencyStatus::UpToDate:
        LLVM_DEBUG(llvm::dbgs() << "Dep " << fullPath << " is up to date\n");
        break;
      case DependencyStatus::OutOfDate:
        LLVM_DEBUG(llvm::dbgs() << "Dep " << fullPath << " is out of date\n");
        rebuildInfo.addOutOfDateDependency(modulePath, fullPath);
        return false;
      case DependencyStatus::Missing:
        LLVM_DEBUG(llvm::dbgs() << "Dep " << fullPath << " is missing\n");
        rebuildInfo.addMissingDependency(modulePath, fullPath);
        return false;
      }
    }
    return true;
  }

  // Check that the output .swiftmodule file is at least as new as all the
  // dependencies it read when it was built last time.
  bool serializedASTBufferIsUpToDate(
      StringRef path, const llvm::MemoryBuffer &buf,
      SmallVectorImpl<FileDependency> &allDeps) {

    // Clear the existing dependencies, because we're going to re-fill them
    // in validateSerializedAST.
    allDeps.clear();

    LLVM_DEBUG(llvm::dbgs() << "Validating deps of " << path << "\n");
    auto validationInfo = serialization::validateSerializedAST(
        buf.getBuffer(), /*ExtendedValidationInfo=*/nullptr, &allDeps);

    if (validationInfo.status != serialization::Status::Valid) {
      rebuildInfo.setSerializationStatus(path, validationInfo.status);
      return false;
    }

    bool skipCheckingSystemDependencies =
        ctx.SearchPathOpts.DisableModulesValidateSystemDependencies;
    return dependenciesAreUpToDate(path, allDeps,
                                   skipCheckingSystemDependencies);
  }

  // Check that the output .swiftmodule file is at least as new as all the
  // dependencies it read when it was built last time.
  bool swiftModuleIsUpToDate(
    StringRef modulePath, SmallVectorImpl<FileDependency> &AllDeps,
    std::unique_ptr<llvm::MemoryBuffer> &moduleBuffer) {
    auto OutBuf = fs.getBufferForFile(modulePath);
    if (!OutBuf)
      return false;
    moduleBuffer = std::move(*OutBuf);
    return serializedASTBufferIsUpToDate(modulePath, *moduleBuffer, AllDeps);
  }

  // Check that a "forwarding" .swiftmodule file is at least as new as all the
  // dependencies it read when it was built last time. Requires that the
  // forwarding module has been loaded from disk.
  bool forwardingModuleIsUpToDate(
      StringRef path, const ForwardingModule &fwd,
      SmallVectorImpl<FileDependency> &deps,
      std::unique_ptr<llvm::MemoryBuffer> &moduleBuffer) {

    // Clear the existing dependencies, because we're going to re-fill them
    // from the forwarding module.
    deps.clear();

    LLVM_DEBUG(llvm::dbgs() << "Validating deps of " << path << "\n");

    // First, make sure the underlying module path exists and is valid.
    auto modBuf = fs.getBufferForFile(fwd.underlyingModulePath);
    if (!modBuf || !serializedASTLooksValid(*modBuf.get()))
      return false;

    // Next, check the dependencies in the forwarding file.
    for (auto &dep : fwd.dependencies) {
      deps.push_back(
        FileDependency::modTimeBased(
          dep.path, dep.isSDKRelative, dep.size, dep.lastModificationTime));
    }

    bool skipCheckingSystemDependencies =
        ctx.SearchPathOpts.DisableModulesValidateSystemDependencies;
    if (!dependenciesAreUpToDate(path, deps, skipCheckingSystemDependencies))
      return false;

    moduleBuffer = std::move(*modBuf);
    return true;
  }

  Optional<StringRef>
  computePrebuiltModulePath(llvm::SmallString<256> &scratch) {
    namespace path = llvm::sys::path;
    StringRef sdkPath = ctx.SearchPathOpts.SDKPath;

    // Check if the interface file comes from the SDK
    if (sdkPath.empty() || !hasPrefix(path::begin(interfacePath),
                                      path::end(interfacePath),
                                      path::begin(sdkPath),
                                      path::end(sdkPath)))
      return None;

    // Assemble the expected path: $PREBUILT_CACHE/Foo.swiftmodule or
    // $PREBUILT_CACHE/Foo.swiftmodule/arch.swiftmodule. Note that there's no
    // cache key here.
    scratch = prebuiltCacheDir;

    // FIXME: Would it be possible to only have architecture-specific names
    // here? Then we could skip this check.
    StringRef inParentDirName =
      path::filename(path::parent_path(interfacePath));
    if (path::extension(inParentDirName) == ".swiftmodule") {
      assert(path::stem(inParentDirName) == moduleName);
      path::append(scratch, inParentDirName);
    }
    path::append(scratch, path::filename(modulePath));

    // If there isn't a file at this location, skip returning a path.
    if (!fs.exists(scratch))
      return None;

    return scratch.str();
  }

  /// Hack to deal with build systems (including the Swift standard library, at
  /// the time of this comment) that aren't yet using target-specific names for
  /// multi-target swiftmodules, in case the prebuilt cache is.
  Optional<StringRef>
  computeFallbackPrebuiltModulePath(llvm::SmallString<256> &scratch) {
    namespace path = llvm::sys::path;
    StringRef sdkPath = ctx.SearchPathOpts.SDKPath;

    // Check if the interface file comes from the SDK
    if (sdkPath.empty() || !hasPrefix(path::begin(interfacePath),
                                      path::end(interfacePath),
                                      path::begin(sdkPath),
                                      path::end(sdkPath)))
      return None;

    // If the module isn't target-specific, there's no fallback path.
    StringRef inParentDirName =
        path::filename(path::parent_path(interfacePath));
    if (path::extension(inParentDirName) != ".swiftmodule")
      return None;

    // If the interface is already using the target-specific name, there's
    // nothing else to try.
    auto normalizedTarget = getTargetSpecificModuleTriple(ctx.LangOpts.Target);
    if (path::stem(modulePath) == normalizedTarget.str())
      return None;

    // Assemble the expected path:
    // $PREBUILT_CACHE/Foo.swiftmodule/target.swiftmodule. Note that there's no
    // cache key here.
    scratch = prebuiltCacheDir;
    path::append(scratch, inParentDirName);
    path::append(scratch, normalizedTarget.str());
    scratch += ".swiftmodule";

    // If there isn't a file at this location, skip returning a path.
    if (!fs.exists(scratch))
      return None;

    return scratch.str();
  }

  bool isInResourceDir(StringRef path) {
    StringRef resourceDir = ctx.SearchPathOpts.RuntimeResourcePath;
    if (resourceDir.empty()) return false;
    return path.startswith(resourceDir);
  }

  /// Finds the most appropriate .swiftmodule, whose dependencies are up to
  /// date, that we can load for the provided .swiftinterface file.
  llvm::ErrorOr<DiscoveredModule> discoverUpToDateModuleForInterface(
    StringRef modulePath, StringRef cachedOutputPath,
    SmallVectorImpl<FileDependency> &deps) {
    auto notFoundError =
      std::make_error_code(std::errc::no_such_file_or_directory);

    // Keep track of whether we should attempt to load a .swiftmodule adjacent
    // to the .swiftinterface.
    bool shouldLoadAdjacentModule = true;

    switch (loadMode) {
    case ModuleLoadingMode::OnlyParseable:
      // Always skip both the caches and adjacent modules, and always build the
      // parseable interface.
      return notFoundError;
    case ModuleLoadingMode::PreferParseable:
      // If we're in the load mode that prefers .swiftinterfaces, specifically
      // skip the module adjacent to the interface, but use the caches if
      // they're present.
      shouldLoadAdjacentModule = false;
      break;
    case ModuleLoadingMode::PreferSerialized:
      // The rest of the function should be covered by this.
      break;
    case ModuleLoadingMode::OnlySerialized:
      llvm_unreachable("module interface loader should not have been created");
    }


    // First, check the cached module path. Whatever's in this cache represents
    // the most up-to-date knowledge we have about the module.
    if (auto cachedBufOrError = fs.getBufferForFile(cachedOutputPath)) {
      auto buf = std::move(*cachedBufOrError);

      // Check to see if the module is a serialized AST. If it's not, then we're
      // probably dealing with a Forwarding Module, which is a YAML file.
      bool isForwardingModule =
        !serialization::isSerializedAST(buf->getBuffer());

      // If it's a forwarding module, load the YAML file from disk and check
      // if it's up-to-date.
      if (isForwardingModule) {
        if (auto forwardingModule = ForwardingModule::load(*buf)) {
          std::unique_ptr<llvm::MemoryBuffer> moduleBuffer;
          if (forwardingModuleIsUpToDate(cachedOutputPath,
                                         *forwardingModule, deps,
                                         moduleBuffer)) {
            LLVM_DEBUG(llvm::dbgs() << "Found up-to-date forwarding module at "
                                    << cachedOutputPath << "\n");
            return DiscoveredModule::forwarded(
              forwardingModule->underlyingModulePath, std::move(moduleBuffer));
          }

          LLVM_DEBUG(llvm::dbgs() << "Found out-of-date forwarding module at "
                     << cachedOutputPath << "\n");
          rebuildInfo.setModuleKind(cachedOutputPath,
                                    ModuleRebuildInfo::ModuleKind::Forwarding);
        }
      // Otherwise, check if the AST buffer itself is up to date.
      } else if (serializedASTBufferIsUpToDate(cachedOutputPath, *buf, deps)) {
        LLVM_DEBUG(llvm::dbgs() << "Found up-to-date cached module at "
                                << cachedOutputPath << "\n");
        return DiscoveredModule::normal(cachedOutputPath, std::move(buf));
      } else {
        LLVM_DEBUG(llvm::dbgs() << "Found out-of-date cached module at "
                   << cachedOutputPath << "\n");
        rebuildInfo.setModuleKind(cachedOutputPath,
                                  ModuleRebuildInfo::ModuleKind::Cached);
      }
    }

    // If we weren't able to open the file for any reason, including it not
    // existing, keep going.

    // If we have a prebuilt cache path, check that too if the interface comes
    // from the SDK.
    if (!prebuiltCacheDir.empty()) {
      llvm::SmallString<256> scratch;
      std::unique_ptr<llvm::MemoryBuffer> moduleBuffer;
      Optional<StringRef> path = computePrebuiltModulePath(scratch);
      if (!path) {
        // Hack: deal with prebuilds of modules that still use the target-based
        // names.
        path = computeFallbackPrebuiltModulePath(scratch);
      }
      if (path) {
        if (swiftModuleIsUpToDate(*path, deps, moduleBuffer)) {
          LLVM_DEBUG(llvm::dbgs() << "Found up-to-date prebuilt module at "
                                  << path->str() << "\n");
          return DiscoveredModule::prebuilt(*path, std::move(moduleBuffer));
        } else {
          LLVM_DEBUG(llvm::dbgs() << "Found out-of-date prebuilt module at "
                                  << path->str() << "\n");
          rebuildInfo.setModuleKind(*path,
                                    ModuleRebuildInfo::ModuleKind::Prebuilt);
        }
      }
    }

    // Finally, if there's a module adjacent to the .swiftinterface that we can
    // _likely_ load (it validates OK and is up to date), bail early with
    // errc::not_supported, so the next (serialized) loader in the chain will
    // load it.
    // Alternately, if there's a .swiftmodule present but we can't even
    // read it (for whatever reason), we should let the other module loader
    // diagnose it.
    if (!shouldLoadAdjacentModule)
      return notFoundError;

    auto adjacentModuleBuffer = fs.getBufferForFile(modulePath);
    if (adjacentModuleBuffer) {
      if (serializedASTBufferIsUpToDate(modulePath, *adjacentModuleBuffer.get(),
                                        deps)) {
        LLVM_DEBUG(llvm::dbgs() << "Found up-to-date module at "
                                << modulePath
                                << "; deferring to serialized module loader\n");
        return std::make_error_code(std::errc::not_supported);
      } else if (isInResourceDir(modulePath) &&
                 loadMode == ModuleLoadingMode::PreferSerialized) {
        // Special-case here: If we're loading a .swiftmodule from the resource
        // dir adjacent to the compiler, defer to the serialized loader instead
        // of falling back. This is mainly to support development of Swift,
        // where one might change the module format version but forget to
        // recompile the standard library. If that happens, don't fall back
        // and silently recompile the standard library -- instead, error like
        // we used to.
        LLVM_DEBUG(llvm::dbgs() << "Found out-of-date module in the "
                                   "resource-dir at "
                                << modulePath
                                << "; deferring to serialized module loader "
                                   "to diagnose\n");
        return std::make_error_code(std::errc::not_supported);
      } else {
        LLVM_DEBUG(llvm::dbgs() << "Found out-of-date module at "
                                << modulePath << "\n");
        rebuildInfo.setModuleKind(modulePath,
                                  ModuleRebuildInfo::ModuleKind::Normal);
      }
    } else if (adjacentModuleBuffer.getError() != notFoundError) {
      LLVM_DEBUG(llvm::dbgs() << "Found unreadable module at "
                              << modulePath
                              << "; deferring to serialized module loader\n");
      return std::make_error_code(std::errc::not_supported);
    }

    // Couldn't find an up-to-date .swiftmodule, will need to build module from
    // interface.
    return notFoundError;
  }

  /// Writes the "forwarding module" that will forward to a module in the
  /// prebuilt cache.
  ///
  /// Since forwarding modules track dependencies separately from the module
  /// they point to, we'll need to grab the up-to-date file status while doing
  /// this. If the write was successful, it also updates the
  /// list of dependencies to match what was written to the forwarding file.
  bool writeForwardingModuleAndUpdateDeps(
      const DiscoveredModule &mod, StringRef outputPath,
      SmallVectorImpl<FileDependency> &deps) {
    assert(mod.isPrebuilt() &&
           "cannot write forwarding file for non-prebuilt module");
    ForwardingModule fwd(mod.path);

    SmallVector<FileDependency, 16> depsAdjustedToMTime;

    // FIXME: We need to avoid re-statting all these dependencies, otherwise
    //        we may record out-of-date information.
    SmallString<128> SDKRelativeBuffer;
    auto addDependency = [&](FileDependency dep) -> FileDependency {
      auto status = fs.status(getFullDependencyPath(dep, SDKRelativeBuffer));
      uint64_t mtime =
        status->getLastModificationTime().time_since_epoch().count();
      fwd.addDependency(dep.getPath(), dep.isSDKRelative(), status->getSize(),
                        mtime);

      // Construct new FileDependency matching what we've added to the
      // forwarding module.
      return FileDependency::modTimeBased(dep.getPath(), dep.isSDKRelative(),
                                          status->getSize(), mtime);
    };

    // Add the prebuilt module as a dependency of the forwarding module, but
    // don't add it to the outer dependency list.
    (void)addDependency(FileDependency::hashBased(fwd.underlyingModulePath,
                                                  /*SDKRelative*/false,
                                                  /*size*/0, /*hash*/0));

    // Add all the dependencies from the prebuilt module, and update our list
    // of dependencies to reflect what's recorded in the forwarding module.
    for (auto dep : deps) {
      auto adjustedDep = addDependency(dep);
      depsAdjustedToMTime.push_back(adjustedDep);
    }

    // Create the module cache if we haven't created it yet.
    StringRef parentDir = path::parent_path(outputPath);
    (void)llvm::sys::fs::create_directories(parentDir);

    auto hadError = withOutputFile(diags, outputPath,
      [&](llvm::raw_pwrite_stream &out) {
        llvm::yaml::Output yamlWriter(out);
        yamlWriter << fwd;
        return false;
      });

    if (hadError)
      return true;

    // If and only if we succeeded writing the forwarding file, update the
    // provided list of dependencies.
    deps = depsAdjustedToMTime;
    return false;
  }

  /// Looks up the best module to load for a given interface, and returns a
  /// buffer of the module's contents. Also reports the module's dependencies
  /// to the parent \c dependencyTracker if it came from the cache, or was built
  /// from the given interface. See the main comment in
  /// \c ParseableInterfaceModuleLoader.h for an explanation of the module
  /// loading strategy.
  llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>>
  findOrBuildLoadableModule() {

    // Track system dependencies if the parent tracker is set to do so.
    bool trackSystemDependencies = false;
    if (dependencyTracker) {
      auto ClangDependencyTracker = dependencyTracker->getClangCollector();
      trackSystemDependencies = ClangDependencyTracker->needSystemDependencies();
    }

    // Set up a builder if we need to build the module. It'll also set up
    // the subinvocation we'll need to use to compute the cache paths.
    ParseableInterfaceBuilder builder(
      ctx.SourceMgr, ctx.Diags, ctx.SearchPathOpts, ctx.LangOpts,
      ctx.getClangModuleLoader(), interfacePath, moduleName, cacheDir,
      prebuiltCacheDir, /*serializeDependencyHashes*/false,
      trackSystemDependencies, remarkOnRebuildFromInterface, diagnosticLoc,
      dependencyTracker);
    auto &subInvocation = builder.getSubInvocation();

    // Compute the output path if we're loading or emitting a cached module.
    llvm::SmallString<256> cachedOutputPath;
    computeCachedOutputPath(subInvocation, cachedOutputPath);

    // Try to find the right module for this interface, either alongside it,
    // in the cache, or in the prebuilt cache.
    SmallVector<FileDependency, 16> allDeps;
    auto moduleOrErr =
      discoverUpToDateModuleForInterface(modulePath, cachedOutputPath, allDeps);

    // If we errored with anything other than 'no such file or directory',
    // fail this load and let the other module loader diagnose it.
    if (!moduleOrErr &&
        moduleOrErr.getError() != std::errc::no_such_file_or_directory)
      return moduleOrErr.getError();

    // We discovered a module! Return that module's buffer so we can load it.
    if (moduleOrErr) {
      auto module = std::move(moduleOrErr.get());

      // If it's prebuilt, use this time to generate a forwarding module and
      // update the dependencies to use modification times.
      if (module.isPrebuilt())
        if (writeForwardingModuleAndUpdateDeps(module, cachedOutputPath,
                                               allDeps))
          return std::make_error_code(std::errc::not_supported);

      // Report the module's dependencies to the dependencyTracker
      if (dependencyTracker) {
        SmallString<128> SDKRelativeBuffer;
        for (auto &dep: allDeps) {
          StringRef fullPath = getFullDependencyPath(dep, SDKRelativeBuffer);
          dependencyTracker->addDependency(fullPath,
                                           /*IsSystem=*/dep.isSDKRelative());
        }
      }

      return std::move(module.moduleBuffer);
    }

    std::unique_ptr<llvm::MemoryBuffer> moduleBuffer;

    // We didn't discover a module corresponding to this interface.

    // Diagnose that we didn't find a loadable module, if we were asked to.
    if (remarkOnRebuildFromInterface) {
      rebuildInfo.diagnose(ctx, diagnosticLoc, moduleName,
                           interfacePath);
    }

    // If we found an out-of-date .swiftmodule, we still want to add it as
    // a dependency of the .swiftinterface. That way if it's updated, but
    // the .swiftinterface remains the same, we invalidate the cache and
    // check the new .swiftmodule, because it likely has more information
    // about the state of the world.
    if (rebuildInfo.sawOutOfDateModule(modulePath))
      builder.addExtraDependency(modulePath);

    if (builder.buildSwiftModule(cachedOutputPath, /*shouldSerializeDeps*/true,
                                 &moduleBuffer))
      return std::make_error_code(std::errc::invalid_argument);

    assert(moduleBuffer &&
           "failed to write module buffer but returned success?");
    return std::move(moduleBuffer);
  }
};

} // end anonymous namespace

bool ParseableInterfaceModuleLoader::isCached(StringRef DepPath) {
  if (!CacheDir.empty() && DepPath.startswith(CacheDir))
    return true;
  return !PrebuiltCacheDir.empty() && DepPath.startswith(PrebuiltCacheDir);
}

/// Load a .swiftmodule associated with a .swiftinterface either from a
/// cache or by converting it in a subordinate \c CompilerInstance, caching
/// the results.
std::error_code ParseableInterfaceModuleLoader::findModuleFilesInDirectory(
  AccessPathElem ModuleID, StringRef DirPath, StringRef ModuleFilename,
  StringRef ModuleDocFilename,
  std::unique_ptr<llvm::MemoryBuffer> *ModuleBuffer,
  std::unique_ptr<llvm::MemoryBuffer> *ModuleDocBuffer) {

  // If running in OnlySerialized mode, ParseableInterfaceModuleLoader
  // should not have been constructed at all.
  assert(LoadMode != ModuleLoadingMode::OnlySerialized);

  auto &fs = *Ctx.SourceMgr.getFileSystem();
  llvm::SmallString<256> ModPath, InPath;

  // First check to see if the .swiftinterface exists at all. Bail if not.
  ModPath = DirPath;
  path::append(ModPath, ModuleFilename);

  auto Ext = file_types::getExtension(file_types::TY_SwiftParseableInterfaceFile);
  InPath = ModPath;
  path::replace_extension(InPath, Ext);
  if (!fs.exists(InPath)) {
    if (fs.exists(ModPath)) {
      LLVM_DEBUG(llvm::dbgs()
        << "No .swiftinterface file found adjacent to module file "
        << ModPath.str() << "\n");
      return std::make_error_code(std::errc::not_supported);
    }
    return std::make_error_code(std::errc::no_such_file_or_directory);
  }

  // Create an instance of the Impl to do the heavy lifting.
  auto ModuleName = ModuleID.first.str();
  ParseableInterfaceModuleLoaderImpl Impl(
                Ctx, ModPath, InPath, ModuleName,
                CacheDir, PrebuiltCacheDir, ModuleID.second,
                RemarkOnRebuildFromInterface, dependencyTracker,
                llvm::is_contained(PreferInterfaceForModules,
                                   ModuleName) ?
                  ModuleLoadingMode::PreferParseable : LoadMode);

  // Ask the impl to find us a module that we can load or give us an error
  // telling us that we couldn't load it.
  auto ModuleBufferOrErr = Impl.findOrBuildLoadableModule();
  if (!ModuleBufferOrErr)
    return ModuleBufferOrErr.getError();

  if (ModuleBuffer) {
    *ModuleBuffer = std::move(*ModuleBufferOrErr);
  }

  // Delegate back to the serialized module loader to load the module doc.
  llvm::SmallString<256> DocPath{DirPath};
  path::append(DocPath, ModuleDocFilename);
  auto DocLoadErr =
    SerializedModuleLoaderBase::openModuleDocFile(ModuleID, DocPath,
                                                  ModuleDocBuffer);
  if (DocLoadErr)
    return DocLoadErr;

  return std::error_code();
}


bool ParseableInterfaceModuleLoader::buildSwiftModuleFromSwiftInterface(
    SourceManager &SourceMgr, DiagnosticEngine &Diags,
    const SearchPathOptions &SearchPathOpts, const LangOptions &LangOpts,
    StringRef CacheDir, StringRef PrebuiltCacheDir,
    StringRef ModuleName, StringRef InPath, StringRef OutPath,
    bool SerializeDependencyHashes, bool TrackSystemDependencies,
    bool RemarkOnRebuildFromInterface) {
  ParseableInterfaceBuilder builder(SourceMgr, Diags, SearchPathOpts, LangOpts,
                                    /*clangImporter*/nullptr, InPath,
                                    ModuleName, CacheDir, PrebuiltCacheDir,
                                    SerializeDependencyHashes,
                                    TrackSystemDependencies,
                                    RemarkOnRebuildFromInterface);
  // FIXME: We really only want to serialize 'important' dependencies here, if
  //        we want to ship the built swiftmodules to another machine.
  return builder.buildSwiftModule(OutPath, /*shouldSerializeDeps*/true,
                                  /*ModuleBuffer*/nullptr);
}

void ParseableInterfaceModuleLoader::collectVisibleTopLevelModuleNames(
    SmallVectorImpl<Identifier> &names) const {
  collectVisibleTopLevelModuleNamesImpl(
      names,
      file_types::getExtension(file_types::TY_SwiftParseableInterfaceFile));
}
