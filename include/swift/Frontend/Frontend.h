//===--- Frontend.h - frontend utility methods ------------------*- C++ -*-===//
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
//
// This file contains declarations of utility methods for parsing and
// performing semantic on modules.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_FRONTEND_H
#define SWIFT_FRONTEND_H

#include "swift/AST/DiagnosticConsumer.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/IRGenOptions.h"
#include "swift/AST/LinkLibrary.h"
#include "swift/AST/Module.h"
#include "swift/AST/SILOptions.h"
#include "swift/AST/SearchPathOptions.h"
#include "swift/AST/SourceFile.h"
#include "swift/Basic/DiagnosticOptions.h"
#include "swift/Basic/LangOptions.h"
#include "swift/Basic/SourceManager.h"
#include "swift/ClangImporter/ClangImporter.h"
#include "swift/Frontend/DiagnosticVerifier.h"
#include "swift/Frontend/FrontendOptions.h"
#include "swift/Frontend/ModuleInterfaceSupport.h"
#include "swift/Migrator/MigratorOptions.h"
#include "swift/Parse/CodeCompletionCallbacks.h"
#include "swift/Parse/Parser.h"
#include "swift/Parse/SyntaxParsingCache.h"
#include "swift/Sema/SourceLoader.h"
#include "swift/Serialization/Validation.h"
#include "swift/Subsystems.h"
#include "swift/TBDGen/TBDGen.h"
#include "llvm/ADT/IntrusiveRefCntPtr.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/Option/ArgList.h"
#include "llvm/Support/Host.h"
#include "llvm/Support/MemoryBuffer.h"
#include "clang/Basic/FileManager.h"

#include <memory>

namespace swift {

class SerializedModuleLoaderBase;
class MemoryBufferSerializedModuleLoader;
class SILModule;

namespace Lowering {
class TypeConverter;
}

struct ModuleBuffers {
  std::unique_ptr<llvm::MemoryBuffer> ModuleBuffer;
  std::unique_ptr<llvm::MemoryBuffer> ModuleDocBuffer;
  std::unique_ptr<llvm::MemoryBuffer> ModuleSourceInfoBuffer;
  ModuleBuffers(std::unique_ptr<llvm::MemoryBuffer> ModuleBuffer,
                std::unique_ptr<llvm::MemoryBuffer> ModuleDocBuffer = nullptr,
                std::unique_ptr<llvm::MemoryBuffer> ModuleSourceInfoBuffer = nullptr):
                  ModuleBuffer(std::move(ModuleBuffer)),
                  ModuleDocBuffer(std::move(ModuleDocBuffer)),
                  ModuleSourceInfoBuffer(std::move(ModuleSourceInfoBuffer)) {}
};

/// The abstract configuration of the compiler, including:
///   - options for all stages of translation,
///   - information about the build environment,
///   - information about the job being performed, and
///   - lists of inputs and outputs.
///
/// A CompilerInvocation can be built from a frontend command line
/// using parseArgs.  It can then be used to build a CompilerInstance,
/// which manages the actual compiler execution.
class CompilerInvocation {
  LangOptions LangOpts;
  TypeCheckerOptions TypeCheckerOpts;
  FrontendOptions FrontendOpts;
  ClangImporterOptions ClangImporterOpts;
  SearchPathOptions SearchPathOpts;
  DiagnosticOptions DiagnosticOpts;
  MigratorOptions MigratorOpts;
  SILOptions SILOpts;
  IRGenOptions IRGenOpts;
  TBDGenOptions TBDGenOpts;
  ModuleInterfaceOptions ModuleInterfaceOpts;
  /// The \c SyntaxParsingCache to use when parsing the main file of this
  /// invocation
  SyntaxParsingCache *MainFileSyntaxParsingCache = nullptr;

  llvm::MemoryBuffer *CodeCompletionBuffer = nullptr;

  /// Code completion offset in bytes from the beginning of the main
  /// source file.  Valid only if \c isCodeCompletion() == true.
  unsigned CodeCompletionOffset = ~0U;

  CodeCompletionCallbacksFactory *CodeCompletionFactory = nullptr;

public:
  CompilerInvocation();

  /// Initializes the compiler invocation for the list of arguments.
  ///
  /// All parsing should be additive, i.e. options should not be reset to their
  /// default values given the /absence/ of a flag. This is because \c parseArgs
  /// may be used to modify an already partially configured invocation.
  ///
  /// Any configuration files loaded as a result of parsing arguments will be
  /// stored in \p ConfigurationFileBuffers, if non-null. The contents of these
  /// buffers should \e not be interpreted by the caller; they are only present
  /// in order to make it possible to reproduce how these arguments were parsed
  /// if the compiler ends up crashing or exhibiting other bad behavior.
  ///
  /// If non-empty, relative search paths are resolved relative to
  /// \p workingDirectory.
  ///
  /// \returns true if there was an error, false on success.
  bool parseArgs(ArrayRef<const char *> Args, DiagnosticEngine &Diags,
                 SmallVectorImpl<std::unique_ptr<llvm::MemoryBuffer>>
                     *ConfigurationFileBuffers = nullptr,
                 StringRef workingDirectory = {},
                 StringRef mainExecutablePath = {});

  /// Sets specific options based on the given serialized Swift binary data.
  ///
  /// This is additive, i.e. options are not reset to their default values given
  /// the /absence/ of a flag. However, flags that only have a single value may
  /// (and should) be overwritten by this method.
  ///
  /// Invoking this on more than one serialized AST is likely to result in
  /// one or both of them failing to load. Please pick one AST to provide base
  /// flags for the entire ASTContext and let the others succeed or fail the
  /// normal way. (Some additive flags, like search paths, will be handled
  /// properly during normal module loading.)
  ///
  /// \returns Status::Valid on success, one of the Status issues on error.
  serialization::Status loadFromSerializedAST(StringRef data);

  /// Serialize the command line arguments for emitting them
  /// to DWARF or CodeView and inject SDKPath if necessary.
  static void buildDebugFlags(std::string &Output,
                              const ArrayRef<const char*> &Args,
                              StringRef SDKPath,
                              StringRef ResourceDir);

  void setTargetTriple(const llvm::Triple &Triple);
  void setTargetTriple(StringRef Triple);

  StringRef getTargetTriple() const {
    return LangOpts.Target.str();
  }

  void setClangModuleCachePath(StringRef Path) {
    ClangImporterOpts.ModuleCachePath = Path.str();
  }

  StringRef getClangModuleCachePath() const {
    return ClangImporterOpts.ModuleCachePath;
  }

  void setImportSearchPaths(const std::vector<std::string> &Paths) {
    SearchPathOpts.ImportSearchPaths = Paths;
  }

  ArrayRef<std::string> getImportSearchPaths() const {
    return SearchPathOpts.ImportSearchPaths;
  }

  void setFrameworkSearchPaths(
             const std::vector<SearchPathOptions::FrameworkSearchPath> &Paths) {
    SearchPathOpts.FrameworkSearchPaths = Paths;
  }

  ArrayRef<SearchPathOptions::FrameworkSearchPath> getFrameworkSearchPaths() const {
    return SearchPathOpts.FrameworkSearchPaths;
  }

  void setExtraClangArgs(const std::vector<std::string> &Args) {
    ClangImporterOpts.ExtraArgs = Args;
  }

  ArrayRef<std::string> getExtraClangArgs() const {
    return ClangImporterOpts.ExtraArgs;
  }

  void addLinkLibrary(StringRef name, LibraryKind kind) {
    IRGenOpts.LinkLibraries.push_back({name, kind});
  }

  ArrayRef<LinkLibrary> getLinkLibraries() const {
    return IRGenOpts.LinkLibraries;
  }

  void setMainExecutablePath(StringRef Path);

  void setRuntimeResourcePath(StringRef Path);

  /// If we haven't explicitly passed -prebuilt-module-cache-path, set it to
  /// the default value of <resource-dir>/<platform>/prebuilt-modules.
  /// @note This should be called once, after search path options and frontend
  ///       options have been parsed.
  void setDefaultPrebuiltCacheIfNecessary();

  /// Computes the runtime resource path relative to the given Swift
  /// executable.
  static void computeRuntimeResourcePathFromExecutablePath(
      StringRef mainExecutablePath, bool shared,
      llvm::SmallVectorImpl<char> &runtimeResourcePath);

  /// Appends `lib/swift[_static]` to the given path
  static void appendSwiftLibDir(llvm::SmallVectorImpl<char> &path, bool shared);

  void setSDKPath(const std::string &Path);

  StringRef getSDKPath() const {
    return SearchPathOpts.SDKPath;
  }

  LangOptions &getLangOptions() {
    return LangOpts;
  }
  const LangOptions &getLangOptions() const {
    return LangOpts;
  }

  TypeCheckerOptions &getTypeCheckerOptions() { return TypeCheckerOpts; }
  const TypeCheckerOptions &getTypeCheckerOptions() const {
    return TypeCheckerOpts;
  }

  FrontendOptions &getFrontendOptions() { return FrontendOpts; }
  const FrontendOptions &getFrontendOptions() const { return FrontendOpts; }

  TBDGenOptions &getTBDGenOptions() { return TBDGenOpts; }
  const TBDGenOptions &getTBDGenOptions() const { return TBDGenOpts; }

  ModuleInterfaceOptions &getModuleInterfaceOptions() { return ModuleInterfaceOpts; }
  const ModuleInterfaceOptions &getModuleInterfaceOptions() const { return ModuleInterfaceOpts; }

  ClangImporterOptions &getClangImporterOptions() { return ClangImporterOpts; }
  const ClangImporterOptions &getClangImporterOptions() const {
    return ClangImporterOpts;
  }

  SearchPathOptions &getSearchPathOptions() { return SearchPathOpts; }
  const SearchPathOptions &getSearchPathOptions() const {
    return SearchPathOpts;
  }

  DiagnosticOptions &getDiagnosticOptions() { return DiagnosticOpts; }
  const DiagnosticOptions &getDiagnosticOptions() const {
    return DiagnosticOpts;
  }

  const MigratorOptions &getMigratorOptions() const {
    return MigratorOpts;
  }

  SILOptions &getSILOptions() { return SILOpts; }
  const SILOptions &getSILOptions() const { return SILOpts; }

  IRGenOptions &getIRGenOptions() { return IRGenOpts; }
  const IRGenOptions &getIRGenOptions() const { return IRGenOpts; }

  void setMainFileSyntaxParsingCache(SyntaxParsingCache *Cache) {
    MainFileSyntaxParsingCache = Cache;
  }

  SyntaxParsingCache *getMainFileSyntaxParsingCache() const {
    return MainFileSyntaxParsingCache;
  }

  void setParseStdlib() {
    FrontendOpts.ParseStdlib = true;
  }

  bool getParseStdlib() const {
    return FrontendOpts.ParseStdlib;
  }

  void setModuleName(StringRef Name) {
    FrontendOpts.ModuleName = Name.str();
    IRGenOpts.ModuleName = Name.str();
  }

  StringRef getModuleName() const {
    return FrontendOpts.ModuleName;
  }

  std::string getOutputFilename() const {
    return FrontendOpts.InputsAndOutputs.getSingleOutputFilename();
  }

  void setCodeCompletionPoint(llvm::MemoryBuffer *Buf, unsigned Offset) {
    assert(Buf);
    CodeCompletionBuffer = Buf;
    CodeCompletionOffset = Offset;
    // We don't need typo-correction for code-completion.
    // FIXME: This isn't really true, but is a performance issue.
    LangOpts.TypoCorrectionLimit = 0;
  }

  std::pair<llvm::MemoryBuffer *, unsigned> getCodeCompletionPoint() const {
    return std::make_pair(CodeCompletionBuffer, CodeCompletionOffset);
  }

  /// \returns true if we are doing code completion.
  bool isCodeCompletion() const {
    return CodeCompletionOffset != ~0U;
  }

  /// Retrieve a module hash string that is suitable for uniquely
  /// identifying the conditions under which the module was built, for use
  /// in generating a cached PCH file for the bridging header.
  std::string getPCHHash() const;

  /// Retrieve the stdlib kind to implicitly import.
  ImplicitStdlibKind getImplicitStdlibKind() const {
    if (FrontendOpts.InputMode == FrontendOptions::ParseInputMode::SIL) {
      return ImplicitStdlibKind::None;
    }
    if (getParseStdlib()) {
      return ImplicitStdlibKind::Builtin;
    }
    return ImplicitStdlibKind::Stdlib;
  }

  /// Whether the Swift -Onone support library should be implicitly imported.
  bool shouldImportSwiftONoneSupport() const;

  /// Performs input setup common to these tools:
  /// sil-opt, sil-func-extractor, sil-llvm-gen, and sil-nm.
  /// Return value includes the buffer so caller can keep it alive.
  llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>>
  setUpInputForSILTool(StringRef inputFilename, StringRef moduleNameArg,
                       bool alwaysSetModuleToMain, bool bePrimary,
                       serialization::ExtendedValidationInfo &extendedInfo);

  const PrimarySpecificPaths &
  getPrimarySpecificPathsForAtMostOnePrimary() const;
  const PrimarySpecificPaths &
  getPrimarySpecificPathsForPrimary(StringRef filename) const;
  const PrimarySpecificPaths &
  getPrimarySpecificPathsForSourceFile(const SourceFile &SF) const;

  std::string getOutputFilenameForAtMostOnePrimary() const;
  std::string getMainInputFilenameForDebugInfoForAtMostOnePrimary() const;
  std::string getObjCHeaderOutputPathForAtMostOnePrimary() const;
  std::string getModuleOutputPathForAtMostOnePrimary() const;
  std::string
  getReferenceDependenciesFilePathForPrimary(StringRef filename) const;
  std::string getSwiftRangesFilePathForPrimary(StringRef filename) const;
  std::string getCompiledSourceFilePathForPrimary(StringRef filename) const;
  std::string getSerializedDiagnosticsPathForAtMostOnePrimary() const;

  /// TBDPath only makes sense in whole module compilation mode,
  /// so return the TBDPath when in that mode and fail an assert
  /// if not in that mode.
  std::string getTBDPathForWholeModule() const;

  /// ModuleInterfaceOutputPath only makes sense in whole module compilation
  /// mode, so return the ModuleInterfaceOutputPath when in that mode and
  /// fail an assert if not in that mode.
  std::string getModuleInterfaceOutputPathForWholeModule() const;
  std::string getPrivateModuleInterfaceOutputPathForWholeModule() const;

  std::string getLdAddCFileOutputPathForWholeModule() const;

public:
  /// Given the current configuration of this frontend invocation, a set of
  /// supplementary output paths, and a module, compute the appropriate set of
  /// serialization options.
  ///
  /// FIXME: The \p module parameter supports the
  /// \c SerializeOptionsForDebugging hack.
  SerializationOptions
  computeSerializationOptions(const SupplementaryOutputPaths &outs,
                              const ModuleDecl *module) const;

  /// Returns an approximation of whether the given module could be
  /// redistributed and consumed by external clients.
  ///
  /// FIXME: The scope of this computation should be limited entirely to
  /// PrintAsObjC. Unfortunately, it has been co-opted to support the
  /// \c SerializeOptionsForDebugging hack. Once this information can be
  /// transferred from module files to the dSYMs, remove this.
  bool isModuleExternallyConsumed(const ModuleDecl *mod) const;
};

/// A class which manages the state and execution of the compiler.
/// This owns the primary compiler singletons, such as the ASTContext,
/// as well as various build products such as the SILModule.
///
/// Before a CompilerInstance can be used, it must be configured by
/// calling \a setup.  If successful, this will create an ASTContext
/// and set up the basic compiler invariants.  Calling \a setup multiple
/// times on a single CompilerInstance is not permitted.
class CompilerInstance {
  CompilerInvocation Invocation;
  SourceManager SourceMgr;
  DiagnosticEngine Diagnostics{SourceMgr};
  std::unique_ptr<ASTContext> Context;
  std::unique_ptr<Lowering::TypeConverter> TheSILTypes;
  std::unique_ptr<DiagnosticVerifier> DiagVerifier;

  /// Null if no tracker.
  std::unique_ptr<DependencyTracker> DepTracker;
  /// If there is no stats output directory by the time the
  /// instance has completed its setup, this will be null.
  std::unique_ptr<UnifiedStatsReporter> Stats;

  mutable ModuleDecl *MainModule = nullptr;
  SerializedModuleLoaderBase *DefaultSerializedLoader = nullptr;
  MemoryBufferSerializedModuleLoader *MemoryBufferLoader = nullptr;

  /// Contains buffer IDs for input source code files.
  std::vector<unsigned> InputSourceCodeBufferIDs;

  /// Contains \c MemoryBuffers for partial serialized module files and
  /// corresponding partial serialized module documentation files. This is
  /// \c mutable as it is consumed by \c loadPartialModulesAndImplicitImports.
  mutable std::vector<ModuleBuffers> PartialModules;

  /// Identifies the set of input buffers in the SourceManager that are
  /// considered primaries.
  llvm::SetVector<unsigned> PrimaryBufferIDs;

  /// Return whether there is an entry in PrimaryInputs for buffer \p BufID.
  bool isPrimaryInput(unsigned BufID) const {
    return PrimaryBufferIDs.count(BufID) != 0;
  }

  /// Record in PrimaryBufferIDs the fact that \p BufID is a primary.
  /// If \p BufID is already in the set, do nothing.
  void recordPrimaryInputBuffer(unsigned BufID);

  bool isWholeModuleCompilation() const { return PrimaryBufferIDs.empty(); }

public:
  // Out of line to avoid having to import SILModule.h.
  CompilerInstance();
  ~CompilerInstance();

  CompilerInstance(const CompilerInstance &) = delete;
  void operator=(const CompilerInstance &) = delete;
  CompilerInstance(CompilerInstance &&) = delete;
  void operator=(CompilerInstance &&) = delete;

  SourceManager &getSourceMgr() { return SourceMgr; }
  const SourceManager &getSourceMgr() const { return SourceMgr; }

  DiagnosticEngine &getDiags() { return Diagnostics; }
  const DiagnosticEngine &getDiags() const { return Diagnostics; }

  llvm::vfs::FileSystem &getFileSystem() { return *SourceMgr.getFileSystem(); }

  ASTContext &getASTContext() { return *Context; }
  const ASTContext &getASTContext() const { return *Context; }

  bool hasASTContext() const { return Context != nullptr; }

  const SILOptions &getSILOptions() const { return Invocation.getSILOptions(); }

  Lowering::TypeConverter &getSILTypes();

  void addDiagnosticConsumer(DiagnosticConsumer *DC) {
    Diagnostics.addConsumer(*DC);
  }

  void removeDiagnosticConsumer(DiagnosticConsumer *DC) {
    Diagnostics.removeConsumer(*DC);
  }

  DependencyTracker *getDependencyTracker() { return DepTracker.get(); }
  const DependencyTracker *getDependencyTracker() const { return DepTracker.get(); }

  UnifiedStatsReporter *getStatsReporter() const { return Stats.get(); }

  /// Retrieve the main module containing the files being compiled.
  ModuleDecl *getMainModule() const;

  /// Replace the current main module with a new one. This is used for top-level
  /// cached code completion.
  void setMainModule(ModuleDecl *newMod);

  MemoryBufferSerializedModuleLoader *
  getMemoryBufferSerializedModuleLoader() const {
    return MemoryBufferLoader;
  }

  ArrayRef<unsigned> getInputBufferIDs() const {
    return InputSourceCodeBufferIDs;
  }

  ArrayRef<LinkLibrary> getLinkLibraries() const {
    return Invocation.getLinkLibraries();
  }

  bool hasSourceImport() const {
    return Invocation.getFrontendOptions().EnableSourceImport;
  }

  /// Gets the set of SourceFiles which are the primary inputs for this
  /// CompilerInstance.
  ArrayRef<SourceFile *> getPrimarySourceFiles() const {
    return getMainModule()->getPrimarySourceFiles();
  }

  /// Gets the SourceFile which is the primary input for this CompilerInstance.
  /// \returns the primary SourceFile, or nullptr if there is no primary input;
  /// if there are _multiple_ primary inputs, fails with an assertion.
  ///
  /// FIXME: This should be removed eventually, once there are no longer any
  /// codepaths that rely on a single primary file.
  SourceFile *getPrimarySourceFile() const {
    auto primaries = getPrimarySourceFiles();
    if (primaries.empty()) {
      return nullptr;
    } else {
      assert(primaries.size() == 1);
      return *primaries.begin();
    }
  }

  /// Returns true if there was an error during setup.
  bool setup(const CompilerInvocation &Invocation);

  const CompilerInvocation &getInvocation() const { return Invocation; }

  /// If a code completion buffer has been set, returns the corresponding source
  /// file.
  SourceFile *getCodeCompletionFile() const;

private:
  /// Set up the file system by loading and validating all VFS overlay YAML
  /// files. If the process of validating VFS files failed, or the overlay
  /// file system could not be initialized, this function returns true. Else it
  /// returns false if setup succeeded.
  bool setUpVirtualFileSystemOverlays();
  void setUpLLVMArguments();
  void setUpDiagnosticOptions();
  bool setUpModuleLoaders();
  bool setUpInputs();
  bool setUpASTContextIfNeeded();
  void setupStatsReporter();
  void setupDiagnosticVerifierIfNeeded();
  void setupDependencyTrackerIfNeeded();
  Optional<unsigned> setUpCodeCompletionBuffer();

  /// Find a buffer for a given input file and ensure it is recorded in
  /// SourceMgr, PartialModules, or InputSourceCodeBufferIDs as appropriate.
  /// Return the buffer ID if it is not already compiled, or None if so.
  /// Set failed on failure.

  Optional<unsigned> getRecordedBufferID(const InputFile &input, bool &failed);

  /// Given an input file, return a buffer to use for its contents,
  /// and a buffer for the corresponding module doc file if one exists.
  /// On failure, return a null pointer for the first element of the returned
  /// pair.
  Optional<ModuleBuffers> getInputBuffersIfPresent(const InputFile &input);

  /// Try to open the module doc file corresponding to the input parameter.
  /// Return None for error, nullptr if no such file exists, or the buffer if
  /// one was found.
  Optional<std::unique_ptr<llvm::MemoryBuffer>>
  openModuleDoc(const InputFile &input);

  /// Try to open the module source info file corresponding to the input parameter.
  /// Return None for error, nullptr if no such file exists, or the buffer if
  /// one was found.
  Optional<std::unique_ptr<llvm::MemoryBuffer>>
  openModuleSourceInfo(const InputFile &input);
public:
  /// Parses and type-checks all input files.
  void performSema();

  /// Parses and performs import resolution on all input files.
  ///
  /// This is similar to a parse-only invocation, but module imports will also
  /// be processed.
  bool performParseAndResolveImportsOnly();

  /// Performs mandatory, diagnostic, and optimization passes over the SIL.
  /// \param silModule The SIL module that was generated during SILGen.
  /// \returns true if any errors occurred.
  bool performSILProcessing(SILModule *silModule);

private:
  /// Creates a new source file for the main module.
  SourceFile *createSourceFileForMainModule(ModuleDecl *mod,
                                            SourceFileKind FileKind,
                                            Optional<unsigned> BufferID,
                                            bool isMainBuffer = false) const;

  /// Creates all the files to be added to the main module, appending them to
  /// \p files. If a loading error occurs, returns \c true.
  bool createFilesForMainModule(ModuleDecl *mod,
                                SmallVectorImpl<FileUnit *> &files) const;
  SourceFile *computeMainSourceFileForModule(ModuleDecl *mod) const;

public:
  void freeASTContext();

  /// If an implicit standard library import is expected, loads the standard
  /// library, returning \c false if we should continue, i.e. no error.
  bool loadStdlibIfNeeded();

private:
  /// Compute the parsing options for a source file in the main module.
  SourceFile::ParsingOptions getSourceFileParsingOptions(bool forPrimary) const;

  /// Retrieve a description of which modules should be implicitly imported.
  ImplicitImportInfo getImplicitImportInfo() const;

  /// For any serialized AST inputs, loads them in as partial module files,
  /// appending them to \p partialModules. If a loading error occurs, returns
  /// \c true.
  bool loadPartialModulesAndImplicitImports(
      ModuleDecl *mod, SmallVectorImpl<FileUnit *> &partialModules) const;

  void forEachFileToTypeCheck(llvm::function_ref<void(SourceFile &)> fn);

  void finishTypeChecking();

public:
  const PrimarySpecificPaths &
  getPrimarySpecificPathsForWholeModuleOptimizationMode() const;
  const PrimarySpecificPaths &
  getPrimarySpecificPathsForPrimary(StringRef filename) const;
  const PrimarySpecificPaths &
  getPrimarySpecificPathsForAtMostOnePrimary() const;
  const PrimarySpecificPaths &
  getPrimarySpecificPathsForSourceFile(const SourceFile &SF) const;

  /// Write out the unparsed (delayed) source ranges
  /// Return true for error
  bool emitSwiftRanges(DiagnosticEngine &diags, SourceFile *primaryFile,
                       StringRef outputPath) const;

  /// Return true for error
  bool emitCompiledSource(DiagnosticEngine &diags,
                          const SourceFile *primaryFile,
                          StringRef outputPath) const;
};

} // namespace swift

#endif
