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
#include "swift/Basic/DiagnosticOptions.h"
#include "swift/Basic/LangOptions.h"
#include "swift/Basic/SourceManager.h"
#include "swift/ClangImporter/ClangImporter.h"
#include "swift/ClangImporter/ClangImporterOptions.h"
#include "swift/Frontend/FrontendOptions.h"
#include "swift/Frontend/ParseableInterfaceSupport.h"
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

class SerializedModuleLoader;
class SILModule;

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
  FrontendOptions FrontendOpts;
  ClangImporterOptions ClangImporterOpts;
  SearchPathOptions SearchPathOpts;
  DiagnosticOptions DiagnosticOpts;
  MigratorOptions MigratorOpts;
  SILOptions SILOpts;
  IRGenOptions IRGenOpts;
  TBDGenOptions TBDGenOpts;
  ParseableInterfaceOptions ParseableInterfaceOpts;
  /// The \c SyntaxParsingCache to use when parsing the main file of this
  /// invocation
  SyntaxParsingCache *MainFileSyntaxParsingCache = nullptr;

  llvm::MemoryBuffer *CodeCompletionBuffer = nullptr;

  /// \brief Code completion offset in bytes from the beginning of the main
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
                 StringRef workingDirectory = {});

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

  void setSDKPath(const std::string &Path) {
    SearchPathOpts.SDKPath = Path;
  }

  StringRef getSDKPath() const {
    return SearchPathOpts.SDKPath;
  }

  LangOptions &getLangOptions() {
    return LangOpts;
  }
  const LangOptions &getLangOptions() const {
    return LangOpts;
  }

  FrontendOptions &getFrontendOptions() { return FrontendOpts; }
  const FrontendOptions &getFrontendOptions() const { return FrontendOpts; }

  TBDGenOptions &getTBDGenOptions() { return TBDGenOpts; }
  const TBDGenOptions &getTBDGenOptions() const { return TBDGenOpts; }

  ParseableInterfaceOptions &getParseableInterfaceOptions() { return ParseableInterfaceOpts; }
  const ParseableInterfaceOptions &getParseableInterfaceOptions() const { return ParseableInterfaceOpts; }

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

  void setInputKind(InputFileKind K) {
    FrontendOpts.InputKind = K;
  }

  InputFileKind getInputKind() const {
    return FrontendOpts.InputKind;
  }

  SourceFileKind getSourceFileKind() const;

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

  void setCodeCompletionFactory(CodeCompletionCallbacksFactory *Factory) {
    CodeCompletionFactory = Factory;
  }

  CodeCompletionCallbacksFactory *getCodeCompletionFactory() const {
    return CodeCompletionFactory;
  }

  /// Retrieve a module hash string that is suitable for uniquely
  /// identifying the conditions under which the module was built, for use
  /// in generating a cached PCH file for the bridging header.
  std::string getPCHHash() const;

  SourceFile::ImplicitModuleImportKind getImplicitModuleImportKind() {
    if (getInputKind() == InputFileKind::SIL) {
      return SourceFile::ImplicitModuleImportKind::None;
    }
    if (getParseStdlib()) {
      return SourceFile::ImplicitModuleImportKind::Builtin;
    }
    return SourceFile::ImplicitModuleImportKind::Stdlib;
  }

  /// Performs input setup common to these tools:
  /// sil-opt, sil-func-extractor, sil-llvm-gen, and sil-nm.
  /// Return value includes the buffer so caller can keep it alive.
  llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>>
  setUpInputForSILTool(StringRef inputFilename, StringRef moduleNameArg,
                       bool alwaysSetModuleToMain, bool bePrimary,
                       serialization::ExtendedValidationInfo &extendedInfo);
  bool hasSerializedAST() {
    return FrontendOpts.InputKind == InputFileKind::SwiftLibrary;
  }

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
  std::string getSerializedDiagnosticsPathForAtMostOnePrimary() const;

  /// TBDPath only makes sense in whole module compilation mode,
  /// so return the TBDPath when in that mode and fail an assert
  /// if not in that mode.
  std::string getTBDPathForWholeModule() const;

  /// ParseableInterfaceOutputPath only makes sense in whole module compilation
  /// mode, so return the ParseableInterfaceOutputPath when in that mode and
  /// fail an assert if not in that mode.
  std::string getParseableInterfaceOutputPathForWholeModule() const;

  SerializationOptions
  computeSerializationOptions(const SupplementaryOutputPaths &outs,
                              bool moduleIsPublic);
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
  std::unique_ptr<SILModule> TheSILModule;

  /// Null if no tracker.
  std::unique_ptr<DependencyTracker> DepTracker;

  ModuleDecl *MainModule = nullptr;
  SerializedModuleLoader *SML = nullptr;

  /// Contains buffer IDs for input source code files.
  std::vector<unsigned> InputSourceCodeBufferIDs;

  struct PartialModuleInputs {
    std::unique_ptr<llvm::MemoryBuffer> ModuleBuffer;
    std::unique_ptr<llvm::MemoryBuffer> ModuleDocBuffer;
  };

  /// Contains \c MemoryBuffers for partial serialized module files and
  /// corresponding partial serialized module documentation files.
  std::vector<PartialModuleInputs> PartialModules;

  enum : unsigned { NO_SUCH_BUFFER = ~0U };
  unsigned MainBufferID = NO_SUCH_BUFFER;

  /// Identifies the set of input buffers in the SourceManager that are
  /// considered primaries.
  llvm::SetVector<unsigned> PrimaryBufferIDs;

  /// Identifies the set of SourceFiles that are considered primaries. An
  /// invariant is that any SourceFile in this set with an associated
  /// buffer will also have its buffer ID in PrimaryBufferIDs.
  std::vector<SourceFile *> PrimarySourceFiles;

  /// Return whether there is an entry in PrimaryInputs for buffer \p BufID.
  bool isPrimaryInput(unsigned BufID) const {
    return PrimaryBufferIDs.count(BufID) != 0;
  }

  /// Record in PrimaryBufferIDs the fact that \p BufID is a primary.
  /// If \p BufID is already in the set, do nothing.
  void recordPrimaryInputBuffer(unsigned BufID);

  /// Record in PrimarySourceFiles the fact that \p SF is a primary, and
  /// call recordPrimaryInputBuffer on \p SF's buffer (if it exists).
  void recordPrimarySourceFile(SourceFile *SF);

  bool isWholeModuleCompilation() { return PrimaryBufferIDs.empty(); }

  void createSILModule();

public:
  // Out of line to avoid having to import SILModule.h.
  CompilerInstance();
  ~CompilerInstance();

  CompilerInstance(const CompilerInstance &) = delete;
  void operator=(const CompilerInstance &) = delete;
  CompilerInstance(CompilerInstance &&) = delete;
  void operator=(CompilerInstance &&) = delete;

  SourceManager &getSourceMgr() { return SourceMgr; }

  DiagnosticEngine &getDiags() { return Diagnostics; }

  llvm::vfs::FileSystem &getFileSystem() { return *SourceMgr.getFileSystem(); }

  ASTContext &getASTContext() {
    return *Context;
  }
  bool hasASTContext() const { return Context != nullptr; }

  SILOptions &getSILOptions() { return Invocation.getSILOptions(); }
  const SILOptions &getSILOptions() const { return Invocation.getSILOptions(); }

  void addDiagnosticConsumer(DiagnosticConsumer *DC) {
    Diagnostics.addConsumer(*DC);
  }

  void createDependencyTracker(bool TrackSystemDeps) {
    assert(!Context && "must be called before setup()");
    DepTracker = llvm::make_unique<DependencyTracker>(TrackSystemDeps);
  }
  DependencyTracker *getDependencyTracker() { return DepTracker.get(); }

  /// Set the SIL module for this compilation instance.
  ///
  /// The CompilerInstance takes ownership of the given SILModule object.
  void setSILModule(std::unique_ptr<SILModule> M);

  SILModule *getSILModule() {
    return TheSILModule.get();
  }

  std::unique_ptr<SILModule> takeSILModule();

  bool hasSILModule() {
    return static_cast<bool>(TheSILModule);
  }

  ModuleDecl *getMainModule();

  SerializedModuleLoader *getSerializedModuleLoader() const { return SML; }

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
  ArrayRef<SourceFile *> getPrimarySourceFiles() {
    return PrimarySourceFiles;
  }

  /// Gets the Primary Source File if one exists, otherwise the main
  /// module. If multiple Primary Source Files exist, fails with an
  /// assertion.
  ModuleOrSourceFile getPrimarySourceFileOrMainModule() {
    if (PrimarySourceFiles.empty())
      return getMainModule();
    else
      return getPrimarySourceFile();
  }

  /// Gets the SourceFile which is the primary input for this CompilerInstance.
  /// \returns the primary SourceFile, or nullptr if there is no primary input;
  /// if there are _multiple_ primary inputs, fails with an assertion.
  ///
  /// FIXME: This should be removed eventually, once there are no longer any
  /// codepaths that rely on a single primary file.
  SourceFile *getPrimarySourceFile() {
    if (PrimarySourceFiles.empty()) {
      return nullptr;
    } else {
      assert(PrimarySourceFiles.size() == 1);
      return *PrimarySourceFiles.begin();
    }
  }

  /// \brief Returns true if there was an error during setup.
  bool setup(const CompilerInvocation &Invocation);

private:
  /// Set up the file system by loading and validating all VFS overlay YAML
  /// files. If the process of validating VFS files failed, or the overlay
  /// file system could not be initialized, this function returns true. Else it
  /// returns false if setup succeeded.
  bool setUpVirtualFileSystemOverlays();
  void setUpLLVMArguments();
  void setUpDiagnosticOptions();
  bool setUpModuleLoaders();
  bool isInputSwift() {
    return Invocation.getInputKind() == InputFileKind::Swift;
  }
  bool isInSILMode() {
    return Invocation.getInputKind() == InputFileKind::SIL;
  }

  bool setUpInputs();
  Optional<unsigned> setUpCodeCompletionBuffer();

  /// Set up all state in the CompilerInstance to process the given input file.
  /// Return true on error.
  bool setUpForInput(const InputFile &input);

  /// Find a buffer for a given input file and ensure it is recorded in
  /// SourceMgr, PartialModules, or InputSourceCodeBufferIDs as appropriate.
  /// Return the buffer ID if it is not already compiled, or None if so.
  /// Set failed on failure.

  Optional<unsigned> getRecordedBufferID(const InputFile &input, bool &failed);

  /// Given an input file, return a buffer to use for its contents,
  /// and a buffer for the corresponding module doc file if one exists.
  /// On failure, return a null pointer for the first element of the returned
  /// pair.
  std::pair<std::unique_ptr<llvm::MemoryBuffer>,
            std::unique_ptr<llvm::MemoryBuffer>>
  getInputBufferAndModuleDocBufferIfPresent(const InputFile &input);

  /// Try to open the module doc file corresponding to the input parameter.
  /// Return None for error, nullptr if no such file exists, or the buffer if
  /// one was found.
  Optional<std::unique_ptr<llvm::MemoryBuffer>>
  openModuleDoc(const InputFile &input);

public:
  /// Parses and type-checks all input files.
  void performSema();

  /// Parses the input file but does no type-checking or module imports.
  /// Note that this only supports parsing an invocation with a single file.
  void performParseOnly(bool EvaluateConditionals = false,
                        bool ParseDelayedBodyOnEnd = false);

  /// Parses and performs name binding on all input files.
  ///
  /// Like a parse-only invocation, a single file is required. Unlike a
  /// parse-only invocation, module imports will be processed.
  void performParseAndResolveImportsOnly();

private:
  SourceFile *
  createSourceFileForMainModule(SourceFileKind FileKind,
                                SourceFile::ImplicitModuleImportKind ImportKind,
                                Optional<unsigned> BufferID);

public:
  void freeASTContext();

  /// Frees up the SILModule that this instance is holding on to.
  void freeSILModule();

private:
  /// Load stdlib & return true if should continue, i.e. no error
  bool loadStdlib();
  ModuleDecl *importUnderlyingModule();
  ModuleDecl *importBridgingHeader();

  void
  getImplicitlyImportedModules(SmallVectorImpl<ModuleDecl *> &importModules);

public: // for static functions in Frontend.cpp
  struct ImplicitImports {
    SourceFile::ImplicitModuleImportKind kind;
    ModuleDecl *objCModuleUnderlyingMixedFramework;
    ModuleDecl *headerModule;
    SmallVector<ModuleDecl *, 4> modules;

    explicit ImplicitImports(CompilerInstance &compiler);
  };

private:
  void createREPLFile(const ImplicitImports &implicitImports);
  std::unique_ptr<DelayedParsingCallbacks>
  computeDelayedParsingCallback(bool isPrimary);

  void addMainFileToModule(const ImplicitImports &implicitImports);

  void performSemaUpTo(SourceFile::ASTStage_t LimitStage);
  void parseAndCheckTypesUpTo(const ImplicitImports &implicitImports,
                              SourceFile::ASTStage_t LimitStage);

  void parseLibraryFile(unsigned BufferID,
                        const ImplicitImports &implicitImports,
                        PersistentParserState &PersistentState,
                        DelayedParsingCallbacks *PrimaryDelayedCB,
                        DelayedParsingCallbacks *SecondaryDelayedCB);

  /// Return true if had load error
  bool
  parsePartialModulesAndLibraryFiles(const ImplicitImports &implicitImports,
                                     PersistentParserState &PersistentState,
                                     DelayedParsingCallbacks *PrimaryDelayedCB,
                                     DelayedParsingCallbacks *SecondaryDelayedCB);

  OptionSet<TypeCheckingFlags> computeTypeCheckingOptions();

  void forEachFileToTypeCheck(llvm::function_ref<void(SourceFile &)> fn);

  void parseAndTypeCheckMainFileUpTo(SourceFile::ASTStage_t LimitStage,
                                     PersistentParserState &PersistentState,
                                     DelayedParsingCallbacks *DelayedParseCB,
                                     OptionSet<TypeCheckingFlags> TypeCheckOptions);

  void finishTypeChecking(OptionSet<TypeCheckingFlags> TypeCheckOptions);

public:
  const PrimarySpecificPaths &
  getPrimarySpecificPathsForWholeModuleOptimizationMode() const;
  const PrimarySpecificPaths &
  getPrimarySpecificPathsForPrimary(StringRef filename) const;
  const PrimarySpecificPaths &
  getPrimarySpecificPathsForAtMostOnePrimary() const;
  const PrimarySpecificPaths &
  getPrimarySpecificPathsForSourceFile(const SourceFile &SF) const;
};

} // namespace swift

#endif
