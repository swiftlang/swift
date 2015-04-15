//===-- Frontend.h - frontend utility methods ------------------*- C++ -*--===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file contains declarations of utility methods for parsing and
// performing semantic on modules.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_FRONTEND_H
#define SWIFT_FRONTEND_H

#include "swift/Basic/DiagnosticConsumer.h"
#include "swift/Basic/DiagnosticOptions.h"
#include "swift/Basic/LangOptions.h"
#include "swift/Basic/SourceManager.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/IRGenOptions.h"
#include "swift/AST/LinkLibrary.h"
#include "swift/AST/Module.h"
#include "swift/AST/SearchPathOptions.h"
#include "swift/AST/SILOptions.h"
#include "swift/Parse/CodeCompletionCallbacks.h"
#include "swift/Parse/Parser.h"
#include "swift/ClangImporter/ClangImporter.h"
#include "swift/ClangImporter/ClangImporterOptions.h"
#include "swift/Frontend/FrontendOptions.h"
#include "swift/Sema/SourceLoader.h"
#include "swift/Serialization/Validation.h"
#include "swift/SIL/SILModule.h"
#include "llvm/ADT/IntrusiveRefCntPtr.h"
#include "llvm/Option/ArgList.h"
#include "llvm/Support/Host.h"
#include "llvm/Support/MemoryBuffer.h"

#include <memory>

namespace swift {

class SerializedModuleLoader;

class CompilerInvocation {
  LangOptions LangOpts;
  FrontendOptions FrontendOpts;
  ClangImporterOptions ClangImporterOpts;
  SearchPathOptions SearchPathOpts;
  DiagnosticOptions DiagnosticOpts;
  SILOptions SILOpts;
  IRGenOptions IRGenOpts;

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
  /// \returns true if there was an error, false on success.
  bool parseArgs(ArrayRef<const char *> Args, DiagnosticEngine &Diags);

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
  /// to DWARF and inject SDKPath if necessary.
  static void buildDWARFDebugFlags(std::string &Output,
                                   const ArrayRef<const char*> &Args,
                                   StringRef SDKPath,
                                   StringRef ResourceDir);

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

  void setFrameworkSearchPaths(const std::vector<std::string> &Paths) {
    SearchPathOpts.FrameworkSearchPaths = Paths;
  }

  ArrayRef<std::string> getFrameworkSearchPaths() const {
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

  void setSerializedDiagnosticsPath(StringRef Path) {
    FrontendOpts.SerializedDiagnosticsPath = Path;
  }
  StringRef getSerializedDiagnosticsPath() const {
    return FrontendOpts.SerializedDiagnosticsPath;
  }

  LangOptions &getLangOptions() {
    return LangOpts;
  }
  const LangOptions &getLangOptions() const {
    return LangOpts;
  }

  FrontendOptions &getFrontendOptions() { return FrontendOpts; }
  const FrontendOptions &getFrontendOptions() const { return FrontendOpts; }

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

  SILOptions &getSILOptions() { return SILOpts; }
  const SILOptions &getSILOptions() const { return SILOpts; }

  IRGenOptions &getIRGenOptions() { return IRGenOpts; }
  const IRGenOptions &getIRGenOptions() const { return IRGenOpts; }

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

  void addInputFilename(StringRef Filename) {
    FrontendOpts.InputFilenames.push_back(Filename);
  }

  /// Does not take ownership of \p Buf.
  void addInputBuffer(llvm::MemoryBuffer *Buf) {
    FrontendOpts.InputBuffers.push_back(Buf);
  }

  void clearInputs() {
    FrontendOpts.InputFilenames.clear();
    FrontendOpts.InputBuffers.clear();
  }

  ArrayRef<std::string> getInputFilenames() const {
    return FrontendOpts.InputFilenames;
  }
  ArrayRef<llvm::MemoryBuffer*> getInputBuffers() const {
    return FrontendOpts.InputBuffers;
  }

  StringRef getOutputFilename() const {
    return FrontendOpts.getSingleOutputFilename();
  }

  void setCodeCompletionPoint(llvm::MemoryBuffer *Buf, unsigned Offset) {
    assert(Buf);
    CodeCompletionBuffer = Buf;
    CodeCompletionOffset = Offset;
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
    if (Factory)
      LangOpts.EnableCodeCompletionDelayedEnumConformanceHack = true;
  }

  CodeCompletionCallbacksFactory *getCodeCompletionFactory() const {
    return CodeCompletionFactory;
  }

  void setDelayedFunctionBodyParsing(bool Val) {
    FrontendOpts.DelayedFunctionBodyParsing = Val;
  }

  bool isDelayedFunctionBodyParsing() const {
    return FrontendOpts.DelayedFunctionBodyParsing;
  }
};

class CompilerInstance {
  CompilerInvocation Invocation;
  SourceManager SourceMgr;
  DiagnosticEngine Diagnostics{SourceMgr};
  std::unique_ptr<ASTContext> Context;
  std::unique_ptr<SILModule> TheSILModule;

  DependencyTracker *DepTracker = nullptr;
  ReferencedNameTracker *NameTracker = nullptr;

  Module *MainModule = nullptr;
  SerializedModuleLoader *SML = nullptr;

  /// Contains buffer IDs for input source code files.
  std::vector<unsigned> BufferIDs;

  struct PartialModuleInputs {
    std::unique_ptr<llvm::MemoryBuffer> ModuleBuffer;
    std::unique_ptr<llvm::MemoryBuffer> ModuleDocBuffer;
  };

  /// Contains \c MemoryBuffers for partial serialized module files and
  /// corresponding partial serialized module documentation files.
  std::vector<PartialModuleInputs> PartialModules;

  enum : unsigned { NO_SUCH_BUFFER = ~0U };
  unsigned MainBufferID = NO_SUCH_BUFFER;
  unsigned PrimaryBufferID = NO_SUCH_BUFFER;

  SourceFile *PrimarySourceFile = nullptr;

  void createSILModule();
  void setPrimarySourceFile(SourceFile *SF);

public:
  SourceManager &getSourceMgr() { return SourceMgr; }

  DiagnosticEngine &getDiags() { return Diagnostics; }

  ASTContext &getASTContext() {
    return *Context;
  }
  bool hasASTContext() const { return Context != nullptr; }

  SILOptions &getSILOptions() { return Invocation.getSILOptions(); }
  const SILOptions &getSILOptions() const { return Invocation.getSILOptions(); }

  void addDiagnosticConsumer(DiagnosticConsumer *DC) {
    Diagnostics.addConsumer(*DC);
  }

  void setDependencyTracker(DependencyTracker *DT) {
    assert(!Context && "must be called before setup()");
    DepTracker = DT;
  }
  DependencyTracker *getDependencyTracker() {
    return DepTracker;
  }

  void setReferencedNameTracker(ReferencedNameTracker *tracker) {
    assert(!PrimarySourceFile && "must be called before performSema()");
    NameTracker = tracker;
  }
  ReferencedNameTracker *getReferencedNameTracker() {
    return NameTracker;
  }

  /// Set the SIL module for this compilation instance.
  ///
  /// The CompilerInstance takes ownership of the given SILModule object.
  void setSILModule(std::unique_ptr<SILModule> M) {
    TheSILModule = std::move(M);
  }

  SILModule *getSILModule() {
    return TheSILModule.get();
  }

  std::unique_ptr<SILModule> takeSILModule() {
    return std::move(TheSILModule);
  }

  bool hasSILModule() {
    return static_cast<bool>(TheSILModule);
  }

  Module *getMainModule();

  SerializedModuleLoader *getSerializedModuleLoader() const { return SML; }

  ArrayRef<unsigned> getInputBufferIDs() const { return BufferIDs; }

  ArrayRef<LinkLibrary> getLinkLibraries() const {
    return Invocation.getLinkLibraries();
  }

  bool hasSourceImport() const {
    return Invocation.getFrontendOptions().EnableSourceImport;
  }

  /// Gets the SourceFile which is the primary input for this CompilerInstance.
  /// \returns the primary SourceFile, or nullptr if there is no primary input
  SourceFile *getPrimarySourceFile() { return PrimarySourceFile; }

  /// \brief Returns true if there was an error during setup.
  bool setup(const CompilerInvocation &Invocation);

  /// Parses and type-checks all input files.
  void performSema();

  /// Parses the input file but does no type-checking or module imports.
  /// Note that this only supports parsing an invocation with a single file.
  void performParseOnly();
};

} // namespace swift

#endif
