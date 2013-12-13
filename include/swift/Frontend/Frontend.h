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
#include "swift/Basic/LangOptions.h"
#include "swift/Basic/SourceManager.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/LinkLibrary.h"
#include "swift/AST/Module.h"
#include "swift/AST/SearchPathOptions.h"
#include "swift/Parse/CodeCompletionCallbacks.h"
#include "swift/Parse/Parser.h"
#include "swift/ClangImporter/ClangImporter.h"
#include "swift/ClangImporter/ClangImporterOptions.h"
#include "swift/Frontend/FrontendOptions.h"
#include "swift/Sema/SourceLoader.h"
#include "swift/SIL/SILModule.h"
#include "llvm/ADT/IntrusiveRefCntPtr.h"
#include "llvm/Support/Host.h"

#include <memory>

namespace llvm {
  class MemoryBuffer;
}

namespace swift {

class SerializedModuleLoader;

class CompilerInvocation {
  std::string TargetTriple;
  SmallVector<LinkLibrary, 4> LinkLibraries;
  std::string ModuleSourceListPath;

  LangOptions LangOpts;
  FrontendOptions FrontendOpts;
  ClangImporterOptions ClangImporterOpts;
  SearchPathOptions SearchPathOpts;

  bool ParseStdlib = false;
  bool ParseOnly = false;
  bool Immediate = false;
  SourceFileKind InputKind = SourceFileKind::Main;

  std::vector<llvm::MemoryBuffer *> InputBuffers;

  llvm::MemoryBuffer *CodeCompletionBuffer = nullptr;

  /// \brief Code completion offset in bytes from the beginning of the main
  /// source file.  Valid only if \c isCodeCompletion() == true.
  unsigned CodeCompletionOffset = ~0U;

  CodeCompletionCallbacksFactory *CodeCompletionFactory = nullptr;

public:
  CompilerInvocation();

  /// \brief Initializes the compiler invocation for the list of arguments.
  /// \returns true if there was an error, false on success.
  bool parseArgs(ArrayRef<const char *> Args, DiagnosticEngine &Diags);

  void setTargetTriple(StringRef Triple) {
    TargetTriple = Triple.str();
  }

  StringRef getTargetTriple() const {
    return TargetTriple;
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
    LinkLibraries.push_back({name, kind});
  }

  ArrayRef<LinkLibrary> getLinkLibraries() const {
    return LinkLibraries;
  }

  void setMainExecutablePath(StringRef Path);

  void setRuntimeIncludePath(StringRef Path) {
    SearchPathOpts.RuntimeIncludePath = Path;
  }

  StringRef getRuntimeIncludePath() const {
    return SearchPathOpts.RuntimeIncludePath;
  }

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

  void setModuleSourceListPath(StringRef Path) {
    ModuleSourceListPath = Path;
  }
  StringRef getModuleSourceListPath() const {
    return ModuleSourceListPath;
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

  void setParseStdlib() {
    ParseStdlib = true;
  }

  bool getParseStdlib() const {
    return ParseStdlib;
  }

  void setParseOnly() {
    ParseOnly = true;
  }

  bool getParseOnly() const {
    return ParseOnly;
  }

  void setInputKind(SourceFileKind K) {
    InputKind = K;
  }

  SourceFileKind getInputKind() const {
    return InputKind;
  }

  void setModuleName(StringRef Name) {
    FrontendOpts.ModuleName = Name.str();
  }

  StringRef getModuleName() const {
    return FrontendOpts.ModuleName;
  }

  void addInputFilename(StringRef Filename) {
    FrontendOpts.InputFilenames.push_back(Filename);
  }

  void addInputBuffer(llvm::MemoryBuffer *Buf) {
    InputBuffers.push_back(Buf);
  }

  void clearInputs() {
    FrontendOpts.InputFilenames.clear();
    InputBuffers.clear();
  }

  ArrayRef<std::string> getInputFilenames() const {
    return FrontendOpts.InputFilenames;
  }
  ArrayRef<llvm::MemoryBuffer*> getInputBuffers() const { return InputBuffers; }

  void setOutputFilename(StringRef Filename) {
    FrontendOpts.OutputFilename = Filename;
  }
  StringRef getOutputFilename() const {
    return FrontendOpts.OutputFilename;
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

  void setImmediate(bool Val) {
    Immediate = Val;
  }
  
  bool isImmediate() const {
    return Immediate;
  }
};

class CompilerInstance {
  CompilerInvocation Invocation;
  SourceManager SourceMgr;
  DiagnosticEngine Diagnostics{SourceMgr};
  std::unique_ptr<ASTContext> Context;
  std::unique_ptr<SILModule> TheSILModule;

  Module *MainModule = nullptr;
  SerializedModuleLoader *SML = nullptr;

  std::vector<unsigned> BufferIDs;

  enum : unsigned { NO_SUCH_BUFFER = ~0U };
  unsigned MainBufferIndex = NO_SUCH_BUFFER;

  void createSILModule();

public:
  SourceManager &getSourceMgr() { return SourceMgr; }

  DiagnosticEngine &getDiags() { return Diagnostics; }

  void addDiagnosticConsumer(DiagnosticConsumer *DC) {
    Diagnostics.addConsumer(*DC);
  }

  ASTContext &getASTContext() {
    return *Context;
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

  Module *getMainModule() {
    return MainModule;
  }

  SerializedModuleLoader *getSerializedModuleLoader() const { return SML; }

  ArrayRef<unsigned> getInputBufferIDs() const { return BufferIDs; }

  ArrayRef<LinkLibrary> getLinkLibraries() const {
    return Invocation.getLinkLibraries();
  }

  /// \brief Returns true if there was an error during setup.
  bool setup(const CompilerInvocation &Invocation);

  /// Parses and type-checks all input files.
  void performParse();
};

} // namespace swift

#endif
