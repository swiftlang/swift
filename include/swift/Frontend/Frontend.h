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

#include "swift/Basic/DiagnosticConsumer.h"
#include "swift/Basic/LangOptions.h"
#include "swift/Basic/SourceManager.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/Module.h"
#include "swift/Parse/CodeCompletionCallbacks.h"
#include "swift/Parse/Parser.h"
#include "swift/ClangImporter/ClangImporter.h"
#include "swift/Sema/SourceLoader.h"
#include "swift/SIL/SILModule.h"
#include "llvm/ADT/IntrusiveRefCntPtr.h"
#include "llvm/Support/Host.h"

#include <memory>

namespace llvm {
  class MemoryBuffer;
}

namespace swift {

class CompilerInvocation {
  std::string TargetTriple;
  std::string ClangModuleCachePath;
  std::vector<std::string> ImportSearchPaths;
  std::vector<std::string> FrameworkSearchPaths;
  std::string MainExecutablePath;
  std::string SDKPath;

  LangOptions LangOpts;

  bool ParseStdlib = false;
  bool ParseOnly = false;
  TranslationUnit::TUKind TUKind = TranslationUnit::Main;

  std::string ModuleName;

  std::vector<std::string> InputFilenames;
  std::vector<llvm::MemoryBuffer *> InputBuffers;

  llvm::MemoryBuffer *CodeCompletionBuffer = nullptr;

  /// \brief Code completion offset in bytes from the beginning of the main
  /// source file.  Valid only if \c isCodeCompletion() == true.
  unsigned CodeCompletionOffset = ~0U;

  CodeCompletionCallbacksFactory *CodeCompletionFactory = nullptr;

  bool DelayedFunctionBodyParsing = false;

  typedef decltype(&ClangImporter::create) ClangImporterCtorTy;
  ClangImporterCtorTy ImporterCtor = nullptr;

public:
  CompilerInvocation();

  void setTargetTriple(StringRef Triple) {
    TargetTriple = Triple.str();
  }

  StringRef getTargetTriple() const {
    return TargetTriple;
  }

  void setClangModuleCachePath(StringRef Path) {
    ClangModuleCachePath = Path.str();
  }

  StringRef getClangModuleCachePath() const {
    return ClangModuleCachePath;
  }

  void setImportSearchPaths(const std::vector<std::string> &Paths) {
    ImportSearchPaths = Paths;
  }

  std::vector<std::string> getImportSearchPaths() const {
    return ImportSearchPaths;
  }

  void setFrameworkSearchPaths(const std::vector<std::string> &Paths) {
    FrameworkSearchPaths = Paths;
  }

  std::vector<std::string> getFrameworkSearchPaths() const {
    return FrameworkSearchPaths;
  }

  void setMainExecutablePath(const std::string &Path) {
    MainExecutablePath = Path;
  }

  std::string getRuntimeIncludePath() const;

  void setSDKPath(const std::string &Path) {
    // Capture the clang importer entrypoint here, so that clients that don't
    // want to link in clang don't have to.
    ImporterCtor = &ClangImporter::create;
    SDKPath = Path;
  }

  ClangImporterCtorTy getClangImporterCtor() {
    return ImporterCtor;
  }

  StringRef getSDKPath() const {
    return SDKPath;
  }

  LangOptions &getLangOptions() {
    return LangOpts;
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

  void setTUKind(TranslationUnit::TUKind K) {
    TUKind = K;
  }

  TranslationUnit::TUKind getTUKind() const {
    return TUKind;
  }

  void setModuleName(StringRef Name) {
    ModuleName = Name.str();
  }

  StringRef getModuleName() const {
    return ModuleName;
  }

  void addInputFilename(StringRef Filename) {
    InputFilenames.push_back(Filename);
  }

  void addInputBuffer(llvm::MemoryBuffer *Buf) {
    InputBuffers.push_back(Buf);
  }

  void clearInputs() {
    InputFilenames.clear();
    InputBuffers.clear();
  }

  ArrayRef<std::string> getInputFilenames() const { return InputFilenames; }
  ArrayRef<llvm::MemoryBuffer*> getInputBuffers() const { return InputBuffers; }

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
    DelayedFunctionBodyParsing = Val;
  }

  bool isDelayedFunctionBodyParsing() const { return DelayedFunctionBodyParsing; }
};

class CompilerInstance {
  CompilerInvocation Invocation;
  SourceManager SourceMgr;
  std::vector<unsigned> BufferIDs;
  DiagnosticEngine Diagnostics;
  std::unique_ptr<ASTContext> Context;
  std::unique_ptr<SILModule> TheSILModule;

  TranslationUnit *TU;

  void createSILModule();

public:
  CompilerInstance() : Diagnostics(SourceMgr), TU(nullptr) {
  }

  SourceManager &getSourceMgr() { return SourceMgr; }

  DiagnosticEngine &getDiags() { return Diagnostics; }

  void addDiagnosticConsumer(DiagnosticConsumer *DC) {
    Diagnostics.addConsumer(*DC);
  }

  ASTContext &getASTContext() {
    return *Context;
  }

  void setSILModule(SILModule *M) {
    TheSILModule.reset(M);
  }

  SILModule *getSILModule() {
    return TheSILModule.get();
  }

  bool hasSILModule() {
    return static_cast<bool>(TheSILModule);
  }

  TranslationUnit *getTU() {
    return TU;
  }

  ArrayRef<unsigned> getInputBufferIDs() const { return BufferIDs; }

  /// \brief Returns true if there was an error during setup.
  bool setup(const CompilerInvocation &Invocation);

  void doIt();
};

} // namespace swift

