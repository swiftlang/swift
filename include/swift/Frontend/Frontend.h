//===-- Frontend.h - frontend utility methods ----------------------------===//
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

#include "swift/Basic/LLVM.h"
#include "swift/Basic/DiagnosticConsumer.h"
#include "swift/Basic/LangOptions.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/Module.h"
#include "swift/Parse/CodeCompletionCallbacks.h"
#include "swift/Parse/Parser.h"
#include "swift/Sema/SourceLoader.h"
#include "swift/SIL/SILModule.h"
#include "llvm/ADT/IntrusiveRefCntPtr.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/Host.h"

#include <memory>

namespace llvm {
  class MemoryBuffer;
}

namespace swift {
  class ASTContext;
  class SILModule;

class CompilerInvocation : public llvm::RefCountedBase<CompilerInvocation> {
  std::string TargetTriple;
  std::string ClangModuleCachePath;
  std::vector<std::string> ImportSearchPaths;
  std::string MainExecutablePath;
  std::string SDKPath;

  LangOptions LangOpts;

  bool ParseStdlib = false;
  bool ParseOnly = false;
  TranslationUnit::TUKind TUKind = TranslationUnit::Main;

  llvm::SourceMgr DriverDiagsSourceMgr;
  DiagnosticEngine DriverDiagnostics;
  std::vector<DiagnosticConsumer *> DiagnosticConsumers;

  std::string ModuleName;

  CodeCompletionCallbacksFactory *CodeCompletionFactory = nullptr;

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

  void setImportSearchPaths(std::vector<std::string> Paths) {
    ImportSearchPaths = std::move(Paths);
  }

  std::vector<std::string> getImportSearchPaths() const {
    return ImportSearchPaths;
  }

  void setMainExecutablePath(std::string Path) {
    MainExecutablePath = Path;
  }

  std::string getRuntimeIncludePath() const;

  void setSDKPath(std::string Path) {
    SDKPath = Path;
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

  DiagnosticEngine &getDriverDiags() {
    return DriverDiagnostics;
  }

  void addDiagnosticConsumer(DiagnosticConsumer *DC) {
    DriverDiagnostics.addConsumer(*DC);
    DiagnosticConsumers.push_back(DC);
  }

  ArrayRef<DiagnosticConsumer *> getDiagnosticConsumers() const {
    return DiagnosticConsumers;
  }

  void setModuleName(StringRef Name) {
    ModuleName = Name.str();
  }

  StringRef getModuleName() const {
    return ModuleName;
  }

  void setCodeCompletionFactory(CodeCompletionCallbacksFactory *Factory) {
    CodeCompletionFactory = Factory;
  }

  CodeCompletionCallbacksFactory *getCodeCompletionFactory() const {
    return CodeCompletionFactory;
  }
};

class CompilerInstance {
  llvm::IntrusiveRefCntPtr<CompilerInvocation> Invocation;
  llvm::SourceMgr SourceMgr;
  std::vector<unsigned> BufferIDs;
  DiagnosticEngine Diagnostics;
  std::unique_ptr<ASTContext> Context;
  std::unique_ptr<SILModule> TheSILModule;
  std::unique_ptr<Parser> TheParser;

  TranslationUnit *TU;

  void createSILModule();

public:
  CompilerInstance(llvm::IntrusiveRefCntPtr<CompilerInvocation> Invocation)
      : Invocation(Invocation), Diagnostics(SourceMgr), TU(nullptr) {
  }

  llvm::SourceMgr &getSourceMgr() { return SourceMgr; }

  void setBufferIDs(std::vector<unsigned> IDs) {
    BufferIDs = std::move(IDs);
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

  void setup();

  void doIt();
};

} // namespace swift

