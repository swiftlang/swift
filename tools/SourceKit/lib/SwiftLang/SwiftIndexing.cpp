//===--- SwiftIndexing.cpp ------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "SwiftASTManager.h"
#include "SwiftLangSupport.h"
#include "SourceKit/Support/Logging.h"
#include "SourceKit/Support/Tracing.h"
#include "SourceKit/Support/UIdent.h"

#include "swift/Frontend/Frontend.h"
#include "swift/Frontend/PrintingDiagnosticConsumer.h"
#include "swift/Index/Index.h"
#include "swift/Serialization/SerializedModuleLoader.h"
// This is included only for createLazyResolver(). Move to different header ?
#include "swift/Sema/IDETypeChecking.h"
#include "llvm/Support/Path.h"

using namespace SourceKit;
using namespace swift;
using namespace swift::index;

static UIdent KindImportModuleClang("source.lang.swift.import.module.clang");
static UIdent KindImportModuleSwift("source.lang.swift.import.module.swift");

static UIdent getUIDForDependencyKind(SymbolKind depKind) {
  switch (depKind) {
  case SymbolKind::Module:
    return KindImportModuleSwift;
  case SymbolKind::ClangModule:
    return KindImportModuleClang;
  default:
    return UIdent();
  }
}

class SKIndexDataConsumer : public IndexDataConsumer {
public:
  SKIndexDataConsumer(IndexingConsumer &C) : impl(C) {}

private:
  void failed(StringRef error) override { impl.failed(error); }

  void warning(StringRef warning) override {
    LOG_WARN_FUNC(warning);
  }

  bool enableWarnings() override {
    return Logger::isLoggingEnabledForLevel(Logger::Level::Warning);
  }

  bool recordHash(StringRef hash, bool isKnown) override {
    return impl.recordHash(hash, isKnown);
  }

  bool startDependency(SymbolKind kind, StringRef name, StringRef path,
                       bool isSystem, StringRef hash) override {
    auto kindUID = getUIDForDependencyKind(kind);
    return impl.startDependency(kindUID, name, path, isSystem, hash);
  }

  bool finishDependency(SymbolKind kind) override {
    return impl.finishDependency(getUIDForDependencyKind(kind));
  }

  bool startSourceEntity(const IndexSymbol &symbol) override {
    return withEntityInfo(symbol, [this](const EntityInfo &info) {
      return impl.startSourceEntity(info);
    });
  }

  bool recordRelatedEntity(const IndexSymbol &symbol) override {
    return withEntityInfo(symbol, [this](const EntityInfo &info) {
      return impl.recordRelatedEntity(info);
    });
  }

  bool finishSourceEntity(SymbolKind kind, SymbolSubKindSet subKinds,
                          SymbolRoleSet roles) override {
    bool isRef = roles & (unsigned)SymbolRole::Reference;
    auto UID = SwiftLangSupport::getUIDForSymbol(kind, subKinds, isRef);
    return impl.finishSourceEntity(UID);
  }

  template <typename F>
  bool withEntityInfo(const IndexSymbol &symbol, F func) {
    EntityInfo info;
    bool isRef = symbol.roles & (unsigned)SymbolRole::Reference;
    info.Kind = SwiftLangSupport::getUIDForSymbol(symbol.kind, symbol.subKinds,
                                                  isRef);
    info.Name = symbol.name;
    info.USR = symbol.USR;
    info.Group = symbol.group;
    info.Line = symbol.line;
    info.Column = symbol.column;
    info.ReceiverUSR = symbol.receiverUSR;
    info.IsDynamic = symbol.roles & (unsigned)SymbolRole::Dynamic;
    info.IsTestCandidate = symbol.subKinds & SymbolSubKind::UnitTest;
    std::vector<UIdent> uidAttrs;
    if (!isRef) {
      uidAttrs =
        SwiftLangSupport::UIDsFromDeclAttributes(symbol.decl->getAttrs());
      info.Attrs = uidAttrs;
    }
    return func(info);
  }

private:
  IndexingConsumer &impl;
};

static void indexModule(llvm::MemoryBuffer *Input,
                        StringRef ModuleName,
                        StringRef Hash,
                        IndexingConsumer &IdxConsumer,
                        CompilerInstance &CI,
                        ArrayRef<const char *> Args) {
  trace::TracedOperation TracedOp;
  if (trace::enabled()) {
    trace::SwiftInvocation SwiftArgs;
    SwiftArgs.Args.Args.assign(Args.begin(), Args.end());
    SwiftArgs.Args.PrimaryFile = Input->getBufferIdentifier();
    SwiftArgs.addFile(Input->getBufferIdentifier(), Input->getBuffer());
    trace::StringPairs OpArgs;
    OpArgs.push_back(std::make_pair("ModuleName", ModuleName));
    OpArgs.push_back(std::make_pair("Hash", Hash));
    TracedOp.start(trace::OperationKind::IndexModule, SwiftArgs, OpArgs);
  }

  ASTContext &Ctx = CI.getASTContext();
  std::unique_ptr<SerializedModuleLoader> Loader;
  Module *Mod = nullptr;
  if (ModuleName == Ctx.StdlibModuleName.str()) {
    Mod = Ctx.getModule({ {Ctx.StdlibModuleName, SourceLoc()} });
  } else {
    Loader = SerializedModuleLoader::create(Ctx);
    auto Buf = std::unique_ptr<llvm::MemoryBuffer>(
        llvm::MemoryBuffer::getMemBuffer(Input->getBuffer(),
                                         Input->getBufferIdentifier()));

    // FIXME: These APIs allocate memory on the ASTContext, meaning it may not
    // be freed for a long time.
    Mod = Module::create(Ctx.getIdentifier(ModuleName), Ctx);
    // Indexing is not using documentation now, so don't open the module
    // documentation file.
    // FIXME: refactor the frontend to provide an easy way to figure out the
    // correct filename here.
    auto FUnit = Loader->loadAST(*Mod, None, std::move(Buf), nullptr);

    // FIXME: Not knowing what went wrong is pretty bad. loadModule() should be
    // more modular, rather than emitting diagnostics itself.
    if (!FUnit) {
      IdxConsumer.failed("failed to load module");
      return;
    }
  }

  // Setup a typechecker for protocol conformance resolving.
  OwnedResolver TypeResolver = createLazyResolver(Ctx);

  SKIndexDataConsumer IdxDataConsumer(IdxConsumer);
  index::indexModule(Mod, Hash, IdxDataConsumer);
}


//===----------------------------------------------------------------------===//
// IndexSource
//===----------------------------------------------------------------------===//

void trace::initTraceInfo(trace::SwiftInvocation &SwiftArgs,
                          StringRef InputFile,
                          ArrayRef<const char *> Args) {
  SwiftArgs.Args.Args.assign(Args.begin(), Args.end());
  SwiftArgs.Args.PrimaryFile = InputFile;
}

void trace::initTraceFiles(trace::SwiftInvocation &SwiftArgs,
                           swift::CompilerInstance &CI) {
  auto &SM = CI.getSourceMgr();
  auto Ids = CI.getInputBufferIDs();
  std::for_each(Ids.begin(), Ids.end(),
                [&] (unsigned Id) {
                  auto Buf = SM.getLLVMSourceMgr().getMemoryBuffer(Id);
                  SwiftArgs.addFile(Buf->getBufferIdentifier(),
                                    Buf->getBuffer());
                });
}

void SwiftLangSupport::indexSource(StringRef InputFile,
                                   IndexingConsumer &IdxConsumer,
                                   ArrayRef<const char *> Args,
                                   StringRef Hash) {
  std::string Error;
  auto InputBuf = ASTMgr->getMemoryBuffer(InputFile, Error);
  if (!InputBuf) {
    IdxConsumer.failed(Error);
    return;
  }

  StringRef Filename = llvm::sys::path::filename(InputFile);
  StringRef FileExt = llvm::sys::path::extension(Filename);

  bool IsModuleIndexing = (FileExt == ".swiftmodule" || FileExt == ".pcm");
  CompilerInstance CI;
  // Display diagnostics to stderr.
  PrintingDiagnosticConsumer PrintDiags;
  CI.addDiagnosticConsumer(&PrintDiags);

  CompilerInvocation Invocation;
  bool Failed = getASTManager().initCompilerInvocation(Invocation, Args,
                                                       CI.getDiags(),
                    /*PrimaryFile=*/IsModuleIndexing ? StringRef() : InputFile,
                                                       Error);
  if (Failed) {
    IdxConsumer.failed(Error);
    return;
  }

  if (IsModuleIndexing) {
    if (CI.setup(Invocation))
      return;
    bool IsClangModule = (FileExt == ".pcm");
    if (IsClangModule) {
      IdxConsumer.failed("Clang module files are not supported");
      return;
    }

    indexModule(InputBuf.get(), llvm::sys::path::stem(Filename),
                Hash, IdxConsumer, CI, Args);
    return;
  }

  if (Invocation.getInputFilenames().empty()) {
    IdxConsumer.failed("no input filenames specified");
    return;
  }

  if (CI.setup(Invocation))
    return;

  trace::TracedOperation TracedOp;
  if (trace::enabled()) {
    trace::SwiftInvocation SwiftArgs;
    trace::initTraceInfo(SwiftArgs, InputFile, Args);
    trace::initTraceFiles(SwiftArgs, CI);
    TracedOp.start(trace::OperationKind::IndexSource, SwiftArgs);
  }

  CI.performSema();

  // NOTE: performSema() may end up with some gruesome error preventing it from
  // setting primary file correctly
  if (!CI.getPrimarySourceFile()) {
    IdxConsumer.failed("no primary source file found");
    return;
  }

  // Setup a typechecker for protocol conformance resolving.
  OwnedResolver TypeResolver = createLazyResolver(CI.getASTContext());

  SKIndexDataConsumer IdxDataConsumer(IdxConsumer);
  index::indexSourceFile(CI.getPrimarySourceFile(), Hash, IdxDataConsumer);
}
