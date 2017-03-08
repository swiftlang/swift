//===--- SwiftASTManager.cpp ----------------------------------------------===//
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

#include "SwiftASTManager.h"
#include "SwiftEditorDiagConsumer.h"
#include "SwiftInvocation.h"
#include "SwiftLangSupport.h"
#include "SourceKit/Core/Context.h"
#include "SourceKit/Support/Concurrency.h"
#include "SourceKit/Support/ImmutableTextBuffer.h"
#include "SourceKit/Support/Logging.h"
#include "SourceKit/Support/Tracing.h"

#include "swift/Basic/Cache.h"
#include "swift/Frontend/Frontend.h"
#include "swift/Frontend/PrintingDiagnosticConsumer.h"
#include "swift/Strings.h"
#include "swift/Subsystems.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
// This is included only for createLazyResolver(). Move to different header ?
#include "swift/Sema/IDETypeChecking.h"

#include "llvm/ADT/FoldingSet.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/Path.h"

using namespace SourceKit;
using namespace swift;
using namespace swift::sys;

namespace {
class StreamDiagConsumer : public DiagnosticConsumer {
  llvm::raw_ostream &OS;

public:
  StreamDiagConsumer(llvm::raw_ostream &OS) : OS(OS) {}

  void handleDiagnostic(SourceManager &SM, SourceLoc Loc,
                        DiagnosticKind Kind, StringRef Text,
                        const DiagnosticInfo &Info) override {
    // FIXME: Print location info if available.
    switch (Kind) {
      case DiagnosticKind::Error: OS << "error: "; break;
      case DiagnosticKind::Warning: OS << "warning: "; break;
      case DiagnosticKind::Note: OS << "note: "; break;
    }
    OS << Text;
  }
};
} // end anonymous namespace

void SwiftASTConsumer::failed(StringRef Error) { }

//===----------------------------------------------------------------------===//
// SwiftInvocation
//===----------------------------------------------------------------------===//

namespace {

struct InvocationOptions {
  const std::vector<std::string> Args;
  const std::string PrimaryFile;
  const CompilerInvocation Invok;

  InvocationOptions(ArrayRef<const char *> CArgs, StringRef PrimaryFile,
                    CompilerInvocation Invok)
    : Args(_convertArgs(CArgs)),
      PrimaryFile(PrimaryFile),
      Invok(std::move(Invok)) {
    // Assert invocation with a primary file. We want to avoid full typechecking
    // for all files.
    assert(!this->PrimaryFile.empty());
    assert(this->Invok.getFrontendOptions().PrimaryInput.hasValue());
  }

  void applyTo(CompilerInvocation &CompInvok) const;
  void profile(llvm::FoldingSetNodeID &ID) const;
  void raw(std::vector<std::string> &Args, std::string &PrimaryFile) const;

private:
  static std::vector<std::string> _convertArgs(ArrayRef<const char *> CArgs) {
    std::vector<std::string> Args;
    Args.reserve(CArgs.size());
    for (auto Arg : CArgs)
      Args.push_back(Arg);
    return Args;
  }
};

struct ASTKey {
  llvm::FoldingSetNodeID FSID;
};

} // end anonymous namespace

struct SwiftInvocation::Implementation {
  InvocationOptions Opts;
  ASTKey Key;

  explicit Implementation(InvocationOptions opts) : Opts(std::move(opts)) {
    Opts.profile(Key.FSID);
  }
};

SwiftInvocation::~SwiftInvocation() {
  delete &Impl;
}

void SwiftInvocation::applyTo(swift::CompilerInvocation &CompInvok) const {
  return Impl.Opts.applyTo(CompInvok);
}

void SwiftInvocation::raw(std::vector<std::string> &Args,
                          std::string &PrimaryFile) const {
  return Impl.Opts.raw(Args, PrimaryFile);
}

void InvocationOptions::applyTo(CompilerInvocation &CompInvok) const {
  CompInvok = this->Invok;
}

void InvocationOptions::raw(std::vector<std::string> &Args,
                            std::string &PrimaryFile) const {
  Args.assign(this->Args.begin(), this->Args.end());
  PrimaryFile = this->PrimaryFile;
}

void InvocationOptions::profile(llvm::FoldingSetNodeID &ID) const {
  // FIXME: This ties ASTs to every argument and the exact order that they were
  // provided, preventing much sharing of ASTs.
  // Note though that previously we tried targeting specific options considered
  // semantically relevant but it proved too fragile (very easy to miss some new
  // compiler invocation option).
  // Possibly have all compiler invocation options auto-generated from a
  // tablegen definition file, thus forcing a decision for each option if it is
  // ok to share ASTs with the option differing.
  for (auto &Arg : Args)
    ID.AddString(Arg);
  ID.AddString(PrimaryFile);
}

//===----------------------------------------------------------------------===//
// SwiftASTManager
//===----------------------------------------------------------------------===//

namespace SourceKit {
  struct ASTUnit::Implementation {
    const uint64_t Generation;
    SmallVector<ImmutableTextSnapshotRef, 4> Snapshots;
    EditorDiagConsumer CollectDiagConsumer;
    CompilerInstance CompInst;
    OwnedResolver TypeResolver{ nullptr, nullptr };
    WorkQueue Queue{ WorkQueue::Dequeuing::Serial, "sourcekit.swift.ConsumeAST" };

    Implementation(uint64_t Generation) : Generation(Generation) {}

    void consumeAsync(SwiftASTConsumerRef ASTConsumer, ASTUnitRef ASTRef);
  };

  void ASTUnit::Implementation::consumeAsync(SwiftASTConsumerRef ConsumerRef,
                                             ASTUnitRef ASTRef) {
    Queue.dispatch([ASTRef, ConsumerRef]{
      SwiftASTConsumer &ASTConsumer = *ConsumerRef;

      CompilerInstance &CI = ASTRef->getCompilerInstance();

      if (CI.getPrimarySourceFile()) {
        ASTConsumer.handlePrimaryAST(ASTRef);
      } else {
        LOG_WARN_FUNC("did not find primary SourceFile");
        ConsumerRef->failed("did not find primary SourceFile");
      }
    });
  }

  ASTUnit::ASTUnit(uint64_t Generation) : Impl(*new Implementation(Generation)) {
  }

  ASTUnit::~ASTUnit() {
    delete &Impl;
  }

  swift::CompilerInstance &ASTUnit::getCompilerInstance() const {
    return Impl.CompInst;
  }

   uint64_t ASTUnit::getGeneration() const {
    return Impl.Generation;
  }

  ArrayRef<ImmutableTextSnapshotRef> ASTUnit::getSnapshots() const {
    return Impl.Snapshots;
  }

  SourceFile &ASTUnit::getPrimarySourceFile() const {
    return *Impl.CompInst.getPrimarySourceFile();
  }

  EditorDiagConsumer &ASTUnit::getEditorDiagConsumer() const {
    return Impl.CollectDiagConsumer;
  }

  void ASTUnit::performAsync(std::function<void()> Fn) {
    Impl.Queue.dispatch(std::move(Fn));
  }
}

namespace {

typedef uint64_t BufferStamp;

struct FileContent {
  ImmutableTextSnapshotRef Snapshot;
  std::unique_ptr<llvm::MemoryBuffer> Buffer;
  BufferStamp Stamp;

  FileContent(ImmutableTextSnapshotRef Snapshot,
              std::unique_ptr<llvm::MemoryBuffer> Buffer,
              BufferStamp Stamp)
    : Snapshot(std::move(Snapshot)),
      Buffer(std::move(Buffer)),
      Stamp(Stamp) {}
};

class ASTProducer : public ThreadSafeRefCountedBase<ASTProducer> {
  SwiftInvocationRef InvokRef;
  SmallVector<BufferStamp, 8> Stamps;
  ThreadSafeRefCntPtr<ASTUnit> AST;
  SmallVector<std::pair<std::string, BufferStamp>, 8> DependencyStamps;
  std::vector<std::pair<SwiftASTConsumerRef, const void*>> QueuedConsumers;
  llvm::sys::Mutex Mtx;

public:
  explicit ASTProducer(SwiftInvocationRef InvokRef)
    : InvokRef(std::move(InvokRef)) {}

  ASTUnitRef getExistingAST() {
    // FIXME: ThreadSafeRefCntPtr is racy.
    llvm::sys::ScopedLock L(Mtx);
    return AST;
  }

  void getASTUnitAsync(SwiftASTManager::Implementation &MgrImpl,
                       ArrayRef<ImmutableTextSnapshotRef> Snapshots,
                std::function<void(ASTUnitRef Unit, StringRef Error)> Receiver);
  bool shouldRebuild(SwiftASTManager::Implementation &MgrImpl,
                     ArrayRef<ImmutableTextSnapshotRef> Snapshots);

  void enqueueConsumer(SwiftASTConsumerRef Consumer, const void *OncePerASTToken);
  std::vector<SwiftASTConsumerRef> popQueuedConsumers();

  size_t getMemoryCost() const {
    // FIXME: Report the memory cost of the overall CompilerInstance.
    if (AST && AST->getCompilerInstance().hasASTContext())
      return AST->Impl.CompInst.getASTContext().getTotalMemory();
    return sizeof(*this) + sizeof(*AST);
  }

private:
  ASTUnitRef getASTUnitImpl(SwiftASTManager::Implementation &MgrImpl,
                            ArrayRef<ImmutableTextSnapshotRef> Snapshots,
                            std::string &Error);

  ASTUnitRef createASTUnit(SwiftASTManager::Implementation &MgrImpl,
                           ArrayRef<ImmutableTextSnapshotRef> Snapshots,
                           std::string &Error);
};

typedef IntrusiveRefCntPtr<ASTProducer> ASTProducerRef;

} // end anonymous namespace

namespace swift {
namespace sys {

template <>
struct CacheValueCostInfo<ASTProducer> {
  static size_t getCost(const ASTProducer &Unit) {
    return Unit.getMemoryCost();
  }
};

template <>
struct CacheKeyHashInfo<ASTKey> {
  static uintptr_t getHashValue(const ASTKey &Key) {
    return Key.FSID.ComputeHash();
  }
  static bool isEqual(void *LHS, void *RHS) {
    return static_cast<ASTKey*>(LHS)->FSID == static_cast<ASTKey*>(RHS)->FSID;
  }
};

} // namespace sys
} // namespace swift

struct SwiftASTManager::Implementation {
  explicit Implementation(SwiftLangSupport &LangSupport)
    : EditorDocs(LangSupport.getEditorDocuments()),
      RuntimeResourcePath(LangSupport.getRuntimeResourcePath()) { }

  SwiftEditorDocumentFileMap &EditorDocs;
  std::string RuntimeResourcePath;
  SourceManager SourceMgr;
  Cache<ASTKey, ASTProducerRef> ASTCache{ "sourcekit.swift.ASTCache" };
  llvm::sys::Mutex CacheMtx;

  WorkQueue ASTBuildQueue{ WorkQueue::Dequeuing::Serial,
                           "sourcekit.swift.ASTBuilding" };

  ASTProducerRef getASTProducer(SwiftInvocationRef InvokRef);
  FileContent getFileContent(StringRef FilePath, std::string &Error);
  BufferStamp getBufferStamp(StringRef FilePath);
  std::unique_ptr<llvm::MemoryBuffer> getMemoryBuffer(StringRef Filename,
                                                      std::string &Error);
};

SwiftASTManager::SwiftASTManager(SwiftLangSupport &LangSupport)
  : Impl(*new Implementation(LangSupport)) {
}

SwiftASTManager::~SwiftASTManager() {
  delete &Impl;
}

std::unique_ptr<llvm::MemoryBuffer>
SwiftASTManager::getMemoryBuffer(StringRef Filename, std::string &Error) {
  return Impl.getMemoryBuffer(Filename, Error);
}

static void setModuleName(CompilerInvocation &Invocation) {
  if (!Invocation.getModuleName().empty())
    return;

  StringRef Filename = Invocation.getOutputFilename();
  if (Filename.empty()) {
    if (Invocation.getInputFilenames().empty()) {
      Invocation.setModuleName("__main__");
      return;
    }
    Filename = Invocation.getInputFilenames()[0];
  }
  Filename = llvm::sys::path::filename(Filename);
  StringRef ModuleName = llvm::sys::path::stem(Filename);
  if (ModuleName.empty() || !Lexer::isIdentifier(ModuleName)) {
    Invocation.setModuleName("__main__");
    return;
  }
  Invocation.setModuleName(ModuleName);
}

static void sanitizeCompilerArgs(ArrayRef<const char *> Args,
                                 SmallVectorImpl<const char *> &NewArgs) {
  for (const char *CArg : Args) {
    StringRef Arg = CArg;
    if (Arg.startswith("-j"))
      continue;
    if (Arg == "-c")
      continue;
    if (Arg == "-Xfrontend")
      continue;
    if (Arg == "-embed-bitcode")
      continue;
    if (Arg == "-enable-bridging-pch" ||
        Arg == "-disable-bridging-pch")
      continue;
    NewArgs.push_back(CArg);
  }
}

bool SwiftASTManager::initCompilerInvocation(CompilerInvocation &Invocation,
                                             ArrayRef<const char *> OrigArgs,
                                             DiagnosticEngine &Diags,
                                             StringRef UnresolvedPrimaryFile,
                                             std::string &Error) {
  SmallVector<const char *, 16> Args;
  sanitizeCompilerArgs(OrigArgs, Args);

  Invocation.setRuntimeResourcePath(Impl.RuntimeResourcePath);
  bool Err = Invocation.parseArgs(Args, Diags);
  if (Err) {
    // FIXME: Get the actual diagnostic.
    Error = "error when parsing the compiler arguments";
    return Err;
  }

  // FIXME: The frontend should be dealing with symlinks, maybe similar to
  // clang's FileManager ?
  std::string PrimaryFile =
    SwiftLangSupport::resolvePathSymlinks(UnresolvedPrimaryFile);
  for (auto &InputFile : Invocation.getFrontendOptions().InputFilenames) {
    InputFile = SwiftLangSupport::resolvePathSymlinks(InputFile);
  }

  ClangImporterOptions &ImporterOpts = Invocation.getClangImporterOptions();
  ImporterOpts.DetailedPreprocessingRecord = true;

  setModuleName(Invocation);
  Invocation.setSerializedDiagnosticsPath(StringRef());
  Invocation.getLangOptions().AttachCommentsToDecls = true;
  Invocation.getLangOptions().DiagnosticsEditorMode = true;
  auto &FrontendOpts = Invocation.getFrontendOptions();
  if (FrontendOpts.PlaygroundTransform) {
    // The playground instrumenter changes the AST in ways that disrupt the
    // SourceKit functionality. Since we don't need the instrumenter, and all we
    // actually need is the playground semantics visible to the user, like
    // silencing the "expression resolves to an unused l-value" error, disable it.
    FrontendOpts.PlaygroundTransform = false;
  }

  if (!PrimaryFile.empty()) {
    Optional<unsigned> PrimaryIndex;
    for (auto i : indices(Invocation.getFrontendOptions().InputFilenames)) {
      auto &CurFile = Invocation.getFrontendOptions().InputFilenames[i];
      if (PrimaryFile == CurFile) {
        PrimaryIndex = i;
        break;
      }
    }
    if (!PrimaryIndex) {
      llvm::SmallString<64> Err;
      llvm::raw_svector_ostream OS(Err);
      OS << "'" << PrimaryFile << "' is not part of the input files";
      Error = OS.str();
      return true;
    }
    Invocation.getFrontendOptions().PrimaryInput = SelectedInput(*PrimaryIndex);
  }

  return Err;
}

bool SwiftASTManager::initCompilerInvocation(CompilerInvocation &CompInvok,
                                             ArrayRef<const char *> OrigArgs,
                                             StringRef PrimaryFile,
                                             std::string &Error) {

  SmallString<32> ErrStr;
  llvm::raw_svector_ostream ErrOS(ErrStr);
  DiagnosticEngine Diagnostics(Impl.SourceMgr);
  StreamDiagConsumer DiagConsumer(ErrOS);
  Diagnostics.addConsumer(DiagConsumer);

  if (initCompilerInvocation(CompInvok, OrigArgs, Diagnostics, PrimaryFile,
                             Error)) {
    if (!ErrOS.str().empty())
      Error = ErrOS.str();
    return true;
  }
  return false;
}

SwiftInvocationRef
SwiftASTManager::getInvocation(ArrayRef<const char *> OrigArgs,
                               StringRef PrimaryFile,
                               std::string &Error) {
  CompilerInvocation CompInvok;
  if (initCompilerInvocation(CompInvok, OrigArgs, PrimaryFile, Error)) {
    return nullptr;
  }

  InvocationOptions Opts(OrigArgs, PrimaryFile, CompInvok);
  return new SwiftInvocation(
      *new SwiftInvocation::Implementation(std::move(Opts)));
}

void SwiftASTManager::processASTAsync(SwiftInvocationRef InvokRef,
                                      SwiftASTConsumerRef ASTConsumer,
                                      const void *OncePerASTToken,
                                 ArrayRef<ImmutableTextSnapshotRef> Snapshots) {
  ASTProducerRef Producer = Impl.getASTProducer(InvokRef);

  if (ASTUnitRef Unit = Producer->getExistingAST()) {
    if (ASTConsumer->canUseASTWithSnapshots(Unit->getSnapshots())) {
      Unit->Impl.consumeAsync(std::move(ASTConsumer), Unit);
      return;
    }
  }

  Producer->enqueueConsumer(std::move(ASTConsumer), OncePerASTToken);

  Producer->getASTUnitAsync(Impl, Snapshots,
    [Producer](ASTUnitRef Unit, StringRef Error) {
      auto Consumers = Producer->popQueuedConsumers();

      for (auto &Consumer : Consumers) {
        if (Unit)
          Unit->Impl.consumeAsync(std::move(Consumer), Unit);
        else
          Consumer->failed(Error);
      }
    });
}

void SwiftASTManager::removeCachedAST(SwiftInvocationRef Invok) {
  Impl.ASTCache.remove(Invok->Impl.Key);
}

ASTProducerRef
SwiftASTManager::Implementation::getASTProducer(SwiftInvocationRef InvokRef) {
  llvm::sys::ScopedLock L(CacheMtx);
  llvm::Optional<ASTProducerRef> OptProducer = ASTCache.get(InvokRef->Impl.Key);
  if (OptProducer.hasValue())
    return OptProducer.getValue();
  ASTProducerRef Producer = new ASTProducer(InvokRef);
  ASTCache.set(InvokRef->Impl.Key, Producer);
  return Producer;
}

static FileContent getFileContentFromSnap(ImmutableTextSnapshotRef Snap,
                                          StringRef FilePath) {
  auto Buf = llvm::MemoryBuffer::getMemBufferCopy(
      Snap->getBuffer()->getText(), FilePath);
  return FileContent(Snap, std::move(Buf), Snap->getStamp());
}

FileContent
SwiftASTManager::Implementation::getFileContent(StringRef UnresolvedPath,
                                                std::string &Error) {
  std::string FilePath = SwiftLangSupport::resolvePathSymlinks(UnresolvedPath);
  if (auto EditorDoc = EditorDocs.findByPath(FilePath))
    return getFileContentFromSnap(EditorDoc->getLatestSnapshot(), FilePath);

  // FIXME: Is there a way to get timestamp and buffer for a file atomically ?
  auto Stamp = getBufferStamp(FilePath);
  auto Buffer = getMemoryBuffer(FilePath, Error);
  return FileContent(nullptr, std::move(Buffer), Stamp);
}

BufferStamp SwiftASTManager::Implementation::getBufferStamp(StringRef FilePath){
  if (auto EditorDoc = EditorDocs.findByPath(FilePath))
    return EditorDoc->getLatestSnapshot()->getStamp();

  llvm::sys::fs::file_status Status;
  if (std::error_code Ret = llvm::sys::fs::status(FilePath, Status)) {
    // Failure to read the file.
    LOG_WARN_FUNC("failed to stat file: " << FilePath
                  << " (" << Ret.message() << ')');
    return -1;
  }
  return Status.getLastModificationTime().time_since_epoch().count();
}

std::unique_ptr<llvm::MemoryBuffer>
SwiftASTManager::Implementation::getMemoryBuffer(StringRef Filename,
                                                 std::string &Error) {
  llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>> FileBufOrErr =
    llvm::MemoryBuffer::getFile(Filename);
  if (FileBufOrErr)
    return std::move(FileBufOrErr.get());

  llvm::raw_string_ostream OSErr(Error);
  OSErr << "error opening input file '" << Filename << "' ("
        << FileBufOrErr.getError().message() << ')';
  return nullptr;
}

void ASTProducer::getASTUnitAsync(SwiftASTManager::Implementation &MgrImpl,
                                  ArrayRef<ImmutableTextSnapshotRef> Snaps,
               std::function<void(ASTUnitRef Unit, StringRef Error)> Receiver) {

  ASTProducerRef ThisProducer = this;
  SmallVector<ImmutableTextSnapshotRef, 4> Snapshots;
  Snapshots.append(Snaps.begin(), Snaps.end());

  MgrImpl.ASTBuildQueue.dispatch([ThisProducer, &MgrImpl, Snapshots, Receiver] {
    std::string Error;
    ASTUnitRef Unit = ThisProducer->getASTUnitImpl(MgrImpl, Snapshots, Error);
    Receiver(Unit, Error);
  }, /*isStackDeep=*/true);
}

ASTUnitRef ASTProducer::getASTUnitImpl(SwiftASTManager::Implementation &MgrImpl,
                                   ArrayRef<ImmutableTextSnapshotRef> Snapshots,
                                   std::string &Error) {
  if (!AST || shouldRebuild(MgrImpl, Snapshots)) {
    bool IsRebuild = AST != nullptr;
    const InvocationOptions &Opts = InvokRef->Impl.Opts;

    LOG_FUNC_SECTION(InfoHighPrio) {
      Log->getOS() << "AST build (";
      if (IsRebuild)
        Log->getOS() << "rebuild";
      else
        Log->getOS() << "first";
      Log->getOS() << "): ";
      Log->getOS() << Opts.Invok.getModuleName() << '/' << Opts.PrimaryFile;
    }

    auto NewAST = createASTUnit(MgrImpl, Snapshots, Error);
    {
      // FIXME: ThreadSafeRefCntPtr is racy.
      llvm::sys::ScopedLock L(Mtx);
      AST = NewAST;
    }

    {
      llvm::sys::ScopedLock L(MgrImpl.CacheMtx);
      // Re-register the object with the cache to update its memory cost.
      ASTProducerRef ThisProducer = this;
      MgrImpl.ASTCache.set(InvokRef->Impl.Key, ThisProducer);
    }
  }

  return AST;
}

void ASTProducer::enqueueConsumer(SwiftASTConsumerRef Consumer,
                                  const void *OncePerASTToken) {
  llvm::sys::ScopedLock L(Mtx);
  if (OncePerASTToken) {
    for (auto I = QueuedConsumers.begin(),
              E = QueuedConsumers.end(); I != E; ++I) {
      if (I->second == OncePerASTToken) {
        I->first->cancelled();
        QueuedConsumers.erase(I);
        break;
      }
    }
  }
  QueuedConsumers.push_back({ std::move(Consumer), OncePerASTToken });
}

std::vector<SwiftASTConsumerRef> ASTProducer::popQueuedConsumers() {
  llvm::sys::ScopedLock L(Mtx);
  std::vector<SwiftASTConsumerRef> Consumers;
  Consumers.reserve(QueuedConsumers.size());
  for (auto &C : QueuedConsumers)
    Consumers.push_back(std::move(C.first));
  QueuedConsumers.clear();
  return Consumers;
}

bool ASTProducer::shouldRebuild(SwiftASTManager::Implementation &MgrImpl,
                                ArrayRef<ImmutableTextSnapshotRef> Snapshots) {
  const SwiftInvocation::Implementation &Invok = InvokRef->Impl;

  // Check if the inputs changed.
  SmallVector<BufferStamp, 8> InputStamps;
  InputStamps.reserve(Invok.Opts.Invok.getInputFilenames().size());
  for (auto &File : Invok.Opts.Invok.getInputFilenames()) {
    bool FoundSnapshot = false;
    for (auto &Snap : Snapshots) {
      if (Snap->getFilename() == File) {
        FoundSnapshot = true;
        InputStamps.push_back(Snap->getStamp());
        break;
      }
    }
    if (!FoundSnapshot)
      InputStamps.push_back(MgrImpl.getBufferStamp(File));
  }
  assert(InputStamps.size() == Invok.Opts.Invok.getInputFilenames().size());
  if (Stamps != InputStamps)
    return true;

  for (auto &Dependency : DependencyStamps) {
    if (Dependency.second != MgrImpl.getBufferStamp(Dependency.first))
      return true;
  }

  return false;
}

static void collectModuleDependencies(ModuleDecl *TopMod,
    llvm::SmallPtrSetImpl<ModuleDecl *> &Visited,
    SmallVectorImpl<std::string> &Filenames) {

  if (!TopMod)
    return;

  auto ClangModuleLoader = TopMod->getASTContext().getClangModuleLoader();
  SmallVector<ModuleDecl::ImportedModule, 8> Imports;
  TopMod->getImportedModules(Imports, ModuleDecl::ImportFilter::All);

  for (auto Import : Imports) {
    ModuleDecl *Mod = Import.second;
    if (Mod->isSystemModule())
      continue;
    // FIXME: Setup dependencies on the included headers.
    if (ClangModuleLoader &&
        Mod == ClangModuleLoader->getImportedHeaderModule())
      continue;
    bool NewVisit = Visited.insert(Mod).second;
    if (!NewVisit)
      continue;

    // FIXME: Handle modules with multiple source files; these will fail on
    // getModuleFilename() (by returning an empty path). Note that such modules
    // may be heterogeneous.
    {
      std::string Path = Mod->getModuleFilename();
      if (Path.empty() || Path == TopMod->getModuleFilename())
        continue; // this is a submodule.
      Filenames.push_back(std::move(Path));
    }

    bool IsClangModule = false;
    for (auto File : Mod->getFiles()) {
      if (File->getKind() == FileUnitKind::ClangModule) {
        IsClangModule = true;
        break;
      }
    }
    if (IsClangModule) {
      // No need to keep track of the clang module dependencies.
      continue;
    }

    collectModuleDependencies(Mod, Visited, Filenames);
  }
}

static std::atomic<uint64_t> ASTUnitGeneration{ 0 };

ASTUnitRef ASTProducer::createASTUnit(SwiftASTManager::Implementation &MgrImpl,
                                      ArrayRef<ImmutableTextSnapshotRef> Snapshots,
                                      std::string &Error) {
  Stamps.clear();
  DependencyStamps.clear();

  const InvocationOptions &Opts = InvokRef->Impl.Opts;

  SmallVector<FileContent, 8> Contents;
  for (auto &File : Opts.Invok.getInputFilenames()) {
    bool FoundSnapshot = false;
    for (auto &Snap : Snapshots) {
      if (Snap->getFilename() == File) {
        FoundSnapshot = true;
        Contents.push_back(getFileContentFromSnap(Snap, File));
        break;
      }
    }
    if (FoundSnapshot)
      continue;

    auto Content = MgrImpl.getFileContent(File, Error);
    if (!Content.Buffer) {
      LOG_WARN_FUNC("failed getting file contents for " << File << ": " << Error);
      // File may not exist, continue and recover as if it was empty.
      Content.Buffer = llvm::MemoryBuffer::getNewMemBuffer(0, File);
    }
    Contents.push_back(std::move(Content));
  }
  assert(Contents.size() == Opts.Invok.getInputFilenames().size());

  for (auto &Content : Contents)
    Stamps.push_back(Content.Stamp);

  trace::SwiftInvocation TraceInfo;

  if (trace::enabled()) {
    TraceInfo.Args.PrimaryFile = Opts.PrimaryFile;
    TraceInfo.Args.Args = Opts.Args;
  }

  ASTUnitRef ASTRef = new ASTUnit(++ASTUnitGeneration);
  for (auto &Content : Contents) {
    if (Content.Snapshot)
      ASTRef->Impl.Snapshots.push_back(Content.Snapshot);

    if (trace::enabled()) {
      TraceInfo.addFile(Content.Buffer->getBufferIdentifier(),
                        Content.Buffer->getBuffer());

    }
  }
  auto &CompIns = ASTRef->Impl.CompInst;
  auto &Consumer = ASTRef->Impl.CollectDiagConsumer;

  // Display diagnostics to stderr.
  CompIns.addDiagnosticConsumer(&Consumer);

  CompilerInvocation Invocation;
  Opts.applyTo(Invocation);

  for (auto &Content : Contents)
    Invocation.addInputBuffer(Content.Buffer.get());

  if (CompIns.setup(Invocation)) {
    // FIXME: Report the diagnostic.
    LOG_WARN_FUNC("Compilation setup failed!!!");
    Error = "compilation setup failed";
    return nullptr;
  }

  trace::TracedOperation TracedOp;
  if (trace::enabled()) {
    TracedOp.start(trace::OperationKind::PerformSema, TraceInfo);
  }

  CloseClangModuleFiles scopedCloseFiles(
      *CompIns.getASTContext().getClangModuleLoader());
  Consumer.setInputBufferIDs(ASTRef->getCompilerInstance().getInputBufferIDs());
  CompIns.performSema();

  llvm::SmallPtrSet<ModuleDecl *, 16> Visited;
  SmallVector<std::string, 8> Filenames;
  collectModuleDependencies(CompIns.getMainModule(), Visited, Filenames);
  // FIXME: There exists a small window where the module file may have been
  // modified after compilation finished and before we get its stamp.
  for (auto &Filename : Filenames) {
    DependencyStamps.push_back(std::make_pair(Filename,
                                              MgrImpl.getBufferStamp(Filename)));
  }

  // Since we only typecheck the primary file (plus referenced constructs
  // from other files), any error is likely to break SIL generation.
  if (!Consumer.hadAnyError()) {
    // FIXME: Any error anywhere in the SourceFile will switch off SIL
    // diagnostics. This means that this can happen:
    //   - The user sees a SIL diagnostic in one function
    //   - The user edits another function in the same file and introduces a
    //     typechecking error.
    //   - The SIL diagnostic in the first function will be gone.
    //
    // Could we maybe selectively SILGen functions from the SourceFile, so
    // that we avoid SILGen'ing the second function with the typecheck error
    // but still allow SILGen'ing the first function ?
    // Or try to keep track of SIL diagnostics emitted previously ?

    // FIXME: We should run SIL diagnostics asynchronously after typechecking
    // so that they don't delay reporting of typechecking diagnostics and they
    // don't block any other AST processing for the same SwiftInvocation.

    if (auto SF = CompIns.getPrimarySourceFile()) {
      SILOptions SILOpts;
      std::unique_ptr<SILModule> SILMod = performSILGeneration(*SF, SILOpts);
      runSILDiagnosticPasses(*SILMod);
    }
  }

  // We mirror the compiler and don't set the TypeResolver during SIL
  // processing. This is to avoid unnecessary typechecking that can occur if the
  // TypeResolver is set before.
  ASTRef->Impl.TypeResolver = createLazyResolver(CompIns.getASTContext());

  return ASTRef;
}
