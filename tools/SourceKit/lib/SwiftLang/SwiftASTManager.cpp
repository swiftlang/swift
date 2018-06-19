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
#include "swift/Driver/FrontendUtil.h"
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
                        DiagnosticKind Kind,
                        StringRef FormatString,
                        ArrayRef<DiagnosticArgument> FormatArgs,
                        const DiagnosticInfo &Info) override {
    // FIXME: Print location info if available.
    switch (Kind) {
      case DiagnosticKind::Error: OS << "error: "; break;
      case DiagnosticKind::Warning: OS << "warning: "; break;
      case DiagnosticKind::Note: OS << "note: "; break;
      case DiagnosticKind::Remark: OS << "remark: "; break;
    }
    DiagnosticEngine::formatDiagnosticText(OS, FormatString, FormatArgs);
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
    assert(this->Invok.getFrontendOptions()
               .InputsAndOutputs.hasUniquePrimaryInput() &&
           "Must have exactly one primary input for code completion, etc.");
  }

  void applyTo(CompilerInvocation &CompInvok) const;
  void
  applyToSubstitutingInputs(CompilerInvocation &CompInvok,
                            FrontendInputsAndOutputs &&InputsAndOutputs) const;
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
void InvocationOptions::applyToSubstitutingInputs(
    CompilerInvocation &CompInvok,
    FrontendInputsAndOutputs &&inputsAndOutputs) const {
  CompInvok = this->Invok;
  CompInvok.getFrontendOptions().InputsAndOutputs = inputsAndOutputs;
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
    SwiftStatistics &Stats;
    SmallVector<ImmutableTextSnapshotRef, 4> Snapshots;
    EditorDiagConsumer CollectDiagConsumer;
    CompilerInstance CompInst;
    OwnedResolver TypeResolver{ nullptr, nullptr };
    WorkQueue Queue{ WorkQueue::Dequeuing::Serial, "sourcekit.swift.ConsumeAST" };

    Implementation(uint64_t Generation, SwiftStatistics &Statistics)
        : Generation(Generation), Stats(Statistics) {}

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

  ASTUnit::ASTUnit(uint64_t Generation, SwiftStatistics &Stats)
      : Impl(*new Implementation(Generation, Stats)) {
    auto numASTs = ++Stats.numASTsInMem;
    Stats.maxASTsInMem.updateMax(numASTs);
  }

  ASTUnit::~ASTUnit() {
    --Impl.Stats.numASTsInMem;
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
} // namespace SourceKit

namespace {

typedef uint64_t BufferStamp;

struct FileContent {
  ImmutableTextSnapshotRef Snapshot;
  std::string Filename;
  std::unique_ptr<llvm::MemoryBuffer> Buffer;
  bool IsPrimary;
  BufferStamp Stamp;

  FileContent(ImmutableTextSnapshotRef Snapshot, std::string Filename,
              std::unique_ptr<llvm::MemoryBuffer> Buffer, bool IsPrimary,
              BufferStamp Stamp)
      : Snapshot(std::move(Snapshot)), Filename(Filename),
        Buffer(std::move(Buffer)), IsPrimary(IsPrimary), Stamp(Stamp) {}

  explicit operator InputFile() const {
    return InputFile(Filename, IsPrimary, Buffer.get());
  }
};

class ASTProducer : public ThreadSafeRefCountedBase<ASTProducer> {
  SwiftInvocationRef InvokRef;
  SmallVector<BufferStamp, 8> Stamps;
  ThreadSafeRefCntPtr<ASTUnit> AST;
  SmallVector<std::pair<std::string, BufferStamp>, 8> DependencyStamps;

  struct QueuedConsumer {
    SwiftASTConsumerRef consumer;
    std::vector<ImmutableTextSnapshotRef> snapshots;
    const void *oncePerASTToken;
  };

  std::vector<QueuedConsumer> QueuedConsumers;
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

  void enqueueConsumer(SwiftASTConsumerRef Consumer,
                       ArrayRef<ImmutableTextSnapshotRef> Snapshots,
                       const void *OncePerASTToken);

  using ConsumerPredicate = llvm::function_ref<bool(
      SwiftASTConsumer *, ArrayRef<ImmutableTextSnapshotRef>)>;
  std::vector<SwiftASTConsumerRef> takeConsumers(ConsumerPredicate predicate);

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

  void findSnapshotAndOpenFiles(SwiftASTManager::Implementation &MgrImpl,
                                ArrayRef<ImmutableTextSnapshotRef> Snapshots,
                                SmallVectorImpl<FileContent> &Contents,
                                std::string &Error) const;
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
        Stats(LangSupport.getStatistics()),
        RuntimeResourcePath(LangSupport.getRuntimeResourcePath()) {}

  SwiftEditorDocumentFileMap &EditorDocs;
  SwiftStatistics &Stats;
  std::string RuntimeResourcePath;
  SourceManager SourceMgr;
  Cache<ASTKey, ASTProducerRef> ASTCache{ "sourcekit.swift.ASTCache" };
  llvm::sys::Mutex CacheMtx;

  WorkQueue ASTBuildQueue{ WorkQueue::Dequeuing::Serial,
                           "sourcekit.swift.ASTBuilding" };

  ASTProducerRef getASTProducer(SwiftInvocationRef InvokRef);
  FileContent getFileContent(StringRef FilePath, bool IsPrimary,
                             std::string &Error);
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

static FrontendInputsAndOutputs
convertFileContentsToInputs(const SmallVectorImpl<FileContent> &contents) {
  FrontendInputsAndOutputs inputsAndOutputs;
  for (const FileContent &content : contents)
    inputsAndOutputs.addInput(InputFile(content));
  return inputsAndOutputs;
}

static FrontendInputsAndOutputs
resolveSymbolicLinksInInputs(FrontendInputsAndOutputs &inputsAndOutputs,
                             StringRef UnresolvedPrimaryFile,
                             std::string &Error) {
  unsigned primaryCount = 0;
  std::string PrimaryFile =
      SwiftLangSupport::resolvePathSymlinks(UnresolvedPrimaryFile);
  // FIXME: The frontend should be dealing with symlinks, maybe similar to
  // clang's FileManager ?
  FrontendInputsAndOutputs replacementInputsAndOutputs;
  for (const InputFile &input : inputsAndOutputs.getAllInputs()) {
    std::string newFilename =
        SwiftLangSupport::resolvePathSymlinks(input.file());
    bool newIsPrimary = input.isPrimary() ||
                        (!PrimaryFile.empty() && PrimaryFile == newFilename);
    if (newIsPrimary) {
      ++primaryCount;
    }
    assert(primaryCount < 2 && "cannot handle multiple primaries");
    replacementInputsAndOutputs.addInput(
        InputFile(newFilename, newIsPrimary, input.buffer()));
  }

  if (PrimaryFile.empty() || primaryCount == 1) {
    return replacementInputsAndOutputs;
  }

  llvm::SmallString<64> Err;
  llvm::raw_svector_ostream OS(Err);
  OS << "'" << PrimaryFile << "' is not part of the input files";
  Error = OS.str();
  return replacementInputsAndOutputs;
}

bool SwiftASTManager::initCompilerInvocation(CompilerInvocation &Invocation,
                                             ArrayRef<const char *> OrigArgs,
                                             DiagnosticEngine &Diags,
                                             StringRef UnresolvedPrimaryFile,
                                             std::string &Error) {
  SmallVector<const char *, 16> Args(OrigArgs.begin(), OrigArgs.end());
  Args.push_back("-resource-dir");
  Args.push_back(Impl.RuntimeResourcePath.c_str());

  if (auto driverInvocation = driver::createCompilerInvocation(Args, Diags)) {
    Invocation = *driverInvocation;
  } else {
    // FIXME: Get the actual diagnostic.
    Error = "error when parsing the compiler arguments";
    return true;
  }

  Invocation.getFrontendOptions().InputsAndOutputs =
      resolveSymbolicLinksInInputs(
          Invocation.getFrontendOptions().InputsAndOutputs,
          UnresolvedPrimaryFile, Error);
  if (!Error.empty())
    return true;

  ClangImporterOptions &ImporterOpts = Invocation.getClangImporterOptions();
  ImporterOpts.DetailedPreprocessingRecord = true;

  assert(!Invocation.getModuleName().empty());
  Invocation.getLangOptions().AttachCommentsToDecls = true;
  Invocation.getLangOptions().DiagnosticsEditorMode = true;
  Invocation.getLangOptions().CollectParsedToken = true;
  auto &FrontendOpts = Invocation.getFrontendOptions();
  if (FrontendOpts.PlaygroundTransform) {
    // The playground instrumenter changes the AST in ways that disrupt the
    // SourceKit functionality. Since we don't need the instrumenter, and all we
    // actually need is the playground semantics visible to the user, like
    // silencing the "expression resolves to an unused l-value" error, disable it.
    FrontendOpts.PlaygroundTransform = false;
  }

  // Disable the index-store functionality for the sourcekitd requests.
  FrontendOpts.IndexStorePath.clear();
  ImporterOpts.IndexStorePath.clear();

  // Force the action type to be -typecheck. This affects importing the
  // SwiftONoneSupport module.
  FrontendOpts.RequestedAction = FrontendOptions::ActionType::Typecheck;

  // We don't care about LLVMArgs
  FrontendOpts.LLVMArgs.clear();

  // Disable expensive SIL options to reduce time spent in SILGen.
  disableExpensiveSILOptions(Invocation.getSILOptions());

  return false;
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

bool SwiftASTManager::initCompilerInvocationNoInputs(
    swift::CompilerInvocation &Invocation, ArrayRef<const char *> OrigArgs,
    swift::DiagnosticEngine &Diags, std::string &Error, bool AllowInputs) {

  SmallVector<const char *, 16> Args(OrigArgs.begin(), OrigArgs.end());
  // Use stdin as a .swift input to satisfy the driver.
  Args.push_back("-");
  if (initCompilerInvocation(Invocation, Args, Diags, "", Error))
    return true;

  if (!AllowInputs &&
      Invocation.getFrontendOptions().InputsAndOutputs.inputCount() > 1) {
    Error = "unexpected input in compiler arguments";
    return true;
  }

  // Clear the inputs.
  Invocation.getFrontendOptions().InputsAndOutputs.clearInputs();
  return false;
}

SwiftInvocationRef
SwiftASTManager::getInvocation(ArrayRef<const char *> OrigArgs,
                               StringRef PrimaryFile,
                               std::string &Error) {

  DiagnosticEngine Diags(Impl.SourceMgr);
  EditorDiagConsumer CollectDiagConsumer;
  Diags.addConsumer(CollectDiagConsumer);

  CompilerInvocation CompInvok;
  if (initCompilerInvocation(CompInvok, OrigArgs, Diags, PrimaryFile, Error)) {
    // We create a traced operation here to represent the failure to parse
    // arguments since we cannot reach `createAST` where that would normally
    // happen.
    trace::TracedOperation TracedOp(trace::OperationKind::PerformSema);
    if (TracedOp.enabled()) {
      trace::SwiftInvocation TraceInfo;
      trace::initTraceInfo(TraceInfo, PrimaryFile, OrigArgs);
      TracedOp.setDiagnosticProvider(
        [&CollectDiagConsumer](SmallVectorImpl<DiagnosticEntryInfo> &diags) {
          CollectDiagConsumer.getAllDiagnostics(diags);
        });
      TracedOp.start(TraceInfo);
    }
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
      ++Impl.Stats.numASTsUsedWithSnaphots;
      Unit->Impl.consumeAsync(std::move(ASTConsumer), Unit);
      return;
    }
  }

  Producer->enqueueConsumer(ASTConsumer, Snapshots, OncePerASTToken);

  auto handleAST = [this, Producer, ASTConsumer](ASTUnitRef unit,
                                                 StringRef error) {
    auto consumers = Producer->takeConsumers(
        [&](SwiftASTConsumer *consumer,
            ArrayRef<ImmutableTextSnapshotRef> snapshots) {
          return consumer == ASTConsumer.get() ||
                 !Producer->shouldRebuild(Impl, snapshots) ||
                 (unit && consumer->canUseASTWithSnapshots(snapshots));
        });

    for (auto &consumer : consumers) {
      if (unit)
        unit->Impl.consumeAsync(std::move(consumer), unit);
      else
        consumer->failed(error);
    }
  };

  Producer->getASTUnitAsync(Impl, Snapshots, std::move(handleAST));
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
                                          bool IsPrimary, StringRef FilePath) {
  auto Buf = llvm::MemoryBuffer::getMemBufferCopy(
      Snap->getBuffer()->getText(), FilePath);
  return FileContent(Snap, FilePath, std::move(Buf), IsPrimary,
                     Snap->getStamp());
}

FileContent SwiftASTManager::Implementation::getFileContent(
    StringRef UnresolvedPath, bool IsPrimary, std::string &Error) {
  std::string FilePath = SwiftLangSupport::resolvePathSymlinks(UnresolvedPath);
  if (auto EditorDoc = EditorDocs.findByPath(FilePath))
    return getFileContentFromSnap(EditorDoc->getLatestSnapshot(), IsPrimary,
                                  FilePath);

  // FIXME: Is there a way to get timestamp and buffer for a file atomically ?
  auto Stamp = getBufferStamp(FilePath);
  auto Buffer = getMemoryBuffer(FilePath, Error);
  return FileContent(nullptr, UnresolvedPath, std::move(Buffer), IsPrimary,
                     Stamp);
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
  } else {
    ++MgrImpl.Stats.numASTCacheHits;
  }

  return AST;
}

void ASTProducer::enqueueConsumer(SwiftASTConsumerRef consumer,
                                  ArrayRef<ImmutableTextSnapshotRef> snapshots,
                                  const void *oncePerASTToken) {
  llvm::sys::ScopedLock L(Mtx);
  if (oncePerASTToken) {
    for (auto I = QueuedConsumers.begin(),
              E = QueuedConsumers.end(); I != E; ++I) {
      if (I->oncePerASTToken == oncePerASTToken) {
        I->consumer->cancelled();
        QueuedConsumers.erase(I);
        break;
      }
    }
  }
  QueuedConsumers.push_back({std::move(consumer), snapshots, oncePerASTToken});
}

std::vector<SwiftASTConsumerRef>
ASTProducer::takeConsumers(ConsumerPredicate predicate) {
  llvm::sys::ScopedLock L(Mtx);
  std::vector<SwiftASTConsumerRef> consumers;

  QueuedConsumers.erase(std::remove_if(QueuedConsumers.begin(),
      QueuedConsumers.end(), [&](QueuedConsumer &qc) {
    if (predicate(qc.consumer.get(), qc.snapshots)) {
      consumers.push_back(std::move(qc.consumer));
      return true;
    }
    return false;
  }), QueuedConsumers.end());
  return consumers;
}

bool ASTProducer::shouldRebuild(SwiftASTManager::Implementation &MgrImpl,
                                ArrayRef<ImmutableTextSnapshotRef> Snapshots) {
  const SwiftInvocation::Implementation &Invok = InvokRef->Impl;

  // Check if the inputs changed.
  SmallVector<BufferStamp, 8> InputStamps;
  InputStamps.reserve(
      Invok.Opts.Invok.getFrontendOptions().InputsAndOutputs.inputCount());
  for (const auto &input :
       Invok.Opts.Invok.getFrontendOptions().InputsAndOutputs.getAllInputs()) {
    const std::string &File = input.file();
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
  assert(InputStamps.size() ==
         Invok.Opts.Invok.getFrontendOptions().InputsAndOutputs.inputCount());
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
  ++MgrImpl.Stats.numASTBuilds;

  Stamps.clear();
  DependencyStamps.clear();

  SmallVector<FileContent, 8> Contents;
  findSnapshotAndOpenFiles(MgrImpl, Snapshots, Contents, Error);

  for (auto &Content : Contents)
    Stamps.push_back(Content.Stamp);

  ASTUnitRef ASTRef = new ASTUnit(++ASTUnitGeneration, MgrImpl.Stats);
  for (auto &Content : Contents) {
    if (Content.Snapshot)
      ASTRef->Impl.Snapshots.push_back(Content.Snapshot);
  }
  auto &CompIns = ASTRef->Impl.CompInst;
  auto &Consumer = ASTRef->Impl.CollectDiagConsumer;
  // Display diagnostics to stderr.
  CompIns.addDiagnosticConsumer(&Consumer);

  trace::TracedOperation TracedOp(trace::OperationKind::PerformSema);
  trace::SwiftInvocation TraceInfo;
  if (TracedOp.enabled()) {
    trace::initTraceInfo(TraceInfo, InvokRef->Impl.Opts.PrimaryFile,
                         InvokRef->Impl.Opts.Args);
    TracedOp.setDiagnosticProvider(
        [&Consumer](SmallVectorImpl<DiagnosticEntryInfo> &diags) {
          Consumer.getAllDiagnostics(diags);
        });
  }

  CompilerInvocation Invocation;
  InvokRef->Impl.Opts.applyToSubstitutingInputs(
      Invocation, convertFileContentsToInputs(Contents));

  Invocation.getLangOptions().CollectParsedToken = true;

  if (CompIns.setup(Invocation)) {
    // FIXME: Report the diagnostic.
    LOG_WARN_FUNC("Compilation setup failed!!!");
    Error = "compilation setup failed";
    return nullptr;
  }

  if (TracedOp.enabled()) {
    TracedOp.start(TraceInfo);
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
      SILOptions SILOpts = Invocation.getSILOptions();
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

void ASTProducer::findSnapshotAndOpenFiles(
    SwiftASTManager::Implementation &MgrImpl,
    ArrayRef<ImmutableTextSnapshotRef> Snapshots,
    SmallVectorImpl<FileContent> &Contents, std::string &Error) const {
  const InvocationOptions &Opts = InvokRef->Impl.Opts;
  for (const auto &input :
       Opts.Invok.getFrontendOptions().InputsAndOutputs.getAllInputs()) {
    const std::string &File = input.file();
    bool IsPrimary = input.isPrimary();
    bool FoundSnapshot = false;
    for (auto &Snap : Snapshots) {
      if (Snap->getFilename() == File) {
        FoundSnapshot = true;
        Contents.push_back(getFileContentFromSnap(Snap, IsPrimary, File));
        break;
      }
    }
    if (FoundSnapshot)
      continue;

    auto Content = MgrImpl.getFileContent(File, IsPrimary, Error);
    if (!Content.Buffer) {
      LOG_WARN_FUNC("failed getting file contents for " << File << ": "
                                                        << Error);
      // File may not exist, continue and recover as if it was empty.
      Content.Buffer = llvm::MemoryBuffer::getNewMemBuffer(0, File);
    }
    Contents.push_back(std::move(Content));
  }
  assert(Contents.size() ==
         Opts.Invok.getFrontendOptions().InputsAndOutputs.inputCount());
}
