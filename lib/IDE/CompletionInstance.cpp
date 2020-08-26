//===--- CompletionInstance.cpp -------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/IDE/CompletionInstance.h"

#include "swift/AST/ASTContext.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/DiagnosticsFrontend.h"
#include "swift/AST/Module.h"
#include "swift/AST/PrettyStackTrace.h"
#include "swift/AST/SourceFile.h"
#include "swift/Basic/LangOptions.h"
#include "swift/Basic/PrettyStackTrace.h"
#include "swift/Basic/SourceManager.h"
#include "swift/ClangImporter/ClangModule.h"
#include "swift/Driver/FrontendUtil.h"
#include "swift/Frontend/Frontend.h"
#include "swift/Parse/Lexer.h"
#include "swift/Parse/PersistentParserState.h"
#include "swift/Serialization/SerializedModuleLoader.h"
#include "swift/Subsystems.h"
#include "clang/AST/ASTContext.h"
#include "llvm/ADT/Hashing.h"
#include "llvm/Support/MemoryBuffer.h"

using namespace swift;
using namespace ide;

std::unique_ptr<llvm::MemoryBuffer>
swift::ide::makeCodeCompletionMemoryBuffer(const llvm::MemoryBuffer *origBuf,
                                           unsigned &Offset,
                                           StringRef bufferIdentifier) {

  auto origBuffSize = origBuf->getBufferSize();
  if (Offset > origBuffSize)
    Offset = origBuffSize;

  auto newBuffer = llvm::WritableMemoryBuffer::getNewUninitMemBuffer(
      origBuffSize + 1, bufferIdentifier);
  auto *pos = origBuf->getBufferStart() + Offset;
  auto *newPos =
      std::copy(origBuf->getBufferStart(), pos, newBuffer->getBufferStart());
  *newPos = '\0';
  std::copy(pos, origBuf->getBufferEnd(), newPos + 1);

  return std::unique_ptr<llvm::MemoryBuffer>(newBuffer.release());
}

namespace {
/// Returns index number of \p D in \p Decls . If it's not found, returns ~0.
template <typename Range>
unsigned findIndexInRange(Decl *D, const Range &Decls) {
  unsigned N = 0;
  for (auto I = Decls.begin(), E = Decls.end(); I != E; ++I) {
    if ((*I)->isImplicit())
      continue;
    if (*I == D)
      return N;
    ++N;
  }
  return ~0U;
}

/// Return the element at \p N in \p Decls .
template <typename Range> Decl *getElementAt(const Range &Decls, unsigned N) {
  for (auto I = Decls.begin(), E = Decls.end(); I != E; ++I) {
    if ((*I)->isImplicit())
      continue;
    if (N == 0)
      return *I;
    --N;
  }
  return nullptr;
}

/// Find the equivalent \c DeclContext with \p DC from \p SF AST.
/// This assumes the AST which contains \p DC has exact the same structure with
/// \p SF.
/// FIXME: This doesn't support IfConfigDecl blocks. If \p DC is in an inactive
///        config block, this function returns \c false.
static DeclContext *getEquivalentDeclContextFromSourceFile(DeclContext *DC,
                                                           SourceFile *SF) {
  PrettyStackTraceDeclContext trace("getting equivalent decl context for", DC);
  auto *newDC = DC;
  // NOTE: Shortcut for DC->getParentSourceFile() == SF case is not needed
  // because they should be always different.

  // Get the index path in the current AST.
  SmallVector<unsigned, 4> IndexStack;
  do {
    auto *D = newDC->getAsDecl();
    if (!D)
      return nullptr;
    auto *parentDC = newDC->getParent();
    unsigned N = ~0U;

    if (auto accessor = dyn_cast<AccessorDecl>(D)) {
      // The AST for accessors is like:
      //   DeclContext -> AbstractStorageDecl -> AccessorDecl
      // We need to push the index of the accessor within the accessor list
      // of the storage.
      auto *storage = accessor->getStorage();
      if (!storage)
        return nullptr;
      auto accessorN = findIndexInRange(accessor, storage->getAllAccessors());
      IndexStack.push_back(accessorN);
      D = storage;
    }

    if (auto parentSF = dyn_cast<SourceFile>(parentDC)) {
      N = findIndexInRange(D, parentSF->getTopLevelDecls());
    } else if (auto parentIDC = dyn_cast_or_null<IterableDeclContext>(
                   parentDC->getAsDecl())) {
      N = findIndexInRange(D, parentIDC->getMembers());
    } else {
#ifndef NDEBUG
      llvm_unreachable("invalid DC kind for finding equivalent DC (indexpath)");
#endif
      return nullptr;
    }

    // Not found in the decl context tree.
    // FIXME: Probably DC is in an inactive #if block.
    if (N == ~0U) {
      return nullptr;
    }

    IndexStack.push_back(N);
    newDC = parentDC;
  } while (!newDC->isModuleScopeContext());

  assert(isa<SourceFile>(newDC) && "DC should be in a SourceFile");

  // Query the equivalent decl context from the base SourceFile using the index
  // path.
  newDC = SF;
  do {
    auto N = IndexStack.pop_back_val();
    Decl *D = nullptr;
    if (auto parentSF = dyn_cast<SourceFile>(newDC))
      D = getElementAt(parentSF->getTopLevelDecls(), N);
    else if (auto parentIDC = dyn_cast<IterableDeclContext>(newDC->getAsDecl()))
      D = getElementAt(parentIDC->getMembers(), N);
    else
      llvm_unreachable("invalid DC kind for finding equivalent DC (query)");

    if (auto storage = dyn_cast_or_null<AbstractStorageDecl>(D)) {
      if (IndexStack.empty())
        return nullptr;
      auto accessorN = IndexStack.pop_back_val();
      D = getElementAt(storage->getAllAccessors(), accessorN);
    }

    newDC = dyn_cast_or_null<DeclContext>(D);
    if (!newDC)
      return nullptr;
  } while (!IndexStack.empty());

  assert(newDC->getContextKind() == DC->getContextKind());

  return newDC;
}

/// For each dependency file in \p CI, run \p callback until the callback
/// returns \c true. Returns \c true if any callback call returns \c true, \c
/// false otherwise.
static bool
forEachDependencyUntilTrue(CompilerInstance &CI, ModuleDecl *CurrentModule,
                           unsigned excludeBufferID,
                           llvm::function_ref<bool(StringRef)> callback) {
  // Check files in the current module.
  for (FileUnit *file : CurrentModule->getFiles()) {
    StringRef filename;
    if (auto SF = dyn_cast<SourceFile>(file)) {
      if (SF->getBufferID() == excludeBufferID)
        continue;
      filename = SF->getFilename();
    } else if (auto LF = dyn_cast<LoadedFile>(file))
      filename = LF->getFilename();
    else
      continue;

    // Ignore synthesized files.
    if (filename.empty() || filename.front() == '<')
      continue;

    if (callback(filename))
      return true;
  }

  // Check other non-system depenencies (e.g. modules, headers).
  for (auto &dep : CI.getDependencyTracker()->getDependencies()) {
    if (callback(dep))
      return true;
  }

  return false;
}

/// Collect hash codes of the dependencies into \c Map.
static void cacheDependencyHashIfNeeded(CompilerInstance &CI,
                                        ModuleDecl *CurrentModule,
                                        unsigned excludeBufferID,
                                        llvm::StringMap<llvm::hash_code> &Map) {
  auto &FS = CI.getFileSystem();
  forEachDependencyUntilTrue(
      CI, CurrentModule, excludeBufferID, [&](StringRef filename) {
        if (Map.count(filename))
          return false;

        auto stat = FS.status(filename);
        if (!stat)
          return false;

        // We will check the hash only if the modification time of the dependecy
        // is zero. See 'areAnyDependentFilesInvalidated() below'.
        if (stat->getLastModificationTime() != llvm::sys::TimePoint<>())
          return false;

        auto buf = FS.getBufferForFile(filename);
        Map[filename] = llvm::hash_value(buf.get()->getBuffer());
        return false;
      });
}

/// Check if any dependent files are modified since \p timestamp.
static bool areAnyDependentFilesInvalidated(
    CompilerInstance &CI, ModuleDecl *CurrentModule, llvm::vfs::FileSystem &FS,
    unsigned excludeBufferID, llvm::sys::TimePoint<> timestamp,
    llvm::StringMap<llvm::hash_code> &Map) {

  return forEachDependencyUntilTrue(
      CI, CurrentModule, excludeBufferID, [&](StringRef filePath) {
        auto stat = FS.status(filePath);
        if (!stat)
          // Missing.
          return true;

        auto lastModTime = stat->getLastModificationTime();
        if (lastModTime > timestamp)
          // Modified.
          return true;

        // If the last modification time is zero, this file is probably from a
        // virtual file system. We need to check the content.
        if (lastModTime == llvm::sys::TimePoint<>()) {
          // Get the hash code of the last content.
          auto oldHashEntry = Map.find(filePath);
          if (oldHashEntry == Map.end())
            // Unreachable? Not virtual in old filesystem, but virtual in new
            // one.
            return true;
          auto oldHash = oldHashEntry->second;

          // Calculate the hash code of the current content.
          auto newContent = FS.getBufferForFile(filePath);
          if (!newContent)
            // Unreachable? stat succeeded, but coundn't get the content.
            return true;

          auto newHash = llvm::hash_value(newContent.get()->getBuffer());

          if (oldHash != newHash)
            return true;
        }

        return false;
      });
}

} // namespace

bool CompletionInstance::performCachedOperationIfPossible(
    const swift::CompilerInvocation &Invocation, llvm::hash_code ArgsHash,
    llvm::IntrusiveRefCntPtr<llvm::vfs::FileSystem> FileSystem,
    llvm::MemoryBuffer *completionBuffer, unsigned int Offset,
    DiagnosticConsumer *DiagC,
    llvm::function_ref<void(CompilerInstance &, bool)> Callback) {
  llvm::PrettyStackTraceString trace(
      "While performing cached completion if possible");

  if (!CachedCI)
    return false;
  if (CachedReuseCount >= MaxASTReuseCount)
    return false;
  if (CachedArgHash != ArgsHash)
    return false;

  auto &CI = *CachedCI;
  auto *oldSF = CI.getCodeCompletionFile().get();
  assert(oldSF->getBufferID());

  auto *oldState = oldSF->getDelayedParserState();
  assert(oldState->hasCodeCompletionDelayedDeclState());
  auto &oldInfo = oldState->getCodeCompletionDelayedDeclState();

  auto &SM = CI.getSourceMgr();
  auto bufferName = completionBuffer->getBufferIdentifier();
  if (SM.getIdentifierForBuffer(*oldSF->getBufferID()) != bufferName)
    return false;

  if (shouldCheckDependencies()) {
    if (areAnyDependentFilesInvalidated(
            CI, CurrentModule, *FileSystem, *oldSF->getBufferID(),
            DependencyCheckedTimestamp, InMemoryDependencyHash))
      return false;
    DependencyCheckedTimestamp = std::chrono::system_clock::now();
  }

  // Parse the new buffer into temporary SourceFile.
  SourceManager tmpSM;
  auto tmpBufferID = tmpSM.addMemBufferCopy(completionBuffer);
  tmpSM.setCodeCompletionPoint(tmpBufferID, Offset);

  LangOptions langOpts = CI.getASTContext().LangOpts;
  langOpts.DisableParserLookup = true;
  TypeCheckerOptions typeckOpts = CI.getASTContext().TypeCheckerOpts;
  SearchPathOptions searchPathOpts = CI.getASTContext().SearchPathOpts;
  DiagnosticEngine tmpDiags(tmpSM);
  std::unique_ptr<ASTContext> tmpCtx(
      ASTContext::get(langOpts, typeckOpts, searchPathOpts, tmpSM, tmpDiags));
  registerParseRequestFunctions(tmpCtx->evaluator);
  registerIDERequestFunctions(tmpCtx->evaluator);
  registerTypeCheckerRequestFunctions(tmpCtx->evaluator);
  registerSILGenRequestFunctions(tmpCtx->evaluator);
  ModuleDecl *tmpM = ModuleDecl::create(Identifier(), *tmpCtx);
  SourceFile *tmpSF = new (*tmpCtx)
      SourceFile(*tmpM, oldSF->Kind, tmpBufferID,
                 SourceFile::ImplicitModuleImportKind::None,
                 /*KeepParsedTokens=*/false, /*BuildSyntaxTree=*/false,
                 oldSF->getParsingOptions());
  tmpSF->enableInterfaceHash();
  // Ensure all non-function-body tokens are hashed into the interface hash
  tmpCtx->LangOpts.EnableTypeFingerprints = false;

  // FIXME: Since we don't setup module loaders on the temporary AST context,
  // 'canImport()' conditional compilation directive always fails. That causes
  // interface hash change and prevents fast-completion.

  // Parse and get the completion context.
  auto *newState = tmpSF->getDelayedParserState();
  // Couldn't find any completion token?
  if (!newState->hasCodeCompletionDelayedDeclState())
    return false;

  auto &newInfo = newState->getCodeCompletionDelayedDeclState();
  unsigned newBufferID;
  DeclContext *traceDC = nullptr;
  switch (newInfo.Kind) {
  case CodeCompletionDelayedDeclKind::FunctionBody: {
    // If the interface has changed, AST must be refreshed.
    llvm::SmallString<32> oldInterfaceHash{};
    llvm::SmallString<32> newInterfaceHash{};
    oldSF->getInterfaceHash(oldInterfaceHash);
    tmpSF->getInterfaceHash(newInterfaceHash);
    if (oldInterfaceHash != newInterfaceHash)
      return false;

    DeclContext *DC =
        getEquivalentDeclContextFromSourceFile(newInfo.ParentContext, oldSF);
    if (!DC || !isa<AbstractFunctionDecl>(DC))
      return false;

    // OK, we can perform fast completion for this. Update the orignal delayed
    // decl state.

    // Fast completion keeps the buffer in memory for multiple completions.
    // To reduce the consumption, slice the source buffer so it only holds
    // the portion that is needed for the second pass.
    auto startOffset = newInfo.StartOffset;
    if (newInfo.PrevOffset != ~0u)
      startOffset = newInfo.PrevOffset;
    auto startLoc = tmpSM.getLocForOffset(tmpBufferID, startOffset);
    startLoc = Lexer::getLocForStartOfLine(tmpSM, startLoc);
    startOffset = tmpSM.getLocOffsetInBuffer(startLoc, tmpBufferID);

    auto endOffset = newInfo.EndOffset;
    auto endLoc = tmpSM.getLocForOffset(tmpBufferID, endOffset);
    endLoc = Lexer::getLocForEndOfToken(tmpSM, endLoc);
    endOffset = tmpSM.getLocOffsetInBuffer(endLoc, tmpBufferID);

    newInfo.StartOffset -= startOffset;
    newInfo.EndOffset -= startOffset;
    if (newInfo.PrevOffset != ~0u)
      newInfo.PrevOffset -= startOffset;

    auto sourceText =
        completionBuffer->getBuffer().slice(startOffset, endOffset);
    auto newOffset = Offset - startOffset;

    newBufferID = SM.addMemBufferCopy(sourceText, bufferName);
    SM.openVirtualFile(SM.getLocForBufferStart(newBufferID),
                       tmpSM.getDisplayNameForLoc(startLoc),
                       tmpSM.getLineAndColumn(startLoc).first - 1);
    SM.setCodeCompletionPoint(newBufferID, newOffset);

    // Construct dummy scopes. We don't need to restore the original scope
    // because they are probably not 'isResolvable()' anyway.
    auto &SI = oldState->getScopeInfo();
    assert(SI.getCurrentScope() == nullptr);
    Scope Top(SI, ScopeKind::TopLevel);
    Scope Body(SI, ScopeKind::FunctionBody);

    oldInfo.ParentContext = DC;
    oldInfo.StartOffset = newInfo.StartOffset;
    oldInfo.EndOffset = newInfo.EndOffset;
    oldInfo.PrevOffset = newInfo.PrevOffset;
    oldState->restoreCodeCompletionDelayedDeclState(oldInfo);

    auto *AFD = cast<AbstractFunctionDecl>(DC);
    if (AFD->isBodySkipped())
      AFD->setBodyDelayed(AFD->getBodySourceRange());

    traceDC = AFD;
    break;
  }
  case CodeCompletionDelayedDeclKind::Decl:
  case CodeCompletionDelayedDeclKind::TopLevelCodeDecl: {
    // Support decl/top-level code only if the completion happens in a single
    // file 'main' script (e.g. playground).
    auto *oldM = oldInfo.ParentContext->getParentModule();
    if (oldM->getFiles().size() != 1 || oldSF->Kind != SourceFileKind::Main)
      return false;

    // Perform fast completion.

    // Prepare the new buffer in the source manager.
    auto sourceText = completionBuffer->getBuffer();
    if (newInfo.Kind == CodeCompletionDelayedDeclKind::TopLevelCodeDecl) {
      // We don't need the source text after the top-level code.
      auto endOffset = newInfo.EndOffset;
      auto endLoc = tmpSM.getLocForOffset(tmpBufferID, endOffset);
      endLoc = Lexer::getLocForEndOfToken(tmpSM, endLoc);
      endOffset = tmpSM.getLocOffsetInBuffer(endLoc, tmpBufferID);
      sourceText = sourceText.slice(0, endOffset);
    }
    newBufferID = SM.addMemBufferCopy(sourceText, bufferName);
    SM.setCodeCompletionPoint(newBufferID, Offset);

    // Create a new module and a source file using the current AST context.
    auto &Ctx = oldM->getASTContext();
    auto newM = ModuleDecl::create(oldM->getName(), Ctx);
    CompilerInstance::ImplicitImports implicitImport(CI);
    SourceFile *newSF = new (Ctx) SourceFile(*newM, SourceFileKind::Main,
                                             newBufferID, implicitImport.kind);
    newM->addFile(*newSF);
    CompilerInstance::addAdditionalInitialImportsTo(newSF, implicitImport);
    newSF->enableInterfaceHash();

    // Tell the compiler instance we've replaced the code completion file.
    CI.setCodeCompletionFile(newSF);

    // Re-process the whole file (parsing will be lazily triggered). Still
    // re-use imported modules.
    performImportResolution(*newSF);
    bindExtensions(*newSF);

    CurrentModule = newM;
    traceDC = newM;
#ifndef NDEBUG
    const auto *reparsedState = newSF->getDelayedParserState();
    assert(reparsedState->hasCodeCompletionDelayedDeclState() &&
           "Didn't find completion token?");

    auto &reparsedInfo = reparsedState->getCodeCompletionDelayedDeclState();
    assert(reparsedInfo.Kind == newInfo.Kind);
#endif
    break;
  }
  }

  {
    PrettyStackTraceDeclContext trace("performing cached completion", traceDC);

    if (DiagC)
      CI.addDiagnosticConsumer(DiagC);

    Callback(CI, /*reusingASTContext=*/true);

    if (DiagC)
      CI.removeDiagnosticConsumer(DiagC);
  }

  CachedReuseCount += 1;

  return true;
}

bool CompletionInstance::performNewOperation(
    Optional<llvm::hash_code> ArgsHash, swift::CompilerInvocation &Invocation,
    llvm::IntrusiveRefCntPtr<llvm::vfs::FileSystem> FileSystem,
    llvm::MemoryBuffer *completionBuffer, unsigned int Offset,
    std::string &Error, DiagnosticConsumer *DiagC,
    llvm::function_ref<void(CompilerInstance &, bool)> Callback) {
  llvm::PrettyStackTraceString trace("While performing new completion");

  auto isCachedCompletionRequested = ArgsHash.hasValue();

  auto TheInstance = std::make_unique<CompilerInstance>();

  // Track dependencies in fast-completion mode to invalidate the compiler
  // instance if any dependent files are modified.
  if (isCachedCompletionRequested)
    TheInstance->createDependencyTracker(false);

  {
    auto &CI = *TheInstance;
    if (DiagC)
      CI.addDiagnosticConsumer(DiagC);

    SWIFT_DEFER {
      if (DiagC)
        CI.removeDiagnosticConsumer(DiagC);
    };

    if (FileSystem != llvm::vfs::getRealFileSystem())
      CI.getSourceMgr().setFileSystem(FileSystem);

    Invocation.setCodeCompletionPoint(completionBuffer, Offset);

    if (CI.setup(Invocation)) {
      Error = "failed to setup compiler instance";
      return false;
    }
    registerIDERequestFunctions(CI.getASTContext().evaluator);

    CI.performParseAndResolveImportsOnly();

    // If we didn't create a source file for completion, bail. This can happen
    // if for example we fail to load the stdlib.
    auto completionFile = CI.getCodeCompletionFile();
    if (!completionFile)
      return true;

    // If we didn't find a code completion token, bail.
    auto *state = completionFile.get()->getDelayedParserState();
    if (!state->hasCodeCompletionDelayedDeclState())
      return true;

    Callback(CI, /*reusingASTContext=*/false);
  }

  // Cache the compiler instance if fast completion is enabled.
  if (isCachedCompletionRequested)
    cacheCompilerInstance(std::move(TheInstance), *ArgsHash);

  return true;
}

void CompletionInstance::cacheCompilerInstance(
    std::unique_ptr<CompilerInstance> CI, llvm::hash_code ArgsHash) {
  CachedCI = std::move(CI);
  CurrentModule = CachedCI->getMainModule();
  CachedArgHash = ArgsHash;
  auto now = std::chrono::system_clock::now();
  DependencyCheckedTimestamp = now;
  CachedReuseCount = 0;
  InMemoryDependencyHash.clear();
  cacheDependencyHashIfNeeded(
      *CachedCI, CurrentModule,
      CachedCI->getASTContext().SourceMgr.getCodeCompletionBufferID(),
      InMemoryDependencyHash);
}

bool CompletionInstance::shouldCheckDependencies() const {
  assert(CachedCI);
  using namespace std::chrono;
  auto now = system_clock::now();
  return DependencyCheckedTimestamp + seconds(DependencyCheckIntervalSecond) <
         now;
}

void CompletionInstance::setDependencyCheckIntervalSecond(unsigned Value) {
  std::lock_guard<std::mutex> lock(mtx);
  DependencyCheckIntervalSecond = Value;
}

bool swift::ide::CompletionInstance::performOperation(
    swift::CompilerInvocation &Invocation, llvm::ArrayRef<const char *> Args,
    llvm::IntrusiveRefCntPtr<llvm::vfs::FileSystem> FileSystem,
    llvm::MemoryBuffer *completionBuffer, unsigned int Offset,
    bool EnableASTCaching, std::string &Error, DiagnosticConsumer *DiagC,
    llvm::function_ref<void(CompilerInstance &, bool)> Callback) {

  // Always disable source location resolutions from .swiftsourceinfo file
  // because they're somewhat heavy operations and aren't needed for completion.
  Invocation.getFrontendOptions().IgnoreSwiftSourceInfo = true;

  // Disable to build syntax tree because code-completion skips some portion of
  // source text. That breaks an invariant of syntax tree building.
  Invocation.getLangOptions().BuildSyntaxTree = false;

  // Since caching uses the interface hash, and since per type fingerprints
  // weaken that hash, disable them here:
  Invocation.getLangOptions().EnableTypeFingerprints = false;

  // We don't need token list.
  Invocation.getLangOptions().CollectParsedToken = false;

  // FIXME: ASTScopeLookup doesn't support code completion yet.
  Invocation.disableASTScopeLookup();

  if (EnableASTCaching) {
    // Compute the signature of the invocation.
    llvm::hash_code ArgsHash(0);
    for (auto arg : Args)
      ArgsHash = llvm::hash_combine(ArgsHash, StringRef(arg));

    // Concurrent completions will block so that they have higher chance to use
    // the cached completion instance.
    std::lock_guard<std::mutex> lock(mtx);

    if (performCachedOperationIfPossible(Invocation, ArgsHash, FileSystem,
                                         completionBuffer, Offset, DiagC,
                                         Callback))
      return true;

    if (performNewOperation(ArgsHash, Invocation, FileSystem, completionBuffer,
                            Offset, Error, DiagC, Callback))
      return true;
  } else {
    // Concurrent completions may happen in parallel when caching is disabled.
    if (performNewOperation(None, Invocation, FileSystem, completionBuffer,
                            Offset, Error, DiagC, Callback))
      return true;
  }

  assert(!Error.empty());
  return false;
}
