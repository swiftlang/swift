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
#include "swift/AST/SourceFile.h"
#include "swift/Basic/LangOptions.h"
#include "swift/Basic/SourceManager.h"
#include "swift/Driver/FrontendUtil.h"
#include "swift/Frontend/Frontend.h"
#include "swift/Parse/PersistentParserState.h"
#include "swift/Subsystems.h"
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
    if (*I == D)
      return N;
    ++N;
  }
  return ~0U;
}

/// Return the element at \p N in \p Decls .
template <typename Range> Decl *getElementAt(const Range &Decls, unsigned N) {
  assert(std::distance(Decls.begin(), Decls.end()) > N);
  auto I = Decls.begin();
  std::advance(I, N);
  return *I;
}

/// Find the equivalent \c DeclContext with \p DC from \p SF AST.
/// This assumes the AST which contains \p DC has exact the same structure with
/// \p SF.
/// FIXME: This doesn't support IfConfigDecl blocks. If \p DC is in an inactive
///        config block, this function returns \c false.
static DeclContext *getEquivalentDeclContextFromSourceFile(DeclContext *DC,
                                                           SourceFile *SF) {
  auto *newDC = DC;
  // NOTE: Shortcut for DC->getParentSourceFile() == SF case is not needed
  // because they should be always different.

  // Get the index path in the current AST.
  SmallVector<unsigned, 4> IndexStack;
  do {
    auto *D = newDC->getAsDecl();
    auto *parentDC = newDC->getParent();
    unsigned N;
    if (auto parentSF = dyn_cast<SourceFile>(parentDC))
      N = findIndexInRange(D, parentSF->Decls);
    else if (auto parentIDC =
                 dyn_cast<IterableDeclContext>(parentDC->getAsDecl()))
      N = findIndexInRange(D, parentIDC->getMembers());
    else
      llvm_unreachable("invalid DC kind for finding equivalent DC (indexpath)");

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
    Decl *D;
    if (auto parentSF = dyn_cast<SourceFile>(newDC))
      D = getElementAt(parentSF->Decls, N);
    else if (auto parentIDC = dyn_cast<IterableDeclContext>(newDC->getAsDecl()))
      D = getElementAt(parentIDC->getMembers(), N);
    else
      llvm_unreachable("invalid DC kind for finding equivalent DC (query)");
    newDC = dyn_cast<DeclContext>(D);
  } while (!IndexStack.empty());

  assert(newDC->getContextKind() == DC->getContextKind());

  return newDC;
}

} // namespace

bool CompletionInstance::performCachedOperaitonIfPossible(
    const swift::CompilerInvocation &Invocation, llvm::hash_code ArgsHash,
    llvm::MemoryBuffer *completionBuffer, unsigned int Offset,
    DiagnosticConsumer *DiagC,
    llvm::function_ref<void(CompilerInstance &)> Callback) {

  if (!CachedCI)
    return false;
  if (CachedReuseCount >= MaxASTReuseCount)
    return false;
  if (CachedArgHash != ArgsHash)
    return false;

  auto &CI = *CachedCI;

  auto &oldState = CI.getPersistentParserState();
  if (!oldState.hasCodeCompletionDelayedDeclState())
    return false;

  auto &SM = CI.getSourceMgr();
  if (SM.getIdentifierForBuffer(SM.getCodeCompletionBufferID()) !=
      completionBuffer->getBufferIdentifier())
    return false;

  auto &oldInfo = oldState.getCodeCompletionDelayedDeclState();

  // Currently, only completions within a function body is supported.
  if (oldInfo.Kind != CodeCompletionDelayedDeclKind::FunctionBody)
    return false;

  auto newBufferID = SM.addMemBufferCopy(completionBuffer);
  SM.setCodeCompletionPoint(newBufferID, Offset);

  // Parse the new buffer into temporary SourceFile.
  LangOptions langOpts;
  langOpts.DisableParserLookup = true;
  TypeCheckerOptions typeckOpts;
  SearchPathOptions searchPathOpts;
  DiagnosticEngine Diags(SM);
  std::unique_ptr<ASTContext> Ctx(
      ASTContext::get(langOpts, typeckOpts, searchPathOpts, SM, Diags));
  registerIDERequestFunctions(Ctx->evaluator);
  registerTypeCheckerRequestFunctions(Ctx->evaluator);
  ModuleDecl *M = ModuleDecl::create(Identifier(), *Ctx);
  unsigned BufferID = SM.getCodeCompletionBufferID();
  PersistentParserState newState;
  SourceFile *newSF =
      new (*Ctx) SourceFile(*M, SourceFileKind::Library, BufferID,
                            SourceFile::ImplicitModuleImportKind::None);
  newSF->enableInterfaceHash();
  parseIntoSourceFileFull(*newSF, BufferID, &newState);
  // Couldn't find any completion token?
  if (!newState.hasCodeCompletionDelayedDeclState())
    return false;

  auto &newInfo = newState.getCodeCompletionDelayedDeclState();

  // The new completion must happens in function body too.
  if (newInfo.Kind != CodeCompletionDelayedDeclKind::FunctionBody)
    return false;

  auto *oldSF = oldInfo.ParentContext->getParentSourceFile();

  // If the interface has changed, AST must be refreshed.
  llvm::SmallString<32> oldInterfaceHash{};
  llvm::SmallString<32> newInterfaceHash{};
  oldSF->getInterfaceHash(oldInterfaceHash);
  newSF->getInterfaceHash(newInterfaceHash);
  if (oldInterfaceHash != newInterfaceHash)
    return false;

  DeclContext *DC =
      getEquivalentDeclContextFromSourceFile(newInfo.ParentContext, oldSF);
  if (!DC)
    return false;

  // OK, we can perform fast completion for this. Update the orignal delayed
  // decl state.

  // Construct dummy scopes. We don't need to restore the original scope
  // because they are probably not 'isResolvable()' anyway.
  auto &SI = oldState.getScopeInfo();
  assert(SI.getCurrentScope() == nullptr);
  Scope Top(SI, ScopeKind::TopLevel);
  Scope Body(SI, ScopeKind::FunctionBody);

  oldInfo.ParentContext = DC;
  oldInfo.StartOffset = newInfo.StartOffset;
  oldInfo.EndOffset = newInfo.EndOffset;
  oldInfo.PrevOffset = newInfo.PrevOffset;
  oldState.restoreCodeCompletionDelayedDeclState(oldInfo);

  auto *AFD = cast<AbstractFunctionDecl>(DC);
  if (AFD->isBodySkipped())
    AFD->setBodyDelayed(AFD->getBodySourceRange());
  if (DiagC)
    CI.addDiagnosticConsumer(DiagC);

  CI.getDiags().diagnose(SM.getLocForOffset(BufferID, newInfo.StartOffset),
                          diag::completion_reusing_astcontext);

  Callback(CI);

  if (DiagC)
    CI.removeDiagnosticConsumer(DiagC);

  CachedReuseCount += 1;

  return true;
}

bool CompletionInstance::performNewOperation(
    Optional<llvm::hash_code> ArgsHash, swift::CompilerInvocation &Invocation,
    llvm::IntrusiveRefCntPtr<llvm::vfs::FileSystem> FileSystem,
    llvm::MemoryBuffer *completionBuffer, unsigned int Offset,
    std::string &Error, DiagnosticConsumer *DiagC,
    llvm::function_ref<void(CompilerInstance &)> Callback) {

  auto TheInstance = std::make_unique<CompilerInstance>();
  auto &CI = *TheInstance;
  if (DiagC)
    CI.addDiagnosticConsumer(DiagC);

  if (FileSystem != llvm::vfs::getRealFileSystem())
    CI.getSourceMgr().setFileSystem(FileSystem);

  Invocation.setCodeCompletionPoint(completionBuffer, Offset);

  if (CI.setup(Invocation)) {
    Error = "failed to setup compiler instance";
    return false;
  }
  registerIDERequestFunctions(CI.getASTContext().evaluator);

  CI.performParseAndResolveImportsOnly();
  Callback(CI);

  if (DiagC)
    CI.removeDiagnosticConsumer(DiagC);

  if (ArgsHash.hasValue()) {
    CachedCI = std::move(TheInstance);
    CachedArgHash = *ArgsHash;
    CachedReuseCount = 0;
  }

  return true;
}

bool swift::ide::CompletionInstance::performOperation(
    swift::CompilerInvocation &Invocation, llvm::ArrayRef<const char *> Args,
    llvm::IntrusiveRefCntPtr<llvm::vfs::FileSystem> FileSystem,
    llvm::MemoryBuffer *completionBuffer, unsigned int Offset,
    bool EnableASTCaching, std::string &Error, DiagnosticConsumer *DiagC,
    llvm::function_ref<void(CompilerInstance &)> Callback) {

  // Always disable source location resolutions from .swiftsourceinfo file
  // because they're somewhat heavy operations and aren't needed for completion.
  Invocation.getFrontendOptions().IgnoreSwiftSourceInfo = true;

  // Disable to build syntax tree because code-completion skips some portion of
  // source text. That breaks an invariant of syntax tree building.
  Invocation.getLangOptions().BuildSyntaxTree = false;

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

    if (performCachedOperaitonIfPossible(Invocation, ArgsHash, completionBuffer,
                                         Offset, DiagC, Callback))
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
