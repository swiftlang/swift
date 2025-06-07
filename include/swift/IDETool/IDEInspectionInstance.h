//===--- IDEInspectionInstance.h ------------------------------------------===//
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

#ifndef SWIFT_IDE_IDEINSPECTIONINSTANCE_H
#define SWIFT_IDE_IDEINSPECTIONINSTANCE_H

#include "swift/Frontend/Frontend.h"
#include "swift/IDE/CancellableResult.h"
#include "swift/IDE/CodeCompletionContext.h"
#include "swift/IDE/CodeCompletionResult.h"
#include "swift/IDE/CodeCompletionResultSink.h"
#include "swift/IDE/ConformingMethodList.h"
#include "swift/IDE/CursorInfo.h"
#include "swift/IDE/ImportDepth.h"
#include "swift/IDE/SwiftCompletionInfo.h"
#include "swift/IDE/TypeContextInfo.h"
#include "llvm/ADT/Hashing.h"
#include "llvm/ADT/IntrusiveRefCntPtr.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/Chrono.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/VirtualFileSystem.h"

namespace swift {

class CompilerInstance;
class CompilerInvocation;
class DiagnosticConsumer;
class PluginRegistry;

namespace ide {

/// Copy a memory buffer inserting '\0' at the position of \c origBuf.
std::unique_ptr<llvm::MemoryBuffer>
makeCodeCompletionMemoryBuffer(const llvm::MemoryBuffer *origBuf,
                               unsigned &Offset,
                               llvm::StringRef bufferIdentifier);

/// The result returned via the callback from the perform*Operation methods.
struct IDEInspectionInstanceResult {
  /// The compiler instance that is prepared for the second pass.
  std::shared_ptr<CompilerInstance> CI;
  /// Whether an AST was reused.
  bool DidReuseAST;
  /// Whether the IDEInspectionInstance target could be found in the source
  /// file. If this is \c false, the user will most likely want to return
  /// empty results.
  bool DidFindIDEInspectionTarget;
};

/// The results returned from \c IDEInspectionInstance::codeComplete.
struct CodeCompleteResult {
  CodeCompletionResultSink &ResultSink;
  SwiftCompletionInfo &Info;
  ImportDepth ImportDep;
};

/// The results returned from \c IDEInspectionInstance::typeContextInfo.
struct TypeContextInfoResult {
  /// The actual results. If empty, no results were found.
  ArrayRef<TypeContextInfoItem> Results;
  /// Whether an AST was reused to produce the results.
  bool DidReuseAST;
};

/// The results returned from \c IDEInspectionInstance::conformingMethodList.
struct ConformingMethodListResults {
  /// The actual results. If \c nullptr, no results were found.
  const ConformingMethodListResult *Result;
  /// Whether an AST was reused to produce the results.
  bool DidReuseAST;
};

/// The results returned from \c IDEInspectionInstance::cursorInfo.
struct CursorInfoResults {
  /// The actual results.
  std::vector<ResolvedCursorInfoPtr> ResolvedCursorInfos;
  /// Whether an AST was reused to produce the results.
  bool DidReuseAST;
};

/// Manages \c CompilerInstance for completion like operations.
class IDEInspectionInstance {
  struct Options {
    unsigned MaxASTReuseCount = 100;
    unsigned DependencyCheckIntervalSecond = 5;
  } Opts;

  std::mutex mtx;

  std::shared_ptr<PluginRegistry> Plugins;

  std::shared_ptr<CompilerInstance> CachedCI;
  llvm::hash_code CachedArgHash;
  llvm::sys::TimePoint<> DependencyCheckedTimestamp;
  llvm::StringMap<llvm::hash_code> InMemoryDependencyHash;
  unsigned CachedReuseCount = 0;
  std::atomic<bool> CachedCIShouldBeInvalidated;

  void cacheCompilerInstance(std::shared_ptr<CompilerInstance> CI,
                             llvm::hash_code ArgsHash);

  bool shouldCheckDependencies() const;

  /// Calls \p Callback with cached \c CompilerInstance if it's usable for the
  /// specified completion request.
  /// Returns \c true if performing the cached operation was possible. Returns
  /// \c false if the compiler argument has changed, primary file is not the
  /// same, the \c Offset is not in function bodies, or the interface hash of
  /// the file has changed.
  /// \p Callback will be called if and only if this function returns \c true.
  bool performCachedOperationIfPossible(
      llvm::hash_code ArgsHash,
      llvm::IntrusiveRefCntPtr<llvm::vfs::FileSystem> FileSystem,
      const SearchPathOptions &SearchPathOpts,
      llvm::MemoryBuffer *ideInspectionTargetBuffer, unsigned int Offset,
      DiagnosticConsumer *DiagC,
      std::shared_ptr<std::atomic<bool>> CancellationFlag,
      llvm::function_ref<void(CancellableResult<IDEInspectionInstanceResult>)>
          Callback);

  /// Calls \p Callback with new \c CompilerInstance for the completion
  /// request. The \c CompilerInstace passed to the callback already performed
  /// the first pass.
  /// Returns \c false if it fails to setup the \c CompilerInstance.
  void performNewOperation(
      std::optional<llvm::hash_code> ArgsHash,
      swift::CompilerInvocation &Invocation,
      llvm::IntrusiveRefCntPtr<llvm::vfs::FileSystem> FileSystem,
      llvm::MemoryBuffer *ideInspectionTargetBuffer, unsigned int Offset,
      DiagnosticConsumer *DiagC,
      std::shared_ptr<std::atomic<bool>> CancellationFlag,
      llvm::function_ref<void(CancellableResult<IDEInspectionInstanceResult>)>
          Callback);

  /// Calls \p Callback with a \c CompilerInstance which is prepared for the
  /// second pass. \p Callback is resposible to perform the second pass on it.
  /// The \c CompilerInstance may be reused from the previous completions,
  /// and may be cached for the next completion.
  /// In case of failure or cancellation, the callback receives the
  /// corresponding failed or cancelled result.
  ///
  /// If \p CancellationFlag is not \c nullptr, code completion can be cancelled
  /// by setting the flag to \c true.
  /// IMPORTANT: If \p CancellationFlag is not \c nullptr, then completion might
  /// be cancelled in the secon pass that's invoked inside \p Callback.
  /// Therefore, \p Callback MUST check whether completion was cancelled before
  /// interpreting the results, since invalid results may be returned in case
  /// of cancellation.
  ///
  /// NOTE: \p Args is only used for checking the equaity of the invocation.
  /// Since this function assumes that it is already normalized, exact the same
  /// arguments including their order is considered as the same invocation.
  void performOperation(
      swift::CompilerInvocation &Invocation, llvm::ArrayRef<const char *> Args,
      llvm::IntrusiveRefCntPtr<llvm::vfs::FileSystem> FileSystem,
      llvm::MemoryBuffer *ideInspectionTargetBuffer, unsigned int Offset,
      DiagnosticConsumer *DiagC,
      std::shared_ptr<std::atomic<bool>> CancellationFlag,
      llvm::function_ref<void(CancellableResult<IDEInspectionInstanceResult>)>
          Callback);

public:
  IDEInspectionInstance(std::shared_ptr<PluginRegistry> Plugins = nullptr)
      : Plugins(Plugins), CachedCIShouldBeInvalidated(false) {}

  // Mark the cached compiler instance "should be invalidated". In the next
  // completion, new compiler instance will be used. (Thread safe.)
  void markCachedCompilerInstanceShouldBeInvalidated();

  // Update options with \c NewOpts. (Thread safe.)
  void setOptions(Options NewOpts);

  void codeComplete(
      swift::CompilerInvocation &Invocation, llvm::ArrayRef<const char *> Args,
      llvm::IntrusiveRefCntPtr<llvm::vfs::FileSystem> FileSystem,
      llvm::MemoryBuffer *ideInspectionTargetBuffer, unsigned int Offset,
      DiagnosticConsumer *DiagC, ide::CodeCompletionContext &CompletionContext,
      std::shared_ptr<std::atomic<bool>> CancellationFlag,
      llvm::function_ref<void(CancellableResult<CodeCompleteResult>)> Callback);

  void typeContextInfo(
      swift::CompilerInvocation &Invocation, llvm::ArrayRef<const char *> Args,
      llvm::IntrusiveRefCntPtr<llvm::vfs::FileSystem> FileSystem,
      llvm::MemoryBuffer *ideInspectionTargetBuffer, unsigned int Offset,
      DiagnosticConsumer *DiagC,
      std::shared_ptr<std::atomic<bool>> CancellationFlag,
      llvm::function_ref<void(CancellableResult<TypeContextInfoResult>)>
          Callback);

  void conformingMethodList(
      swift::CompilerInvocation &Invocation, llvm::ArrayRef<const char *> Args,
      llvm::IntrusiveRefCntPtr<llvm::vfs::FileSystem> FileSystem,
      llvm::MemoryBuffer *ideInspectionTargetBuffer, unsigned int Offset,
      DiagnosticConsumer *DiagC, ArrayRef<const char *> ExpectedTypeNames,
      std::shared_ptr<std::atomic<bool>> CancellationFlag,
      llvm::function_ref<void(CancellableResult<ConformingMethodListResults>)>
          Callback);

  void cursorInfo(
      swift::CompilerInvocation &Invocation, llvm::ArrayRef<const char *> Args,
      llvm::IntrusiveRefCntPtr<llvm::vfs::FileSystem> FileSystem,
      llvm::MemoryBuffer *ideInspectionTargetBuffer, unsigned int Offset,
      DiagnosticConsumer *DiagC,
      std::shared_ptr<std::atomic<bool>> CancellationFlag,
      llvm::function_ref<void(CancellableResult<CursorInfoResults>)> Callback);
};

} // namespace ide
} // namespace swift

#endif // SWIFT_IDE_IDEINSPECTIONINSTANCE_H
