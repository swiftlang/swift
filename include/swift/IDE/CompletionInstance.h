//===--- CompletionInstance.h ---------------------------------------------===//
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

#ifndef SWIFT_IDE_COMPLETIONINSTANCE_H
#define SWIFT_IDE_COMPLETIONINSTANCE_H

#include "swift/Frontend/Frontend.h"
#include "llvm/ADT/Hashing.h"
#include "llvm/ADT/IntrusiveRefCntPtr.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/VirtualFileSystem.h"

namespace swift {

class CompilerInstance;
class CompilerInvocation;
class DiagnosticConsumer;

namespace ide {

/// Copy a memory buffer inserting '\0' at the position of \c origBuf.
std::unique_ptr<llvm::MemoryBuffer>
makeCodeCompletionMemoryBuffer(const llvm::MemoryBuffer *origBuf,
                               unsigned &Offset,
                               llvm::StringRef bufferIdentifier);

/// Manages \c CompilerInstance for completion like operations.
class CompletionInstance {
  unsigned MaxASTReuseCount = 100;

  std::mutex mtx;

  std::unique_ptr<CompilerInstance> CachedCI;
  llvm::hash_code CachedArgHash;
  unsigned CachedReuseCount = 0;

  /// Calls \p Callback with cached \c CompilerInstance if it's usable for the
  /// specified completion request.
  /// Returns \c if the callback was called. Returns \c false if the compiler
  /// argument has changed, primary file is not the same, the \c Offset is not
  /// in function bodies, or the interface hash of the file has changed.
  bool performCachedOperaitonIfPossible(
      const swift::CompilerInvocation &Invocation, llvm::hash_code ArgsHash,
      llvm::MemoryBuffer *completionBuffer, unsigned int Offset,
      DiagnosticConsumer *DiagC,
      llvm::function_ref<void(CompilerInstance &)> Callback);

  /// Calls \p Callback with new \c CompilerInstance for the completion
  /// request. The \c CompilerInstace passed to the callback already performed
  /// the first pass.
  /// Returns \c false if it fails to setup the \c CompilerInstance.
  bool performNewOperation(
      llvm::Optional<llvm::hash_code> ArgsHash,
      swift::CompilerInvocation &Invocation,
      llvm::IntrusiveRefCntPtr<llvm::vfs::FileSystem> FileSystem,
      llvm::MemoryBuffer *completionBuffer, unsigned int Offset,
      std::string &Error, DiagnosticConsumer *DiagC,
      llvm::function_ref<void(CompilerInstance &)> Callback);

public:
  /// Calls \p Callback with a \c CompilerInstance which is prepared for the
  /// second pass. \p Callback is resposible to perform the second pass on it.
  /// The \c CompilerInstance may be reused from the previous completions,
  /// and may be cached for the next completion.
  /// Return \c true if \p is successfully called, \c it fails. In failure
  /// cases \p Error is populated with an error message.
  ///
  /// NOTE: \p Args is only used for checking the equaity of the invocation.
  /// Since this function assumes that it is already normalized, exact the same
  /// arguments including their order is considered as the same invocation.
  bool performOperation(
      swift::CompilerInvocation &Invocation, llvm::ArrayRef<const char *> Args,
      llvm::IntrusiveRefCntPtr<llvm::vfs::FileSystem> FileSystem,
      llvm::MemoryBuffer *completionBuffer, unsigned int Offset,
      bool EnableASTCaching, std::string &Error, DiagnosticConsumer *DiagC,
      llvm::function_ref<void(CompilerInstance &)> Callback);
};

} // namespace ide
} // namespace swift

#endif // SWIFT_IDE_COMPLETIONINSTANCE_H
