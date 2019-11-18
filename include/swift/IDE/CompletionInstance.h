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

/// Copy a memory buffer inserting '0' at the position of \c origBuf.
std::unique_ptr<llvm::MemoryBuffer>
makeCodeCompletionMemoryBuffer(const llvm::MemoryBuffer *origBuf,
                               unsigned &Offset,
                               llvm::StringRef bufferIdentifier);

class CompletionInstance {
  std::unique_ptr<CompilerInstance> CachedCI = nullptr;

  swift::CompilerInstance *
  getReusingCompilerInstance(const swift::CompilerInvocation &Invocation,
                             llvm::MemoryBuffer *completionBuffer,
                             unsigned int Offset, DiagnosticConsumer *DiagC);

  swift::CompilerInstance *renewCompilerInstance(
      swift::CompilerInvocation &Invocation,
      llvm::IntrusiveRefCntPtr<llvm::vfs::FileSystem> FileSystem,
      llvm::MemoryBuffer *completionBuffer, unsigned int Offset,
      std::string &Error, DiagnosticConsumer *DiagC);

public:
  swift::CompilerInstance *getCompilerInstance(
      swift::CompilerInvocation &Invocation,
      llvm::IntrusiveRefCntPtr<llvm::vfs::FileSystem> FileSystem,
      llvm::MemoryBuffer *completionBuffer, unsigned int Offset,
      std::string &Error, DiagnosticConsumer *DiagC);
};

} // namespace ide
} // namespace swift

#endif // SWIFT_IDE_COMPLETIONINSTANCE_H
