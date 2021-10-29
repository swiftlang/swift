//===--- Context.h - --------------------------------------------*- C++ -*-===//
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

#ifndef LLVM_SOURCEKIT_CORE_CONTEXT_H
#define LLVM_SOURCEKIT_CORE_CONTEXT_H

#include "SourceKit/Core/LLVM.h"
#include "SourceKit/Support/CancellationToken.h"
#include "SourceKit/Support/Concurrency.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/Mutex.h"
#include <map>
#include <memory>
#include <string>

namespace llvm {
  class MemoryBuffer;
}

namespace SourceKit {
  class LangSupport;
  class NotificationCenter;

class GlobalConfig {
public:
  struct Settings {
    struct CompletionOptions {

      /// Max count of reusing ASTContext for cached code completion.
      unsigned MaxASTContextReuseCount = 100;

      /// Interval second for checking dependencies in cached code completion.
      unsigned CheckDependencyInterval = 5;
    } CompletionOpts;
  };

private:
  Settings State;
  mutable llvm::sys::Mutex Mtx;

public:
  Settings update(Optional<unsigned> CompletionMaxASTContextReuseCount,
                  Optional<unsigned> CompletionCheckDependencyInterval);
  Settings::CompletionOptions getCompletionOpts() const;
};

class SlowRequestSimulator {
  std::map<SourceKitCancellationToken, std::shared_ptr<Semaphore>>
      InProgressRequests;

  /// Mutex guarding \c InProgressRequests.
  llvm::sys::Mutex InProgressRequestsMutex;

public:
  /// Simulate that a request takes \p DurationMs to execute. While waiting that
  /// duration, the request can be cancelled using the \p CancellationToken.
  /// Returns \c true if the request waited the required duration and \c false
  /// if it was cancelled.
  bool simulateLongRequest(int64_t DurationMs,
                           SourceKitCancellationToken CancellationToken) {
    auto Sema = std::make_shared<Semaphore>(0);
    {
      llvm::sys::ScopedLock L(InProgressRequestsMutex);
      InProgressRequests[CancellationToken] = Sema;
    }
    bool DidTimeOut = Sema->wait(DurationMs);
    {
      llvm::sys::ScopedLock L(InProgressRequestsMutex);
      InProgressRequests[CancellationToken] = nullptr;
    }
    // If we timed out, we waited the required duration. If we didn't time out,
    // the semaphore was cancelled.
    return DidTimeOut;
  }

  /// Cancel a simulated long request. If the required wait duration already
  /// elapsed, this is a no-op.
  void cancel(SourceKitCancellationToken CancellationToken) {
    llvm::sys::ScopedLock L(InProgressRequestsMutex);
    if (auto InProgressSema = InProgressRequests[CancellationToken]) {
      InProgressSema->signal();
    }
  }
};

class Context {
  std::string RuntimeLibPath;
  std::string DiagnosticDocumentationPath;
  std::unique_ptr<LangSupport> SwiftLang;
  std::shared_ptr<NotificationCenter> NotificationCtr;
  std::shared_ptr<GlobalConfig> Config;
  std::shared_ptr<SlowRequestSimulator> SlowRequestSim;

public:
  Context(StringRef RuntimeLibPath, StringRef DiagnosticDocumentationPath,
          llvm::function_ref<std::unique_ptr<LangSupport>(Context &)>
              LangSupportFactoryFn,
          bool shouldDispatchNotificationsOnMain = true);
  ~Context();

  StringRef getRuntimeLibPath() const { return RuntimeLibPath; }
  StringRef getDiagnosticDocumentationPath() const {
    return DiagnosticDocumentationPath;
  }

  LangSupport &getSwiftLangSupport() { return *SwiftLang; }

  std::shared_ptr<NotificationCenter> getNotificationCenter() { return NotificationCtr; }

  std::shared_ptr<GlobalConfig> getGlobalConfiguration() { return Config; }

  std::shared_ptr<SlowRequestSimulator> getSlowRequestSimulator() {
    return SlowRequestSim;
  }
};

} // namespace SourceKit

#endif
