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
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/ADT/StringRef.h"
#include <memory>
#include <string>

namespace llvm {
  class MemoryBuffer;
}

namespace SourceKit {
class FileSystemProvider;
class LangSupport;
class NotificationCenter;

class Context {
  std::string RuntimeLibPath;
  std::unique_ptr<LangSupport> SwiftLang;
  std::shared_ptr<NotificationCenter> NotificationCtr;

  llvm::StringMap<FileSystemProvider *> FileSystemProviders;

public:
  Context(StringRef RuntimeLibPath,
          llvm::function_ref<
              std::unique_ptr<LangSupport>(Context &)> LangSupportFactoryFn,
          bool shouldDispatchNotificationsOnMain = true);
  ~Context();

  StringRef getRuntimeLibPath() const { return RuntimeLibPath; }

  LangSupport &getSwiftLangSupport() { return *SwiftLang; }

  std::shared_ptr<NotificationCenter> getNotificationCenter() { return NotificationCtr; }

  /// Returns the FileSystemProvider registered under Name.
  FileSystemProvider *getFileSystemProvider(StringRef Name);

  /// Registers the given FileSystemProvider under Name. The caller is
  /// responsible for keeping FileSystemProvider alive as long as this Context.
  void setFileSystemProvider(StringRef Name,
                             FileSystemProvider *FileSystemProvider);
};

} // namespace SourceKit

#endif
