//===----------- FileManager.h - Status-caching file manager ----*- C++ -*-===//
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

#ifndef SWIFT_BASIC_FILEMANAGER_H
#define SWIFT_BASIC_FILEMANAGER_H

#include "swift/Basic/LLVM.h"
#include "clang/Basic/FileManager.h"
#include "llvm/Support/Errc.h"
#include "llvm/Support/VirtualFileSystem.h"

namespace llvm {
namespace vfs {
class FileSystem;
}
}

namespace clang {
class FileManager;
}

namespace swift {

/// A file manager that caches file stats, both to avoid re-statting files, but
/// also to ensure we don't accidentally record newer stats than when we first
/// read the file.
class FileManager: public llvm::RefCountedBase<FileManager> {
private:
  friend class SourceManager;

  clang::FileSystemOptions FileSystemOpts;
  llvm::IntrusiveRefCntPtr<clang::FileManager> clangManager;
  StringRef getCachedFilename(StringRef path);
public:
  FileManager(llvm::IntrusiveRefCntPtr<llvm::vfs::FileSystem> fs =
              llvm::vfs::getRealFileSystem()) {}

  llvm::vfs::FileSystem &getFileSystem() {
    return *clangManager->getVirtualFileSystem();
  }

  /// Sets the file system which will be used for file operations.
  /// Setting this will clear the stat cache.
  void setFileSystem(llvm::IntrusiveRefCntPtr<llvm::vfs::FileSystem> fs) {
    clangManager = new clang::FileManager(FileSystemOpts, fs);
  }

  /// Gets and caches the filesystem status for the provided path.
  llvm::ErrorOr<llvm::vfs::Status>
  status(StringRef path, bool isDirectory = false);

  /// Opens the file at the provided path and returns a buffer of that file's
  /// contents. The file's status will be cached just before reading the file.
  llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>>
  getBufferForFile(StringRef path, bool isVolatile = false);

  /// Opens the file at the provided path (or reads from stdin, if the provided
  /// path is '-') and returns a buffer of that file's contents. The file's
  /// status will be cached just before reading the file.
  llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>>
  getBufferForFileOrSTDIN(StringRef path, bool isVolatile = false);

  /// Removes the file at the provided path and removes the cached status value.
  std::error_code remove(StringRef path);

  /// Determines if a file exists on disk at the provided path.
  bool exists(StringRef path);
};

} // end namespace swift

#endif // !defined(SWIFT_BASIC_FILEMANAGER_H)
