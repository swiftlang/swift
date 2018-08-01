//===--- FileSystem.cpp - Extra helpers for manipulating files ------------===//
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

#include "swift/Basic/FileSystem.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Process.h"
#include "clang/Basic/FileManager.h"

using namespace swift;

namespace {
  class OpenFileRAII {
    static const int INVALID_FD = -1;
  public:
    int fd = INVALID_FD;

    ~OpenFileRAII() {
      if (fd != INVALID_FD)
        llvm::sys::Process::SafelyCloseFileDescriptor(fd);
    }
  };
} // end anonymous namespace

std::error_code swift::moveFileIfDifferent(const llvm::Twine &source,
                                           const llvm::Twine &destination) {
  namespace fs = llvm::sys::fs;

  // First check for a self-move.
  if (fs::equivalent(source, destination))
    return std::error_code();

  OpenFileRAII sourceFile;
  fs::file_status sourceStatus;
  if (std::error_code error = fs::openFileForRead(source, sourceFile.fd)) {
    // If we can't open the source file, fail.
    return error;
  }
  if (std::error_code error = fs::status(sourceFile.fd, sourceStatus)) {
    // If we can't stat the source file, fail.
    return error;
  }

  OpenFileRAII destFile;
  fs::file_status destStatus;
  bool couldReadDest = !fs::openFileForRead(destination, destFile.fd);
  if (couldReadDest)
    couldReadDest = !fs::status(destFile.fd, destStatus);

  // If we could read the destination file, and it matches the source file in
  // size, they may be the same. Do an actual comparison of the contents.
  if (couldReadDest && sourceStatus.getSize() == destStatus.getSize()) {
    uint64_t size = sourceStatus.getSize();
    bool same = false;
    if (size == 0) {
      same = true;
    } else {
      std::error_code sourceRegionErr;
      fs::mapped_file_region sourceRegion(sourceFile.fd,
                                          fs::mapped_file_region::readonly,
                                          size, 0, sourceRegionErr);
      if (sourceRegionErr)
        return sourceRegionErr;

      std::error_code destRegionErr;
      fs::mapped_file_region destRegion(destFile.fd,
                                        fs::mapped_file_region::readonly,
                                        size, 0, destRegionErr);

      if (!destRegionErr) {
        same = (0 == memcmp(sourceRegion.const_data(), destRegion.const_data(),
                            size));
      }
    }

    // If the file contents are the same, we are done. Just delete the source.
    if (same)
      return fs::remove(source);
  }

  // If we get here, we weren't able to prove that the files are the same.
  return fs::rename(source, destination);
}

llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>>
swift::vfs::getFileOrSTDIN(clang::vfs::FileSystem &FS,
                           const llvm::Twine &Filename,
                           int64_t FileSize,
                           bool RequiresNullTerminator,
                           bool IsVolatile) {
  llvm::SmallString<256> NameBuf;
  llvm::StringRef NameRef = Filename.toStringRef(NameBuf);

  if (NameRef == "-")
    return llvm::MemoryBuffer::getSTDIN();
  return FS.getBufferForFile(Filename, FileSize,
                             RequiresNullTerminator, IsVolatile);
}
