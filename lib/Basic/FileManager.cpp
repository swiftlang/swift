//===---------- FileManager.cpp - Status-caching file manager ---*- C++ -*-===//
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


#include "swift/Basic/FileManager.h"
#include "llvm/Support/xxhash.h"

using namespace swift;

llvm::ErrorOr<llvm::vfs::Status>
FileManager::status(StringRef path) {
  llvm::vfs::Status status;
  bool result = clang::FileSystemStatCache::get(path, status, /*isFile*/true,
                                                /*File*/nullptr, &cachedStats,
                                                *fileSystem);
  if (result)
    return std::make_error_code(std::errc::no_such_file_or_directory);
  return status;
}

StringRef FileManager::getCachedFilename(StringRef path) {
  llvm::vfs::Status stats;
  auto stat = status(path);
  if (!stat)
    return path;
  return cachedStats.StatCalls[path].getName();
}

llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>>
FileManager::getBufferForFile(StringRef path, int64_t fileSize,
                              bool requiresNullTerminator,
                              bool isVolatile) {

  llvm::vfs::Status status;
  std::unique_ptr<llvm::vfs::File> file;
  bool pathDoesNotExist =
    clang::FileSystemStatCache::get(path, status, /*isFile*/true,
                                    &file, &cachedStats,
                                    *fileSystem);
  if (pathDoesNotExist)
    return std::make_error_code(std::errc::no_such_file_or_directory);
  auto buf = file->getBuffer(path, fileSize, requiresNullTerminator,
                             isVolatile);
  if (!buf) return buf.getError();
  return buf;
}

llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>>
FileManager::getBufferForFileOrSTDIN(StringRef path, int64_t fileSize,
                              bool requiresNullTerminator,
                              bool isVolatile) {
  if (path == "-")
    return llvm::MemoryBuffer::getSTDIN();
  return getBufferForFile(path, fileSize, requiresNullTerminator,
                          isVolatile);
}

std::error_code FileManager::remove(StringRef path) {
  auto err = llvm::sys::fs::remove(path);
  if (err) return err;
  cachedStats.StatCalls.erase(path);
  return {};
}

bool FileManager::exists(StringRef path) {
  // Don't just call into FileSystem::exists, because that'll waste a stat.
  auto stat = status(path);
  return stat && stat->exists();
}
