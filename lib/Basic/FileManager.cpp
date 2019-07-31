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
FileManager::status(StringRef path, bool isDirectory) {
  return clangManager->getStatus(path, isDirectory);
}

StringRef FileManager::getCachedFilename(StringRef path) {
  auto file = clangManager->getFile(path);
  if (!file) return path;
  return (*file)->getName();
}

llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>>
FileManager::getBufferForFile(StringRef path, bool isVolatile) {
  return clangManager->getBufferForFile(path, isVolatile);
}

llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>>
FileManager::getBufferForFileOrSTDIN(StringRef path, bool isVolatile) {
  if (path == "-")
    return llvm::MemoryBuffer::getSTDIN();
  return getBufferForFile(path, isVolatile);
}

std::error_code FileManager::remove(StringRef path) {
  auto file = clangManager->getFile(path);
  if (!file) return file.getError();
  clangManager->invalidateCache(*file);
  return llvm::sys::fs::remove(path);
}

bool FileManager::exists(StringRef path) {
  // Don't just call into FileSystem::exists, because that'll waste a stat.
  auto stat = status(path);
  return stat && stat->exists();
}
