//===--- SymbolicLinks.cpp - Utility functions for resolving symlinks -----===//
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

#include "swift/Basic/SymbolicLinks.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Support/Path.h"

using namespace llvm;

static std::error_code resolveSymbolicLinks(
    StringRef InputPath,
    llvm::vfs::FileSystem &FileSystem,
    SmallVectorImpl<char> &Result) {

  if (auto ErrorCode = FileSystem.getRealPath(InputPath, Result)) {
    return ErrorCode;
  }

  if (!is_style_windows(llvm::sys::path::Style::native)) {
    return std::error_code();
  }

  // For Windows paths, make sure we didn't resolve across drives.
  SmallString<128> AbsPathBuf = InputPath;
  if (auto ErrorCode = FileSystem.makeAbsolute(AbsPathBuf)) {
    // We can't guarantee that the real path preserves the drive
    return ErrorCode;
  }

  if (llvm::sys::path::root_name(StringRef(Result.data(), Result.size())) ==
      llvm::sys::path::root_name(AbsPathBuf)) {
    // Success, the real path preserves the drive
    return std::error_code();
  }

  // Fallback to using the absolute path.
  // Simplifying /../ is semantically valid on Windows even in the
  // presence of symbolic links.
  llvm::sys::path::remove_dots(AbsPathBuf, /*remove_dot_dot=*/ true);
  Result.assign(AbsPathBuf);
  return std::error_code();
}

std::string swift::resolveSymbolicLinks(
  StringRef InputPath,
  llvm::vfs::FileSystem &FileSystem,
  llvm::Optional<llvm::sys::path::Style> Style) {

  llvm::SmallString<128> OutputPathBuf;
  if (::resolveSymbolicLinks(InputPath, FileSystem, OutputPathBuf)) {
    // Error, fallback on input path
    OutputPathBuf = InputPath;
  }

  if (Style) {
    llvm::sys::path::native(OutputPathBuf, *Style);
  }

  return std::string(OutputPathBuf);
}
