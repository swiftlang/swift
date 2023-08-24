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
#include "llvm/Support/Path.h"

using namespace llvm;

SmallString<128> swift::resolveSymbolicLinks(StringRef FilePath) {
  SmallString<128> RealPathBuf;
  if (llvm::sys::fs::real_path(FilePath, RealPathBuf)) {
    return FilePath;
  }

  if (!is_style_windows(llvm::sys::path::Style::native)) {
    return RealPathBuf;
  }

  // For Windows paths, only use the real path if it doesn't resolve
  // a substitute drive, as those are used to avoid MAX_PATH issues.
  SmallString<128> AbsPathBuf = FilePath;
  if (llvm::sys::fs::make_absolute(AbsPathBuf)) {
    return FilePath;
  }

  if (llvm::sys::path::root_name(RealPathBuf) ==
      llvm::sys::path::root_name(AbsPathBuf)) {
    return RealPathBuf;
  }

  // Fallback to using the absolute path.
  // Simplifying /../ is semantically valid on Windows even in the
  // presence of symbolic links.
  llvm::sys::path::remove_dots(AbsPathBuf, /*remove_dot_dot=*/ true);
  return AbsPathBuf;
}

// Resolves symbolic links in a given file path, stopping short
// of resolving across substitute drives on Windows since
// those are used to avoid running into MAX_PATH issues.
llvm::SmallString<128> swift::resolveSymbolicLinks(
  StringRef InputPath,
  llvm::vfs::FileSystem *FileSystem) {

  llvm::SmallString<128> RealPathBuf;
  if (FileSystem->getRealPath(InputPath, RealPathBuf)) {
    return InputPath;
  }

  if (!is_style_windows(llvm::sys::path::Style::native)) {
    return RealPathBuf;
  }

  // For Windows paths, make sure we didn't resolve across drives.
  SmallString<128> AbsPathBuf = InputPath;
  if (FileSystem->makeAbsolute(AbsPathBuf)) {
    return InputPath;
  }

  if (llvm::sys::path::root_name(RealPathBuf) ==
      llvm::sys::path::root_name(AbsPathBuf)) {
    return RealPathBuf;
  }

  // Fallback to using the absolute path.
  // Simplifying /../ is semantically valid on Windows even in the
  // presence of symbolic links.
  llvm::sys::path::remove_dots(AbsPathBuf, /*remove_dot_dot=*/ true);
  return AbsPathBuf;
}
