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

// Resolves symbolic links in a given file path, stopping short
// of resolving across substitute drives on Windows since
// those are used to avoid running into MAX_PATH issues.
std::string swift::resolveSymbolicLinks(
  StringRef InputPath,
  llvm::vfs::FileSystem *FileSystem) {

  llvm::SmallString<128> RealPathBuf;
  if (FileSystem->getRealPath(InputPath, RealPathBuf)) {
    return InputPath.str();
  }

  if (!is_style_windows(llvm::sys::path::Style::native)) {
    return RealPathBuf.str().str();
  }

  // For Windows paths, make sure we didn't resolve across drives.
  SmallString<128> AbsPathBuf = InputPath;
  if (FileSystem->makeAbsolute(AbsPathBuf)) {
    return InputPath.str();
  }

  if (llvm::sys::path::root_name(RealPathBuf) ==
      llvm::sys::path::root_name(AbsPathBuf)) {
    return RealPathBuf.str().str();
  }

  // Fallback to using the absolute path.
  // Simplifying /../ is semantically valid on Windows even in the
  // presence of symbolic links.
  llvm::sys::path::remove_dots(AbsPathBuf, /*remove_dot_dot=*/ true);
  return AbsPathBuf.str().str();
}
