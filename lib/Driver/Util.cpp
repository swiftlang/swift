//===--- Util.cpp - Common Driver Utilities -------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/Driver/Util.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Path.h"

using namespace swift;

std::string driver::findRelativeExecutable(StringRef compilerPath,
                                           StringRef executableName) {
  llvm::SmallString<128> path{compilerPath};
  llvm::sys::path::remove_filename(path); // swift

  // First try "bin/<executable>" (i.e. next to Swift).
  llvm::sys::path::append(path, executableName);
  if (llvm::sys::fs::exists(path.str()))
    return path.str().str();

  // Then see if we're in an Xcode toolchain.
  llvm::sys::path::remove_filename(path); // <executable>
  llvm::sys::path::remove_filename(path); // bin
  llvm::sys::path::remove_filename(path); // usr
  if (llvm::sys::path::extension(path) == ".xctoolchain") {
    llvm::sys::path::remove_filename(path); // *.xctoolchain
    llvm::sys::path::remove_filename(path); // Toolchains
    llvm::sys::path::append(path, "usr", "bin", executableName);
    if (llvm::sys::fs::exists(path.str()))
      return path.str().str();
  }

  return {};
}
