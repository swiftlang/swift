//===--- SymbolicLinks.h ----------------------------------------*- C++ -*-===//
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

#ifndef SWIFT_BASIC_SYMBOLICLINKS_H
#define SWIFT_BASIC_SYMBOLICLINKS_H

#include "swift/Basic/LLVM.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/Support/VirtualFileSystem.h"

namespace swift {

/// Tries to resolve symbolic links in a given path, but preserves
/// substitute drives on Windows to avoid MAX_PATH issues.
/// On failure, returns the original path.
llvm::SmallString<128> resolveSymbolicLinks(llvm::StringRef InputPath);

/// Tries to resolve symbolic links in a given path, but preserves
/// substitute drives on Windows to avoid MAX_PATH issues.
/// On failure, returns the original path.
llvm::SmallString<128> resolveSymbolicLinks(llvm::StringRef InputPath,
    llvm::vfs::FileSystem *FileSystem);

} // namespace swift

#endif // SWIFT_BASIC_BLOTSETVECTOR_H
