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
#include "llvm/ADT/Optional.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/VirtualFileSystem.h"
#include <string>
#include <optional>

namespace swift {

/// Tries to resolve symbolic links in a given path, but preserves
/// substitute drives on Windows to avoid MAX_PATH issues.
/// \param InputPath The path to be resolved.
/// \param FileSystem The FileSystem for resolving the path.
/// \param Style An optional path style to honor in the return value.
/// \returns The resolved path, or the original path on failure. 
std::string resolveSymbolicLinks(llvm::StringRef InputPath,
    llvm::vfs::FileSystem &FileSystem,
    llvm::Optional<llvm::sys::path::Style> Style = llvm::None);

} // namespace swift

#endif // SWIFT_BASIC_BLOTSETVECTOR_H
