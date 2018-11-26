//===--- FileSystem.h - Extra helpers for manipulating files ----*- C++ -*-===//
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

#ifndef SWIFT_BASIC_FILESYSTEM_H
#define SWIFT_BASIC_FILESYSTEM_H

#include "llvm/ADT/Twine.h"
#include "llvm/Support/MemoryBuffer.h"
#include <system_error>

namespace clang {
  namespace vfs {
    class FileSystem;
  }
}

namespace swift {
  /// Moves a file from \p source to \p destination, unless there is already
  /// a file at \p destination that contains the same data as \p source.
  ///
  /// In the latter case, the file at \p source is deleted. If an error occurs,
  /// the file at \p source will still be present at \p source.
  std::error_code moveFileIfDifferent(const llvm::Twine &source,
                                      const llvm::Twine &destination);

  namespace vfs {
    llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>>
    getFileOrSTDIN(clang::vfs::FileSystem &FS,
                   const llvm::Twine &Name, int64_t FileSize = -1,
                   bool RequiresNullTerminator = true, bool IsVolatile = false);
  } // end namespace vfs
} // end namespace swift

#endif // SWIFT_BASIC_FILESYSTEM_H
