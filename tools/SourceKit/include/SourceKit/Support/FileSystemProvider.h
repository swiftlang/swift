//===--- FileSystemProvider.h - ---------------------------------*- C++ -*-===//
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

#ifndef LLVM_SOURCEKIT_SUPPORT_FILESYSTEMPROVIDER_H
#define LLVM_SOURCEKIT_SUPPORT_FILESYSTEMPROVIDER_H

#include "llvm/ADT/IntrusiveRefCntPtr.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/Support/VirtualFileSystem.h"

namespace SourceKit {

/// Allows clients of SourceKit to specify custom llvm::vfs::FileSystems to be
/// used while serving a request.
///
/// Requests to SourceKit select FileSystemProviders by specifying
/// 'key.vfs.name', and pass arguments to the FileSystemProviders by
/// specifying 'key.vfs.args'. SourceKit then passes the given arguments to the
/// selected FileSystemProvider, and uses the resulting llvm::vfs::FileSystem
/// while serving the request.
///
/// The following requests currently support custom FileSystemProviders (other
/// requests respond with an invalid request error if you try):
/// - source.request.editor.open:
///     Associates the given custom filesystem with this editor file, so that
///     all subsequent requests on the file use it, unless the subsequent
///     request specifies its own filesystem.
/// - source.request.cursorinfo:
///     Uses the given custom filesystem. If none is specified, uses the
///     filesystem associated with the file.
/// - source.request.codecomplete:
///     Uses the given custom filesystem.
/// - source.request.codecomplete.open:
///     Associates the given custom filesystem with this completion session, so
///     that all subsequent requests in the session use it.
/// - source.request.codecomplete.update:
///     Uses the filesystem associated with the completion session.
class FileSystemProvider {
public:
  virtual ~FileSystemProvider() = default;

  /// Returns a llvm::vfs::FileSystem to be used while serving a request, or
  /// nullptr on failure.
  /// \param options arguments passed into the request under 'key.vfs.options'.
  /// \param [out] error filled with an error message on failure.
  virtual llvm::IntrusiveRefCntPtr<llvm::vfs::FileSystem>
  getFileSystem(OptionsDictionary &options, std::string &error) = 0;
};

} // namespace SourceKit

#endif
