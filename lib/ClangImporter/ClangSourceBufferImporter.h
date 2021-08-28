//===--- ClangSourceBufferImporter.h - Map Clang buffers over ---*- C++ -*-===//
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

#ifndef SWIFT_CLANGIMPORTER_CLANGSOURCEBUFFERIMPORTER_H
#define SWIFT_CLANGIMPORTER_CLANGSOURCEBUFFERIMPORTER_H

#include "swift/Basic/LLVM.h"
#include "swift/Basic/SourceLoc.h"
#include "clang/Basic/SourceLocation.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/IntrusiveRefCntPtr.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SmallVector.h"

namespace llvm {
class MemoryBuffer;
}

namespace clang {
class SourceManager;
}

namespace swift {
class SourceManager;

namespace importer {

/// A helper class used to import clang::SourceLocation to swift::SourceLoc,
/// for eg. attaching to declarations or for diagnostics.
///
/// Rather than copying the underlying MemoryBuffer, instead keep any
/// clang::SourceManager with imported locations alive and reference its
/// underlying buffer directly.
class ClangSourceBufferImporter {
  struct MirrorEntry {
    unsigned FileID;
    bool Complete;
  };

  llvm::SmallPtrSet<const clang::SourceManager *, 4> clangSourceManagers;
  SmallVector<llvm::IntrusiveRefCntPtr<const clang::SourceManager>, 4>
    clangSourceManagerRefs;
  llvm::DenseMap<const char *, MirrorEntry> mirroredBuffers;
  SourceManager &swiftSourceManager;

public:
  explicit ClangSourceBufferImporter(SourceManager &sourceMgr)
      : swiftSourceManager(sourceMgr) {}

  /// Returns a Swift source location that points into a Clang buffer.
  ///
  /// This will keep the Clang buffer alive as long as this object.
  SourceLoc importSourceLoc(
      const clang::SourceManager &clangSourceManager,
      clang::SourceLocation clangLoc, bool forDiagnostics=false);
};

} // end namespace importer
} // end namespace swift

#endif
