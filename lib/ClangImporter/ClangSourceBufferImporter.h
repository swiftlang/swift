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

/// A helper class used to keep alive the Clang source managers where
/// diagnostics have been reported.
///
/// This is a bit of a hack, but LLVM's source manager (and by extension
/// Swift's) does not support buffers going away, so if we want to report
/// diagnostics in them we have to do it this way.
class ClangSourceBufferImporter {
  // This is not using SmallPtrSet or similar because we need the
  // IntrusiveRefCntPtr to stay a ref-counting pointer.
  SmallVector<llvm::IntrusiveRefCntPtr<const clang::SourceManager>, 4>
    sourceManagersWithDiagnostics;
  llvm::DenseMap<const llvm::MemoryBuffer *, unsigned> mirroredBuffers;
  SourceManager &swiftSourceManager;

public:
  explicit ClangSourceBufferImporter(SourceManager &sourceMgr)
    : swiftSourceManager(sourceMgr) {}

  /// Returns a Swift source location that points into a Clang buffer.
  ///
  /// This will keep the Clang buffer alive as long as this object.
  SourceLoc resolveSourceLocation(const clang::SourceManager &clangSrcMgr,
                                  clang::SourceLocation clangLoc);
};

} // end namespace importer
} // end namespace swift

#endif
