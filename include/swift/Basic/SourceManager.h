//===--- SourceManager.h - Manager for Source Buffers -----------*- C++ -*-===//
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

#ifndef SWIFT_SOURCEMANAGER_H
#define SWIFT_SOURCEMANAGER_H

#include "swift/Basic/SourceLoc.h"
#include "llvm/Support/SourceMgr.h"

namespace swift {

/// \brief This class manages and owns source buffers.
class SourceManager {
  llvm::SourceMgr LLVMSourceMgr;

  unsigned CodeCompletionBufferID = ~0U;
  unsigned CodeCompletionOffset;

  /// \brief The buffer ID where a hashbang line #! is allowed.
  unsigned HashbangBufferID = ~0U;

public:
  SourceManager() {}

  llvm::SourceMgr *operator->() { return &LLVMSourceMgr; }
  const llvm::SourceMgr *operator->() const { return &LLVMSourceMgr; }

  const llvm::SourceMgr &getLLVMSourceMgr() const {
    return LLVMSourceMgr;
  }

  void setCodeCompletionPoint(unsigned BufferID, unsigned Offset) {
    assert(BufferID != ~0U && "Buffer should be valid");
    assert(CodeCompletionBufferID == ~0U &&
           "Code completion point already set");

    CodeCompletionBufferID = BufferID;
    CodeCompletionOffset = Offset;
  }

  unsigned getCodeCompletionBufferID() const {
    return CodeCompletionBufferID;
  }

  unsigned getCodeCompletionOffset() const {
    return CodeCompletionOffset;
  }

  SourceLoc getCodeCompletionLoc() const;

  void setHashbangBufferID(unsigned BufferID) {
    assert(HashbangBufferID == ~0U && "Hashbang buffer ID already set");
    HashbangBufferID = BufferID;
  }

  unsigned getHashbangBufferID() const {
    return HashbangBufferID;
  }
};

} // namespace swift

#endif // LLVM_SWIFT_SOURCEMANAGER_H

