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

/// \brief A decomposed representation of a source location, useful for
/// printing diagnostics.
struct DecomposedLoc {
  const llvm::MemoryBuffer *Buffer;
  unsigned Line;
  unsigned Column;
};

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

  /// Returns true if \c LHS is before \c RHS in the source buffer.
  bool isBeforeInBuffer(SourceLoc LHS, SourceLoc RHS) const {
    return LHS.Value.getPointer() < RHS.Value.getPointer();
  }

  /// Returns true if range \c R contains the location \c Loc.  The location
  /// \c Loc should point at the beginning of the token.
  bool rangeContainsTokenLoc(SourceRange R, SourceLoc Loc) const {
    return Loc == R.Start || Loc == R.End ||
           (isBeforeInBuffer(R.Start, Loc) && isBeforeInBuffer(Loc, R.End));
  }

  /// Returns true if range \c Enclosing contains the range \c Inner.
  bool rangeContains(SourceRange Enclosing, SourceRange Inner) const {
    return rangeContainsTokenLoc(Enclosing, Inner.Start) &&
           rangeContainsTokenLoc(Enclosing, Inner.End);
  }

  int findBufferContainingLoc(SourceLoc Loc) const {
    return LLVMSourceMgr.FindBufferContainingLoc(Loc.Value);
  }

  size_t addNewSourceBuffer(llvm::MemoryBuffer *Buffer, SourceLoc IncludeLoc) {
    return LLVMSourceMgr.AddNewSourceBuffer(Buffer, IncludeLoc.Value);
  }

  /// \brief Returns the SourceLoc for the beginning of the specified buffer
  /// (at offset zero).
  ///
  /// Note that the resulting location might not point at the first token: it
  /// might point at whitespace or comment.
  SourceLoc getLocForBufferStart(unsigned BufferID) const;

  /// \brief Returns the offset in bytes for the given source location.
  unsigned getLocOffsetInBuffer(SourceLoc Loc, unsigned BufferID) const;

  DecomposedLoc decompose(SourceLoc Loc) const;

  std::pair<unsigned, unsigned> getLineAndColumn(SourceLoc Loc,
                                                 int BufferID = -1) const {
    return LLVMSourceMgr.getLineAndColumn(Loc.Value, BufferID);
  }
};

} // namespace swift

#endif // LLVM_SWIFT_SOURCEMANAGER_H

