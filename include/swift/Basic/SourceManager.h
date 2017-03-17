//===--- SourceManager.h - Manager for Source Buffers -----------*- C++ -*-===//
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

#ifndef SWIFT_BASIC_SOURCEMANAGER_H
#define SWIFT_BASIC_SOURCEMANAGER_H

#include "swift/Basic/SourceLoc.h"
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/Support/SourceMgr.h"
#include <map>

namespace swift {

/// \brief This class manages and owns source buffers.
class SourceManager {
  llvm::SourceMgr LLVMSourceMgr;
  unsigned CodeCompletionBufferID = 0U;
  unsigned CodeCompletionOffset;

  /// \brief The buffer ID where a hashbang line #! is allowed.
  unsigned HashbangBufferID = 0U;

  /// Associates buffer identifiers to buffer IDs.
  llvm::StringMap<unsigned> BufIdentIDMap;

  // #line directive handling.
  struct VirtualFile {
    CharSourceRange Range;
    std::string Name;
    int LineOffset;
  };
  std::map<const char *, VirtualFile> VirtualFiles;
  mutable std::pair<const char *, const VirtualFile*> CachedVFile = {nullptr, nullptr};

public:
  llvm::SourceMgr &getLLVMSourceMgr() {
    return LLVMSourceMgr;
  }
  const llvm::SourceMgr &getLLVMSourceMgr() const {
    return LLVMSourceMgr;
  }

  void setCodeCompletionPoint(unsigned BufferID, unsigned Offset) {
    assert(BufferID != 0U && "Buffer should be valid");

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
    assert(HashbangBufferID == 0U && "Hashbang buffer ID already set");
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

  /// Returns the buffer ID for the specified *valid* location.
  ///
  /// Because a valid source location always corresponds to a source buffer,
  /// this routine always returns a valid buffer ID.
  unsigned findBufferContainingLoc(SourceLoc Loc) const;

  /// Adds a memory buffer to the SourceManager, taking ownership of it.
  unsigned addNewSourceBuffer(std::unique_ptr<llvm::MemoryBuffer> Buffer);

  /// Add a #line-defined virtual file region.
  ///
  /// By default, this region continues to the end of the buffer.
  ///
  /// \returns True if the new file was added, false if the file already exists.
  /// The name and line offset must match exactly in that case.
  ///
  /// \sa closeVirtualFile.
  bool openVirtualFile(SourceLoc loc, StringRef name, int lineOffset);

  /// Close a #line-defined virtual file region.
  void closeVirtualFile(SourceLoc end);

  /// Creates a copy of a \c MemoryBuffer and adds it to the \c SourceManager,
  /// taking ownership of the copy.
  unsigned addMemBufferCopy(llvm::MemoryBuffer *Buffer);

  /// Creates and adds a memory buffer to the \c SourceManager, taking
  /// ownership of the newly created copy.
  ///
  /// \p InputData and \p BufIdentifier are copied, so that this memory can go
  /// away as soon as this function returns.
  unsigned addMemBufferCopy(StringRef InputData, StringRef BufIdentifier = "");

  /// Returns a buffer ID for a previously added buffer with the given
  /// buffer identifier, or None if there is no such buffer.
  Optional<unsigned> getIDForBufferIdentifier(StringRef BufIdentifier);

  /// \brief Returns a SourceRange covering the entire specified buffer.
  ///
  /// Note that the start location might not point at the first token: it
  /// might point at whitespace or a comment.
  CharSourceRange getRangeForBuffer(unsigned BufferID) const;

  /// Returns the SourceLoc for the beginning of the specified buffer
  /// (at offset zero).
  ///
  /// Note that the resulting location might not point at the first token: it
  /// might point at whitespace or a comment.
  SourceLoc getLocForBufferStart(unsigned BufferID) const {
    return getRangeForBuffer(BufferID).getStart();
  }

  /// \brief Returns the offset in bytes for the given valid source location.
  unsigned getLocOffsetInBuffer(SourceLoc Loc, unsigned BufferID) const;

  /// \brief Returns the distance in bytes between the given valid source
  /// locations.
  unsigned getByteDistance(SourceLoc Start, SourceLoc End) const;

  /// Returns the SourceLoc for the byte offset in the specified buffer.
  SourceLoc getLocForOffset(unsigned BufferID, unsigned Offset) const {
    return getLocForBufferStart(BufferID).getAdvancedLoc(Offset);
  }

  /// Returns the identifier string for the buffer with the given ID.
  ///
  /// \p BufferID must be a valid buffer ID.
  StringRef getIdentifierForBuffer(unsigned BufferID) const;

  /// Returns the identifier string for the buffer containing the given valid 
  /// source location.
  ///
  /// Note that this doesn't respect #sourceLocation directives.
  StringRef getIdentifierForBuffer(SourceLoc Loc) const {
    return getIdentifierForBuffer(findBufferContainingLoc(Loc));
  }

  /// Returns the physical line and column represented by the given valid
  /// source location.
  ///
  /// Note that this doesn't respect #sourceLocation directives.
  /// If \p BufferID is provided, \p Loc must come from that source buffer.
  std::pair<unsigned, unsigned>
  getLineAndColumnInBuffer(SourceLoc Loc, unsigned BufferID = 0) const {
    return LLVMSourceMgr.getLineAndColumn(Loc.Value, BufferID);
  }

  /// Translate line and column pair to the offset.
  llvm::Optional<unsigned>
  getOffsetForLineAndColumnInBuffer(unsigned BufferId, unsigned Line,
                                    unsigned Col) const;


  /// Returns the source location from the given physical line and col in the
  /// specified buffer.
  SourceLoc getLocForLineAndColumnInBuffer(unsigned BufferId, unsigned Line,
                                           unsigned Col) const {
    auto Offset = getOffsetForLineAndColumnInBuffer(BufferId, Line, Col);
    return Offset.hasValue() ? getLocForOffset(BufferId, Offset.getValue()) :
                               SourceLoc();
  }

  /// Returns the presumed filename string containing the given valid source
  /// location.
  ///
  /// This respects \c #sourceLocation direvtive.
  StringRef getPresumedFilenameForLoc(SourceLoc Loc) const {
    if (auto VFile = getVirtualFile(Loc))
      return VFile->Name;
    else
      return getIdentifierForBuffer(Loc);
  }

  /// Returns the line and column represented by the given valid source
  /// location.
  ///
  /// If \p BufferID is provided, \p Loc must come from that source buffer.
  /// This respects \c #sourceLocation direvtive.
  std::pair<unsigned, unsigned>
  getPresumedLineAndColumnForLoc(SourceLoc Loc, unsigned BufferID = 0) const {
    assert(Loc.isValid());
    int l, c;
    std::tie(l, c) = LLVMSourceMgr.getLineAndColumn(Loc.Value, BufferID);

    if (auto VFile = getVirtualFile(Loc)) {
      assert(l + VFile->LineOffset > 0 && "bogus line offset");
      l += VFile->LineOffset;
    }
    return { l, c };
  }

  StringRef extractText(CharSourceRange Range,
                        Optional<unsigned> BufferID = None) const;

  llvm::SMDiagnostic GetMessage(SourceLoc Loc, llvm::SourceMgr::DiagKind Kind,
                                const Twine &Msg,
                                ArrayRef<llvm::SMRange> Ranges,
                                ArrayRef<llvm::SMFixIt> FixIts) const;

  /// Verifies that all buffers are still valid.
  void verifyAllBuffers() const;

private:
  const VirtualFile *getVirtualFile(SourceLoc Loc) const;
};

} // end namespace swift

#endif // SWIFT_BASIC_SOURCEMANAGER_H

