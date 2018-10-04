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

#include "swift/Basic/FileSystem.h"
#include "swift/Basic/SourceLoc.h"
#include "clang/Basic/FileManager.h"
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/Support/SourceMgr.h"
#include <map>

namespace swift {

/// \brief This class manages and owns source buffers.
class SourceManager {
  llvm::SourceMgr LLVMSourceMgr;
  llvm::IntrusiveRefCntPtr<clang::vfs::FileSystem> FileSystem;
  unsigned CodeCompletionBufferID = 0U;
  unsigned CodeCompletionOffset;

  /// Associates buffer identifiers to buffer IDs.
  llvm::DenseMap<StringRef, unsigned> BufIdentIDMap;

  /// A cache mapping buffer identifiers to vfs Status entries.
  ///
  /// This is as much a hack to prolong the lifetime of status objects as it is
  /// to speed up stats.
  mutable llvm::DenseMap<StringRef, clang::vfs::Status> StatusCache;

  // \c #sourceLocation directive handling.
  struct VirtualFile {
    CharSourceRange Range;
    std::string Name;
    int LineOffset;
  };
  std::map<const char *, VirtualFile> VirtualFiles;
  mutable std::pair<const char *, const VirtualFile*> CachedVFile = {nullptr, nullptr};

public:
  SourceManager(llvm::IntrusiveRefCntPtr<clang::vfs::FileSystem> FS =
                    clang::vfs::getRealFileSystem())
    : FileSystem(FS) {}

  llvm::SourceMgr &getLLVMSourceMgr() {
    return LLVMSourceMgr;
  }
  const llvm::SourceMgr &getLLVMSourceMgr() const {
    return LLVMSourceMgr;
  }

  void setFileSystem(llvm::IntrusiveRefCntPtr<clang::vfs::FileSystem> FS) {
    FileSystem = FS;
  }

  llvm::IntrusiveRefCntPtr<clang::vfs::FileSystem> getFileSystem() {
    return FileSystem;
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

  /// Add a \c #sourceLocation-defined virtual file region.
  ///
  /// By default, this region continues to the end of the buffer.
  ///
  /// \returns True if the new file was added, false if the file already exists.
  /// The name and line offset must match exactly in that case.
  ///
  /// \sa closeVirtualFile
  bool openVirtualFile(SourceLoc loc, StringRef name, int lineOffset);

  /// Close a \c #sourceLocation-defined virtual file region.
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

  /// Returns the identifier for the buffer with the given ID.
  ///
  /// \p BufferID must be a valid buffer ID.
  ///
  /// This should not be used for displaying information about the \e contents
  /// of a buffer, since lines within the buffer may be marked as coming from
  /// other files using \c #sourceLocation. Use #getDisplayNameForLoc instead
  /// in that case.
  StringRef getIdentifierForBuffer(unsigned BufferID) const;

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

  /// Returns a buffer identifier suitable for display to the user containing
  /// the given source location.
  ///
  /// This respects \c #sourceLocation directives and the 'use-external-names'
  /// directive in VFS overlay files. If you need an on-disk file name, use
  /// #getIdentifierForBuffer instead.
  StringRef getDisplayNameForLoc(SourceLoc Loc) const;

  /// Returns the line and column represented by the given source location.
  ///
  /// If \p BufferID is provided, \p Loc must come from that source buffer.
  ///
  /// This respects \c #sourceLocation directives.
  std::pair<unsigned, unsigned>
  getLineAndColumn(SourceLoc Loc, unsigned BufferID = 0) const {
    assert(Loc.isValid());
    int LineOffset = getLineOffset(Loc);
    int l, c;
    std::tie(l, c) = LLVMSourceMgr.getLineAndColumn(Loc.Value, BufferID);
    assert(LineOffset+l > 0 && "bogus line offset");
    return { LineOffset + l, c };
  }

  /// Returns the real line number for a source location.
  ///
  /// If \p BufferID is provided, \p Loc must come from that source buffer.
  ///
  /// This does not respect \c #sourceLocation directives.
  unsigned getLineNumber(SourceLoc Loc, unsigned BufferID = 0) const {
    assert(Loc.isValid());
    return LLVMSourceMgr.FindLineNumber(Loc.Value, BufferID);
  }

  StringRef getEntireTextForBuffer(unsigned BufferID) const;

  StringRef extractText(CharSourceRange Range,
                        Optional<unsigned> BufferID = None) const;

  llvm::SMDiagnostic GetMessage(SourceLoc Loc, llvm::SourceMgr::DiagKind Kind,
                                const Twine &Msg,
                                ArrayRef<llvm::SMRange> Ranges,
                                ArrayRef<llvm::SMFixIt> FixIts) const;

  /// Verifies that all buffers are still valid.
  void verifyAllBuffers() const;

  /// Translate line and column pair to the offset.
  llvm::Optional<unsigned> resolveFromLineCol(unsigned BufferId, unsigned Line,
                                              unsigned Col) const;

  SourceLoc getLocForLineCol(unsigned BufferId, unsigned Line, unsigned Col) const {
    auto Offset = resolveFromLineCol(BufferId, Line, Col);
    return Offset.hasValue() ? getLocForOffset(BufferId, Offset.getValue()) :
                               SourceLoc();
  }

private:
  const VirtualFile *getVirtualFile(SourceLoc Loc) const;

  int getLineOffset(SourceLoc Loc) const {
    if (auto VFile = getVirtualFile(Loc))
      return VFile->LineOffset;
    else
      return 0;
  }
};

} // end namespace swift

#endif // SWIFT_BASIC_SOURCEMANAGER_H

