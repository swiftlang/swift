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
#include "swift/Basic/Optional.h"
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
    const char *Begin, *End;
    std::string Name;
    int LineOffset;
  };
  std::map<const char *, VirtualFile> VirtualFiles;
  mutable std::pair<const char *, const VirtualFile*> CachedVFile
    = { 0, nullptr };
  /// Used during parsing.
  Optional<VirtualFile> CurrentVFile;

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
  ///
  /// FIXME: remove this overload.
  unsigned addNewSourceBuffer(llvm::MemoryBuffer *Buffer);

  unsigned addNewSourceBuffer(std::unique_ptr<llvm::MemoryBuffer> Buffer) {
    return addNewSourceBuffer(Buffer.release());
  }

  bool inVirtualFile() { return CurrentVFile.hasValue(); }

  /// Start a new #line-defined virtual file region.
  void beginVirtualFile(const char *Begin, StringRef Name, int LineOffset) {
    assert(!CurrentVFile.hasValue() && "previous VFile still open");
    CurrentVFile = VirtualFile{ Begin, nullptr, Name.str(), LineOffset };
  }

  /// Close a #line-defined virtual file region.
  void closeVirtualFile(const char *End) {
    assert(CurrentVFile.hasValue() && "no open VFile");
    VirtualFiles[End] = { CurrentVFile->Begin, End,
                          CurrentVFile->Name,
                          CurrentVFile->LineOffset };
    CurrentVFile.reset();
  }

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
  /// buffer identifier, or Nothing if there is no such buffer.
  Optional<unsigned> getIDForBufferIdentifier(StringRef BufIdentifier);

  /// Returns the identifier for the buffer with the given ID.
  ///
  /// \p BufferID must be a valid buffer ID.
  const char *getIdentifierForBuffer(unsigned BufferID) const;

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

  const VirtualFile *getVirtualFile(llvm::SMLoc Loc) const;

  int getLineOffset(llvm::SMLoc Loc) const {
    if (auto VFile = getVirtualFile(Loc))
      return VFile->LineOffset;
    else
      return 0;
  }

  const char *getBufferIdentifierForLoc(SourceLoc Loc) const {
    if (auto VFile = getVirtualFile(Loc.Value))
      return VFile->Name.c_str();
    else
      return getIdentifierForBuffer(findBufferContainingLoc(Loc));
  }

  std::pair<unsigned, unsigned>
  getLineAndColumn(SourceLoc Loc, unsigned BufferID = 0) const {
    return getLineAndColumn(Loc.Value, BufferID);
  }

  std::pair<unsigned, unsigned> getLineAndColumn(llvm::SMLoc Loc,
                                                 unsigned BufferID = 0) const {
    assert(Loc.isValid());
    int LineOffset = getLineOffset(Loc);
    int l, c;
    std::tie(l, c) = LLVMSourceMgr.getLineAndColumn(Loc, BufferID);
    assert(LineOffset+l > 0 && "bogus line offset");
    return { LineOffset + l, c };
  }

  unsigned getLineNumber(SourceLoc Loc, unsigned BufferID = 0) const {
    assert(Loc.isValid());
    return LLVMSourceMgr.FindLineNumber(Loc.Value, BufferID);
  }

  StringRef extractText(CharSourceRange Range,
                        Optional<unsigned> BufferID = {}) const;

  llvm::SMDiagnostic GetMessage(SourceLoc Loc, llvm::SourceMgr::DiagKind Kind,
                                const Twine &Msg,
                                ArrayRef<llvm::SMRange> Ranges,
                                ArrayRef<llvm::SMFixIt> FixIts) const;
};

} // namespace swift

#endif // LLVM_SWIFT_SOURCEMANAGER_H

