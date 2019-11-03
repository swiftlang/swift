//===--- SourceLoc.cpp - SourceLoc and SourceRange implementations --------===//
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

#include "swift/Basic/SourceLoc.h"
#include "swift/Basic/SourceManager.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/PrettyStackTrace.h"
#include "llvm/Support/raw_ostream.h"

using namespace swift;

void SourceManager::verifyAllBuffers() const {
  llvm::PrettyStackTraceString backtrace{
    "Checking that all source buffers are still valid"
  };

  // FIXME: This depends on the buffer IDs chosen by llvm::SourceMgr.
  LLVM_ATTRIBUTE_USED static char arbitraryTotal = 0;
  for (unsigned i = 1, e = LLVMSourceMgr.getNumBuffers(); i <= e; ++i) {
    auto *buffer = LLVMSourceMgr.getMemoryBuffer(i);
    if (buffer->getBufferSize() == 0)
      continue;
    arbitraryTotal += buffer->getBufferStart()[0];
    arbitraryTotal += buffer->getBufferEnd()[-1];
  }
}

SourceLoc SourceManager::getCodeCompletionLoc() const {
  if (CodeCompletionBufferID == 0U)
    return SourceLoc();

  return getLocForBufferStart(CodeCompletionBufferID)
      .getAdvancedLoc(CodeCompletionOffset);
}

StringRef SourceManager::getDisplayNameForLoc(SourceLoc Loc) const {
  // Respect #line first
  if (auto VFile = getVirtualFile(Loc))
    return VFile->Name;

  // Next, try the stat cache
  auto Ident = getIdentifierForBuffer(findBufferContainingLoc(Loc));
  auto found = StatusCache.find(Ident);
  if (found != StatusCache.end()) {
    return found->second.getName();
  }

  // Populate the cache with a (virtual) stat.
  if (auto Status = FileSystem->status(Ident)) {
    return (StatusCache[Ident] = Status.get()).getName();
  }

  // Finally, fall back to the buffer identifier.
  return Ident;
}

unsigned
SourceManager::addNewSourceBuffer(std::unique_ptr<llvm::MemoryBuffer> Buffer) {
  assert(Buffer);
  StringRef BufIdentifier = Buffer->getBufferIdentifier();
  auto ID = LLVMSourceMgr.AddNewSourceBuffer(std::move(Buffer), llvm::SMLoc());
  BufIdentIDMap[BufIdentifier] = ID;
  return ID;
}

unsigned SourceManager::addMemBufferCopy(llvm::MemoryBuffer *Buffer) {
  return addMemBufferCopy(Buffer->getBuffer(), Buffer->getBufferIdentifier());
}

unsigned SourceManager::addMemBufferCopy(StringRef InputData,
                                         StringRef BufIdentifier) {
  auto Buffer = std::unique_ptr<llvm::MemoryBuffer>(
      llvm::MemoryBuffer::getMemBufferCopy(InputData, BufIdentifier));
  return addNewSourceBuffer(std::move(Buffer));
}

bool SourceManager::openVirtualFile(SourceLoc loc, StringRef name,
                                    int lineOffset) {
  CharSourceRange fullRange = getRangeForBuffer(findBufferContainingLoc(loc));
  SourceLoc end;

  auto nextRangeIter = VirtualFiles.upper_bound(loc.Value.getPointer());
  if (nextRangeIter != VirtualFiles.end() &&
      fullRange.contains(nextRangeIter->second.Range.getStart())) {
    const VirtualFile &existingFile = nextRangeIter->second;
    if (existingFile.Range.getStart() == loc) {
      assert(existingFile.Name == name);
      assert(existingFile.LineOffset == lineOffset);
      return false;
    }
    assert(!existingFile.Range.contains(loc) &&
           "must close current open file first");
    end = nextRangeIter->second.Range.getStart();
  } else {
    end = fullRange.getEnd();
  }

  CharSourceRange range = CharSourceRange(*this, loc, end);
  VirtualFiles[end.Value.getPointer()] = { range, name, lineOffset };
  CachedVFile = {nullptr, nullptr};
  return true;
}

void SourceManager::closeVirtualFile(SourceLoc end) {
  auto *virtualFile = const_cast<VirtualFile *>(getVirtualFile(end));
  if (!virtualFile) {
#ifndef NDEBUG
    unsigned bufferID = findBufferContainingLoc(end);
    CharSourceRange fullRange = getRangeForBuffer(bufferID);
    assert((fullRange.getByteLength() == 0 ||
            getVirtualFile(end.getAdvancedLoc(-1))) &&
           "no open virtual file for this location");
    assert(fullRange.getEnd() == end);
#endif
    return;
  }
  CachedVFile = {nullptr, nullptr};

  CharSourceRange oldRange = virtualFile->Range;
  virtualFile->Range = CharSourceRange(*this, virtualFile->Range.getStart(),
                                       end);
  VirtualFiles[end.Value.getPointer()] = std::move(*virtualFile);

  bool existed = VirtualFiles.erase(oldRange.getEnd().Value.getPointer());
  assert(existed);
  (void)existed;
}

const SourceManager::VirtualFile *
SourceManager::getVirtualFile(SourceLoc Loc) const {
  const char *p = Loc.Value.getPointer();

  if (CachedVFile.first == p)
    return CachedVFile.second;

  // Returns the first element that is >p.
  auto VFileIt = VirtualFiles.upper_bound(p);
  if (VFileIt != VirtualFiles.end() && VFileIt->second.Range.contains(Loc)) {
    CachedVFile = { p, &VFileIt->second };
    return CachedVFile.second;
  }

  return nullptr;
}


Optional<unsigned> SourceManager::getIDForBufferIdentifier(
    StringRef BufIdentifier) {
  auto It = BufIdentIDMap.find(BufIdentifier);
  if (It == BufIdentIDMap.end())
    return None;
  return It->second;
}

StringRef SourceManager::getIdentifierForBuffer(unsigned bufferID) const {
  auto *buffer = LLVMSourceMgr.getMemoryBuffer(bufferID);
  assert(buffer && "invalid buffer ID");
  return buffer->getBufferIdentifier();
}

CharSourceRange SourceManager::getRangeForBuffer(unsigned bufferID) const {
  auto *buffer = LLVMSourceMgr.getMemoryBuffer(bufferID);
  SourceLoc start{llvm::SMLoc::getFromPointer(buffer->getBufferStart())};
  return CharSourceRange(start, buffer->getBufferSize());
}

unsigned SourceManager::getLocOffsetInBuffer(SourceLoc Loc,
                                             unsigned BufferID) const {
  assert(Loc.isValid() && "location should be valid");
  auto *Buffer = LLVMSourceMgr.getMemoryBuffer(BufferID);
  assert(Loc.Value.getPointer() >= Buffer->getBuffer().begin() &&
         Loc.Value.getPointer() <= Buffer->getBuffer().end() &&
         "Location is not from the specified buffer");
  return Loc.Value.getPointer() - Buffer->getBuffer().begin();
}

unsigned SourceManager::getByteDistance(SourceLoc Start, SourceLoc End) const {
  assert(Start.isValid() && "start location should be valid");
  assert(End.isValid() && "end location should be valid");
#ifndef NDEBUG
  unsigned BufferID = findBufferContainingLoc(Start);
  auto *Buffer = LLVMSourceMgr.getMemoryBuffer(BufferID);
  assert(End.Value.getPointer() >= Buffer->getBuffer().begin() &&
         End.Value.getPointer() <= Buffer->getBuffer().end() &&
         "End location is not from the same buffer");
#endif
  // When we have a rope buffer, could be implemented in terms of
  // getLocOffsetInBuffer().
  return End.Value.getPointer() - Start.Value.getPointer();
}

StringRef SourceManager::getEntireTextForBuffer(unsigned BufferID) const {
  return LLVMSourceMgr.getMemoryBuffer(BufferID)->getBuffer();
}

StringRef SourceManager::extractText(CharSourceRange Range,
                                     Optional<unsigned> BufferID) const {
  assert(Range.isValid() && "range should be valid");

  if (!BufferID)
    BufferID = findBufferContainingLoc(Range.getStart());
  StringRef Buffer = LLVMSourceMgr.getMemoryBuffer(*BufferID)->getBuffer();
  return Buffer.substr(getLocOffsetInBuffer(Range.getStart(), *BufferID),
                       Range.getByteLength());
}

Optional<unsigned>
SourceManager::findBufferContainingLocInternal(SourceLoc Loc) const {
  assert(Loc.isValid());
  // Search the buffers back-to front, so later alias buffers are
  // visited first.
  auto less_equal = std::less_equal<const char *>();
  for (unsigned i = LLVMSourceMgr.getNumBuffers(), e = 1; i >= e; --i) {
    auto Buf = LLVMSourceMgr.getMemoryBuffer(i);
    if (less_equal(Buf->getBufferStart(), Loc.Value.getPointer()) &&
        // Use <= here so that a pointer to the null at the end of the buffer
        // is included as part of the buffer.
        less_equal(Loc.Value.getPointer(), Buf->getBufferEnd()))
      return i;
  }
  return None;
}

unsigned SourceManager::findBufferContainingLoc(SourceLoc Loc) const {
  auto Id = findBufferContainingLocInternal(Loc);
  if (Id.hasValue())
    return *Id;
  llvm_unreachable("no buffer containing location found");
}

bool SourceManager::isOwning(SourceLoc Loc) const {
  return findBufferContainingLocInternal(Loc).hasValue();
}

void SourceRange::widen(SourceRange Other) {
  if (Other.Start.Value.getPointer() < Start.Value.getPointer())
    Start = Other.Start;
  if (Other.End.Value.getPointer() > End.Value.getPointer())
    End = Other.End;
}

void SourceLoc::printLineAndColumn(raw_ostream &OS, const SourceManager &SM,
                                   unsigned BufferID) const {
  if (isInvalid()) {
    OS << "<invalid loc>";
    return;
  }

  auto LineAndCol = SM.getLineAndColumn(*this, BufferID);
  OS << "line:" << LineAndCol.first << ':' << LineAndCol.second;
}

void SourceLoc::print(raw_ostream &OS, const SourceManager &SM,
                      unsigned &LastBufferID) const {
  if (isInvalid()) {
    OS << "<invalid loc>";
    return;
  }

  unsigned BufferID = SM.findBufferContainingLoc(*this);
  if (BufferID != LastBufferID) {
    OS << SM.getIdentifierForBuffer(BufferID);
    LastBufferID = BufferID;
  } else {
    OS << "line";
  }

  auto LineAndCol = SM.getLineAndColumn(*this, BufferID);
  OS << ':' << LineAndCol.first << ':' << LineAndCol.second;
}

void SourceLoc::dump(const SourceManager &SM) const {
  print(llvm::errs(), SM);
}

void SourceRange::print(raw_ostream &OS, const SourceManager &SM,
                        unsigned &LastBufferID, bool PrintText) const {
  // FIXME: CharSourceRange is a half-open character-based range, while
  // SourceRange is a closed token-based range, so this conversion omits the
  // last token in the range. Unfortunately, we can't actually get to the end
  // of the token without using the Lex library, which would be a layering
  // violation. This is still better than nothing.
  CharSourceRange(SM, Start, End).print(OS, SM, LastBufferID, PrintText);
}

void SourceRange::dump(const SourceManager &SM) const {
  print(llvm::errs(), SM);
}

CharSourceRange::CharSourceRange(const SourceManager &SM, SourceLoc Start,
                                 SourceLoc End)
    : Start(Start) {
  assert(Start.isValid() == End.isValid() &&
         "Start and end should either both be valid or both be invalid!");
  if (Start.isValid())
    ByteLength = SM.getByteDistance(Start, End);
}

void CharSourceRange::print(raw_ostream &OS, const SourceManager &SM,
                            unsigned &LastBufferID, bool PrintText) const {
  OS << '[';
  Start.print(OS, SM, LastBufferID);
  OS << " - ";
  getEnd().print(OS, SM, LastBufferID);
  OS << ']';

  if (Start.isInvalid() || getEnd().isInvalid())
    return;

  if (PrintText) {
    OS << " RangeText=\"" << SM.extractText(*this) << '"';
  }
}

void CharSourceRange::dump(const SourceManager &SM) const {
  print(llvm::errs(), SM);
}

llvm::Optional<unsigned>
SourceManager::resolveOffsetForEndOfLine(unsigned BufferId,
                                         unsigned Line) const {
  return resolveFromLineCol(BufferId, Line, ~0u);
}

llvm::Optional<unsigned> SourceManager::resolveFromLineCol(unsigned BufferId,
                                                           unsigned Line,
                                                           unsigned Col) const {
  if (Line == 0 || Col == 0) {
    return None;
  }
  const bool LineEnd = Col == ~0u;
  auto InputBuf = getLLVMSourceMgr().getMemoryBuffer(BufferId);
  const char *Ptr = InputBuf->getBufferStart();
  const char *End = InputBuf->getBufferEnd();
  const char *LineStart = Ptr;
  --Line;
  for (; Line && (Ptr < End); ++Ptr) {
    if (*Ptr == '\n') {
      --Line;
      LineStart = Ptr+1;
    }
  }
  if (Line != 0) {
    return None;
  }
  Ptr = LineStart;
  // The <= here is to allow for non-inclusive range end positions at EOF
  for (; ; ++Ptr) {
    --Col;
    if (Col == 0)
      return Ptr - InputBuf->getBufferStart();
    if (*Ptr == '\n' || Ptr == End) {
      if (LineEnd) {
        return Ptr - InputBuf->getBufferStart();
      } else {
        break;
      }
    }
  }
  return None;
}

unsigned SourceManager::getExternalSourceBufferId(StringRef Path) {
  auto It = BufIdentIDMap.find(Path);
  if (It != BufIdentIDMap.end()) {
    return It->getSecond();
  }
  unsigned Id = 0u;
  auto InputFileOrErr = swift::vfs::getFileOrSTDIN(*getFileSystem(), Path);
  if (InputFileOrErr) {
    // This assertion ensures we can look up from the map in the future when
    // using the same Path.
    assert(InputFileOrErr.get()->getBufferIdentifier() == Path);
    Id = addNewSourceBuffer(std::move(InputFileOrErr.get()));
  }
  return Id;
}

SourceLoc
SourceManager::getLocFromExternalSource(StringRef Path, unsigned Line,
                                        unsigned Col) {
  auto BufferId = getExternalSourceBufferId(Path);
  if (BufferId == 0u)
    return SourceLoc();
  auto Offset = resolveFromLineCol(BufferId, Line, Col);
  if (!Offset.hasValue())
    return SourceLoc();
  return getLocForOffset(BufferId, *Offset);
}
