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

#include "swift/AST/SourceFile.h"
#include "swift/Basic/Assertions.h"
#include "swift/Basic/Range.h"
#include "swift/Basic/SourceLoc.h"
#include "swift/Basic/SourceManager.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/PrettyStackTrace.h"
#include "llvm/Support/Signals.h"
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
  (void)arbitraryTotal;
}

SourceLoc SourceManager::getIDEInspectionTargetLoc() const {
  if (IDEInspectionTargetBufferID == 0U)
    return SourceLoc();

  return getLocForBufferStart(IDEInspectionTargetBufferID)
      .getAdvancedLoc(IDEInspectionTargetOffset);
}

bool SourceManager::containsRespectingReplacedRanges(SourceRange Range,
                                                     SourceLoc Loc) const {
  if (Loc.isInvalid() || Range.isInvalid()) {
    return false;
  }

  if (Range.contains(Loc)) {
    return true;
  }
  for (const auto &pair : getReplacedRanges()) {
    auto OriginalRange = pair.first;
    auto NewRange = pair.second;
    if (NewRange.contains(Loc) && Range.overlaps(OriginalRange)) {
      return true;
    }
  }
  return false;
}

bool SourceManager::rangeContainsRespectingReplacedRanges(
    SourceRange Enclosing, SourceRange Inner) const {
  return containsRespectingReplacedRanges(Enclosing, Inner.Start) &&
         containsRespectingReplacedRanges(Enclosing, Inner.End);
}

StringRef SourceManager::getDisplayNameForLoc(SourceLoc Loc, bool ForceGeneratedSourceToDisk) const {
  // Respect #line first
  if (auto VFile = getVirtualFile(Loc))
    return VFile->Name;

  // Next, try the stat cache
  auto Ident = getIdentifierForBuffer(
      findBufferContainingLoc(Loc), ForceGeneratedSourceToDisk);
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

void SourceManager::createVirtualFile(SourceLoc Loc, StringRef Name,
                                      int LineOffset, unsigned Length) {
  CharSourceRange Range = CharSourceRange(Loc, Length);

  // Skip if this range has already been added
  VirtualFile &File = VirtualFiles[Range.getEnd().Value.getPointer()];
  if (File.Range.isValid()) {
    assert(Name == StringRef(File.Name));
    assert(LineOffset == File.LineOffset);
    assert(Range == File.Range);
    return;
  }

  File.Range = Range;
  File.Name = Name.str();
  File.LineOffset = LineOffset;

  if (CachedVFile.first && Range.contains(SourceLoc(llvm::SMLoc::getFromPointer(
                               CachedVFile.first)))) {
    CachedVFile = {nullptr, nullptr};
  }
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
  VirtualFiles[end.Value.getPointer()] = {range, name.str(), lineOffset};
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

std::optional<unsigned>
SourceManager::getIDForBufferIdentifier(StringRef BufIdentifier) const {
  auto It = BufIdentIDMap.find(BufIdentifier);
  if (It == BufIdentIDMap.end())
    return std::nullopt;
  return It->second;
}

void SourceManager::recordSourceFile(unsigned bufferID, SourceFile *sourceFile){
  bufferIDToSourceFiles[bufferID].push_back(sourceFile);
}

llvm::TinyPtrVector<SourceFile *>
SourceManager::getSourceFilesForBufferID(unsigned bufferID) const {
  auto found = bufferIDToSourceFiles.find(bufferID);
  if (found == bufferIDToSourceFiles.end())
    return { };

  return found->second;
}

SourceManager::~SourceManager() {
  for (auto &generated : GeneratedSourceInfos) {
    free((void*)generated.second.onDiskBufferCopyFileName.data());

    if (generated.second.ancestors.size() > 0) {
      delete [] generated.second.ancestors.data();
    }
  }
}

/// Dump the contents of the given memory buffer to a file, returning the
/// name of that file (when successful) and \c None otherwise.
static std::optional<std::string>
dumpBufferToFile(const llvm::MemoryBuffer *buffer,
                 const SourceManager &sourceMgr,
                 CharSourceRange originalSourceRange) {
  // Create file in the system temporary directory.
  SmallString<128> outputFileName;
  llvm::sys::path::system_temp_directory(true, outputFileName);
  llvm::sys::path::append(outputFileName, "swift-generated-sources");
  if (llvm::sys::fs::create_directory(outputFileName))
    return std::nullopt;

  // Finalize the name of the resulting file. This is unique based on name
  // mangling.
  llvm::sys::path::append(outputFileName, buffer->getBufferIdentifier());

  std::error_code ec = atomicallyWritingToFile(outputFileName,
     [&](llvm::raw_pwrite_stream &out) {
       auto contents = buffer->getBuffer();
       out << contents;

        // Make sure we have a trailing newline.
        if (contents.empty() || contents.back() != '\n')
          out << "\n";

        // If we know the source range this comes from, append it later in
        // the file so one can trace.
        if (originalSourceRange.isValid()) {
          out << "\n";

          auto originalFilename =
            sourceMgr.getDisplayNameForLoc(originalSourceRange.getStart(),
                                           true);
          unsigned startLine, startColumn, endLine, endColumn;
          std::tie(startLine, startColumn) =
              sourceMgr.getPresumedLineAndColumnForLoc(
                originalSourceRange.getStart());
          std::tie(endLine, endColumn) =
              sourceMgr.getPresumedLineAndColumnForLoc(
                originalSourceRange.getEnd());
          out << "// original-source-range: "
              << originalFilename
              << ":" << startLine << ":" << startColumn
              << "-" << endLine << ":" << endColumn
              << "\n";
      }
    });
  if (ec)
    return std::nullopt;

  return outputFileName.str().str();
}

StringRef SourceManager::getIdentifierForBuffer(
    unsigned bufferID, bool ForceGeneratedSourceToDisk
) const {
  auto *buffer = LLVMSourceMgr.getMemoryBuffer(bufferID);
  assert(buffer && "invalid buffer ID");

  // If this is generated source code, and we're supposed to force it to disk
  // so external clients can see it, do so now.
  if (ForceGeneratedSourceToDisk) {
    if (const GeneratedSourceInfo *generatedInfo =
            getGeneratedSourceInfo(bufferID)) {
      // We only care about macro expansion buffers, so skip everything else.
      if (generatedInfo->kind == GeneratedSourceInfo::ReplacedFunctionBody ||
          generatedInfo->kind == GeneratedSourceInfo::PrettyPrinted ||
          generatedInfo->kind == GeneratedSourceInfo::DefaultArgument ||
          generatedInfo->kind == GeneratedSourceInfo::AttributeFromClang)
        return buffer->getBufferIdentifier();

      if (generatedInfo->onDiskBufferCopyFileName.empty()) {
        if (auto newFileNameOpt = dumpBufferToFile(
                buffer, *this,  generatedInfo->originalSourceRange)) {
          generatedInfo->onDiskBufferCopyFileName =
              strdup(newFileNameOpt->c_str());
        }
      }

      if (!generatedInfo->onDiskBufferCopyFileName.empty())
        return generatedInfo->onDiskBufferCopyFileName;
    }
  }

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

unsigned SourceManager::getColumnInBuffer(SourceLoc Loc,
                                          unsigned BufferID) const {
  assert(Loc.isValid());

  const StringRef Buffer = getEntireTextForBuffer(BufferID);
  const char *Ptr = static_cast<const char *>(Loc.getOpaquePointerValue());

  StringRef UpToLoc = Buffer.slice(0, Ptr - Buffer.data());

  size_t ColumnNo = UpToLoc.size();
  size_t NewlinePos = UpToLoc.find_last_of("\r\n");
  if (NewlinePos != StringRef::npos)
    ColumnNo -= NewlinePos;

  return static_cast<unsigned>(ColumnNo);
}

StringRef SourceManager::getEntireTextForBuffer(unsigned BufferID) const {
  return LLVMSourceMgr.getMemoryBuffer(BufferID)->getBuffer();
}

StringRef SourceManager::extractText(CharSourceRange Range,
                                     std::optional<unsigned> BufferID) const {
  assert(Range.isValid() && "range should be valid");

  if (!BufferID)
    BufferID = findBufferContainingLoc(Range.getStart());
  StringRef Buffer = LLVMSourceMgr.getMemoryBuffer(*BufferID)->getBuffer();
  return Buffer.substr(getLocOffsetInBuffer(Range.getStart(), *BufferID),
                       Range.getByteLength());
}

void SourceManager::setGeneratedSourceInfo(
    unsigned bufferID, GeneratedSourceInfo info
) {
  assert(GeneratedSourceInfos.count(bufferID) == 0);
  GeneratedSourceInfos[bufferID] = info;

  switch (info.kind) {
#define MACRO_ROLE(Name, Description) \
  case GeneratedSourceInfo::Name##MacroExpansion:
#include "swift/Basic/MacroRoles.def"
  case GeneratedSourceInfo::PrettyPrinted:
  case GeneratedSourceInfo::DefaultArgument:
  case GeneratedSourceInfo::AttributeFromClang:
    break;

  case GeneratedSourceInfo::ReplacedFunctionBody:
    // Keep track of the replaced range.
    SourceRange orig(info.originalSourceRange.getStart(),
                     info.originalSourceRange.getEnd());
    ReplacedRanges[orig] =
        SourceRange(info.generatedSourceRange.getStart(),
                    info.generatedSourceRange.getEnd());
    break;
  }
}

bool SourceManager::hasGeneratedSourceInfo(unsigned bufferID) {
  return GeneratedSourceInfos.count(bufferID);
}

const GeneratedSourceInfo *
SourceManager::getGeneratedSourceInfo(unsigned bufferID) const {
  auto known = GeneratedSourceInfos.find(bufferID);
  if (known == GeneratedSourceInfos.end())
    return nullptr;
  return &known->second;
}

namespace {
  /// Compare the source location ranges for two buffers, as an ordering to
  /// use for fast searches.
  struct BufferIDRangeComparison {
    const SourceManager *sourceMgr;

    bool operator()(unsigned lhsID, unsigned rhsID) const {
      auto lhsRange = sourceMgr->getRangeForBuffer(lhsID);
      auto rhsRange = sourceMgr->getRangeForBuffer(rhsID);

      // If the source buffers are identical, we want the higher-numbered
      // source buffers to occur first. This is important when uniquing.
      if (lhsRange == rhsRange)
        return lhsID > rhsID;

      std::less<const char *> pointerCompare;
      return pointerCompare(
          (const char *)lhsRange.getStart().getOpaquePointerValue(),
          (const char *)rhsRange.getStart().getOpaquePointerValue());
    }

    bool operator()(unsigned lhsID, SourceLoc rhsLoc) const {
      auto lhsRange = sourceMgr->getRangeForBuffer(lhsID);

      std::less<const char *> pointerCompare;
      return pointerCompare(
          (const char *)lhsRange.getEnd().getOpaquePointerValue(),
          (const char *)rhsLoc.getOpaquePointerValue());
    }

    bool operator()(SourceLoc lhsLoc, unsigned rhsID) const {
      auto rhsRange = sourceMgr->getRangeForBuffer(rhsID);

      std::less<const char *> pointerCompare;
      return pointerCompare(
          (const char *)lhsLoc.getOpaquePointerValue(),
          (const char *)rhsRange.getEnd().getOpaquePointerValue());
    }
  };

  /// Determine whether the source ranges for two buffers are equivalent.
  struct BufferIDSameRange {
    const SourceManager *sourceMgr;

    bool operator()(unsigned lhsID, unsigned rhsID) const {
      auto lhsRange = sourceMgr->getRangeForBuffer(lhsID);
      auto rhsRange = sourceMgr->getRangeForBuffer(rhsID);

      return lhsRange == rhsRange;
    }
  };
}

std::optional<unsigned>
SourceManager::findBufferContainingLocInternal(SourceLoc Loc) const {
  assert(Loc.isValid());

  // If the cache is out-of-date, update it now.
  unsigned numBuffers = LLVMSourceMgr.getNumBuffers();
  if (numBuffers != LocCache.numBuffersOriginal) {
    LocCache.sortedBuffers.assign(std::begin(range(1, numBuffers + 1)),
                                  std::end(range(1, numBuffers + 1)));
    LocCache.numBuffersOriginal = numBuffers;

    // Sort the buffer IDs by source range.
    std::sort(LocCache.sortedBuffers.begin(), LocCache.sortedBuffers.end(),
              BufferIDRangeComparison{this});

    // Remove lower-numbered buffers with the same source ranges as higher-
    // numbered buffers. We want later alias buffers to be found first.
    auto newEnd =
        std::unique(LocCache.sortedBuffers.begin(),
                    LocCache.sortedBuffers.end(), BufferIDSameRange{this});
    LocCache.sortedBuffers.erase(newEnd, LocCache.sortedBuffers.end());

    // Forget the last buffer we looked at; it might have been replaced.
    LocCache.lastBufferID = std::nullopt;
  }

  // Determine whether the source location we're looking for is within the
  // given buffer ID.
  auto isInBuffer = [&](unsigned bufferID) {
    auto less_equal = std::less_equal<const char *>();
    auto buffer = LLVMSourceMgr.getMemoryBuffer(bufferID);

    return less_equal(buffer->getBufferStart(), Loc.Value.getPointer()) &&
           // Use <= here so that a pointer to the null at the end of the
           // buffer is included as part of the buffer.
           less_equal(Loc.Value.getPointer(), buffer->getBufferEnd());
  };

  // Check the last buffer we looked in.
  if (auto lastBufferID = LocCache.lastBufferID) {
    if (isInBuffer(*lastBufferID))
      return *lastBufferID;
  }

  // Search the sorted list of buffer IDs.
  auto found = std::lower_bound(LocCache.sortedBuffers.begin(),
                                LocCache.sortedBuffers.end(), Loc,
                                BufferIDRangeComparison{this});

  // If the location was past the range covered by source buffers or
  // is not within any of the source buffers, fail.
  if (found == LocCache.sortedBuffers.end() || !isInBuffer(*found))
    return std::nullopt;

  // Cache the buffer ID we just found, because the next location is likely to
  // be close by.
  LocCache.lastBufferID = *found;
  return *found;
}

unsigned SourceManager::findBufferContainingLoc(SourceLoc Loc) const {
  auto Id = findBufferContainingLocInternal(Loc);
  if (Id.has_value())
    return *Id;
  llvm_unreachable("no buffer containing location found");
}

bool SourceManager::isOwning(SourceLoc Loc) const {
  return findBufferContainingLocInternal(Loc).has_value();
}

SourceRange SourceRange::combine(ArrayRef<SourceRange> ranges) {
  if (ranges.empty())
    return SourceRange();

  SourceRange result = ranges.front();
  for (auto other : ranges.drop_front()) {
    if (!other)
      continue;
    if (!result) {
      result = other;
      continue;
    }
    result.widen(other);
  }
  return result;
}

void SourceRange::widen(SourceRange Other) {
  if (Other.Start.Value.getPointer() < Start.Value.getPointer())
    Start = Other.Start;
  if (Other.End.Value.getPointer() > End.Value.getPointer())
    End = Other.End;
}

bool SourceRange::contains(SourceLoc Loc) const {
  return Start.Value.getPointer() <= Loc.Value.getPointer() &&
         Loc.Value.getPointer() <= End.Value.getPointer();
}

bool SourceRange::overlaps(SourceRange Other) const {
  return contains(Other.Start) || Other.contains(Start);
}

void SourceLoc::printLineAndColumn(raw_ostream &OS, const SourceManager &SM,
                                   unsigned BufferID) const {
  if (isInvalid()) {
    OS << "<invalid loc>";
    return;
  }

  auto LineAndCol = SM.getPresumedLineAndColumnForLoc(*this, BufferID);
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

  auto LineAndCol = SM.getPresumedLineAndColumnForLoc(*this, BufferID);
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

std::optional<unsigned>
SourceManager::resolveOffsetForEndOfLine(unsigned BufferId,
                                         unsigned Line) const {
  return resolveFromLineCol(BufferId, Line, ~0u);
}

std::optional<unsigned> SourceManager::getLineLength(unsigned BufferId,
                                                     unsigned Line) const {
  auto BegOffset = resolveFromLineCol(BufferId, Line, 0);
  auto EndOffset = resolveFromLineCol(BufferId, Line, ~0u);
  if (BegOffset && EndOffset) {
     return EndOffset.value() - BegOffset.value();
  }
  return std::nullopt;
}

std::optional<unsigned> SourceManager::resolveFromLineCol(unsigned BufferId,
                                                          unsigned Line,
                                                          unsigned Col) const {
  if (Line == 0) {
    return std::nullopt;
  }
  const bool LineEnd = (Col == ~0u);
  if (LineEnd)
    Col = 0;

  auto loc = const_cast<SourceManager *>(this)
                 ->getLLVMSourceMgr()
                 .FindLocForLineAndColumn(BufferId, Line, Col);
  if (!loc.isValid())
    return std::nullopt;

  auto InputBuf = getLLVMSourceMgr().getMemoryBuffer(BufferId);
  const char *Ptr = loc.getPointer();
  if (LineEnd) {
    const char *End = InputBuf->getBufferEnd();
    for (;; ++Ptr) {
      if (Ptr == End || *Ptr == '\n')
        break;
    }
  }
  return Ptr - InputBuf->getBufferStart();
}

unsigned SourceManager::getExternalSourceBufferID(StringRef Path) {
  auto It = BufIdentIDMap.find(Path);
  if (It != BufIdentIDMap.end()) {
    return It->getSecond();
  }
  unsigned Id = 0u;
  auto InputFileOrErr =
      swift::vfs::getFileOrSTDIN(*getFileSystem(), Path,
                                 /* FileSize */ -1,
                                 /* RequiresNullTerminator */ true,
                                 /* isVolatile */ this->OpenSourcesAsVolatile);
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
  auto BufferId = getExternalSourceBufferID(Path);
  if (BufferId == 0u)
    return SourceLoc();
  auto Offset = resolveFromLineCol(BufferId, Line, Col);
  if (!Offset.has_value())
    return SourceLoc();
  return getLocForOffset(BufferId, *Offset);
}

SourceLoc
SourceManager::getLocForForeignLoc(SourceLoc otherLoc,
                                   SourceManager &otherMgr) {
  if (&otherMgr == this || otherLoc.isInvalid())
    return otherLoc;

  assert(otherMgr.isOwning(otherLoc));

  if (auto otherBufferID = otherMgr.findBufferContainingLocInternal(otherLoc)) {
    auto offset = otherMgr.getLocOffsetInBuffer(otherLoc, *otherBufferID);

    auto otherBufferName = otherMgr.getIdentifierForBuffer(*otherBufferID);
    auto thisBufferID = getIDForBufferIdentifier(otherBufferName);
    if (!thisBufferID) {
      thisBufferID = addMemBufferCopy(
              otherMgr.getEntireTextForBuffer(*otherBufferID), otherBufferName);
    }

    return getLocForOffset(*thisBufferID, offset);
  }

  return SourceLoc();
}

/// Populate the ancestors list for this buffer, with the root source buffer
/// at the beginning and the given source buffer at the end.
static void populateAncestors(
    const SourceManager &sourceMgr, unsigned bufferID,
    SmallVectorImpl<unsigned> &ancestors) {
  if (auto info = sourceMgr.getGeneratedSourceInfo(bufferID)) {
    auto ancestorLoc = info->originalSourceRange.getStart();
    if (ancestorLoc.isValid()) {
      auto ancestorBufferID = sourceMgr.findBufferContainingLoc(ancestorLoc);
      populateAncestors(sourceMgr, ancestorBufferID, ancestors);
    }
  }

  ancestors.push_back(bufferID);
}

ArrayRef<unsigned> SourceManager::getAncestors(
    unsigned bufferID, unsigned &scratch
) const {
  // If there is no generated source information for this buffer, then this is
  // the only buffer here. Avoid memory allocation by using the scratch space
  // we were given.
  auto knownInfo = GeneratedSourceInfos.find(bufferID);
  if (knownInfo == GeneratedSourceInfos.end()) {
    scratch = bufferID;
    return ArrayRef<unsigned>(&scratch, 1);
  }

  // If we already have the ancestors cached, use them.
  if (!knownInfo->second.ancestors.empty())
    return knownInfo->second.ancestors;

  // Compute all of the ancestors. We only do this once for a given buffer.
  SmallVector<unsigned, 4> ancestors;
  populateAncestors(*this, bufferID, ancestors);

  // Cache the ancestors in the generated source info record.
  unsigned *ancestorsPtr = new unsigned [ancestors.size()];
  std::copy(ancestors.begin(), ancestors.end(), ancestorsPtr);
  knownInfo->second.ancestors = llvm::ArrayRef(ancestorsPtr, ancestors.size());
  return knownInfo->second.ancestors;
}


/// Determine whether the first source location precedes the second, accounting
/// for macro expansions.
static bool isBeforeInSource(
    const SourceManager &sourceMgr, SourceLoc firstLoc, SourceLoc secondLoc,
    bool allowEqual) {
  // If the two locations are in the same source buffer, compare their pointers.
  unsigned firstBufferID = sourceMgr.findBufferContainingLoc(firstLoc);
  unsigned secondBufferID = sourceMgr.findBufferContainingLoc(secondLoc);
  if (firstBufferID == secondBufferID) {
    return sourceMgr.isBeforeInBuffer(firstLoc, secondLoc) ||
        (allowEqual && firstLoc == secondLoc);
  }

  // If the two locations are in different source buffers, we need to compute
  // the least common ancestor.
  unsigned firstScratch, secondScratch;
  auto firstAncestors = sourceMgr.getAncestors(firstBufferID, firstScratch);
  auto secondAncestors = sourceMgr.getAncestors(secondBufferID, secondScratch);

  // Find the first mismatch between the two ancestor lists; this is the
  // point of divergence.
  auto [firstMismatch, secondMismatch] = std::mismatch(
      firstAncestors.begin(), firstAncestors.end(),
      secondAncestors.begin(), secondAncestors.end());
  assert(firstMismatch != firstAncestors.begin() &&
         secondMismatch != secondAncestors.begin() &&
         "Ancestors don't have the same root source file");

  SourceLoc firstLocInLCA = firstMismatch == firstAncestors.end()
      ? firstLoc
      : sourceMgr.getGeneratedSourceInfo(*firstMismatch)
          ->originalSourceRange.getEnd();
  SourceLoc secondLocInLCA = secondMismatch == secondAncestors.end()
      ? secondLoc
      : sourceMgr.getGeneratedSourceInfo(*secondMismatch)
          ->originalSourceRange.getEnd();
  return sourceMgr.isBeforeInBuffer(firstLocInLCA, secondLocInLCA) ||
    (allowEqual && firstLocInLCA == secondLocInLCA);
}

bool SourceManager::isBefore(SourceLoc first, SourceLoc second) const {
  return isBeforeInSource(*this, first, second, /*allowEqual=*/false);
}

bool SourceManager::isAtOrBefore(SourceLoc first, SourceLoc second) const {
  return isBeforeInSource(*this, first, second, /*allowEqual=*/true);
}

bool SourceManager::containsTokenLoc(SourceRange range, SourceLoc loc) const {
  return isAtOrBefore(range.Start, loc) && isAtOrBefore(loc, range.End);
}

bool SourceManager::containsLoc(SourceRange range, SourceLoc loc) const {
  return isAtOrBefore(range.Start, loc) && isBefore(loc, range.End);
}

bool SourceManager::encloses(SourceRange enclosing, SourceRange inner) const {
  return containsLoc(enclosing, inner.Start) &&
      isAtOrBefore(inner.End, enclosing.End);
}
