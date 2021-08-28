//===--- ClangSourceBufferImporter.cpp - Map Clang buffers to Swift -------===//
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

#include "ClangSourceBufferImporter.h"
#include "swift/Basic/SourceManager.h"
#include "clang/Basic/SourceManager.h"
#include "clang/Basic/SourceManagerInternals.h"
#include "llvm/Support/MemoryBuffer.h"

using namespace swift;
using namespace swift::importer;

static SourceLoc findEndOfLine(SourceManager &SM, SourceLoc loc,
                               unsigned bufferID) {
  CharSourceRange entireBuffer = SM.getRangeForBuffer(bufferID);
  CharSourceRange rangeFromLoc{SM, loc, entireBuffer.getEnd()};
  StringRef textFromLoc = SM.extractText(rangeFromLoc);
  size_t newlineOffset = textFromLoc.find_first_of({"\r\n\0", 3});
  if (newlineOffset == StringRef::npos)
    return entireBuffer.getEnd();
  return loc.getAdvancedLoc(newlineOffset);
}

SourceLoc ClangSourceBufferImporter::importSourceLoc(
    const clang::SourceManager &clangSourceManager,
    clang::SourceLocation clangLoc, bool forDiagnostics) {
  // Map the location to one in an actual file (as opposed to within a macro)
  clangLoc = clangSourceManager.getFileLoc(clangLoc);

  clang::FileID clangFileID;
  unsigned offset;
  std::tie(clangFileID, offset) =
      clangSourceManager.getDecomposedLoc(clangLoc);
  if (clangFileID.isInvalid())
    return SourceLoc();

  llvm::Optional<llvm::MemoryBufferRef> buffer =
      clangSourceManager.getBufferOrNone(clangFileID);
  if (!buffer)
    return SourceLoc();

  // Grab the already mapped buffer or add a new one (without copying) if it
  // hasn't been added before
  unsigned mirrorID;
  auto mirrorIter = mirroredBuffers.find(buffer->getBufferStart());
  bool allDirectives = false;
  if (mirrorIter != mirroredBuffers.end()) {
    mirrorID = mirrorIter->second.FileID;
    allDirectives = mirrorIter->second.Complete;
  } else {
    std::unique_ptr<llvm::MemoryBuffer> mirrorBuffer{
      llvm::MemoryBuffer::getMemBuffer(buffer->getBuffer(),
                                       buffer->getBufferIdentifier(),
                                       /*RequiresNullTerminator=*/true)
    };
    mirrorID = swiftSourceManager.addNewSourceBuffer(std::move(mirrorBuffer));
    mirroredBuffers[buffer->getBufferStart()] = { mirrorID, !forDiagnostics };

    // Make sure to keep a reference to the underlying manager
    if (clangSourceManagers.insert(&clangSourceManager).second) {
      clangSourceManagerRefs.emplace_back(&clangSourceManager);
    }
  }

  SourceLoc loc = swiftSourceManager.getLocForOffset(mirrorID, offset);
  if (allDirectives)
    return swiftSourceManager.getLocForOffset(mirrorID, offset);

  // Now add any line directives, if any. Note that the line table will be
  // incomplete if a module is broken and diagnostics are output, but otherwise
  // we'll be reading from the serialized module and hence have the full line
  // table.
  //
  // Always add the enclosing line directive if importing a source location for
  // a diagnostic, but otherwise assume we have the full line table and add
  // all directives at once to avoid adding a virtual file for every location.

  bool Invalid = false;
  const clang::SrcMgr::SLocEntry &Entry =
      clangSourceManager.getSLocEntry(clangFileID, &Invalid);
  if (Invalid || !Entry.isFile())
    return loc;

  // TODO: The LineTable is obstensibly part of the "internals", should we be
  //       relying on it here?
  if (!clangSourceManager.hasLineTable() ||
      !Entry.getFile().hasLineDirectives())
    return loc;

  clang::LineTableInfo &lineTable =
      *clangSourceManager.getCurrentLineTable();

  ArrayRef<clang::LineEntry> lineEntries;
  if (!forDiagnostics) {
    mirroredBuffers[buffer->getBufferStart()].Complete = true;

    lineEntries = lineTable.entries(clangFileID);
    if (lineEntries.empty())
      return loc;
  } else {
    const clang::LineEntry *entry =
        lineTable.FindNearestLineEntry(clangFileID, offset);
    if (!entry)
      return loc;
    lineEntries = llvm::makeArrayRef(*entry);
  }

  for (const clang::LineEntry &entry : lineEntries) {
    SourceLoc fileLoc =
        swiftSourceManager.getLocForOffset(mirrorID, entry.FileOffset);
    const SourceManager::VirtualFile *file = swiftSourceManager.getVirtualFile(fileLoc);
    if (file) {
      if (file->Range.getStart() == fileLoc)
        continue;
      swiftSourceManager.closeVirtualFile(fileLoc);
    }

    StringRef fileName = lineTable.getFilename(entry.FilenameID);
    unsigned line =
        swiftSourceManager.getLineAndColumnInBuffer(fileLoc).first;
    swiftSourceManager.openVirtualFile(fileLoc, fileName,
                                       entry.LineNo - line - 1);
  }

  return loc;
}
