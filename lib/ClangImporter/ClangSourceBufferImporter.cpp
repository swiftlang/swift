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
#include "swift/Basic/Assertions.h"
#include "swift/Basic/SourceManager.h"
#include "clang/Basic/SourceManager.h"
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

SourceLoc ClangSourceBufferImporter::resolveSourceLocation(
    const clang::SourceManager &clangSrcMgr,
    clang::SourceLocation clangLoc) {
  SourceLoc loc;

  clangLoc = clangSrcMgr.getFileLoc(clangLoc);
  auto decomposedLoc = clangSrcMgr.getDecomposedLoc(clangLoc);
  if (decomposedLoc.first.isInvalid())
    return loc;

  auto clangFileID = decomposedLoc.first;
  auto buffer = clangSrcMgr.getBufferOrFake(clangFileID);
  unsigned mirrorID;

  auto mirrorIter = mirroredBuffers.find(buffer.getBufferStart());
  if (mirrorIter != mirroredBuffers.end()) {
    mirrorID = mirrorIter->second;
  } else {
    StringRef bufIdent = buffer.getBufferIdentifier();
    std::unique_ptr<llvm::MemoryBuffer> mirrorBuffer{
        llvm::MemoryBuffer::getMemBuffer(buffer.getBuffer(), bufIdent,
                                         /*RequiresNullTerminator=*/true)};
    // The same underlying file can exist as multiple clang file IDs. E.g. as
    // part of the its own module, and then later loaded as an import in another
    // module. Don't deduplicate files like "<module-imports>" that have
    // different contents in different modules, despite the same name.
    auto IDOpt = swiftSourceManager.getIDForBufferIdentifier(bufIdent);
    if (IDOpt.has_value() && clangSrcMgr.getFileEntryForID(clangFileID)) {
      mirrorID = IDOpt.value();
    } else
      mirrorID = swiftSourceManager.addNewSourceBuffer(std::move(mirrorBuffer));
    mirroredBuffers[buffer.getBufferStart()] = mirrorID;
  }
  loc = swiftSourceManager.getLocForOffset(mirrorID, decomposedLoc.second);

  auto presumedLoc = clangSrcMgr.getPresumedLoc(clangLoc);
  if (!presumedLoc.getFilename())
    return loc;
  if (presumedLoc.getLine() == 0)
    return SourceLoc();

  unsigned bufferLineNumber =
    clangSrcMgr.getLineNumber(decomposedLoc.first, decomposedLoc.second);

  StringRef presumedFile = presumedLoc.getFilename();
  SourceLoc startOfLine = loc.getAdvancedLoc(-presumedLoc.getColumn() + 1);

  // FIXME: Virtual files can't actually model the EOF position correctly, so
  // if this virtual file would start at EOF, just hope the physical location
  // will do.
  if (startOfLine != swiftSourceManager.getRangeForBuffer(mirrorID).getEnd()) {
    bool isNewVirtualFile = swiftSourceManager.openVirtualFile(
        startOfLine, presumedFile, presumedLoc.getLine() - bufferLineNumber);
    if (isNewVirtualFile) {
      SourceLoc endOfLine = findEndOfLine(swiftSourceManager, loc, mirrorID);
      swiftSourceManager.closeVirtualFile(endOfLine);
    }
  }

  using SourceManagerRef = llvm::IntrusiveRefCntPtr<const clang::SourceManager>;
  auto iter = std::lower_bound(sourceManagersWithDiagnostics.begin(),
                               sourceManagersWithDiagnostics.end(),
                               &clangSrcMgr,
                               [](const SourceManagerRef &inArray,
                                  const clang::SourceManager *toInsert) {
    return std::less<const clang::SourceManager *>()(inArray.get(), toInsert);
  });
  if (iter == sourceManagersWithDiagnostics.end() ||
      iter->get() != &clangSrcMgr) {
    sourceManagersWithDiagnostics.insert(iter, &clangSrcMgr);
  }

  return loc;
}
