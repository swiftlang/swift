//===--- SourceLoc.cpp - SourceLoc and SourceRange implementations --------===//
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

#include "swift/Basic/SourceLoc.h"
#include "swift/Basic/SourceManager.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/raw_ostream.h"

using namespace swift;

SourceLoc SourceManager::getCodeCompletionLoc() const {
  return getLocForBufferStart(CodeCompletionBufferID)
      .getAdvancedLoc(CodeCompletionOffset);
}

SourceLoc SourceManager::getLocForBufferStart(unsigned BufferID) const {
  auto *Buffer = LLVMSourceMgr.getMemoryBuffer(BufferID);
  return SourceLoc(llvm::SMLoc::getFromPointer(Buffer->getBufferStart()));
}

unsigned SourceManager::getLocOffsetInBuffer(SourceLoc Loc,
                                             unsigned BufferID) const {
  auto *Buffer = LLVMSourceMgr.getMemoryBuffer(BufferID);
  assert(Loc.Value.getPointer() >= Buffer->getBuffer().begin() &&
         Loc.Value.getPointer() <= Buffer->getBuffer().end() &&
         "Location is not from the specified buffer");
  return Loc.Value.getPointer() - Buffer->getBuffer().begin();
}

DecomposedLoc SourceManager::decompose(SourceLoc Loc) const {
  assert(Loc.isValid());

  unsigned BufferID =
      unsigned(LLVMSourceMgr.FindBufferContainingLoc(Loc.Value));
  assert(BufferID != ~0U);

  DecomposedLoc Result;
  Result.Buffer = LLVMSourceMgr.getMemoryBuffer(BufferID);
  std::tie(Result.Line, Result.Column) =
      LLVMSourceMgr.getLineAndColumn(Loc.Value, BufferID);

  return Result;
}

void SourceLoc::printLineAndColumn(raw_ostream &OS,
                                   const SourceManager &SM) const {
  if (isInvalid()) {
    OS << "<invalid loc>";
    return;
  }

  auto LineAndCol = SM.getLineAndColumn(*this);
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
    OS << SM->getMemoryBuffer(BufferID)->getBufferIdentifier();
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
  OS << '[';
  Start.print(OS, SM, LastBufferID);
  OS << " - ";
  End.print(OS, SM, LastBufferID);
  OS << ']';
  
  if (Start.isInvalid() || End.isInvalid())
    return;
  
  if (PrintText) {
    OS << " RangeText=\""
       << StringRef(Start.Value.getPointer(),
                    End.Value.getPointer() - Start.Value.getPointer()+1)
       << '"';
  }
}

void SourceRange::dump(const SourceManager &SM) const {
  print(llvm::errs(), SM);
}

