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
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/SourceMgr.h"
using namespace swift;

void SourceLoc::print(raw_ostream &OS, const llvm::SourceMgr &SM,
                      int &LastBuffer, int &LastLine) const {
  if (isInvalid()) {
    OS << "<invalid loc>";
    return;
  }
  int BufferIndex = SM.FindBufferContainingLoc(Value);
  if (BufferIndex == -1) {
    OS << "<malformed loc>";
    return;
  }

  auto LineAndCol = SM.getLineAndColumn(Value, BufferIndex);

  if (BufferIndex != LastBuffer) {
    OS << SM.getMemoryBuffer((unsigned)BufferIndex)->getBufferIdentifier();
    LastBuffer = BufferIndex;
    LastLine = LineAndCol.first;
    OS << ':' << LineAndCol.first << ':' << LineAndCol.second;
  } else if (LineAndCol.first != unsigned(LastLine)) {
    LastLine = LineAndCol.first;
    OS << "line:" << LineAndCol.first << ':' << LineAndCol.second;
  } else {
    OS << "col:" << LineAndCol.second;
  }
}

void SourceLoc::dump(const llvm::SourceMgr &SM) const {
  print(llvm::errs(), SM);
}

void SourceRange::print(raw_ostream &OS, const llvm::SourceMgr &SM,
                        int &LastBuffer, int &LastLine, bool PrintText) const {
  OS << '[';
  Start.print(OS, SM, LastBuffer, LastLine);
  OS << " - ";
  End.print(OS, SM, LastBuffer, LastLine);
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

void SourceRange::dump(const llvm::SourceMgr &SM) const {
  print(llvm::errs(), SM);
}

