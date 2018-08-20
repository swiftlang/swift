//===--- PrintingDiagnosticConsumer.cpp - Print Text Diagnostics ----------===//
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
//
//  This file implements the PrintingDiagnosticConsumer class.
//
//===----------------------------------------------------------------------===//

#include "swift/Frontend/PrintingDiagnosticConsumer.h"
#include "swift/Basic/LLVM.h"
#include "swift/Basic/SourceManager.h"
#include "swift/AST/DiagnosticEngine.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/Twine.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/raw_ostream.h"

using namespace swift;

namespace {
  class ColoredStream : public raw_ostream {
    raw_ostream &Underlying;
  public:
    explicit ColoredStream(raw_ostream &underlying) : Underlying(underlying) {}
    ~ColoredStream() override { flush(); }

    raw_ostream &changeColor(Colors color, bool bold = false,
                             bool bg = false) override {
      Underlying.changeColor(color, bold, bg);
      return *this;
    }
    raw_ostream &resetColor() override {
      Underlying.resetColor();
      return *this;
    }
    raw_ostream &reverseColor() override {
      Underlying.reverseColor();
      return *this;
    }
    bool has_colors() const override {
      return true;
    }

    void write_impl(const char *ptr, size_t size) override {
      Underlying.write(ptr, size);
    }
    uint64_t current_pos() const override {
      return Underlying.tell() - GetNumBytesInBuffer();
    }

    size_t preferred_buffer_size() const override {
      return 0;
    }
  };
} // end anonymous namespace

void PrintingDiagnosticConsumer::handleDiagnostic(
    SourceManager &SM, SourceLoc Loc, DiagnosticKind Kind,
    StringRef FormatString, ArrayRef<DiagnosticArgument> FormatArgs,
    const DiagnosticInfo &Info) {
  // Determine what kind of diagnostic we're emitting.
  llvm::SourceMgr::DiagKind SMKind;
  switch (Kind) {
    case DiagnosticKind::Error:
      SMKind = llvm::SourceMgr::DK_Error;
      break;
    case DiagnosticKind::Warning: 
      SMKind = llvm::SourceMgr::DK_Warning; 
      break;

    case DiagnosticKind::Note:
      SMKind = llvm::SourceMgr::DK_Note;
      break;

    case DiagnosticKind::Remark:
      SMKind = llvm::SourceMgr::DK_Remark;
      break;
  }

  if (Kind == DiagnosticKind::Error) {
    DidErrorOccur = true;
  }
  
  // Translate ranges.
  SmallVector<llvm::SMRange, 2> Ranges;
  for (auto R : Info.Ranges)
    Ranges.push_back(getRawRange(SM, R));

  // Translate fix-its.
  SmallVector<llvm::SMFixIt, 2> FixIts;
  for (DiagnosticInfo::FixIt F : Info.FixIts)
    FixIts.push_back(getRawFixIt(SM, F));

  // Display the diagnostic.
  ColoredStream coloredErrs{Stream};
  raw_ostream &out = ForceColors ? coloredErrs : Stream;
  const llvm::SourceMgr &rawSM = SM.getLLVMSourceMgr();
  
  // Actually substitute the diagnostic arguments into the diagnostic text.
  llvm::SmallString<256> Text;
  {
    llvm::raw_svector_ostream Out(Text);
    DiagnosticEngine::formatDiagnosticText(Out, FormatString, FormatArgs);
  }
  
  auto Msg = SM.GetMessage(Loc, SMKind, Text, Ranges, FixIts);
  rawSM.PrintMessage(out, Msg);
}

llvm::SMDiagnostic
SourceManager::GetMessage(SourceLoc Loc, llvm::SourceMgr::DiagKind Kind,
                          const Twine &Msg,
                          ArrayRef<llvm::SMRange> Ranges,
                          ArrayRef<llvm::SMFixIt> FixIts) const {

  // First thing to do: find the current buffer containing the specified
  // location to pull out the source line.
  SmallVector<std::pair<unsigned, unsigned>, 4> ColRanges;
  std::pair<unsigned, unsigned> LineAndCol;
  StringRef BufferID = "<unknown>";
  std::string LineStr;

  if (Loc.isValid()) {
    BufferID = getDisplayNameForLoc(Loc);
    auto CurMB = LLVMSourceMgr.getMemoryBuffer(findBufferContainingLoc(Loc));

    // Scan backward to find the start of the line.
    const char *LineStart = Loc.Value.getPointer();
    const char *BufStart = CurMB->getBufferStart();
    while (LineStart != BufStart && LineStart[-1] != '\n' &&
           LineStart[-1] != '\r')
      --LineStart;

    // Get the end of the line.
    const char *LineEnd = Loc.Value.getPointer();
    const char *BufEnd = CurMB->getBufferEnd();
    while (LineEnd != BufEnd && LineEnd[0] != '\n' && LineEnd[0] != '\r')
      ++LineEnd;
    LineStr = std::string(LineStart, LineEnd);

    // Convert any ranges to column ranges that only intersect the line of the
    // location.
    for (unsigned i = 0, e = Ranges.size(); i != e; ++i) {
      llvm::SMRange R = Ranges[i];
      if (!R.isValid()) continue;

      // If the line doesn't contain any part of the range, then ignore it.
      if (R.Start.getPointer() > LineEnd || R.End.getPointer() < LineStart)
        continue;

      // Ignore pieces of the range that go onto other lines.
      if (R.Start.getPointer() < LineStart)
        R.Start = llvm::SMLoc::getFromPointer(LineStart);
      if (R.End.getPointer() > LineEnd)
        R.End = llvm::SMLoc::getFromPointer(LineEnd);

      // Translate from SMLoc ranges to column ranges.
      // FIXME: Handle multibyte characters.
      ColRanges.push_back(std::make_pair(R.Start.getPointer()-LineStart,
                                         R.End.getPointer()-LineStart));
    }

    LineAndCol = getLineAndColumn(Loc);
  }

  return llvm::SMDiagnostic(LLVMSourceMgr, Loc.Value, BufferID,
                            LineAndCol.first,
                            LineAndCol.second-1, Kind, Msg.str(),
                            LineStr, ColRanges, FixIts);
}

