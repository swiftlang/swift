//===- PrintingDiagnosticConsumer.cpp - Print Text Diagnostics ------------===//
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
//
//  This file implements the PrintingDiagnosticConsumer class.
//
//===----------------------------------------------------------------------===//

#include "swift/Frontend/PrintingDiagnosticConsumer.h"
#include "swift/Basic/LLVM.h"
#include "swift/Basic/SourceManager.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/Twine.h"
#include "llvm/Support/raw_ostream.h"

using namespace swift;

namespace {
  class ColoredStream : public raw_ostream {
    raw_ostream &Underlying;
  public:
    explicit ColoredStream(raw_ostream &underlying) : Underlying(underlying) {}
    ~ColoredStream() { flush(); }

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

void
PrintingDiagnosticConsumer::handleDiagnostic(SourceManager &SM, SourceLoc Loc,
                                             DiagnosticKind Kind, 
                                             StringRef Text,
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
  ColoredStream coloredErrs{llvm::errs()};
  raw_ostream &out = ForceColors ? coloredErrs : llvm::errs();
  SM->PrintMessage(out, getRawLoc(Loc), SMKind, Text, Ranges, FixIts);
}

