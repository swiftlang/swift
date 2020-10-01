//===--- PrintingDiagnosticConsumer.h - Print Text Diagnostics --*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  This file defines the PrintingDiagnosticConsumer class, which displays
//  diagnostics as text to a terminal.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_PRINTINGDIAGNOSTICCONSUMER_H
#define SWIFT_PRINTINGDIAGNOSTICCONSUMER_H

#include "swift/AST/DiagnosticConsumer.h"
#include "swift/Basic/DiagnosticOptions.h"
#include "swift/Basic/LLVM.h"

#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/Process.h"

namespace swift {
class AnnotatedSourceSnippet;

/// Diagnostic consumer that displays diagnostics to standard error.
class PrintingDiagnosticConsumer : public DiagnosticConsumer {
  llvm::raw_ostream &Stream;
  bool ForceColors = false;
  bool PrintEducationalNotes = false;
  bool DidErrorOccur = false;
  DiagnosticOptions::FormattingStyle FormattingStyle =
      DiagnosticOptions::FormattingStyle::LLVM;
  // The current snippet used to display an error/warning/remark and the notes
  // implicitly associated with it. Uses `std::unique_ptr` so that
  // `AnnotatedSourceSnippet` can be forward declared.
  std::unique_ptr<AnnotatedSourceSnippet> currentSnippet;
  // Educational notes which are buffered until the consumer is finished
  // constructing a snippet.
  SmallVector<std::string, 1> BufferedEducationalNotes;
  bool SuppressOutput = false;

public:
  PrintingDiagnosticConsumer(llvm::raw_ostream &stream = llvm::errs());
  ~PrintingDiagnosticConsumer();

  virtual void handleDiagnostic(SourceManager &SM,
                                const DiagnosticInfo &Info) override;

  virtual bool finishProcessing() override;

  void flush(bool includeTrailingBreak);

  virtual void flush() override { flush(false); }

  void forceColors() {
    ForceColors = true;
    llvm::sys::Process::UseANSIEscapeCodes(true);
  }

  void setPrintEducationalNotes(bool ShouldPrint) {
    PrintEducationalNotes = ShouldPrint;
  }

  void setFormattingStyle(DiagnosticOptions::FormattingStyle style) {
    FormattingStyle = style;
  }

  bool didErrorOccur() {
    return DidErrorOccur;
  }

  void setSuppressOutput(bool suppressOutput) {
    SuppressOutput = suppressOutput;
  }

private:
  void printDiagnostic(SourceManager &SM, const DiagnosticInfo &Info);
};
  
}

#endif
