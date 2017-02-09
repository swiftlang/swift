//===--- PrintingDiagnosticConsumer.h - Print Text Diagnostics --*- C++ -*-===//
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
//  This file defines the PrintingDiagnosticConsumer class, which displays
//  diagnostics as text to a terminal.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_PRINTINGDIAGNOSTICCONSUMER_H
#define SWIFT_PRINTINGDIAGNOSTICCONSUMER_H

#include "swift/Basic/LLVM.h"
#include "swift/Basic/DiagnosticConsumer.h"

#include "llvm/Support/raw_ostream.h"

namespace swift {

/// \brief Diagnostic consumer that displays diagnostics to standard error.
class PrintingDiagnosticConsumer : public DiagnosticConsumer {
  llvm::raw_ostream &Stream;
  bool ForceColors = false;
  bool DidErrorOccur = false;
public:
  PrintingDiagnosticConsumer(llvm::raw_ostream &stream = llvm::errs()) :
    Stream(stream) { }

  virtual void handleDiagnostic(SourceManager &SM, SourceLoc Loc,
                                DiagnosticKind Kind, StringRef Text,
                                const DiagnosticInfo &Info) override;

  void forceColors() {
    ForceColors = true;
  }

  bool didErrorOccur() {
    return DidErrorOccur;
  }
};
  
}

#endif
