//===- PrintingDiagnosticConsumer.cpp - Print Text Diagnostics---*- C++ -*-===//
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
//  This file defines the PrintingDiagnosticConsumer class, which displays
//  diagnostics as text to a terminal.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_PRINTINGDIAGNOSTICCONSUMER_H
#define SWIFT_PRINTINGDIAGNOSTICCONSUMER_H

#include "swift/Basic/DiagnosticConsumer.h"

namespace swift {

/// \brief Diagnostic consumer that displays diagnostics to standard error.
class PrintingDiagnosticConsumer : public DiagnosticConsumer {
public:
  virtual void handleDiagnostic(llvm::SourceMgr &SM, SourceLoc Loc,
                                DiagnosticKind Kind, llvm::StringRef Text,
                                const DiagnosticInfo &Info);
};
  
}

#endif
