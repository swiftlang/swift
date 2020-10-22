//===- AccumulatingDiagnosticConsumer.h - Collect Text Diagnostics  C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  This file defines the AccumulatingDiagnosticConsumer class, which collects
//  all emitted diagnostics into an externally-owned collection.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_ACCUMULATINGDIAGNOSTICCONSUMER_H
#define SWIFT_ACCUMULATINGDIAGNOSTICCONSUMER_H

#include "swift/AST/DiagnosticConsumer.h"
#include "swift/Basic/DiagnosticOptions.h"
#include "swift/Basic/LLVM.h"

#include <string>
#include <sstream>

namespace swift {
/// Diagnostic consumer that simply collects all emitted diagnostics into the provided
/// collection.
class AccumulatingFileDiagnosticConsumer : public DiagnosticConsumer {
  std::vector<std::string> &Diagnostics;
public:
  AccumulatingFileDiagnosticConsumer(std::vector<std::string> &DiagBuffer)
    : Diagnostics(DiagBuffer) {}

private:
  void handleDiagnostic(SourceManager &SM,
                        const DiagnosticInfo &Info) override {
    std::string DiagMsg;
    llvm::raw_string_ostream DiagOS(DiagMsg);
    DiagnosticEngine::formatDiagnosticText(DiagOS, Info.FormatString,
                                           Info.FormatArgs);
    auto LC = SM.getPresumedLineAndColumnForLoc(Info.Loc);
    std::ostringstream StrOS;
    StrOS << LC.first << ", " << LC.second << ": " << DiagOS.str();
    Diagnostics.push_back(StrOS.str());
  }
};
}
#endif
