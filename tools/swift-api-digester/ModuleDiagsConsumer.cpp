//===--- ModuleDiagsConsumer.cpp - Print module differ diagnostics --*- C++ -*-===//
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
//  This file implements the ModuleDifferDiagsConsumer class, which displays
//  diagnostics from the module differ as text to an output.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/DiagnosticsModuleDiffer.h"
#include "ModuleDiagsConsumer.h"

void swift::ide::api::
ModuleDifferDiagsConsumer::handleDiagnostic(SourceManager &SM, SourceLoc Loc,
                        DiagnosticKind Kind,
                        StringRef FormatString,
                        ArrayRef<DiagnosticArgument> FormatArgs,
                        const DiagnosticInfo &Info) {
  PrintingDiagnosticConsumer::handleDiagnostic(SM, Loc, Kind, FormatString,
    FormatArgs, Info);
}
