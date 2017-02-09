//===--- DiagnosticConsumer.cpp - Diagnostic Consumer Impl ----------------===//
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
//  This file implements the DiagnosticConsumer class.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "swift-basic"
#include "swift/Basic/DiagnosticConsumer.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"
using namespace swift;

DiagnosticConsumer::~DiagnosticConsumer() { }

void NullDiagnosticConsumer::handleDiagnostic(SourceManager &SM,
                                              SourceLoc Loc,
                                              DiagnosticKind Kind,
                                              StringRef Text,
                                              const DiagnosticInfo &Info) {
  DEBUG(llvm::dbgs() << "NullDiagnosticConsumer received diagnostic: "
                     << Text << "\n");
}
