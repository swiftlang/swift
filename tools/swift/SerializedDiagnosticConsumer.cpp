//===- SerializedDiagnosticConsumer.cpp - Serialize Diagnostics --*- C++ -*-===//
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
//  This file implements the SerializedDiagnosticConsumer class.
//
//===----------------------------------------------------------------------===//

#include "SerializedDiagnosticConsumer.h"
#include "swift/Basic/LLVM.h"
#include "swift/Parse/Lexer.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/Twine.h"
#include "llvm/Support/SourceMgr.h"

using namespace swift;

void
SerializedDiagnosticConsumer::handleDiagnostic(llvm::SourceMgr &SM,
                                               SourceLoc Loc,
                                               DiagnosticKind Kind,
                                               llvm::StringRef Text,
                                               const DiagnosticInfo &Info) {
  // Implement.
}

