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
#include "swift/Basic/DiagnosticConsumer.h"
#include "swift/Basic/LLVM.h"
#include "swift/Parse/Lexer.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/Twine.h"
#include "llvm/ADT/OwningPtr.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/SourceMgr.h"

using namespace swift;

namespace {
/// \brief Diagnostic consumer that serializes diagnostics to a stream.
class SerializedDiagnosticConsumer : public DiagnosticConsumer {
  llvm::OwningPtr<raw_ostream> OS;
public:
  SerializedDiagnosticConsumer(raw_ostream *OS) : OS(OS) {}

  virtual void handleDiagnostic(llvm::SourceMgr &SM, SourceLoc Loc,
                                DiagnosticKind Kind, llvm::StringRef Text,
                                const DiagnosticInfo &Info);
};
}

namespace swift { namespace serialized_diagnostics {
  DiagnosticConsumer *createConsumer(llvm::raw_ostream *OS) {
    return new SerializedDiagnosticConsumer(OS);
  }
}}

void
SerializedDiagnosticConsumer::handleDiagnostic(llvm::SourceMgr &SM,
                                               SourceLoc Loc,
                                               DiagnosticKind Kind,
                                               llvm::StringRef Text,
                                               const DiagnosticInfo &Info) {
  // Implement.
}

