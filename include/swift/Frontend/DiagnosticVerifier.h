//===--- DiagnosticVerifier.h - Diagnostic Verifier (-verify) ---*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file exposes support for the diagnostic verifier, which is used to
// implement -verify mode in the compiler.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_FRONTEND_DIAGNOSTIC_VERIFIER_H
#define SWIFT_FRONTEND_DIAGNOSTIC_VERIFIER_H

#include "swift/AST/DiagnosticConsumer.h"
#include "swift/Basic/LLVM.h"

namespace swift {
struct ExpectedFixIt;

struct CapturedDiagnosticInfo {
  llvm::SmallString<128> Message;
  llvm::SmallString<32> FileName;
  DiagnosticKind Classification;
  SourceLoc Loc;
  unsigned Line;
  unsigned Column;
  SmallVector<DiagnosticInfo::FixIt, 2> FixIts;
};
/// This class implements support for -verify mode in the compiler.  It
/// buffers up diagnostics produced during compilation, then checks them
/// against expected-error markers in the source file.
class DiagnosticVerifier : public DiagnosticConsumer {
  SourceManager &SM;
  std::vector<CapturedDiagnosticInfo> CapturedDiagnostics;
  ArrayRef<unsigned> BufferIDs;
  bool AutoApplyFixes;
  bool IgnoreUnknown;

public:
  explicit DiagnosticVerifier(SourceManager &SM, ArrayRef<unsigned> BufferIDs,
                              bool AutoApplyFixes, bool IgnoreUnknown)
      : SM(SM), BufferIDs(BufferIDs), AutoApplyFixes(AutoApplyFixes),
        IgnoreUnknown(IgnoreUnknown) {}

  virtual void handleDiagnostic(SourceManager &SM,
                                const DiagnosticInfo &Info) override;

  virtual bool finishProcessing() override;

private:
  /// verifyFile - After the file has been processed, check to see if we
  /// got all of the expected diagnostics and check to see if there were any
  /// unexpected ones.
  bool verifyFile(unsigned BufferID);


  bool checkForFixIt(const ExpectedFixIt &Expected,
                     const CapturedDiagnosticInfo &D, StringRef buffer);

  // Render the verifier syntax for a given set of fix-its.
  std::string renderFixits(ArrayRef<DiagnosticInfo::FixIt> fixits,
                           StringRef InputFile);
};
}

#endif
