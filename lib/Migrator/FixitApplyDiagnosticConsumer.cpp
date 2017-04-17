//===--- FixitApplyDiagnosticConsumer.cpp -----------------------*- C++ -*-===//
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

#include "swift/Basic/SourceManager.h"
#include "swift/Frontend/Frontend.h"
#include "swift/Migrator/FixitApplyDiagnosticConsumer.h"
#include "swift/Migrator/Migrator.h"
#include "swift/Migrator/MigratorOptions.h"
#include "swift/AST/DiagnosticsSema.h"

using namespace swift;
using namespace swift::migrator;

FixitApplyDiagnosticConsumer::
FixitApplyDiagnosticConsumer(const MigratorOptions &MigratorOpts,
                             const StringRef Text,
                             const StringRef BufferName)
  : MigratorOpts(MigratorOpts),
    Text(Text), BufferName(BufferName), NumFixitsApplied(0) {
  RewriteBuf.Initialize(Text);
}

void FixitApplyDiagnosticConsumer::printResult(llvm::raw_ostream &OS) const {
  RewriteBuf.write(OS);
}

bool FixitApplyDiagnosticConsumer::
shouldTakeFixit(const DiagnosticInfo &Info,
                const DiagnosticInfo::FixIt &F) const {

  if (MigratorOpts.AddObjC) {
    if (Info.ID == diag::objc_inference_swift3_addobjc.ID) {
      return true;
    }
  }

  return false;
}

void FixitApplyDiagnosticConsumer::
handleDiagnostic(SourceManager &SM, SourceLoc Loc,
                 DiagnosticKind Kind,
                 StringRef FormatString,
                 ArrayRef<DiagnosticArgument> FormatArgs,
                 const DiagnosticInfo &Info) {
  auto ThisBufferID = SM.findBufferContainingLoc(Loc);
  auto ThisBufferName = SM.getIdentifierForBuffer(ThisBufferID);
  if (ThisBufferName != BufferName) {
    return;
  }

  for (const auto &Fixit : Info.FixIts) {
    if (!shouldTakeFixit(Info, Fixit)) {
      continue;
    }

    auto Offset = SM.getLocOffsetInBuffer(Fixit.getRange().getStart(),
                                          ThisBufferID);
    auto Length = Fixit.getRange().getByteLength();

    RewriteBuf.ReplaceText(Offset, Length, Fixit.getText());
    ++NumFixitsApplied;

    Replacements.push_back({
      BufferName, Offset, Length, Fixit.getText().str(), Info.ID
    });
  }
}
