//===--- FixitApplyDiagnosticConsumer.cpp ---------------------------------===//
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
FixitApplyDiagnosticConsumer(const StringRef Text,
                             const StringRef BufferName)
  : Text(Text), BufferName(BufferName), NumFixitsApplied(0) {
  RewriteBuf.Initialize(Text);
}

void FixitApplyDiagnosticConsumer::printResult(llvm::raw_ostream &OS) const {
  RewriteBuf.write(OS);
}

void FixitApplyDiagnosticConsumer::
handleDiagnostic(SourceManager &SM, SourceLoc Loc,
                 DiagnosticKind Kind,
                 StringRef FormatString,
                 ArrayRef<DiagnosticArgument> FormatArgs,
                 const DiagnosticInfo &Info) {
  if (Loc.isInvalid()) {
    return;
  }
  auto ThisBufferID = SM.findBufferContainingLoc(Loc);
  auto ThisBufferName = SM.getIdentifierForBuffer(ThisBufferID);
  if (ThisBufferName != BufferName) {
    return;
  }

  if (!shouldTakeFixit(Kind, Info)) {
    return;
  }

  for (const auto &Fixit : Info.FixIts) {

    auto Offset = SM.getLocOffsetInBuffer(Fixit.getRange().getStart(),
                                          ThisBufferID);
    auto Length = Fixit.getRange().getByteLength();
    auto Text = Fixit.getText();

    // Ignore meaningless Fix-its.
    if (Length == 0 && Text.size() == 0)
      return;

    // Ignore pre-applied equivalents.
    Replacement R { Offset, Length, Text };
    if (Replacements.count(R)) {
      return;
    } else {
      Replacements.insert(R);
    }

    if (Length == 0) {
      RewriteBuf.InsertText(Offset, Text);
    } else if (Text.size() == 0) {
      RewriteBuf.RemoveText(Offset, Length);
    } else {
      RewriteBuf.ReplaceText(Offset, Length, Text);
    }
    ++NumFixitsApplied;
  }
}
