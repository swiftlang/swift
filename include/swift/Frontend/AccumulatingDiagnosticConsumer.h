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
    addDiagnostic(SM, Info);

    for (auto ChildInfo : Info.ChildDiagnosticInfo) {
      addDiagnostic(SM, *ChildInfo);
    }
  }

  // TODO: Support Swift-style diagnostic formatting
  void addDiagnostic(SourceManager &SM, const DiagnosticInfo &Info) {
    // Determine what kind of diagnostic we're emitting.
    llvm::SourceMgr::DiagKind SMKind;
    switch (Info.Kind) {
    case DiagnosticKind::Error:
      SMKind = llvm::SourceMgr::DK_Error;
      break;
    case DiagnosticKind::Warning:
      SMKind = llvm::SourceMgr::DK_Warning;
      break;

    case DiagnosticKind::Note:
      SMKind = llvm::SourceMgr::DK_Note;
      break;

    case DiagnosticKind::Remark:
      SMKind = llvm::SourceMgr::DK_Remark;
      break;
    }

    // Translate ranges.
    SmallVector<llvm::SMRange, 2> Ranges;
    for (auto R : Info.Ranges)
      Ranges.push_back(getRawRange(SM, R));

    // Translate fix-its.
    SmallVector<llvm::SMFixIt, 2> FixIts;
    for (DiagnosticInfo::FixIt F : Info.FixIts)
      FixIts.push_back(getRawFixIt(SM, F));

    // Actually substitute the diagnostic arguments into the diagnostic text.
    llvm::SmallString<256> Text;
    {
      llvm::raw_svector_ostream Out(Text);
      DiagnosticEngine::formatDiagnosticText(Out, Info.FormatString,
                                             Info.FormatArgs);
    }

    const llvm::SourceMgr &rawSM = SM.getLLVMSourceMgr();
    auto Msg = SM.GetMessage(Info.Loc, SMKind, Text, Ranges, FixIts);
    std::string result;
    llvm::raw_string_ostream os(result);
    rawSM.PrintMessage(os, Msg, false);
    os.flush();
    Diagnostics.push_back(result);
  }
};
}
#endif
