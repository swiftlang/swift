//===--- ASTBridging.cpp - AST bridging functions -------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/AST/ASTBridging.h"

#include "swift/AST/DiagnosticEngine.h"
#include "swift/Basic/BridgingUtils.h"

using namespace swift;

namespace {
/// BridgedDiagEngine -> DiagnosticEngine *.
DiagnosticEngine *getDiagnosticEngine(const BridgedDiagEngine &bridged) {
  return static_cast<DiagnosticEngine *>(bridged.object);
}

} // namespace

static_assert(sizeof(BridgedDiagnosticArgument) >= sizeof(DiagnosticArgument),
              "BridgedDiagnosticArgument has wrong size");

BridgedDiagnosticArgument::BridgedDiagnosticArgument(SwiftInt i)
  : BridgedDiagnosticArgument(DiagnosticArgument((int)i)) {}

BridgedDiagnosticArgument::BridgedDiagnosticArgument(BridgedStringRef s)
  : BridgedDiagnosticArgument(DiagnosticArgument(s.get())) {}

static_assert(sizeof(BridgedDiagnosticFixIt) >= sizeof(DiagnosticInfo::FixIt),
              "BridgedDiagnosticFixIt has wrong size");

static SourceLoc getSourceLoc(BridgedSourceLoc bridgedLoc) {
  return SourceLoc(llvm::SMLoc::getFromPointer(bridgedLoc.getLoc()));
}

BridgedDiagnosticFixIt::BridgedDiagnosticFixIt(BridgedSourceLoc start, uint32_t length, BridgedStringRef text)
  : BridgedDiagnosticFixIt(DiagnosticInfo::FixIt(
      CharSourceRange(getSourceLoc(start), length),
      text.get(),
      llvm::ArrayRef<DiagnosticArgument>())) {}

void DiagnosticEngine_diagnose(
    BridgedDiagEngine bridgedEngine, BridgedSourceLoc loc,
    BridgedDiagID bridgedDiagID,
    BridgedArrayRef /*BridgedDiagnosticArgument*/ bridgedArguments,
    BridgedSourceLoc highlightStart, uint32_t hightlightLength,
    BridgedArrayRef /*BridgedDiagnosticFixIt*/ bridgedFixIts) {
  auto *D = getDiagnosticEngine(bridgedEngine);

  auto diagID = static_cast<DiagID>(bridgedDiagID);
  SmallVector<DiagnosticArgument, 2> arguments;
  for (auto arg : getArrayRef<BridgedDiagnosticArgument>(bridgedArguments)) {
    arguments.push_back(arg.get());
  }
  auto inflight = D->diagnose(SourceLoc(llvm::SMLoc::getFromPointer(loc.getLoc())), diagID, arguments);

  // Add highlight.
  if (highlightStart.isValid()) {
    CharSourceRange highlight(getSourceLoc(highlightStart), (unsigned)hightlightLength);
    inflight.highlightChars(highlight.getStart(), highlight.getEnd());
  }

  // Add fix-its.
  for (const BridgedDiagnosticFixIt &fixIt : getArrayRef<BridgedDiagnosticFixIt>(bridgedFixIts)) {
    auto range = fixIt.get().getRange();
    auto text = fixIt.get().getText();
    inflight.fixItReplaceChars(range.getStart(), range.getEnd(), text);
  }
}

bool DiagnosticEngine_hadAnyError(BridgedDiagEngine bridgedEngine) {
  auto *D = getDiagnosticEngine(bridgedEngine);
  return D->hadAnyError();
}
