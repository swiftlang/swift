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

void DiagnosticEngine_diagnose(
    DiagnosticEngine &engine, SourceLoc loc, BridgedDiagID bridgedDiagID,
    BridgedArrayRef /*DiagnosticArgument*/ bridgedArguments,
    CharSourceRange highlight,
    BridgedArrayRef /*DiagnosticInfo::FixIt*/ bridgedFixIts) {

  auto diagID = static_cast<DiagID>(bridgedDiagID);
  SmallVector<DiagnosticArgument, 2> arguments;
  for (auto arg : getArrayRef<DiagnosticArgument>(bridgedArguments)) {
    arguments.push_back(arg);
  }
  auto inflight = engine.diagnose(loc, diagID, arguments);

  // Add highlight.
  if (highlight.isValid()) {
    inflight.highlightChars(highlight.getStart(), highlight.getEnd());
  }

  // Add fix-its.
  for (auto fixIt : getArrayRef<DiagnosticInfo::FixIt>(bridgedFixIts)) {
    auto range = fixIt.getRange();
    auto text = fixIt.getText();
    inflight.fixItReplaceChars(range.getStart(), range.getEnd(), text);
  }
}
