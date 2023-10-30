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

#ifdef PURE_BRIDGING_MODE
// In PURE_BRIDGING_MODE, bridging functions are not inlined and therefore
// inluded in the cpp file.
#include "swift/AST/ASTBridgingImpl.h"
#endif

using namespace swift;

static_assert(sizeof(BridgedDiagnosticArgument) >= sizeof(DiagnosticArgument),
              "BridgedDiagnosticArgument has wrong size");

BridgedDiagnosticArgument::BridgedDiagnosticArgument(SwiftInt i)
  : BridgedDiagnosticArgument(DiagnosticArgument((int)i)) {}

BridgedDiagnosticArgument::BridgedDiagnosticArgument(BridgedStringRef s)
  : BridgedDiagnosticArgument(DiagnosticArgument(s.get())) {}

static_assert(sizeof(BridgedDiagnosticFixIt) >= sizeof(DiagnosticInfo::FixIt),
              "BridgedDiagnosticFixIt has wrong size");

BridgedDiagnosticFixIt::BridgedDiagnosticFixIt(BridgedSourceLoc start,
                                               uint32_t length,
                                               BridgedStringRef text)
    : BridgedDiagnosticFixIt(DiagnosticInfo::FixIt(
          CharSourceRange(start.get(), length), text.get(),
          llvm::ArrayRef<DiagnosticArgument>())) {}

void DiagnosticEngine_diagnose(
    BridgedDiagnosticEngine bridgedEngine, BridgedSourceLoc loc,
    BridgedDiagID bridgedDiagID,
    BridgedArrayRef /*BridgedDiagnosticArgument*/ bridgedArguments,
    BridgedSourceLoc highlightStart, uint32_t hightlightLength,
    BridgedArrayRef /*BridgedDiagnosticFixIt*/ bridgedFixIts) {
  auto *D = bridgedEngine.get();

  auto diagID = static_cast<DiagID>(bridgedDiagID);
  SmallVector<DiagnosticArgument, 2> arguments;
  for (auto arg : getArrayRef<BridgedDiagnosticArgument>(bridgedArguments)) {
    arguments.push_back(arg.get());
  }
  auto inflight = D->diagnose(loc.get(), diagID, arguments);

  // Add highlight.
  if (highlightStart.get().isValid()) {
    CharSourceRange highlight(highlightStart.get(), (unsigned)hightlightLength);
    inflight.highlightChars(highlight.getStart(), highlight.getEnd());
  }

  // Add fix-its.
  for (const BridgedDiagnosticFixIt &fixIt : getArrayRef<BridgedDiagnosticFixIt>(bridgedFixIts)) {
    auto range = fixIt.get().getRange();
    auto text = fixIt.get().getText();
    inflight.fixItReplaceChars(range.getStart(), range.getEnd(), text);
  }
}

bool DiagnosticEngine_hadAnyError(BridgedDiagnosticEngine bridgedEngine) {
  return bridgedEngine.get()->hadAnyError();
}

//===----------------------------------------------------------------------===//
// BridgedNominalTypeDecl
//===----------------------------------------------------------------------===//

bool BridgedNominalTypeDecl_isStructWithUnreferenceableStorage(
    BridgedNominalTypeDecl decl) {
  if (auto *structDecl = dyn_cast<swift::StructDecl>(decl.get())) {
    return structDecl->hasUnreferenceableStorage();
  }
  return false;
}
