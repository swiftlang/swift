//===--- Bridging/DiagnosticsBridging.cpp -----------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022-2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/AST/ASTBridging.h"

#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/DiagnosticsCommon.h"
#include "swift/Basic/Assertions.h"
#include "swift/Basic/SourceManager.h"

using namespace swift;

//===----------------------------------------------------------------------===//
// MARK: Diagnostics
//===----------------------------------------------------------------------===//

static_assert(sizeof(BridgedDiagnosticArgument) >= sizeof(DiagnosticArgument),
              "BridgedDiagnosticArgument has wrong size");

BridgedDiagnosticArgument::BridgedDiagnosticArgument(SwiftInt i)
    : BridgedDiagnosticArgument(DiagnosticArgument((int)i)) {}

BridgedDiagnosticArgument::BridgedDiagnosticArgument(BridgedStringRef s)
    : BridgedDiagnosticArgument(DiagnosticArgument(s.unbridged())) {}

static_assert(sizeof(BridgedDiagnosticFixIt) >= sizeof(DiagnosticInfo::FixIt),
              "BridgedDiagnosticFixIt has wrong size");

const swift::DiagnosticInfo::FixIt &unbridge(const BridgedDiagnosticFixIt &fixit) {
  return *reinterpret_cast<const swift::DiagnosticInfo::FixIt *>(&fixit.storage);
}

BridgedDiagnosticFixIt::BridgedDiagnosticFixIt(SourceLoc start, uint32_t length,
                                               BridgedStringRef text) {
  DiagnosticInfo::FixIt fixit(CharSourceRange(start, length), text.unbridged(),
                              llvm::ArrayRef<DiagnosticArgument>());
  *reinterpret_cast<swift::DiagnosticInfo::FixIt *>(&storage) = fixit;
}

void BridgedDiagnosticEngine_diagnose(
    BridgedDiagnosticEngine bridgedEngine, SourceLoc loc, DiagID diagID,
    BridgedArrayRef /*BridgedDiagnosticArgument*/ bridgedArguments,
    SourceLoc highlightStart, uint32_t hightlightLength,
    BridgedArrayRef /*BridgedDiagnosticFixIt*/ bridgedFixIts) {
  auto *D = bridgedEngine.unbridged();

  SmallVector<DiagnosticArgument, 2> arguments;
  for (auto arg : bridgedArguments.unbridged<BridgedDiagnosticArgument>()) {
    arguments.push_back(arg.unbridged());
  }
  auto inflight = D->diagnose(loc, diagID, arguments);

  // Add highlight.
  if (highlightStart.isValid()) {
    CharSourceRange highlight(highlightStart, (unsigned)hightlightLength);
    inflight.highlightChars(highlight.getStart(), highlight.getEnd());
  }

  // Add fix-its.
  for (const BridgedDiagnosticFixIt &fixIt :
       bridgedFixIts.unbridged<BridgedDiagnosticFixIt>()) {
    auto range = unbridge(fixIt).getRange();
    auto text = unbridge(fixIt).getText();
    inflight.fixItReplaceChars(range.getStart(), range.getEnd(), text);
  }
}

SourceLoc BridgedDiagnostic_getLocationFromExternalSource(
    BridgedDiagnosticEngine bridgedEngine, BridgedStringRef path, SwiftInt line,
    SwiftInt column) {
  auto *d = bridgedEngine.unbridged();
  auto loc = d->SourceMgr.getLocFromExternalSource(path.unbridged(), line, column);
  return loc;
}

bool BridgedDiagnosticEngine_hadAnyError(
    BridgedDiagnosticEngine bridgedEngine) {
  return bridgedEngine.unbridged()->hadAnyError();
}

struct BridgedDiagnostic::Impl {
  typedef llvm::MallocAllocator Allocator;

  InFlightDiagnostic inFlight;
  std::vector<StringRef> textBlobs;

  Impl(InFlightDiagnostic inFlight, std::vector<StringRef> textBlobs)
      : inFlight(std::move(inFlight)), textBlobs(std::move(textBlobs)) {}

  Impl(const Impl &) = delete;
  Impl(Impl &&) = delete;
  Impl &operator=(const Impl &) = delete;
  Impl &operator=(Impl &&) = delete;

  ~Impl() {
    inFlight.flush();

    Allocator allocator;
    for (auto text : textBlobs) {
      allocator.Deallocate(text.data(), text.size());
    }
  }
};

BridgedDiagnostic BridgedDiagnostic_create(SourceLoc loc,
                                           BridgedStringRef cText,
                                           DiagnosticKind severity,
                                           BridgedDiagnosticEngine cDiags) {
  StringRef origText = cText.unbridged();
  BridgedDiagnostic::Impl::Allocator alloc;
  StringRef text = origText.copy(alloc);

  Diag<StringRef> diagID;
  switch (severity) {
  case DiagnosticKind::Error:
    diagID = diag::bridged_error;
    break;
  case DiagnosticKind::Note:
    diagID = diag::bridged_note;
    break;
  case DiagnosticKind::Remark:
    diagID = diag::bridged_remark;
    break;
  case DiagnosticKind::Warning:
    diagID = diag::bridged_warning;
    break;
  }

  DiagnosticEngine &diags = *cDiags.unbridged();
  return new BridgedDiagnostic::Impl{diags.diagnose(loc, diagID, text), {text}};
}

/// Highlight a source range as part of the diagnostic.
void BridgedDiagnostic_highlight(BridgedDiagnostic cDiag, SourceLoc startLoc,
                                 SourceLoc endLoc) {
  BridgedDiagnostic::Impl *diag = cDiag.unbridged();
  diag->inFlight.highlightChars(startLoc, endLoc);
}

/// Add a Fix-It to replace a source range as part of the diagnostic.
void BridgedDiagnostic_fixItReplace(BridgedDiagnostic cDiag, SourceLoc startLoc,
                                    SourceLoc endLoc,
                                    BridgedStringRef cReplaceText) {
  StringRef origReplaceText = cReplaceText.unbridged();
  BridgedDiagnostic::Impl::Allocator alloc;
  StringRef replaceText = origReplaceText.copy(alloc);

  BridgedDiagnostic::Impl *diag = cDiag.unbridged();
  diag->textBlobs.push_back(replaceText);
  diag->inFlight.fixItReplaceChars(startLoc, endLoc, replaceText);
}

/// Finish the given diagnostic and emit it.
void BridgedDiagnostic_finish(BridgedDiagnostic cDiag) {
  BridgedDiagnostic::Impl *diag = cDiag.unbridged();
  delete diag;
}
