//===--- DiagnosticBridge.cpp - Diagnostic Bridge to swift-syntax ---------===//
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
//
//  This file implements the DiagnosticBridge class.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/DiagnosticBridge.h"
#include "swift/AST/ASTBridging.h"
#include "swift/AST/DiagnosticConsumer.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/Basic/SourceManager.h"
#include "swift/Bridging/ASTGen.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/Support/raw_ostream.h"

using namespace swift;

#if SWIFT_BUILD_SWIFT_SYNTAX
static BridgedDiagnosticSeverity bridgeDiagnosticSeverity(DiagnosticKind kind) {
  switch (kind) {
  case DiagnosticKind::Error:
    return BridgedDiagnosticSeverity::BridgedError;

  case DiagnosticKind::Warning:
    return BridgedDiagnosticSeverity::BridgedWarning;

  case DiagnosticKind::Remark:
    return BridgedDiagnosticSeverity::BridgedRemark;

  case DiagnosticKind::Note:
    return BridgedDiagnosticSeverity::BridgedNote;
  }
}

/// Enqueue a diagnostic with ASTGen's diagnostic rendering.
static void addQueueDiagnostic(void *queuedDiagnostics,
                               void *perFrontendState,
                               const DiagnosticInfo &info, SourceManager &SM) {
  llvm::SmallString<256> text;
  {
    llvm::raw_svector_ostream out(text);
    DiagnosticEngine::formatDiagnosticText(out, info.FormatString,
                                           info.FormatArgs);
  }

  BridgedDiagnosticSeverity severity = bridgeDiagnosticSeverity(info.Kind);

  // Map the highlight ranges.
  SmallVector<BridgedCharSourceRange, 2> highlightRanges;
  for (const auto &range : info.Ranges) {
    if (range.isValid())
      highlightRanges.push_back(range);
  }

  StringRef documentationPath = info.CategoryDocumentationURL;

  SmallVector<BridgedFixIt, 2> fixIts;
  for (const auto &fixIt : info.FixIts) {
    fixIts.push_back(BridgedFixIt{ fixIt.getRange(), fixIt.getText() });
  }

  swift_ASTGen_addQueuedDiagnostic(
      queuedDiagnostics, perFrontendState,
      text.str(),
      severity, info.Loc,
      info.Category,
      documentationPath,
      highlightRanges.data(), highlightRanges.size(),
      llvm::ArrayRef<BridgedFixIt>(fixIts));

  // TODO: A better way to do this would be to pass the notes as an
  // argument to `swift_ASTGen_addQueuedDiagnostic` but that requires
  // bridging of `Note` structure and new serialization.
  for (auto *childNote : info.ChildDiagnosticInfo) {
    addQueueDiagnostic(queuedDiagnostics, perFrontendState, *childNote, SM);
  }
}

void DiagnosticBridge::emitDiagnosticWithoutLocation(
    const DiagnosticInfo &info, llvm::raw_ostream &out, bool forceColors) {
  ASSERT(queuedDiagnostics == nullptr);

  // If we didn't have per-frontend state before, create it now.
  if (!perFrontendState) {
    perFrontendState = swift_ASTGen_createPerFrontendDiagnosticState();
  }

  llvm::SmallString<256> text;
  {
    llvm::raw_svector_ostream out(text);
    DiagnosticEngine::formatDiagnosticText(out, info.FormatString,
                                           info.FormatArgs);
  }

  BridgedDiagnosticSeverity severity = bridgeDiagnosticSeverity(info.Kind);

  BridgedStringRef bridgedRenderedString{nullptr, 0};
  swift_ASTGen_renderSingleDiagnostic(
      perFrontendState, text.str(), severity, info.Category,
      llvm::StringRef(info.CategoryDocumentationURL), forceColors ? 1 : 0,
      &bridgedRenderedString);

  auto renderedString = bridgedRenderedString.unbridged();
  if (renderedString.data()) {
    out << "<unknown>:0: ";
    out.write(renderedString.data(), renderedString.size());
    swift_ASTGen_freeBridgedString(renderedString);
    out << "\n";
  }
}

void DiagnosticBridge::enqueueDiagnostic(SourceManager &SM,
                                         const DiagnosticInfo &Info,
                                         unsigned innermostBufferID) {
  // If we didn't have per-frontend state before, create it now.
  if (!perFrontendState) {
    perFrontendState = swift_ASTGen_createPerFrontendDiagnosticState();
  }

  // If there are no enqueued diagnostics, or we have hit a non-note
  // diagnostic, flush any enqueued diagnostics and start fresh.
  if (!queuedDiagnostics)
    queuedDiagnostics = swift_ASTGen_createQueuedDiagnostics();

  queueBuffer(SM, innermostBufferID);
  addQueueDiagnostic(queuedDiagnostics, perFrontendState, Info, SM);
}

void DiagnosticBridge::flush(llvm::raw_ostream &OS, bool includeTrailingBreak,
                             bool forceColors) {
  if (!queuedDiagnostics)
    return;

  BridgedStringRef bridgedRenderedString{nullptr, 0};
  swift_ASTGen_renderQueuedDiagnostics(queuedDiagnostics, /*contextSize=*/2,
                                       forceColors ? 1 : 0,
                                       &bridgedRenderedString);
  auto renderedString = bridgedRenderedString.unbridged();
  if (renderedString.data()) {
    OS.write(renderedString.data(), renderedString.size());
    swift_ASTGen_freeBridgedString(renderedString);
  }
  swift_ASTGen_destroyQueuedDiagnostics(queuedDiagnostics);
  queuedDiagnostics = nullptr;
  queuedBuffers.clear();

  if (includeTrailingBreak)
    OS << "\n";
}

void DiagnosticBridge::printCategoryFootnotes(llvm::raw_ostream &os,
                                              bool forceColors) {
  if (!perFrontendState)
    return;

  BridgedStringRef bridgedRenderedString{nullptr, 0};
  swift_ASTGen_renderCategoryFootnotes(
      perFrontendState, forceColors ? 1 : 0, &bridgedRenderedString);

  auto renderedString = bridgedRenderedString.unbridged();
  if (auto renderedData = renderedString.data()) {
    os.write(renderedData, renderedString.size());
    swift_ASTGen_freeBridgedString(renderedString);
  }
}

void *DiagnosticBridge::getSourceFileSyntax(SourceManager &sourceMgr,
                                            unsigned bufferID,
                                            StringRef displayName) {
  auto known = sourceFileSyntax.find({&sourceMgr, bufferID});
  if (known != sourceFileSyntax.end())
    return known->second;

  auto bufferContents = sourceMgr.getEntireTextForBuffer(bufferID);
  auto sourceFile = swift_ASTGen_parseSourceFile(
      bufferContents, StringRef{"module"}, displayName,
      /*declContextPtr=*/nullptr, BridgedGeneratedSourceFileKindNone);

  sourceFileSyntax[{&sourceMgr, bufferID}] = sourceFile;
  return sourceFile;
}

void DiagnosticBridge::queueBuffer(SourceManager &sourceMgr,
                                   unsigned bufferID) {
  QueuedBuffer knownSourceFile = queuedBuffers[bufferID];
  if (knownSourceFile)
    return;

  // Find the parent and position in parent, if there is one.
  int parentID = -1;
  int positionInParent = 0;
  std::string displayName;
  auto generatedSourceInfo = sourceMgr.getGeneratedSourceInfo(bufferID);
  if (generatedSourceInfo) {
    SourceLoc parentLoc = generatedSourceInfo->originalSourceRange.getEnd();
    if (parentLoc.isValid()) {
      parentID = sourceMgr.findBufferContainingLoc(parentLoc);
      positionInParent = sourceMgr.getLocOffsetInBuffer(parentLoc, parentID);

      // Queue the parent buffer.
      queueBuffer(sourceMgr, parentID);
    }

    if (!generatedSourceInfo->macroName.empty()) {
      if (generatedSourceInfo->attachedMacroCustomAttr)
        displayName = "macro expansion @" + generatedSourceInfo->macroName;
      else
        displayName = "macro expansion #" + generatedSourceInfo->macroName;
    }
  }

  if (displayName.empty()) {
    displayName =
        sourceMgr.getDisplayNameForLoc(sourceMgr.getLocForBufferStart(bufferID))
            .str();
  }

  auto sourceFile = getSourceFileSyntax(sourceMgr, bufferID, displayName);
  swift_ASTGen_addQueuedSourceFile(queuedDiagnostics, bufferID, sourceFile,
                                   (const uint8_t *)displayName.data(),
                                   displayName.size(), parentID,
                                   positionInParent);
  queuedBuffers[bufferID] = sourceFile;
}

SmallVector<unsigned, 1>
DiagnosticBridge::getSourceBufferStack(SourceManager &sourceMgr,
                                       SourceLoc loc) {
  SmallVector<unsigned, 1> stack;
  while (true) {
    if (loc.isInvalid())
      return stack;

    unsigned bufferID = sourceMgr.findBufferContainingLoc(loc);
    stack.push_back(bufferID);

    auto generatedSourceInfo = sourceMgr.getGeneratedSourceInfo(bufferID);
    if (!generatedSourceInfo)
      return stack;

    loc = generatedSourceInfo->originalSourceRange.getStart();
  }
}

DiagnosticBridge::~DiagnosticBridge() {
  if (perFrontendState) {
    swift_ASTGen_destroyPerFrontendDiagnosticState(perFrontendState);
  }

  assert(!queuedDiagnostics && "unflushed diagnostics");
  for (const auto &sourceFileSyntax : sourceFileSyntax) {
    swift_ASTGen_destroySourceFile(sourceFileSyntax.second);
  }
}

#endif // SWIFT_BUILD_SWIFT_SYNTAX
