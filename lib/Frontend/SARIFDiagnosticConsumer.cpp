//===--- SARIFDiagnosticConsumer.cpp - Export Diagnostics as SARIF -------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  This file implements the SARIF diagnostic consumer, which exports
//  diagnostics in SARIF v2.1.0 JSON format.
//
//===----------------------------------------------------------------------===//

#if SWIFT_BUILD_SWIFT_SYNTAX

#include "swift/AST/DiagnosticConsumer.h"
#include "swift/AST/DiagnosticsFrontend.h"
#include "swift/Basic/SourceManager.h"
#include "swift/Bridging/ASTGen.h"
#include "swift/Frontend/PrintingDiagnosticConsumer.h"
#include "llvm/Support/raw_ostream.h"

using namespace swift;

namespace {

/// Diagnostic consumer that exports diagnostics to SARIF format.
class SARIFDiagnosticConsumer : public DiagnosticConsumer {
  std::string OutputPath;
  void *queuedDiagnostics = nullptr;
  void *perFrontendState = nullptr;
  llvm::DenseMap<unsigned, void *> queuedBuffers;
  llvm::DenseMap<std::pair<SourceManager *, unsigned>, void *> sourceFileSyntax;
  bool CalledFinishProcessing = false;

public:
  SARIFDiagnosticConsumer(StringRef outputPath) : OutputPath(outputPath.str()) {
    queuedDiagnostics = swift_ASTGen_createQueuedDiagnostics();
    perFrontendState = swift_ASTGen_createPerFrontendDiagnosticState();
  }

  ~SARIFDiagnosticConsumer() {
    assert(CalledFinishProcessing && "did not call finishProcessing()");

    if (perFrontendState) {
      swift_ASTGen_destroyPerFrontendDiagnosticState(perFrontendState);
    }

    // Queued diagnostics should have been destroyed in finishProcessing
    assert(!queuedDiagnostics && "unflushed diagnostics");

    for (const auto &entry : sourceFileSyntax) {
      swift_ASTGen_destroySourceFile(entry.second);
    }
  }

  bool finishProcessing() override {
    assert(!CalledFinishProcessing &&
           "called finishProcessing() multiple times");
    CalledFinishProcessing = true;

    if (!queuedDiagnostics)
      return false;

    BridgedStringRef bridgedErrorMessage{nullptr, 0};
    swift_ASTGen_renderQueuedDiagnosticsAsSARIF(
        queuedDiagnostics,
        llvm::StringRef(swift::version::getCompilerVersion()),
        llvm::StringRef(OutputPath), &bridgedErrorMessage);

    // Clean up queued diagnostics
    swift_ASTGen_destroyQueuedDiagnostics(queuedDiagnostics);
    queuedDiagnostics = nullptr;
    queuedBuffers.clear();

    const llvm::StringRef errorMessage = bridgedErrorMessage.unbridged();

    if (errorMessage.data()) {
      SourceManager dummyMgr;
      DiagnosticEngine DE(dummyMgr);
      PrintingDiagnosticConsumer PDC;
      DE.addConsumer(PDC);
      DE.diagnose(SourceLoc(), diag::cannot_serialize_diagnostics_to_sarif,
                  OutputPath, errorMessage);

      swift_ASTGen_freeBridgedString(bridgedErrorMessage);

      return true;
    }

    return false;
  }

  void handleDiagnostic(SourceManager &SM,
                        const DiagnosticInfo &Info) override {
    if (!queuedDiagnostics)
      return;

    // Queue the source buffer(s) for this diagnostic
    if (Info.Loc.isValid()) {
      unsigned bufferID = SM.findBufferContainingLoc(Info.Loc);
      queueBuffer(SM, bufferID);
    }

    // Add the diagnostic to the queue
    addQueuedDiagnostic(Info, SM);

    // Add child diagnostics (notes)
    for (auto *childNote : Info.ChildDiagnosticInfo) {
      addQueuedDiagnostic(*childNote, SM);
    }
  }

private:
  void addQueuedDiagnostic(const DiagnosticInfo &info, SourceManager &SM) {
    llvm::SmallString<256> text;
    {
      llvm::raw_svector_ostream out(text);
      DiagnosticEngine::formatDiagnosticText(out, info.FormatString,
                                             info.FormatArgs);
    }

    StringRef documentationPath = info.CategoryDocumentationURL;

    SmallVector<BridgedFixIt, 2> fixIts;
    for (const auto &fixIt : info.FixIts) {
      fixIts.push_back(BridgedFixIt{fixIt.getRange(), fixIt.getText()});
    }

    swift_ASTGen_addQueuedDiagnostic(
        queuedDiagnostics, perFrontendState, text.str(), info.Kind, info.Loc,
        info.Category, documentationPath, info.Ranges.data(),
        info.Ranges.size(), llvm::ArrayRef<BridgedFixIt>(fixIts));
  }

  void *getSourceFileSyntax(SourceManager &sourceMgr, unsigned bufferID,
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

  void queueBuffer(SourceManager &sourceMgr, unsigned bufferID) {
    if (queuedBuffers.count(bufferID))
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

        // Queue the parent buffer recursively
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
          sourceMgr
              .getDisplayNameForLoc(sourceMgr.getLocForBufferStart(bufferID))
              .str();
    }

    auto sourceFile = getSourceFileSyntax(sourceMgr, bufferID, displayName);
    swift_ASTGen_addQueuedSourceFile(queuedDiagnostics, bufferID, sourceFile,
                                     (const uint8_t *)displayName.data(),
                                     displayName.size(), parentID,
                                     positionInParent);

    queuedBuffers[bufferID] = sourceFile;
  }
};

} // anonymous namespace

namespace swift {
namespace sarif_diagnostics {

std::unique_ptr<DiagnosticConsumer> createConsumer(StringRef outputPath) {
  return std::make_unique<SARIFDiagnosticConsumer>(outputPath);
}

} // namespace sarif_diagnostics
} // namespace swift

#endif // SWIFT_BUILD_SWIFT_SYNTAX
