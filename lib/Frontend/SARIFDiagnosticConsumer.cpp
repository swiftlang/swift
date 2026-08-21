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
#include "swift/Basic/LangOptions.h"
#include "swift/Basic/SourceManager.h"
#include "swift/Bridging/ASTGen.h"
#include "swift/Frontend/Frontend.h"
#include "swift/Frontend/PrintingDiagnosticConsumer.h"
#include "llvm/Support/raw_ostream.h"

using namespace swift;

namespace {

std::string to_string(const swift::StrictConcurrency level) {
  switch (level) {
  case StrictConcurrency::Minimal:
    return "Minimal";
  case StrictConcurrency::Targeted:
    return "Targeted";
  case StrictConcurrency::Complete:
    return "Complete";
  }
  llvm_unreachable("invalid StrictConcurrency");
}

std::string to_string(const ConcurrencyModel model) {
  switch (model) {
  case ConcurrencyModel::Standard:
    return "Standard";
  case ConcurrencyModel::TaskToThread:
    return "TaskToThread";
  };
  llvm_unreachable("invalid ConcurrencyModel");
}

/// Diagnostic consumer that exports diagnostics to SARIF format.
class SARIFDiagnosticConsumer : public DiagnosticConsumer {
  struct InvocationProperty {
    std::string category;
    std::string key;
    std::string value;
    uint8_t type;

    InvocationProperty(const std::string &category, const std::string &key,
                       const std::string &value)
        : category(category), key(key), value(value), type(0) {}

    InvocationProperty(const std::string &category, const std::string &key,
                       int value)
        : category(category), key(key), value(std::to_string(value)), type(1) {}

    InvocationProperty(const std::string &category, const std::string &key,
                       bool value)
        : category(category), key(key), value(value ? "true" : "false"),
          type(2) {}
  };

  struct BridgedInvocationProperty {
    BridgedStringRef category;
    BridgedStringRef key;
    BridgedStringRef value;
    uint8_t type;
  };

  std::string OutputPath;
  std::string ExecutablePath;
  std::vector<std::string> Arguments;
  std::vector<InvocationProperty> InvocationProperties;
  void *queuedDiagnostics = nullptr;
  void *perFrontendState = nullptr;
  llvm::DenseMap<unsigned, void *> queuedBuffers;
  llvm::DenseMap<std::pair<SourceManager *, unsigned>, void *> sourceFileSyntax;
  bool CalledFinishProcessing = false;

public:
  SARIFDiagnosticConsumer(StringRef outputPath,
                          const CompilerInvocation &invocation)
      : OutputPath(outputPath.str()) {

    readInvocationProperties(invocation);

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

    SmallVector<BridgedInvocationProperty, 32> bridgedInvocationProperties{
        bridgeInvocationProperties(InvocationProperties)};

    BridgedStringRef bridgedErrorMessage{nullptr, 0};

    swift_ASTGen_renderQueuedDiagnosticsAsSARIF(
        queuedDiagnostics,
        llvm::StringRef(swift::version::getCompilerVersion()),
        llvm::StringRef(OutputPath),
        BridgedArrayRef(bridgedInvocationProperties.data(),
                        bridgedInvocationProperties.size()),
        &bridgedErrorMessage);

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

  void readInvocationProperties(const CompilerInvocation &invocation) {
    const auto &langOpts = invocation.getLangOptions();
    const auto &frontendOpts = invocation.getFrontendOptions();

    InvocationProperties = {
        {"Language", "strictConcurrencyLevel",
         to_string(langOpts.StrictConcurrencyLevel)},
        {"Language", "activeConcurrencyModel",
         to_string(langOpts.ActiveConcurrencyModel)},
        {"Frontend", "moduleName", frontendOpts.ModuleName}};
  }

  SmallVector<BridgedInvocationProperty, 32> bridgeInvocationProperties(
      const std::vector<InvocationProperty> &InvocationProperties) {
    SmallVector<BridgedInvocationProperty, 32> result;
    for (const auto &prop : InvocationProperties) {
      result.push_back(
          {BridgedStringRef(prop.category.data(), prop.category.size()),
           BridgedStringRef(prop.key.data(), prop.key.size()),
           BridgedStringRef(prop.value.data(), prop.value.size()), prop.type});
    }
    return result;
  }
};

} // anonymous namespace

namespace swift {
namespace sarif_diagnostics {

std::unique_ptr<DiagnosticConsumer>
createConsumer(StringRef outputPath, const CompilerInvocation &invocation) {
  return std::make_unique<SARIFDiagnosticConsumer>(outputPath, invocation);
}

} // namespace sarif_diagnostics
} // namespace swift

#endif // SWIFT_BUILD_SWIFT_SYNTAX
