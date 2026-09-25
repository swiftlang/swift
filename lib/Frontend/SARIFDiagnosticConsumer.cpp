//===--- SARIFDiagnosticConsumer.cpp - SARIF Diagnostics ------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file implements a DiagnosticConsumer that serializes diagnostics to
// SARIF. It follows the structure of SerializedDiagnosticConsumer: diagnostics
// are accumulated as compilation proceeds and the file is written once, in
// finishProcessing().
//
// Diagnostics are accumulated through DiagnosticBridge, which surfaces them to
// the Swift side of the compiler as swift-syntax diagnostics; the conversion to
// SARIF is implemented there, in ASTGen's SARIFDiagnostics.swift.
//
//===----------------------------------------------------------------------===//

#include "swift/Frontend/SARIFDiagnosticConsumer.h"
#include "swift/AST/DiagnosticBridge.h"
#include "swift/AST/DiagnosticConsumer.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/DiagnosticsFrontend.h"
#include "swift/Basic/Assertions.h"
#include "swift/Basic/SourceManager.h"
#include "swift/Basic/Version.h"
#include "swift/Frontend/PrintingDiagnosticConsumer.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/raw_ostream.h"

using namespace swift;

#if SWIFT_BUILD_SARIF

namespace {

class SARIFDiagnosticConsumer : public DiagnosticConsumer {
  /// Where the log is written, in finishProcessing().
  std::string SARIFDiagnosticsPath;

  bool CalledFinishProcessing = false;
  bool CompilationWasComplete = true;

  /// Accumulates diagnostics on the Swift side until we serialize them.
  DiagnosticBridge Bridge;

  /// Report that the log could not be produced, using a temporary diagnostic
  /// engine as the serialized diagnostics consumer does. Always returns true,
  /// the "an error occurred" result of finishProcessing().
  bool reportFailure(StringRef reason) {
    SourceManager dummyMgr;
    DiagnosticEngine DE(dummyMgr);
    PrintingDiagnosticConsumer PDC;
    DE.addConsumer(PDC);
    DE.diagnose(SourceLoc(), diag::cannot_open_serialized_file,
                SARIFDiagnosticsPath, reason);
    return true;
  }

public:
  explicit SARIFDiagnosticConsumer(StringRef outputPath)
      : SARIFDiagnosticsPath(outputPath.str()) {}

  ~SARIFDiagnosticConsumer() {
    ASSERT(CalledFinishProcessing && "did not call finishProcessing()");
  }

  bool finishProcessing() override {
    ASSERT(!CalledFinishProcessing &&
           "called finishProcessing() multiple times");
    CalledFinishProcessing = true;

    // Create the file before anything that can fail, as the
    // SerializedDiagnosticsConsumer does.
    std::error_code EC;
    std::unique_ptr<llvm::raw_fd_ostream> OS;
    OS.reset(new llvm::raw_fd_ostream(SARIFDiagnosticsPath, EC,
                                      llvm::sys::fs::OF_None));
    if (EC)
      return reportFailure(EC.message());

    // In batch mode, if any error occurs then no primaries can be compiled. In
    // that case we match the existing behavior of serialized diagnostics
    // consumer by leaving a zero-byte file so the driver can differentiate a
    // primary that compiled cleanly from one that did not.
    if (!CompilationWasComplete) {
      Bridge.clearQueuedDiagnostics();
      return false;
    }

    // 'take' empties the queue on success and failure alike.
    auto sarif =
        Bridge.takeQueuedDiagnosticsAsSARIF(version::getSwiftFullVersion());
    if (!sarif)
      return reportFailure(llvm::toString(sarif.takeError()));

    *OS << *sarif;
    OS->flush();

    return false;
  }

  void informDriverOfIncompleteBatchModeCompilation() override {
    CompilationWasComplete = false;
  }

  void handleDiagnostic(SourceManager &SM,
                        const DiagnosticInfo &Info) override {
    // Notes are delivered again as children of the diagnostic they belong to,
    // and are queued along with it, so skip them here.
    if (Info.IsChildNote)
      return;

    // A diagnostic with no source location, such as a failure to open an input
    // file, still belongs in the log as a result with no locations. The binary
    // format records these, and dropping them would let a failed compilation
    // produce a log that looks clean.
    auto bufferStack = DiagnosticBridge::getSourceBufferStack(SM, Info.Loc);
    std::optional<unsigned> innermostBufferID;
    if (!bufferStack.empty())
      innermostBufferID = bufferStack.front();

    Bridge.enqueueDiagnostic(SM, Info, innermostBufferID);
  }
};

} // end anonymous namespace

#endif // SWIFT_BUILD_SARIF

std::unique_ptr<DiagnosticConsumer>
sarif_diagnostics::createConsumer(StringRef outputPath,
                                  bool emitMacroExpansionFiles) {
  // 'emitMacroExpansionFiles' is accepted for signature parity with the
  // serialized diagnostics consumer; SARIF does not represent macro expansion
  // buffers yet.
  (void)emitMacroExpansionFiles;

#if SWIFT_BUILD_SARIF
  return std::make_unique<SARIFDiagnosticConsumer>(outputPath);
#else
  // '-serialize-diagnostics=sarif' is rejected while parsing arguments in this
  // configuration, so the SARIF format is never selected and nothing asks for
  // this consumer.
  (void)outputPath;
  llvm_unreachable("SARIF diagnostics requested from a build without SARIF");
#endif
}
