//===--- DiagnosticHelper.cpp - Diagnostic Helper ---------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  This file implements the DiagnosticHelper class.
//
//===----------------------------------------------------------------------===//

#include "swift/Frontend/DiagnosticHelper.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/DiagnosticsFrontend.h"
#include "swift/Basic/Edit.h"
#include "swift/Basic/SourceManager.h"
#include "swift/Frontend/Frontend.h"
#include "swift/Frontend/PrintingDiagnosticConsumer.h"
#include "swift/Frontend/SerializedDiagnosticConsumer.h"
#include "swift/Migrator/FixitFilter.h"
#include "llvm/Support/raw_ostream.h"

using namespace swift;

class LLVM_LIBRARY_VISIBILITY DiagnosticHelper::Implementation {
  friend class DiagnosticHelper;

public:
  Implementation(CompilerInstance &instance,
                 const CompilerInvocation &invocation,
                 llvm::raw_pwrite_stream &OS);

  void initDiagnosticConsumers();
  void setSuppressOutput(bool suppressOutput);

  void diagnoseFatalError(const char *reason, bool shouldCrash);

private:
  CompilerInstance &instance;
  const CompilerInvocation &invocation;

  // potentially created diagnostic consumers.
  PrintingDiagnosticConsumer PDC;
  std::unique_ptr<DiagnosticConsumer> SerializedConsumerDispatcher;
  std::unique_ptr<DiagnosticConsumer> FixItsConsumer;
};

namespace {

/// If there is an error with fixits it writes the fixits as edits in json
/// format.
class JSONFixitWriter : public DiagnosticConsumer,
                        public migrator::FixitFilter {
  std::string FixitsOutputPath;
  std::unique_ptr<llvm::raw_ostream> OSPtr;
  bool FixitAll;
  SourceEdits AllEdits;

public:
  JSONFixitWriter(std::string fixitsOutputPath,
                  const DiagnosticOptions &DiagOpts)
      : FixitsOutputPath(std::move(fixitsOutputPath)),
        FixitAll(DiagOpts.FixitCodeForAllDiagnostics) {}

private:
  void handleDiagnostic(SourceManager &SM,
                        const DiagnosticInfo &Info) override {
    if (!(FixitAll || shouldTakeFixit(Info)))
      return;
    for (const auto &Fix : Info.FixIts)
      AllEdits.addEdit(SM, Fix.getRange(), Fix.getText());
  }

  bool finishProcessing() override {
    std::error_code EC;
    std::unique_ptr<llvm::raw_fd_ostream> OS;
    OS.reset(
        new llvm::raw_fd_ostream(FixitsOutputPath, EC, llvm::sys::fs::OF_None));
    if (EC) {
      // Create a temporary diagnostics engine to print the error to stderr.
      SourceManager dummyMgr;
      DiagnosticEngine DE(dummyMgr);
      PrintingDiagnosticConsumer PDC;
      DE.addConsumer(PDC);
      DE.diagnose(SourceLoc(), diag::cannot_open_file, FixitsOutputPath,
                  EC.message());
      return true;
    }

    swift::writeEditsInJson(AllEdits, *OS);
    return false;
  }
};

} // anonymous namespace

/// Creates a diagnostic consumer that handles dispatching diagnostics to
/// multiple output files, based on the supplementary output paths specified by
/// \p inputsAndOutputs.
///
/// If no output files are needed, returns null.
static std::unique_ptr<DiagnosticConsumer>
createDispatchingDiagnosticConsumerIfNeeded(
    const FrontendInputsAndOutputs &inputsAndOutputs,
    llvm::function_ref<std::unique_ptr<DiagnosticConsumer>(const InputFile &)>
        maybeCreateConsumerForDiagnosticsFrom) {

  // The "4" here is somewhat arbitrary. In practice we're going to have one
  // sub-consumer for each diagnostic file we're trying to output, which (again
  // in practice) is going to be 1 in WMO mode and equal to the number of
  // primary inputs in batch mode. That in turn is going to be "the number of
  // files we need to recompile in this build, divided by the number of jobs".
  // So a value of "4" here means that there would be no heap allocation on a
  // clean build of a module with up to 32 files on an 8-core machine, if the
  // user doesn't customize anything.
  SmallVector<FileSpecificDiagnosticConsumer::Subconsumer, 4> subconsumers;

  inputsAndOutputs.forEachInputProducingSupplementaryOutput(
      [&](const InputFile &input) -> bool {
        if (auto consumer = maybeCreateConsumerForDiagnosticsFrom(input))
          subconsumers.emplace_back(input.getFileName(), std::move(consumer));
        return false;
      });
  // For batch mode, the compiler must sometimes swallow diagnostics pertaining
  // to non-primary files in order to avoid Xcode showing the same diagnostic
  // multiple times. So, create a diagnostic "eater" for those non-primary
  // files.
  //
  // This routine gets called in cases where no primary subconsumers are
  // created. Don't bother to create non-primary subconsumers if there aren't
  // any primary ones.
  //
  // To avoid introducing bugs into WMO or single-file modes, test for multiple
  // primaries.
  if (!subconsumers.empty() && inputsAndOutputs.hasMultiplePrimaryInputs()) {
    inputsAndOutputs.forEachNonPrimaryInput(
        [&](const InputFile &input) -> bool {
          subconsumers.emplace_back(input.getFileName(), nullptr);
          return false;
        });
  }

  return FileSpecificDiagnosticConsumer::consolidateSubconsumers(subconsumers);
}

/// Creates a diagnostic consumer that handles serializing diagnostics, based on
/// the supplementary output paths specified by \p inputsAndOutputs.
///
/// The returned consumer will handle producing multiple serialized diagnostics
/// files if necessary, by using sub-consumers for each file and dispatching to
/// the right one.
///
/// If no serialized diagnostics are being produced, returns null.
static std::unique_ptr<DiagnosticConsumer>
createSerializedDiagnosticConsumerIfNeeded(
    const FrontendInputsAndOutputs &inputsAndOutputs,
    bool emitMacroExpansionFiles) {
  return createDispatchingDiagnosticConsumerIfNeeded(
      inputsAndOutputs,
      [emitMacroExpansionFiles](
          const InputFile &input) -> std::unique_ptr<DiagnosticConsumer> {
        auto serializedDiagnosticsPath = input.getSerializedDiagnosticsPath();
        if (serializedDiagnosticsPath.empty())
          return nullptr;
        return serialized_diagnostics::createConsumer(serializedDiagnosticsPath,
                                                      emitMacroExpansionFiles);
      });
}

/// Creates a diagnostic consumer that handles JSONFixIt diagnostics, based on
/// the supplementary output paths specified in \p options.
///
/// If no json fixit diagnostics are being produced, returns null.
static std::unique_ptr<DiagnosticConsumer>
createJSONFixItDiagnosticConsumerIfNeeded(
    const CompilerInvocation &invocation) {
  return createDispatchingDiagnosticConsumerIfNeeded(
      invocation.getFrontendOptions().InputsAndOutputs,
      [&](const InputFile &input) -> std::unique_ptr<DiagnosticConsumer> {
        auto fixItsOutputPath = input.getFixItsOutputPath();
        if (fixItsOutputPath.empty())
          return nullptr;
        return std::make_unique<JSONFixitWriter>(
            fixItsOutputPath.str(), invocation.getDiagnosticOptions());
      });
}

DiagnosticHelper::Implementation::Implementation(
    CompilerInstance &instance, const CompilerInvocation &invocation,
    llvm::raw_pwrite_stream &OS)
    : instance(instance), invocation(invocation), PDC(OS) {
  instance.addDiagnosticConsumer(&PDC);
}

void DiagnosticHelper::Implementation::initDiagnosticConsumers() {
  // Because the serialized diagnostics consumer is initialized here,
  // diagnostics emitted above, within CompilerInvocation::parseArgs, are never
  // serialized. This is a non-issue because, in nearly all cases, frontend
  // arguments are generated by the driver, not directly by a user. The driver
  // is responsible for emitting diagnostics for its own errors.
  // See https://github.com/apple/swift/issues/45288 for details.
  SerializedConsumerDispatcher = createSerializedDiagnosticConsumerIfNeeded(
      invocation.getFrontendOptions().InputsAndOutputs,
      invocation.getDiagnosticOptions().EmitMacroExpansionFiles);
  if (SerializedConsumerDispatcher)
    instance.addDiagnosticConsumer(SerializedConsumerDispatcher.get());

  FixItsConsumer = createJSONFixItDiagnosticConsumerIfNeeded(invocation);
  if (FixItsConsumer)
    instance.addDiagnosticConsumer(FixItsConsumer.get());

  if (invocation.getDiagnosticOptions().UseColor)
    PDC.forceColors();

  PDC.setFormattingStyle(
      invocation.getDiagnosticOptions().PrintedFormattingStyle);

  PDC.setEmitMacroExpansionFiles(
      invocation.getDiagnosticOptions().EmitMacroExpansionFiles);
}

void DiagnosticHelper::Implementation::setSuppressOutput(bool suppressOutput) {
  PDC.setSuppressOutput(suppressOutput);
}

void DiagnosticHelper::Implementation::diagnoseFatalError(const char *reason,
                                                          bool shouldCrash) {
  static const char *recursiveFatalError = nullptr;
  if (recursiveFatalError) {
    // Report the /original/ error through LLVM's default handler, not
    // whatever we encountered.
    llvm::remove_fatal_error_handler();
    llvm::report_fatal_error(recursiveFatalError, shouldCrash);
  }
  recursiveFatalError = reason;

  SourceManager dummyMgr;

  DiagnosticInfo errorInfo(
      DiagID(0), SourceLoc(), DiagnosticKind::Error,
      "fatal error encountered during compilation; " SWIFT_BUG_REPORT_MESSAGE,
      {}, StringRef(), SourceLoc(), {}, {}, {}, false);
  DiagnosticInfo noteInfo(DiagID(0), SourceLoc(), DiagnosticKind::Note, reason,
                          {}, StringRef(), SourceLoc(), {}, {}, {}, false);
  PDC.handleDiagnostic(dummyMgr, errorInfo);
  PDC.handleDiagnostic(dummyMgr, noteInfo);
  if (shouldCrash)
    abort();
}

DiagnosticHelper DiagnosticHelper::create(CompilerInstance &instance,
                                          const CompilerInvocation &invocation,
                                          llvm::raw_pwrite_stream &OS) {
  return DiagnosticHelper(instance, invocation, OS);
}

DiagnosticHelper::DiagnosticHelper(CompilerInstance &instance,
                                   const CompilerInvocation &invocation,
                                   llvm::raw_pwrite_stream &OS)
    : Impl(*new Implementation(instance, invocation, OS)) {}

DiagnosticHelper::~DiagnosticHelper() { delete &Impl; }

void DiagnosticHelper::initDiagnosticConsumers() {
  Impl.initDiagnosticConsumers();
}

void DiagnosticHelper::setSuppressOutput(bool suppressOutput) {
  Impl.setSuppressOutput(suppressOutput);
}

void DiagnosticHelper::diagnoseFatalError(const char *reason,
                                          bool shouldCrash) {
  Impl.diagnoseFatalError(reason, shouldCrash);
}
