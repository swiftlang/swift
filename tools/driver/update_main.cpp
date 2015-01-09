//===--- update-main.cpp - Swift code updating ----------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
// This tool updates swift code to be able to build with a newer swift compiler.
//===----------------------------------------------------------------------===//

#include "swift/AST/DiagnosticsFrontend.h"
#include "swift/Basic/SourceManager.h"
#include "swift/Driver/FrontendUtil.h"
#include "swift/Frontend/Frontend.h"
#include "swift/Frontend/PrintingDiagnosticConsumer.h"
#include "swift/Frontend/SerializedDiagnosticConsumer.h"
#include "llvm/Support/FileSystem.h"

using namespace swift;

namespace {

/// If there is an error with fixits it writes the fixits in json format and
/// filters out the diagnostic.
class JSONFixitWriter : public DiagnosticConsumer {
  llvm::raw_ostream &OS;
  SmallVector<DiagnosticConsumer *, 2> Consumers;
  bool HasUnfixedError = false;
  bool PrevErrorFilteredOut = false;

public:
  explicit JSONFixitWriter(llvm::raw_ostream &OS) : OS(OS) {
    OS << "[\n";
  }
  ~JSONFixitWriter() {
    OS << "]\n";
  }

  /// \brief Add an additional DiagnosticConsumer to receive diagnostics.
  void addConsumer(DiagnosticConsumer *Consumer) {
    Consumers.push_back(Consumer);
  }
  void addConsumers(std::vector<DiagnosticConsumer*> NewConsumers) {
    Consumers.append(NewConsumers.begin(), NewConsumers.end());
  }

  bool hasUnfixedError() const { return HasUnfixedError; }

private:
  void handleDiagnostic(SourceManager &SM, SourceLoc Loc,
                        DiagnosticKind Kind, StringRef Text,
                        const DiagnosticInfo &Info) override {
    if (Kind == DiagnosticKind::Note && PrevErrorFilteredOut)
      return;
    PrevErrorFilteredOut = false;

    if (tryFixing(SM, Kind, Info)) {
      PrevErrorFilteredOut = true;
      return; // filter out.
    }

    if (Kind == DiagnosticKind::Error)
      HasUnfixedError = true;
    // Transfer the diagnostic to other consumers.
    for (auto *Consumer : Consumers)
      Consumer->handleDiagnostic(SM, Loc, Kind, Text, Info);
  }

  bool tryFixing(SourceManager &SM,
                 DiagnosticKind Kind,
                 const DiagnosticInfo &Info) {
    if (Kind != DiagnosticKind::Error)
      return false;
    if (Info.FixIts.empty())
      return false;
    for (const auto &Fix : Info.FixIts) {
      writeEdit(SM, Fix.getRange(), Fix.getText(), OS);
    }
    return true;
  }

  void writeEdit(SourceManager &SM, CharSourceRange Range, StringRef Text,
                 llvm::raw_ostream &OS) {
    SourceLoc Loc = Range.getStart();
    unsigned BufID = SM.findBufferContainingLoc(Loc);
    unsigned Offset = SM.getLocOffsetInBuffer(Loc, BufID);
    unsigned Length = Range.getByteLength();
    SmallString<200> Path =
      StringRef(SM.getIdentifierForBuffer(BufID));

    OS << " {\n";
    OS << "  \"file\": \"";
    OS.write_escaped(Path.str()) << "\",\n";
    OS << "  \"offset\": " << Offset << ",\n";
    if (Length != 0)
      OS << "  \"remove\": " << Length << ",\n";
    if (!Text.empty()) {
      OS << "  \"text\": \"";
      OS.write_escaped(Text) << "\",\n";
    }
    OS << " },\n";
  }
};

} // anonymous namespace

int update_main(ArrayRef<const char *> Args, const char *Argv0, void *MainAddr){
  CompilerInstance Instance;
  PrintingDiagnosticConsumer PDC;
  Instance.addDiagnosticConsumer(&PDC);

  CompilerInvocation Invocation;
  std::string MainExecutablePath = llvm::sys::fs::getMainExecutable(Argv0,
                                                                    MainAddr);
  Invocation.setMainExecutablePath(MainExecutablePath);

  // Parse arguments.
  if (Invocation.parseArgs(Args, Instance.getDiags())) {
    return 1;
  }

  std::error_code EC;
  llvm::raw_fd_ostream OutOS(Invocation.getOutputFilename(), EC, llvm::sys::fs::F_None);

  if (OutOS.has_error() || EC) {
    Instance.getDiags().diagnose(SourceLoc(), diag::error_opening_output,
                                 Invocation.getOutputFilename(), EC.message());
    OutOS.clear_error();
    return true;
  }

  JSONFixitWriter FixWriter(OutOS);
  FixWriter.addConsumers(Instance.getDiags().takeConsumers());
  Instance.addDiagnosticConsumer(&FixWriter);

  // TODO: reorder, if possible, so that diagnostics emitted during
  // CompilerInvocation::parseArgs are included in the serialized file.
  std::unique_ptr<DiagnosticConsumer> SerializedConsumer;
  {
    const std::string &SerializedDiagnosticsPath =
    Invocation.getFrontendOptions().SerializedDiagnosticsPath;
    if (!SerializedDiagnosticsPath.empty()) {
      std::error_code EC;
      std::unique_ptr<llvm::raw_fd_ostream> OS;
      OS.reset(new llvm::raw_fd_ostream(SerializedDiagnosticsPath,
                                        EC,
                                        llvm::sys::fs::F_None));

      if (EC) {
        Instance.getDiags().diagnose(SourceLoc(),
                                     diag::cannot_open_serialized_file,
                                     SerializedDiagnosticsPath, EC.message());
        return 1;
      }

      SerializedConsumer.reset(
                               serialized_diagnostics::createConsumer(std::move(OS)));
      FixWriter.addConsumer(SerializedConsumer.get());
    }
  }

  if (Invocation.getDiagnosticOptions().UseColor)
    PDC.forceColors();

  if (Instance.setup(Invocation)) {
    return 1;
  }

  Instance.performSema();
  return FixWriter.hasUnfixedError();
}
