//===--- Migrator.cpp ------------------------------------------*- C++ -*-===//
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

#include "swift/Frontend/Frontend.h"
#include "swift/Migrator/FixitApplyDiagnosticConsumer.h"
#include "swift/Migrator/Migrator.h"
#include "clang/Rewrite/Core/RewriteBuffer.h"
#include "llvm/Support/FileSystem.h"

using namespace swift;
using namespace swift::migrator;

bool migrator::updateCodeAndEmitRemap(CompilerInstance &Instance,
                                      const CompilerInvocation &Invocation) {

  Migrator M { Instance, Invocation }; // Provide inputs and configuration

  // Phase 1:
  // Perform any syntactic transformations if requested.

  // TODO

  // Phase 2:
  // Perform fix-it based migrations on the compiler, some number of times in
  // order to give the compiler an opportunity to
  // take its time reaching a fixed point.

  if (M.getMigratorOptions().EnableMigratorFixits) {
    M.repeatFixitMigrations(Migrator::MaxCompilerFixitPassIterations);
  }

  // OK, we have a final resulting text. Now we compare against the input
  // to calculate a replacement map describing the changes to the input
  // necessary to get the output.
  // TODO: Document replacement map format.

  SmallVector<Replacement, 16> FinalReplacements;
  M.getReplacements(FinalReplacements);
  auto EmitRemapFailed =
    Replacement::emitRemap(Invocation.getOutputFilename(),
                           llvm::makeArrayRef(FinalReplacements));
  auto EmitMigratedFailed = M.emitMigratedFile();
  auto DumpMigrationStatesFailed = M.dumpStates();
  return EmitRemapFailed || EmitMigratedFailed || DumpMigrationStatesFailed;
}

static raw_ostream &printEscaped(raw_ostream &Stream, StringRef Str) {
  for (unsigned i = 0, e = Str.size(); i != e; ++i) {
    unsigned char c = Str[i];

    switch (c) {
      case '\\':
        Stream << '\\' << '\\';
        break;
      case '\t':
        Stream << '\\' << 't';
        break;
      case '\n':
        Stream << '\\' << 'n';
        break;
      case '"':
        Stream << '\\' << '"';
        break;
      default:
        Stream << c;
        break;
    }
  }
  return Stream;
}

void Replacement::printJSON(llvm::raw_ostream &OS) const {
  OS << "  {\n";

  OS << "    \"file\": \"";
  printEscaped(OS, Filename);
  OS << "\",\n";

  OS << "    \"offset\": " << OrigOffset << ",\n";

  if (OrigLength > 0) {
    OS << "    \"remove\": \"" << OrigLength << ",\n";
  }

  if (!ReplacementText.empty()) {
    OS << "    \"text\": \"";
    printEscaped(OS, ReplacementText);
    OS << "\"\n";
  }

  OS << "  }";
}

Migrator::Migrator(CompilerInstance &StartInstance,
                   const CompilerInvocation &StartInvocation)
  : StartInstance(StartInstance), StartInvocation(StartInvocation) {

    auto ErrorOrStartBuffer = llvm::MemoryBuffer::getFile(getInputFilename());
    auto &StartBuffer = ErrorOrStartBuffer.get();
    auto StartBufferID = SrcMgr.addNewSourceBuffer(std::move(StartBuffer));
    States.push_back(FixitMigrationState::start(SrcMgr, StartBufferID));
}

void Migrator::
repeatFixitMigrations(const unsigned Iterations) {
  for (unsigned i = 0; i < Iterations; ++i) {
    auto ThisResult = performAFixItMigration();
    if (!ThisResult.hasValue()) {
      // Something went wrong? Track error in the state?
      break;
    } else {
      if (ThisResult.getValue()->outputDiffersFromInput()) {
        States.push_back(ThisResult.getValue());
      } else {
        break;
      }
    }
  }
}

llvm::Optional<RC<FixitMigrationState>>
Migrator::performAFixItMigration() {

  auto InputState = cast<FixitMigrationState>(States.back());
  auto InputBuffer =
    llvm::MemoryBuffer::getMemBufferCopy(InputState->getOutputText(),
                                     getInputFilename());

  CompilerInvocation Invocation { StartInvocation };
  Invocation.clearInputs();
  Invocation.addInputBuffer(InputBuffer.get());

  CompilerInstance Instance;
  if (Instance.setup(Invocation)) {
    // TODO: Return a state with an error attached?
    return None;
  }

  FixitApplyDiagnosticConsumer FixitApplyConsumer {
    getMigratorOptions(),
    InputState->getInputText(),
    getInputFilename(),
  };
  Instance.addDiagnosticConsumer(&FixitApplyConsumer);

  Instance.performSema();

  StringRef ResultText = InputState->getInputText();
  unsigned ResultBufferID = InputState->getInputBufferID();

  if (FixitApplyConsumer.getNumFixitsApplied() > 0) {
    SmallString<4096> Scratch;
    llvm::raw_svector_ostream OS(Scratch);
    FixitApplyConsumer.printResult(OS);
    auto ResultBuffer = llvm::MemoryBuffer::getMemBufferCopy(OS.str());
    ResultText = ResultBuffer->getBuffer();
    ResultBufferID = SrcMgr.addNewSourceBuffer(std::move(ResultBuffer));
  }

  return FixitMigrationState::make(SrcMgr, InputState->getInputBufferID(),
                                   ResultBufferID,
                                   FixitApplyConsumer.getReplacements());
}

llvm::Optional<RC<MigrationState>>
Migrator::performSyntacticPasses(/* TODO: Array of passes */) {
  // TODO
  return None;
}

void
Migrator::getReplacements(SmallVectorImpl<Replacement> &Replacements) const {
  // TODO: Use DMP to iterate over diffs and generate replacements
  // between the start state and end state.
}

bool Migrator::emitMigratedFile() const {
  const auto &OutFilename = getMigratorOptions().EmitMigratedFilePath;
  if (OutFilename.empty()) {
    return false;
  }

  std::error_code Error;
  llvm::raw_fd_ostream FileOS(OutFilename,
                              Error, llvm::sys::fs::F_Text);
  if (FileOS.has_error()) {
    return true;
  }

  FileOS << States.back()->getOutputText();

  FileOS.flush();

  return FileOS.has_error();
}

bool Migrator::dumpStates() const {
  const auto &OutDir = getMigratorOptions().DumpMigrationStatesDir;
  if (OutDir.empty()) {
    return false;
  }

  auto Failed = false;
  for (size_t i = 0; i < States.size(); ++i) {
    Failed |= States[i]->print(i, OutDir);
  }

  return Failed;
}

const MigratorOptions &Migrator::getMigratorOptions() const {
  return StartInvocation.getMigratorOptions();
}

const StringRef Migrator::getInputFilename() const {
  auto PrimaryInput =
    StartInvocation.getFrontendOptions().PrimaryInput.getValue();
  return StartInvocation.getInputFilenames()[PrimaryInput.Index];
}
