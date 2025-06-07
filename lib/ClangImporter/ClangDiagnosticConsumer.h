//===--- ClangDiagnosticConsumer.h - Handle Clang diagnostics ---*- C++ -*-===//
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
#ifndef SWIFT_CLANG_DIAGNOSTIC_CONSUMER_H
#define SWIFT_CLANG_DIAGNOSTIC_CONSUMER_H

#include "swift/ClangImporter/ClangImporter.h"
#include "clang/Basic/Diagnostic.h"
#include "clang/Basic/IdentifierTable.h"
#include "clang/Frontend/TextDiagnosticPrinter.h"
#include "llvm/Support/MemoryBuffer.h"

namespace swift {

class ClangDiagnosticConsumer : public clang::TextDiagnosticPrinter {
  struct LoadModuleRAII {
    ClangDiagnosticConsumer *Consumer;
    clang::DiagnosticsEngine *Engine;
    bool DiagEngineClearPriorToLookup;

  public:
    LoadModuleRAII(ClangDiagnosticConsumer &consumer,
                   clang::DiagnosticsEngine &engine,
                   const clang::IdentifierInfo *import)
        : Consumer(&consumer), Engine(&engine) {
      assert(import);
      assert(!Consumer->CurrentImport);
      assert(!Consumer->CurrentImportNotFound);
      Consumer->CurrentImport = import;
      DiagEngineClearPriorToLookup = !engine.hasErrorOccurred();
    }

    LoadModuleRAII(LoadModuleRAII &) = delete;
    LoadModuleRAII &operator=(LoadModuleRAII &) = delete;

    LoadModuleRAII(LoadModuleRAII &&other) {
      *this = std::move(other);
    }
    LoadModuleRAII &operator=(LoadModuleRAII &&other) {
      Consumer = other.Consumer;
      other.Consumer = nullptr;
      return *this;
    }

    ~LoadModuleRAII() {
      if (Consumer) {
        // We must reset Clang's diagnostic engine here since we know that only
        // the module lookup errors have been emitted. While the
        // ClangDiagnosticConsumer takes care of filtering out the diagnostics
        // from the output and from being handled by Swift's DiagnosticEngine,
        // we must ensure to also reset Clang's DiagnosticEngine because its
        // state is queried in later stages of compilation and errors emitted on
        // "module_not_found" should not be counted. Use a soft reset that only
        // clear the errors but not reset the states.
        // FIXME: We must instead allow for module loading in Clang to fail
        // without needing to emit a diagnostic.
        if (Engine && Consumer->CurrentImportNotFound &&
            DiagEngineClearPriorToLookup)
          Engine->Reset(/*soft=*/true);
        Consumer->CurrentImport = nullptr;
        Consumer->CurrentImportNotFound = false;
      }
    }
  };

private:
  friend struct LoadModuleRAII;

  ClangImporter::Implementation &ImporterImpl;

  const clang::IdentifierInfo *CurrentImport = nullptr;
  bool CurrentImportNotFound = false;
  SourceLoc DiagLoc;
  const bool DumpToStderr;

public:
  ClangDiagnosticConsumer(ClangImporter::Implementation &impl,
                          clang::DiagnosticOptions &clangDiagOptions,
                          bool dumpToStderr);

  LoadModuleRAII handleImport(const clang::IdentifierInfo *name,
                              clang::DiagnosticsEngine &engine,
                              SourceLoc diagLoc) {
    DiagLoc = diagLoc;
    return LoadModuleRAII(*this, engine, name);
  }

  void HandleDiagnostic(clang::DiagnosticsEngine::Level diagLevel,
                        const clang::Diagnostic &info) override;
};

} // end namespace swift

#endif
