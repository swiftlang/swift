//===--- ClangDiagnosticConsumer.h - Handle Clang diagnostics ---*- C++ -*-===//
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
#ifndef SWIFT_CLANG_DIAGNOSTIC_CONSUMER_H
#define SWIFT_CLANG_DIAGNOSTIC_CONSUMER_H

#include "swift/ClangImporter/ClangImporter.h"
#include "clang/Basic/Diagnostic.h"
#include "clang/Basic/IdentifierTable.h"
#include "clang/Frontend/TextDiagnosticPrinter.h"

namespace swift {

class ClangDiagnosticConsumer : public clang::TextDiagnosticPrinter {
  struct LoadModuleRAII {
    ClangDiagnosticConsumer *Consumer;
  public:
    LoadModuleRAII(ClangDiagnosticConsumer &consumer,
                   const clang::IdentifierInfo *import)
        : Consumer(&consumer) {
      assert(import);
      assert(!Consumer->CurrentImport);
      Consumer->CurrentImport = import;
      Consumer->CurrentImportNotFound = false;
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
      if (Consumer)
        Consumer->CurrentImport = nullptr;
    }
  };

private:
  friend struct LoadModuleRAII;

  ClangImporter::Implementation &ImporterImpl;
  const clang::IdentifierInfo *CurrentImport = nullptr;
  bool CurrentImportNotFound;
  bool DumpToStderr;

public:
  ClangDiagnosticConsumer(ClangImporter::Implementation &impl,
                          clang::DiagnosticOptions &clangDiagOptions,
                          bool dumpToStderr);

  LoadModuleRAII handleImport(const clang::IdentifierInfo *name) {
    return LoadModuleRAII(*this, name);
  }

  bool isCurrentImportMissing() const {
    assert(CurrentImport && "nothing being loaded");
    return CurrentImportNotFound;
  }

  void setCurrentImportMissing() {
    assert(CurrentImport && "nothing being loaded");
    CurrentImportNotFound = true;
  }

  void HandleDiagnostic(clang::DiagnosticsEngine::Level diagLevel,
                        const clang::Diagnostic &info) override;
};

} // end namespace swift

#endif
