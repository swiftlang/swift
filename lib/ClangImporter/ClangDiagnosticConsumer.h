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

namespace swift {

class ClangDiagnosticConsumer : public clang::DiagnosticConsumer {
  struct LoadModuleRAII {
    ClangDiagnosticConsumer *Consumer;
  public:
    LoadModuleRAII(ClangDiagnosticConsumer &consumer,
                   const clang::IdentifierInfo *name)
        : Consumer(&consumer) {
      assert(name);
      Consumer->TopModuleBeingImported = name;
      Consumer->TopModuleNotFound = false;
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
        Consumer->TopModuleBeingImported = nullptr;
    }
  };

private:
  friend struct LoadModuleRAII;

  ClangImporter::Implementation &ImporterImpl;
  const clang::IdentifierInfo *TopModuleBeingImported = nullptr;
  bool TopModuleNotFound;

public:
  ClangDiagnosticConsumer(ClangImporter::Implementation &Impl)
    : ImporterImpl(Impl) {}

  LoadModuleRAII handleLoadModule(const clang::IdentifierInfo *name) {
    return LoadModuleRAII(*this, name);
  }

  bool isTopModuleMissing() const {
    assert(TopModuleBeingImported && "no module being loaded");
    return TopModuleNotFound;
  }

  void setTopModuleMissing() {
    assert(TopModuleBeingImported && "no module being loaded");
    TopModuleNotFound = true;
  }

  void HandleDiagnostic(clang::DiagnosticsEngine::Level diagLevel,
                        const clang::Diagnostic &info) override;
};

} // end namespace swift

#endif
