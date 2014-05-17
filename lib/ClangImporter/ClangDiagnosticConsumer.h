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
  ClangImporter::Implementation &ImporterImpl;

public:
  ClangDiagnosticConsumer(ClangImporter::Implementation &Impl)
      : ImporterImpl(Impl) {}

  const clang::IdentifierInfo *TopModuleBeingImported = nullptr;

  void HandleDiagnostic(clang::DiagnosticsEngine::Level diagLevel,
                        const clang::Diagnostic &info) override;
};

} // end namespace swift

#endif
