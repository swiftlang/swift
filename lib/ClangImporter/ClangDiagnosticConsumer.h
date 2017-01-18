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

  /// Keeps alive the Clang source managers where diagnostics have been
  /// reported.
  ///
  /// This is a bit of a hack, but LLVM's source manager (and by extension
  /// Swift's) does not support buffers going away.
  //
  // This is not using SmallPtrSet or similar because we need the
  // IntrusiveRefCntPtr to stay a ref-counting pointer.
  SmallVector<llvm::IntrusiveRefCntPtr<const clang::SourceManager>, 4>
    sourceManagersWithDiagnostics;
  llvm::DenseMap<const llvm::MemoryBuffer *, unsigned> mirroredBuffers;

  const clang::IdentifierInfo *CurrentImport = nullptr;
  SourceLoc DiagLoc;
  const bool DumpToStderr;

public:
  ClangDiagnosticConsumer(ClangImporter::Implementation &impl,
                          clang::DiagnosticOptions &clangDiagOptions,
                          bool dumpToStderr);

  LoadModuleRAII handleImport(const clang::IdentifierInfo *name,
                              SourceLoc diagLoc) {
    DiagLoc = diagLoc;
    return LoadModuleRAII(*this, name);
  }

  /// Returns a Swift source location that points into a Clang buffer.
  ///
  /// This will keep the Clang buffer alive as long as this diagnostic consumer.
  SourceLoc resolveSourceLocation(const clang::SourceManager &clangSrcMgr,
                                  clang::SourceLocation clangLoc);

  void HandleDiagnostic(clang::DiagnosticsEngine::Level diagLevel,
                        const clang::Diagnostic &info) override;
};

} // end namespace swift

#endif
