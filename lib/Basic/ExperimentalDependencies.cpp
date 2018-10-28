//===--- ExperimentalDependencies.cpp - Generates swiftdeps files ---------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include <stdio.h>

#include "swift/Basic/ExperimentalDependencies.h"

// may not all be needed
#include "swift/AST/ASTContext.h"
#include "swift/AST/ASTMangler.h"
#include "swift/AST/Decl.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/DiagnosticsFrontend.h"
#include "swift/AST/ExistentialLayout.h"
#include "swift/AST/FileSystem.h"
#include "swift/AST/Module.h"
#include "swift/AST/ModuleLoader.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/Types.h"
#include "swift/Basic/FileSystem.h"
#include "swift/Basic/LLVM.h"
#include "swift/Frontend/FrontendOptions.h"
#include "llvm/ADT/MapVector.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/YAMLParser.h"

using namespace swift;
using namespace ExperimentalDependencies;

namespace {
/// Emits the reference dependencies from the frontend so that the driver
/// can compute a dependency graph for the whole module, and use it to decide
/// which files need to be recompiled when doing incremental compilation.
class ReferenceDependenciesEmitter {
  SourceFile *const SF;
  const DependencyTracker &depTracker;
  llvm::raw_ostream &out;

  ReferenceDependenciesEmitter(SourceFile *const SF,
                               const DependencyTracker &depTracker,
                               llvm::raw_ostream &out)
      : SF(SF), depTracker(depTracker), out(out) {}

public:
  /// Emits the provided and depended-upon dependencies to a file
  ///
  /// \param diags Where problems opening the file are emitted
  /// \param SF The SourceFile containing the code with the dependences
  /// \param depTracker The entities depended-upon
  /// \param outputPath Where the dependencies are written
  ///
  /// \return true on error
  static bool emit(DiagnosticEngine &diags, SourceFile *SF,
                   const DependencyTracker &depTracker, StringRef outputPath);

  /// Emit the dependencies.
  static void emit(SourceFile *SF, const DependencyTracker &depTracker,
                   llvm::raw_ostream &out);

private:
  /// Emits all the dependency information.
  void emit() const;

  void emitProvides() const;
  void emitDepends() const;
  void emitInterfaceHash() const;
};

/// Emits the declarations provided by a source file.
class ProvidesEmitter {
  const SourceFile *const SF;
  llvm::raw_ostream &out;

  ProvidesEmitter(const SourceFile *const SF, llvm::raw_ostream &out)
      : SF(SF), out(out) {}

public:
  static void emit(const SourceFile *SF, llvm::raw_ostream &out) {
    ProvidesEmitter(SF, out).emit();
  }

private:
  /// Emit all provided declartions.
  void emit() const;
};

/// Emit the depended-upon declartions.
class DependsEmitter {
  /// The file that dependes upon the declarations.
  const SourceFile *const SF;
  /// The dependencies collected by the compiler.
  const DependencyTracker &depTracker;

  llvm::raw_ostream &out;

  DependsEmitter(const SourceFile *SF, const DependencyTracker &depTracker,
                 llvm::raw_ostream &out)
      : SF(SF), depTracker(depTracker), out(out) {}

public:
  /// \param SF SourceFile containing the dependent code
  /// \param depTracker Contains the dependencies found during compilation
  /// \param out Where the dependencies are emitted
  static void emit(const SourceFile *SF, const DependencyTracker &depTracker,
                   llvm::raw_ostream &out) {
    DependsEmitter(SF, depTracker, out).emit();
  }

private:
  /// Emit all the dependencies.
  void emit() const;
};
} // namespace

bool ReferenceDependenciesEmitter::emit(DiagnosticEngine &diags,
                                        SourceFile *const SF,
                                        const DependencyTracker &depTracker,
                                        StringRef outputPath) {
  // Before writing to the dependencies file path, preserve any previous file
  // that may have been there. No error handling -- this is just a nicety, it
  // doesn't matter if it fails.
  llvm::sys::fs::rename(outputPath, outputPath + "~");
  return withOutputFile(diags, outputPath, [&](llvm::raw_pwrite_stream &out) {
    ReferenceDependenciesEmitter::emit(SF, depTracker, out);
    return false;
  });
}

void ReferenceDependenciesEmitter::emit(SourceFile *SF,
                                        const DependencyTracker &depTracker,
                                        llvm::raw_ostream &out) {
  ReferenceDependenciesEmitter(SF, depTracker, out).emit();
}

void ReferenceDependenciesEmitter::emit() const {
  assert(SF && "Cannot emit reference dependencies without a SourceFile");
  out << "### Swift dependencies file v0 ###\n";
  emitProvides();
  emitDepends();
}

void ReferenceDependenciesEmitter::emitProvides() const {
  ProvidesEmitter::emit(SF, out);
}

void ReferenceDependenciesEmitter::emitDepends() const {
  DependsEmitter::emit(SF, depTracker, out);
}

void ProvidesEmitter::emit() const {}

void DependsEmitter::emit() const {}

bool swift::ExperimentalDependencies::emitReferenceDependencies(
    DiagnosticEngine &diags, SourceFile *SF,
    const DependencyTracker &depTracker, StringRef outputPath) {
  return ReferenceDependenciesEmitter::emit(diags, SF, depTracker, outputPath);
}
