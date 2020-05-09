//===--- Migrator.h - Migrator ----------------------------------*- C++ -*-===//
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
//
// The top-level Swift Migrator driver.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_MIGRATOR_MIGRATOR_H
#define SWIFT_MIGRATOR_MIGRATOR_H

#include "swift/Migrator/MigrationState.h"
#include "swift/Syntax/References.h"

namespace swift {
class CompilerInstance;

namespace migrator {

/// If needed, run the migrator on the compiler invocation's input file and emit
/// a "replacement map" describing the requested changes to the source file.
/// \return true on error.
bool updateCodeAndEmitRemapIfNeeded(CompilerInstance *Instance);

/// Specify options when running syntactic migration pass.
struct SyntacticPassOptions {
  bool RunOptionalTryMigration = false;
};

struct Migrator {
  CompilerInstance *StartInstance;
  const CompilerInvocation &StartInvocation;
  SourceManager SrcMgr;
  std::vector<RC<MigrationState>> States;

  Migrator(CompilerInstance *StartInstance,
           const CompilerInvocation &StartInvocation);

  /// The maximum number of times to run the compiler over the input to get
  /// fix-its. Nullability changes may expose other fix-its in subsequent
  /// compilations.
  static constexpr unsigned MaxCompilerFixitPassIterations = 7;

  /// Repeatedly perform a number of compiler-fix-it migrations in a row, until
  /// there are no new suggestions from the compiler or some other error
  /// occurred.
  ///
  /// Returns the last CompilerInstance used in the iterations, provided
  /// that the CompilerInvocation used to set it up was successful. Otherwise,
  /// returns nullptr.
  std::unique_ptr<swift::CompilerInstance>
  repeatFixitMigrations(const unsigned Iterations,
                        swift::version::Version SwiftLanguageVersion);

  /// Perform a single compiler fix-it migration on the last state, and push
  /// the result onto the state history.
  ///
  /// Returns the CompilerInstance used for the fix-it run, provided its
  /// setup from a CompilerInvocation was successful.
  std::unique_ptr<swift::CompilerInstance>
  performAFixItMigration(swift::version::Version SwiftLanguageVersion);

  /// Starting with the last state, perform the following migration passes.
  ///
  /// Returns true if failed:
  ///   - Setting up the Swift CompilerInstance failed.
  ///   - performSema emitted fatal errors along the way.
  bool performSyntacticPasses(SyntacticPassOptions Opts);

  /// Emit a replacement map from the very start state's output text to the
  /// final state's output text to the StartInvocation's output file.
  bool emitRemap() const;

  /// Emit the output text of the final state in States to the path specified
  /// by -emit-migrated-file-path in StartInvocation.MigratorOptions.
  ///
  /// Returns true if an attempt was made and failed.
  bool emitMigratedFile() const;

  /// Dump all of the migration states encountered so far to
  /// StartInvocation.MigratorOptions.DumpMigrationStatesDir.
  ///
  /// Returns true if an attempt was made and failed.
  bool dumpStates() const;

  /// Get the options for the Migrator.
  const MigratorOptions &getMigratorOptions() const;

  /// Get the filename of the input given by this->StartInvocation.
  const StringRef getInputFilename() const;
};

} // end namespace migrator
} // end namespace swift

#endif // SWIFT_MIGRATOR_MIGRATOR_H
