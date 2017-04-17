//===--- MigrationState.h - Migration State --------------------*- C++ -*-===//
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
// This class is an explicit container for a state during migration, its input
// and output text, as well as what created this state.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_MIGRATOR_MIGRATIONSTATE_H
#define SWIFT_MIGRATOR_MIGRATIONSTATE_H

#include "swift/Migrator/Replacement.h"
#include "swift/Syntax/References.h"
#include "llvm/ADT/IntrusiveRefCntPtr.h"
#include "llvm/ADT/StringRef.h"

namespace swift {

class SourceManager;

namespace migrator {

enum class MigrationKind {
  /// The compiler has made several passes over the input file and we
  /// applied the suggested fix-its we deemed appropriate.
  CompilerFixits,
};

struct MigrationState : public llvm::ThreadSafeRefCountedBase<MigrationState> {
  const MigrationKind Kind;
  SourceManager &SrcMgr;

  MigrationState(const MigrationKind Kind, SourceManager &SrcMgr)
    : Kind(Kind), SrcMgr(SrcMgr) {}

  MigrationKind getKind() const {
    return Kind;
  }

  std::string getInputText() const;
  std::string getOutputText() const;

  /// Write all relevant information about the state to OutDir, such as the
  /// input file, output file, replacements, syntax trees, etc.
  bool print(size_t StateNumber, StringRef OutDir) const;
};

struct FixitMigrationState final : public MigrationState {
  const unsigned InputBufferID;
  const unsigned OutputBufferID;
  const std::vector<Replacement> Replacements;

  // Other information about the migration?

  FixitMigrationState(SourceManager &SrcMgr,
                      const unsigned InputBufferID,
                      const unsigned OutputBufferID,
                      const std::vector<Replacement> &Replacements)
    : MigrationState(MigrationKind::CompilerFixits, SrcMgr),
      InputBufferID(InputBufferID),
      OutputBufferID(OutputBufferID),
      Replacements(Replacements) {}

  std::string getInputText() const;

  unsigned getInputBufferID() const {
    return InputBufferID;
  }

  std::string getOutputText() const;

  unsigned getOutputBufferID() const {
    return OutputBufferID;
  }

  bool print(size_t StateNumber, StringRef OutDir) const;

  bool outputDiffersFromInput() const {
    return InputBufferID != OutputBufferID;
  }

  /// Dump the remap between the current input and output text.
  void dumpRemap() const;

  static RC<FixitMigrationState>
  make(SourceManager &SrcMgr, const unsigned InputBufferID,
       const unsigned OutputBufferID,
       const std::vector<Replacement> &Replacements) {
    return RC<FixitMigrationState> {
      new FixitMigrationState {
        SrcMgr,
        InputBufferID,
        // The input is the output here, because nothing happened yet.
        OutputBufferID,
        Replacements
      }
    };
  }

  static RC<FixitMigrationState>
  start(SourceManager &SrcMgr, const unsigned InputBufferID) {
    return make(SrcMgr, InputBufferID, InputBufferID, {});
  }

  static bool classof(const MigrationState *MS) {
    return MS->getKind() == MigrationKind::CompilerFixits;
  }
};

}
}
#endif // SWIFT_MIGRATOR_MIGRATIONSTATE_H
