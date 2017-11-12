//===--- MigrationState.h - Migration State ---------------------*- C++ -*-===//
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

#include "swift/Syntax/References.h"
#include "llvm/ADT/IntrusiveRefCntPtr.h"
#include "llvm/ADT/StringRef.h"

namespace swift {

class SourceManager;

namespace migrator {

enum class MigrationKind {
  /// The start state of the migrator.
  Start,

  /// A syntactic pass. This is generic for now because there isn't a good
  /// way to separate out and compose syntactic passes until lib/Syntax is
  /// integrated.
  Syntactic,

  /// The compiler has made several passes over the input file and we
  /// applied the suggested fix-its we deemed appropriate.
  CompilerFixits,
};

struct MigrationState : public llvm::ThreadSafeRefCountedBase<MigrationState> {
  MigrationKind Kind;
  SourceManager &SrcMgr;
  unsigned InputBufferID;
  unsigned OutputBufferID;

  MigrationState(const MigrationKind Kind, SourceManager &SrcMgr,
                 const unsigned InputBufferID, const unsigned OutputBufferID)
    : Kind(Kind), SrcMgr(SrcMgr),
      InputBufferID(InputBufferID),
      OutputBufferID(OutputBufferID) {}

  MigrationKind getKind() const {
    return Kind;
  }

  std::string getInputText() const;

  unsigned getInputBufferID() const {
    return InputBufferID;
  }

  std::string getOutputText() const;

  unsigned getOutputBufferID() const {
    return OutputBufferID;
  }

  /// Write all relevant information about the state to OutDir, such as the
  /// input file, output file, replacements, syntax trees, etc.
  bool print(size_t StateNumber, StringRef OutDir) const;

  bool noChangesOccurred() const {
    return InputBufferID == OutputBufferID;
  }

  static RC<MigrationState>
  start(SourceManager &SrcMgr, const unsigned InputBufferID) {
    return RC<MigrationState> {
      new MigrationState {
        MigrationKind::Start, SrcMgr, InputBufferID, InputBufferID
      }
    };
  }

  static RC<MigrationState>
  make(MigrationKind Kind, SourceManager &SrcMgr, const unsigned InputBufferID,
       const unsigned OutputBufferID) {
    return RC<MigrationState> {
      new MigrationState {
        Kind,
        SrcMgr,
        InputBufferID,
        // The input is the output here, because nothing happened yet.
        OutputBufferID
      }
    };
  }
};

}
}
#endif // SWIFT_MIGRATOR_MIGRATIONSTATE_H
