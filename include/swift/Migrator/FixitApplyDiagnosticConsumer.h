//===--- FixitApplyDiagnosticConsumer.h - ----------------------*- C++ -*-===//
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
// This class records compiler interesting fix-its as textual edits.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_MIGRATOR_FIXITAPPLYDIAGNOSTICCONSUMER_H
#define SWIFT_MIGRATOR_FIXITAPPLYDIAGNOSTICCONSUMER_H

#include "swift/AST/DiagnosticConsumer.h"
#include "clang/Rewrite/Core/RewriteBuffer.h"

namespace swift {

class CompilerInvocation;
struct DiagnosticInfo;
struct MigratorOptions;
class SourceManager;

namespace migrator {

class Replacement;

class FixitApplyDiagnosticConsumer final : public DiagnosticConsumer {
  clang::RewriteBuffer RewriteBuf;

  /// The Migrator options collected by the Swift CompilerInvocation,
  /// used to drive decisions about which fix-its to apply.
  const MigratorOptions &MigratorOpts;

  /// The entire text of the input file.
  const StringRef Text;

  /// The name of the buffer, which should be the absolute path of the input
  /// filename.
  const StringRef BufferName;

  /// The number of fix-its pushed into the rewrite buffer. Use this to
  /// determine whether to call `printResult`.
  unsigned NumFixitsApplied;

  /// A list of replacements that have been applied while diagnosing the
  /// input file.
  std::vector<Replacement> Replacements;

  /// Returns true if the fix-it should be applied.
  bool shouldTakeFixit(const DiagnosticInfo &Info,
                       const DiagnosticInfo::FixIt &F) const;

public:
  FixitApplyDiagnosticConsumer(const MigratorOptions &MigratorOpts,
                               const StringRef Text,
                               const StringRef BufferName);

  /// Print the resulting text, applying the caught fix-its to the given
  /// output stream.
  void printResult(llvm::raw_ostream &OS) const;

  const std::vector<Replacement> &getReplacements() const {
    return Replacements;
  }

  void handleDiagnostic(SourceManager &SM, SourceLoc Loc,
                        DiagnosticKind Kind,
                        StringRef FormatString,
                        ArrayRef<DiagnosticArgument> FormatArgs,
                        const DiagnosticInfo &Info) override;

  unsigned getNumFixitsApplied() const {
    return NumFixitsApplied;
  }
};
} // end namespace migrator
} // end namespace swift

#endif // SWIFT_MIGRATOR_FIXITAPPLYDIAGNOSTICCONSUMER_H
