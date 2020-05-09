//===--- FixitApplyDiagnosticConsumer.h -------------------------*- C++ -*-===//
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
#include "swift/Migrator/FixitFilter.h"
#include "swift/Migrator/Migrator.h"
#include "swift/Migrator/Replacement.h"
#include "clang/Rewrite/Core/RewriteBuffer.h"
#include "llvm/ADT/DenseSet.h"

namespace swift {

class CompilerInvocation;
struct DiagnosticInfo;
struct MigratorOptions;
class SourceManager;

namespace migrator {

struct Replacement;

class FixitApplyDiagnosticConsumer final
  : public DiagnosticConsumer, public FixitFilter {
  clang::RewriteBuffer RewriteBuf;

  /// The entire text of the input file.
  const StringRef Text;

  /// The name of the buffer, which should be the absolute path of the input
  /// filename.
  const StringRef BufferName;

  /// The number of fix-its pushed into the rewrite buffer. Use this to
  /// determine whether to call `printResult`.
  unsigned NumFixitsApplied;

  /// Tracks previous replacements so we don't pump the rewrite buffer with
  /// multiple equivalent replacements, which can result in weird behavior.
  llvm::SmallSet<Replacement, 32> Replacements;

public:
  FixitApplyDiagnosticConsumer(const StringRef Text,
                               const StringRef BufferName);

  /// Print the resulting text, applying the caught fix-its to the given
  /// output stream.
  void printResult(llvm::raw_ostream &OS) const;

  void handleDiagnostic(SourceManager &SM, const DiagnosticInfo &Info) override;

  unsigned getNumFixitsApplied() const {
    return NumFixitsApplied;
  }
};
} // end namespace migrator
} // end namespace swift

#endif // SWIFT_MIGRATOR_FIXITAPPLYDIAGNOSTICCONSUMER_H
