//===--- SyntacticMigratorPass.h --------------------------------*- C++ -*-===//
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
// A base class for a syntactic migrator pass that uses the temporary
// swift::migrator::EditorAdapter infrastructure.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_MIGRATOR_SYNTACTICMIGRATORPASS_H
#define SWIFT_MIGRATOR_SYNTACTICMIGRATORPASS_H

#include "swift/Migrator/EditorAdapter.h"

namespace swift {
class SourceManager;
struct MigratorOptions;

namespace migrator {
class SyntacticMigratorPass {
  struct Implementation;
  Implementation &Impl;
public:
  SyntacticMigratorPass(EditorAdapter &Editor, SourceFile *SF,
                        const MigratorOptions &Opts);
  ~SyntacticMigratorPass();
  void run();
  const clang::edit::Commit &getEdits() const;
};

} // end namespace migrator
} // end namespace swift

#endif // SWIFT_MIGRATOR_SYNTACTICMIGRATORPASS_H
