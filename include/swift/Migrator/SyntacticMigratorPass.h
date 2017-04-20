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
#include "clang/Basic/SourceManager.h"

namespace swift {
class SourceManager;

namespace migrator {

class MigratorPass {
protected:
  swift::SourceManager &SwiftSrcMgr

public:
  MigratorPass(SourceManager &SwiftSrcMgr) : SwiftSrcMgr(SwiftSrcMgr) {}
  virtual void run() {}
};

class SyntacticMigratorPass {
protected:
  EditorAdapter &Editor;
  const SourceFile *SF;

public:
  SyntacticMigratorPass(EditorAdapter &Editor,
                        swift::SourceManager &SwiftSrcMgr,
                        clang::SourceManager &ClangSrcMgr,
                        const SourceFile *SF)
    : MigratorPass(SwiftSrcMgr),
      Editor(Editor),
      ClangSrcMgr(ClangSrcMgr),
      Editor(SwiftSrcMgr, ClangSrcMgr),
      SF(SF) {}

  const clang::edit::Commit &getEdits() const {
    return Editor.getEdits();
  }

  virtual void run() = 0;
};

} // end namespae migrator
} // end namespace swift

#endif // SWIFT_MIGRATOR_SYNTACTICMIGRATORPASS_H
