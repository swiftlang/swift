//===-- Helpers.cpp - frontend utility methods ----------------------------===//
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

#include "swift/Immediate/Helpers.h"

#include "swift/Immediate/Immediate.h"
#include "swift/SIL/SILModule.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Module.h"
#include "swift/Basic/SourceManager.h"
#include "swift/Parse/PersistentParserState.h"
#include "swift/Subsystems.h"
#include "swift/SILPasses/Passes.h"
using namespace swift;

bool swift::appendToREPLFile(SourceFile &SF,
                             PersistentParserState &PersistentState,
                             REPLContext &RC,
                             std::unique_ptr<llvm::MemoryBuffer> Buffer) {
  assert(SF.Kind == SourceFileKind::REPL && "Can't append to a non-REPL file");

  SourceManager &SrcMgr = SF.getParentModule()->getASTContext().SourceMgr;
  RC.CurBufferID = SrcMgr.addNewSourceBuffer(std::move(Buffer));

  bool FoundAnySideEffects = false;
  unsigned CurElem = RC.CurElem;
  bool Done;
  do {
    FoundAnySideEffects |=
        parseIntoSourceFile(SF, RC.CurBufferID, &Done, nullptr,
                            &PersistentState);
    performTypeChecking(SF, PersistentState.getTopLevelContext(), None,
                        CurElem);
    CurElem = SF.Decls.size();
  } while (!Done);
  return FoundAnySideEffects;
}
