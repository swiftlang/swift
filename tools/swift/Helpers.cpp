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

#include "Helpers.h"
#include "Immediate.h"
#include "swift/SIL/SILModule.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Module.h"
#include "swift/Basic/SourceManager.h"
#include "swift/Parse/PersistentParserState.h"
#include "swift/Subsystems.h"
using namespace swift;

bool swift::appendToREPLTranslationUnit(TranslationUnit *TU,
                                        REPLContext &RC,
                                        llvm::MemoryBuffer *Buffer) {
  assert(TU->Kind == TranslationUnit::REPL && "Can't append to a non-REPL TU");
  
  RC.CurBufferID
    = TU->getASTContext().SourceMgr->AddNewSourceBuffer(Buffer, llvm::SMLoc());
  
  bool FoundAnySideEffects = false;
  unsigned CurTUElem = RC.CurTUElem;
  PersistentParserState PersistentState;
  bool Done;
  do {
    FoundAnySideEffects |=
        parseIntoTranslationUnit(TU, RC.CurBufferID, &Done, nullptr,
                                 &PersistentState);
    performTypeChecking(TU, CurTUElem);
    CurTUElem = TU->Decls.size();
  } while (!Done);
  return FoundAnySideEffects;
}


/// \brief Returns true if the diagnostic passes produced an error.
bool swift::runSILDiagnosticPasses(SILModule &Module) {
  auto &Ctx = Module.getASTContext();
  performSILMandatoryInlining(&Module);

  performSILDefiniteInitialization(&Module);
  performSILAllocBoxToStackPromotion(&Module);
  performInOutDeshadowing(&Module);

  performSILConstantPropagation(&Module);
  performSILDeadCodeElimination(&Module);

  // Generate diagnostics.
  emitSILDataflowDiagnostics(&Module);

  Module.setStage(SILStage::Canonical);

  // If errors were produced during SIL analysis, return true.
  return Ctx.hadError();
}

bool swift::runSILOptimizationPasses(SILModule &Module) {
  auto &Ctx = Module.getASTContext();

  // If errors were produced during SIL optimization, return true.
  return Ctx.hadError();
}
