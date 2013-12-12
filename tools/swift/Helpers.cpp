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
#include "swift/SILPasses/Passes.h"
using namespace swift;

bool swift::appendToREPLFile(SourceFile &SF, REPLContext &RC,
                             llvm::MemoryBuffer *Buffer) {
  assert(SF.Kind == SourceFileKind::REPL && "Can't append to a non-REPL file");

  SourceManager &SrcMgr = SF.getParentModule()->Ctx.SourceMgr;
  RC.CurBufferID = SrcMgr.addNewSourceBuffer(Buffer);
  
  bool FoundAnySideEffects = false;
  unsigned CurElem = RC.CurElem;
  PersistentParserState PersistentState;
  bool Done;
  do {
    FoundAnySideEffects |=
        parseIntoSourceFile(SF, RC.CurBufferID, &Done, nullptr,
                            &PersistentState);
    performTypeChecking(SF, CurElem);
    CurElem = SF.Decls.size();
  } while (!Done);
  return FoundAnySideEffects;
}


/// \brief Returns true if the diagnostic passes produced an error.
bool swift::runSILDiagnosticPasses(SILModule &Module) {
  // If we parsed a .sil file that is already in canonical form, don't rerun
  // the diagnostic passes.
  if (Module.getStage() == SILStage::Canonical)
    return false;
  
  auto &Ctx = Module.getASTContext();
 
  performSILMandatoryInlining(&Module);

  performSILCapturePromotion(&Module);
  performSILAllocBoxToStackPromotion(&Module);
  performInOutDeshadowing(&Module);
  performSILDefiniteInitialization(&Module);
  performSILPredictableMemoryOptimizations(&Module);

  performSILConstantPropagation(&Module);
  performSILDeadCodeElimination(&Module);

  // Generate diagnostics.
  emitSILDataflowDiagnostics(&Module);

  Module.setStage(SILStage::Canonical);

  // If errors were produced during SIL analysis, return true.
  return Ctx.hadError();
}

void swift::runSILOptimizationPasses(SILModule &Module) {
  performSILMem2Reg(&Module);
  performSILCSE(&Module);
  performSILCombine(&Module);
  performSimplifyCFG(&Module);
  performSILSpecialization(&Module);
}
