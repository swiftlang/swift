//===-- Frontend.cpp - frontend utility methods ---------------------------===//
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
//
// This file contains utility methods for parsing and performing semantic
// on modules.
//
//===----------------------------------------------------------------------===//

#include "Frontend.h"
#include "Immediate.h"
#include "swift/AST/Identifier.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Component.h"
#include "swift/AST/Diagnostics.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Module.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/Pattern.h"
#include "swift/AST/Stmt.h"
#include "swift/AST/Types.h"
#include "swift/Parse/Lexer.h"
#include "swift/Subsystems.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/SourceMgr.h"

using namespace swift;

bool swift::appendToREPLTranslationUnit(TranslationUnit *TU,
                                        REPLContext &RC,
                                        llvm::MemoryBuffer *Buffer,
                                        unsigned &BufferOffset,
                                        unsigned BufferEndOffset) {
  assert(TU->Kind == TranslationUnit::REPL && "Can't append to a non-REPL TU");
  
  RC.CurBufferID
    = TU->getASTContext().SourceMgr.AddNewSourceBuffer(Buffer, llvm::SMLoc());
  
  bool FoundAnySideEffects = false;
  unsigned CurTUElem = RC.CurTUElem;
  do {
    FoundAnySideEffects |= parseIntoTranslationUnit(TU, RC.CurBufferID,
                                                    &BufferOffset,
                                                    BufferEndOffset);
    performTypeChecking(TU, CurTUElem);
    CurTUElem = TU->Decls.size();
  } while (BufferOffset != BufferEndOffset);
  return FoundAnySideEffects;
}
