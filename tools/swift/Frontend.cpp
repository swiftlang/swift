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

static Identifier getModuleIdentifier(StringRef OutputName,
                                      ASTContext &Context,
                                      TranslationUnit::TUKind moduleKind) {
  StringRef moduleName = OutputName;

  // As a special case, recognize <stdin>.
  if (moduleName == "<stdin>")
    return Context.getIdentifier("stdin");

  // Find the stem of the filename.
  moduleName = llvm::sys::path::stem(moduleName);

  // Complain about non-identifier characters in the module name.
  if (!Lexer::isIdentifier(moduleName)) {
    if (moduleKind == TranslationUnit::Main) {
      moduleName = "main";
    } else {
      SourceLoc Loc;
      Context.Diags.diagnose(Loc, diag::bad_module_name, moduleName);
      moduleName = "bad";
    }
  }

  return Context.getIdentifier(moduleName);
}


/// "SIL" is non-null when we're parsing a .sil file instead of a .swift file.
TranslationUnit*
swift::buildSingleTranslationUnit(ASTContext &Context,
                                  StringRef OutputName,
                                  ArrayRef<unsigned> BufferIDs,
                                  bool ParseOnly, TranslationUnit::TUKind Kind,
                                  SILModule *SIL) {
  Component *Comp = new (Context.Allocate<Component>(1)) Component();
  Identifier ID = getModuleIdentifier(OutputName, Context, Kind);
  TranslationUnit *TU = new (Context) TranslationUnit(ID, Comp, Context, Kind);
  Context.LoadedModules[ID.str()] = TU;

  // If we're in SIL mode, don't auto import any libraries.
  if (Kind != TranslationUnit::SIL)
    performAutoImport(TU);

  unsigned CurTUElem = 0;
  for (auto &BufferID : BufferIDs) {
    unsigned BufferOffset = 0;
    const llvm::MemoryBuffer *Buffer =
      Context.SourceMgr.getMemoryBuffer(BufferID);
    do {
      parseIntoTranslationUnit(TU, BufferID, &BufferOffset, 0, SIL);
      if (!ParseOnly && BufferIDs.size() == 1) {
        performNameBinding(TU, CurTUElem);
        performTypeChecking(TU, CurTUElem);
        CurTUElem = TU->Decls.size();
      }
    } while (BufferOffset != Buffer->getBufferSize());
  }

  if (!ParseOnly && BufferIDs.size() > 1) {
    performNameBinding(TU);
    performTypeChecking(TU);
  }

  return TU;
}

bool swift::appendToREPLTranslationUnit(TranslationUnit *TU,
                                        REPLContext &RC,
                                        llvm::MemoryBuffer *Buffer,
                                        unsigned &BufferOffset,
                                        unsigned BufferEndOffset) {
  assert(TU->Kind == TranslationUnit::Repl && "Can't append to a non-REPL TU");
  
  RC.CurBufferID
    = TU->getASTContext().SourceMgr.AddNewSourceBuffer(Buffer, llvm::SMLoc());
  
  bool FoundAnySideEffects = false;
  unsigned CurTUElem = RC.CurTUElem;
  do {
    FoundAnySideEffects |= parseIntoTranslationUnit(TU, RC.CurBufferID,
                                                    &BufferOffset,
                                                    BufferEndOffset);
    performNameBinding(TU, CurTUElem);
    performTypeChecking(TU, CurTUElem);
    CurTUElem = TU->Decls.size();
  } while (BufferOffset != BufferEndOffset);
  return FoundAnySideEffects;
}
