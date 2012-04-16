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
#include "swift/AST/Identifier.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Component.h"
#include "swift/AST/Diagnostics.h"
#include "swift/AST/Module.h"
#include "swift/AST/Stmt.h"
#include "swift/Parse/Lexer.h"
#include "swift/Subsystems.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/PathV2.h"
#include "llvm/Support/SourceMgr.h"

using namespace swift;

static Identifier getModuleIdentifier(const llvm::MemoryBuffer *Buffer, ASTContext &Context) {
  StringRef moduleName = Buffer->getBufferIdentifier();

  // As a special case, recognize <stdin>.
  if (moduleName == "<stdin>")
    return Context.getIdentifier("stdin");

  // Find the stem of the filename.
  moduleName = llvm::sys::path::stem(moduleName);

  // Complain about non-identifier characters in the module name.
  if (!Lexer::isIdentifier(moduleName)) {
    SourceLoc Loc = SourceLoc(llvm::SMLoc::getFromPointer(Buffer->getBuffer().begin()));
    Context.Diags.diagnose(Loc, diag::bad_module_name);
    moduleName = "bad";
  }

  return Context.getIdentifier(moduleName);
}

TranslationUnit*
swift::buildSingleTranslationUnit(ASTContext &Context, unsigned BufferID,
                                  bool ParseOnly, bool IsMainModule) {
  Component *Comp = new (Context.Allocate<Component>(1)) Component();
  const llvm::MemoryBuffer *Buffer = Context.SourceMgr.getMemoryBuffer(BufferID);
  Identifier ID = getModuleIdentifier(Buffer, Context);
  TranslationUnit *TU = new (Context) TranslationUnit(ID, Comp, Context,
                                                      IsMainModule);

  unsigned BufferOffset = 0;
  unsigned CurTUElem = 0;
  bool CompleteParse;
  do {
    CompleteParse = parseIntoTranslationUnit(TU, BufferID, &BufferOffset);
    if (!ParseOnly) {
      performNameBinding(TU, CurTUElem);
      performTypeChecking(TU, CurTUElem);
      CurTUElem = TU->Body->getNumElements();
    }
  } while (!CompleteParse);

  return TU;
}

void swift::appendToMainTranslationUnit(TranslationUnit *TU, unsigned BufferID) {
  unsigned BufferOffset = 0;
  unsigned CurTUElem = TU->Body->getNumElements();
  bool CompleteParse;
  do {
    CompleteParse = parseIntoTranslationUnit(TU, BufferID, &BufferOffset);
    performNameBinding(TU, CurTUElem);
    performTypeChecking(TU, CurTUElem);
    CurTUElem = TU->Body->getNumElements();
  } while (!CompleteParse);
}
