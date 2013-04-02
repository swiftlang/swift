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
#include "llvm/Support/PathV2.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/raw_ostream.h"

using namespace swift;

static Identifier getModuleIdentifier(std::string OutputName,
                                      ASTContext &Context,
                                      bool IsMainModule) {
  StringRef moduleName = OutputName;

  // As a special case, recognize <stdin>.
  if (moduleName == "<stdin>")
    return Context.getIdentifier("stdin");

  // Find the stem of the filename.
  moduleName = llvm::sys::path::stem(moduleName);

  // Complain about non-identifier characters in the module name.
  if (!Lexer::isIdentifier(moduleName)) {
    if (IsMainModule) {
      moduleName = "main";
    } else {
      SourceLoc Loc;
      Context.Diags.diagnose(Loc, diag::bad_module_name);
      moduleName = "bad";
    }
  }

  return Context.getIdentifier(moduleName);
}

TranslationUnit*
swift::buildSingleTranslationUnit(ASTContext &Context,
                                  std::string OutputName,
                                  std::vector<unsigned> BufferIDs,
                                  bool ParseOnly, bool IsMainModule) {
  Component *Comp = new (Context.Allocate<Component>(1)) Component();
  Identifier ID = getModuleIdentifier(OutputName, Context, IsMainModule);
  TranslationUnit *TU = new (Context) TranslationUnit(ID, Comp, Context,
                                                      IsMainModule,
                                                      /*IsReplModule=*/false);
  Context.LoadedModules[ID.str()] = TU;

  unsigned CurTUElem = 0;
  for (auto &BufferID : BufferIDs) {
    unsigned BufferOffset = 0;
    const llvm::MemoryBuffer *Buffer =
      Context.SourceMgr.getMemoryBuffer(BufferID);
    do {
      parseIntoTranslationUnit(TU, BufferID, &BufferOffset);
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

static VarDecl *getNextREPLMetavar(DeclContext *DC,
                                   REPLContext &RC,
                                   SourceLoc Loc) {
  static const char * const MetavarPrefix = "r";
  
  ASTContext &C = DC->getASTContext();
  
  llvm::SmallString<4> namebuf;
  llvm::raw_svector_ostream names(namebuf);
  Identifier ident;
  
  bool nameUsed = false;
  do {
    names.flush();
    namebuf.clear();
    
    names << MetavarPrefix << RC.NextResponseVariableIndex++;
  
    ident = C.getIdentifier(names.str());
    UnqualifiedLookup lookup(ident, DC);
    nameUsed = lookup.isSuccess();
  } while (nameUsed);
  
  VarDecl *vd = new (C) VarDecl(Loc, ident,
                                UnstructuredUnresolvedType::get(C),
                                DC);
  vd->setREPLResult(true);
  return vd;
}

static bool shouldBindREPLMetavariableToExpr(Expr *E) {
  // Don't mind REPL metavariables to simple declrefs.
  if (isa<DeclRefExpr>(E))
    return false;
  if (isa<UnresolvedDeclRefExpr>(E))
    return false;
  
  return true;
}

static void reparseREPLMetavariable(TranslationUnit *TU,
                                    REPLContext &RC) {
  // Turn a TopLevelCodeDecl containing an expression into a variable binding.
  if (TU->Decls.size() != RC.CurTUElem + 1)
    return;
  
  auto *TLCD = dyn_cast<TopLevelCodeDecl>(TU->Decls[RC.CurTUElem]);
  if (!TLCD)
    return;
  
  auto *E = TLCD->getBody().dyn_cast<Expr*>();
  if (!E)
    return;

  if (!shouldBindREPLMetavariableToExpr(E))
    return;
        
  ASTContext &C = TU->getASTContext();

  VarDecl *metavar = getNextREPLMetavar(TU, RC, E->getStartLoc());
  Pattern *metavarPat = new (C) NamedPattern(metavar);
  PatternBindingDecl *metavarBinding
    = new (C) PatternBindingDecl(E->getStartLoc(), metavarPat, E, TU);
  
  TU->Decls.pop_back();
  TU->Decls.push_back(metavar);
  TU->Decls.push_back(metavarBinding);
}

bool swift::appendToREPLTranslationUnit(TranslationUnit *TU,
                                        REPLContext &RC,
                                        unsigned &BufferOffset,
                                        unsigned BufferEndOffset) {
  assert(TU->Kind == TranslationUnit::Repl && "Can't append to a non-REPL TU");
  
  bool FoundAnySideEffects = false;
  unsigned CurTUElem = RC.CurTUElem;
  do {
    FoundAnySideEffects |= parseIntoTranslationUnit(TU, RC.BufferID,
                                                    &BufferOffset,
                                                    BufferEndOffset);
    // If we got a top-level expression, transform it into a VarDecl and
    // PatternBinding to bind the result to a metavariable.
    reparseREPLMetavariable(TU, RC);
    
    performNameBinding(TU, CurTUElem);
    performTypeChecking(TU, CurTUElem);
    CurTUElem = TU->Decls.size();
  } while (BufferOffset != BufferEndOffset);
  return FoundAnySideEffects;
}
