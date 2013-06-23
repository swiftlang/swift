//===--- IRGenModule.cpp - Swift Global LLVM IR Generation ----------------===//
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
//  This file implements IR generation for global declarations in Swift.
//
//===----------------------------------------------------------------------===//

#include "IRGenDebugInfo.h"
#include "swift/AST/Expr.h"
#include "swift/SIL/SILDebugScope.h"
#include "llvm/DebugInfo.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/Support/Dwarf.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/IR/Module.h"


namespace swift {
namespace irgen {


IRGenDebugInfo::IRGenDebugInfo(llvm::SourceMgr &SM, llvm::Module &M)
  : SM(SM), DBuilder(M) {
  std::string MainFileName = "<unknown>";//CGM.getCodeGenOpts().MainFileName;

  // Save filename string.
  char *FilenamePtr = DebugInfoNames.Allocate<char>(MainFileName.length());
  memcpy(FilenamePtr, MainFileName.c_str(), MainFileName.length());
  StringRef Filename(FilenamePtr, MainFileName.length());

  // FIXME: If we use a new enum value here, we hit an assert in LLVM.
  unsigned Lang = llvm::dwarf::DW_LANG_Python;
  StringRef Dir = getCurrentDirname();
  // FIXME.
  StringRef Producer = StringRef();
  // FIXME.
  bool IsOptimized = false;

  // FIXME.
  StringRef Flags = StringRef();

  // FIXME.
  unsigned RuntimeVersion = 1;

  // FIXME.
  StringRef SplitName = StringRef();
  DBuilder.createCompileUnit(Lang, Filename, Dir, Producer,
                             IsOptimized, Flags, RuntimeVersion,
                             SplitName);

}


typedef struct {
  unsigned Line, Col;
  const char* Filename;
} Location;

/// Use the SM to figure out the actual line/column of a SourceLoc.
template<typename WithLoc>
Location getStartLoc(llvm::SourceMgr& SM, WithLoc *S) {
  Location L = {0};

  SourceLoc Start = S->getStartLoc();
  int BufferIndex = SM.FindBufferContainingLoc(Start.Value);
  if (BufferIndex == -1)
    return L;

  L.Filename = SM.getMemoryBuffer((unsigned)BufferIndex)->getBufferIdentifier();
  L.Line = SM.FindLineNumber(Start.Value, BufferIndex);
  return L;
}

/// getStartLoc - extract the start location from a SILLocation.
static Location getStartLoc(llvm::SourceMgr& SM, SILLocation Loc) {
  if (Expr* E = Loc.dyn_cast<Expr*>())
    return getStartLoc(SM, E);

  if (Stmt* S = Loc.dyn_cast<Stmt*>())
    return getStartLoc(SM, S);

  if (Decl* D = Loc.dyn_cast<Decl*>())
    return getStartLoc(SM, D);

  Location None = {0};
  return None;
}

void IRGenDebugInfo::SetCurrentLoc(IRBuilder& Builder, SILLocation Loc,
                                   SILDebugScope *DS) {
  Location L = getStartLoc(SM, Loc);
  // No location: we should stick with the previous location.
  if (L.Line == 0)
    return;

  llvm::DIDescriptor Scope = getOrCreateScope(DS);
  llvm::MDNode *InlinedAt = 0;
  llvm::DebugLoc DL = llvm::DebugLoc::get(L.Line, L.Col, Scope, InlinedAt);
  Builder.SetCurrentDebugLocation(DL);
}

/// getOrCreateScope - Translate a SILDebugScope into an llvm::DIDescriptor.
llvm::DIDescriptor IRGenDebugInfo::getOrCreateScope(SILDebugScope *DS) {
  if (DS == 0)
    return llvm::DIDescriptor();

  // Try to find it in the cache first.
  llvm::DenseMap<SILDebugScope*, llvm::DIDescriptor>::iterator
    CachedScope = ScopeMap.find(DS);
  if (CachedScope != ScopeMap.end()) {
    return CachedScope->second;
  }

  // Create a new location.
  Location L = getStartLoc(SM, DS->Loc);
  llvm::DIFile File = getOrCreateFile(L.Filename);
  llvm::DIDescriptor Parent = getOrCreateScope(DS->Parent);
  if (Parent == 0) Parent = File;

  llvm::DILexicalBlock DScope =
    DBuilder.createLexicalBlock(Parent, File, L.Line, L.Col);

  // Cache it.
  ScopeMap[DS] = DScope;
  return DScope;
}

/// getCurrentDirname - Return the current working directory.
StringRef IRGenDebugInfo::getCurrentDirname() {
  // FIXME.
  //if (!CGM.getCodeGenOpts().DebugCompilationDir.empty())
  //  return CGM.getCodeGenOpts().DebugCompilationDir;

  if (!CWDName.empty())
    return CWDName;
  llvm::SmallString<256> CWD;
  llvm::sys::fs::current_path(CWD);
  char *CompDirnamePtr = DebugInfoNames.Allocate<char>(CWD.size());
  memcpy(CompDirnamePtr, CWD.data(), CWD.size());
  return CWDName = StringRef(CompDirnamePtr, CWD.size());
}

/// getOrCreateFile - Translate filenames into DIFiles.
llvm::DIFile IRGenDebugInfo::getOrCreateFile(const char *filename) {
  // Look in the cache first.
  llvm::DenseMap<const char *, llvm::WeakVH>::iterator it =
    DIFileCache.find(filename);

  if (it != DIFileCache.end()) {
    // Verify that the information still exists.
    if (llvm::Value *V = it->second)
      return llvm::DIFile(cast<llvm::MDNode>(V));
  }

  // Create a new one.
  llvm::DIFile F = DBuilder.createFile(filename, getCurrentDirname());

  // Cache it.
  DIFileCache[filename] = F;
  return F;
}


} // irgen
} // swift
