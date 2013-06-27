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
#include "swift/IRGen/Options.h"
#include "swift/SIL/SILDebugScope.h"
#include "llvm/config/config.h"
#include "llvm/DebugInfo.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/Support/Dwarf.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/IR/Module.h"


namespace swift {
namespace irgen {

/// Strdup S using the bump pointer.
static
StringRef BumpAllocatedString(std::string S, llvm::BumpPtrAllocator &BP) {
  char *Ptr = BP.Allocate<char>(S.length());
  memcpy(Ptr, S.c_str(), S.length());
  return StringRef(Ptr, S.length());
}

IRGenDebugInfo::IRGenDebugInfo(const Options &Opts, llvm::SourceMgr &SM,
                               llvm::Module &M)
  : SM(SM), DBuilder(M), Opts(Opts) {
  assert(Opts.DebugInfo);
  std::string MainFileName = Opts.MainInputFilename;
  if (MainFileName.empty())
    MainFileName = "<unknown>";

  StringRef Filename = BumpAllocatedString(MainFileName, DebugInfoNames);
  // DW_LANG_Haskell+1 = 0x19 is the first unused language value in DWARF 5.
  // But for now, we need to use a constant in
  // DW_LANG_lo_user..DW_LANG_hi_user, because LLVM will assert on that.
  unsigned Lang = 0x9999 /*llvm::dwarf::DW_LANG_Swift*/;
  StringRef Dir = getCurrentDirname();

  std::string buf;
  llvm::raw_string_ostream OS(buf);
  OS << "Swift version ? (based on LLVM " << PACKAGE_VERSION << ")";
  StringRef Producer = BumpAllocatedString(OS.str(), DebugInfoNames);

  bool IsOptimized = Opts.OptLevel > 0;

  // FIXME.
  StringRef Flags = StringRef();

  // FIXME.
  unsigned RuntimeVersion = 1;

  // FIXME.
  StringRef SplitName = StringRef();
  DBuilder.createCompileUnit(Lang, Filename, Dir, Producer,
                             IsOptimized, Flags, RuntimeVersion,
                             SplitName);
  TheCU = llvm::DICompileUnit(DBuilder.getCU());
}


typedef struct {
  unsigned Line, Col;
  const char* Filename;
} Location;

/// Use the SM to figure out the actual line/column of a SourceLoc.
template<typename WithLoc>
Location getStartLoc(llvm::SourceMgr& SM, WithLoc *S) {
  Location L = {};
  if (S == nullptr) return L;

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

  Location None = {};
  return None;
}

void IRGenDebugInfo::setCurrentLoc(IRBuilder& Builder, SILLocation Loc,
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
  if (Parent == 0)
    Parent = File;

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
  return BumpAllocatedString(CWD.str(), DebugInfoNames);
}

/// getOrCreateFile - Translate filenames into DIFiles.
llvm::DIFile IRGenDebugInfo::getOrCreateFile(const char *Filename) {
  if (!Filename)
    return llvm::DIFile();

  // Look in the cache first.
  llvm::DenseMap<const char *, llvm::WeakVH>::iterator it =
    DIFileCache.find(Filename);

  if (it != DIFileCache.end()) {
    // Verify that the information still exists.
    if (llvm::Value *V = it->second)
      return llvm::DIFile(cast<llvm::MDNode>(V));
  }

  // Create a new one.
  llvm::SmallString<64>  File = llvm::sys::path::filename(Filename);
  llvm::SmallString<512> Path(Filename);
  llvm::sys::path::remove_filename(Path);
  llvm::error_code ec = llvm::sys::fs::make_absolute(Path);
  // Basically ignore any error.
  assert(ec == llvm::errc::success);
  // remove any trailing path separator
  llvm::sys::path::remove_filename(Path);
  llvm::DIFile F = DBuilder.createFile(File, Path);

  // Cache it.
  DIFileCache[Filename] = F;
  return F;
}

/// Attempt to figure out the unmangled name of a function.
static StringRef getName(SILLocation L) {
  if (Expr* E = L.dyn_cast<Expr*>())
    if (FuncExpr* FE = dyn_cast<FuncExpr>(E))
      if (FuncDecl* FD = FE->getDecl())
        return FD->getName().str();

  if (Decl* D = L.dyn_cast<Decl*>())
    if (FuncDecl* FD = dyn_cast<FuncDecl>(D))
      return FD->getName().str();

  return StringRef();
}

void IRGenDebugInfo::createFunction(SILDebugScope* DS,
                                    llvm::Function *Fn) {
  StringRef Name;
  Location L = {};
  if (DS) {
    L = getStartLoc(SM, DS->Loc);
    Name = getName(DS->Loc);
  }
  StringRef LinkageName = Fn->getName();
  llvm::DIFile File = getOrCreateFile(L.Filename);
  llvm::DIDescriptor Scope = TheCU;
  unsigned Line = L.Line;

  // We don't support debug info for types.
  llvm::DIArray ParameterTypes;
  llvm::DICompositeType FnType =
    DBuilder.createSubroutineType(File, ParameterTypes);
  llvm::DIArray TParams;
  llvm::DISubprogram Decl;

  // Various flags
  bool isLocalToUnit = false;
  bool isDefinition = true;
  unsigned Flags = 0;
  if (Name.empty())
    Flags |= llvm::DIDescriptor::FlagArtificial;
  bool isOptimized = Opts.OptLevel > 0;

  llvm::DISubprogram SP =
    DBuilder.createFunction(Scope, Name, LinkageName, File, Line,
                            FnType, isLocalToUnit, isDefinition,
                            /*ScopeLine =*/Line,
                            Flags, isOptimized, Fn, TParams, Decl);
  ScopeMap[DS] = SP;
}

void IRGenDebugInfo::Finalize() {
  DBuilder.finalize();
}

} // irgen
} // swift
