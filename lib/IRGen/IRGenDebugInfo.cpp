//===--- IRGenDebugInfo.h - Debug Info Support-----------------------------===//
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
//  This file implements IR debug info generatio for Swift.
//
//===----------------------------------------------------------------------===//

#include "IRGenDebugInfo.h"
#include "swift/AST/Expr.h"
#include "swift/IRGen/Options.h"
#include "swift/SIL/SILDebugScope.h"
#include "swift/SIL/SILModule.h"
#include "llvm/config/config.h"
#include "llvm/DebugInfo.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/Dwarf.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/IR/Module.h"

using namespace swift;
using namespace irgen;

// DW_LANG_Haskell+1 = 0x19 is the first unused language value in
// DWARF 5.  We can't use it, because LLVM asserts that there are no
// languages >DW_LANG_Python=0x14.  Wouldn't it would be much more
// appropriate to use a constant in DW_LANG_lo_user..DW_LANG_hi_user
// anyway, you may ask? Well, CompileUnit::constructTypeDIE() will
// always use a DW_FORM_data1, which is too small for that range!  And
// by fixing that in LLVM we would hint at developing a new language.
// So instead, let's hijack a language with a very low potential for
// accidental conflicts for now.
static const unsigned DW_LANG_Swift = 0xf; /*llvm::dwarf::DW_LANG_Swift*/;

/// Strdup a raw char array using the bump pointer.
static
StringRef BumpAllocatedString(const char* Data, size_t Length,
                              llvm::BumpPtrAllocator &BP) {
  char *Ptr = BP.Allocate<char>(Length);
  memcpy(Ptr, Data, Length);
  return StringRef(Ptr, Length);
}

/// Strdup std::string S using the bump pointer.
static
StringRef BumpAllocatedString(std::string S, llvm::BumpPtrAllocator &BP) {
  return BumpAllocatedString(S.c_str(), S.length(), BP);
}

/// Strdup StringRef S using the bump pointer.
static
StringRef BumpAllocatedString(StringRef S, llvm::BumpPtrAllocator &BP) {
  return BumpAllocatedString(S.data(), S.size(), BP);
}

IRGenDebugInfo::IRGenDebugInfo(const Options &Opts, llvm::SourceMgr &SM,
                               llvm::Module &M)
  : SM(SM), DBuilder(M), Opts(Opts), LastLoc({}), LastScope(nullptr) {
  assert(Opts.DebugInfo);
  StringRef Dir, Filename;
  std::string MainFilename = Opts.MainInputFilename;
  if (MainFilename.empty()) {
    Filename = "<unknown>";
    Dir = getCurrentDirname();
  } else {
    // Separate path and filename.
    llvm::SmallString<64>  File = llvm::sys::path::filename(MainFilename);
    llvm::SmallString<512> Path(MainFilename);
    llvm::sys::path::remove_filename(Path);
    llvm::sys::fs::make_absolute(Path);
    Filename = BumpAllocatedString(File, DebugInfoNames);
    Dir = BumpAllocatedString(Path, DebugInfoNames);
  }

  unsigned Lang = DW_LANG_Swift;

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

void IRGenDebugInfo::finalize() {
  DBuilder.finalize();
}

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

void IRGenDebugInfo::setCurrentLoc(IRBuilder& Builder,
                                   SILDebugScope *DS,
                                   SILLocation Loc) {
  Location L = getStartLoc(SM, Loc);

  llvm::DIDescriptor Scope = getOrCreateScope(DS);
  if (!Scope.Verify()) return;

  if (L.Filename && L.Filename != getStartLoc(SM, DS->Loc).Filename) {
    // We changed files in the middle of a scope. This happens, for
    // example, when constructors are inlined. Create a new scope to
    // reflect this.
    Scope = DBuilder.createLexicalBlockFile(Scope, getOrCreateFile(L.Filename));
  }

  if (L.Line == 0 && DS == LastScope) {
    // Reuse the last source location if we are still in the same
    // scope to get a more contiguous line table.
    L.Line = LastLoc.Line;
    L.Col = LastLoc.Col;
  }
  LastLoc = L;
  LastScope = DS;

  llvm::MDNode *InlinedAt = 0;
  auto DL = llvm::DebugLoc::get(L.Line, L.Col, Scope, InlinedAt);
  Builder.SetCurrentDebugLocation(DL);
}

/// getOrCreateScope - Translate a SILDebugScope into an llvm::DIDescriptor.
llvm::DIDescriptor IRGenDebugInfo::getOrCreateScope(SILDebugScope *DS) {
  if (DS == 0)
    return llvm::DIDescriptor();

  // Try to find it in the cache first.
  auto CachedScope = ScopeCache.find(DS);
  if (CachedScope != ScopeCache.end()) {
    return CachedScope->second;
  }

  Location L = getStartLoc(SM, DS->Loc);
  llvm::DIFile File = getOrCreateFile(L.Filename);
  llvm::DIDescriptor Parent = getOrCreateScope(DS->Parent);
  if (Parent == 0)
    Parent = File;

  llvm::DILexicalBlock DScope =
    DBuilder.createLexicalBlock(Parent, File, L.Line, L.Col);

  // Cache it.
  ScopeCache[DS] = DScope;
  return DScope;
}

/// getCurrentDirname - Return the current working directory.
StringRef IRGenDebugInfo::getCurrentDirname() {
  // FIXME: Clang has a global option to set the compilation
  // directory. Do we have something similar for swift?

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
  auto CachedFile =
    DIFileCache.find(Filename);

  if (CachedFile != DIFileCache.end()) {
    // Verify that the information still exists.
    if (llvm::Value *V = CachedFile->second)
      return llvm::DIFile(cast<llvm::MDNode>(V));
  }

  // Create a new one.
  llvm::SmallString<64>  File = llvm::sys::path::filename(Filename);
  llvm::SmallString<512> Path(Filename);
  llvm::sys::path::remove_filename(Path);
  llvm::error_code ec = llvm::sys::fs::make_absolute(Path);
  // Basically ignore any error.
  assert(ec == llvm::errc::success);
  (void)ec; // Silence the unused variable warning
  llvm::DIFile F = DBuilder.createFile(File, Path);

  // Cache it.
  DIFileCache[Filename] = F;
  return F;
}


/// Attempt to figure out the unmangled name of a function.
StringRef IRGenDebugInfo::getName(const FuncDecl& FD) {
  // Getters and Setters are anonymous functions, so we forge a name
  // using its parent declaration.
  if (FD.isGetterOrSetter())
    if (Decl* D = FD.getGetterOrSetterDecl()) {
      if (ValueDecl* VD = dyn_cast<ValueDecl>(D)) {
        bool IsGetter = FD.getGetterDecl();
        llvm::SmallVector<char, 64> Buf;
        StringRef Name = (VD->getName().str() +
                          Twine(IsGetter ? ".get" : ".set")).toStringRef(Buf);
        return BumpAllocatedString(Name, DebugInfoNames);
      }
    }

  if (!FD.getName().empty())
    return FD.getName().str();

  return StringRef();
}

/// Attempt to figure out the unmangled name of a function.
StringRef IRGenDebugInfo::getName(SILLocation L) {
  if (Expr* E = L.dyn_cast<Expr*>())
    if (FuncExpr* FE = dyn_cast<FuncExpr>(E))
      if (FuncDecl* FD = FE->getDecl())
        return getName(*FD);

  if (Decl* D = L.dyn_cast<Decl*>())
    if (FuncDecl* FD = dyn_cast<FuncDecl>(D))
      return getName(*FD);

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
  ScopeCache[DS] = SP;
}

void IRGenDebugInfo::createArtificialFunction(SILModule &SILMod,
                                              IRBuilder &Builder,
                                              llvm::Function *Fn) {
  SILDebugScope *Scope = new (SILMod) SILDebugScope();
  createFunction(Scope, Fn);
  setCurrentLoc(Builder, Scope);
}

void IRGenDebugInfo::emitStackVariableDeclaration(IRBuilder& Builder,
                                                  llvm::Value *Storage,
                                                  DebugTypeInfo Ty,
                                                  const llvm::Twine &Name) {
  emitVariableDeclaration(Builder, Storage, Ty, Name,
                          llvm::dwarf::DW_TAG_auto_variable);
}

/// Return the DIFile that is the ancestor of Scope.
static llvm::DIFile getFile(llvm::DIDescriptor Scope) {
  while (!Scope.isFile()) {
    switch (Scope.getTag()) {
    case llvm::dwarf::DW_TAG_lexical_block:
      Scope = llvm::DILexicalBlock(Scope).getContext();
      break;
    case llvm::dwarf::DW_TAG_subprogram:
      Scope = llvm::DISubprogram(Scope).getContext();
      break;
    default:
      return llvm::DIFile();
    }
    if (Scope.Verify())
      return llvm::DIFile();
  }
  llvm::DIFile File(Scope);
  assert(File.Verify());
  return File;
}

void IRGenDebugInfo::emitVariableDeclaration(IRBuilder& Builder,
                                             llvm::Value *Storage,
                                             DebugTypeInfo Ty,
                                             const llvm::Twine &Name,
                                             unsigned Tag,
                                             unsigned ArgNo) {
  llvm::DebugLoc DL = Builder.getCurrentDebugLocation();
  llvm::DIDescriptor Scope(DL.getScope(Builder.getContext()));
  if (!Scope.Verify())
    return;

  llvm::DIFile Unit = getFile(Scope);
  llvm::DIType DTy = getOrCreateType(Ty, Scope);

  // If there is no debug info for this type then do not emit debug info
  // for this variable.
  if (!DTy)
    return;

  unsigned Line = DL.getLine();
  unsigned Flags = 0;

  // Create the descriptor for the variable.
  llvm::DIVariable D =
    DBuilder.createLocalVariable(Tag, Scope, Name.str(), Unit, Line, DTy,
                                 Opts.OptLevel > 0, Flags, ArgNo);

  // Insert an llvm.dbg.declare into the current block.
  llvm::Instruction *Call =
    DBuilder.insertDeclare(Storage, D, Builder.GetInsertBlock());
  Call->setDebugLoc(llvm::DebugLoc::get(Line, DL.getCol(), Scope));
}

void IRGenDebugInfo::emitGlobalVariableDeclaration(llvm::GlobalValue *Var,
                                                   StringRef Name,
                                                   StringRef LinkageName,
                                                   DebugTypeInfo DebugType,
                                                   SILLocation Loc) {
  Location L = getStartLoc(SM, Loc);
  llvm::DIFile Unit = getOrCreateFile(L.Filename);

  // FIXME: Can there be nested types?
  llvm::DIDescriptor DContext = Unit;
  DBuilder.createStaticVariable(DContext, Name, LinkageName, Unit,
                                L.Line, getOrCreateType(DebugType, Unit),
                                Var->hasInternalLinkage(), Var, nullptr);
}

/// Construct a DIType from a DebugTypeInfo object.
///
/// At this point we do not plan to emit full DWARF for all swift
/// types, the goal is to emit only the name and provenance of the
/// type, where possible. A consumer would then load the type
/// definition directly from the "module" the type is specified in.
llvm::DIType IRGenDebugInfo::createType(DebugTypeInfo Ty,
                                        llvm::DIDescriptor Scope,
                                        llvm::DIFile File) {
  TypeBase* BaseTy = Ty.CanTy.getPointer();
  if (!BaseTy)
    return llvm::DIType();

  StringRef Name;
  uint64_t Size = Ty.SizeInBits;
  uint64_t Align = Ty.AlignmentInBits;
  unsigned Encoding = 0;
  unsigned Flags = 0;

  switch (BaseTy->getKind()) {
  case TypeKind::BuiltinInteger: {
    auto IntTy = BaseTy->castTo<BuiltinIntegerType>();
    Size = IntTy->getBitWidth();
    Name = "Int";
    break;
  }

  case TypeKind::BuiltinFloat: {
    auto FloatTy = BaseTy->castTo<BuiltinFloatType>();
    Size = FloatTy->getBitWidth();
    Name = "Float";
    break;
  }

  // Even builtin swift types usually come boxed in a struct.
  case TypeKind::Struct: {
    auto StructTy = BaseTy->castTo<StructType>();
    if (auto Decl = StructTy->getDecl()) {
      Name = Decl->getName().str();
      Location L = getStartLoc(SM, Decl);
      return DBuilder.createStructType(Scope,
                                       Name,
                                       getOrCreateFile(L.Filename),
                                       L.Line,
                                       Size, Align, Flags,
                                       llvm::DIType(),  // DerivedFrom
                                       llvm::DIArray(), // Elements
                                       DW_LANG_Swift    // RunTimeLang
                                       );
    }
    return llvm::DIType();
  }

  case TypeKind::Class: {
    // Classes are represented as DW_TAG_structure_type. This way the
    // DW_AT_APPLE_runtime_class( DW_LANG_Swift ) attribute can be
    // used to differentiate them from C++ and ObjC classes.
    auto ClassTy = BaseTy->castTo<ClassType>();
    if (auto Decl = ClassTy->getDecl()) {
      Name = Decl->getName().str();
      Location L = getStartLoc(SM, Decl);
      return DBuilder.createStructType(Scope,
                                      Name,
                                      getOrCreateFile(L.Filename),
                                      L.Line,
                                      Size, Align, Flags,
                                      llvm::DIType(),  // DerivedFrom
                                      llvm::DIArray(), // Elements
                                      DW_LANG_Swift    // RunTimeLang
                                      );
    }
    return llvm::DIType();
  }

  // Handle everything else that is based off NominalType.
  case TypeKind::OneOf:
  case TypeKind::Protocol: {
    auto NominalTy = BaseTy->castTo<NominalType>();
    if (auto Decl = NominalTy->getDecl()) {
      Name = Decl->getName().str();
      break;
    }
    return llvm::DIType();
  }
  default:
    return llvm::DIType();
  }
  // FIXME: For Size, clang uses the actual size of the type on the
  // target machine instead of the storage size that is alloca'd in
  // the LLVM IR. To look up the size of the type on the target, clang
  // keeps a Basic/TargetInfo object around.  Right now this is not an
  // actual problem, since all Swift types are Structs anyway, but
  // once we describe the individual fields, this needs to be fixed as
  // LLVM won't even accept a Bool (i1).
  return DBuilder.createBasicType(Name, Size, Align, Encoding);
}

/// Get the DIType corresponding to this DebugTypeInfo from the cache,
/// or build a fresh DIType otherwise.
llvm::DIType IRGenDebugInfo::getOrCreateType(DebugTypeInfo Ty,
                                             llvm::DIDescriptor Scope) {
  // Is this an empty type?
  if (Ty.CanTy.isNull())
    return llvm::DIType();

  // Look in the cache first.
  auto CachedType = DITypeCache.find(Ty);

  if (CachedType != DITypeCache.end()) {
    // Verify that the information still exists.
    if (llvm::Value *DITy = CachedType->second)
      return llvm::DIType(cast<llvm::MDNode>(DITy));
  }

  llvm::DIType DITy = createType(Ty, Scope, getFile(Scope));

  DITypeCache[Ty] = DITy;
  return DITy;
}
