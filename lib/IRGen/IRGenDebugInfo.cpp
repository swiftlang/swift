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
#include "GenType.h"
#include "Linking.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Mangle.h"
#include "swift/AST/Module.h"
#include "swift/AST/ModuleLoader.h"
#include "swift/AST/Pattern.h"
#include "swift/Basic/Dwarf.h"
#include "swift/Basic/Punycode.h"
#include "swift/Basic/SourceManager.h"
#include "swift/IRGen/Options.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILBasicBlock.h"
#include "swift/SIL/SILDebugScope.h"
#include "swift/SIL/SILModule.h"
#include "clang/Basic/TargetInfo.h"
#include "llvm/Config/config.h"
#include "llvm/DebugInfo.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/raw_ostream.h"

using namespace swift;
using namespace irgen;

/// Strdup a raw char array using the bump pointer.
StringRef IRGenDebugInfo::BumpAllocatedString(const char* Data, size_t Length) {
  char *Ptr = DebugInfoNames.Allocate<char>(Length);
  memcpy(Ptr, Data, Length);
  return StringRef(Ptr, Length);
}

/// Strdup S using the bump pointer.
StringRef IRGenDebugInfo::BumpAllocatedString(std::string S) {
  return BumpAllocatedString(S.c_str(), S.length());
}

/// Strdup StringRef S using the bump pointer.
StringRef IRGenDebugInfo::BumpAllocatedString(StringRef S) {
  return BumpAllocatedString(S.data(), S.size());
}

IRGenDebugInfo::IRGenDebugInfo(const Options &Opts,
                               const clang::TargetInfo &TargetInfo,
                               TypeConverter &Types,
                               ASTContext &Context,
                               llvm::Module &M)
  : Opts(Opts),
    TargetInfo(TargetInfo),
    Context(Context),
    SM(Context.SourceMgr),
    M(M),
    DBuilder(M),
    Types(Types),
    LastFn(nullptr),
    MetadataTypeDecl(nullptr),
    LastLoc({}),
    LastScope(nullptr)
{
  assert(Opts.DebugInfo);
  StringRef Dir, Filename;
  if (Opts.MainInputFilename.empty()) {
    Filename = "<unknown>";
    Dir = getCurrentDirname();
  } else {
    // Separate path and filename.
    Filename =
      BumpAllocatedString(llvm::sys::path::filename(Opts.MainInputFilename));
    llvm::SmallString<512> Path(Opts.MainInputFilename);
    llvm::sys::path::remove_filename(Path);
    llvm::sys::fs::make_absolute(Path);
    llvm::SmallString<512> NPath;
    llvm::sys::path::native(Twine(Path), NPath);
    Dir = BumpAllocatedString(NPath);
  }
  // The fallback file.
  MainFilename = Dir;
  llvm::sys::path::append(MainFilename, Filename);
  MainFile = getOrCreateFile(MainFilename.c_str());

  unsigned Lang = DW_LANG_Swift;

  std::string buf;
  llvm::raw_string_ostream OS(buf);
  OS << "Swift version ? (based on LLVM " << PACKAGE_VERSION << ")";
  StringRef Producer = BumpAllocatedString(OS.str());

  bool IsOptimized = Opts.OptLevel > 0;
  StringRef Flags = Opts.DWARFDebugFlags;

  // FIXME.
  unsigned RuntimeVersion = 1;

  // FIXME.
  StringRef SplitName = StringRef();
  TheCU = DBuilder.createCompileUnit(Lang, Filename, Dir, Producer,
                                     IsOptimized, Flags, RuntimeVersion,
                                     SplitName);
}

void IRGenDebugInfo::finalize() {
  assert(LocationStack.empty() && "Mismatch of pushLoc() and popLoc().");

  // The default for a function is to be in the file-level scope.
  for (auto FVH: Functions) {
    auto F = llvm::DISubprogram(cast<llvm::MDNode>(FVH.second));
    auto Scope = F.getContext().resolve(DIRefMap);
    if (Scope.isType() && llvm::DIType(Scope).isForwardDecl())
      Scope->replaceAllUsesWith(MainFile);
  }

  DBuilder.finalize();
}


Location getDeserializedLoc(Pattern*) { return {}; }
Location getDeserializedLoc(Expr*)    { return {}; }
Location getDeserializedLoc(Stmt*)    { return {}; }
Location getDeserializedLoc(Decl* D)  {
  Location L = {};
  if (auto LM = dyn_cast<LoadedModule>(D->getModuleContext())) {
    L.Filename = LM->getDebugModuleName();
    L.Line = 1;
  }
  return L;
}

Location getLoc(SourceManager &SM, SourceLoc Loc) {
  Location L = {};
  unsigned BufferID = SM.findBufferContainingLoc(Loc);
  L.Filename = SM->getMemoryBuffer(BufferID)->getBufferIdentifier();
  std::tie(L.Line, L.Col) = SM.getLineAndColumn(Loc, BufferID);
  return L;
}

/// Use the SM to figure out the actual line/column of a SourceLoc.
template<typename WithLoc>
Location getLoc(SourceManager &SM, WithLoc *S, bool End = false) {
  Location L = {};
  if (S == nullptr)
    return L;

  SourceLoc Loc = End ? S->getEndLoc() : S->getStartLoc();
  if (Loc.isInvalid())
    // This may be a deserialized or clang-imported decl. And modules
    // don't come with SourceLocs right now. Get at least the name of
    // the module.
    return getDeserializedLoc(S);


  return getLoc(SM, Loc);
}

/// getLocForLinetable - extract the start location from a SILLocation.
///
/// This returns a FullLocation, which contains the location that
/// should be used for the linetable and the "true" AST location.
static FullLocation getLocation(SourceManager &SM, Optional<SILLocation> OptLoc) {
  if (!OptLoc)
    return {};

  SILLocation Loc = OptLoc.getValue();

  if (Loc.isNull()) return {};
  if (Expr *E = Loc.getAsASTNode<Expr>()) {
    // Code that has an autoclosure as location should not show up in
    // the line table (rdar://problem/14627460). Note also that the
    // closure function still has a valid DW_AT_decl_line.  Depending
    // on how we decide to resolve rdar://problem/14627460, we may
    // want to use the regular getLoc instead and rather use the
    // column info.
    auto ELoc = getLoc(SM, E);
    if (isa<AutoClosureExpr>(E))
      return {{}, ELoc};
    return {ELoc, ELoc};
  }
  if (Pattern* P = Loc.getAsASTNode<Pattern>()) {
    auto PLoc = getLoc(SM, P);
    return {PLoc, PLoc};
  }
  if (Stmt* S = Loc.getAsASTNode<Stmt>()) {
    auto SLoc = getLoc(SM, S);
    return {SLoc, SLoc};
  }

  if (Decl* D = Loc.getAsASTNode<Decl>()) {
    auto DLoc = getLoc(SM, D);
    auto LinetableLoc = DLoc;
    if (Loc.isInPrologue())
      LinetableLoc = {};

    // FIXME: It may or may not be better to fix this in SILLocation.
    // If this is an implicit return, we want the end of the
    // AbstractFunctionDecl's body.
    else if (Loc.getKind() == SILLocation::ImplicitReturnKind ||
             Loc.getKind() == SILLocation::CleanupKind)
      LinetableLoc = getLoc(SM, D, true);

    return {LinetableLoc, DLoc};
  }

  llvm_unreachable("unexpected location type");
}

/// Determine whether this debug scope belongs to an explicit closure.
static bool isExplicitClosure(SILDebugScope *DS) {
  if (DS)
    if (Expr *E = DS->Loc.getAsASTNode<Expr>())
      if (isa<ClosureExpr>(E))
        return true;
  return false;
}

/// Determine whether this location is some kind of closure.
static bool isAbstractClosure(const SILLocation &Loc) {
    if (Expr *E = Loc.getAsASTNode<Expr>())
      if (isa<AbstractClosureExpr>(E))
        return true;
  return false;
}


void IRGenDebugInfo::setCurrentLoc(IRBuilder& Builder,
                                   SILDebugScope *DS,
                                   Optional<SILLocation> Loc) {
  FullLocation L = getLocation(SM, Loc);
  // In LLVM IR, the function prologue has neither location nor scope.
  if (Loc && Loc->isInPrologue()) return;

  llvm::DIDescriptor Scope = getOrCreateScope(DS);
  if (!Scope.Verify()) return;

  if (L.LocForLinetable.Filename &&
      L.LocForLinetable.Filename !=
      getLocation(SM, DS->Loc).LocForLinetable.Filename) {
    // We changed files in the middle of a scope. This happens, for
    // example, when constructors are inlined. Create a new scope to
    // reflect this.
    auto File = getOrCreateFile(L.LocForLinetable.Filename);
    Scope = DBuilder.createLexicalBlockFile(Scope, File);
  }

  // Both the code that is used to set up a closure object and the
  // (beginning of) the closure itself has the AbstractClosureExpr as
  // location. We are only interested in the latter case and want to
  // ignore the setup code.
  //
  // callWithClosure(
  //  { // <-- a breakpoint here should only stop inside of the closure.
  //    foo();
  //  })
  //
  // The actual closure has a closure expression as scope.
  if (Loc && isAbstractClosure(*Loc) && !isAbstractClosure(DS->Loc))
    return;

  if (L.LocForLinetable.Line == 0 && DS == LastScope) {
    // Reuse the last source location if we are still in the same
    // scope to get a more contiguous line table.
    L = LastLoc;
  }
  LastLoc = L;
  LastScope = DS;

  llvm::MDNode *InlinedAt = 0;
  auto DL = llvm::DebugLoc::get(L.LocForLinetable.Line, L.LocForLinetable.Col,
                                Scope, InlinedAt);
  // TODO: Write a strongly-worded letter to the person that came up
  // with a pair of functions spelled "get" and "Set".
  Builder.SetCurrentDebugLocation(DL);
}

/// getOrCreateScope - Translate a SILDebugScope into an llvm::DIDescriptor.
llvm::DIDescriptor IRGenDebugInfo::getOrCreateScope(SILDebugScope *DS) {
  if (DS == 0)
    return MainFile;

  // Try to find it in the cache first.
  auto CachedScope = ScopeCache.find(DS);
  if (CachedScope != ScopeCache.end()) {
    return llvm::DIDescriptor(cast<llvm::MDNode>(CachedScope->second));
  }

  Location L = getLocation(SM, DS->Loc).LocForLinetable;
  llvm::DIFile File = getOrCreateFile(L.Filename);
  llvm::DIDescriptor Parent = getOrCreateScope(DS->Parent);
  if (Parent == 0)
    Parent = File;

  llvm::DILexicalBlock DScope =
    DBuilder.createLexicalBlock(Parent, File, L.Line, L.Col);

  // Cache it.
  ScopeCache[DS] = llvm::WeakVH(DScope);

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
  return BumpAllocatedString(CWD.str());
}

/// getOrCreateFile - Translate filenames into DIFiles.
llvm::DIFile IRGenDebugInfo::getOrCreateFile(const char *Filename) {
  if (!Filename)
    return MainFile;

  // Look in the cache first.
  auto CachedFile = DIFileCache.find(Filename);

  if (CachedFile != DIFileCache.end()) {
    // Verify that the information still exists.
    if (llvm::Value *V = CachedFile->second)
      return llvm::DIFile(cast<llvm::MDNode>(V));
  }

  // Create a new one.
  StringRef File = BumpAllocatedString(llvm::sys::path::filename(Filename));
  llvm::SmallString<512> Path(Filename);
  llvm::sys::path::remove_filename(Path);
  llvm::error_code ec = llvm::sys::fs::make_absolute(Path);
  // Basically ignore any error.
  assert(ec == llvm::errc::success);
  (void)ec; // Silence the unused variable warning
  llvm::DIFile F =
    DBuilder.createFile(File, BumpAllocatedString(Path));

  // Cache it.
  DIFileCache[Filename] = llvm::WeakVH(F);
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
        SmallVector<char, 64> Buf;
        StringRef Name = (VD->getName().str() +
                          Twine(IsGetter ? ".get" : ".set")).toStringRef(Buf);
        return BumpAllocatedString(Name);
      }
    }

  if (!FD.getName().empty())
    return FD.getName().str();

  return StringRef();
}

/// Attempt to figure out the unmangled name of a function.
StringRef IRGenDebugInfo::getName(SILLocation L) {
  if (L.isNull())
    return StringRef();

  if (FuncDecl *FD = L.getAsASTNode<FuncDecl>())
    return getName(*FD);

  if (L.isASTNode<ConstructorDecl>())
    return "init";

  return StringRef();
}

static AnyFunctionType* getFunctionType(SILType SILTy) {
  TypeBase* Ty = SILTy.getSwiftType().getPointer();
  if (!Ty)
    return nullptr;

  auto FnTy = dyn_cast<AnyFunctionType>(Ty);
  if (!FnTy) {
    DEBUG(llvm::dbgs() << "Unexpected function type: "; SILTy.dump();
          llvm::dbgs() << "\n");
    return nullptr;
  }

  return FnTy;
}


/// Create a single parameter type and push it.
void IRGenDebugInfo::
createParameterType(llvm::SmallVectorImpl<llvm::Value*>& Parameters,
                    CanType CanTy,
                    llvm::DIDescriptor Scope,
                    DeclContext* DeclCtx) {
  if (DeclCtx) {
    VarDecl VD(/*static*/ false,
               SourceLoc(), Identifier::getEmptyKey(), CanTy, DeclCtx);
    DebugTypeInfo DTy(&VD, Types.getCompleteTypeInfo(CanTy));
    Parameters.push_back(getOrCreateType(DTy, Scope));
  } else {
    DebugTypeInfo DTy(CanTy, Types.getCompleteTypeInfo(CanTy));
    Parameters.push_back(getOrCreateType(DTy, Scope));
  }
}

/// Create the array of function parameters for FnTy. SIL Version.
llvm::DIArray IRGenDebugInfo::createParameterTypes(SILModule &SILMod,
                                                   SILType SILTy,
                                                   llvm::DIDescriptor Scope,
                                                   DeclContext* DeclCtx) {
  if (!SILTy.getSwiftType())
    return llvm::DIArray();

  SILFunctionType *TypeInfo = SILTy.getFunctionTypeInfo(SILMod);
  if (!TypeInfo) return llvm::DIArray();

  SmallVector<llvm::Value *, 16> Parameters;

  // The function return type is the first element in the list.
  createParameterType(Parameters, TypeInfo->getResult().getType(),
                      Scope, DeclCtx);

  // Actually, the input type is either a single type or a tuple
  // type. We currently represent a function with one n-tuple argument
  // as an n-ary function.
  for (auto Param : TypeInfo->getParameters())
    createParameterType(Parameters, Param.getSILType().getSwiftType(),
                        Scope, DeclCtx);

  return DBuilder.getOrCreateArray(Parameters);
}

/// Create the array of function parameters for FnTy. Swift Version.
llvm::DIArray IRGenDebugInfo::createParameterTypes(AnyFunctionType *FnTy,
                                                   llvm::DIDescriptor Scope,
                                                   DeclContext* DeclCtx) {
  SmallVector<llvm::Value *, 16> Parameters;

  // The function return type is the first element in the list.
  createParameterType(Parameters, FnTy->getResult()->getCanonicalType(),
                      Scope, DeclCtx);

  // The input type is either a single type or a tuple type. We
  // currently represent a function with one n-tuple argument as an
  // n-ary function.
  auto Input = FnTy->getInput()->getCanonicalType();
  if (auto Params = dyn_cast<TupleType>(Input)) {
    for (auto Param : Params->getElementTypes()) {
      CanType CanTy = Param->getCanonicalType();
      createParameterType(Parameters, CanTy, Scope, DeclCtx);
    }
  } else createParameterType(Parameters, Input, Scope, DeclCtx);

  return DBuilder.getOrCreateArray(Parameters);
}


void IRGenDebugInfo::emitFunction(SILModule &SILMod, SILDebugScope *DS,
                                  llvm::Function *Fn,
                                  AbstractCC CC, SILType SILTy,
                                  DeclContext *DeclCtx) {
  StringRef Name;
  Location L = {};
  Location PrologLoc = {}; // The source line used for the function prologue.
  if (DS) {
    auto FL = getLocation(SM, DS->Loc);
    L = FL.Loc;
    PrologLoc = FL.LocForLinetable;
    Name = getName(DS->Loc);
  }
  assert(Fn);
  auto LinkageName = Fn->getName();
  auto File = getOrCreateFile(L.Filename);
  // This placeholder scope gets RAUW'd when the namespaces are
  // created after we are finished with the entire translation unit.
  auto Scope = DBuilder.createForwardDecl(llvm::dwarf::DW_TAG_subroutine_type,
                                          LinkageName, MainFile, MainFile, 0);
  auto Line = L.Line;

  // We know that top_level_code always comes from MainFile.
  if (!L.Filename && LinkageName == "top_level_code") {
    File = MainFile;
    Line = 1;
  }

  AnyFunctionType* FnTy = getFunctionType(SILTy);
  auto Params = createParameterTypes(SILMod, SILTy, Scope, DeclCtx);
  llvm::DICompositeType DIFnTy = DBuilder.createSubroutineType(File, Params);
  llvm::DIArray TemplateParameters;
  llvm::DISubprogram Decl;

  // Various flags
  bool IsLocalToUnit = false;
  bool IsDefinition = true;
  bool IsOptimized = Opts.OptLevel > 0;
  unsigned Flags = 0;

  // Mark everything that is not visible from the source code (i.e.,
  // does not have a Swift name) as artificial, so the debugger can
  // ignore it. Explicit closures are exempt from this rule. We also
  // make an exception for top_level_code, which albeit it does not
  // have a Swift name, it does appear prominently in the source code.
  if (Name.empty() && LinkageName != "top_level_code" && !isExplicitClosure(DS))
    Flags |= llvm::DIDescriptor::FlagArtificial;

  if (FnTy && FnTy->isBlock())
    Flags |= llvm::DIDescriptor::FlagAppleBlock;

  switch (CC) {
  // FIXME: We need to invent new DWARF attributes for the CC, but we
  // can't do that without patching the LLVM backend.
  // Hijacking a completely different field for now.
  case AbstractCC::C:
  case AbstractCC::ObjCMethod:
    IsLocalToUnit = true;
    break;
  case AbstractCC::Method:
  case AbstractCC::Freestanding:
    IsLocalToUnit = false;
  }

  llvm::DISubprogram SP =
    DBuilder.createFunction(Scope, Name, LinkageName, File, Line,
                            DIFnTy, IsLocalToUnit, IsDefinition,
                            PrologLoc.Line,
                            Flags, IsOptimized, Fn, TemplateParameters, Decl);
  ScopeCache[DS] = llvm::WeakVH(SP);
  Functions[LinkageName] = llvm::WeakVH(SP);
}

static bool isNonAscii(StringRef str) {
  for (unsigned char c : str) {
    if (c >= 0x80)
      return true;
  }
  return false;
}

// Mangle a single non-operator identifier.
static void mangleIdent(llvm::raw_string_ostream &OS, StringRef Id) {
  // If the identifier contains non-ASCII character, we mangle with an initial
  // X and Punycode the identifier string.
  llvm::SmallString<32> PunycodeBuf;
  if (isNonAscii(Id)) {
    OS << 'X';
    Punycode::encodePunycode(Id, PunycodeBuf);
    Id = PunycodeBuf;
  }
  OS << Id.size() << Id;
  OS.flush();
  return;
}

void IRGenDebugInfo::emitImport(ImportDecl *D) {
  // Imports are visited after SILFunctions.
  llvm::DIScope Namespace = MainFile;
  std::string Printed, Mangled("_T");
  {
    llvm::raw_string_ostream MS(Mangled), PS(Printed);
    bool first = true;
    for (auto elt : D->getModulePath()) {
      auto Component = elt.first.str();

      // We model each component of the access path as a namespace.
      if (first && Component == D->getASTContext().StdlibModuleName.str())
        MS << "S";
      else
        mangleIdent(MS, Component);
      Namespace = getOrCreateNamespace(Namespace, Component, MainFile, 1);

      if (first) first = false;
      else PS << '.';
      PS << Component;
    }
  }

  // Create the imported module.
  StringRef Name = BumpAllocatedString(Printed);
  Location L = getLoc(SM, D, false);
  auto Import = DBuilder.createImportedModule(TheCU,
                                              llvm::DINameSpace(Namespace),
                                              L.Line, Name);

  // Add all functions that belong to this namespace to it.
  //
  // TODO: Since we have the mangled names anyway, this part is purely
  // cosmetic and we may consider removing it.
  for (auto F = Functions.lower_bound(Mangled); F != Functions.end(); ++F) {
    if (Mangled != F->first.substr(0, Mangled.length()))
      break;

    auto SP = llvm::DISubprogram(cast<llvm::MDNode>(F->second));
    // RAUW the context of the function with the namespace.
    auto Scope = SP.getContext().resolve(DIRefMap);
    if (Scope.isType() && llvm::DIType(Scope).isForwardDecl())
      Scope->replaceAllUsesWith(Namespace);

    DBuilder.createImportedDeclaration(llvm::DIScope(Import), SP, L.Line);
  }
}

/// Return a cached namespace for a mangled access path or create a new one.
llvm::DIScope IRGenDebugInfo::getOrCreateNamespace(llvm::DIScope Namespace,
                                                   std::string MangledName,
                                                   llvm::DIFile File,
                                                   unsigned Line) {
   // Look in the cache first.
  auto CachedNS = DINameSpaceCache.find(MangledName);

  if (CachedNS != DINameSpaceCache.end())
    // Verify that the information still exists.
    if (llvm::Value *Val = CachedNS->second)
      return llvm::DINameSpace(cast<llvm::MDNode>(Val));

  auto NS = DBuilder.createNameSpace(Namespace, MangledName, MainFile, Line);
  DINameSpaceCache[MangledName] = llvm::WeakVH(NS);
  return NS;
}


void IRGenDebugInfo::emitFunction(SILFunction *SILFn, llvm::Function *Fn) {
  emitFunction(SILFn->getModule(),
               SILFn->getDebugScope(), Fn,
               SILFn->getAbstractCC(),
               SILFn->getLoweredType(),
               SILFn->getDeclContext());
}

void IRGenDebugInfo::emitArtificialFunction(SILModule &SILMod,
                                            IRBuilder &Builder,
                                            llvm::Function *Fn,
                                            SILType SILTy) {
  RegularLocation ALoc = RegularLocation::getAutoGeneratedLocation();
  SILDebugScope *Scope = new (SILMod) SILDebugScope(ALoc);
  emitFunction(SILMod, Scope, Fn, AbstractCC::Freestanding, SILTy);
  setCurrentLoc(Builder, Scope);
}

/// Return the position of Arg in Fn's signature, counting from 1.
unsigned IRGenDebugInfo::getArgNo(SILFunction *Fn, SILArgument *Arg) {
  // Based on the assumption that arguments will appear in order in the
  // instruction stream, make one attempt to reuse the last iterator.
  // LastFn also acts as a sentinel for LastArg/End.
  if (Fn == LastFn) {
    ++LastArg; ++LastArgNo;
    if (LastArg != LastEnd && *LastArg == Arg)
      return LastArgNo;
  }
  // Otherwise perform a linear scan through all the arguments.
  LastArgNo = 1;
  if (!Fn->empty()) {
    const SILBasicBlock &FirstBB = Fn->front();
    LastArg = FirstBB.bbarg_begin();
    LastEnd = FirstBB.bbarg_end();
    LastFn = Fn;
    while (LastArg != LastEnd) {
      if (*LastArg == Arg)
        return LastArgNo;

      ++LastArg; ++LastArgNo;
    }
  }
  LastFn = nullptr;
  DEBUG(llvm::dbgs() << "Failed to find argument number for ";
        Arg->dump(); llvm::dbgs() << "\nIn:"; Fn->dump());
  return 0;
}

bool IRGenDebugInfo::emitVarDeclForSILArgOrNull(IRBuilder& Builder,
                                                llvm::Value *Storage,
                                                DebugTypeInfo Ty,
                                                StringRef Name,
                                                SILFunction *Fn,
                                                SILValue Value,
                                                IndirectionKind Indirection) {
  if (auto SILArg = dyn_cast<SILArgument>(Value)) {
    if (unsigned ArgNo = getArgNo(Fn, SILArg)) {
      emitArgVariableDeclaration(Builder, Storage, Ty, Name,
                                 ArgNo, Indirection);
      return true;
    }
  }
  return false;
}

TypeAliasDecl *IRGenDebugInfo::getMetadataType() {
  if (!MetadataTypeDecl)
    MetadataTypeDecl = new (Context)
      TypeAliasDecl(SourceLoc(),
                    Context.getIdentifier("$swift.type"), SourceLoc(),
                    TypeLoc::withoutLoc(Context.TheRawPointerType),
                    Context.TheBuiltinModule,
                    MutableArrayRef<TypeLoc>());
  return MetadataTypeDecl;
}

void IRGenDebugInfo::emitStackVariableDeclaration(IRBuilder& B,
                                                  llvm::Value *Storage,
                                                  DebugTypeInfo Ty,
                                                  StringRef Name,
                                                  SILInstruction *I,
                                                  IndirectionKind Indirection) {
  auto IntrinsicKind = Declare;
  // There are variables without storage, such as "struct { func foo() {} }".
  if (isa<llvm::UndefValue>(Storage)) {
    llvm::Type *Int64Ty = llvm::Type::getInt64Ty(M.getContext());
    Storage = llvm::ConstantInt::get(Int64Ty, 0);
    IntrinsicKind = Value;
  }
  // Make a best effort to find out if this variable is actually an
  // argument of the current function. This is done by looking at the
  // source of the first store to this alloca.  Unless we start
  // enriching SIL with debug metadata or debug intrinsics, that's the
  // best we can do.
  for (auto Use : I->getUses()) {
    if (auto Store = dyn_cast<StoreInst>(Use->getUser())) {
      auto Src = Store->getSrc();
      // Detect the pattern of an inout argument.
      if (auto Load = dyn_cast<LoadInst>(Src))
        Src = Load->getOperand();
      if (emitVarDeclForSILArgOrNull(B, Storage, Ty, Name,
                                     I->getFunction(), Src, Indirection))
        return;
    } else if (auto CopyAddr = dyn_cast<CopyAddrInst>(Use->getUser())) {
      if (emitVarDeclForSILArgOrNull(B, Storage, Ty, Name, I->getFunction(),
                                     CopyAddr->getSrc(), Indirection))
        return;
    }
  }

  emitVariableDeclaration(B, Storage, Ty, Name,
                          llvm::dwarf::DW_TAG_auto_variable, 0, Indirection,
                          RealValue, IntrinsicKind);
}


void IRGenDebugInfo::
emitTypeMetadata(IRGenFunction &IGF, llvm::Value *Metadata, StringRef Name) {
  auto TName = BumpAllocatedString(("$swift.type."+Name).str());
  DebugTypeInfo DTI(getMetadataType(),
                    (Size)TargetInfo.getPointerWidth(0),
                    (Alignment)TargetInfo.getPointerAlign(0),
                    IGF.getDebugScope());
  emitVariableDeclaration
    (IGF.Builder, Metadata, DTI,
     TName, llvm::dwarf::DW_TAG_auto_variable, 0,
     isa<llvm::AllocaInst>(Metadata) ? IndirectValue : DirectValue,
     ArtificialValue,
     isa<llvm::AllocaInst>(Metadata) ? Declare : Value);
}


void IRGenDebugInfo::emitArgVariableDeclaration(IRBuilder& Builder,
                                                llvm::Value *Storage,
                                                DebugTypeInfo Ty,
                                                StringRef Name,
                                                unsigned ArgNo,
                                                IndirectionKind Indirection,
                                                IntrinsicKind Intrinsic) {
  assert(ArgNo > 0);
  emitVariableDeclaration(Builder, Storage, Ty, Name,
                          llvm::dwarf::DW_TAG_arg_variable, ArgNo,
                          Indirection,
                          Name == "self" ? ArtificialValue : RealValue,
                          Intrinsic);
}

/// Return the DIFile that is the ancestor of Scope.
llvm::DIFile IRGenDebugInfo::getFile(llvm::DIDescriptor Scope) {
  while (!Scope.isFile()) {
    switch (Scope.getTag()) {
    case llvm::dwarf::DW_TAG_lexical_block:
      Scope = llvm::DILexicalBlock(Scope).getContext();
      break;
    case llvm::dwarf::DW_TAG_subprogram:
      Scope = llvm::DISubprogram(Scope).getContext().resolve(DIRefMap);
      break;
    default:
      return MainFile;
    }
    if (Scope.Verify())
      return MainFile;
  }
  llvm::DIFile File(Scope);
  assert(File.Verify());
  return File;
}

void IRGenDebugInfo::emitVariableDeclaration(IRBuilder& Builder,
                                             llvm::Value *Storage,
                                             DebugTypeInfo Ty,
                                             StringRef Name,
                                             unsigned Tag,
                                             unsigned ArgNo,
                                             IndirectionKind Indirection,
                                             ArtificialKind Artificial,
                                             IntrinsicKind DbgValue) {
  llvm::DIDescriptor Scope = getOrCreateScope(Ty.getDebugScope());
  Location Loc = getLoc(SM, Ty.getDecl());

  // If this is an argument, attach it to the current function scope.
  if (ArgNo > 0) {
    while (Scope.isLexicalBlock())
      Scope = llvm::DILexicalBlock(Scope).getContext();
  }

  assert(Scope.Verify() && Scope.isScope());
  if (!(Scope.Verify() && Scope.isScope()))
    return;

  llvm::DIFile Unit = getFile(Scope);
  llvm::DIType DTy = getOrCreateType(Ty, Scope);

  // If there is no debug info for this type then do not emit debug info
  // for this variable.
  assert(DTy);
  if (!DTy)
    return;

  unsigned Line = Loc.Line;
  unsigned Flags = 0;
  if (Artificial)
    Flags |= llvm::DIDescriptor::FlagArtificial;

  // Create the descriptor for the variable.
  llvm::DIVariable Descriptor;

  if (Indirection) {
    // Classes are always passed by reference.
    llvm::Type *Int64Ty = llvm::Type::getInt64Ty(M.getContext());
    SmallVector<llvm::Value *, 1> Addr;
    Addr.push_back(llvm::ConstantInt::get(Int64Ty, llvm::DIBuilder::OpDeref));
    //assert(Flags == 0 && "Complex variables cannot have flags");
    Descriptor = DBuilder.createComplexVariable(Tag, Scope, Name,
                                                Unit, Line, DTy, Addr, ArgNo);
  } else {
    Descriptor = DBuilder.createLocalVariable(Tag, Scope, Name,
                                              Unit, Line, DTy,
                                              Opts.OptLevel > 0, Flags, ArgNo);
  }

  // Insert a debug intrinsic into the current block.
  auto Call = DbgValue
    ? DBuilder.insertDbgValueIntrinsic(Storage, 0, Descriptor,
                                       Builder.GetInsertBlock())
    : DBuilder.insertDeclare(Storage, Descriptor, Builder.GetInsertBlock());
  Call->setDebugLoc(llvm::DebugLoc::get(Line, Loc.Col, Scope));
}

void IRGenDebugInfo::emitGlobalVariableDeclaration(llvm::GlobalValue *Var,
                                                   StringRef Name,
                                                   StringRef LinkageName,
                                                   DebugTypeInfo DebugType,
                                                   Optional<SILLocation> Loc) {
  Location L = getLocation(SM, Loc).Loc;
  llvm::DIFile Unit = getOrCreateFile(L.Filename);

  // FIXME: Can there be nested types?
  llvm::DIDescriptor DContext = Unit;
  DBuilder.createStaticVariable(DContext, Name, LinkageName, Unit,
                                L.Line, getOrCreateType(DebugType, Unit),
                                Var->hasInternalLinkage(), Var, nullptr);
}

/// Return the mangled name of any nominal type, including the global
/// _Tt prefix, which marks the Swift namespace for types in DWARF.
StringRef IRGenDebugInfo::getMangledName(DebugTypeInfo DTI) {
  llvm::SmallString<128> Buffer;
  if (auto Decl = DTI.getDecl()) {
    LinkEntity::forDebuggerTypeMangling(Decl).mangle(Buffer);
  } else {
    auto CanTy = DTI.getType()->getCanonicalType();
    LinkEntity::forDebuggerTypeMangling(CanTy).mangle(Buffer);
  }
  return BumpAllocatedString(Buffer);
}

/// Create a member of a struct, class, tuple, or enum.
llvm::DIDerivedType IRGenDebugInfo::createMemberType(DebugTypeInfo DTI,
                                                     unsigned &OffsetInBits,
                                                     llvm::DIDescriptor Scope,
                                                     llvm::DIFile File,
                                                     unsigned Flags) {
  unsigned SizeOfByte = TargetInfo.getCharWidth();
  auto Ty = getOrCreateType(DTI, Scope);
  auto DTy = DBuilder.createMemberType(Scope, StringRef(), File, 0,
                                       SizeOfByte*DTI.size.getValue(),
                                       SizeOfByte*DTI.align.getValue(),
                                       OffsetInBits, Flags, Ty);
  OffsetInBits += Ty.getSizeInBits();
  OffsetInBits = llvm::RoundUpToAlignment(OffsetInBits,
                                          SizeOfByte*DTI.align.getValue());
  return DTy;
}

/// Return an array with the DITypes for each of a tuple's elements.
llvm::DIArray IRGenDebugInfo::getTupleElements(TupleType *TupleTy,
                                               llvm::DIDescriptor Scope,
                                               llvm::DIFile File,
                                               unsigned Flags,
                                               DeclContext *DeclContext) {
  SmallVector<llvm::Value *, 16> Elements;
  unsigned OffsetInBits = 0;
  for (auto ElemTy : TupleTy->getElementTypes()) {
    VarDecl VD(/*static*/ false,
               SourceLoc(), Identifier::getEmptyKey(), ElemTy, DeclContext);
    DebugTypeInfo DTI(&VD, Types.getCompleteTypeInfo(ElemTy->getCanonicalType()));
    Elements.push_back(createMemberType(DTI, OffsetInBits, Scope, File, Flags));
  }
  return DBuilder.getOrCreateArray(Elements);
}

/// Return an array with the DITypes for each of a struct's elements.
llvm::DIArray IRGenDebugInfo::getStructMembers(NominalTypeDecl *D,
                                               llvm::DIDescriptor Scope,
                                               llvm::DIFile File,
                                               unsigned Flags) {
  SmallVector<llvm::Value *, 16> Elements;
  unsigned OffsetInBits = 0;
  for (auto Decl : D->getMembers())
    if (VarDecl *VD = dyn_cast<VarDecl>(Decl))
      if (!VD->isComputed()) {
        auto Ty = VD->getType()->getCanonicalType();
        DebugTypeInfo DTI(VD, Types.getCompleteTypeInfo(Ty));
        Elements.push_back(createMemberType(DTI, OffsetInBits,
                                            Scope, File, Flags));
      }
  return DBuilder.getOrCreateArray(Elements);
}


/// Create a temporary forward declaration for a struct and add it to
/// the type cache so we can safely build recursive types.
llvm::DICompositeType
IRGenDebugInfo::createStructType(DebugTypeInfo DbgTy,
                                 NominalTypeDecl *Decl,
                                 StringRef Name,
                                 llvm::DIDescriptor Scope,
                                 llvm::DIFile File, unsigned Line,
                                 unsigned SizeInBits, unsigned AlignInBits,
                                 unsigned Flags,
                                 llvm::DIType DerivedFrom,
                                 unsigned RuntimeLang) {
  auto FwdDecl = DBuilder.createForwardDecl
    (llvm::dwarf::DW_TAG_structure_type,
     Name, Scope, File, Line, DW_LANG_Swift, SizeInBits, AlignInBits);

  DITypeCache[DbgTy.getHash()] = llvm::WeakVH(FwdDecl);

  auto DTy = DBuilder.createStructType
    (Scope, Name, File, Line, SizeInBits, AlignInBits, Flags, DerivedFrom,
     getStructMembers(Decl, Scope, File, Flags), RuntimeLang);

  FwdDecl->replaceAllUsesWith(DTy);
  return DTy;
}


/// Return an array with the DITypes for each of an enum's elements.
llvm::DIArray IRGenDebugInfo::
getEnumElements(DebugTypeInfo DbgTy,
                 EnumDecl *D, llvm::DIDescriptor Scope, llvm::DIFile File,
                 unsigned Flags) {
  SmallVector<llvm::Value *, 16> Elements;
  for (auto Decl : D->getAllElements()) {
    auto CanTy = Decl->getType()->getCanonicalType();
    // Use Decl as DeclContext.
    DebugTypeInfo DTI(Decl, DbgTy.size, DbgTy.align);
    auto DTy = getOrCreateDesugaredType(CanTy, DTI, Scope);
    Elements.push_back(DTy);
  }
  return DBuilder.getOrCreateArray(Elements);
}

/// Create a temporary forward declaration for an enum and add it to
/// the type cache so we can safely build recursive types.
llvm::DICompositeType
IRGenDebugInfo::createEnumType(DebugTypeInfo DbgTy,
                                EnumDecl *Decl,
                                StringRef Name,
                                llvm::DIDescriptor Scope,
                                llvm::DIFile File, unsigned Line,
                                unsigned Flags) {
  unsigned SizeOfByte = TargetInfo.getCharWidth();
  unsigned SizeInBits = DbgTy.size.getValue() * SizeOfByte;
  unsigned AlignInBits = DbgTy.align.getValue() * SizeOfByte;
  // FIXME: Is DW_TAG_union_type the right thing here?
  auto FwdDecl = DBuilder.createForwardDecl
    (llvm::dwarf::DW_TAG_union_type,
     Name, Scope, File, Line, dwarf::DW_LANG_Swift, SizeInBits, AlignInBits);

  DITypeCache[DbgTy.getHash()] = llvm::WeakVH(FwdDecl);

  auto DTy = DBuilder.createUnionType(Scope, Name, File, Line,
                                      SizeInBits, AlignInBits, Flags,
                                      getEnumElements(DbgTy, Decl, Scope, File,
                                                       Flags),
                                      dwarf::DW_LANG_Swift);
  FwdDecl->replaceAllUsesWith(DTy);
  return DTy;
}

/// Return a DIType for Ty reusing any DeclContext found in DbgTy.
llvm::DIType IRGenDebugInfo::getOrCreateDesugaredType(Type Ty,
                                                      DebugTypeInfo DbgTy,
                                                      llvm::DIDescriptor Scope)
{
  if (auto Decl = DbgTy.getDecl()) {
    VarDecl VD(/*static*/ false,
               SourceLoc(), Identifier::getEmptyKey(), Ty, Decl->getDeclContext());
    return getOrCreateType(DebugTypeInfo(&VD, DbgTy.size, DbgTy.align), Scope);
  }
  return getOrCreateType(DebugTypeInfo(Ty, DbgTy.size, DbgTy.align), Scope);
}


/// Construct a DIType from a DebugTypeInfo object.
///
/// At this point we do not plan to emit full DWARF for all swift
/// types, the goal is to emit only the name and provenance of the
/// type, where possible. A can import the type definition directly
/// from the module/framework/source file the type is specified in.
/// For this reason we emit the fully qualified (=mangled) name for
/// each type whenever possible.
///
/// The final goal, once we forked LLVM, is to emit something like a
/// DW_TAG_APPLE_ast_ref_type (an external reference) instead of a
/// local reference to the type.
llvm::DIType IRGenDebugInfo::createType(DebugTypeInfo DbgTy,
                                        llvm::DIDescriptor Scope,
                                        llvm::DIFile File) {
  StringRef Name;
  // FIXME: For SizeInBits, clang uses the actual size of the type on
  // the target machine instead of the storage size that is alloca'd
  // in the LLVM IR. For all types that are boxed in a struct, we are
  // emitting the storage size of the struct, but it may be necessary
  // to emit the (target!) size of the underlying basic type.
  unsigned SizeOfByte = TargetInfo.getCharWidth();
  uint64_t SizeInBits = DbgTy.size.getValue() * SizeOfByte;
  uint64_t AlignInBits = DbgTy.align.getValue() * SizeOfByte;
  unsigned Encoding = 0;
  unsigned Flags = 0;

  TypeBase* BaseTy = DbgTy.getType();
  if (!BaseTy) {
    DEBUG(llvm::dbgs() << "Type without TypeBase: "; DbgTy.getType()->dump();
          llvm::dbgs() << "\n");
    Name = "<null>";
    return DBuilder.createForwardDecl(llvm::dwarf::DW_TAG_structure_type,
                                      Name, Scope, File, /*Line*/ 0,
                                      DW_LANG_Swift, SizeInBits, AlignInBits);
  }

  switch (BaseTy->getKind()) {
  case TypeKind::BuiltinInteger: {
    auto IntegerTy = BaseTy->castTo<BuiltinIntegerType>();
    // FIXME: Translate this into the actually allocated number of
    // bits using TargetInfo.
    if (IntegerTy->isFixedWidth()) {
      SizeInBits = IntegerTy->getFixedWidth();
      switch (SizeInBits) {
      case   8: Name = "Builtin.Int8";   break;
      case  16: Name = "Builtin.Int16";  break;
      case  32: Name = "Builtin.Int32";  break;
      case  64: Name = "Builtin.Int64";  break;
      case 128: Name = "Builtin.Int128"; break;
      default:  Name = "Builtin.Int";
      }
    } else if (IntegerTy->getWidth().isPointerWidth()) {
      Name = "Builtin.Word";
    } else {
      llvm_unreachable("impossible width value");
    }
    Encoding = llvm::dwarf::DW_ATE_unsigned;
    break;
  }

  case TypeKind::BuiltinFloat: {
    auto FloatTy = BaseTy->castTo<BuiltinFloatType>();
    // Assuming that the bitwidth and FloatTy->getFPKind() are identical.
    SizeInBits = FloatTy->getBitWidth();
    switch (SizeInBits) {
    case  16: Name = "Builtin.Float16";  break;
    case  32: Name = "Builtin.Float32";  break;
    case  64: Name = "Builtin.Float64";  break;
    case  80: Name = "Builtin.Float80";  break;
    case 128: Name = "Builtin.Float128"; break;
    default:  Name = "Builtin.Float";
    }
    Encoding = llvm::dwarf::DW_ATE_float;
    break;
  }

  case TypeKind::BuiltinObjCPointer: {
    // The builtin opaque Objective-C pointer type. Useful for pushing
    // an Objective-C type through swift.
    auto IdTy = DBuilder.
      createStructType(Scope, "objc_object", File, 0, 0, 0, 0,
                       llvm::DIType(), llvm::DIArray(), DW_LANG_ObjC);
    return DBuilder.createPointerType(IdTy, SizeInBits, AlignInBits);
  }

  case TypeKind::BuiltinObjectPointer: {
    Name = getMangledName(DbgTy);
    auto PTy = DBuilder.createPointerType(llvm::DIType(),
                                          SizeInBits, AlignInBits, Name);
    return DBuilder.createObjectPointerType(PTy);
  }

  case TypeKind::BuiltinRawPointer:
    Name = getMangledName(DbgTy);
    return DBuilder.createPointerType(llvm::DIType(),
                                      SizeInBits, AlignInBits, Name);

  // Even builtin swift types usually come boxed in a struct.
  case TypeKind::Struct: {
    Name = getMangledName(DbgTy);
    auto StructTy = BaseTy->castTo<StructType>();
    if (auto Decl = StructTy->getDecl()) {
      Location L = getLoc(SM, Decl);
      return createStructType(DbgTy, Decl, Name, Scope,
                              getOrCreateFile(L.Filename), L.Line,
                              SizeInBits, AlignInBits, Flags,
                              llvm::DIType(),  // DerivedFrom
                              DW_LANG_Swift);
    }
    DEBUG(llvm::dbgs() << "Struct without Decl: "; DbgTy.getType()->dump();
          llvm::dbgs() << "\n");
    break;
  }

  case TypeKind::Class: {
    // Classes are represented as DW_TAG_structure_type. This way the
    // DW_AT_APPLE_runtime_class( DW_LANG_Swift ) attribute can be
    // used to differentiate them from C++ and ObjC classes.
    Name = getMangledName(DbgTy);
    auto ClassTy = BaseTy->castTo<ClassType>();
    if (auto Decl = ClassTy->getDecl()) {
      Location L = getLoc(SM, Decl);
      auto Attrs = Decl->getAttrs();
      auto RuntimeLang = Attrs.isObjC() ? DW_LANG_ObjC : DW_LANG_Swift;
      return createStructType(DbgTy, Decl, Name, Scope,
                              getOrCreateFile(L.Filename), L.Line,
                              SizeInBits, AlignInBits, Flags,
                              llvm::DIType(),  // DerivedFrom
                              RuntimeLang);
    }
    DEBUG(llvm::dbgs() << "Class without Decl: "; DbgTy.getType()->dump();
          llvm::dbgs() << "\n");
    break;
  }

  case TypeKind::Protocol: {
    Name = getMangledName(DbgTy);
    auto ProtocolTy = BaseTy->castTo<ProtocolType>();
    if (auto Decl = ProtocolTy->getDecl()) {
      // FIXME: (LLVM branch) Should be DW_TAG_interface_type
      Location L = getLoc(SM, Decl);
      return createStructType(DbgTy, Decl, Name, Scope,
                              getOrCreateFile(L.Filename), L.Line,
                              SizeInBits, AlignInBits, Flags,
                              llvm::DIType(),  // DerivedFrom
                              DW_LANG_Swift);
    }
    break;
  }

  case TypeKind::ProtocolComposition: {
    Name = getMangledName(DbgTy);
    Location L = getLoc(SM, DbgTy.getDecl());
    auto File = getOrCreateFile(L.Filename);

    // FIXME: emit types
    //auto ProtocolCompositionTy = BaseTy->castTo<ProtocolCompositionType>();
    return DBuilder.
      createStructType(Scope, Name, File, L.Line,
                       SizeInBits, AlignInBits, Flags,
                       llvm::DIType(), // DerivedFrom
                       llvm::DIArray(),
                       DW_LANG_Swift);
    break;
  }

  case TypeKind::UnboundGeneric: {
    Name = getMangledName(DbgTy);
    auto UnboundTy = BaseTy->castTo<UnboundGenericType>();
    if (auto Decl = UnboundTy->getDecl()) {
      Location L = getLoc(SM, Decl);
      return DBuilder.
        createStructType(Scope, Name,
                         getOrCreateFile(L.Filename), L.Line,
                         SizeInBits, AlignInBits, Flags,
                         llvm::DIType(), // DerivedFrom
                         llvm::DIArray(),
                         DW_LANG_Swift);
    }
    DEBUG(llvm::dbgs() << "Unbound generic without Decl: ";
          DbgTy.getType()->dump(); llvm::dbgs() << "\n");
    break;
  }

  case TypeKind::BoundGenericStruct: {
    Name = getMangledName(DbgTy);
    auto StructTy = BaseTy->castTo<BoundGenericStructType>();
    if (auto Decl = StructTy->getDecl()) {
      Location L = getLoc(SM, Decl);
      return createStructType(DbgTy, Decl, Name, Scope,
                              getOrCreateFile(L.Filename), L.Line,
                              SizeInBits, AlignInBits, Flags,
                              llvm::DIType(), // DerivedFrom
                              DW_LANG_Swift);
    }
    DEBUG(llvm::dbgs() << "Bound Generic struct without Decl: ";
          DbgTy.getType()->dump(); llvm::dbgs() << "\n");
    break;
  }

  case TypeKind::BoundGenericClass: {
    Name = getMangledName(DbgTy);
    auto ClassTy = BaseTy->castTo<BoundGenericClassType>();
    if (auto Decl = ClassTy->getDecl()) {
      Location L = getLoc(SM, Decl);
      auto Attrs = Decl->getAttrs();
      auto RuntimeLang = Attrs.isObjC() ? DW_LANG_ObjC : DW_LANG_Swift;
      return createStructType(DbgTy, Decl, Name, Scope,
                              getOrCreateFile(L.Filename), L.Line,
                              SizeInBits, AlignInBits, Flags,
                              llvm::DIType(),  // DerivedFrom
                              RuntimeLang);
    }
    DEBUG(llvm::dbgs() << "Bound Generic class without Decl: ";
          DbgTy.getType()->dump(); llvm::dbgs() << "\n");
    break;
  }

  case TypeKind::Tuple: {
    Name = getMangledName(DbgTy);
    auto TupleTy = BaseTy->castTo<TupleType>();
    auto Decl = DbgTy.getDecl();
    Location L = getLoc(SM, Decl);
    auto File = getOrCreateFile(L.Filename);
    // Tuples are also represented as structs.
    return DBuilder.
      createStructType(Scope, Name,
                       File, L.Line,
                       SizeInBits, AlignInBits, Flags,
                       llvm::DIType(), // DerivedFrom
                       getTupleElements(TupleTy, Scope, File, Flags,
                                        Decl ? Decl->getDeclContext() : nullptr),
                       DW_LANG_Swift);
  }

  case TypeKind::LValue: {
    // This is a [inout] type.
    // FIXME: handle LValueTy->getQualifiers();
    auto LValueTy = BaseTy->castTo<LValueType>();
    auto DT = getOrCreateDesugaredType(LValueTy->getObjectType(), DbgTy, Scope);
    return DBuilder.createReferenceType(llvm::dwarf::DW_TAG_reference_type, DT);
  }

  case TypeKind::Archetype: {
    auto Archetype = BaseTy->castTo<ArchetypeType>();
    Name = getMangledName(DbgTy);
    Location L = getLoc(SM, Archetype->getAssocType());
    auto Superclass = Archetype->getSuperclass();
    auto DerivedFrom = Superclass.isNull() ? llvm::DIType() :
      getOrCreateDesugaredType(Superclass, DbgTy, Scope);
    auto DITy = DBuilder.createStructType(Scope, Name, File, L.Line,
                                          SizeInBits, AlignInBits, Flags,
                                          DerivedFrom, llvm::DIArray(),
                                          DW_LANG_Swift);
    // Emit the protocols the archetypes conform to.
    SmallVector<llvm::Value *, 4> Protocols;
    for (auto ProtocolDecl : Archetype->getConformsTo()) {
      auto PTy = ProtocolDecl->getType()->getCanonicalType();
      auto PDbgTy = DebugTypeInfo(ProtocolDecl, Types.getCompleteTypeInfo(PTy));
      auto PDITy = getOrCreateType(PDbgTy, Scope);
      Protocols.push_back(DBuilder.createInheritance(DITy, PDITy, 0, Flags));
    }
    DITy.setTypeArray(DBuilder.getOrCreateArray(Protocols));
    return DITy;
  }

  case TypeKind::MetaType: {
    // Metatypes are (mostly) singleton type descriptors, often without storage.
    auto Metatype = BaseTy->castTo<MetaTypeType>();
    auto Ty = Metatype->getInstanceType();
    // The type this metatype is describing.
    // FIXME: Reusing the size and alignment of the metatype for the type is wrong.
    auto DITy = getOrCreateDesugaredType(Ty, DbgTy, Scope);
    return DBuilder.createQualifiedType(DW_TAG_meta_type, DITy);
  }

  case TypeKind::Function:
  case TypeKind::PolymorphicFunction:
  {
    auto FunctionTy = BaseTy->castTo<AnyFunctionType>();
    DeclContext *DC = nullptr;
    if (auto Decl = DbgTy.getDecl())
      DC = Decl->getDeclContext();
    auto Params = createParameterTypes(FunctionTy, Scope, DC);
    auto FnTy = DBuilder.createSubroutineType(MainFile, Params);
    return DBuilder.createPointerType(FnTy, SizeInBits, AlignInBits);
  }

  case TypeKind::Enum:
  {
    Name = getMangledName(DbgTy);
    auto EnumTy = BaseTy->castTo<EnumType>();
    if (auto Decl = EnumTy->getDecl()) {
      Location L = getLoc(SM, Decl);
      return createEnumType(DbgTy, Decl, Name, Scope,
                             getOrCreateFile(L.Filename), L.Line, Flags);
    }
    DEBUG(llvm::dbgs() << "Enum type without Decl: ";
          DbgTy.getType()->dump(); llvm::dbgs() << "\n");
    break;
  }

  case TypeKind::BoundGenericEnum:
  {
    Name = getMangledName(DbgTy);
    auto EnumTy = BaseTy->castTo<BoundGenericEnumType>();
    if (auto Decl = EnumTy->getDecl()) {
      Location L = getLoc(SM, Decl);
      return createEnumType(DbgTy, Decl, Name, Scope,
                             getOrCreateFile(L.Filename), L.Line, Flags);
    }
    DEBUG(llvm::dbgs() << "Bound generic enum type without Decl: ";
          DbgTy.getType()->dump(); llvm::dbgs() << "\n");
    break;
  }

  case TypeKind::BuiltinVector: {
    Name = getMangledName(DbgTy);
    (void)Name; // FIXME emit the name somewhere.
    auto BuiltinVectorTy = BaseTy->castTo<BuiltinVectorType>();
    auto DTI = DebugTypeInfo(BuiltinVectorTy->getElementType(),
                             DbgTy.size, DbgTy.align);
    auto Subscripts = llvm::DIArray();
    return DBuilder.createVectorType(BuiltinVectorTy->getNumElements(),
                                     AlignInBits,
                                     getOrCreateType(DTI, File),
                                     Subscripts);
  }

  // Reference storage types.
  case TypeKind::UnownedStorage:
  case TypeKind::WeakStorage: {
    Name = getMangledName(DbgTy);
    auto ReferenceTy = cast<ReferenceStorageType>(BaseTy);
    auto CanTy = ReferenceTy->getReferentType();
    Location L = getLoc(SM, DbgTy.getDecl());
    auto File = getOrCreateFile(L.Filename);
    return DBuilder.createTypedef(getOrCreateDesugaredType(CanTy, DbgTy, Scope),
                                  Name, File, L.Line, File);
  }

  // Sugared types.

  case TypeKind::NameAlias: {
    // We cannot use BaseTy->castTo<>(), because it will use the desugared type!
    auto NameAliasTy = cast<NameAliasType>(BaseTy);
    if (auto Decl = NameAliasTy->getDecl()) {
      Name = Decl->getName().str();
      Location L = getLoc(SM, Decl);
      auto AliasedTy = Decl->getUnderlyingType();
      auto File = getOrCreateFile(L.Filename);
      // For NameAlias types, the DeclContext for the aliasED type is
      // in the decl of the alias type.
      VarDecl VD(/*static*/ false,
                 SourceLoc(), Identifier::getEmptyKey(), AliasedTy,
                 Decl->getDeclContext());
      DebugTypeInfo DTI(&VD, DbgTy.size, DbgTy.align);
      return DBuilder.createTypedef(getOrCreateType(DTI, Scope),
                                    Name, File, L.Line, File);
    }
    DEBUG(llvm::dbgs() << "Name alias without Decl: ";
          DbgTy.getType()->dump(); llvm::dbgs() << "\n");
    break;
  }

  case TypeKind::Substituted: {
    Name = getMangledName(DbgTy);
    auto SubstitutedTy = cast<SubstitutedType>(BaseTy);
    auto OrigTy = SubstitutedTy->getReplacementType();
    Location L = getLoc(SM, DbgTy.getDecl());
    auto File = getOrCreateFile(L.Filename);
    return DBuilder.createTypedef(getOrCreateDesugaredType(OrigTy, DbgTy, Scope),
                                  Name, File, L.Line, File);
  }

  case TypeKind::Paren:
  {
    Name = getMangledName(DbgTy);
    auto ParenTy = cast<ParenType>(BaseTy);
    auto Ty = ParenTy->getUnderlyingType();
    Location L = getLoc(SM, DbgTy.getDecl());
    auto File = getOrCreateFile(L.Filename);
    return DBuilder.createTypedef(getOrCreateDesugaredType(Ty, DbgTy, Scope),
                                  Name, File, L.Line, File);
  }

  // SyntaxSugarType derivations.
  case TypeKind::ArraySlice:
  case TypeKind::Optional:
  {
    auto SyntaxSugarTy = cast<SyntaxSugarType>(BaseTy);
    auto CanTy = SyntaxSugarTy->getDesugaredType();
    return getOrCreateDesugaredType(CanTy, DbgTy, Scope);
  }

  default:
    DEBUG(llvm::errs() << "Unhandled type: "; DbgTy.getType()->dump();
          llvm::errs() << "\n");
    Name = "<unknown>";
    llvm_unreachable("Debug info: Unhandled type");
  }
  return DBuilder.createBasicType(Name, SizeInBits, AlignInBits, Encoding);
}

/// Get the DIType corresponding to this DebugTypeInfo from the cache,
/// or build a fresh DIType otherwise.  There is the underlying
/// assumption that no two types that share the same canonical type
/// can have different storage size or alignment.
llvm::DIType IRGenDebugInfo::getOrCreateType(DebugTypeInfo DbgTy,
                                             llvm::DIDescriptor Scope) {
  // Is this an empty type?
  if (DbgTy.isNull())
    // We use the empty type as an index into DenseMap.
    return createType(DbgTy, Scope, getFile(Scope));

  // Look in the cache first.
  auto CachedType = DITypeCache.find(DbgTy.getHash());

  if (CachedType != DITypeCache.end()) {
    // Verify that the information still exists.
    if (llvm::Value *Val = CachedType->second) {
      auto DITy = llvm::DIType(cast<llvm::MDNode>(Val));
      if (DITy.Verify())
        return DITy;
    }
  }

  llvm::DIType DITy = createType(DbgTy, Scope, getFile(Scope));
  DITy.Verify();
  auto CompTy = llvm::getDICompositeType(DITy);
  DIRefMap.insert({ CompTy.getIdentifier(), CompTy });

  DITypeCache[DbgTy.getHash()] = llvm::WeakVH(DITy);
  return DITy;
}
