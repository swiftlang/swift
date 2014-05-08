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
//  This file implements IR debug info generation for Swift.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "debug-info"
#include "IRGenDebugInfo.h"
#include "GenType.h"
#include "Linking.h"
#include "swift/AST/Expr.h"
#include "swift/AST/IRGenOptions.h"
#include "swift/AST/Mangle.h"
#include "swift/AST/Module.h"
#include "swift/AST/ModuleLoader.h"
#include "swift/AST/Pattern.h"
#include "swift/Basic/Dwarf.h"
#include "swift/Basic/Punycode.h"
#include "swift/Basic/SourceManager.h"
#include "swift/Basic/Version.h"
#include "swift/ClangImporter/ClangImporter.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILBasicBlock.h"
#include "swift/SIL/SILDebugScope.h"
#include "swift/SIL/SILModule.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/Decl.h"
#include "clang/Basic/Module.h"
#include "clang/Basic/SourceLocation.h"
#include "clang/Basic/SourceManager.h"
#include "clang/Basic/TargetInfo.h"
#include "llvm/Config/config.h"
#include "llvm/IR/DebugInfo.h"
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

IRGenDebugInfo::IRGenDebugInfo(const IRGenOptions &Opts,
                               ClangImporter &CI,
                               IRGenModule &IGM,
                               llvm::Module &M)
  : Opts(Opts),
    CI(CI),
    SM(IGM.Context.SourceMgr),
    M(M),
    DBuilder(M),
    IGM(IGM),
    MetadataTypeDecl(nullptr),
    InternalType(nullptr),
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

  StringRef Producer = BumpAllocatedString(version::getSwiftFullVersion());

  bool IsOptimized = Opts.OptLevel > 0;
  StringRef Flags = Opts.DWARFDebugFlags;

  // FIXME.
  unsigned RuntimeVersion = 1;

  // No split DWARF on Darwin.
  StringRef SplitName = StringRef();
  TheCU = DBuilder.createCompileUnit(Lang, Filename, Dir, Producer,
                                     IsOptimized, Flags, RuntimeVersion,
                                     SplitName);

  if (IGM.SILMod->lookUpFunction(SWIFT_ENTRY_POINT_FUNCTION)) {
    IsLibrary = false;
    EntryPointFn =
      DBuilder.createForwardDecl(llvm::dwarf::DW_TAG_subroutine_type,
                                 SWIFT_ENTRY_POINT_FUNCTION,
                                 MainFile, MainFile, 0);
  }

  // Create a module for the current compile unit.
  MainModule = getOrCreateModule(TheCU, Opts.ModuleName, MainFile);
  std::string Mangled("_TF");
  llvm::raw_string_ostream MS(Mangled);
  if (Opts.ModuleName == IGM.Context.StdlibModuleName.str())
    MS << "S";
  else
    mangleIdent(MS, Opts.ModuleName);
  createImportedModule(Opts.ModuleName, MS.str(), MainModule, 1);
}

static const char* getFilenameFromDC(const DeclContext *DC) {
  if (auto LF = dyn_cast<LoadedFile>(DC)) {
    // FIXME: Today, the subclasses of LoadedFile happen to return StringRefs
    // that are backed by null-terminated strings, but that's certainly not
    // guaranteed in the future.
    StringRef Fn = LF->getFilename();
    assert(((Fn.size() == 0) ||
            (Fn.data()[Fn.size()] == '\0')) && "not a C string");
    return Fn.data();
  } if (auto SF = dyn_cast<SourceFile>(DC))
    return SF->getFilename().data();
  else if (auto M = dyn_cast<Module>(DC))
    return M->getModuleFilename().data();
  else
    return nullptr;
}


Location getDeserializedLoc(Pattern*) { return {}; }
Location getDeserializedLoc(Expr*)    { return {}; }
Location getDeserializedLoc(Stmt*)    { return {}; }
Location getDeserializedLoc(Decl* D)  {
  Location L = {};
  const DeclContext *DC = D->getDeclContext()->getModuleScopeContext();
  if (const char* Filename = getFilenameFromDC(DC)) {
    L.Filename = Filename;
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
/// should be used for the linetable and the "true" AST location (used
/// for, e.g., variable declarations).
static FullLocation
getLocation(SourceManager &SM, Optional<SILLocation> OptLoc) {
  if (!OptLoc)
    return {};

  SILLocation Loc = OptLoc.getValue();
  bool UseEnd = Loc.alwaysPointsToEnd();

  if (Loc.isNull() || Loc.isAutoGenerated()) return {};
  if (Expr *E = Loc.getAsASTNode<Expr>()) {
    // Code that has an autoclosure as location should not show up in
    // the line table (rdar://problem/14627460). Note also that the
    // closure function still has a valid DW_AT_decl_line.  Depending
    // on how we decide to resolve rdar://problem/14627460, we may
    // want to use the regular getLoc instead and rather use the
    // column info.
    auto ELoc = getLoc(SM, E, UseEnd);
    if (isa<AutoClosureExpr>(E))
      return {{}, ELoc};
    return {ELoc, ELoc};
  }
  if (Pattern* P = Loc.getAsASTNode<Pattern>()) {
    auto PLoc = getLoc(SM, P, UseEnd);
    return {PLoc, PLoc};
  }
  if (Stmt* S = Loc.getAsASTNode<Stmt>()) {
    auto SLoc = getLoc(SM, S, UseEnd);
    auto LinetableLoc = (Loc.getKind() == SILLocation::CleanupKind)
      ? getLoc(SM, S, true) : SLoc;
    return {LinetableLoc, SLoc};
  }

  if (Decl* D = Loc.getAsASTNode<Decl>()) {
    auto DLoc = getLoc(SM, D, UseEnd);
    auto LinetableLoc = DLoc;
    if (Loc.isInPrologue())
      LinetableLoc = {};

    // For return and cleanup code SILLocation points to the start,
    // because this is where we want diagnostics to appear. For the
    // line table, we want them to point to the end of the Decl.
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
  // In LLVM IR, the function prologue has neither location nor scope.
  if (Loc && Loc->isInPrologue()) return;

  llvm::DIDescriptor Scope = getOrCreateScope(DS);
  if (!Scope.Verify()) return;

  FullLocation L = getLocation(SM, Loc);
  if (DS && L.LocForLinetable.Filename &&
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
  if (CachedScope != ScopeCache.end())
    return llvm::DIDescriptor(cast<llvm::MDNode>(CachedScope->second));

  Location L = getLocation(SM, DS->Loc).Loc;
  llvm::DIFile File = getOrCreateFile(L.Filename);
  llvm::DIDescriptor Parent = getOrCreateScope(DS->Parent);
  if (!Parent)
    Parent = File;

  llvm::DILexicalBlock DScope =
    DBuilder.createLexicalBlock(Parent, File, L.Line, L.Col, 0);

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
StringRef IRGenDebugInfo::getName(const FuncDecl &FD) {
  // Getters and Setters are anonymous functions, so we forge a name
  // using its parent declaration.
  if (FD.isAccessor())
    if (ValueDecl *VD = FD.getAccessorStorageDecl()) {
      const char *Kind;
      switch (FD.getAccessorKind()) {
      case AccessorKind::NotAccessor: llvm_unreachable("this is an accessor");
      case AccessorKind::IsGetter: Kind = ".get"; break;
      case AccessorKind::IsSetter: Kind = ".set"; break;
      case AccessorKind::IsWillSet: Kind = ".willset"; break;
      case AccessorKind::IsDidSet: Kind = ".didset"; break;
      }
      
      SmallVector<char, 64> Buf;
      StringRef Name = (VD->getName().str() +Twine(Kind)).toStringRef(Buf);
      return BumpAllocatedString(Name);
    }

  if (FD.hasName())
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

static CanSILFunctionType getFunctionType(SILType SILTy) {
  if (!SILTy) return CanSILFunctionType();

  auto FnTy = SILTy.getAs<SILFunctionType>();
  if (!FnTy) {
    DEBUG(llvm::dbgs() << "Unexpected function type: "; SILTy.dump();
          llvm::dbgs() << "\n");
    return CanSILFunctionType();
  }

  return FnTy;
}

/// Build the context chain for a given DeclContext.
llvm::DIScope IRGenDebugInfo::getOrCreateContext(DeclContext *DC) {
  if (!DC)
    return TheCU;

  switch (DC->getContextKind()) {
  // TODO: Create a cache for functions.
  case DeclContextKind::AbstractClosureExpr:
  case DeclContextKind::AbstractFunctionDecl:

  // We don't model these in DWARF.
  case DeclContextKind::Initializer:
  case DeclContextKind::ExtensionDecl:
    return getOrCreateContext(DC->getParent());
  case DeclContextKind::TopLevelCodeDecl:
   return EntryPointFn;
  case DeclContextKind::Module: {
    auto File = getOrCreateFile(getFilenameFromDC(DC));
    return getOrCreateModule(TheCU, cast<Module>(DC)->getName().str(), File);
  }
  case DeclContextKind::FileUnit:
    // A module may contain multiple files.
    return getOrCreateContext(DC->getParent());
  case DeclContextKind::NominalTypeDecl: {
    auto CachedType = DITypeCache.find(cast<NominalTypeDecl>(DC)->
                                       getDeclaredType().getPointer());
    if (CachedType != DITypeCache.end()) {
      // Verify that the information still exists.
      if (llvm::Value *Val = CachedType->second) {
        auto DITy = llvm::DIType(cast<llvm::MDNode>(Val));
        if (DITy.Verify())
          return DITy;
      }
    }

    // Create a Forward-declared type.
    auto TyDecl = cast<NominalTypeDecl>(DC);
    auto Loc = getLoc(SM, TyDecl);
    auto File = getOrCreateFile(Loc.Filename);
    auto Line = Loc.Line;
    auto FwdDecl = DBuilder.createForwardDecl
      (llvm::dwarf::DW_TAG_structure_type,
       TyDecl->getName().str(), getOrCreateContext(DC->getParent()),
       File, Line, DW_LANG_Swift, 0, 0);

    return FwdDecl;
  }
  }
  return TheCU;
}

/// Create a single parameter type and push it.
void IRGenDebugInfo::
createParameterType(llvm::SmallVectorImpl<llvm::Value*>& Parameters,
                    SILType type,
                    DeclContext* DeclCtx) {
  // FIXME: This use of getSwiftType() is extremely suspect.
  DebugTypeInfo DbgTy(type.getSwiftType(), IGM.getTypeInfo(type), DeclCtx);
  Parameters.push_back(getOrCreateType(DbgTy));
}

/// Create the array of function parameters for FnTy. SIL Version.
llvm::DIArray IRGenDebugInfo::createParameterTypes(SILType SILTy,
                                                   DeclContext* DeclCtx) {
  if (!SILTy) return llvm::DIArray();
  return createParameterTypes(SILTy.castTo<SILFunctionType>(), DeclCtx);
}

/// Create the array of function parameters for a function type.
llvm::DIArray IRGenDebugInfo::createParameterTypes(CanSILFunctionType FnTy,
                                                   DeclContext* DeclCtx) {
  SmallVector<llvm::Value *, 16> Parameters;

  GenericContextScope Scope(IGM, FnTy->getGenericSignature());

  // The function return type is the first element in the list.
  createParameterType(Parameters,
                      FnTy->getSemanticInterfaceResultSILType(),
                      DeclCtx);

  // Actually, the input type is either a single type or a tuple
  // type. We currently represent a function with one n-tuple argument
  // as an n-ary function.
  for (auto Param : FnTy->getInterfaceParameters())
    createParameterType(Parameters, Param.getSILType(),
                        DeclCtx);

  return DBuilder.getOrCreateArray(Parameters);
}

/// FIXME: replace this condition with something more sane.
static bool isAllocatingConstructor(AbstractCC CC, DeclContext *DeclCtx) {
  return CC != AbstractCC::Method && DeclCtx && isa<ConstructorDecl>(DeclCtx);
}

void IRGenDebugInfo::emitFunction(SILModule &SILMod, SILDebugScope *DS,
                                  llvm::Function *Fn,
                                  AbstractCC CC, SILType SILTy,
                                  DeclContext *DeclCtx) {
  StringRef Name;
  Location L = {};
  unsigned ScopeLine = 0; // The source line used for the function prologue.
  if (DS) {
    Name = getName(DS->Loc);
    auto FL = getLocation(SM, DS->Loc);
    L = FL.Loc;
    ScopeLine = FL.LocForLinetable.Line;
  }
  assert(Fn);
  auto LinkageName = Fn->getName();
  auto File = getOrCreateFile(L.Filename);
  auto Scope = MainModule;
  auto Line = L.Line;

  // We know that top_level_code always comes from MainFile.
  if (LinkageName == SWIFT_ENTRY_POINT_FUNCTION) {
    if (!L.Filename) File = MainFile;
    Line = 1;
  }

  CanSILFunctionType FnTy = getFunctionType(SILTy);
  auto Params = createParameterTypes(SILTy, DeclCtx);
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
  // make an exception for top_level_code, which, albeit it does not
  // have a Swift name, does appear prominently in the source code.
  if ((Name.empty() && LinkageName != SWIFT_ENTRY_POINT_FUNCTION
       && !isExplicitClosure(DS)) ||
      // ObjC thunks should also not show up in the linetable, because we
      // never want to set a breakpoint there.
      (CC == AbstractCC::ObjCMethod) ||
      isAllocatingConstructor(CC, DeclCtx)) {
    Flags |= llvm::DIDescriptor::FlagArtificial;
    ScopeLine = 0;
  }

  if (FnTy && FnTy->getRepresentation() == FunctionType::Representation::Block)
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
  case AbstractCC::WitnessMethod:
    IsLocalToUnit = false;
  }

  llvm::DISubprogram SP =
    DBuilder.createFunction(Scope, Name, LinkageName, File, Line,
                            DIFnTy, IsLocalToUnit, IsDefinition,
                            ScopeLine,
                            Flags, IsOptimized, Fn, TemplateParameters, Decl);
  ScopeCache[DS] = llvm::WeakVH(SP);

  // RAUW the entry point function forward declaration with the real thing.
  if (LinkageName == SWIFT_ENTRY_POINT_FUNCTION)
    EntryPointFn->replaceAllUsesWith(SP);
}

/// TODO: This is no longer needed.
void IRGenDebugInfo::eraseFunction(llvm::Function *Fn) {
}

/// The DWARF output for import decls is similar to that of a using
/// directive in C++:
///   import Foundation
///   -->
///   0: DW_TAG_imported_module
///        DW_AT_import(*1)
///   1: DW_TAG_module // instead of DW_TAG_namespace.
///        DW_AT_name("Foundation")
///
void IRGenDebugInfo::emitImport(ImportDecl *D) {
  // Imports are visited after SILFunctions.
  llvm::DIScope Module = MainFile;
  swift::Module *M = IGM.Context.getModule(D->getFullAccessPath());
  assert(M && "Could not find module for import decl.");
  if (!M) return;
  auto File = getOrCreateFile(getFilenameFromDC(M->getFiles().front()));

  std::string Printed, Mangled("_T");
  {
    llvm::raw_string_ostream MS(Mangled), PS(Printed);
    bool first = true;
    for (auto elt : D->getModulePath()) {
      auto Component = elt.first.str();

      // We model each component of the access path as a module.
      if (first && Component == D->getASTContext().StdlibModuleName.str())
        MS << "S";
      else
        mangleIdent(MS, Component);
      Module = getOrCreateModule(Module, Component, File);

      if (first) first = false;
      else PS << '.';
      PS << Component;
    }
  }

  StringRef Name = BumpAllocatedString(Printed);
  unsigned Line = getLoc(SM, D).Line;
  createImportedModule(Name, Mangled, llvm::DIModule(Module), Line);
}

// Create an imported module and import declarations for all functions
// from that module.
void IRGenDebugInfo::createImportedModule(StringRef Name, StringRef Mangled,
                                          llvm::DIModule Module,
                                          unsigned Line) {
  llvm::DIScope File;
  {
    llvm::SmallString<512> Path(Module.getDirectory());
    llvm::sys::path::append(Path, Module.getFilename());
    File = getOrCreateFile(Path.c_str());
  } 
//  llvm::DIImportedEntity Import =
  DBuilder.createImportedModule(File, Module, Line);
}

/// Return a cached module for an access path or create a new one.
llvm::DIModule IRGenDebugInfo::getOrCreateModule(llvm::DIScope Parent,
                                                 std::string Name,
                                                 llvm::DIFile File) {
   // Look in the cache first.
  auto CachedM = DIModuleCache.find(Name);

  if (CachedM != DIModuleCache.end())
    // Verify that the information still exists.
    if (llvm::Value *Val = CachedM->second)
      return llvm::DIModule(cast<llvm::MDNode>(Val));

  auto M = DBuilder.createModule(Parent, Name, File, 1);
  DIModuleCache[Name] = llvm::WeakVH(M);
  return M;
}


void IRGenDebugInfo::emitFunction(SILFunction *SILFn, llvm::Function *Fn) {
  if (isAvailableExternally(SILFn->getLinkage()))
    return;
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

TypeAliasDecl *IRGenDebugInfo::getMetadataType() {
  if (!MetadataTypeDecl)
    MetadataTypeDecl = new (IGM.Context)
      TypeAliasDecl(SourceLoc(),
                    IGM.Context.getIdentifier("$swift.type"), SourceLoc(),
                    TypeLoc::withoutLoc(IGM.Context.TheRawPointerType),
                    IGM.Context.TheBuiltinModule);
  return MetadataTypeDecl;
}

void IRGenDebugInfo::
emitTypeMetadata(IRGenFunction &IGF, llvm::Value *Metadata, StringRef Name) {
  auto TName = BumpAllocatedString(("$swift.type."+Name).str());
  DebugTypeInfo DbgTy(getMetadataType(),
                    (Size)CI.getTargetInfo().getPointerWidth(0),
                    (Alignment)CI.getTargetInfo().getPointerAlign(0));
  emitVariableDeclaration(IGF.Builder, Metadata, DbgTy,
                          IGF.getDebugScope(),
                          TName, llvm::dwarf::DW_TAG_auto_variable, 0,
                          // swift.type is a already pointer type,
                          // having a shadow copy doesn't add another
                          // layer of indirection.
                          DirectValue, ArtificialValue);
}

void IRGenDebugInfo::emitStackVariableDeclaration(IRBuilder& B,
                                                  llvm::Value *Storage,
                                                  DebugTypeInfo DbgTy,
                                                  SILDebugScope *DS,
                                                  StringRef Name,
                                                  IndirectionKind Indirection) {
  emitVariableDeclaration(B, Storage, DbgTy, DS, Name,
                          llvm::dwarf::DW_TAG_auto_variable,
                          0, Indirection, RealValue);
}


void IRGenDebugInfo::emitArgVariableDeclaration(IRBuilder& Builder,
                                                llvm::Value *Storage,
                                                DebugTypeInfo DbgTy,
                                                SILDebugScope *DS,
                                                StringRef Name,
                                                unsigned ArgNo,
                                                IndirectionKind Indirection,
                                                ArtificialKind IsArtificial) {
  assert(ArgNo > 0);
  if (Name == IGM.Context.Id_self.str())
    emitVariableDeclaration(Builder, Storage, DbgTy, DS, Name,
                            llvm::dwarf::DW_TAG_arg_variable, ArgNo,
                            DirectValue, ArtificialValue);
  else
    emitVariableDeclaration(Builder, Storage, DbgTy, DS, Name,
                            llvm::dwarf::DW_TAG_arg_variable, ArgNo,
                            Indirection, IsArtificial);
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
                                             DebugTypeInfo DbgTy,
                                             SILDebugScope *DS,
                                             StringRef Name,
                                             unsigned Tag,
                                             unsigned ArgNo,
                                             IndirectionKind Indirection,
                                             ArtificialKind Artificial) {
  // There are variables without storage, such as "struct { func foo() {} }".
  // Emit them as constant 0.
  if (isa<llvm::UndefValue>(Storage))
    Storage = llvm::ConstantInt::get(llvm::Type::getInt64Ty(M.getContext()), 0);

  // FIXME: enable this assertion.
  //assert(DS);
  llvm::DIDescriptor Scope = getOrCreateScope(DS);
  Location Loc = getLoc(SM, DbgTy.getDecl());

  // If this is an argument, attach it to the current function scope.
  if (ArgNo > 0) {
    while (Scope.isLexicalBlock())
      Scope = llvm::DILexicalBlock(Scope).getContext();
  }

  assert(Scope.Verify() && Scope.isScope());
  if (!(Scope.Verify() && Scope.isScope()))
    return;

  llvm::DIFile Unit = getFile(Scope);
  // FIXME: this should be the scope of the type's declaration.
  llvm::DIType DITy = getOrCreateType(DbgTy);

  unsigned Derefs = Indirection ? 1 : 0;
  // If this is a function pointer we need an extra DW_OP_deref, so we
  // can distinuguish this from a function symbol.
  if (DbgTy.getType()->getCanonicalType()->is<AnyFunctionType>())
    ++Derefs;

  // If there is no debug info for this type then do not emit debug info
  // for this variable.
  assert(DITy);
  if (!DITy)
    return;

  unsigned Line = Loc.Line;
  unsigned Flags = 0;
  if (Artificial || DITy.isArtificial() || DITy == InternalType)
    Flags |= llvm::DIDescriptor::FlagArtificial;

  // Create the descriptor for the variable.
  llvm::DIVariable Descriptor;

  if (Derefs) {
    // Classes are always passed by reference.
    llvm::Type *Int64Ty = llvm::Type::getInt64Ty(M.getContext());
    SmallVector<llvm::Value *, 1> Addr;
    while (Derefs--)
      Addr.push_back(llvm::ConstantInt::get(Int64Ty, llvm::DIBuilder::OpDeref));
    assert(Flags == 0 && "Complex variables cannot have flags");
    Descriptor = DBuilder.createComplexVariable(Tag, Scope, Name,
                                                Unit, Line, DITy, Addr, ArgNo);
  } else {
    Descriptor = DBuilder.createLocalVariable(Tag, Scope, Name,
                                              Unit, Line, DITy,
                                              Opts.OptLevel > 0, Flags, ArgNo);
  }
  // Insert a debug intrinsic into the current block.
  auto BB = Builder.GetInsertBlock();
  auto Call = isa<llvm::AllocaInst>(Storage)
    ? DBuilder.insertDeclare(Storage, Descriptor, BB)
    : DBuilder.insertDbgValueIntrinsic(Storage, 0, Descriptor, BB);

  // Set the location/scope of the intrinsic.
  Call->setDebugLoc(llvm::DebugLoc::get(Line, Loc.Col, Scope));
}

void IRGenDebugInfo::emitGlobalVariableDeclaration(llvm::GlobalValue *Var,
                                                   StringRef Name,
                                                   StringRef LinkageName,
                                                   DebugTypeInfo DbgTy,
                                                   Optional<SILLocation> Loc) {
  llvm::DIType Ty = getOrCreateType(DbgTy);
  if (Ty.isArtificial() || Ty == InternalType)
    // FIXME: Really these should be marked as artificial, but LLVM
    // currently has no support for flags to be put on global
    // variables. In the mean time, elide these variables, they
    // would confuse both the user and LLDB.
    return;

  Location L = getLocation(SM, Loc).Loc;

  // Global variables in the top level compilation unit are emitted as
  // local static variables of SWIFT_ENTRY_POINT_FUNCTION so they
  // won't get confused with addressors.
  auto File = getOrCreateFile(L.Filename);
  llvm::DIScope Context = IsLibrary ? MainModule : EntryPointFn;
  DBuilder.createStaticVariable(Context, Name, LinkageName, File,
                                L.Line, Ty,
                                Var->hasInternalLinkage(), Var, nullptr);
}

/// Return the mangled name of any nominal type, including the global
/// _Tt prefix, which marks the Swift namespace for types in DWARF.
StringRef IRGenDebugInfo::getMangledName(DebugTypeInfo DbgTy) {
  llvm::SmallString<160> Buffer;
  {
    llvm::raw_svector_ostream S(Buffer);
    Mangle::Mangler M(S, /* DWARF */ true);
    M.mangleTypeForDebugger(DbgTy.getType(), DbgTy.getDeclContext());
  }
  assert(!Buffer.empty() && "mangled name came back empty");
  return BumpAllocatedString(Buffer);
}


/// Create a member of a struct, class, tuple, or enum.
llvm::DIDerivedType IRGenDebugInfo::createMemberType(DebugTypeInfo DbgTy,
                                                     StringRef Name,
                                                     unsigned &OffsetInBits,
                                                     llvm::DIDescriptor Scope,
                                                     llvm::DIFile File,
                                                     unsigned Flags) {
  unsigned SizeOfByte = CI.getTargetInfo().getCharWidth();
  auto Ty = getOrCreateType(DbgTy);
  auto DITy = DBuilder.createMemberType(Scope, Name, File, 0,
                                       SizeOfByte*DbgTy.size.getValue(),
                                       SizeOfByte*DbgTy.align.getValue(),
                                       OffsetInBits, Flags, Ty);
  OffsetInBits += Ty.getSizeInBits();
  OffsetInBits = llvm::RoundUpToAlignment(OffsetInBits,
                                          SizeOfByte*DbgTy.align.getValue());
  return DITy;
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
    DebugTypeInfo DbgTy(ElemTy, IGM.getTypeInfoForUnlowered(ElemTy), DeclContext);
    Elements.push_back(createMemberType(DbgTy, StringRef(), OffsetInBits,
                                        Scope, File, Flags));
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
      if (VD->hasStorage()) {
        auto Ty = VD->getType()->getCanonicalType();
        DebugTypeInfo DbgTy(VD, IGM.getTypeInfoForUnlowered(Ty));
        Elements.push_back(createMemberType(DbgTy, VD->getName().str(),
                                            OffsetInBits, Scope, File, Flags));
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
                                 unsigned RuntimeLang,
                                 StringRef UniqueID) {
  // Forward declare this first because types may be recursive.
  auto FwdDecl = DBuilder.createForwardDecl
    (llvm::dwarf::DW_TAG_structure_type,
     Name, Scope, File, Line, DW_LANG_Swift, SizeInBits, AlignInBits);

#ifndef NDEBUG
  if (UniqueID.empty())
    assert(!Name.empty() && "no mangled name and no human readable name given");
  else
    assert(UniqueID.size() > 2 && UniqueID[0] == '_' && UniqueID[1] == 'T' &&
           "UID is not a mangled name");
#endif

  DITypeCache[DbgTy.getType()] = llvm::WeakVH(FwdDecl);
  auto DITy = DBuilder.createStructType
    (Scope, Name, File, Line, SizeInBits, AlignInBits, Flags, DerivedFrom,
     getStructMembers(Decl, Scope, File, Flags), RuntimeLang,
     llvm::DIType(), UniqueID);

  FwdDecl->replaceAllUsesWith(DITy);
  return DITy;
}


/// Return an array with the DITypes for each of an enum's elements.
llvm::DIArray IRGenDebugInfo::
getEnumElements(DebugTypeInfo DbgTy, EnumDecl *D,
                llvm::DIDescriptor Scope, llvm::DIFile File,
                unsigned Flags) {
  SmallVector<llvm::Value *, 16> Elements;
  for (auto ElemDecl : D->getAllElements()) {
    // FIXME <rdar://problem/14845818> Support enums.
    // Swift Enums can be both like DWARF enums and DWARF unions.
    // We currently cannot represent the enum-like subset of enums.
    if (ElemDecl->hasType()) {
      // Use Decl as DeclContext.
      DebugTypeInfo ElemDbgTy(ElemDecl, DbgTy.size, DbgTy.align);
      unsigned Offset = 0;
      auto MTy = createMemberType(ElemDbgTy, ElemDecl->getName().str(),
                                  Offset, Scope, File, Flags);
      Elements.push_back(MTy);
    }
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
  unsigned SizeOfByte = CI.getTargetInfo().getCharWidth();
  unsigned SizeInBits = DbgTy.size.getValue() * SizeOfByte;
  unsigned AlignInBits = DbgTy.align.getValue() * SizeOfByte;
  // FIXME: Is DW_TAG_union_type the right thing here?
  auto FwdDecl = DBuilder.createForwardDecl
    (llvm::dwarf::DW_TAG_union_type,
     Name, Scope, File, Line, dwarf::DW_LANG_Swift, SizeInBits, AlignInBits);

  DITypeCache[DbgTy.getType()] = llvm::WeakVH(FwdDecl);

  auto DITy =
    DBuilder.createUnionType(Scope, Name, File, Line,
                             SizeInBits, AlignInBits, Flags,
                             getEnumElements(DbgTy, Decl, Scope, File, Flags),
                             dwarf::DW_LANG_Swift);
  FwdDecl->replaceAllUsesWith(DITy);
  return DITy;
}

/// Return a DIType for Ty reusing any DeclContext found in DbgTy.
llvm::DIType IRGenDebugInfo::getOrCreateDesugaredType(Type Ty,
                                                      DebugTypeInfo DbgTy)
{
  DebugTypeInfo BlandDbgTy(Ty, DbgTy.size, DbgTy.align, DbgTy.getDeclContext());
  return getOrCreateType(BlandDbgTy);
}

uint64_t IRGenDebugInfo::getSizeOfBasicType(DebugTypeInfo DbgTy) {
  uint64_t SizeOfByte = CI.getTargetInfo().getCharWidth();
  uint64_t BitWidth = DbgTy.size.getValue() * SizeOfByte;
  llvm::Type *StorageType = DbgTy.StorageType
    ? DbgTy.StorageType
    : IGM.DataLayout.getSmallestLegalIntType(IGM.getLLVMContext(), BitWidth);

  if (StorageType)
    return IGM.DataLayout.getTypeSizeInBits(StorageType);

  // This type is too large to fit in a register.
  assert(BitWidth > IGM.DataLayout.getLargestLegalIntTypeSize());
  return BitWidth;
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
/// The ultimate goal is to emit something like a
/// DW_TAG_APPLE_ast_ref_type (an external reference) instead of a
/// local reference to the type.
llvm::DIType IRGenDebugInfo::createType(DebugTypeInfo DbgTy,
                                        StringRef MangledName,
                                        llvm::DIDescriptor Scope,
                                        llvm::DIFile File) {
  // FIXME: For SizeInBits, clang uses the actual size of the type on
  // the target machine instead of the storage size that is alloca'd
  // in the LLVM IR. For all types that are boxed in a struct, we are
  // emitting the storage size of the struct, but it may be necessary
  // to emit the (target!) size of the underlying basic type.
  uint64_t SizeOfByte = CI.getTargetInfo().getCharWidth();
  uint64_t SizeInBits = DbgTy.size.getValue() * SizeOfByte;
  uint64_t AlignInBits = DbgTy.align.getValue() * SizeOfByte;
  unsigned Encoding = 0;
  unsigned Flags = 0;

  TypeBase* BaseTy = DbgTy.getType();

  if (!BaseTy) {
    DEBUG(llvm::dbgs() << "Type without TypeBase: "; DbgTy.getType()->dump();
          llvm::dbgs() << "\n");
    if (!InternalType) {
      StringRef Name = "<internal>";
      InternalType = DBuilder.
        createForwardDecl(llvm::dwarf::DW_TAG_structure_type, Name, Scope, File,
                          /*Line*/ 0, DW_LANG_Swift, SizeInBits, AlignInBits);
    }
    return llvm::DIType(InternalType);
  }

  // Here goes!
  switch (BaseTy->getKind()) {
  case TypeKind::BuiltinInteger: {
    Encoding = llvm::dwarf::DW_ATE_unsigned;
    SizeInBits = getSizeOfBasicType(DbgTy);
    break;
  }

  case TypeKind::BuiltinFloat: {
    auto FloatTy = BaseTy->castTo<BuiltinFloatType>();
    // Assuming that the bitwidth and FloatTy->getFPKind() are identical.
    SizeInBits = FloatTy->getBitWidth();
    Encoding = llvm::dwarf::DW_ATE_float;
    break;
  }

  case TypeKind::BuiltinUnknownObject: {
    // The builtin opaque Objective-C pointer type. Useful for pushing
    // an Objective-C type through swift.
    auto IdTy = DBuilder.
      createStructType(Scope, MangledName, File, 0, 0, 0, 0,
                       llvm::DIType(), llvm::DIArray(), DW_LANG_Swift,
                       llvm::DIType(), MangledName);
    return DBuilder.createPointerType(IdTy, SizeInBits, AlignInBits);
  }

  case TypeKind::BuiltinNativeObject: {
    auto PTy = DBuilder.createPointerType(llvm::DIType(),
                                          SizeInBits, AlignInBits, MangledName);
    return DBuilder.createObjectPointerType(PTy);
  }

  case TypeKind::BuiltinRawPointer:
    return DBuilder.createPointerType(llvm::DIType(),
                                      SizeInBits, AlignInBits, MangledName);

  case TypeKind::DynamicSelf: {
    // Self. We don't have a way to represent instancetype in DWARF,
    // so we emit the static type instead. This is similar to what we
    // do with instancetype in Objective-C.
    auto DynamicSelfTy = BaseTy->castTo<DynamicSelfType>();
    return getOrCreateDesugaredType(DynamicSelfTy->getSelfType(), DbgTy);
  }

  // Even builtin swift types usually come boxed in a struct.
  case TypeKind::Struct: {
    auto StructTy = BaseTy->castTo<StructType>();
    if (auto Decl = StructTy->getDecl()) {
      Location L = getLoc(SM, Decl);
      return createStructType(DbgTy, Decl, Decl->getName().str(), Scope,
                              getOrCreateFile(L.Filename), L.Line,
                              SizeInBits, AlignInBits, Flags,
                              llvm::DIType(),  // DerivedFrom
                              DW_LANG_Swift, MangledName);
    }
    DEBUG(llvm::dbgs() << "Struct without Decl: "; DbgTy.getType()->dump();
          llvm::dbgs() << "\n");
    break;
  }

  case TypeKind::Class: {
    // Classes are represented as DW_TAG_structure_type. This way the
    // DW_AT_APPLE_runtime_class( DW_LANG_Swift ) attribute can be
    // used to differentiate them from C++ and ObjC classes.
    auto ClassTy = BaseTy->castTo<ClassType>();
    if (auto *Decl = ClassTy->getDecl()) {
      Location L = getLoc(SM, Decl);
      // TODO: We may want to peek at Decl->isObjC() and set this
      // attribute accordingly.
      auto RuntimeLang = DW_LANG_Swift;
      if (auto *ClangDecl = Decl->getClangDecl()) {
        auto ClangSrcLoc = ClangDecl->getLocStart();
        clang::SourceManager &ClangSM =
          CI.getClangASTContext().getSourceManager();
        L.Line = ClangSM.getPresumedLineNumber(ClangSrcLoc);
        L.Filename = ClangSM.getBufferName(ClangSrcLoc);

        // Use "ObjectiveC" as default for implicit decls.  FIXME: Do
        // something more clever based on the decl's mangled name.
        StringRef ModuleName = "ObjectiveC";
        if (auto *OwningModule = ClangDecl->getOwningModule())
          ModuleName = OwningModule->getTopLevelModuleName();
        else
          assert(ClangDecl->isImplicit() &&
                 "explicit clang decl without an owning module");

        auto ModuleFile = getOrCreateFile(L.Filename);
        // This placeholder gets RAUW'd by finalize().
        Scope = getOrCreateModule(ModuleFile, ModuleName, ModuleFile);
      }
      return createStructType(DbgTy, Decl, Decl->getName().str(), Scope,
                              getOrCreateFile(L.Filename), L.Line,
                              SizeInBits, AlignInBits, Flags,
                              llvm::DIType(),  // DerivedFrom
                              RuntimeLang, MangledName);
    }
    DEBUG(llvm::dbgs() << "Class without Decl: "; DbgTy.getType()->dump();
          llvm::dbgs() << "\n");
    break;
  }

  case TypeKind::Protocol: {
    auto ProtocolTy = BaseTy->castTo<ProtocolType>();
    if (auto Decl = ProtocolTy->getDecl()) {
      // FIXME: (LLVM branch) Should be DW_TAG_interface_type
      Location L = getLoc(SM, Decl);
      return createStructType(DbgTy, Decl, Decl->getName().str(), Scope,
                              getOrCreateFile(L.Filename), L.Line,
                              SizeInBits, AlignInBits, Flags,
                              llvm::DIType(),  // DerivedFrom
                              DW_LANG_Swift, MangledName);
    }
    break;
  }

  case TypeKind::ProtocolComposition: {
    if (auto Decl = DbgTy.getDecl()) {
      Location L = getLoc(SM, Decl);
      auto File = getOrCreateFile(L.Filename);

      // FIXME: emit types
      //auto ProtocolCompositionTy = BaseTy->castTo<ProtocolCompositionType>();
      return DBuilder.
        createStructType(Scope, Decl->getName().str(), File, L.Line,
                         SizeInBits, AlignInBits, Flags,
                         llvm::DIType(), // DerivedFrom
                         llvm::DIArray(),
                         DW_LANG_Swift, llvm::DIType(), MangledName);
    }
    DEBUG(llvm::dbgs() << "ProtocolCompositionType without Decl: ";
          DbgTy.getType()->dump(); llvm::dbgs() << "\n");
    break;
  }

  case TypeKind::UnboundGeneric: {
    auto UnboundTy = BaseTy->castTo<UnboundGenericType>();
    if (auto Decl = UnboundTy->getDecl()) {
      Location L = getLoc(SM, Decl);
      return DBuilder.
        createStructType(Scope, Decl->getName().str(),
                         getOrCreateFile(L.Filename), L.Line,
                         SizeInBits, AlignInBits, Flags,
                         llvm::DIType(), // DerivedFrom
                         llvm::DIArray(),
                         DW_LANG_Swift, llvm::DIType(), MangledName);
    }
    DEBUG(llvm::dbgs() << "Unbound generic without Decl: ";
          DbgTy.getType()->dump(); llvm::dbgs() << "\n");
    break;
  }

  case TypeKind::BoundGenericStruct: {
    auto StructTy = BaseTy->castTo<BoundGenericStructType>();
    if (auto Decl = StructTy->getDecl()) {
      Location L = getLoc(SM, Decl);
      return createStructType(DbgTy, Decl, Decl->getName().str(), Scope,
                              getOrCreateFile(L.Filename), L.Line,
                              SizeInBits, AlignInBits, Flags,
                              llvm::DIType(), // DerivedFrom
                              DW_LANG_Swift, MangledName);
    }
    DEBUG(llvm::dbgs() << "Bound Generic struct without Decl: ";
          DbgTy.getType()->dump(); llvm::dbgs() << "\n");
    break;
  }

  case TypeKind::BoundGenericClass: {
    auto ClassTy = BaseTy->castTo<BoundGenericClassType>();
    if (auto Decl = ClassTy->getDecl()) {
      Location L = getLoc(SM, Decl);
      // TODO: We may want to peek at Decl->isObjC() and set this
      // attribute accordingly.
      auto RuntimeLang = DW_LANG_Swift;
      return createStructType(DbgTy, Decl, Decl->getName().str(), Scope,
                              getOrCreateFile(L.Filename), L.Line,
                              SizeInBits, AlignInBits, Flags,
                              llvm::DIType(),  // DerivedFrom
                              RuntimeLang, MangledName);
    }
    DEBUG(llvm::dbgs() << "Bound Generic class without Decl: ";
          DbgTy.getType()->dump(); llvm::dbgs() << "\n");
    break;
  }

  case TypeKind::Tuple: {
    auto TupleTy = BaseTy->castTo<TupleType>();
    Location L = getLoc(SM, DbgTy.getDecl());
    auto File = getOrCreateFile(L.Filename);
    // Tuples are also represented as structs.
    return DBuilder.
      createStructType(Scope, MangledName, File, L.Line,
                       SizeInBits, AlignInBits, Flags,
                       llvm::DIType(), // DerivedFrom
                       getTupleElements(TupleTy, Scope, File, Flags,
                                        DbgTy.getDeclContext()),
                       DW_LANG_Swift, llvm::DIType(), MangledName);
  }

  case TypeKind::InOut: {
    // This is an inout type.
    auto ObjectTy = BaseTy->castTo<InOutType>()->getObjectType();
    auto DT = getOrCreateDesugaredType(ObjectTy, DbgTy);
    return DBuilder.createReferenceType(llvm::dwarf::DW_TAG_reference_type, DT);
  }

  case TypeKind::Archetype: {
    auto Archetype = BaseTy->castTo<ArchetypeType>();
    Location L = getLoc(SM, Archetype->getAssocType());
    auto Superclass = Archetype->getSuperclass();
    auto DerivedFrom = Superclass.isNull() ? llvm::DIType() :
      getOrCreateDesugaredType(Superclass, DbgTy);
    auto DITy = DBuilder.createStructType(Scope, MangledName, File, L.Line,
                                          SizeInBits, AlignInBits, Flags,
                                          DerivedFrom, llvm::DIArray(),
                                          DW_LANG_Swift, llvm::DIType(),
                                          MangledName);
    // Emit the protocols the archetypes conform to.
    SmallVector<llvm::Value *, 4> Protocols;
    for (auto ProtocolDecl : Archetype->getConformsTo()) {
      auto PTy = IGM.SILMod->Types.getLoweredType(ProtocolDecl->getType())
        .getSwiftRValueType();
      auto PDbgTy = DebugTypeInfo(ProtocolDecl, IGM.getTypeInfoForLowered(PTy));
      auto PDITy = getOrCreateType(PDbgTy);
      Protocols.push_back(DBuilder.createInheritance(DITy, PDITy, 0, Flags));
    }
    DITy.setTypeArray(DBuilder.getOrCreateArray(Protocols));
    return DITy;
  }

  case TypeKind::ExistentialMetatype:
  case TypeKind::Metatype: {
    // Metatypes are (mostly) singleton type descriptors, often without storage.
    Flags |= llvm::DIDescriptor::FlagArtificial;
    Location L = getLoc(SM, DbgTy.getDecl());
    auto File = getOrCreateFile(L.Filename);
    return DBuilder.createStructType(Scope, MangledName, File, L.Line,
                                     SizeInBits, AlignInBits, Flags,
                                     llvm::DIType(), llvm::DIArray(),
                                     DW_LANG_Swift, llvm::DIType(),
                                     MangledName);
  }

  case TypeKind::SILFunction:
  case TypeKind::Function:
  case TypeKind::PolymorphicFunction:
  case TypeKind::GenericFunction:
  {
    CanSILFunctionType FunctionTy;
    if (auto SILFnTy = dyn_cast<SILFunctionType>(BaseTy))
      FunctionTy = CanSILFunctionType(SILFnTy);
    // FIXME: Handling of generic parameters in SIL type lowering is in flux.
    // DebugInfo doesn't appear to care about the generic context, so just
    // throw it away before lowering.
    else if (isa<GenericFunctionType>(BaseTy) ||
             isa<PolymorphicFunctionType>(BaseTy)) {
      auto fTy = cast<AnyFunctionType>(BaseTy);
      auto nongenericTy = FunctionType::get(fTy->getInput(),
                                            fTy->getResult(),
                                            fTy->getExtInfo());
                                            
      FunctionTy = IGM.SILMod->Types.getLoweredType(nongenericTy)
                              .castTo<SILFunctionType>();
    } else
      FunctionTy = IGM.SILMod->Types.getLoweredType(BaseTy)
                              .castTo<SILFunctionType>();
    auto Params=createParameterTypes(FunctionTy, DbgTy.getDeclContext());
    return DBuilder.createSubroutineType(MainFile, Params);
  }

  case TypeKind::Enum:
  {
    auto EnumTy = BaseTy->castTo<EnumType>();
    if (auto Decl = EnumTy->getDecl()) {
      Location L = getLoc(SM, Decl);
      return createEnumType(DbgTy, Decl, MangledName, Scope,
                            getOrCreateFile(L.Filename), L.Line, Flags);
    }
    DEBUG(llvm::dbgs() << "Enum type without Decl: ";
          DbgTy.getType()->dump(); llvm::dbgs() << "\n");
    break;
  }

  case TypeKind::BoundGenericEnum:
  {
    auto EnumTy = BaseTy->castTo<BoundGenericEnumType>();
    if (auto Decl = EnumTy->getDecl()) {
      Location L = getLoc(SM, Decl);
      return createEnumType(DbgTy, Decl, MangledName, Scope,
                             getOrCreateFile(L.Filename), L.Line, Flags);
    }
    DEBUG(llvm::dbgs() << "Bound generic enum type without Decl: ";
          DbgTy.getType()->dump(); llvm::dbgs() << "\n");
    break;
  }

  case TypeKind::BuiltinVector: {
    (void)MangledName; // FIXME emit the name somewhere.
    auto BuiltinVectorTy = BaseTy->castTo<BuiltinVectorType>();
    DebugTypeInfo ElemDbgTy(BuiltinVectorTy->getElementType(),
                        DbgTy.size, DbgTy.align, DbgTy.getDeclContext());
    auto Subscripts = llvm::DIArray();
    return DBuilder.createVectorType(BuiltinVectorTy->getNumElements(),
                                     AlignInBits,
                                     getOrCreateType(ElemDbgTy),
                                     Subscripts);
  }

  // Reference storage types.
  case TypeKind::UnownedStorage:
  case TypeKind::UnmanagedStorage:
  case TypeKind::WeakStorage: {
    auto ReferenceTy = cast<ReferenceStorageType>(BaseTy);
    auto CanTy = ReferenceTy->getReferentType();
    Location L = getLoc(SM, DbgTy.getDecl());
    auto File = getOrCreateFile(L.Filename);
    return DBuilder.createTypedef(getOrCreateDesugaredType(CanTy, DbgTy),
                                  MangledName, File, L.Line, File);
  }

  // Sugared types.

  case TypeKind::NameAlias: {

    auto NameAliasTy = cast<NameAliasType>(BaseTy);
    if (auto Decl = NameAliasTy->getDecl()) {
      Location L = getLoc(SM, Decl);
      auto AliasedTy = Decl->getUnderlyingType();
      auto File = getOrCreateFile(L.Filename);
      // For NameAlias types, the DeclContext for the aliasED type is
      // in the decl of the alias type.
      DebugTypeInfo AliasedDbgTy(AliasedTy, DbgTy.size, DbgTy.align,
                                 DbgTy.getDeclContext());
      return DBuilder.createTypedef(getOrCreateType(AliasedDbgTy),
                                    MangledName, File, L.Line, File);
    }
    DEBUG(llvm::dbgs() << "Name alias without Decl: ";
          DbgTy.getType()->dump(); llvm::dbgs() << "\n");
    break;
  }

  case TypeKind::Substituted: {
    auto SubstitutedTy = cast<SubstitutedType>(BaseTy);
    auto OrigTy = SubstitutedTy->getReplacementType();
    Location L = getLoc(SM, DbgTy.getDecl());
    auto File = getOrCreateFile(L.Filename);
    return DBuilder.createTypedef(getOrCreateDesugaredType(OrigTy, DbgTy),
                                  MangledName, File, L.Line, File);
  }

  case TypeKind::Paren: {
    auto ParenTy = cast<ParenType>(BaseTy);
    auto Ty = ParenTy->getUnderlyingType();
    Location L = getLoc(SM, DbgTy.getDecl());
    auto File = getOrCreateFile(L.Filename);
    return DBuilder.createTypedef(getOrCreateDesugaredType(Ty, DbgTy),
                                  MangledName, File, L.Line, File);
  }

  // SyntaxSugarType derivations.
  case TypeKind::ArraySlice:
  case TypeKind::Optional:
  case TypeKind::ImplicitlyUnwrappedOptional: {
    auto SyntaxSugarTy = cast<SyntaxSugarType>(BaseTy);
    auto CanTy = SyntaxSugarTy->getDesugaredType();
    return getOrCreateDesugaredType(CanTy, DbgTy);
  }

  case TypeKind::GenericTypeParam: {
    auto ParamTy = cast<GenericTypeParamType>(BaseTy);
    // FIXME: Provide a more meaningful debug type.
    return DBuilder.createUnspecifiedType(ParamTy->getName().str());
  }
  case TypeKind::DependentMember: {
    auto MemberTy = cast<DependentMemberType>(BaseTy);
    // FIXME: Provide a more meaningful debug type.
    return DBuilder.createUnspecifiedType(MemberTy->getName().str());
  }

  // The following types exist primarily for internal use by the type
  // checker.
  case TypeKind::AssociatedType:
  case TypeKind::Error:
  case TypeKind::LValue:
  case TypeKind::TypeVariable:
  case TypeKind::Module:
  case TypeKind::SILBlockStorage:

    DEBUG(llvm::errs() << "Unhandled type: "; DbgTy.getType()->dump();
         llvm::errs() << "\n");
   MangledName = "<unknown>";
  }
  return DBuilder.
    createBasicType(MangledName, SizeInBits, AlignInBits, Encoding);
}

/// Determine if there exists a name mangling for the given type.
static bool canMangle(TypeBase *Ty) {
  switch (Ty->getKind()) {
  case TypeKind::SILFunction:
  case TypeKind::Function:
  case TypeKind::PolymorphicFunction:
  case TypeKind::GenericFunction:
  case TypeKind::SILBlockStorage:
    return false;
  case TypeKind::InOut: {
    auto *ObjectTy = Ty->castTo<InOutType>()->getObjectType().getPointer();
    return canMangle(ObjectTy);
  }
  default:
    return true;
  }
}

/// Get the DIType corresponding to this DebugTypeInfo from the cache,
/// or build a fresh DIType otherwise.  There is the underlying
/// assumption that no two types that share the same canonical type
/// can have different storage size or alignment.
llvm::DIType IRGenDebugInfo::getOrCreateType(DebugTypeInfo DbgTy) {
  // Is this an empty type?
  if (DbgTy.isNull())
    // We can't use the empty type as an index into DenseMap.
    return createType(DbgTy, "", TheCU, MainFile);

  // Look in the cache first.
  auto CachedType = DITypeCache.find(DbgTy.getType());
  if (CachedType != DITypeCache.end()) {
    // Verify that the information still exists.
    if (llvm::Value *Val = CachedType->second) {
      auto DITy = llvm::DIType(cast<llvm::MDNode>(Val));
      if (DITy.Verify())
        return DITy;
    }
  }

  // Second line of defense: Look up the mangled name. TypeBase*'s are
  // not necessarily unique, but name mangling is too expensive to do
  // every time.
  StringRef MangledName;
  if (canMangle(DbgTy.getType())) {
    MangledName = getMangledName(DbgTy);
    auto UID = llvm::MDString::get(IGM.getLLVMContext(), MangledName);
    if (auto *CachedTy = DIRefMap.lookup(UID)) {
      auto DITy = llvm::DIType(CachedTy);
      if (DITy.Verify())
        return DITy;
    }
  }

  // Retrieve the context of the type, as opposed to the DeclContext
  // of the variable.
  //
  // FIXME: Builtin and qualified types in LLVM have no parent
  // scope. TODO: This can be fixed by extending DIBuilder.
  DeclContext *Context =
    DbgTy.getType()->getNominalOrBoundGenericNominal();
  if (Context)
    Context = Context->getParent();
  llvm::DIScope Scope = getOrCreateContext(Context);
  llvm::DIType DITy = createType(DbgTy, MangledName, Scope, getFile(Scope));
  DITy.Verify();

  // Incrementally build the DIRefMap.
  if (DITy.isCompositeType()) {
    auto CompTy = llvm::DICompositeType(DITy);
    DIRefMap.insert({ CompTy.getIdentifier(), CompTy });
  }
  // Store it in the cache.
  DITypeCache[DbgTy.getType()] = llvm::WeakVH(DITy);

  return DITy;
}

void IRGenDebugInfo::finalize() {
  assert(LocationStack.empty() && "Mismatch of pushLoc() and popLoc().");

  // Finalize the DIBuilder.
  DBuilder.finalize();
}
