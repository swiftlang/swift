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
StringRef IRGenDebugInfo::BumpAllocatedString(const char *Data, size_t Length) {
  char *Ptr = DebugInfoNames.Allocate<char>(Length+1);
  memcpy(Ptr, Data, Length);
  *(Ptr+Length) = 0;
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
  std::string PunycodeBuf;
  if (isNonAscii(Id)) {
    OS << 'X';
    Punycode::encodePunycodeUTF8(Id, PunycodeBuf);
    Id = PunycodeBuf;
  }
  OS << Id.size() << Id;
  OS.flush();
  return;
}

/// Resolve a DITypeRef using our TrackingMDNodeRef-based map.
static llvm::DIType resolve(const llvm::DITypeRef TyRef,
                            const TrackingDIRefMap &Map) {
  llvm::Metadata *Val = TyRef;
  if (!Val)
    return llvm::DIType();

  if (const llvm::MDNode *MD = dyn_cast<llvm::MDNode>(Val)) {
    llvm::DIType DITy(MD);
    assert(DITy.Verify() && "MDNode in DIRef should be a DIType.");
    return DITy;
  }

  // Find the corresponding MDNode.
  auto I = Map.find(cast<llvm::MDString>(Val));
  assert(I != Map.end() && "Identifier not in the type map?");
  llvm::DIType DITy(cast<llvm::MDNode>(I->second));
  assert(DITy.Verify() && "MDNode in DITypeIdentifierMap should be a DIType.");
  return DITy;

}

/// Return the size reported by a type.
static unsigned getSizeInBits(llvm::DIType Ty,
                              const TrackingDIRefMap &Map) {
  // Follow derived types until we reach a type that
  // reports back a size.
  while (Ty.isDerivedType() && !Ty.getSizeInBits()) {
    llvm::DIDerivedType DT(&*Ty);
    Ty = resolve(DT.getTypeDerivedFrom(), Map);
  }
  return Ty.getSizeInBits();
}

/// Return the size reported by the variable's type.
static unsigned getSizeInBits(const llvm::DIVariable Var,
                              const TrackingDIRefMap &Map) {
  llvm::DIType Ty = resolve(Var.getType(), Map);
  return getSizeInBits(Ty, Map);
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
    EntryPointFn(nullptr),
    MetadataTypeDecl(nullptr),
    InternalType(nullptr),
    LastDebugLoc({}),
    LastScope(nullptr)
{
  assert(Opts.DebugInfoKind > IRGenDebugInfoKind::None
         && "no debug info should be generated");
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

  unsigned Lang = llvm::dwarf::DW_LANG_Swift;

  StringRef Producer = BumpAllocatedString(version::getSwiftFullVersion());

  bool IsOptimized = Opts.Optimize;
  StringRef Flags = Opts.DWARFDebugFlags;
  unsigned Major, Minor;
  std::tie(Major, Minor) = version::getSwiftNumericVersion();
  unsigned RuntimeVersion = Major*100 + Minor;

  // No split DWARF on Darwin.
  StringRef SplitName = StringRef();
  TheCU = DBuilder.
    createCompileUnit(Lang, Filename, Dir, Producer, IsOptimized,
                      Flags, RuntimeVersion, SplitName,
                      Opts.DebugInfoKind == IRGenDebugInfoKind::LineTables
                      ? llvm::DIBuilder::LineTablesOnly
                      : llvm::DIBuilder::FullDebug);

  if (IGM.SILMod->lookUpFunction(SWIFT_ENTRY_POINT_FUNCTION)) {
    IsLibrary = false;
    EntryPointFn = DBuilder.createReplaceableForwardDecl(
        llvm::dwarf::DW_TAG_subroutine_type, SWIFT_ENTRY_POINT_FUNCTION,
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

static const char *getFilenameFromDC(const DeclContext *DC) {
  if (auto LF = dyn_cast<LoadedFile>(DC)) {
    // FIXME: Today, the subclasses of LoadedFile happen to return StringRefs
    // that are backed by null-terminated strings, but that's certainly not
    // guaranteed in the future.
    StringRef Fn = LF->getFilename();
    assert(((Fn.size() == 0) ||
            (Fn.data()[Fn.size()] == '\0')) && "not a C string");
    return Fn.data();
  }
  if (auto SF = dyn_cast<SourceFile>(DC))
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
  if (const char *Filename = getFilenameFromDC(DC)) {
    L.Filename = Filename;
    L.Line = 0;
  }
  return L;
}

Location getLoc(SourceManager &SM, SourceLoc Loc) {
  Location L = {};
  if (Loc.isValid()) {
    L.Filename = SM.getBufferIdentifierForLoc(Loc);
    std::tie(L.Line, L.Col) = SM.getLineAndColumn(Loc);
  }
  return L;
}

/// Use the SM to figure out the actual line/column of a SourceLoc.
template <typename WithLoc>
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

/// \brief Return the start of the location's source range.
static Location getStartLocation(SourceManager &SM,
                                 Optional<SILLocation> OptLoc) {
  if (!OptLoc) return {};
  return getLoc(SM, OptLoc->getStartSourceLoc());
}

/// \brief Return the debug location from a SILLocation.
static Location getDebugLocation(SourceManager &SM,
                                 Optional<SILLocation> OptLoc) {
  if (!OptLoc) return {};
  return getLoc(SM, OptLoc->getDebugSourceLoc());
}


/// \brief Extract the start location from a SILLocation.
///
/// This returns a FullLocation, which contains the location that
/// should be used for the linetable and the "true" AST location (used
/// for, e.g., variable declarations).
static FullLocation getLocation(SourceManager &SM,
                                Optional<SILLocation> OptLoc) {
  if (!OptLoc) return {};

  SILLocation Loc = OptLoc.getValue();
  return { getLoc(SM, Loc.getDebugSourceLoc()),
           getLoc(SM, Loc.getSourceLoc())};
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

/// Construct an inlined-at location from a SILScope.
llvm::MDNode* IRGenDebugInfo::createInlinedAt(SILDebugScope *InlinedScope) {
  assert(InlinedScope);
  assert(InlinedScope->InlinedCallSite && "not an inlined scope");
  SILDebugScope *CallSite = InlinedScope->InlinedCallSite;

#ifndef NDEBUG
  llvm::DILexicalBlock LB(getOrCreateScope(InlinedScope));
  while (!LB.isSubprogram()) {
    LB = llvm::DILexicalBlock(LB.getContext());
    assert(LB && "Lexical block parent chain must contain a subprogram");
  }
#endif

  auto ParentScope = getOrCreateScope(CallSite->Parent);
  llvm::MDNode *InlinedAt = nullptr;

  // If this is itself an inlined location, recursively create the
  // inlined-at location for it.
  if (CallSite->InlinedCallSite)
    InlinedAt = createInlinedAt(CallSite);

  auto InlineLoc = getLoc(SM, CallSite->Loc.getDebugSourceLoc());
  auto DL = llvm::DebugLoc::get(InlineLoc.Line, InlineLoc.Col,
                                ParentScope, InlinedAt);
  return DL.getAsMDNode(IGM.getLLVMContext());
}

#ifndef NDEBUG
/// Perform a couple of sanity checks on scopes.
static bool parentScopesAreSane(SILDebugScope *DS) {
  SILDebugScope *Parent = DS->Parent;
  while (Parent) {
    if (!DS->InlinedCallSite) {
      assert(!Parent->InlinedCallSite &&
             "non-inlined scope has an inlined parent");
      assert(DS->SILFn == Parent->SILFn
             && "non-inlined parent scope from different function?");
    }
    Parent = Parent->Parent;
  }
  return true;
}

bool IRGenDebugInfo::lineNumberIsSane(IRBuilder &Builder, unsigned Line) {
  if (IGM.Opts.Optimize)
    return true;

  // Assert monotonically increasing line numbers within the same basic block;
  llvm::BasicBlock *CurBasicBlock = Builder.GetInsertBlock();
  if (CurBasicBlock == LastBasicBlock) {
    return Line >= LastDebugLoc.Line;
  }
  LastBasicBlock = CurBasicBlock;
  return true;
}
#endif

void IRGenDebugInfo::setCurrentLoc(IRBuilder &Builder, SILDebugScope *DS,
                                   Optional<SILLocation> Loc) {
  // In LLVM IR, the function prologue has neither location nor scope.
  if (Loc && Loc->isInPrologue())
    return;

  assert(DS && "empty scope");
  llvm::DIDescriptor Scope = getOrCreateScope(DS);
  if (!Scope)
    return;
  assert(Scope.Verify() && "could not verify scope");

  Location L = getDebugLocation(SM, Loc);
  auto File = getOrCreateFile(L.Filename);
  if (Scope.isScope() &&
      File.getFilename() != llvm::DIScope(Scope).getFilename()) {
    // We changed files in the middle of a scope. This happens, for
    // example, when constructors are inlined. Create a new scope to
    // reflect this.
    auto File = getOrCreateFile(L.Filename);
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
  if (Loc && isAbstractClosure(*Loc) && DS && !isAbstractClosure(DS->Loc)
      && Loc->getKind() != SILLocation::ImplicitReturnKind)
    return;

  if (L.Line == 0 && DS == LastScope) {
    // Reuse the last source location if we are still in the same
    // scope to get a more contiguous line table.
    L = LastDebugLoc;
  }

  //FIXME: Enable this assertion.
  //assert(lineNumberIsSane(Builder, L.Line) &&
  //       "-Onone, but line numbers are not monotonically increasing within bb");
  LastDebugLoc = L;
  LastScope = DS;

  llvm::MDNode *InlinedAt = nullptr;
  if (DS->InlinedCallSite) {
    assert(Scope && "Inlined location without a lexical scope");
    InlinedAt = createInlinedAt(DS);
  }

  assert(((!InlinedAt) || (InlinedAt && Scope)) && "inlined w/o scope");
  assert(parentScopesAreSane(DS) && "parent scope sanity check failed");
  auto DL = llvm::DebugLoc::get(L.Line, L.Col, Scope, InlinedAt);
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

  // If this is a (inlined) function scope, the function may
  // not have been created yet.
  if (!DS->Parent ||
      DS->Loc.getKind() == SILLocation::SILFileKind ||
      DS->Loc.isASTNode<AbstractFunctionDecl>() ||
      DS->Loc.isASTNode<AbstractClosureExpr>() ||
      DS->Loc.isASTNode<EnumElementDecl>()) {

    auto *FnScope = DS->SILFn->getDebugScope();
    // FIXME: This is a bug in the SIL deserialization.
    if (!FnScope)
      DS->SILFn->setDebugScope(DS);

    auto CachedScope = ScopeCache.find(FnScope);
    if (CachedScope != ScopeCache.end())
      return llvm::DIDescriptor(cast<llvm::MDNode>(CachedScope->second));

    // Force the debug info for the function to be emitted, even if it
    // is external or has been inlined.
    llvm::Function *Fn = nullptr;
    if (!DS->SILFn->getName().empty() && !DS->SILFn->isZombie())
      Fn = IGM.getAddrOfSILFunction(DS->SILFn, NotForDefinition);
    llvm::DIDescriptor SP = emitFunction(*DS->SILFn, Fn);

    // Cache it.
    ScopeCache[DS] = llvm::TrackingMDNodeRef(SP);
    return SP;
  }

  llvm::DIDescriptor Parent = getOrCreateScope(DS->Parent);
  if (Opts.DebugInfoKind == IRGenDebugInfoKind::LineTables)
    return Parent;

  assert(DS->Parent && "lexical block must have a parent subprogram");
  Location L = getStartLocation(SM, DS->Loc);
  llvm::DIFile File = getOrCreateFile(L.Filename);
  llvm::DILexicalBlock DScope =
      DBuilder.createLexicalBlock(Parent, File, L.Line, L.Col);

  // Cache it.
  ScopeCache[DS] = llvm::TrackingMDNodeRef(DScope);

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
    if (llvm::Metadata *V = CachedFile->second)
      return llvm::DIFile(cast<llvm::MDNode>(V));
  }

  // Create a new one.
  StringRef File = BumpAllocatedString(llvm::sys::path::filename(Filename));
  llvm::SmallString<512> Path(Filename);
  llvm::sys::path::remove_filename(Path);
  std::error_code ec = llvm::sys::fs::make_absolute(Path);
  // Basically ignore any error.
  assert(ec == std::error_code());
  (void)ec; // Silence the unused variable warning
  llvm::DIFile F = DBuilder.createFile(File, BumpAllocatedString(Path));

  // Cache it.
  DIFileCache[Filename] = llvm::TrackingMDNodeRef(F);
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
      case AccessorKind::IsMaterializeForSet: Kind = ".materialize"; break;
      case AccessorKind::IsAddressor: Kind = ".addressor"; break;
      case AccessorKind::IsMutableAddressor: Kind = ".mutableAddressor"; break;
      }

      SmallVector<char, 64> Buf;
      StringRef Name = (VD->getName().str() + Twine(Kind)).toStringRef(Buf);
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

  if (L.isASTNode<DestructorDecl>())
    return "deinit";

  return StringRef();
}

static CanSILFunctionType getFunctionType(SILType SILTy) {
  if (!SILTy)
    return CanSILFunctionType();

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
    return llvm::DIScope(EntryPointFn);
  case DeclContextKind::Module: {
    auto File = getOrCreateFile(getFilenameFromDC(DC));
    return getOrCreateModule(TheCU, cast<Module>(DC)->getName().str(), File);
  }
  case DeclContextKind::FileUnit:
    // A module may contain multiple files.
    return getOrCreateContext(DC->getParent());
  case DeclContextKind::NominalTypeDecl: {
    auto CachedType = DITypeCache.find(
        cast<NominalTypeDecl>(DC)->getDeclaredType().getPointer());
    if (CachedType != DITypeCache.end()) {
      // Verify that the information still exists.
      if (llvm::Metadata *Val = CachedType->second) {
        auto DITy = llvm::DIType(cast<llvm::MDNode>(Val));
        if (DITy.Verify())
          return DITy;
      }
    }

    // Create a Forward-declared type.
    auto *TyDecl = cast<NominalTypeDecl>(DC);
    auto Loc = getLoc(SM, TyDecl);
    auto File = getOrCreateFile(Loc.Filename);
    auto Line = Loc.Line;
    auto FwdDecl = DBuilder.createForwardDecl(
        llvm::dwarf::DW_TAG_structure_type, TyDecl->getName().str(),
        getOrCreateContext(DC->getParent()), File, Line,
        llvm::dwarf::DW_LANG_Swift, 0, 0);

    return FwdDecl;
  }
  }
  return TheCU;
}

/// Create a single parameter type and push it.
void IRGenDebugInfo::createParameterType(
    llvm::SmallVectorImpl<llvm::Metadata *> &Parameters, SILType type,
    DeclContext *DeclCtx) {
  // FIXME: This use of getSwiftType() is extremely suspect.
  DebugTypeInfo DbgTy(type.getSwiftType(), IGM.getTypeInfo(type), DeclCtx);
  Parameters.push_back(getOrCreateType(DbgTy));
}

/// Create the array of function parameters for FnTy. SIL Version.
llvm::DITypeArray IRGenDebugInfo::createParameterTypes(SILType SILTy,
                                                   DeclContext *DeclCtx) {
  if (!SILTy)
    return llvm::DITypeArray();
  return createParameterTypes(SILTy.castTo<SILFunctionType>(), DeclCtx);
}

/// Create the array of function parameters for a function type.
llvm::DITypeArray IRGenDebugInfo::createParameterTypes(CanSILFunctionType FnTy,
                                                   DeclContext *DeclCtx) {
  SmallVector<llvm::Metadata *, 16> Parameters;

  GenericContextScope Scope(IGM, FnTy->getGenericSignature());

  // The function return type is the first element in the list.
  createParameterType(Parameters, FnTy->getSemanticResultSILType(),
                      DeclCtx);

  // Actually, the input type is either a single type or a tuple
  // type. We currently represent a function with one n-tuple argument
  // as an n-ary function.
  for (auto Param : FnTy->getParameters())
    createParameterType(Parameters, Param.getSILType(), DeclCtx);

  return DBuilder.getOrCreateTypeArray(Parameters);
}

/// FIXME: replace this condition with something more sane.
static bool isAllocatingConstructor(AbstractCC CC, DeclContext *DeclCtx) {
  return CC != AbstractCC::Method && DeclCtx && isa<ConstructorDecl>(DeclCtx);
}

llvm::DIDescriptor IRGenDebugInfo::
emitFunction(SILModule &SILMod, SILDebugScope *DS, llvm::Function *Fn,
             AbstractCC CC, SILType SILTy, DeclContext *DeclCtx) {
  // Returned a previously cached entry for an abstract (inlined) function.
  auto cached = ScopeCache.find(DS);
  if (cached != ScopeCache.end())
    return llvm::DIDescriptor(cast<llvm::MDNode>(cached->second));

  StringRef LinkageName;
  if (Fn)
    LinkageName = Fn->getName();
  else if (DS)
    LinkageName = DS->SILFn->getName();
  else
    llvm_unreachable("function has no mangled name");

  StringRef Name = LinkageName;
  if (DS) {
    if (DS->Loc.getKind() == SILLocation::SILFileKind)
      Name = DS->SILFn->getName();
    else
      Name = getName(DS->Loc);
  }

  Location L = {};
  unsigned ScopeLine = 0; /// The source line used for the function prologue.
  // Bare functions such as thunks should not have a line number. This
  // is especially important for shared functions like reabstraction
  // thunk helpers, where getLocation() returns an arbitrary location
  // of whichever use was emitted first.
  if (DS && !(DS->SILFn && DS->SILFn->isBare())) {
    auto FL = getLocation(SM, DS->Loc);
    L = FL.Loc;
    ScopeLine = FL.LocForLinetable.Line;
  }

  auto File = getOrCreateFile(L.Filename);
  auto Scope = MainModule;
  auto Line = L.Line;

  // We know that main always comes from MainFile.
  if (LinkageName == SWIFT_ENTRY_POINT_FUNCTION) {
    if (!L.Filename)
      File = MainFile;
    Line = 1;
    Name = LinkageName;
  }

  CanSILFunctionType FnTy = getFunctionType(SILTy);
  auto Params =
    Opts.DebugInfoKind == IRGenDebugInfoKind::LineTables ? llvm::DITypeArray()
    : createParameterTypes(SILTy, DeclCtx);
  llvm::DICompositeType DIFnTy = DBuilder.createSubroutineType(File, Params);
  llvm::DIArray TemplateParameters;
  llvm::DISubprogram Decl;

  // Various flags
  bool IsLocalToUnit = Fn ? Fn->hasInternalLinkage() : true;
  bool IsDefinition = true;
  bool IsOptimized = Opts.Optimize;
  unsigned Flags = 0;

  // Mark everything that is not visible from the source code (i.e.,
  // does not have a Swift name) as artificial, so the debugger can
  // ignore it. Explicit closures are exempt from this rule. We also
  // make an exception for main, which, albeit it does not
  // have a Swift name, does appear prominently in the source code.
  if ((Name.empty() && LinkageName != SWIFT_ENTRY_POINT_FUNCTION &&
       !isExplicitClosure(DS)) ||
      // ObjC thunks should also not show up in the linetable, because we
      // never want to set a breakpoint there.
      (CC == AbstractCC::ObjCMethod) || isAllocatingConstructor(CC, DeclCtx)) {
    Flags |= llvm::DIDescriptor::FlagArtificial;
    ScopeLine = 0;
  }

  if (FnTy && FnTy->getRepresentation() == FunctionType::Representation::Block)
    Flags |= llvm::DIDescriptor::FlagAppleBlock;

  llvm::DISubprogram SP = DBuilder.createFunction(
      Scope, Name, LinkageName, File, Line, DIFnTy, IsLocalToUnit, IsDefinition,
      ScopeLine, Flags, IsOptimized, Fn, TemplateParameters, Decl);

  // RAUW the entry point function forward declaration with the real thing.
  if (LinkageName == SWIFT_ENTRY_POINT_FUNCTION) {
    assert(EntryPointFn->isTemporary() &&
           "more than one entry point function");
    EntryPointFn->replaceAllUsesWith(SP);
    llvm::MDNode::deleteTemporary(EntryPointFn);
    EntryPointFn = SP;
  }

  if (!DS)
    return llvm::DIDescriptor();

  ScopeCache[DS] = llvm::TrackingMDNodeRef(SP);
  return SP;
}

/// TODO: This is no longer needed.
void IRGenDebugInfo::eraseFunction(llvm::Function *Fn) {}

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
  if (Opts.DebugInfoKind == IRGenDebugInfoKind::LineTables)
    return;

  // Imports are visited after SILFunctions.
  llvm::DIModule Module = MainModule;
  swift::Module *M = IGM.Context.getModule(D->getModulePath());
  if (!M && D->getModulePath()[0].first.str() == "Builtin")
    M = IGM.Context.TheBuiltinModule;
  if (!M) {
    assert(M && "Could not find module for import decl.");
    return;
  }
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

      if (first)
        first = false;
      else
        PS << '.';
      PS << Component;
    }
  }

  StringRef Name = BumpAllocatedString(Printed);
  unsigned Line = getLoc(SM, D).Line;
  createImportedModule(Name, Mangled, Module, Line);
}

// Create an imported module and import declarations for all functions
// from that module.
void IRGenDebugInfo::createImportedModule(StringRef Name, StringRef Mangled,
                                          llvm::DIModule Module,
                                          unsigned Line) {
  llvm::SmallString<512> Path(Module.getDirectory());
  llvm::sys::path::append(Path, Module.getFilename());
  auto File = getOrCreateFile(BumpAllocatedString(Path).data());
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
    if (llvm::Metadata *Val = CachedM->second)
      return llvm::DIModule(cast<llvm::MDNode>(Val));

  auto M = DBuilder.createModule(Parent, Name, File, 1);
  DIModuleCache[Name] = llvm::TrackingMDNodeRef(M);
  return M;
}

llvm::DIDescriptor IRGenDebugInfo::emitFunction(SILFunction &SILFn,
                                                llvm::Function *Fn) {
  auto *DS = SILFn.getDebugScope();
  if (DS && !DS->SILFn)
    DS->SILFn = &SILFn;

  return emitFunction(SILFn.getModule(), SILFn.getDebugScope(), Fn,
                      SILFn.getAbstractCC(), SILFn.getLoweredType(),
                      SILFn.getDeclContext());
}

void IRGenDebugInfo::emitArtificialFunction(SILModule &SILMod,
                                            IRBuilder &Builder,
                                            llvm::Function *Fn, SILType SILTy) {
  RegularLocation ALoc = RegularLocation::getAutoGeneratedLocation();
  SILDebugScope *Scope = new (SILMod) SILDebugScope(ALoc);
  emitFunction(SILMod, Scope, Fn, AbstractCC::Freestanding, SILTy);
  setCurrentLoc(Builder, Scope);
}

TypeAliasDecl *IRGenDebugInfo::getMetadataType() {
  if (!MetadataTypeDecl)
    MetadataTypeDecl = new (IGM.Context) TypeAliasDecl(
        SourceLoc(), IGM.Context.getIdentifier("$swift.type"), SourceLoc(),
        TypeLoc::withoutLoc(IGM.Context.TheRawPointerType),
        IGM.Context.TheBuiltinModule);
  return MetadataTypeDecl;
}

void IRGenDebugInfo::emitTypeMetadata(IRGenFunction &IGF,
                                      llvm::Value *Metadata,
                                      StringRef Name) {
  if (Opts.DebugInfoKind == IRGenDebugInfoKind::LineTables)
    return;

  auto TName = BumpAllocatedString(("$swift.type." + Name).str());
  DebugTypeInfo DbgTy(getMetadataType(), Metadata->getType(),
                      (Size)CI.getTargetInfo().getPointerWidth(0),
                      (Alignment)CI.getTargetInfo().getPointerAlign(0));
  emitVariableDeclaration(IGF.Builder, Metadata, DbgTy, IGF.getDebugScope(),
                          TName, llvm::dwarf::DW_TAG_auto_variable, 0,
                          // swift.type is a already pointer type,
                          // having a shadow copy doesn't add another
                          // layer of indirection.
                          DirectValue, ArtificialValue);
}

void IRGenDebugInfo::emitStackVariableDeclaration(
    IRBuilder &B, ArrayRef<llvm::Value *> Storage, DebugTypeInfo DbgTy,
    SILDebugScope *DS, StringRef Name, IndirectionKind Indirection) {
  emitVariableDeclaration(B, Storage, DbgTy, DS, Name,
                          llvm::dwarf::DW_TAG_auto_variable, 0, Indirection,
                          RealValue);
}

void IRGenDebugInfo::emitArgVariableDeclaration(
    IRBuilder &Builder, ArrayRef<llvm::Value *> Storage, DebugTypeInfo DbgTy,
    SILDebugScope *DS, StringRef Name, unsigned ArgNo,
    IndirectionKind Indirection, ArtificialKind IsArtificial) {
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
    case llvm::dwarf::DW_TAG_subprogram: {
      // Scopes are not indexed by UID.
      llvm::DITypeIdentifierMap EmptyMap;
      Scope = llvm::DISubprogram(Scope).getContext().resolve(EmptyMap);
      break;
    }
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

/// Return the storage size of an explosion value.
static uint64_t getSizeFromExplosionValue(const clang::TargetInfo &TI,
                                          llvm::Value *V) {
  llvm::Type *Ty = V->getType();
  if (unsigned PrimitiveSize = Ty->getPrimitiveSizeInBits())
    return PrimitiveSize;
  else if (Ty->isPointerTy())
    return TI.getPointerWidth(0);
  else
    llvm_unreachable("unhandled type of explosion value");
}

/// A generator that recursively returns the size of each element of a
/// composite type.
class ElementSizes {
  const TrackingDIRefMap &DIRefMap;
  SmallVector<const llvm::MDNode *, 12> Stack;

public:
  ElementSizes(const llvm::MDNode *DITy, const TrackingDIRefMap &DIRefMap)
      : DIRefMap(DIRefMap), Stack(1, DITy) {}

  struct SizeAlign {
    uint64_t SizeInBits, AlignInBits;
  };

  struct SizeAlign getNext() {
    if (Stack.empty())
      return {0, 0};

    llvm::DIType Cur(Stack.pop_back_val());
    if (Cur.isCompositeType() &&
        Cur.getTag() != llvm::dwarf::DW_TAG_subroutine_type) {
      llvm::DICompositeType CTy(Cur);
      llvm::DIArray Elts = CTy.getElements();
      unsigned N = Cur.getTag() == llvm::dwarf::DW_TAG_union_type
                       ? 1 // For unions, pick any one.
                       : Elts.getNumElements();

      if (N) {
        // Push all elements in reverse order.
        // FIXME: With a little more state we don't need to actually
        // store them on the Stack.
        for (unsigned I = N; I > 0; --I)
          Stack.push_back(Elts.getElement(I - 1));
        return getNext();
      }
    }
    switch (Cur.getTag()) {
    case llvm::dwarf::DW_TAG_member:
    case llvm::dwarf::DW_TAG_typedef: {
      // Replace top of stack.
      llvm::DIDerivedType DTy(Cur);
      Stack.push_back(resolve(DTy.getTypeDerivedFrom(), DIRefMap));
      return getNext();
    }
    default:
      return {Cur.getSizeInBits(), Cur.getAlignInBits()};
    }
  }
};

void IRGenDebugInfo::emitVariableDeclaration(
    IRBuilder &Builder, ArrayRef<llvm::Value *> Storage, DebugTypeInfo DbgTy,
    SILDebugScope *DS, StringRef Name, unsigned Tag, unsigned ArgNo,
    IndirectionKind Indirection, ArtificialKind Artificial) {
  // FIXME: Make this an assertion.
  //assert(DS && "variable has no scope");
  if (!DS)
    return;

  if (Opts.DebugInfoKind == IRGenDebugInfoKind::LineTables)
    return;

  llvm::DIDescriptor Scope = getOrCreateScope(DS);
  Location Loc = getLoc(SM, DbgTy.getDecl());

  // If this is an argument, attach it to the current function scope.
  if (ArgNo > 0) {
    while (Scope.isLexicalBlock())
      Scope = llvm::DILexicalBlock(Scope).getContext();
  }

  assert(Scope.Verify() && Scope.isScope() && "variable has no scope");

  llvm::DIFile Unit = getFile(Scope);
  // FIXME: this should be the scope of the type's declaration.
  llvm::DIType DITy = getOrCreateType(DbgTy);
  assert(DITy && "could not determine debug type of variable");

  unsigned Line = Loc.Line;
  unsigned Flags = 0;
  if (Artificial || DITy.isArtificial() || DITy == InternalType)
    Flags |= llvm::DIDescriptor::FlagArtificial;

  // Create the descriptor for the variable.
  llvm::DIVariable Var;
  llvm::DIExpression Expr = DBuilder.createExpression();

  if (Indirection) {
    // Classes are always passed by reference.
    int64_t Addr[] = { llvm::dwarf::DW_OP_deref };
    Expr = DBuilder.createExpression(Addr);
    // FIXME: assert(Flags == 0 && "Complex variables cannot have flags");
  }
  Var = DBuilder.createLocalVariable(
        Tag, Scope, Name, Unit, Line, DITy, Opts.Optimize, Flags, ArgNo);

  // Create inlined variables.
  if (DS && DS->InlinedCallSite) {
    auto *InlinedAt = createInlinedAt(DS);
    Var = createInlinedVariable(Var, InlinedAt, M.getContext());
  }

  // Insert a debug intrinsic into the current block.
  unsigned OffsetInBytes = 0;
  auto *BB = Builder.GetInsertBlock();
  bool IsPiece = Storage.size() > 1;
  uint64_t SizeOfByte = CI.getTargetInfo().getCharWidth();
  ElementSizes EltSizes(DITy, DIRefMap);
  for (llvm::Value *Piece : Storage) {
    assert(Piece && "already-claimed explosion value?");

    // There are variables without storage, such as "struct { func foo() {} }".
    // Emit them as constant 0.
    if (isa<llvm::UndefValue>(Piece))
      Piece = llvm::ConstantInt::get(llvm::Type::getInt64Ty(M.getContext()), 0);

    if (IsPiece) {
      if (Indirection)
        // Indirect pieces are not supported by LLVM.
        return;

      // Try to get the size from the type if possible.
      auto Dim = EltSizes.getNext();
      auto StorageSize = getSizeFromExplosionValue(CI.getTargetInfo(), Piece);
      // FIXME: Occasionally, there is a discrepancy between the AST
      // type and the Storage type. Usually this is due to reference
      // counting.
      if (!Dim.SizeInBits || (StorageSize && Dim.SizeInBits > StorageSize))
        Dim.SizeInBits = StorageSize;
      if (!Dim.AlignInBits)
        Dim.AlignInBits = SizeOfByte;

      unsigned SizeInBytes =
        llvm::RoundUpToAlignment(Dim.SizeInBits, Dim.AlignInBits) / SizeOfByte;

      // FIXME: Occasionally we miss out that the Storage is acually a
      // refcount wrapper. Silently skip these for now.
      unsigned VarSizeInBits = getSizeInBits(Var, DIRefMap);
      assert(VarSizeInBits > 0 && "zero-sized variable");
      if ((OffsetInBytes+SizeInBytes)*SizeOfByte > VarSizeInBits)
        break;
      if ((OffsetInBytes == 0) &&
          SizeInBytes*SizeOfByte == VarSizeInBits)
        break;
      if (SizeInBytes == 0)
        break;

      assert(SizeInBytes < VarSizeInBits
             && "piece covers entire var");
      assert((OffsetInBytes+SizeInBytes)*SizeOfByte<=VarSizeInBits
             && "pars > totum");
      Expr = DBuilder.createPieceExpression(OffsetInBytes, SizeInBytes);
      OffsetInBytes += SizeInBytes;
    }
    emitDbgIntrinsic(BB, Piece, Var, Expr, Line, Loc.Col, Scope, DS);
  }

  // Emit locationless intrinsic for variables that were optimized away.
  if (Storage.size() == 0) {
    auto *undef = llvm::UndefValue::get(DbgTy.StorageType);
    emitDbgIntrinsic(BB, undef, Var, Expr, Line, Loc.Col, Scope, DS);
  }
}

void IRGenDebugInfo::
emitDbgIntrinsic(llvm::BasicBlock *BB,
                 llvm::Value* Storage, llvm::DIVariable Var,
                 llvm::DIExpression Expr,
                 unsigned Line, unsigned Col, llvm::DIDescriptor Scope,
                 SILDebugScope *DS) {
  auto *Call = (isa<llvm::AllocaInst>(Storage) || isa<llvm::UndefValue>(Storage))
    ? DBuilder.insertDeclare(Storage, Var, Expr, BB)
    : DBuilder.insertDbgValueIntrinsic(Storage, 0, Var, Expr, BB);

  // Set the location/scope of the intrinsic.
  llvm::MDNode *InlinedAt = nullptr;
  if (DS && DS->InlinedCallSite) {
    assert(Scope && "Inlined location without a lexical scope");
    InlinedAt = createInlinedAt(DS);
  }
  Call->setDebugLoc(llvm::DebugLoc::get(Line, Col, Scope, InlinedAt));
}


void IRGenDebugInfo::emitGlobalVariableDeclaration(llvm::GlobalValue *Var,
                                                   StringRef Name,
                                                   StringRef LinkageName,
                                                   DebugTypeInfo DbgTy,
                                                   Optional<SILLocation> Loc) {
  if (Opts.DebugInfoKind == IRGenDebugInfoKind::LineTables)
    return;

  llvm::DIType Ty = getOrCreateType(DbgTy);
  if (Ty.isArtificial() || Ty == InternalType)
    // FIXME: Really these should be marked as artificial, but LLVM
    // currently has no support for flags to be put on global
    // variables. In the mean time, elide these variables, they
    // would confuse both the user and LLDB.
    return;

  Location L = getStartLocation(SM, Loc);
  auto File = getOrCreateFile(L.Filename);

  // Emit it as global variable of the current module.
  DBuilder.createGlobalVariable(MainModule, Name, LinkageName, File, L.Line, Ty,
                                Var->hasInternalLinkage(), Var, nullptr);
}

/// Return the mangled name of any nominal type, including the global
/// _Tt prefix, which marks the Swift namespace for types in DWARF.
StringRef IRGenDebugInfo::getMangledName(DebugTypeInfo DbgTy) {
  if (MetadataTypeDecl && DbgTy.getDecl() == MetadataTypeDecl)
    return BumpAllocatedString(DbgTy.getDecl()->getName().str());

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
llvm::DIDerivedType IRGenDebugInfo::createMemberType(
    DebugTypeInfo DbgTy, StringRef Name, unsigned &OffsetInBits,
    llvm::DIDescriptor Scope, llvm::DIFile File, unsigned Flags) {
  unsigned SizeOfByte = CI.getTargetInfo().getCharWidth();
  auto Ty = getOrCreateType(DbgTy);
  auto DITy = DBuilder.createMemberType(
      Scope, Name, File, 0, SizeOfByte * DbgTy.size.getValue(),
      SizeOfByte * DbgTy.align.getValue(), OffsetInBits, Flags, Ty);
  OffsetInBits += getSizeInBits(Ty, DIRefMap);
  OffsetInBits = llvm::RoundUpToAlignment(OffsetInBits,
                                          SizeOfByte * DbgTy.align.getValue());
  return DITy;
}

/// Return an array with the DITypes for each of a tuple's elements.
llvm::DIArray IRGenDebugInfo::getTupleElements(
    TupleType *TupleTy, llvm::DIDescriptor Scope, llvm::DIFile File,
    unsigned Flags, DeclContext *DeclContext, unsigned &SizeInBits) {
  SmallVector<llvm::Metadata *, 16> Elements;
  unsigned OffsetInBits = 0;
  for (auto ElemTy : TupleTy->getElementTypes()) {
    DebugTypeInfo DbgTy(ElemTy, IGM.getTypeInfoForUnlowered(ElemTy),
                        DeclContext);
    Elements.push_back(
        createMemberType(DbgTy, StringRef(), OffsetInBits, Scope, File, Flags));
  }
  SizeInBits = OffsetInBits;
  return DBuilder.getOrCreateArray(Elements);
}

/// Return an array with the DITypes for each of a struct's elements.
llvm::DIArray IRGenDebugInfo::getStructMembers(NominalTypeDecl *D, Type BaseTy,
                                               llvm::DIDescriptor Scope,
                                               llvm::DIFile File,
                                               unsigned Flags,
                                               unsigned &SizeInBits) {
  SmallVector<llvm::Metadata *, 16> Elements;
  unsigned OffsetInBits = 0;
  for (VarDecl *VD : D->getStoredProperties()) {
    auto memberTy =
        BaseTy->getTypeOfMember(IGM.SILMod->getSwiftModule(), VD, nullptr);
    DebugTypeInfo DbgTy(VD, IGM.getTypeInfoForUnlowered(
                                AbstractionPattern(VD->getType()), memberTy));
    Elements.push_back(createMemberType(DbgTy, VD->getName().str(),
                                        OffsetInBits, Scope, File, Flags));
  }
  if (OffsetInBits > SizeInBits)
    SizeInBits = OffsetInBits;
  return DBuilder.getOrCreateArray(Elements);
}

/// Create a temporary forward declaration for a struct and add it to
/// the type cache so we can safely build recursive types.
llvm::DICompositeType IRGenDebugInfo::createStructType(
    DebugTypeInfo DbgTy, NominalTypeDecl *Decl, Type BaseTy,
    llvm::DIDescriptor Scope, llvm::DIFile File, unsigned Line,
    unsigned SizeInBits, unsigned AlignInBits, unsigned Flags,
    llvm::DIType DerivedFrom, unsigned RuntimeLang, StringRef UniqueID) {
  StringRef Name = Decl->getName().str();

  // Forward declare this first because types may be recursive.
  auto FwdDecl = DBuilder.createReplaceableForwardDecl(
      llvm::dwarf::DW_TAG_structure_type, Name, Scope, File, Line,
        llvm::dwarf::DW_LANG_Swift, SizeInBits, AlignInBits);

#ifndef NDEBUG
  if (UniqueID.empty())
    assert(!Name.empty() && "no mangled name and no human readable name given");
  else
    assert(UniqueID.size() > 2 && UniqueID[0] == '_' && UniqueID[1] == 'T' &&
           "UID is not a mangled name");
#endif

  auto TH = llvm::TrackingMDNodeRef(FwdDecl);
  DITypeCache[DbgTy.getType()] = TH;
  auto Members = getStructMembers(Decl, BaseTy, Scope, File, Flags, SizeInBits);
  auto DITy = DBuilder.createStructType(
      Scope, Name, File, Line, SizeInBits, AlignInBits, Flags, DerivedFrom,
      Members, RuntimeLang, llvm::DIType(), UniqueID);
  FwdDecl->replaceAllUsesWith(DITy);
  llvm::MDNode::deleteTemporary(FwdDecl);
  return DITy;
}

/// Return an array with the DITypes for each of an enum's elements.
llvm::DIArray IRGenDebugInfo::getEnumElements(DebugTypeInfo DbgTy, EnumDecl *D,
                                              llvm::DIDescriptor Scope,
                                              llvm::DIFile File,
                                              unsigned Flags) {
  SmallVector<llvm::Metadata *, 16> Elements;
  for (auto *ElemDecl : D->getAllElements()) {
    // FIXME <rdar://problem/14845818> Support enums.
    // Swift Enums can be both like DWARF enums and DWARF unions.
    // They should probably be emitted as DW_TAG_variant_type.
    if (ElemDecl->hasType()) {
      // Use Decl as DeclContext.
      DebugTypeInfo ElemDbgTy;
      if (ElemDecl->hasArgumentType())
        ElemDbgTy = DebugTypeInfo(ElemDecl->getArgumentType(),
                                  DbgTy.StorageType,
                                  DbgTy.size, DbgTy.align, D);
      else
        if (D->hasRawType())
          ElemDbgTy = DebugTypeInfo(D->getRawType(), DbgTy.StorageType,
                                    DbgTy.size, DbgTy.align, D);
        else
          // Fallback to Int as the element type.
          ElemDbgTy = DebugTypeInfo(IGM.Context.getIntDecl()->getDeclaredType(),
                                    DbgTy.StorageType,
                                    DbgTy.size, DbgTy.align, D);
      unsigned Offset = 0;
      auto MTy = createMemberType(ElemDbgTy, ElemDecl->getName().str(), Offset,
                                  Scope, File, Flags);
      Elements.push_back(MTy);
    }
  }
  return DBuilder.getOrCreateArray(Elements);
}

/// Create a temporary forward declaration for an enum and add it to
/// the type cache so we can safely build recursive types.
llvm::DICompositeType
IRGenDebugInfo::createEnumType(DebugTypeInfo DbgTy, EnumDecl *Decl,
                               StringRef MangledName, llvm::DIDescriptor Scope,
                               llvm::DIFile File, unsigned Line,
                               unsigned Flags) {
  unsigned SizeOfByte = CI.getTargetInfo().getCharWidth();
  unsigned SizeInBits = DbgTy.size.getValue() * SizeOfByte;
  unsigned AlignInBits = DbgTy.align.getValue() * SizeOfByte;

  // FIXME: Is DW_TAG_union_type the right thing here?
  // Consider using a DW_TAG_variant_type instead.
  auto FwdDecl =
    DBuilder.createReplaceableForwardDecl(
      llvm::dwarf::DW_TAG_union_type, MangledName, Scope, File, Line,
        llvm::dwarf::DW_LANG_Swift, SizeInBits, AlignInBits);

  auto TH = llvm::TrackingMDNodeRef(FwdDecl);
  DITypeCache[DbgTy.getType()] = TH;

  auto DITy = DBuilder.createUnionType(
      Scope, MangledName, File, Line, SizeInBits, AlignInBits, Flags,
      getEnumElements(DbgTy, Decl, Scope, File, Flags),
      llvm::dwarf::DW_LANG_Swift);

  FwdDecl->replaceAllUsesWith(DITy);
  llvm::MDNode::deleteTemporary(FwdDecl);
  return DITy;
}

/// Return a DIType for Ty reusing any DeclContext found in DbgTy.
llvm::DIType IRGenDebugInfo::getOrCreateDesugaredType(Type Ty,
                                                      DebugTypeInfo DbgTy) {
  DebugTypeInfo BlandDbgTy(Ty, DbgTy.StorageType, DbgTy.size, DbgTy.align,
                           DbgTy.getDeclContext());
  return getOrCreateType(BlandDbgTy);
}

uint64_t IRGenDebugInfo::getSizeOfBasicType(DebugTypeInfo DbgTy) {
  uint64_t SizeOfByte = CI.getTargetInfo().getCharWidth();
  uint64_t BitWidth = DbgTy.size.getValue() * SizeOfByte;
  llvm::Type *StorageType = DbgTy.StorageType
                                ? DbgTy.StorageType
                                : IGM.DataLayout.getSmallestLegalIntType(
                                      IGM.getLLVMContext(), BitWidth);

  if (StorageType)
    return IGM.DataLayout.getTypeSizeInBits(StorageType);

  // This type is too large to fit in a register.
  assert(BitWidth > IGM.DataLayout.getLargestLegalIntTypeSize());
  return BitWidth;
}

/// Convenience function that creates a forward declaration for PointeeTy.
llvm::DIType IRGenDebugInfo::createPointerSizedStruct(
    llvm::DIDescriptor Scope, StringRef Name, llvm::DIFile File, unsigned Line,
    unsigned Flags, StringRef MangledName) {
  auto FwdDecl = DBuilder.createForwardDecl(llvm::dwarf::DW_TAG_structure_type,
                                            Name, Scope, File, Line,
                                            llvm::dwarf::DW_LANG_Swift, 0, 0);
  return createPointerSizedStruct(Scope, Name, FwdDecl, File, Line, Flags,
                                  MangledName);
}

/// Create a pointer-sized struct with a mangled name and a single
/// member of PointeeTy.
llvm::DIType IRGenDebugInfo::createPointerSizedStruct(
    llvm::DIDescriptor Scope, StringRef Name, llvm::DIType PointeeTy,
    llvm::DIFile File, unsigned Line, unsigned Flags, StringRef MangledName) {
  unsigned PtrSize = CI.getTargetInfo().getPointerWidth(0);
  unsigned PtrAlign = CI.getTargetInfo().getPointerAlign(0);
  auto PtrTy = DBuilder.createPointerType(PointeeTy, PtrSize, PtrAlign);
  llvm::Metadata *Elements[] = {
    DBuilder.createMemberType(Scope, "pointer", File, 0,
                              PtrSize, PtrAlign, 0, Flags, PtrTy)
  };
  return DBuilder.createStructType(
      Scope, Name, File, Line, PtrSize, PtrAlign, Flags,
      llvm::DIType(), // DerivedFrom
      DBuilder.getOrCreateArray(Elements), llvm::dwarf::DW_LANG_Swift,
      llvm::DIType(), MangledName);
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

  TypeBase *BaseTy = DbgTy.getType();

  if (!BaseTy) {
    DEBUG(llvm::dbgs() << "Type without TypeBase: "; DbgTy.getType()->dump();
          llvm::dbgs() << "\n");
    if (!InternalType) {
      StringRef Name = "<internal>";
      InternalType = DBuilder.createForwardDecl(
          llvm::dwarf::DW_TAG_structure_type, Name, Scope, File,
          /*Line*/ 0, llvm::dwarf::DW_LANG_Swift, SizeInBits, AlignInBits);
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
    auto *FloatTy = BaseTy->castTo<BuiltinFloatType>();
    // Assuming that the bitwidth and FloatTy->getFPKind() are identical.
    SizeInBits = FloatTy->getBitWidth();
    Encoding = llvm::dwarf::DW_ATE_float;
    break;
  }

  case TypeKind::BuiltinUnknownObject: {
    // The builtin opaque Objective-C pointer type. Useful for pushing
    // an Objective-C type through swift.
    unsigned PtrSize = CI.getTargetInfo().getPointerWidth(0);
    unsigned PtrAlign = CI.getTargetInfo().getPointerAlign(0);
    auto IdTy = DBuilder.createForwardDecl(
      llvm::dwarf::DW_TAG_structure_type, MangledName, Scope, File, 0,
        llvm::dwarf::DW_LANG_ObjC, 0, 0);
    return DBuilder.createPointerType(IdTy, PtrSize, PtrAlign, MangledName);
  }

  case TypeKind::BuiltinNativeObject: {
    unsigned PtrSize = CI.getTargetInfo().getPointerWidth(0);
    unsigned PtrAlign = CI.getTargetInfo().getPointerAlign(0);
    auto PTy = DBuilder.createPointerType(llvm::DIType(), PtrSize, PtrAlign,
                                          MangledName);
    return DBuilder.createObjectPointerType(PTy);
  }

  case TypeKind::BuiltinBridgeObject: {
    unsigned PtrSize = CI.getTargetInfo().getPointerWidth(0);
    unsigned PtrAlign = CI.getTargetInfo().getPointerAlign(0);
    auto PTy = DBuilder.createPointerType(llvm::DIType(), PtrSize, PtrAlign,
                                          MangledName);
    return DBuilder.createObjectPointerType(PTy);
  }

  case TypeKind::BuiltinRawPointer: {
    unsigned PtrSize = CI.getTargetInfo().getPointerWidth(0);
    unsigned PtrAlign = CI.getTargetInfo().getPointerAlign(0);
    return DBuilder.createPointerType(llvm::DIType(), PtrSize, PtrAlign,
                                      MangledName);
  }

  case TypeKind::DynamicSelf: {
    // Self. We don't have a way to represent instancetype in DWARF,
    // so we emit the static type instead. This is similar to what we
    // do with instancetype in Objective-C.
    auto *DynamicSelfTy = BaseTy->castTo<DynamicSelfType>();
    auto SelfTy = getOrCreateDesugaredType(DynamicSelfTy->getSelfType(), DbgTy);
    return DBuilder.createTypedef(SelfTy, MangledName, File, 0, File);

  }

  // Even builtin swift types usually come boxed in a struct.
  case TypeKind::Struct: {
    auto *StructTy = BaseTy->castTo<StructType>();
    auto *Decl = StructTy->getDecl();
    Location L = getLoc(SM, Decl);
    return createStructType(DbgTy, Decl, StructTy, Scope,
                            getOrCreateFile(L.Filename), L.Line, SizeInBits,
                            AlignInBits, Flags,
                            llvm::DIType(), // DerivedFrom
                            llvm::dwarf::DW_LANG_Swift, MangledName);
  }

  case TypeKind::Class: {
    // Classes are represented as DW_TAG_structure_type. This way the
    // DW_AT_APPLE_runtime_class( DW_LANG_Swift ) attribute can be
    // used to differentiate them from C++ and ObjC classes.
    auto *ClassTy = BaseTy->castTo<ClassType>();
    auto *Decl = ClassTy->getDecl();
    Location L = getLoc(SM, Decl);
    // TODO: We may want to peek at Decl->isObjC() and set this
    // attribute accordingly.
    auto RuntimeLang = llvm::dwarf::DW_LANG_Swift;
    if (auto *ClangDecl = Decl->getClangDecl()) {
      auto ClangSrcLoc = ClangDecl->getLocStart();
      clang::SourceManager &ClangSM =
          CI.getClangASTContext().getSourceManager();
      L.Line = ClangSM.getPresumedLineNumber(ClangSrcLoc);
      L.Filename = ClangSM.getBufferName(ClangSrcLoc);

     // Use "ObjectiveC" as default for implicit decls.
      // FIXME 1: Do something more clever based on the decl's mangled name.
      // FIXME 2: Clang submodules are not handled here.
      StringRef ModuleName = "ObjectiveC";
      if (auto *OwningModule = ClangDecl->getOwningModule())
        ModuleName = OwningModule->getTopLevelModuleName();

      auto ModuleFile = getOrCreateFile(L.Filename);
      // This placeholder gets RAUW'd by finalize().
      Scope = getOrCreateModule(ModuleFile, ModuleName, ModuleFile);
    }
    return createStructType(DbgTy, Decl, ClassTy, Scope,
                            getOrCreateFile(L.Filename), L.Line, SizeInBits,
                            AlignInBits, Flags,
                            llvm::DIType(), // DerivedFrom
                            RuntimeLang, MangledName);
  }

  case TypeKind::Protocol: {
    auto *ProtocolTy = BaseTy->castTo<ProtocolType>();
    auto *Decl = ProtocolTy->getDecl();
    // FIXME: (LLVM branch) This should probably be a DW_TAG_interface_type.
    Location L = getLoc(SM, Decl);
    auto File = getOrCreateFile(L.Filename);
    return createPointerSizedStruct(Scope,
                                    Decl ? Decl->getNameStr() : MangledName,
                                    File, L.Line, Flags, MangledName);
  }

  case TypeKind::ProtocolComposition: {
    auto *Decl = DbgTy.getDecl();
    Location L = getLoc(SM, Decl);
    auto File = getOrCreateFile(L.Filename);

    // FIXME: emit types
    // auto ProtocolCompositionTy = BaseTy->castTo<ProtocolCompositionType>();
    return createPointerSizedStruct(Scope,
                                    Decl ? Decl->getNameStr() : MangledName,
                                    File, L.Line, Flags, MangledName);
  }

  case TypeKind::UnboundGeneric: {
    auto *UnboundTy = BaseTy->castTo<UnboundGenericType>();
    auto *Decl = UnboundTy->getDecl();
    Location L = getLoc(SM, Decl);
    return createPointerSizedStruct(Scope,
                                    Decl ? Decl->getNameStr() : MangledName,
                                    File, L.Line, Flags, MangledName);
  }

  case TypeKind::BoundGenericStruct: {
    auto *StructTy = BaseTy->castTo<BoundGenericStructType>();
    auto *Decl = StructTy->getDecl();
    Location L = getLoc(SM, Decl);
    return createPointerSizedStruct(Scope,
                                    Decl ? Decl->getNameStr() : MangledName,
                                    File, L.Line, Flags, MangledName);
  }

  case TypeKind::BoundGenericClass: {
    auto *ClassTy = BaseTy->castTo<BoundGenericClassType>();
    auto *Decl = ClassTy->getDecl();
    Location L = getLoc(SM, Decl);
    // TODO: We may want to peek at Decl->isObjC() and set this
    // attribute accordingly.
    return createPointerSizedStruct(Scope,
                                    Decl ? Decl->getNameStr() : MangledName,
                                    File, L.Line, Flags, MangledName);
  }

  case TypeKind::Tuple: {
    auto *TupleTy = BaseTy->castTo<TupleType>();
    // Tuples are also represented as structs.
    unsigned RealSize;
    auto Elements = getTupleElements(TupleTy, Scope, MainFile, Flags,
                                     DbgTy.getDeclContext(), RealSize);
    return DBuilder.createStructType(
        Scope, MangledName, File, 0, RealSize, AlignInBits, Flags,
        llvm::DIType(), // DerivedFrom
        Elements, llvm::dwarf::DW_LANG_Swift, llvm::DIType(), MangledName);
  }

  case TypeKind::InOut: {
    // This is an inout type. Naturally we would be emitting them as
    // DW_TAG_reference_type types, but LLDB can deal better with pointer-sized
    // struct that has the appropriate mangled name.
    auto ObjectTy = BaseTy->castTo<InOutType>()->getObjectType();
    auto DT = getOrCreateDesugaredType(ObjectTy, DbgTy);
    return createPointerSizedStruct(Scope, MangledName, DT, File, 0, Flags,
                                    BaseTy->isUnspecializedGeneric()
                                    ? StringRef() : MangledName);
  }

  case TypeKind::Archetype: {
    auto *Archetype = BaseTy->castTo<ArchetypeType>();
    Location L = getLoc(SM, Archetype->getAssocType());
    auto Superclass = Archetype->getSuperclass();
    auto DerivedFrom = Superclass.isNull()
                           ? llvm::DIType()
                           : getOrCreateDesugaredType(Superclass, DbgTy);
    // Essentially a %swift.opaque pointer.
    unsigned SizeInBits = DbgTy.StorageType->isSized()
      ? IGM.DataLayout.getTypeSizeInBits(DbgTy.StorageType)
      : IGM.DataLayout.getPointerSizeInBits();

    auto FwdDecl = DBuilder.createReplaceableForwardDecl(
      llvm::dwarf::DW_TAG_structure_type, MangledName, Scope, File, L.Line,
        llvm::dwarf::DW_LANG_Swift, SizeInBits, AlignInBits);

    // Emit the protocols the archetypes conform to.
    SmallVector<llvm::Metadata *, 4> Protocols;
    for (auto *ProtocolDecl : Archetype->getConformsTo()) {
      auto PTy = IGM.SILMod->Types.getLoweredType(ProtocolDecl->getType())
                     .getSwiftRValueType();
      auto PDbgTy = DebugTypeInfo(ProtocolDecl, IGM.getTypeInfoForLowered(PTy));
      auto PDITy = getOrCreateType(PDbgTy);
      Protocols.push_back(DBuilder.createInheritance(FwdDecl, PDITy, 0, Flags));
    }
    auto DITy = DBuilder.createStructType(
        Scope, MangledName, File, L.Line, SizeInBits, AlignInBits, Flags,
        DerivedFrom, DBuilder.getOrCreateArray(Protocols),
        llvm::dwarf::DW_LANG_Swift, llvm::DIType(),
        StringRef() /*don't unique*/);

    FwdDecl->replaceAllUsesWith(DITy);
    llvm::MDNode::deleteTemporary(FwdDecl);
    return DITy;
  }

  case TypeKind::ExistentialMetatype:
  case TypeKind::Metatype: {
    // Metatypes are (mostly) singleton type descriptors, often without storage.
    Flags |= llvm::DIDescriptor::FlagArtificial;
    Location L = getLoc(SM, DbgTy.getDecl());
    auto File = getOrCreateFile(L.Filename);
    return DBuilder.createStructType(
        Scope, MangledName, File, L.Line, SizeInBits, AlignInBits, Flags,
        llvm::DIType(), llvm::DIArray(), llvm::dwarf::DW_LANG_Swift,
        llvm::DIType(), MangledName);
  }

  case TypeKind::SILFunction:
  case TypeKind::Function:
  case TypeKind::PolymorphicFunction:
  case TypeKind::GenericFunction: {
    CanSILFunctionType FunctionTy;
    if (auto *SILFnTy = dyn_cast<SILFunctionType>(BaseTy))
      FunctionTy = CanSILFunctionType(SILFnTy);
    // FIXME: Handling of generic parameters in SIL type lowering is in flux.
    // DebugInfo doesn't appear to care about the generic context, so just
    // throw it away before lowering.
    else if (isa<GenericFunctionType>(BaseTy) ||
             isa<PolymorphicFunctionType>(BaseTy)) {
      auto *fTy = cast<AnyFunctionType>(BaseTy);
      auto *nongenericTy = FunctionType::get(fTy->getInput(), fTy->getResult(),
                                            fTy->getExtInfo());

      FunctionTy = IGM.SILMod->Types.getLoweredType(nongenericTy)
                       .castTo<SILFunctionType>();
    } else
      FunctionTy =
          IGM.SILMod->Types.getLoweredType(BaseTy).castTo<SILFunctionType>();
    auto Params = createParameterTypes(FunctionTy, DbgTy.getDeclContext());

    // Functions are actually stored as a Pointer or a FunctionPairTy:
    // { i8*, %swift.refcounted* }
    auto FnTy = DBuilder.createSubroutineType(MainFile, Params, Flags);
    return createPointerSizedStruct(Scope, MangledName, FnTy,
                                    MainFile, 0, Flags, MangledName);

  }

  case TypeKind::Enum: {
    auto *EnumTy = BaseTy->castTo<EnumType>();
    auto *Decl = EnumTy->getDecl();
    Location L = getLoc(SM, Decl);
    return createEnumType(DbgTy, Decl, MangledName, Scope,
                          getOrCreateFile(L.Filename), L.Line, Flags);
  }

  case TypeKind::BoundGenericEnum: {
    auto *EnumTy = BaseTy->castTo<BoundGenericEnumType>();
    auto *Decl = EnumTy->getDecl();
    Location L = getLoc(SM, Decl);
    return createEnumType(DbgTy, Decl, MangledName, Scope,
                          getOrCreateFile(L.Filename), L.Line, Flags);
  }

  case TypeKind::BuiltinVector: {
    (void)MangledName; // FIXME emit the name somewhere.
    auto *BuiltinVectorTy = BaseTy->castTo<BuiltinVectorType>();
    DebugTypeInfo ElemDbgTy(BuiltinVectorTy->getElementType(),
                            DbgTy.StorageType,
                            DbgTy.size, DbgTy.align, DbgTy.getDeclContext());
    auto Subscripts = llvm::DIArray();
    return DBuilder.createVectorType(BuiltinVectorTy->getNumElements(),
                                     AlignInBits, getOrCreateType(ElemDbgTy),
                                     Subscripts);
  }

  // Reference storage types.
  case TypeKind::UnownedStorage:
  case TypeKind::UnmanagedStorage:
  case TypeKind::WeakStorage: {
    auto *ReferenceTy = cast<ReferenceStorageType>(BaseTy);
    auto CanTy = ReferenceTy->getReferentType();
    Location L = getLoc(SM, DbgTy.getDecl());
    auto File = getOrCreateFile(L.Filename);
    return DBuilder.createTypedef(getOrCreateDesugaredType(CanTy, DbgTy),
                                  MangledName, File, L.Line, File);
  }

  // Sugared types.

  case TypeKind::NameAlias: {

    auto *NameAliasTy = cast<NameAliasType>(BaseTy);
    auto *Decl = NameAliasTy->getDecl();
    Location L = getLoc(SM, Decl);
    auto AliasedTy = Decl->getUnderlyingType();
    auto File = getOrCreateFile(L.Filename);
    // For NameAlias types, the DeclContext for the aliasED type is
    // in the decl of the alias type.
    DebugTypeInfo AliasedDbgTy(AliasedTy, DbgTy.StorageType,
                               DbgTy.size, DbgTy.align, DbgTy.getDeclContext());
    return DBuilder.createTypedef(getOrCreateType(AliasedDbgTy), MangledName,
                                  File, L.Line, File);
  }

  case TypeKind::Substituted: {
    auto OrigTy = cast<SubstitutedType>(BaseTy)->getReplacementType();
    return getOrCreateDesugaredType(OrigTy, DbgTy);
  }

  case TypeKind::Paren: {
    auto Ty = cast<ParenType>(BaseTy)->getUnderlyingType();
    return getOrCreateDesugaredType(Ty, DbgTy);
  }

  // SyntaxSugarType derivations.
  case TypeKind::ArraySlice:
  case TypeKind::Optional:
  case TypeKind::ImplicitlyUnwrappedOptional: {
    auto *SyntaxSugarTy = cast<SyntaxSugarType>(BaseTy);
    auto *CanTy = SyntaxSugarTy->getSinglyDesugaredType();
    return getOrCreateDesugaredType(CanTy, DbgTy);
  }

  case TypeKind::Dictionary: {
    auto *DictionaryTy = cast<DictionaryType>(BaseTy);
    auto *CanTy = DictionaryTy->getDesugaredType();
    return getOrCreateDesugaredType(CanTy, DbgTy);
  }
    
  case TypeKind::GenericTypeParam: {
    auto *ParamTy = cast<GenericTypeParamType>(BaseTy);
    // FIXME: Provide a more meaningful debug type.
    return DBuilder.createUnspecifiedType(ParamTy->getName().str());
  }
  case TypeKind::DependentMember: {
    auto *MemberTy = cast<DependentMemberType>(BaseTy);
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
  case TypeKind::BuiltinUnsafeValueBuffer:

    DEBUG(llvm::errs() << "Unhandled type: "; DbgTy.getType()->dump();
          llvm::errs() << "\n");
    MangledName = "<unknown>";
  }
  return DBuilder.createBasicType(MangledName, SizeInBits, AlignInBits,
                                  Encoding);
}

/// Determine if there exists a name mangling for the given type.
static bool canMangle(TypeBase *Ty) {
  switch (Ty->getKind()) {
  case TypeKind::PolymorphicFunction: // Mangler crashes.
  case TypeKind::GenericFunction:     // Not yet supported.
  case TypeKind::SILBlockStorage:     // Not suported at all.
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
    if (llvm::Metadata *Val = CachedType->second) {
      auto DITy = llvm::DIType(cast<llvm::MDNode>(Val));
      if (DITy.Verify())
        return DITy;
    }
  }

  // Second line of defense: Look up the mangled name. TypeBase*'s are
  // not necessarily unique, but name mangling is too expensive to do
  // every time.
  StringRef MangledName;
  llvm::MDString *UID = nullptr;
  if (canMangle(DbgTy.getType())) {
    MangledName = getMangledName(DbgTy);
    UID = llvm::MDString::get(IGM.getLLVMContext(), MangledName);
    if (llvm::Metadata *CachedTy = DIRefMap.lookup(UID)) {
      auto DITy = llvm::DIType(cast<llvm::MDNode>(CachedTy));
      if (DITy.Verify())
        return DITy;
    }
  }

  // Retrieve the context of the type, as opposed to the DeclContext
  // of the variable.
  //
  // FIXME: Builtin and qualified types in LLVM have no parent
  // scope. TODO: This can be fixed by extending DIBuilder.
  DeclContext *Context = DbgTy.getType()->getNominalOrBoundGenericNominal();
  if (Context)
    Context = Context->getParent();
  llvm::DIScope Scope = getOrCreateContext(Context);
  llvm::DIType DITy = createType(DbgTy, MangledName, Scope, getFile(Scope));
  assert(DITy.Verify() && "type did not verify");

  // Incrementally build the DIRefMap.
  if (DITy.isCompositeType()) {
#ifndef NDEBUG
    // Sanity check.
    if (llvm::Metadata *V = DIRefMap.lookup(UID)) {
      llvm::DIType CachedTy(cast<llvm::MDNode>(V));
      if (CachedTy.Verify())
        assert(CachedTy == DITy && "conflicting types for one UID");
    }
#endif
    // If this type supports a UID, enter it to the cache.
    if (auto *UID = llvm::DICompositeType(DITy).getIdentifier()) {
      assert(UID->getString() == MangledName &&
             "Unique identifier is different from mangled name ");
      DIRefMap[UID] = llvm::TrackingMDNodeRef(DITy);
    }
  }

  // After the specializer did its work, archetypes may have different
  // storage types so we can't cache them based on their Swift type.
  if (DbgTy.getType()->isUnspecializedGeneric()) {
    // In order to create recursive data structures we temporarily
    // still insert them into the cache, so remove them.
    DITypeCache.erase(DbgTy.getType());
    return DITy;
  }

  // Store it in the cache.
  DITypeCache[DbgTy.getType()] = llvm::TrackingMDNodeRef(DITy);

  return DITy;
}

void IRGenDebugInfo::finalize() {
  assert(LocationStack.empty() && "Mismatch of pushLoc() and popLoc().");

  // Finalize the DIBuilder.
  DBuilder.finalize();
}
