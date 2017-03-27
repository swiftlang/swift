//===--- IRGenDebugInfo.cpp - Debug Info Support --------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  This file implements IR debug info generation for Swift.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "debug-info"
#include "IRGenDebugInfo.h"
#include "GenOpaque.h"
#include "GenType.h"
#include "Linking.h"
#include "swift/AST/Expr.h"
#include "swift/AST/IRGenOptions.h"
#include "swift/AST/ASTMangler.h"
#include "swift/AST/Module.h"
#include "swift/AST/ModuleLoader.h"
#include "swift/AST/Pattern.h"
#include "swift/Basic/Dwarf.h"
#include "swift/Basic/SourceManager.h"
#include "swift/Basic/Version.h"
#include "swift/Demangling/ManglingMacros.h"
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
#include "llvm/Transforms/Utils/Local.h"

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

/// Return the size reported by a type.
static unsigned getSizeInBits(llvm::DIType *Ty) {
  // Follow derived types until we reach a type that
  // reports back a size.
  while (isa<llvm::DIDerivedType>(Ty) && !Ty->getSizeInBits()) {
    auto *DT = cast<llvm::DIDerivedType>(Ty);
    Ty = DT->getBaseType().resolve();
    if (!Ty)
      return 0;
  }
  return Ty->getSizeInBits();
}

#ifndef NDEBUG

/// Return the size reported by the variable's type.
static unsigned getSizeInBits(const llvm::DILocalVariable *Var) {
  llvm::DIType *Ty = Var->getType().resolve();
  return getSizeInBits(Ty);
}

#endif

IRGenDebugInfo::IRGenDebugInfo(const IRGenOptions &Opts,
                               ClangImporter &CI,
                               IRGenModule &IGM,
                               llvm::Module &M,
                               SourceFile *SF)
  : Opts(Opts),
    CI(CI),
    SM(IGM.Context.SourceMgr),
    M(M),
    DBuilder(M),
    IGM(IGM),
    MetadataTypeDecl(nullptr),
    InternalType(nullptr),
    LastDebugLoc({}),
    LastScope(nullptr)
{
  assert(Opts.DebugInfoKind > IRGenDebugInfoKind::None
         && "no debug info should be generated");
  StringRef SourceFileName = SF ? SF->getFilename() :
                                  StringRef(Opts.MainInputFilename);
  StringRef Dir;
  llvm::SmallString<256> AbsMainFile;
  if (SourceFileName.empty())
    AbsMainFile = "<unknown>";
  else {
    AbsMainFile = SourceFileName;
    llvm::sys::fs::make_absolute(AbsMainFile);
  }

  unsigned Lang = llvm::dwarf::DW_LANG_Swift;
  std::string Producer = version::getSwiftFullVersion(
    IGM.Context.LangOpts.EffectiveLanguageVersion);
  bool IsOptimized = Opts.Optimize;
  StringRef Flags = Opts.DWARFDebugFlags;
  unsigned Major, Minor;
  std::tie(Major, Minor) = version::getSwiftNumericVersion();
  unsigned MajorRuntimeVersion = Major;

  // No split DWARF on Darwin.
  StringRef SplitName = StringRef();
  // Note that File + Dir need not result in a valid path.
  // Clang is doing the same thing here.
  TheCU = DBuilder.createCompileUnit(
      Lang, DBuilder.createFile(AbsMainFile, Opts.DebugCompilationDir),
      Producer, IsOptimized, Flags, MajorRuntimeVersion, SplitName,
      Opts.DebugInfoKind > IRGenDebugInfoKind::LineTables
          ? llvm::DICompileUnit::FullDebug
          : llvm::DICompileUnit::LineTablesOnly);
  MainFile = getOrCreateFile(BumpAllocatedString(AbsMainFile));

  // Because the swift compiler relies on Clang to setup the Module,
  // the clang CU is always created first.  Several dwarf-reading
  // tools (older versions of ld64, and lldb) can get confused if the
  // first CU in an object is empty, so ensure that the Swift CU comes
  // first by rearranging the list of CUs in the LLVM module.
  llvm::NamedMDNode *CU_Nodes = M.getNamedMetadata("llvm.dbg.cu");
  SmallVector<llvm::DICompileUnit *, 2> CUs;
  for (auto *N : CU_Nodes->operands())
    CUs.push_back(cast<llvm::DICompileUnit>(N));
  CU_Nodes->dropAllReferences();
  for (auto CU = CUs.rbegin(), CE = CUs.rend(); CU != CE; ++CU)
    CU_Nodes->addOperand(*CU);

  // Create a module for the current compile unit.
  llvm::sys::path::remove_filename(AbsMainFile);
  MainModule =
      getOrCreateModule(Opts.ModuleName, TheCU, Opts.ModuleName, AbsMainFile);
  DBuilder.createImportedModule(MainFile, MainModule, 1);
}

static StringRef getFilenameFromDC(const DeclContext *DC) {
  if (auto LF = dyn_cast<LoadedFile>(DC))
    return LF->getFilename();
  if (auto SF = dyn_cast<SourceFile>(DC))
    return SF->getFilename();
  else if (auto M = dyn_cast<ModuleDecl>(DC))
    return M->getModuleFilename();
  else
    return StringRef();
}

SILLocation::DebugLoc getDeserializedLoc(Pattern *) { return {}; }
SILLocation::DebugLoc getDeserializedLoc(Expr *) { return {}; }
SILLocation::DebugLoc getDeserializedLoc(Stmt *) { return {}; }
SILLocation::DebugLoc getDeserializedLoc(Decl *D) {
  SILLocation::DebugLoc L;
  const DeclContext *DC = D->getDeclContext()->getModuleScopeContext();
  StringRef Filename = getFilenameFromDC(DC);
  if (!Filename.empty())
    L.Filename = Filename;
  return L;
}

/// Use the SM to figure out the actual line/column of a SourceLoc.
template <typename WithLoc>
SILLocation::DebugLoc getDebugLoc(SourceManager &SM, WithLoc *S,
                                  bool End = false) {
  SILLocation::DebugLoc L;
  if (S == nullptr)
    return L;

  SourceLoc Loc = End ? S->getEndLoc() : S->getStartLoc();
  if (Loc.isInvalid())
    // This may be a deserialized or clang-imported decl. And modules
    // don't come with SourceLocs right now. Get at least the name of
    // the module.
    return getDeserializedLoc(S);

  return SILLocation::decode(Loc, SM);
}

/// Return the start of the location's source range.
static SILLocation::DebugLoc getStartLocation(Optional<SILLocation> OptLoc,
                                              SourceManager &SM) {
  if (!OptLoc) return {};
  return SILLocation::decode(OptLoc->getStartSourceLoc(), SM);
}

/// Return the debug location from a SILLocation.
static SILLocation::DebugLoc getDebugLocation(Optional<SILLocation> OptLoc,
                                              SourceManager &SM) {
  if (!OptLoc || OptLoc->isInPrologue())
    return {};
  return OptLoc->decodeDebugLoc(SM);
}


/// Determine whether this debug scope belongs to an explicit closure.
static bool isExplicitClosure(const SILFunction *SILFn) {
  if (SILFn && SILFn->hasLocation())
    if (Expr *E = SILFn->getLocation().getAsASTNode<Expr>())
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

llvm::MDNode *IRGenDebugInfo::createInlinedAt(const SILDebugScope *DS) {
  llvm::MDNode *InlinedAt = nullptr;
  if (DS) {
    // The inlined-at chain, starting with the innermost (noninlined) scope.
    auto Scopes = DS->flattenedInlineTree();

    // See if we share a common prefix with the last chain of inline scopes.
    unsigned N = 0;
    while (N < LastInlineChain.size() && N < Scopes.size() &&
           LastInlineChain[N].first == Scopes[N])
      InlinedAt = LastInlineChain[N++].second;
    LastInlineChain.resize(N);

    // Construct the new suffix.
    for (; N < Scopes.size(); ++N) {
      auto *CS = Scopes[N];
      // In SIL the inlined-at information is part of the scopes, in
      // LLVM IR it is part of the location. Transforming the inlined-at
      // SIL scope to a location means skipping the inlined-at scope.
      auto *Parent = CS->Parent.get<const SILDebugScope *>();
      auto *ParentScope = getOrCreateScope(Parent);
      auto L = CS->Loc.decodeDebugLoc(SM);
      InlinedAt = llvm::DebugLoc::get(L.Line, L.Column, ParentScope, InlinedAt);

      // Cache the suffix.
      LastInlineChain.push_back({CS, llvm::TrackingMDNodeRef(InlinedAt)});
    }
  }
  return InlinedAt;
}

#ifndef NDEBUG
/// Perform a couple of sanity checks on scopes.
static bool parentScopesAreSane(const SILDebugScope *DS) {
  auto *Parent = DS;
  while ((Parent = Parent->Parent.dyn_cast<const SILDebugScope *>())) {
    if (!DS->InlinedCallSite)
      assert(!Parent->InlinedCallSite &&
             "non-inlined scope has an inlined parent");
  }
  return true;
}

bool IRGenDebugInfo::lineNumberIsSane(IRBuilder &Builder, unsigned Line) {
  if (IGM.IRGen.Opts.Optimize)
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

void IRGenDebugInfo::setCurrentLoc(IRBuilder &Builder, const SILDebugScope *DS,
                                   Optional<SILLocation> Loc) {
  assert(DS && "empty scope");
  // Inline info is emitted as part of the location below; extract the
  // original scope here.
  auto *Scope = getOrCreateScope(DS->getInlinedScope());
  if (!Scope)
    return;

  auto L = getDebugLocation(Loc, SM);
  auto *File = getOrCreateFile(L.Filename);
  if (File->getFilename() != Scope->getFilename()) {
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
  if (Loc && isAbstractClosure(*Loc) && DS && !isAbstractClosure(DS->Loc) &&
      !Loc->is<ImplicitReturnLocation>())
    L.Line = L.Column = 0;

  if (auto *Fn = DS->getInlinedFunction())
    if (Fn->isThunk())
      L.Line = L.Column = 0;

  // Reuse the last source location if we are still in the same
  // scope to get a more contiguous line table.
  if (L.Line == 0 && DS == LastScope)
    L = LastDebugLoc;

  // FIXME: Enable this assertion.
  //assert(lineNumberIsSane(Builder, L.Line) &&
  //       "-Onone, but line numbers are not monotonically increasing within bb");
  LastDebugLoc = L;
  LastScope = DS;

  auto *InlinedAt = createInlinedAt(DS);
  assert(((!InlinedAt) || (InlinedAt && Scope)) && "inlined w/o scope");
  assert(parentScopesAreSane(DS) && "parent scope sanity check failed");
  auto DL = llvm::DebugLoc::get(L.Line, L.Column, Scope, InlinedAt);
  // TODO: Write a strongly-worded letter to the person that came up
  // with a pair of functions spelled "get" and "Set".
  Builder.SetCurrentDebugLocation(DL);
}

void IRGenDebugInfo::setEntryPointLoc(IRBuilder &Builder) {
  auto DL = llvm::DebugLoc::get(0, 0, getEntryPointFn(), nullptr);
  Builder.SetCurrentDebugLocation(DL);
}

llvm::DIScope *IRGenDebugInfo::getOrCreateScope(const SILDebugScope *DS) {
  if (DS == nullptr)
    return MainFile;

  // Try to find it in the cache first.
  auto CachedScope = ScopeCache.find(DS);
  if (CachedScope != ScopeCache.end())
    return cast<llvm::DIScope>(CachedScope->second);

  // If this is an (inlined) function scope, the function may
  // not have been created yet.
  if (auto *SILFn = DS->Parent.dyn_cast<SILFunction *>()) {
    auto *FnScope = SILFn->getDebugScope();
    // FIXME: This is a bug in the SIL deserialization.
    if (!FnScope)
      SILFn->setDebugScope(DS);

    auto CachedScope = ScopeCache.find(FnScope);
    if (CachedScope != ScopeCache.end())
      return cast<llvm::DIScope>(CachedScope->second);

    // Force the debug info for the function to be emitted, even if it
    // is external or has been inlined.
    llvm::Function *Fn = nullptr;
    if (!SILFn->getName().empty() && !SILFn->isZombie())
      Fn = IGM.getAddrOfSILFunction(SILFn, NotForDefinition);
    auto *SP = emitFunction(*SILFn, Fn);

    // Cache it.
    ScopeCache[DS] = llvm::TrackingMDNodeRef(SP);
    return SP;
  }

  auto *ParentScope = DS->Parent.get<const SILDebugScope *>();
  llvm::DIScope *Parent = getOrCreateScope(ParentScope);
  assert(isa<llvm::DILocalScope>(Parent) && "not a local scope");

  if (Opts.DebugInfoKind <= IRGenDebugInfoKind::LineTables)
    return Parent;

  assert(DS->Parent && "lexical block must have a parent subprogram");
  auto L = getStartLocation(DS->Loc, SM);
  llvm::DIFile *File = getOrCreateFile(L.Filename);
  auto *DScope = DBuilder.createLexicalBlock(Parent, File, L.Line, L.Column);

  // Cache it.
  ScopeCache[DS] = llvm::TrackingMDNodeRef(DScope);

  return DScope;
}

llvm::DIFile *IRGenDebugInfo::getOrCreateFile(StringRef Filename) {
  if (Filename.empty())
    return MainFile;

  if (MainFile) {
    SmallString<256> AbsMainFile, ThisFile;
    AbsMainFile = Filename;
    llvm::sys::fs::make_absolute(AbsMainFile);
    llvm::sys::path::append(ThisFile, MainFile->getDirectory(),
                            MainFile->getFilename());
    if (ThisFile == AbsMainFile) {
      DIFileCache[Filename] = llvm::TrackingMDNodeRef(MainFile);
      return MainFile;
    }
  }

  // Look in the cache first.
  auto CachedFile = DIFileCache.find(Filename);

  if (CachedFile != DIFileCache.end()) {
    // Verify that the information still exists.
    if (llvm::Metadata *V = CachedFile->second)
      return cast<llvm::DIFile>(V);
  }

  // Create a new one.
  StringRef File = llvm::sys::path::filename(Filename);
  llvm::SmallString<512> Path(Filename);
  llvm::sys::path::remove_filename(Path);
  llvm::DIFile *F = DBuilder.createFile(File, Path);

  // Cache it.
  DIFileCache[Filename] = llvm::TrackingMDNodeRef(F);
  return F;
}

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

llvm::DIScope *IRGenDebugInfo::getEntryPointFn() {
  // Lazily create EntryPointFn.
  if (!EntryPointFn) {
    EntryPointFn = DBuilder.createReplaceableCompositeType(
      llvm::dwarf::DW_TAG_subroutine_type, SWIFT_ENTRY_POINT_FUNCTION,
        MainFile, MainFile, 0);
  }
  return EntryPointFn;
}


llvm::DIScope *IRGenDebugInfo::getOrCreateContext(DeclContext *DC) {
  if (!DC)
    return TheCU;

  if (isa<FuncDecl>(DC))
    if (auto *Decl = IGM.getSILModule().lookUpFunction(
          SILDeclRef(cast<AbstractFunctionDecl>(DC), SILDeclRef::Kind::Func)))
      return getOrCreateScope(Decl->getDebugScope());

  switch (DC->getContextKind()) {
  // The interesting cases are already handled above.
  case DeclContextKind::AbstractFunctionDecl:
  case DeclContextKind::AbstractClosureExpr:

  // We don't model these in DWARF.
  case DeclContextKind::SerializedLocal:
  case DeclContextKind::Initializer:
  case DeclContextKind::ExtensionDecl:
  case DeclContextKind::SubscriptDecl:
  case DeclContextKind::TopLevelCodeDecl:
    return getOrCreateContext(DC->getParent());

  case DeclContextKind::Module:
    return getOrCreateModule({ModuleDecl::AccessPathTy(), cast<ModuleDecl>(DC)});
  case DeclContextKind::FileUnit:
    // A module may contain multiple files.
    return getOrCreateContext(DC->getParent());
  case DeclContextKind::GenericTypeDecl: {
    auto *NTD = cast<NominalTypeDecl>(DC);
    auto *Ty = NTD->getDeclaredType().getPointer();
    if (auto *DITy = getTypeOrNull(Ty))
      return DITy;

    // Create a Forward-declared type.
    auto Loc = getDebugLoc(SM, NTD);
    auto File = getOrCreateFile(Loc.Filename);
    auto Line = Loc.Line;
    auto FwdDecl = DBuilder.createReplaceableCompositeType(
        llvm::dwarf::DW_TAG_structure_type, NTD->getName().str(),
        getOrCreateContext(DC->getParent()), File, Line,
        llvm::dwarf::DW_LANG_Swift, 0, 0);
    ReplaceMap.emplace_back(
        std::piecewise_construct, std::make_tuple(Ty),
        std::make_tuple(static_cast<llvm::Metadata *>(FwdDecl)));
    return FwdDecl;
  }
  }
  return TheCU;
}

void IRGenDebugInfo::createParameterType(
    llvm::SmallVectorImpl<llvm::Metadata *> &Parameters, SILType type,
      DeclContext *DeclCtx, GenericEnvironment *GE) {
  auto RealType = type.getSwiftRValueType();
  if (type.isAddress())
    RealType = CanInOutType::get(RealType);
  auto DbgTy = DebugTypeInfo::getFromTypeInfo(DeclCtx, GE, RealType,
                                              IGM.getTypeInfo(type));
  Parameters.push_back(getOrCreateType(DbgTy));
}

// This is different from SILFunctionType::getAllResultsType() in some subtle
// ways.
static SILType getResultTypeForDebugInfo(CanSILFunctionType fnTy) {
  if (fnTy->getNumResults() == 1) {
    return fnTy->getResults()[0].getSILStorageType();
  } else if (!fnTy->getNumIndirectFormalResults()) {
    return fnTy->getDirectFormalResultsType();
  } else {
    SmallVector<TupleTypeElt, 4> eltTys;
    for (auto &result : fnTy->getResults()) {
      eltTys.push_back(result.getType());
    }
    return SILType::getPrimitiveAddressType(
      CanType(TupleType::get(eltTys, fnTy->getASTContext())));
  }
}

llvm::DITypeRefArray
IRGenDebugInfo::createParameterTypes(SILType SILTy, DeclContext *DeclCtx,
                                     GenericEnvironment *GE) {
  if (!SILTy)
    return nullptr;
  return createParameterTypes(SILTy.castTo<SILFunctionType>(), DeclCtx, GE);
}

llvm::DITypeRefArray IRGenDebugInfo::createParameterTypes(
    CanSILFunctionType FnTy, DeclContext *DeclCtx, GenericEnvironment *GE) {
  SmallVector<llvm::Metadata *, 16> Parameters;

  GenericContextScope scope(IGM, FnTy->getGenericSignature());

  // The function return type is the first element in the list.
  createParameterType(Parameters, getResultTypeForDebugInfo(FnTy), DeclCtx, GE);

  // Actually, the input type is either a single type or a tuple
  // type. We currently represent a function with one n-tuple argument
  // as an n-ary function.
  for (auto Param : FnTy->getParameters())
    createParameterType(Parameters, IGM.silConv.getSILType(Param), DeclCtx, GE);

  return DBuilder.getOrCreateTypeArray(Parameters);
}

/// FIXME: replace this condition with something more sane.
static bool isAllocatingConstructor(SILFunctionTypeRepresentation Rep,
                                    DeclContext *DeclCtx) {
  return Rep != SILFunctionTypeRepresentation::Method
          && DeclCtx && isa<ConstructorDecl>(DeclCtx);
}

llvm::DISubprogram *
IRGenDebugInfo::emitFunction(const SILDebugScope *DS, llvm::Function *Fn,
                             SILFunctionTypeRepresentation Rep, SILType SILTy,
                             DeclContext *DeclCtx, GenericEnvironment *GE) {
  auto Cached = ScopeCache.find(DS);
  if (Cached != ScopeCache.end()) {
    auto SP = cast<llvm::DISubprogram>(Cached->second);
    // If we created the DISubprogram for a forward declaration,
    // attach it to the function now.
    if (!Fn->getSubprogram() && !Fn->isDeclaration())
      Fn->setSubprogram(SP);
    return SP;
  }

  // Some IRGen-generated helper functions don't have a corresponding
  // SIL function, hence the dyn_cast.
  SILFunction *SILFn = DS ? DS->Parent.dyn_cast<SILFunction *>() : nullptr;
  StringRef LinkageName;
  if (Fn)
    LinkageName = Fn->getName();
  else if (DS)
    LinkageName = SILFn->getName();
  else
    llvm_unreachable("function has no mangled name");

  StringRef Name;
  if (DS) {
    if (DS->Loc.isSILFile())
      Name = SILFn->getName();
    else
      Name = getName(DS->Loc);
  }

  SILLocation::DebugLoc L;
  unsigned ScopeLine = 0; /// The source line used for the function prologue.
  // Bare functions and thunks should not have any line numbers. This
  // is especially important for shared functions like reabstraction
  // thunk helpers, where DS->Loc is an arbitrary location of whichever use
  // was emitted first.
  if (DS && (!SILFn || (!SILFn->isBare() && !SILFn->isThunk()))) {
    L = DS->Loc.decodeDebugLoc(SM);
    ScopeLine = L.Line;
    if (!DS->Loc.isDebugInfoLoc())
      L = SILLocation::decode(DS->Loc.getSourceLoc(), SM);
  }

  auto Line = L.Line;
  auto File = getOrCreateFile(L.Filename);
  llvm::DIScope *Scope = MainModule;
  if (SILFn && SILFn->getDeclContext())
    Scope = getOrCreateContext(SILFn->getDeclContext()->getParent());

  // We know that main always comes from MainFile.
  if (LinkageName == SWIFT_ENTRY_POINT_FUNCTION) {
    if (L.Filename.empty())
      File = MainFile;
    Line = 1;
    Name = LinkageName;
  }

  CanSILFunctionType FnTy = getFunctionType(SILTy);
  auto Params = Opts.DebugInfoKind > IRGenDebugInfoKind::LineTables
                    ? createParameterTypes(SILTy, DeclCtx, GE)
                    : nullptr;
  llvm::DISubroutineType *DIFnTy = DBuilder.createSubroutineType(Params);
  llvm::DITemplateParameterArray TemplateParameters = nullptr;
  llvm::DISubprogram *Decl = nullptr;

  // Various flags
  bool IsLocalToUnit = Fn ? Fn->hasInternalLinkage() : true;
  bool IsDefinition = true;
  bool IsOptimized = Opts.Optimize;
  llvm::DINode::DIFlags Flags = llvm::DINode::FlagZero;

  // Mark everything that is not visible from the source code (i.e.,
  // does not have a Swift name) as artificial, so the debugger can
  // ignore it. Explicit closures are exempt from this rule. We also
  // make an exception for toplevel code, which, although it does not
  // have a Swift name, does appear prominently in the source code.
  if ((Name.empty() && LinkageName != SWIFT_ENTRY_POINT_FUNCTION &&
       !isExplicitClosure(SILFn)) ||
      // ObjC thunks should also not show up in the linetable, because we
      // never want to set a breakpoint there.
      (Rep == SILFunctionTypeRepresentation::ObjCMethod) ||
      isAllocatingConstructor(Rep, DeclCtx)) {
    Flags |= llvm::DINode::FlagArtificial;
    ScopeLine = 0;
  }

  if (FnTy && FnTy->getRepresentation()
        == SILFunctionType::Representation::Block)
    Flags |= llvm::DINode::FlagAppleBlock;

  llvm::DISubprogram *SP = DBuilder.createFunction(
      Scope, Name, LinkageName, File, Line, DIFnTy, IsLocalToUnit, IsDefinition,
      ScopeLine, Flags, IsOptimized, TemplateParameters, Decl);

  if (Fn && !Fn->isDeclaration())
    Fn->setSubprogram(SP);

  // RAUW the entry point function forward declaration with the real thing.
  if (LinkageName == SWIFT_ENTRY_POINT_FUNCTION) {
    if (EntryPointFn) {
      assert(EntryPointFn->isTemporary() &&
             "more than one entry point function");
      EntryPointFn->replaceAllUsesWith(SP);
      llvm::MDNode::deleteTemporary(EntryPointFn);
    }
    EntryPointFn = SP;
  }

  if (!DS)
    return nullptr;

  ScopeCache[DS] = llvm::TrackingMDNodeRef(SP);
  return SP;
}

void IRGenDebugInfo::emitImport(ImportDecl *D) {
  if (Opts.DebugInfoKind <= IRGenDebugInfoKind::LineTables)
    return;

  swift::ModuleDecl *M = IGM.Context.getModule(D->getModulePath());
  if (!M &&
      D->getModulePath()[0].first == IGM.Context.TheBuiltinModule->getName())
    M = IGM.Context.TheBuiltinModule;
  if (!M) {
    assert(M && "Could not find module for import decl.");
    return;
  }
  auto DIMod = getOrCreateModule({D->getModulePath(), M});
  auto L = getDebugLoc(SM, D);
  DBuilder.createImportedModule(getOrCreateFile(L.Filename), DIMod, L.Line);
}

llvm::DIModule *
IRGenDebugInfo::getOrCreateModule(ModuleDecl::ImportedModule M) {
  StringRef Path = getFilenameFromDC(M.second);
  if (M.first.empty()) {
    StringRef Name = M.second->getName().str();
    return getOrCreateModule(Name, TheCU, Name, Path);
  }

  unsigned I = 0;
  SmallString<128> AccessPath;
  llvm::DIScope *Scope = TheCU;
  llvm::raw_svector_ostream OS(AccessPath);
  for (auto elt : M.first) {
    auto Component = elt.first.str();
    if (++I > 1)
      OS << '.';
    OS << Component;
    Scope = getOrCreateModule(AccessPath, Scope, Component, Path);
  }
  return cast<llvm::DIModule>(Scope);
}

llvm::DIModule *IRGenDebugInfo::getOrCreateModule(StringRef Key,
                                                  llvm::DIScope *Parent,
                                                  StringRef Name,
                                                  StringRef IncludePath) {
  // Look in the cache first.
  auto Val = DIModuleCache.find(Key);
  if (Val != DIModuleCache.end())
    return cast<llvm::DIModule>(Val->second);

  StringRef ConfigMacros;
  StringRef Sysroot = IGM.Context.SearchPathOpts.SDKPath;
  auto M =
      DBuilder.createModule(Parent, Name, ConfigMacros, IncludePath, Sysroot);
  DIModuleCache.insert({Key, llvm::TrackingMDNodeRef(M)});
  return M;
}

llvm::DISubprogram *IRGenDebugInfo::emitFunction(SILFunction &SILFn,
                                                 llvm::Function *Fn) {
  auto *DS = SILFn.getDebugScope();
  assert(DS && "SIL function has no debug scope");
  (void) DS;
  return emitFunction(SILFn.getDebugScope(), Fn, SILFn.getRepresentation(),
                      SILFn.getLoweredType(), SILFn.getDeclContext(),
                      SILFn.getGenericEnvironment());
}

void IRGenDebugInfo::emitArtificialFunction(IRBuilder &Builder,
                                            llvm::Function *Fn, SILType SILTy) {
  RegularLocation ALoc = RegularLocation::getAutoGeneratedLocation();
  const SILDebugScope *Scope = new (IGM.getSILModule()) SILDebugScope(ALoc);
  emitFunction(Scope, Fn, SILFunctionTypeRepresentation::Thin, SILTy);
  setCurrentLoc(Builder, Scope);
}

TypeAliasDecl *IRGenDebugInfo::getMetadataType() {
  if (!MetadataTypeDecl) {
    MetadataTypeDecl = new (IGM.Context) TypeAliasDecl(
        SourceLoc(), SourceLoc(),
        IGM.Context.getIdentifier("$swift.type"), SourceLoc(),
        /*genericparams*/nullptr, IGM.Context.TheBuiltinModule);
    MetadataTypeDecl->setUnderlyingType(IGM.Context.TheRawPointerType);
  }
  return MetadataTypeDecl;
}

void IRGenDebugInfo::emitTypeMetadata(IRGenFunction &IGF,
                                      llvm::Value *Metadata,
                                      StringRef Name) {
  if (Opts.DebugInfoKind <= IRGenDebugInfoKind::LineTables)
    return;

  auto TName = BumpAllocatedString(("$swift.type." + Name).str());
  auto DbgTy = DebugTypeInfo::getMetadata(
      getMetadataType()->getDeclaredInterfaceType().getPointer(),
      Metadata->getType(), Size(CI.getTargetInfo().getPointerWidth(0)),
      Alignment(CI.getTargetInfo().getPointerAlign(0)));
  emitVariableDeclaration(IGF.Builder, Metadata, DbgTy, IGF.getDebugScope(),
                          nullptr, TName, 0,
                          // swift.type is already a pointer type,
                          // having a shadow copy doesn't add another
                          // layer of indirection.
                          DirectValue, ArtificialValue);
}

/// Return the DIFile that is the ancestor of Scope.
llvm::DIFile *IRGenDebugInfo::getFile(llvm::DIScope *Scope) {
  while (!isa<llvm::DIFile>(Scope)) {
    switch (Scope->getTag()) {
    case llvm::dwarf::DW_TAG_lexical_block:
      Scope = cast<llvm::DILexicalBlock>(Scope)->getScope();
      break;
    case llvm::dwarf::DW_TAG_subprogram:
      Scope = cast<llvm::DISubprogram>(Scope)->getFile();
      break;
    default:
      return MainFile;
    }
    if (Scope)
      return MainFile;
  }
  return cast<llvm::DIFile>(Scope);
}

static Size
getStorageSize(const llvm::DataLayout &DL, ArrayRef<llvm::Value *> Storage) {
  unsigned size = 0;
  for (llvm::Value *Piece : Storage)
    size += DL.getTypeSizeInBits(Piece->getType());
  return Size(size);
}

void IRGenDebugInfo::emitVariableDeclaration(
    IRBuilder &Builder, ArrayRef<llvm::Value *> Storage, DebugTypeInfo DbgTy,
    const SILDebugScope *DS, ValueDecl *VarDecl, StringRef Name, unsigned ArgNo,
    IndirectionKind Indirection, ArtificialKind Artificial) {
  // Self is always an artificial argument.
  if (ArgNo > 0 && Name == IGM.Context.Id_self.str())
    Artificial = ArtificialValue;

  // FIXME: Make this an assertion.
  // assert(DS && "variable has no scope");
  if (!DS)
    return;

  if (Opts.DebugInfoKind <= IRGenDebugInfoKind::LineTables)
    return;

  if (!DbgTy.size)
    DbgTy.size = getStorageSize(IGM.DataLayout, Storage);

  auto *Scope = dyn_cast<llvm::DILocalScope>(getOrCreateScope(DS));
  assert(Scope && "variable has no local scope");
  auto Loc = getDebugLoc(SM, VarDecl);

  // FIXME: this should be the scope of the type's declaration.
  // If this is an argument, attach it to the current function scope.
  if (ArgNo > 0) {
    while (isa<llvm::DILexicalBlock>(Scope))
      Scope = cast<llvm::DILexicalBlock>(Scope)->getScope();
  }
  assert(Scope && isa<llvm::DIScope>(Scope) && "variable has no scope");
  llvm::DIFile *Unit = getFile(Scope);
  llvm::DIType *DITy = getOrCreateType(DbgTy);
  assert(DITy && "could not determine debug type of variable");

  unsigned Line = Loc.Line;
  llvm::DINode::DIFlags Flags = llvm::DINode::FlagZero;
  if (Artificial || DITy->isArtificial() || DITy == InternalType)
    Flags |= llvm::DINode::FlagArtificial;

  // This could be Opts.Optimize if we would also unique DIVariables here.
  bool Optimized = false;
  // Create the descriptor for the variable.
  llvm::DILocalVariable *Var =
      (ArgNo > 0)
          ? DBuilder.createParameterVariable(Scope, Name, ArgNo, Unit, Line,
                                             DITy, Optimized, Flags)
          : DBuilder.createAutoVariable(Scope, Name, Unit, Line, DITy,
                                        Optimized, Flags);

  // Running variables for the current/previous piece.
  bool IsPiece = Storage.size() > 1;
  uint64_t SizeOfByte = CI.getTargetInfo().getCharWidth();
  unsigned AlignInBits = SizeOfByte;
  unsigned OffsetInBits = 0;
  unsigned SizeInBits = 0;

  for (llvm::Value *Piece : Storage) {
    SmallVector<uint64_t, 3> Operands;
    if (Indirection)
      Operands.push_back(llvm::dwarf::DW_OP_deref);

    // There are variables without storage, such as "struct { func foo() {} }".
    // Emit them as constant 0.
    if (isa<llvm::UndefValue>(Piece))
      Piece = llvm::ConstantInt::get(IGM.Int64Ty, 0);

    if (IsPiece) {
      // Advance the offset and align it for the next piece.
      OffsetInBits += llvm::alignTo(SizeInBits, AlignInBits);
      SizeInBits = IGM.DataLayout.getTypeSizeInBits(Piece->getType());
      AlignInBits = IGM.DataLayout.getABITypeAlignment(Piece->getType());
      if (!AlignInBits)
        AlignInBits = SizeOfByte;

      // Sanity checks.
      assert(SizeInBits && "zero-sized piece");
      assert(SizeInBits < getSizeInBits(Var) && "piece covers entire var");
      assert(OffsetInBits+SizeInBits <= getSizeInBits(Var) && "pars > totum");

      // Add the piece DWARF expression.
      Operands.push_back(llvm::dwarf::DW_OP_LLVM_fragment);
      Operands.push_back(OffsetInBits);
      Operands.push_back(SizeInBits);
    }
    emitDbgIntrinsic(Builder, Piece, Var, DBuilder.createExpression(Operands),
                     Line, Loc.Column, Scope, DS);
  }

  // Emit locationless intrinsic for variables that were optimized away.
  if (Storage.size() == 0)
    emitDbgIntrinsic(Builder, llvm::ConstantInt::get(IGM.Int64Ty, 0), Var,
                     DBuilder.createExpression(), Line, Loc.Column, Scope, DS);
}

void IRGenDebugInfo::emitDbgIntrinsic(IRBuilder &Builder, llvm::Value *Storage,
                                      llvm::DILocalVariable *Var,
                                      llvm::DIExpression *Expr, unsigned Line,
                                      unsigned Col, llvm::DILocalScope *Scope,
                                      const SILDebugScope *DS) {
  // Set the location/scope of the intrinsic.
  auto *InlinedAt = createInlinedAt(DS);
  auto DL = llvm::DebugLoc::get(Line, Col, Scope, InlinedAt);
  auto *BB = Builder.GetInsertBlock();

  // An alloca may only be described by exactly one dbg.declare.
  if (isa<llvm::AllocaInst>(Storage) && llvm::FindAllocaDbgDeclare(Storage))
    return;
  
  // A dbg.declare is only meaningful if there is a single alloca for
  // the variable that is live throughout the function. With SIL
  // optimizations this is not guaranteed and a variable can end up in
  // two allocas (for example, one function inlined twice).
  if (isa<llvm::AllocaInst>(Storage)) {
    DBuilder.insertDeclare(Storage, Var, Expr, DL, BB);
    return;
  }

  // If the storage is an instruction, insert the dbg.value directly after it.
  if (auto *I = dyn_cast<llvm::Instruction>(Storage)) {
    auto InsPt = std::next(I->getIterator());
    auto E = I->getParent()->end();
    while (InsPt != E && isa<llvm::PHINode>(&*InsPt))
        ++InsPt;
    if (InsPt != E) {
      DBuilder.insertDbgValueIntrinsic(Storage, 0, Var, Expr, DL, &*InsPt);
      return;
    }
  }

  // Otherwise just insert it at the current insertion point.
  DBuilder.insertDbgValueIntrinsic(Storage, 0, Var, Expr, DL, BB);
}

void IRGenDebugInfo::emitGlobalVariableDeclaration(
    llvm::GlobalVariable *Var, StringRef Name, StringRef LinkageName,
    DebugTypeInfo DbgTy, bool IsLocalToUnit, Optional<SILLocation> Loc) {
  if (Opts.DebugInfoKind <= IRGenDebugInfoKind::LineTables)
    return;

  llvm::DIType *Ty = getOrCreateType(DbgTy);
  if (Ty->isArtificial() || Ty == InternalType || !Loc)
    // FIXME: Really these should be marked as artificial, but LLVM
    // currently has no support for flags to be put on global
    // variables. In the mean time, elide these variables, they
    // would confuse both the user and LLDB.
    return;

  auto L = getStartLocation(Loc, SM);
  auto File = getOrCreateFile(L.Filename);

  // Emit it as global variable of the current module.
  auto *Expr = Var ? nullptr : DBuilder.createConstantValueExpression(0);
  auto *GV = DBuilder.createGlobalVariableExpression(
      MainModule, Name, LinkageName, File, L.Line, Ty, IsLocalToUnit, Expr);
  if (Var)
    Var->addDebugInfo(GV);
}

StringRef IRGenDebugInfo::getMangledName(DebugTypeInfo DbgTy) {
  if (MetadataTypeDecl && DbgTy.getDecl() == MetadataTypeDecl)
    return BumpAllocatedString(DbgTy.getDecl()->getName().str());
  
  Mangle::ASTMangler Mangler;
  std::string Name = Mangler.mangleTypeForDebugger(
      DbgTy.getType(), DbgTy.getDeclContext(), DbgTy.getGenericEnvironment());
  return BumpAllocatedString(Name);
}

llvm::DIDerivedType *
IRGenDebugInfo::createMemberType(DebugTypeInfo DbgTy, StringRef Name,
                                 unsigned &OffsetInBits, llvm::DIScope *Scope,
                                 llvm::DIFile *File,
                                 llvm::DINode::DIFlags Flags) {
  unsigned SizeOfByte = CI.getTargetInfo().getCharWidth();
  auto *Ty = getOrCreateType(DbgTy);
  auto *DITy = DBuilder.createMemberType(
      Scope, Name, File, 0, SizeOfByte * DbgTy.size.getValue(),
      SizeOfByte * DbgTy.align.getValue(), OffsetInBits, Flags, Ty);
  OffsetInBits += getSizeInBits(Ty);
  OffsetInBits =
      llvm::alignTo(OffsetInBits, SizeOfByte * DbgTy.align.getValue());
  return DITy;
}

llvm::DINodeArray IRGenDebugInfo::getTupleElements(
    TupleType *TupleTy, llvm::DIScope *Scope, llvm::DIFile *File,
    llvm::DINode::DIFlags Flags, DeclContext *DeclContext,
    GenericEnvironment *GE, unsigned &SizeInBits) {
  SmallVector<llvm::Metadata *, 16> Elements;
  unsigned OffsetInBits = 0;
  auto genericSig = IGM.getSILTypes().getCurGenericContext();
  for (auto ElemTy : TupleTy->getElementTypes()) {
    auto &elemTI = IGM.getTypeInfoForUnlowered(
        AbstractionPattern(genericSig, ElemTy->getCanonicalType()), ElemTy);
    auto DbgTy =
        DebugTypeInfo::getFromTypeInfo(DeclContext, GE, ElemTy, elemTI);
    Elements.push_back(
        createMemberType(DbgTy, StringRef(), OffsetInBits, Scope, File, Flags));
  }
  SizeInBits = OffsetInBits;
  return DBuilder.getOrCreateArray(Elements);
}

llvm::DINodeArray IRGenDebugInfo::getStructMembers(
    NominalTypeDecl *D, Type BaseTy, llvm::DIScope *Scope, llvm::DIFile *File,
    llvm::DINode::DIFlags Flags, unsigned &SizeInBits) {
  SmallVector<llvm::Metadata *, 16> Elements;
  unsigned OffsetInBits = 0;
  for (VarDecl *VD : D->getStoredProperties()) {
    auto memberTy =
        BaseTy->getTypeOfMember(IGM.getSwiftModule(), VD, nullptr);

    auto DbgTy = DebugTypeInfo::getFromTypeInfo(
        VD->getDeclContext(),
        VD->getDeclContext()->getGenericEnvironmentOfContext(),
        VD->getInterfaceType(),
        IGM.getTypeInfoForUnlowered(IGM.getSILTypes().getAbstractionPattern(VD),
                                    memberTy));
    Elements.push_back(createMemberType(DbgTy, VD->getName().str(),
                                        OffsetInBits, Scope, File, Flags));
  }
  if (OffsetInBits > SizeInBits)
    SizeInBits = OffsetInBits;
  return DBuilder.getOrCreateArray(Elements);
}

llvm::DICompositeType *IRGenDebugInfo::createStructType(
    DebugTypeInfo DbgTy, NominalTypeDecl *Decl, Type BaseTy,
    llvm::DIScope *Scope, llvm::DIFile *File, unsigned Line,
    unsigned SizeInBits, unsigned AlignInBits, llvm::DINode::DIFlags Flags,
    llvm::DIType *DerivedFrom, unsigned RuntimeLang, StringRef UniqueID) {
  StringRef Name = Decl->getName().str();

  // Forward declare this first because types may be recursive.
  auto FwdDecl = llvm::TempDIType(
    DBuilder.createReplaceableCompositeType(
      llvm::dwarf::DW_TAG_structure_type, Name, Scope, File, Line,
        llvm::dwarf::DW_LANG_Swift, SizeInBits, AlignInBits, Flags, UniqueID));

#ifndef NDEBUG
  if (UniqueID.empty())
    assert(!Name.empty() && "no mangled name and no human readable name given");
  else
    assert((UniqueID.startswith("_T") ||
              UniqueID.startswith(MANGLING_PREFIX_STR)) &&
           "UID is not a mangled name");
#endif

  auto TH = llvm::TrackingMDNodeRef(FwdDecl.get());
  DITypeCache[DbgTy.getType()] = TH;
  auto Members = getStructMembers(Decl, BaseTy, Scope, File, Flags, SizeInBits);
  auto DITy = DBuilder.createStructType(
      Scope, Name, File, Line, SizeInBits, AlignInBits, Flags, DerivedFrom,
      Members, RuntimeLang, nullptr, UniqueID);
  DBuilder.replaceTemporary(std::move(FwdDecl), DITy);
  return DITy;
}

llvm::DINodeArray IRGenDebugInfo::getEnumElements(DebugTypeInfo DbgTy,
                                                  EnumDecl *ED,
                                                  llvm::DIScope *Scope,
                                                  llvm::DIFile *File,
                                                  llvm::DINode::DIFlags Flags) {
  SmallVector<llvm::Metadata *, 16> Elements;

  for (auto *ElemDecl : ED->getAllElements()) {
    // FIXME <rdar://problem/14845818> Support enums.
    // Swift Enums can be both like DWARF enums and discriminated unions.
    DebugTypeInfo ElemDbgTy;
    if (ED->hasRawType())
      // An enum with a raw type (enum E : Int {}), similar to a
      // DWARF enum.
      //
      // The storage occupied by the enum may be smaller than the
      // one of the raw type as long as it is large enough to hold
      // all enum values. Use the raw type for the debug type, but
      // the storage size from the enum.
      ElemDbgTy =
          DebugTypeInfo(ED, DbgTy.getGenericEnvironment(), ED->getRawType(),
                        DbgTy.StorageType, DbgTy.size, DbgTy.align);
    else if (auto ArgTy = ElemDecl->getArgumentInterfaceType()) {
      // A discriminated union. This should really be described as a
      // DW_TAG_variant_type. For now only describing the data.
      ArgTy = ElemDecl->getParentEnum()->mapTypeIntoContext(ArgTy);
      auto &TI = IGM.getTypeInfoForUnlowered(ArgTy);
      ElemDbgTy = DebugTypeInfo::getFromTypeInfo(
          ElemDecl->getDeclContext(),
          ElemDecl->getDeclContext()->getGenericEnvironmentOfContext(), ArgTy,
          TI);
    } else {
      // Discriminated union case without argument. Fallback to Int
      // as the element type; there is no storage here.
      Type IntTy = IGM.Context.getIntDecl()->getDeclaredType();
      ElemDbgTy = DebugTypeInfo(
          ElemDecl->getDeclContext(),
          ElemDecl->getDeclContext()->getGenericEnvironmentOfContext(), IntTy,
          DbgTy.StorageType, Size(0), Alignment(1));
    }
    unsigned Offset = 0;
    auto MTy = createMemberType(ElemDbgTy, ElemDecl->getName().str(), Offset,
                                Scope, File, Flags);
    Elements.push_back(MTy);
  }
  return DBuilder.getOrCreateArray(Elements);
}

llvm::DICompositeType *IRGenDebugInfo::createEnumType(
    DebugTypeInfo DbgTy, EnumDecl *Decl, StringRef MangledName,
    llvm::DIScope *Scope, llvm::DIFile *File, unsigned Line,
    llvm::DINode::DIFlags Flags) {
  unsigned SizeOfByte = CI.getTargetInfo().getCharWidth();
  unsigned SizeInBits = DbgTy.size.getValue() * SizeOfByte;
  unsigned AlignInBits = DbgTy.align.getValue() * SizeOfByte;

  // FIXME: Is DW_TAG_union_type the right thing here?
  // Consider using a DW_TAG_variant_type instead.
  auto FwdDecl = llvm::TempDIType(
    DBuilder.createReplaceableCompositeType(
      llvm::dwarf::DW_TAG_union_type, MangledName, Scope, File, Line,
        llvm::dwarf::DW_LANG_Swift, SizeInBits, AlignInBits, Flags,
        MangledName));

  auto TH = llvm::TrackingMDNodeRef(FwdDecl.get());
  DITypeCache[DbgTy.getType()] = TH;

  auto DITy = DBuilder.createUnionType(
      Scope, Decl->getName().str(), File, Line, SizeInBits, AlignInBits, Flags,
      getEnumElements(DbgTy, Decl, Scope, File, Flags),
      llvm::dwarf::DW_LANG_Swift, MangledName);

  DBuilder.replaceTemporary(std::move(FwdDecl), DITy);
  return DITy;
}

llvm::DIType *IRGenDebugInfo::getOrCreateDesugaredType(Type Ty,
                                                       DebugTypeInfo DbgTy) {
  DebugTypeInfo BlandDbgTy(DbgTy.getDeclContext(),
                           DbgTy.getGenericEnvironment(), Ty, DbgTy.StorageType,
                           DbgTy.size, DbgTy.align);
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
  assert(BitWidth > IGM.DataLayout.getLargestLegalIntTypeSizeInBits());
  return BitWidth;
}

llvm::DIType *IRGenDebugInfo::createPointerSizedStruct(
    llvm::DIScope *Scope, StringRef Name, llvm::DIFile *File, unsigned Line,
    llvm::DINode::DIFlags Flags, StringRef MangledName) {
  if (Opts.DebugInfoKind > IRGenDebugInfoKind::ASTTypes) {
    auto FwdDecl = DBuilder.createForwardDecl(
        llvm::dwarf::DW_TAG_structure_type, Name, Scope, File, Line,
        llvm::dwarf::DW_LANG_Swift, 0, 0);
    return createPointerSizedStruct(Scope, Name, FwdDecl, File, Line, Flags,
                                    MangledName);
  } else {
    unsigned SizeInBits = CI.getTargetInfo().getPointerWidth(0);
    unsigned AlignInBits = CI.getTargetInfo().getPointerAlign(0);
    return createOpaqueStruct(Scope, Name, File, Line, SizeInBits, AlignInBits,
                              Flags, MangledName);
  }
}

llvm::DIType *IRGenDebugInfo::createPointerSizedStruct(
    llvm::DIScope *Scope, StringRef Name, llvm::DIType *PointeeTy,
    llvm::DIFile *File, unsigned Line, llvm::DINode::DIFlags Flags,
    StringRef MangledName) {
  unsigned PtrSize = CI.getTargetInfo().getPointerWidth(0);
  unsigned PtrAlign = CI.getTargetInfo().getPointerAlign(0);
  auto PtrTy = DBuilder.createPointerType(PointeeTy, PtrSize, PtrAlign);
  llvm::Metadata *Elements[] = {
    DBuilder.createMemberType(Scope, "ptr", File, 0,
                              PtrSize, PtrAlign, 0, Flags, PtrTy)
  };
  return DBuilder.createStructType(
      Scope, Name, File, Line, PtrSize, PtrAlign, Flags,
      /* DerivedFrom */ nullptr, DBuilder.getOrCreateArray(Elements),
      llvm::dwarf::DW_LANG_Swift, nullptr, MangledName);
}

llvm::DIType *IRGenDebugInfo::createDoublePointerSizedStruct(
    llvm::DIScope *Scope, StringRef Name, llvm::DIType *PointeeTy,
    llvm::DIFile *File, unsigned Line, llvm::DINode::DIFlags Flags,
    StringRef MangledName) {
  unsigned PtrSize = CI.getTargetInfo().getPointerWidth(0);
  unsigned PtrAlign = CI.getTargetInfo().getPointerAlign(0);
  llvm::Metadata *Elements[] = {
      DBuilder.createMemberType(
          Scope, "ptr", File, 0, PtrSize, PtrAlign, 0, Flags,
          DBuilder.createPointerType(PointeeTy, PtrSize, PtrAlign)),
      DBuilder.createMemberType(
          Scope, "_", File, 0, PtrSize, PtrAlign, 0, Flags,
          DBuilder.createPointerType(nullptr, PtrSize, PtrAlign))};
  return DBuilder.createStructType(
      Scope, Name, File, Line, 2*PtrSize, PtrAlign, Flags,
      /* DerivedFrom */ nullptr, DBuilder.getOrCreateArray(Elements),
      llvm::dwarf::DW_LANG_Swift, nullptr, MangledName);
}

llvm::DIType *
IRGenDebugInfo::createFunctionPointer(DebugTypeInfo DbgTy, llvm::DIScope *Scope,
                                      unsigned SizeInBits, unsigned AlignInBits,
                                      llvm::DINode::DIFlags Flags,
                                      StringRef MangledName) {
  auto FwdDecl = llvm::TempDINode(DBuilder.createReplaceableCompositeType(
      llvm::dwarf::DW_TAG_subroutine_type, MangledName, Scope, MainFile, 0,
      llvm::dwarf::DW_LANG_Swift, SizeInBits, AlignInBits, Flags, MangledName));

  auto TH = llvm::TrackingMDNodeRef(FwdDecl.get());
  DITypeCache[DbgTy.getType()] = TH;

  CanSILFunctionType FunTy;
  TypeBase *BaseTy = DbgTy.getType();
  if (auto *SILFnTy = dyn_cast<SILFunctionType>(BaseTy))
    FunTy = CanSILFunctionType(SILFnTy);
  // FIXME: Handling of generic parameters in SIL type lowering is in flux.
  // DebugInfo doesn't appear to care about the generic context, so just
  // throw it away before lowering.
  else if (isa<GenericFunctionType>(BaseTy)) {
    auto *fTy = cast<AnyFunctionType>(BaseTy);
    auto *nongenericTy =
        FunctionType::get(fTy->getInput(), fTy->getResult(), fTy->getExtInfo());

    FunTy = IGM.getLoweredType(nongenericTy).castTo<SILFunctionType>();
  } else
    FunTy = IGM.getLoweredType(BaseTy).castTo<SILFunctionType>();
  auto Params = createParameterTypes(FunTy, DbgTy.getDeclContext(),
                                     DbgTy.getGenericEnvironment());

  auto FnTy = DBuilder.createSubroutineType(Params, Flags);
  llvm::DIType *DITy;
  if (FunTy->getRepresentation() == SILFunctionType::Representation::Thick) {
    if (SizeInBits == 2 * CI.getTargetInfo().getPointerWidth(0))
      // This is a FunctionPairTy: { i8*, %swift.refcounted* }.
      DITy = createDoublePointerSizedStruct(Scope, MangledName, FnTy, MainFile,
                                            0, Flags, MangledName);
    else
      // This is a generic function as noted above.
      DITy = createOpaqueStruct(Scope, MangledName, MainFile, 0, SizeInBits,
                                AlignInBits, Flags, MangledName);
  } else {
    assert(SizeInBits == CI.getTargetInfo().getPointerWidth(0));
    DITy = createPointerSizedStruct(Scope, MangledName, FnTy, MainFile, 0,
                                    Flags, MangledName);
  }
  DBuilder.replaceTemporary(std::move(FwdDecl), DITy);
  return DITy;
}

llvm::DIType *IRGenDebugInfo::createTuple(DebugTypeInfo DbgTy,
                                          llvm::DIScope *Scope,
                                          unsigned SizeInBits,
                                          unsigned AlignInBits,
                                          llvm::DINode::DIFlags Flags,
                                          StringRef MangledName) {
  TypeBase *BaseTy = DbgTy.getType();
  auto *TupleTy = BaseTy->castTo<TupleType>();
  auto FwdDecl = llvm::TempDINode(DBuilder.createReplaceableCompositeType(
      llvm::dwarf::DW_TAG_structure_type, MangledName, Scope, MainFile, 0,
      llvm::dwarf::DW_LANG_Swift, SizeInBits, AlignInBits, Flags, MangledName));

  DITypeCache[DbgTy.getType()] = llvm::TrackingMDNodeRef(FwdDecl.get());

  unsigned RealSize;
  auto Elements =
      getTupleElements(TupleTy, Scope, MainFile, Flags, DbgTy.getDeclContext(),
                       DbgTy.getGenericEnvironment(), RealSize);
  // FIXME: Handle %swift.opaque members and make this into an assertion.
  if (!RealSize)
    RealSize = SizeInBits;

  auto DITy = DBuilder.createStructType(
      Scope, MangledName, MainFile, 0, RealSize, AlignInBits, Flags,
      nullptr, // DerivedFrom
      Elements, llvm::dwarf::DW_LANG_Swift, nullptr, MangledName);

  DBuilder.replaceTemporary(std::move(FwdDecl), DITy);
  return DITy;
}

llvm::DIType *
IRGenDebugInfo::createOpaqueStruct(llvm::DIScope *Scope, StringRef Name,
                                   llvm::DIFile *File, unsigned Line,
                                   unsigned SizeInBits, unsigned AlignInBits,
                                   llvm::DINode::DIFlags Flags,
                                   StringRef MangledName) {
  return DBuilder.createStructType(
      Scope, Name, File, Line, SizeInBits, AlignInBits, Flags,
      /* DerivedFrom */ nullptr,
      DBuilder.getOrCreateArray(ArrayRef<llvm::Metadata *>()),
      llvm::dwarf::DW_LANG_Swift, nullptr, MangledName);
}

llvm::DIType *IRGenDebugInfo::createType(DebugTypeInfo DbgTy,
                                         StringRef MangledName,
                                         llvm::DIScope *Scope,
                                         llvm::DIFile *File) {
  // FIXME: For SizeInBits, clang uses the actual size of the type on
  // the target machine instead of the storage size that is alloca'd
  // in the LLVM IR. For all types that are boxed in a struct, we are
  // emitting the storage size of the struct, but it may be necessary
  // to emit the (target!) size of the underlying basic type.
  uint64_t SizeOfByte = CI.getTargetInfo().getCharWidth();
  uint64_t SizeInBits = DbgTy.size.getValue() * SizeOfByte;
  uint64_t AlignInBits = DbgTy.align.getValue() * SizeOfByte;
  unsigned Encoding = 0;
  llvm::DINode::DIFlags Flags = llvm::DINode::FlagZero;

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
    return InternalType;
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
    return DBuilder.createPointerType(IdTy, PtrSize, PtrAlign,
                                      /* DWARFAddressSpace */ None,
                                      MangledName);
  }

  case TypeKind::BuiltinNativeObject: {
    unsigned PtrSize = CI.getTargetInfo().getPointerWidth(0);
    unsigned PtrAlign = CI.getTargetInfo().getPointerAlign(0);
    auto PTy = DBuilder.createPointerType(nullptr, PtrSize, PtrAlign,
                                          /* DWARFAddressSpace */ None,
                                          MangledName);
    return DBuilder.createObjectPointerType(PTy);
  }

  case TypeKind::BuiltinBridgeObject: {
    unsigned PtrSize = CI.getTargetInfo().getPointerWidth(0);
    unsigned PtrAlign = CI.getTargetInfo().getPointerAlign(0);
    auto PTy = DBuilder.createPointerType(nullptr, PtrSize, PtrAlign,
                                          /* DWARFAddressSpace */ None,
                                          MangledName);
    return DBuilder.createObjectPointerType(PTy);
  }

  case TypeKind::BuiltinRawPointer: {
    unsigned PtrSize = CI.getTargetInfo().getPointerWidth(0);
    unsigned PtrAlign = CI.getTargetInfo().getPointerAlign(0);
    return DBuilder.createPointerType(nullptr, PtrSize, PtrAlign,
                                      /* DWARFAddressSpace */ None,
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
    auto L = getDebugLoc(SM, Decl);
    auto *File = getOrCreateFile(L.Filename);
    if (Opts.DebugInfoKind > IRGenDebugInfoKind::ASTTypes)
      return createStructType(DbgTy, Decl, StructTy, Scope, File, L.Line,
                              SizeInBits, AlignInBits, Flags,
                              nullptr, // DerivedFrom
                              llvm::dwarf::DW_LANG_Swift, MangledName);
    else
      return createOpaqueStruct(Scope, Decl->getName().str(), File, L.Line,
                                SizeInBits, AlignInBits, Flags, MangledName);
  }

  case TypeKind::Class: {
    // Classes are represented as DW_TAG_structure_type. This way the
    // DW_AT_APPLE_runtime_class(DW_LANG_Swift) attribute can be
    // used to differentiate them from C++ and ObjC classes.
    auto *ClassTy = BaseTy->castTo<ClassType>();
    auto *Decl = ClassTy->getDecl();
    auto L = getDebugLoc(SM, Decl);
    if (auto *ClangDecl = Decl->getClangDecl()) {
      auto ClangSrcLoc = ClangDecl->getLocStart();
      clang::SourceManager &ClangSM =
          CI.getClangASTContext().getSourceManager();
      L.Line = ClangSM.getPresumedLineNumber(ClangSrcLoc);
      L.Filename = ClangSM.getBufferName(ClangSrcLoc);

      // Use "__ObjC" as default for implicit decls.
      // FIXME: Do something more clever based on the decl's mangled name.
      std::string FullModuleNameBuffer;
      StringRef ModulePath;
      StringRef ModuleName = "__ObjC";
      if (auto *OwningModule = ClangDecl->getImportedOwningModule())
        ModuleName = OwningModule->getTopLevelModuleName();

      if (auto *SwiftModule = Decl->getParentModule())
        if (auto *ClangModule = SwiftModule->findUnderlyingClangModule()) {
          // FIXME: Clang submodules are not handled here.
          // FIXME: Clang module config macros are not handled here.
          FullModuleNameBuffer = ClangModule->getFullModuleName();
          ModuleName = FullModuleNameBuffer;
          // FIXME: A clang module's Directory is supposed to be the
          // directory containing the module map, but ClangImporter
          // sets it to the module cache directory.
          if (ClangModule->Directory)
            ModulePath = ClangModule->Directory->getName();
        }
      Scope = getOrCreateModule(ModuleName, TheCU, ModuleName, ModulePath);
    }
    assert(SizeInBits == CI.getTargetInfo().getPointerWidth(0));
    return createPointerSizedStruct(Scope, Decl->getNameStr(),
                                    getOrCreateFile(L.Filename), L.Line, Flags,
                                    MangledName);
  }

  case TypeKind::Protocol: {
    auto *ProtocolTy = BaseTy->castTo<ProtocolType>();
    auto *Decl = ProtocolTy->getDecl();
    // FIXME: (LLVM branch) This should probably be a DW_TAG_interface_type.
    auto L = getDebugLoc(SM, Decl);
    auto File = getOrCreateFile(L.Filename);
    return createOpaqueStruct(Scope, Decl ? Decl->getNameStr() : MangledName,
                              File, L.Line, SizeInBits, AlignInBits, Flags,
                              MangledName);
  }

  case TypeKind::ProtocolComposition: {
    auto *Decl = DbgTy.getDecl();
    auto L = getDebugLoc(SM, Decl);
    auto File = getOrCreateFile(L.Filename);

    // FIXME: emit types
    // auto ProtocolCompositionTy = BaseTy->castTo<ProtocolCompositionType>();
    return createOpaqueStruct(Scope, Decl ? Decl->getNameStr() : MangledName,
                              File, L.Line, SizeInBits, AlignInBits, Flags,
                              MangledName);
  }

  case TypeKind::UnboundGeneric: {
    auto *UnboundTy = BaseTy->castTo<UnboundGenericType>();
    auto *Decl = UnboundTy->getDecl();
    auto L = getDebugLoc(SM, Decl);
    assert(SizeInBits == CI.getTargetInfo().getPointerWidth(0));
    return createPointerSizedStruct(Scope,
                                    Decl ? Decl->getNameStr() : MangledName,
                                    File, L.Line, Flags, MangledName);
  }

  case TypeKind::BoundGenericStruct: {
    auto *StructTy = BaseTy->castTo<BoundGenericStructType>();
    auto *Decl = StructTy->getDecl();
    auto L = getDebugLoc(SM, Decl);
    return createOpaqueStruct(Scope, Decl ? Decl->getNameStr() : MangledName,
                              File, L.Line, SizeInBits, AlignInBits, Flags,
                              MangledName);
  }

  case TypeKind::BoundGenericClass: {
    auto *ClassTy = BaseTy->castTo<BoundGenericClassType>();
    auto *Decl = ClassTy->getDecl();
    auto L = getDebugLoc(SM, Decl);
    // TODO: We may want to peek at Decl->isObjC() and set this
    // attribute accordingly.
    assert(SizeInBits == CI.getTargetInfo().getPointerWidth(0));
    return createPointerSizedStruct(Scope,
                                    Decl ? Decl->getNameStr() : MangledName,
                                    File, L.Line, Flags, MangledName);
  }

  case TypeKind::Tuple: {
    // Tuples are also represented as structs.
    if (Opts.DebugInfoKind > IRGenDebugInfoKind::ASTTypes)
      return createTuple(DbgTy, Scope, SizeInBits, AlignInBits, Flags,
                         MangledName);
    else
      return createOpaqueStruct(Scope, MangledName, MainFile, 0, SizeInBits,
                                AlignInBits, Flags, MangledName);
  }

  case TypeKind::InOut: {
    // This is an inout type. Naturally we would be emitting them as
    // DW_TAG_reference_type types, but LLDB can deal better with pointer-sized
    // struct that has the appropriate mangled name.
    auto ObjectTy = BaseTy->castTo<InOutType>()->getObjectType();
    if (Opts.DebugInfoKind > IRGenDebugInfoKind::ASTTypes) {
      auto DT = getOrCreateDesugaredType(ObjectTy, DbgTy);
      return createPointerSizedStruct(
          Scope, MangledName, DT, File, 0, Flags,
          MangledName);
    } else
      return createOpaqueStruct(Scope, MangledName, File, 0, SizeInBits,
                                AlignInBits, Flags, MangledName);
  }

  case TypeKind::Archetype: {
    auto *Archetype = BaseTy->castTo<ArchetypeType>();
    auto L = getDebugLoc(SM, Archetype->getAssocType());
    auto Superclass = Archetype->getSuperclass();
    auto DerivedFrom = Superclass.isNull()
                           ? nullptr
                           : getOrCreateDesugaredType(Superclass, DbgTy);
    auto FwdDecl = llvm::TempDIType(
      DBuilder.createReplaceableCompositeType(
        llvm::dwarf::DW_TAG_structure_type, MangledName, Scope, File, L.Line,
          llvm::dwarf::DW_LANG_Swift, SizeInBits, AlignInBits, Flags,
          MangledName));

    // Emit the protocols the archetypes conform to.
    SmallVector<llvm::Metadata *, 4> Protocols;
    for (auto *ProtocolDecl : Archetype->getConformsTo()) {
      auto PTy = IGM.getLoweredType(ProtocolDecl->getInterfaceType())
                     .getSwiftRValueType();
      auto PDbgTy = DebugTypeInfo::getFromTypeInfo(
          DbgTy.getDeclContext(), DbgTy.getGenericEnvironment(),
          ProtocolDecl->getInterfaceType(), IGM.getTypeInfoForLowered(PTy));
      auto PDITy = getOrCreateType(PDbgTy);
      Protocols.push_back(DBuilder.createInheritance(FwdDecl.get(),
                                                     PDITy, 0, Flags));
    }
    auto DITy = DBuilder.createStructType(
        Scope, MangledName, File, L.Line, SizeInBits, AlignInBits, Flags,
        DerivedFrom, DBuilder.getOrCreateArray(Protocols),
        llvm::dwarf::DW_LANG_Swift, nullptr,
        MangledName);

    DBuilder.replaceTemporary(std::move(FwdDecl), DITy);
    return DITy;
  }

  case TypeKind::ExistentialMetatype:
  case TypeKind::Metatype: {
    // Metatypes are (mostly) singleton type descriptors, often without storage.
    Flags |= llvm::DINode::FlagArtificial;
    auto L = getDebugLoc(SM, DbgTy.getDecl());
    auto File = getOrCreateFile(L.Filename);
    return DBuilder.createStructType(
        Scope, MangledName, File, L.Line, SizeInBits, AlignInBits, Flags,
        nullptr, nullptr, llvm::dwarf::DW_LANG_Swift,
        nullptr, MangledName);
  }

  case TypeKind::SILFunction:
  case TypeKind::Function:
 case TypeKind::GenericFunction: {
    if (Opts.DebugInfoKind > IRGenDebugInfoKind::ASTTypes)
      return createFunctionPointer(DbgTy, Scope, SizeInBits, AlignInBits, Flags,
                                   MangledName);
    else
      return createOpaqueStruct(Scope, MangledName, MainFile, 0, SizeInBits,
                                AlignInBits, Flags, MangledName);
  }

  case TypeKind::Enum: {
    auto *EnumTy = BaseTy->castTo<EnumType>();
    auto *Decl = EnumTy->getDecl();
    auto L = getDebugLoc(SM, Decl);
    auto *File = getOrCreateFile(L.Filename);
    if (Opts.DebugInfoKind > IRGenDebugInfoKind::ASTTypes)
      return createEnumType(DbgTy, Decl, MangledName, Scope, File, L.Line,
                            Flags);
    else
      return createOpaqueStruct(Scope, Decl->getName().str(), File, L.Line,
                                SizeInBits, AlignInBits, Flags, MangledName);
  }

  case TypeKind::BoundGenericEnum: {
    auto *EnumTy = BaseTy->castTo<BoundGenericEnumType>();
    auto *Decl = EnumTy->getDecl();
    auto L = getDebugLoc(SM, Decl);
    auto *File = getOrCreateFile(L.Filename);
    if (Opts.DebugInfoKind > IRGenDebugInfoKind::ASTTypes)
      return createEnumType(DbgTy, Decl, MangledName, Scope, File, L.Line,
                            Flags);
    else
      return createOpaqueStruct(Scope, Decl->getName().str(), File, L.Line,
                                SizeInBits, AlignInBits, Flags, MangledName);
  }

  case TypeKind::BuiltinVector: {
    (void)MangledName; // FIXME emit the name somewhere.
    auto *BuiltinVectorTy = BaseTy->castTo<BuiltinVectorType>();
    DebugTypeInfo ElemDbgTy(DbgTy.getDeclContext(),
                            DbgTy.getGenericEnvironment(),
                            BuiltinVectorTy->getElementType(),
                            DbgTy.StorageType, DbgTy.size, DbgTy.align);
    auto Subscripts = nullptr;
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
    auto L = getDebugLoc(SM, DbgTy.getDecl());
    auto File = getOrCreateFile(L.Filename);
    return DBuilder.createTypedef(getOrCreateDesugaredType(CanTy, DbgTy),
                                  MangledName, File, L.Line, File);
  }

  // Sugared types.

  case TypeKind::NameAlias: {

    auto *NameAliasTy = cast<NameAliasType>(BaseTy);
    auto *Decl = NameAliasTy->getDecl();
    auto L = getDebugLoc(SM, Decl);
    auto AliasedTy = NameAliasTy->getSinglyDesugaredType();
    auto File = getOrCreateFile(L.Filename);
    // For NameAlias types, the DeclContext for the aliasED type is
    // in the decl of the alias type.
    DebugTypeInfo AliasedDbgTy(DbgTy.getDeclContext(),
                               DbgTy.getGenericEnvironment(), AliasedTy,
                               DbgTy.StorageType, DbgTy.size, DbgTy.align);
    return DBuilder.createTypedef(getOrCreateType(AliasedDbgTy), MangledName,
                                  File, L.Line, File);
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
  case TypeKind::Error:
  case TypeKind::Unresolved:
  case TypeKind::LValue:
  case TypeKind::TypeVariable:
  case TypeKind::Module:
  case TypeKind::SILBlockStorage:
  case TypeKind::SILBox:
  case TypeKind::BuiltinUnsafeValueBuffer:

    DEBUG(llvm::errs() << "Unhandled type: "; DbgTy.getType()->dump();
          llvm::errs() << "\n");
    MangledName = "<unknown>";
  }
  return DBuilder.createBasicType(MangledName, SizeInBits, Encoding);
}

/// Determine if there exists a name mangling for the given type.
static bool canMangle(TypeBase *Ty) {
  switch (Ty->getKind()) {
  case TypeKind::GenericFunction:     // Not yet supported.
  case TypeKind::SILBlockStorage:     // Not supported at all.
  case TypeKind::SILBox:
    return false;
  case TypeKind::InOut: {
    auto *ObjectTy = Ty->castTo<InOutType>()->getObjectType().getPointer();
    return canMangle(ObjectTy);
  }
  default:
    return true;
  }
}

llvm::DIType *IRGenDebugInfo::getTypeOrNull(TypeBase *Ty) {
  auto CachedType = DITypeCache.find(Ty);
  if (CachedType != DITypeCache.end()) {
    // Verify that the information still exists.
    if (llvm::Metadata *Val = CachedType->second) {
      auto DITy = cast<llvm::DIType>(Val);
      return DITy;
    }
  }
  return nullptr;
}

llvm::DIType *IRGenDebugInfo::getOrCreateType(DebugTypeInfo DbgTy) {
  // Is this an empty type?
  if (DbgTy.isNull())
    // We can't use the empty type as an index into DenseMap.
    return createType(DbgTy, "", TheCU, MainFile);

  // Look in the cache first.
  if (auto *DITy = getTypeOrNull(DbgTy.getType()))
    return DITy;

  // Second line of defense: Look up the mangled name. TypeBase*'s are
  // not necessarily unique, but name mangling is too expensive to do
  // every time.
  StringRef MangledName;
  llvm::MDString *UID = nullptr;
  if (canMangle(DbgTy.getType())) {
    MangledName = getMangledName(DbgTy);
    UID = llvm::MDString::get(IGM.getLLVMContext(), MangledName);
    if (llvm::Metadata *CachedTy = DIRefMap.lookup(UID)) {
      auto DITy = cast<llvm::DIType>(CachedTy);
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
  llvm::DIScope *Scope = getOrCreateContext(Context);
  llvm::DIType *DITy = createType(DbgTy, MangledName, Scope, getFile(Scope));

  // Incrementally build the DIRefMap.
  if (auto *CTy = dyn_cast<llvm::DICompositeType>(DITy)) {
#ifndef NDEBUG
    // Sanity check.
    if (llvm::Metadata *V = DIRefMap.lookup(UID)) {
      auto *CachedTy = cast<llvm::DIType>(V);
      assert(CachedTy == DITy && "conflicting types for one UID");
    }
#endif
    // If this type supports a UID, enter it to the cache.
    if (auto UID = CTy->getRawIdentifier()) {
      assert(UID->getString() == MangledName &&
             "Unique identifier is different from mangled name ");
      DIRefMap[UID] = llvm::TrackingMDNodeRef(DITy);
    }
  }

  // Store it in the cache.
  DITypeCache.insert({DbgTy.getType(), llvm::TrackingMDNodeRef(DITy)});

  return DITy;
}

void IRGenDebugInfo::finalize() {
  assert(LocationStack.empty() && "Mismatch of pushLoc() and popLoc().");

  // Finalize all replaceable forward declarations.
  for (auto &Ty : ReplaceMap) {
    llvm::TempMDNode FwdDecl(cast<llvm::MDNode>(Ty.second));
    llvm::Metadata *Replacement;
    if (auto *FullType = getTypeOrNull(Ty.first))
      Replacement = FullType;
    else
      Replacement = Ty.second;
    DBuilder.replaceTemporary(std::move(FwdDecl),
                              cast<llvm::MDNode>(Replacement));
  }
  // Finalize the DIBuilder.
  DBuilder.finalize();
}
