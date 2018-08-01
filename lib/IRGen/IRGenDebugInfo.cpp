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
#include "swift/AST/ASTMangler.h"
#include "swift/AST/Expr.h"
#include "swift/AST/IRGenOptions.h"
#include "swift/AST/Module.h"
#include "swift/AST/ModuleLoader.h"
#include "swift/AST/Pattern.h"
#include "swift/Basic/Dwarf.h"
#include "swift/Basic/SourceManager.h"
#include "swift/Basic/Version.h"
#include "swift/ClangImporter/ClangImporter.h"
#include "swift/Demangling/ManglingMacros.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILBasicBlock.h"
#include "swift/SIL/SILDebugScope.h"
#include "swift/SIL/SILModule.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/Decl.h"
#include "clang/AST/ExternalASTSource.h"
#include "clang/Basic/Module.h"
#include "clang/Basic/SourceLocation.h"
#include "clang/Basic/SourceManager.h"
#include "clang/Basic/TargetInfo.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Serialization/ASTReader.h"
#include "llvm/Config/config.h"
#include "llvm/IR/DIBuilder.h"
#include "llvm/IR/DebugInfo.h"
#include "llvm/IR/IntrinsicInst.h"
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

namespace {
using TrackingDIRefMap =
    llvm::DenseMap<const llvm::MDString *, llvm::TrackingMDNodeRef>;

class IRGenDebugInfoImpl : public IRGenDebugInfo {
  friend class IRGenDebugInfoImpl;
  const IRGenOptions &Opts;
  ClangImporter &CI;
  SourceManager &SM;
  llvm::DIBuilder DBuilder;
  IRGenModule &IGM;
  const PathRemapper &DebugPrefixMap;

  /// Various caches.
  /// @{
  llvm::DenseMap<const SILDebugScope *, llvm::TrackingMDNodeRef> ScopeCache;
  llvm::DenseMap<const SILDebugScope *, llvm::TrackingMDNodeRef> InlinedAtCache;
  llvm::DenseMap<const void *, SILLocation::DebugLoc> DebugLocCache;
  llvm::DenseMap<TypeBase *, llvm::TrackingMDNodeRef> DITypeCache;
  llvm::DenseMap<const void *, llvm::TrackingMDNodeRef> DIModuleCache;
  llvm::StringMap<llvm::TrackingMDNodeRef> DIFileCache;
  TrackingDIRefMap DIRefMap;
  /// @}

  /// A list of replaceable fwddecls that need to be RAUWed at the end.
  std::vector<std::pair<TypeBase *, llvm::TrackingMDRef>> ReplaceMap;
  /// The set of imported modules.
  llvm::DenseSet<ModuleDecl *> ImportedModules;

  llvm::BumpPtrAllocator DebugInfoNames;
  StringRef CWDName;                    /// The current working directory.
  SmallString<0> ConfigMacros;          /// User-provided -D macro definitions.
  llvm::DICompileUnit *TheCU = nullptr; /// The current compilation unit.
  llvm::DIFile *MainFile = nullptr;     /// The main file.
  llvm::DIModule *MainModule = nullptr; /// The current module.
  llvm::DIScope *EntryPointFn =
      nullptr;                     /// Scope of SWIFT_ENTRY_POINT_FUNCTION.
  TypeAliasDecl *MetadataTypeDecl; /// The type decl for swift.type.
  llvm::DIType *InternalType;      /// Catch-all type for opaque internal types.

  SILLocation::DebugLoc LastDebugLoc; /// The last location that was emitted.
  const SILDebugScope *LastScope;     /// The scope of that last location.

  /// Used by pushLoc.
  SmallVector<std::pair<SILLocation::DebugLoc, const SILDebugScope *>, 8>
      LocationStack;

public:
  IRGenDebugInfoImpl(const IRGenOptions &Opts, ClangImporter &CI,
                     IRGenModule &IGM, llvm::Module &M,
                     StringRef MainOutputFilenameForDebugInfo);
  void finalize();

  void setCurrentLoc(IRBuilder &Builder, const SILDebugScope *DS,
                     SILLocation Loc);
  void clearLoc(IRBuilder &Builder);
  void pushLoc();
  void popLoc();
  void setInlinedTrapLocation(IRBuilder &Builder, const SILDebugScope *Scope);
  void setEntryPointLoc(IRBuilder &Builder);
  llvm::DIScope *getEntryPointFn();
  llvm::DIScope *getOrCreateScope(const SILDebugScope *DS);
  void emitImport(ImportDecl *D);
  llvm::DISubprogram *emitFunction(const SILDebugScope *DS, llvm::Function *Fn,
                                   SILFunctionTypeRepresentation Rep,
                                   SILType Ty, DeclContext *DeclCtx = nullptr,
                                   GenericEnvironment *GE = nullptr);
  llvm::DISubprogram *emitFunction(SILFunction &SILFn, llvm::Function *Fn);
  void emitArtificialFunction(IRBuilder &Builder, llvm::Function *Fn,
                              SILType SILTy);
  void emitVariableDeclaration(IRBuilder &Builder,
                               ArrayRef<llvm::Value *> Storage,
                               DebugTypeInfo Ty, const SILDebugScope *DS,
                               ValueDecl *VarDecl, StringRef Name,
                               unsigned ArgNo = 0,
                               IndirectionKind = DirectValue,
                               ArtificialKind = RealValue);
  void emitDbgIntrinsic(IRBuilder &Builder, llvm::Value *Storage,
                        llvm::DILocalVariable *Var, llvm::DIExpression *Expr,
                        unsigned Line, unsigned Col, llvm::DILocalScope *Scope,
                        const SILDebugScope *DS);
  void emitGlobalVariableDeclaration(llvm::GlobalVariable *Storage,
                                     StringRef Name, StringRef LinkageName,
                                     DebugTypeInfo DebugType,
                                     bool IsLocalToUnit, bool InFixedBuffer,
                                     Optional<SILLocation> Loc);
  void emitTypeMetadata(IRGenFunction &IGF, llvm::Value *Metadata,
                        unsigned Depth, unsigned Index, StringRef AssocType);

  /// Return the DIBuilder.
  llvm::DIBuilder &getBuilder() { return DBuilder; }

  /// Decode (and cache) a SourceLoc.
  SILLocation::DebugLoc decodeSourceLoc(SourceLoc SL);

  IRGenDebugInfoFormat getDebugInfoFormat() { return Opts.DebugInfoFormat; }

private:
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
  SILLocation::DebugLoc getDebugLoc(IRGenDebugInfo &DI, WithLoc *S,
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

    return DI.decodeSourceLoc(Loc);
  }

  SILLocation::DebugLoc getStartLocation(Optional<SILLocation> OptLoc) {
    if (!OptLoc)
      return {};
    return decodeSourceLoc(OptLoc->getStartSourceLoc());
  }

  SILLocation::DebugLoc sanitizeCodeViewDebugLoc(SILLocation::DebugLoc DLoc) {
    if (Opts.DebugInfoFormat == IRGenDebugInfoFormat::CodeView)
      // When WinDbg finds two locations with the same line but different
      // columns, the user must select an address when they break on that
      // line. Also, clang does not emit column locations in CodeView for C++.
      DLoc.Column = 0;
    return DLoc;
  }

  SILLocation::DebugLoc decodeDebugLoc(SILLocation Loc) {
    if (Loc.isDebugInfoLoc())
      return sanitizeCodeViewDebugLoc(Loc.getDebugInfoLoc());
    return decodeSourceLoc(Loc.getDebugSourceLoc());
  }

  SILLocation::DebugLoc getDebugLocation(Optional<SILLocation> OptLoc) {
    if (!OptLoc || (Opts.DebugInfoFormat != IRGenDebugInfoFormat::CodeView &&
                    OptLoc->isInPrologue()))
      return {};
    return decodeDebugLoc(*OptLoc);
  }

  /// Strdup a raw char array using the bump pointer.
  StringRef BumpAllocatedString(const char *Data, size_t Length) {
    char *Ptr = DebugInfoNames.Allocate<char>(Length + 1);
    memcpy(Ptr, Data, Length);
    *(Ptr + Length) = 0;
    return StringRef(Ptr, Length);
  }

  /// Strdup S using the bump pointer.
  StringRef BumpAllocatedString(std::string S) {
    return BumpAllocatedString(S.c_str(), S.length());
  }

  /// Strdup StringRef S using the bump pointer.
  StringRef BumpAllocatedString(StringRef S) {
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

  /// Determine whether this debug scope belongs to an explicit closure.
  static bool isExplicitClosure(const SILFunction *SILFn) {
    if (SILFn && SILFn->hasLocation())
      if (Expr *E = SILFn->getLocation().getAsASTNode<Expr>())
        if (isa<ClosureExpr>(E))
          return true;
    return false;
  }

  llvm::MDNode *createInlinedAt(const SILDebugScope *DS) {
    auto *CS = DS->InlinedCallSite;
    if (!CS)
      return nullptr;

    auto CachedInlinedAt = InlinedAtCache.find(CS);
    if (CachedInlinedAt != InlinedAtCache.end())
      return cast<llvm::MDNode>(CachedInlinedAt->second);

    auto L = decodeDebugLoc(CS->Loc);
    auto Scope = getOrCreateScope(CS->Parent.dyn_cast<const SILDebugScope *>());
    // Pretend transparent functions don't exist.
    if (!Scope)
      return createInlinedAt(CS);
    auto InlinedAt =
        llvm::DebugLoc::get(L.Line, L.Column, Scope, createInlinedAt(CS));
    InlinedAtCache.insert(
        {CS, llvm::TrackingMDNodeRef(InlinedAt.getAsMDNode())});
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
#endif

  llvm::DIFile *getOrCreateFile(StringRef Filename) {
    if (Filename.empty())
      return MainFile;

    // Look in the cache first.
    auto CachedFile = DIFileCache.find(Filename);
    if (CachedFile != DIFileCache.end()) {
      // Verify that the information still exists.
      if (llvm::Metadata *V = CachedFile->second)
        return cast<llvm::DIFile>(V);
    }

    // Detect the main file.
    if (MainFile && Filename.endswith(MainFile->getFilename())) {
      SmallString<256> AbsThisFile, AbsMainFile;
      AbsThisFile = Filename;
      llvm::sys::fs::make_absolute(AbsThisFile);
      llvm::sys::path::append(AbsMainFile, MainFile->getDirectory(),
                              MainFile->getFilename());
      if (AbsThisFile == AbsMainFile) {
        DIFileCache[Filename] = llvm::TrackingMDNodeRef(MainFile);
        return MainFile;
      }
    }

    // Create a new one.
    StringRef File = llvm::sys::path::filename(Filename);
    llvm::SmallString<512> Path(Filename);
    llvm::sys::path::remove_filename(Path);
    llvm::DIFile *F = DBuilder.createFile(DebugPrefixMap.remapPath(File),
                                          DebugPrefixMap.remapPath(Path));

    // Cache it.
    DIFileCache[Filename] = llvm::TrackingMDNodeRef(F);
    return F;
  }

  StringRef getName(const FuncDecl &FD) {
    // Getters and Setters are anonymous functions, so we forge a name
    // using its parent declaration.
    if (auto accessor = dyn_cast<AccessorDecl>(&FD))
      if (ValueDecl *VD = accessor->getStorage()) {
        const char *Kind;
        switch (accessor->getAccessorKind()) {
        case AccessorKind::Get:
          Kind = ".get";
          break;
        case AccessorKind::Set:
          Kind = ".set";
          break;
        case AccessorKind::WillSet:
          Kind = ".willset";
          break;
        case AccessorKind::DidSet:
          Kind = ".didset";
          break;
        case AccessorKind::MaterializeForSet:
          Kind = ".materialize";
          break;
        case AccessorKind::Address:
          Kind = ".addressor";
          break;
        case AccessorKind::MutableAddress:
          Kind = ".mutableAddressor";
          break;
        case AccessorKind::Read:
          Kind = ".read";
          break;
        case AccessorKind::Modify:
          Kind = ".modify";
          break;
        }

        SmallVector<char, 64> Buf;
        StringRef Name = (VD->getBaseName().userFacingName() +
                          Twine(Kind)).toStringRef(Buf);
        return BumpAllocatedString(Name);
      }

    if (FD.hasName())
      return FD.getName().str();

    return StringRef();
  }

  StringRef getName(SILLocation L) {
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
      LLVM_DEBUG(llvm::dbgs() << "Unexpected function type: "; SILTy.dump();
                 llvm::dbgs() << "\n");
      return CanSILFunctionType();
    }

    return FnTy;
  }

  llvm::DIScope *getOrCreateContext(DeclContext *DC) {
    if (!DC)
      return TheCU;

    if (isa<FuncDecl>(DC))
      if (auto *Decl = IGM.getSILModule().lookUpFunction(SILDeclRef(
              cast<AbstractFunctionDecl>(DC), SILDeclRef::Kind::Func)))
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
      return getOrCreateModule(
          {ModuleDecl::AccessPathTy(), cast<ModuleDecl>(DC)});
    case DeclContextKind::FileUnit:
      // A module may contain multiple files.
      return getOrCreateContext(DC->getParent());
    case DeclContextKind::GenericTypeDecl: {
      auto *NTD = cast<NominalTypeDecl>(DC);
      auto *Ty = NTD->getDeclaredType().getPointer();
      if (auto *DITy = getTypeOrNull(Ty))
        return DITy;

      // Create a Forward-declared type.
      auto Loc = getDebugLoc(*this, NTD);
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

  void createParameterType(llvm::SmallVectorImpl<llvm::Metadata *> &Parameters,
                           SILType type, DeclContext *DeclCtx,
                           GenericEnvironment *GE) {
    auto RealType = type.getASTType();
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

  llvm::DITypeRefArray createParameterTypes(SILType SILTy, DeclContext *DeclCtx,
                                            GenericEnvironment *GE) {
    if (!SILTy)
      return nullptr;
    return createParameterTypes(SILTy.castTo<SILFunctionType>(), DeclCtx, GE);
  }

  llvm::DITypeRefArray createParameterTypes(CanSILFunctionType FnTy,
                                            DeclContext *DeclCtx,
                                            GenericEnvironment *GE) {
    SmallVector<llvm::Metadata *, 16> Parameters;

    GenericContextScope scope(IGM, FnTy->getGenericSignature());

    // The function return type is the first element in the list.
    createParameterType(Parameters, getResultTypeForDebugInfo(FnTy), DeclCtx,
                        GE);

    // Actually, the input type is either a single type or a tuple
    // type. We currently represent a function with one n-tuple argument
    // as an n-ary function.
    for (auto Param : FnTy->getParameters())
      createParameterType(Parameters, IGM.silConv.getSILType(Param), DeclCtx,
                          GE);

    return DBuilder.getOrCreateTypeArray(Parameters);
  }

  /// FIXME: replace this condition with something more sane.
  static bool isAllocatingConstructor(SILFunctionTypeRepresentation Rep,
                                      DeclContext *DeclCtx) {
    return Rep != SILFunctionTypeRepresentation::Method && DeclCtx &&
           isa<ConstructorDecl>(DeclCtx);
  }

  llvm::DIModule *getOrCreateModule(const void *Key, llvm::DIScope *Parent,
                                    StringRef Name, StringRef IncludePath,
                                    StringRef ConfigMacros = StringRef()) {
    // Look in the cache first.
    auto Val = DIModuleCache.find(Key);
    if (Val != DIModuleCache.end())
      return cast<llvm::DIModule>(Val->second);

    StringRef Sysroot = IGM.Context.SearchPathOpts.SDKPath;
    auto M = DBuilder.createModule(
        Parent, Name, ConfigMacros, DebugPrefixMap.remapPath(IncludePath),
        Sysroot);
    DIModuleCache.insert({Key, llvm::TrackingMDNodeRef(M)});
    return M;
  }

  llvm::DIModule *
  getOrCreateModule(clang::ExternalASTSource::ASTSourceDescriptor Desc) {
    // Handle Clang modules.
    if (const clang::Module *ClangModule = Desc.getModuleOrNull()) {
      llvm::DIModule *Parent = nullptr;
      if (ClangModule->Parent)
        Parent = getOrCreateModule(*ClangModule->Parent);

      return getOrCreateModule(ClangModule, Parent,
                               Desc.getModuleName(), Desc.getPath(),
                               ConfigMacros);
    }
    // Handle PCH.
    return getOrCreateModule(Desc.getASTFile().bytes_begin(), nullptr,
                             Desc.getModuleName(), Desc.getPath(),
                             ConfigMacros);
  };

  llvm::DIModule *getOrCreateModule(ModuleDecl::ImportedModule IM) {
    ModuleDecl *M = IM.second;
    if (auto *ClangModule = M->findUnderlyingClangModule())
      return getOrCreateModule(*ClangModule);

    StringRef Path = getFilenameFromDC(M);
    StringRef Name = M->getName().str();
    return getOrCreateModule(M, TheCU, Name, Path);
  }

  TypeAliasDecl *getMetadataType() {
    if (!MetadataTypeDecl) {
      MetadataTypeDecl = new (IGM.Context) TypeAliasDecl(
          SourceLoc(), SourceLoc(), IGM.Context.getIdentifier("$swift.type"),
          SourceLoc(),
          /*genericparams*/ nullptr, IGM.Context.TheBuiltinModule);
      MetadataTypeDecl->setUnderlyingType(IGM.Context.TheRawPointerType);
    }
    return MetadataTypeDecl;
  }

  /// Return the DIFile that is the ancestor of Scope.
  llvm::DIFile *getFile(llvm::DIScope *Scope) {
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

  static Size getStorageSize(const llvm::DataLayout &DL,
                             ArrayRef<llvm::Value *> Storage) {
    unsigned size = 0;
    for (llvm::Value *Piece : Storage)
      size += DL.getTypeSizeInBits(Piece->getType());
    return Size(size);
  }

  StringRef getMangledName(DebugTypeInfo DbgTy) {
    if (MetadataTypeDecl && DbgTy.getDecl() == MetadataTypeDecl)
      return BumpAllocatedString(DbgTy.getDecl()->getName().str());

    Type Ty = DbgTy.getType();
    if (!Ty->hasTypeParameter())
      Ty = Ty->mapTypeOutOfContext();

    Mangle::ASTMangler Mangler;
    std::string Name = Mangler.mangleTypeForDebugger(
        Ty, DbgTy.getDeclContext(), DbgTy.getGenericEnvironment());
    return BumpAllocatedString(Name);
  }

  llvm::DIDerivedType *createMemberType(DebugTypeInfo DbgTy, StringRef Name,
                                        unsigned &OffsetInBits,
                                        llvm::DIScope *Scope,
                                        llvm::DIFile *File,
                                        llvm::DINode::DIFlags Flags) {
    unsigned SizeOfByte = CI.getTargetInfo().getCharWidth();
    auto *Ty = getOrCreateType(DbgTy);
    auto *DITy = DBuilder.createMemberType(Scope, Name, File, 0,
                                           SizeOfByte * DbgTy.size.getValue(),
                                           0, OffsetInBits, Flags, Ty);
    OffsetInBits += getSizeInBits(Ty);
    OffsetInBits =
        llvm::alignTo(OffsetInBits, SizeOfByte * DbgTy.align.getValue());
    return DITy;
  }

  llvm::DINodeArray
  getTupleElements(TupleType *TupleTy, llvm::DIScope *Scope, llvm::DIFile *File,
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
      Elements.push_back(createMemberType(DbgTy, StringRef(), OffsetInBits,
                                          Scope, File, Flags));
    }
    SizeInBits = OffsetInBits;
    return DBuilder.getOrCreateArray(Elements);
  }

  llvm::DINodeArray getStructMembers(NominalTypeDecl *D, Type BaseTy,
                                     llvm::DIScope *Scope, llvm::DIFile *File,
                                     llvm::DINode::DIFlags Flags,
                                     unsigned &SizeInBits) {
    SmallVector<llvm::Metadata *, 16> Elements;
    unsigned OffsetInBits = 0;
    for (VarDecl *VD : D->getStoredProperties()) {
      auto memberTy =
          BaseTy->getTypeOfMember(IGM.getSwiftModule(), VD, nullptr);

      auto DbgTy = DebugTypeInfo::getFromTypeInfo(
          VD->getDeclContext(),
          VD->getDeclContext()->getGenericEnvironmentOfContext(),
          VD->getInterfaceType(),
          IGM.getTypeInfoForUnlowered(
              IGM.getSILTypes().getAbstractionPattern(VD), memberTy));
      Elements.push_back(createMemberType(DbgTy, VD->getName().str(),
                                          OffsetInBits, Scope, File, Flags));
    }
    if (OffsetInBits > SizeInBits)
      SizeInBits = OffsetInBits;
    return DBuilder.getOrCreateArray(Elements);
  }

  llvm::DICompositeType *
  createStructType(DebugTypeInfo DbgTy, NominalTypeDecl *Decl, Type BaseTy,
                   llvm::DIScope *Scope, llvm::DIFile *File, unsigned Line,
                   unsigned SizeInBits, unsigned AlignInBits,
                   llvm::DINode::DIFlags Flags, llvm::DIType *DerivedFrom,
                   unsigned RuntimeLang, StringRef UniqueID) {
    StringRef Name = Decl->getName().str();

    // Forward declare this first because types may be recursive.
    auto FwdDecl = llvm::TempDIType(DBuilder.createReplaceableCompositeType(
        llvm::dwarf::DW_TAG_structure_type, Name, Scope, File, Line,
        llvm::dwarf::DW_LANG_Swift, SizeInBits, 0, Flags, UniqueID));

#ifndef NDEBUG
    if (UniqueID.empty())
      assert(!Name.empty() &&
             "no mangled name and no human readable name given");
    else
      assert((UniqueID.startswith("_T") ||
              UniqueID.startswith(MANGLING_PREFIX_STR)) &&
             "UID is not a mangled name");
#endif

    auto TH = llvm::TrackingMDNodeRef(FwdDecl.get());
    DITypeCache[DbgTy.getType()] = TH;
    auto Members =
        getStructMembers(Decl, BaseTy, Scope, File, Flags, SizeInBits);
    auto DITy = DBuilder.createStructType(
        Scope, Name, File, Line, SizeInBits, AlignInBits, Flags, DerivedFrom,
        Members, RuntimeLang, nullptr, UniqueID);
    DBuilder.replaceTemporary(std::move(FwdDecl), DITy);
    return DITy;
  }

  llvm::DINodeArray getEnumElements(DebugTypeInfo DbgTy, EnumDecl *ED,
                                    llvm::DIScope *Scope, llvm::DIFile *File,
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
                          DbgTy.StorageType, DbgTy.size, DbgTy.align, true);
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
            DbgTy.StorageType, Size(0), Alignment(1), true);
      }
      unsigned Offset = 0;
      auto MTy = createMemberType(ElemDbgTy, ElemDecl->getName().str(), Offset,
                                  Scope, File, Flags);
      Elements.push_back(MTy);
    }
    return DBuilder.getOrCreateArray(Elements);
  }

  llvm::DICompositeType *createEnumType(DebugTypeInfo DbgTy, EnumDecl *Decl,
                                        StringRef MangledName,
                                        llvm::DIScope *Scope,
                                        llvm::DIFile *File, unsigned Line,
                                        llvm::DINode::DIFlags Flags) {
    unsigned SizeOfByte = CI.getTargetInfo().getCharWidth();
    unsigned SizeInBits = DbgTy.size.getValue() * SizeOfByte;
    // Default, since Swift doesn't allow specifying a custom alignment.
    unsigned AlignInBits = 0;

    // FIXME: Is DW_TAG_union_type the right thing here?
    // Consider using a DW_TAG_variant_type instead.
    auto FwdDecl = llvm::TempDIType(DBuilder.createReplaceableCompositeType(
        llvm::dwarf::DW_TAG_union_type, MangledName, Scope, File, Line,
        llvm::dwarf::DW_LANG_Swift, SizeInBits, AlignInBits, Flags,
        MangledName));

    auto TH = llvm::TrackingMDNodeRef(FwdDecl.get());
    DITypeCache[DbgTy.getType()] = TH;

    auto DITy = DBuilder.createUnionType(
        Scope, Decl->getName().str(), File, Line, SizeInBits, AlignInBits,
        Flags, getEnumElements(DbgTy, Decl, Scope, File, Flags),
        llvm::dwarf::DW_LANG_Swift, MangledName);

    DBuilder.replaceTemporary(std::move(FwdDecl), DITy);
    return DITy;
  }

  llvm::DIType *getOrCreateDesugaredType(Type Ty, DebugTypeInfo DbgTy) {
    DebugTypeInfo BlandDbgTy(
        DbgTy.getDeclContext(), DbgTy.getGenericEnvironment(), Ty,
        DbgTy.StorageType, DbgTy.size, DbgTy.align, DbgTy.DefaultAlignment);
    return getOrCreateType(BlandDbgTy);
  }

  uint64_t getSizeOfBasicType(DebugTypeInfo DbgTy) {
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

  llvm::DIType *createPointerSizedStruct(llvm::DIScope *Scope, StringRef Name,
                                         llvm::DIFile *File, unsigned Line,
                                         llvm::DINode::DIFlags Flags,
                                         StringRef MangledName) {
    if (Opts.DebugInfoLevel > IRGenDebugInfoLevel::ASTTypes) {
      auto FwdDecl = DBuilder.createForwardDecl(
          llvm::dwarf::DW_TAG_structure_type, Name, Scope, File, Line,
          llvm::dwarf::DW_LANG_Swift, 0, 0);
      return createPointerSizedStruct(Scope, Name, FwdDecl, File, Line, Flags,
                                      MangledName);
    } else {
      unsigned SizeInBits = CI.getTargetInfo().getPointerWidth(0);
      return createOpaqueStruct(Scope, Name, File, Line, SizeInBits, 0, Flags,
                                MangledName);
    }
  }

  llvm::DIType *createPointerSizedStruct(llvm::DIScope *Scope, StringRef Name,
                                         llvm::DIType *PointeeTy,
                                         llvm::DIFile *File, unsigned Line,
                                         llvm::DINode::DIFlags Flags,
                                         StringRef MangledName) {
    unsigned PtrSize = CI.getTargetInfo().getPointerWidth(0);
    auto PtrTy = DBuilder.createPointerType(PointeeTy, PtrSize, 0);
    llvm::Metadata *Elements[] = {DBuilder.createMemberType(
        Scope, "ptr", File, 0, PtrSize, 0, 0, Flags, PtrTy)};
    return DBuilder.createStructType(
        Scope, Name, File, Line, PtrSize, 0, Flags,
        /* DerivedFrom */ nullptr, DBuilder.getOrCreateArray(Elements),
        llvm::dwarf::DW_LANG_Swift, nullptr, MangledName);
  }

  llvm::DIType *
  createDoublePointerSizedStruct(llvm::DIScope *Scope, StringRef Name,
                                 llvm::DIType *PointeeTy, llvm::DIFile *File,
                                 unsigned Line, llvm::DINode::DIFlags Flags,
                                 StringRef MangledName) {
    unsigned PtrSize = CI.getTargetInfo().getPointerWidth(0);
    llvm::Metadata *Elements[] = {
        DBuilder.createMemberType(
            Scope, "ptr", File, 0, PtrSize, 0, 0, Flags,
            DBuilder.createPointerType(PointeeTy, PtrSize, 0)),
        DBuilder.createMemberType(
            Scope, "_", File, 0, PtrSize, 0, 0, Flags,
            DBuilder.createPointerType(nullptr, PtrSize, 0))};
    return DBuilder.createStructType(
        Scope, Name, File, Line, 2 * PtrSize, 0, Flags,
        /* DerivedFrom */ nullptr, DBuilder.getOrCreateArray(Elements),
        llvm::dwarf::DW_LANG_Swift, nullptr, MangledName);
  }

  llvm::DIType *createFunctionPointer(DebugTypeInfo DbgTy, llvm::DIScope *Scope,
                                      unsigned SizeInBits, unsigned AlignInBits,
                                      llvm::DINode::DIFlags Flags,
                                      StringRef MangledName) {
    auto FwdDecl = llvm::TempDINode(DBuilder.createReplaceableCompositeType(
        llvm::dwarf::DW_TAG_subroutine_type, MangledName, Scope, MainFile, 0,
        llvm::dwarf::DW_LANG_Swift, SizeInBits, AlignInBits, Flags,
        MangledName));

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
      auto *nongenericTy = FunctionType::get(fTy->getInput(), fTy->getResult(),
                                             fTy->getExtInfo());

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
        DITy = createDoublePointerSizedStruct(Scope, MangledName, FnTy,
                                              MainFile, 0, Flags, MangledName);
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

  llvm::DIType *createTuple(DebugTypeInfo DbgTy, llvm::DIScope *Scope,
                            unsigned SizeInBits, unsigned AlignInBits,
                            llvm::DINode::DIFlags Flags,
                            StringRef MangledName) {
    TypeBase *BaseTy = DbgTy.getType();
    auto *TupleTy = BaseTy->castTo<TupleType>();
    auto FwdDecl = llvm::TempDINode(DBuilder.createReplaceableCompositeType(
        llvm::dwarf::DW_TAG_structure_type, MangledName, Scope, MainFile, 0,
        llvm::dwarf::DW_LANG_Swift, SizeInBits, AlignInBits, Flags,
        MangledName));

    DITypeCache[DbgTy.getType()] = llvm::TrackingMDNodeRef(FwdDecl.get());

    unsigned RealSize;
    auto Elements = getTupleElements(TupleTy, Scope, MainFile, Flags,
                                     DbgTy.getDeclContext(),
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

  llvm::DIType *createOpaqueStruct(llvm::DIScope *Scope, StringRef Name,
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

  llvm::DIType *createType(DebugTypeInfo DbgTy, StringRef MangledName,
                           llvm::DIScope *Scope, llvm::DIFile *File) {
    // FIXME: For SizeInBits, clang uses the actual size of the type on
    // the target machine instead of the storage size that is alloca'd
    // in the LLVM IR. For all types that are boxed in a struct, we are
    // emitting the storage size of the struct, but it may be necessary
    // to emit the (target!) size of the underlying basic type.
    uint64_t SizeOfByte = CI.getTargetInfo().getCharWidth();
    uint64_t SizeInBits = DbgTy.size.getValue() * SizeOfByte;
    unsigned AlignInBits =
        DbgTy.DefaultAlignment ? 0 : DbgTy.align.getValue() * SizeOfByte;
    unsigned Encoding = 0;
    llvm::DINode::DIFlags Flags = llvm::DINode::FlagZero;

    TypeBase *BaseTy = DbgTy.getType();

    if (!BaseTy) {
      LLVM_DEBUG(llvm::dbgs() << "Type without TypeBase: ";
                 DbgTy.getType()->dump();
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
      auto IdTy = DBuilder.createForwardDecl(llvm::dwarf::DW_TAG_structure_type,
                                             MangledName, Scope, File, 0,
                                             llvm::dwarf::DW_LANG_ObjC, 0, 0);
      return DBuilder.createPointerType(IdTy, PtrSize, 0,
                                        /* DWARFAddressSpace */ None,
                                        MangledName);
    }

    case TypeKind::BuiltinNativeObject: {
      unsigned PtrSize = CI.getTargetInfo().getPointerWidth(0);
      auto PTy =
          DBuilder.createPointerType(nullptr, PtrSize, 0,
                                     /* DWARFAddressSpace */ None, MangledName);
      return DBuilder.createObjectPointerType(PTy);
    }

    case TypeKind::BuiltinBridgeObject: {
      unsigned PtrSize = CI.getTargetInfo().getPointerWidth(0);
      auto PTy =
          DBuilder.createPointerType(nullptr, PtrSize, 0,
                                     /* DWARFAddressSpace */ None, MangledName);
      return DBuilder.createObjectPointerType(PTy);
    }

    case TypeKind::BuiltinRawPointer: {
      unsigned PtrSize = CI.getTargetInfo().getPointerWidth(0);
      return DBuilder.createPointerType(nullptr, PtrSize, 0,
                                        /* DWARFAddressSpace */ None,
                                        MangledName);
    }

    case TypeKind::DynamicSelf: {
      // Self. We don't have a way to represent instancetype in DWARF,
      // so we emit the static type instead. This is similar to what we
      // do with instancetype in Objective-C.
      auto *DynamicSelfTy = BaseTy->castTo<DynamicSelfType>();
      auto SelfTy =
          getOrCreateDesugaredType(DynamicSelfTy->getSelfType(), DbgTy);
      return DBuilder.createTypedef(SelfTy, MangledName, File, 0, File);
    }

    // Even builtin swift types usually come boxed in a struct.
    case TypeKind::Struct: {
      auto *StructTy = BaseTy->castTo<StructType>();
      auto *Decl = StructTy->getDecl();
      auto L = getDebugLoc(*this, Decl);
      if (auto *ClangDecl = Decl->getClangDecl()) {
        auto ClangSrcLoc = ClangDecl->getLocStart();
        clang::SourceManager &ClangSM =
            CI.getClangASTContext().getSourceManager();
        L.Line = ClangSM.getPresumedLineNumber(ClangSrcLoc);
        L.Filename = ClangSM.getBufferName(ClangSrcLoc);
      }
      auto *File = getOrCreateFile(L.Filename);
      if (Opts.DebugInfoLevel > IRGenDebugInfoLevel::ASTTypes)
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
      auto L = getDebugLoc(*this, Decl);
      if (auto *ClangDecl = Decl->getClangDecl()) {
        auto ClangSrcLoc = ClangDecl->getLocStart();
        clang::SourceManager &ClangSM =
            CI.getClangASTContext().getSourceManager();
        L.Line = ClangSM.getPresumedLineNumber(ClangSrcLoc);
        L.Filename = ClangSM.getBufferName(ClangSrcLoc);
      }
      assert(SizeInBits == CI.getTargetInfo().getPointerWidth(0));
      return createPointerSizedStruct(Scope, Decl->getNameStr(),
                                      getOrCreateFile(L.Filename), L.Line,
                                      Flags, MangledName);
    }

    case TypeKind::Protocol: {
      auto *ProtocolTy = BaseTy->castTo<ProtocolType>();
      auto *Decl = ProtocolTy->getDecl();
      // FIXME: (LLVM branch) This should probably be a DW_TAG_interface_type.
      auto L = getDebugLoc(*this, Decl);
      auto File = getOrCreateFile(L.Filename);
      return createOpaqueStruct(Scope, Decl ? Decl->getNameStr() : MangledName,
                                File, L.Line, SizeInBits, AlignInBits, Flags,
                                MangledName);
    }

    case TypeKind::ProtocolComposition: {
      auto *Decl = DbgTy.getDecl();
      auto L = getDebugLoc(*this, Decl);
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
      auto L = getDebugLoc(*this, Decl);
      assert(SizeInBits == CI.getTargetInfo().getPointerWidth(0));
      return createPointerSizedStruct(Scope,
                                      Decl ? Decl->getNameStr() : MangledName,
                                      File, L.Line, Flags, MangledName);
    }

    case TypeKind::BoundGenericStruct: {
      auto *StructTy = BaseTy->castTo<BoundGenericStructType>();
      auto *Decl = StructTy->getDecl();
      auto L = getDebugLoc(*this, Decl);
      return createOpaqueStruct(Scope, Decl ? Decl->getNameStr() : MangledName,
                                File, L.Line, SizeInBits, AlignInBits, Flags,
                                MangledName);
    }

    case TypeKind::BoundGenericClass: {
      auto *ClassTy = BaseTy->castTo<BoundGenericClassType>();
      auto *Decl = ClassTy->getDecl();
      auto L = getDebugLoc(*this, Decl);
      // TODO: We may want to peek at Decl->isObjC() and set this
      // attribute accordingly.
      assert(SizeInBits == CI.getTargetInfo().getPointerWidth(0));
      return createPointerSizedStruct(Scope,
                                      Decl ? Decl->getNameStr() : MangledName,
                                      File, L.Line, Flags, MangledName);
    }

    case TypeKind::Tuple: {
      // Tuples are also represented as structs.
      if (Opts.DebugInfoLevel > IRGenDebugInfoLevel::ASTTypes)
        return createTuple(DbgTy, Scope, SizeInBits, AlignInBits, Flags,
                           MangledName);
      else
        return createOpaqueStruct(Scope, MangledName, MainFile, 0, SizeInBits,
                                  AlignInBits, Flags, MangledName);
    }

    case TypeKind::InOut: {
      // This is an inout type. Naturally we would be emitting them as
      // DW_TAG_reference_type types, but LLDB can deal better with
      // pointer-sized struct that has the appropriate mangled name.
      auto ObjectTy = BaseTy->castTo<InOutType>()->getObjectType();
      auto Name = MangledName;
      if (auto *Decl = ObjectTy->getAnyNominal())
        Name = Decl->getName().str();
      if (Opts.DebugInfoLevel > IRGenDebugInfoLevel::ASTTypes) {
        auto DT = getOrCreateDesugaredType(ObjectTy, DbgTy);
        return createPointerSizedStruct(Scope, Name, DT, File, 0, Flags,
                                        MangledName);
      } else
        return createOpaqueStruct(Scope, Name, File, 0, SizeInBits, AlignInBits,
                                  Flags, MangledName);
    }

    case TypeKind::Archetype: {
      auto *Archetype = BaseTy->castTo<ArchetypeType>();
      auto L = getDebugLoc(*this, Archetype->getAssocType());
      auto Superclass = Archetype->getSuperclass();
      auto DerivedFrom = Superclass.isNull()
                             ? nullptr
                             : getOrCreateDesugaredType(Superclass, DbgTy);
      auto FwdDecl = llvm::TempDIType(DBuilder.createReplaceableCompositeType(
          llvm::dwarf::DW_TAG_structure_type, MangledName, Scope, File, L.Line,
          llvm::dwarf::DW_LANG_Swift, SizeInBits, AlignInBits, Flags,
          MangledName));

      // Emit the protocols the archetypes conform to.
      SmallVector<llvm::Metadata *, 4> Protocols;
      for (auto *ProtocolDecl : Archetype->getConformsTo()) {
        auto PTy = IGM.getLoweredType(ProtocolDecl->getInterfaceType())
                       .getASTType();
        auto PDbgTy = DebugTypeInfo::getFromTypeInfo(
            DbgTy.getDeclContext(), DbgTy.getGenericEnvironment(),
            ProtocolDecl->getInterfaceType(), IGM.getTypeInfoForLowered(PTy));
        auto PDITy = getOrCreateType(PDbgTy);
        Protocols.push_back(
            DBuilder.createInheritance(FwdDecl.get(), PDITy, 0, Flags));
      }
      auto DITy = DBuilder.createStructType(
          Scope, MangledName, File, L.Line, SizeInBits, AlignInBits, Flags,
          DerivedFrom, DBuilder.getOrCreateArray(Protocols),
          llvm::dwarf::DW_LANG_Swift, nullptr, MangledName);

      DBuilder.replaceTemporary(std::move(FwdDecl), DITy);
      return DITy;
    }

    case TypeKind::ExistentialMetatype:
    case TypeKind::Metatype: {
      // Metatypes are (mostly) singleton type descriptors, often without
      // storage.
      Flags |= llvm::DINode::FlagArtificial;
      auto L = getDebugLoc(*this, DbgTy.getDecl());
      auto File = getOrCreateFile(L.Filename);
      return DBuilder.createStructType(
          Scope, MangledName, File, L.Line, SizeInBits, AlignInBits, Flags,
          nullptr, nullptr, llvm::dwarf::DW_LANG_Swift, nullptr, MangledName);
    }

    case TypeKind::SILFunction:
    case TypeKind::Function:
    case TypeKind::GenericFunction: {
      if (Opts.DebugInfoLevel > IRGenDebugInfoLevel::ASTTypes)
        return createFunctionPointer(DbgTy, Scope, SizeInBits, AlignInBits,
                                     Flags, MangledName);
      else
        return createOpaqueStruct(Scope, MangledName, MainFile, 0, SizeInBits,
                                  AlignInBits, Flags, MangledName);
    }

    case TypeKind::Enum: {
      auto *EnumTy = BaseTy->castTo<EnumType>();
      auto *Decl = EnumTy->getDecl();
      auto L = getDebugLoc(*this, Decl);
      auto *File = getOrCreateFile(L.Filename);
      if (Opts.DebugInfoLevel > IRGenDebugInfoLevel::ASTTypes)
        return createEnumType(DbgTy, Decl, MangledName, Scope, File, L.Line,
                              Flags);
      else
        return createOpaqueStruct(Scope, Decl->getName().str(), File, L.Line,
                                  SizeInBits, AlignInBits, Flags, MangledName);
    }

    case TypeKind::BoundGenericEnum: {
      auto *EnumTy = BaseTy->castTo<BoundGenericEnumType>();
      auto *Decl = EnumTy->getDecl();
      auto L = getDebugLoc(*this, Decl);
      auto *File = getOrCreateFile(L.Filename);
      if (Opts.DebugInfoLevel > IRGenDebugInfoLevel::ASTTypes)
        return createEnumType(DbgTy, Decl, MangledName, Scope, File, L.Line,
                              Flags);
      else
        return createOpaqueStruct(Scope, Decl->getName().str(), File, L.Line,
                                  SizeInBits, AlignInBits, Flags, MangledName);
    }

    case TypeKind::BuiltinVector: {
      (void)MangledName; // FIXME emit the name somewhere.
      auto *BuiltinVectorTy = BaseTy->castTo<BuiltinVectorType>();
      auto ElemTy = BuiltinVectorTy->getElementType();
      auto ElemDbgTy = DebugTypeInfo::getFromTypeInfo(
          DbgTy.getDeclContext(), DbgTy.getGenericEnvironment(), ElemTy,
          IGM.getTypeInfoForUnlowered(ElemTy));
      unsigned Count = BuiltinVectorTy->getNumElements();
      auto Subscript = DBuilder.getOrCreateSubrange(0, Count ? Count : -1);
      return DBuilder.createVectorType(SizeInBits,
                                       AlignInBits, getOrCreateType(ElemDbgTy),
                                       DBuilder.getOrCreateArray(Subscript));
    }

    // Reference storage types.
#define REF_STORAGE(Name, ...) \
    case TypeKind::Name##Storage:
#include "swift/AST/ReferenceStorage.def"
    {
      auto *ReferenceTy = cast<ReferenceStorageType>(BaseTy);
      auto CanTy = ReferenceTy->getReferentType();
      auto L = getDebugLoc(*this, DbgTy.getDecl());
      auto File = getOrCreateFile(L.Filename);
      return DBuilder.createTypedef(getOrCreateDesugaredType(CanTy, DbgTy),
                                    MangledName, File, L.Line, File);
    }

    // Sugared types.

    case TypeKind::NameAlias: {
      auto *NameAliasTy = cast<NameAliasType>(BaseTy);
      auto *Decl = NameAliasTy->getDecl();
      auto L = getDebugLoc(*this, Decl);
      auto AliasedTy = NameAliasTy->getSinglyDesugaredType();
      auto File = getOrCreateFile(L.Filename);
      // For NameAlias types, the DeclContext for the aliasED type is
      // in the decl of the alias type.
      DebugTypeInfo AliasedDbgTy(
         DbgTy.getDeclContext(), DbgTy.getGenericEnvironment(), AliasedTy,
         DbgTy.StorageType, DbgTy.size, DbgTy.align, DbgTy.DefaultAlignment);
      return DBuilder.createTypedef(getOrCreateType(AliasedDbgTy), MangledName,
                                    File, L.Line, File);
    }

    case TypeKind::Paren: {
      auto Ty = cast<ParenType>(BaseTy)->getUnderlyingType();
      return getOrCreateDesugaredType(Ty, DbgTy);
    }

    // SyntaxSugarType derivations.
    case TypeKind::Dictionary:
    case TypeKind::ArraySlice:
    case TypeKind::Optional: {
      auto *SyntaxSugarTy = cast<SyntaxSugarType>(BaseTy);
      auto *CanTy = SyntaxSugarTy->getSinglyDesugaredType();
      return getOrCreateDesugaredType(CanTy, DbgTy);
    }

    case TypeKind::DependentMember:
    case TypeKind::GenericTypeParam: {
      // FIXME: Provide a more meaningful debug type.
      return DBuilder.createStructType(
          Scope, MangledName, File, 0, SizeInBits, AlignInBits, Flags,
          nullptr, nullptr,
          llvm::dwarf::DW_LANG_Swift, nullptr, MangledName);
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
    case TypeKind::SILToken:
    case TypeKind::BuiltinUnsafeValueBuffer:

      LLVM_DEBUG(llvm::errs() << "Unhandled type: "; DbgTy.getType()->dump();
                 llvm::errs() << "\n");
      MangledName = "<unknown>";
    }
    return DBuilder.createBasicType(MangledName, SizeInBits, Encoding);
  }

  /// Determine if there exists a name mangling for the given type.
  static bool canMangle(TypeBase *Ty) {
    switch (Ty->getKind()) {
    case TypeKind::GenericFunction: // Not yet supported.
    case TypeKind::SILBlockStorage: // Not supported at all.
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

  llvm::DIType *getTypeOrNull(TypeBase *Ty) {
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

  /// The private discriminator is represented as an inline namespace.
  llvm::DIScope *getFilePrivateScope(llvm::DIScope *Parent, TypeDecl *Decl) {
    // Retrieve the private discriminator.
    auto *MSC = Decl->getDeclContext()->getModuleScopeContext();
    auto *FU = cast<FileUnit>(MSC);
    Identifier PD = FU->getDiscriminatorForPrivateValue(Decl);
    bool ExportSymbols = true;
    return DBuilder.createNameSpace(Parent, PD.str(), ExportSymbols);
  }

  llvm::DIType *getOrCreateType(DebugTypeInfo DbgTy) {
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
    llvm::DIScope *Scope = nullptr;
    DeclContext *Context = DbgTy.getType()->getNominalOrBoundGenericNominal();
    if (Context) {
      if (auto *D = Context->getAsNominalTypeOrNominalTypeExtensionContext())
        if (auto *ClangDecl = D->getClangDecl()) {
          clang::ASTReader &Reader = *CI.getClangInstance().getModuleManager();
          auto Idx = ClangDecl->getOwningModuleID();
          if (auto Info = Reader.getSourceDescriptor(Idx))
            Scope = getOrCreateModule(*Info);
        }
      Context = Context->getParent();
    }
    if (!Scope)
      Scope = getOrCreateContext(Context);

    // Scope outermost fileprivate decls in an inline private discriminator
    // namespace.
    if (auto *Decl = DbgTy.getDecl())
      if (Decl->isOutermostPrivateOrFilePrivateScope())
        Scope = getFilePrivateScope(Scope, Decl);

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
};

IRGenDebugInfoImpl::IRGenDebugInfoImpl(const IRGenOptions &Opts,
                                       ClangImporter &CI, IRGenModule &IGM,
                                       llvm::Module &M,
                                       StringRef MainOutputFilenameForDebugInfo)
    : Opts(Opts), CI(CI), SM(IGM.Context.SourceMgr), DBuilder(M),
      IGM(IGM), DebugPrefixMap(Opts.DebugPrefixMap), MetadataTypeDecl(nullptr),
      InternalType(nullptr), LastDebugLoc({}), LastScope(nullptr) {
  assert(Opts.DebugInfoLevel > IRGenDebugInfoLevel::None &&
         "no debug info should be generated");
  llvm::SmallString<256> SourcePath;
  if (MainOutputFilenameForDebugInfo.empty())
    SourcePath = "<unknown>";
  else
    SourcePath = MainOutputFilenameForDebugInfo;

  unsigned Lang = llvm::dwarf::DW_LANG_Swift;
  std::string Producer = version::getSwiftFullVersion(
      IGM.Context.LangOpts.EffectiveLanguageVersion);
  StringRef Flags = Opts.DebugFlags;
  unsigned Major, Minor;
  std::tie(Major, Minor) = version::getSwiftNumericVersion();
  unsigned MajorRuntimeVersion = Major;

  // No split DWARF on Darwin.
  StringRef SplitName = StringRef();
  // Note that File + Dir need not result in a valid path.
  // Clang is doing the same thing here.
  TheCU = DBuilder.createCompileUnit(
      Lang, DBuilder.createFile(
          DebugPrefixMap.remapPath(SourcePath),
          DebugPrefixMap.remapPath(Opts.DebugCompilationDir)),
      Producer, Opts.shouldOptimize(), Flags, MajorRuntimeVersion, SplitName,
      Opts.DebugInfoLevel > IRGenDebugInfoLevel::LineTables
          ? llvm::DICompileUnit::FullDebug
          : llvm::DICompileUnit::LineTablesOnly);
  MainFile = getOrCreateFile(BumpAllocatedString(SourcePath));

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
  auto *MDecl = IGM.getSwiftModule();
  llvm::sys::path::remove_filename(SourcePath);
  MainModule = getOrCreateModule(MDecl, TheCU, Opts.ModuleName, SourcePath);
  DBuilder.createImportedModule(MainFile, MainModule, MainFile, 0);

  // Macro definitions that were defined by the user with "-Xcc -D" on the
  // command line. This does not include any macros defined by ClangImporter.
  llvm::raw_svector_ostream OS(ConfigMacros);
  unsigned I = 0;
  // Translate the macro definitions back into a commmand line.
  for (auto &Macro : Opts.ClangDefines) {
    if (++I > 1)
      OS << ' ';
    OS << '"';
    for (char c : Macro)
      switch (c) {
      case '\\': OS << "\\\\"; break;
      case '"':  OS << "\\\""; break;
      default: OS << c;
      }
    OS << '"';
  }
}

void IRGenDebugInfoImpl::finalize() {
  assert(LocationStack.empty() && "Mismatch of pushLoc() and popLoc().");

  // Get the list of imported modules (which may actually be different
  // from all ImportDecls).
  SmallVector<ModuleDecl::ImportedModule, 8> ModuleWideImports;
  IGM.getSwiftModule()->getImportedModules(ModuleWideImports,
                                           ModuleDecl::ImportFilter::All);
  for (auto M : ModuleWideImports)
    if (!ImportedModules.count(M.second))
      DBuilder.createImportedModule(MainFile, getOrCreateModule(M), MainFile,
                                    0);

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

void IRGenDebugInfoImpl::setCurrentLoc(IRBuilder &Builder,
                                       const SILDebugScope *DS,
                                       SILLocation Loc) {
  assert(DS && "empty scope");
  auto *Scope = getOrCreateScope(DS);
  if (!Scope)
    return;

  SILLocation::DebugLoc L;
  SILFunction *Fn = DS->getInlinedFunction();
  if (Fn && (Fn->isThunk() || Fn->isTransparent())) {
    L = SILLocation::getCompilerGeneratedDebugLoc();
  } else if (DS == LastScope && Loc.isAutoGenerated()) {
    // Reuse the last source location if we are still in the same
    // scope to get a more contiguous line table.
    L = LastDebugLoc;
  } else if (DS == LastScope && Loc.is<ArtificialUnreachableLocation>() &&
             Opts.DebugInfoFormat == IRGenDebugInfoFormat::CodeView) {
    // Remove unreachable locations with line zero because line zero does not
    // represent an artificial location in CodeView.
    L = LastDebugLoc;
  } else {
    // Decode the location.
    L = getDebugLocation(Loc);
    // Otherwise use a line 0 artificial location, but the file from the
    // location. If we are emitting CodeView, we do not want to use line zero
    // since it does not represent an artificial line location.
    if (Loc.isAutoGenerated() &&
        Opts.DebugInfoFormat != IRGenDebugInfoFormat::CodeView) {
      L.Line = 0;
      L.Column = 0;
    }
  }

  auto *File = getOrCreateFile(L.Filename);
  if (File->getFilename() != Scope->getFilename()) {
    // We changed files in the middle of a scope. This happens, for
    // example, when constructors are inlined. Create a new scope to
    // reflect this.
    auto File = getOrCreateFile(L.Filename);
    Scope = DBuilder.createLexicalBlockFile(Scope, File);
  }

  // FIXME: Enable this assertion.
  // assert(lineNumberIsSane(Builder, L.Line) &&
  //       "-Onone, but line numbers are not monotonically increasing within
  //       bb");
  LastDebugLoc = L;
  LastScope = DS;

  auto *InlinedAt = createInlinedAt(DS);
  assert(((!InlinedAt) || (InlinedAt && Scope)) && "inlined w/o scope");
  assert(parentScopesAreSane(DS) && "parent scope sanity check failed");
  auto DL = llvm::DebugLoc::get(L.Line, L.Column, Scope, InlinedAt);
  Builder.SetCurrentDebugLocation(DL);
}

void IRGenDebugInfoImpl::clearLoc(IRBuilder &Builder) {
  LastDebugLoc = {};
  LastScope = nullptr;
  Builder.SetCurrentDebugLocation(llvm::DebugLoc());
}

/// Push the current debug location onto a stack and initialize the
/// IRBuilder to an empty location.
void IRGenDebugInfoImpl::pushLoc() {
  LocationStack.push_back(std::make_pair(LastDebugLoc, LastScope));
  LastDebugLoc = {};
  LastScope = nullptr;
}

/// Restore the current debug location from the stack.
void IRGenDebugInfoImpl::popLoc() {
  std::tie(LastDebugLoc, LastScope) = LocationStack.pop_back_val();
}

/// This is done for WinDbg to avoid having two non-contiguous sets of
/// instructions because the ``@llvm.trap`` instruction gets placed at the end
/// of the function.
void IRGenDebugInfoImpl::setInlinedTrapLocation(IRBuilder &Builder,
                                                const SILDebugScope *Scope) {
  if (Opts.DebugInfoFormat != IRGenDebugInfoFormat::CodeView)
    return;
  auto DLInlinedAt = llvm::DebugLoc::get(LastDebugLoc.Line, LastDebugLoc.Column,
                                         getOrCreateScope(LastScope));
  // FIXME: This location should point to stdlib instead of being artificial.
  auto DL = llvm::DebugLoc::get(0, 0, getOrCreateScope(Scope), DLInlinedAt);
  Builder.SetCurrentDebugLocation(DL);
}

void IRGenDebugInfoImpl::setEntryPointLoc(IRBuilder &Builder) {
  auto DL = llvm::DebugLoc::get(0, 0, getEntryPointFn(), nullptr);
  Builder.SetCurrentDebugLocation(DL);
}

llvm::DIScope *IRGenDebugInfoImpl::getEntryPointFn() {
  // Lazily create EntryPointFn.
  if (!EntryPointFn) {
    EntryPointFn = DBuilder.createReplaceableCompositeType(
        llvm::dwarf::DW_TAG_subroutine_type, SWIFT_ENTRY_POINT_FUNCTION,
        MainFile, MainFile, 0);
  }
  return EntryPointFn;
}

llvm::DIScope *IRGenDebugInfoImpl::getOrCreateScope(const SILDebugScope *DS) {
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

  if (Opts.DebugInfoLevel <= IRGenDebugInfoLevel::LineTables)
    return Parent;

  assert(DS->Parent && "lexical block must have a parent subprogram");
  auto L = getStartLocation(DS->Loc);
  llvm::DIFile *File = getOrCreateFile(L.Filename);
  auto *DScope = DBuilder.createLexicalBlock(Parent, File, L.Line, L.Column);

  // Cache it.
  ScopeCache[DS] = llvm::TrackingMDNodeRef(DScope);
  return DScope;
}

void IRGenDebugInfoImpl::emitImport(ImportDecl *D) {
  if (Opts.DebugInfoLevel <= IRGenDebugInfoLevel::LineTables)
    return;

  assert(D->getModule() && "compiler-synthesized ImportDecl is incomplete");
  ModuleDecl::ImportedModule Imported = {D->getModulePath(), D->getModule()};
  auto DIMod = getOrCreateModule(Imported);
  auto L = getDebugLoc(*this, D);
  auto *File = getOrCreateFile(L.Filename);
  DBuilder.createImportedModule(File, DIMod, File, L.Line);
  ImportedModules.insert(Imported.second);
}

llvm::DISubprogram *IRGenDebugInfoImpl::emitFunction(SILFunction &SILFn,
                                                     llvm::Function *Fn) {
  auto *DS = SILFn.getDebugScope();
  assert(DS && "SIL function has no debug scope");
  (void)DS;
  return emitFunction(SILFn.getDebugScope(), Fn, SILFn.getRepresentation(),
                      SILFn.getLoweredType(), SILFn.getDeclContext(),
                      SILFn.getGenericEnvironment());
}

llvm::DISubprogram *
IRGenDebugInfoImpl::emitFunction(const SILDebugScope *DS, llvm::Function *Fn,
                                 SILFunctionTypeRepresentation Rep,
                                 SILType SILTy, DeclContext *DeclCtx,
                                 GenericEnvironment *GE) {
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
  auto *SILFn = DS ? DS->Parent.dyn_cast<SILFunction *>() : nullptr;

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

  /// The source line used for the function prologue.
  unsigned ScopeLine = 0;
  SILLocation::DebugLoc L;
  if (!DS || (SILFn && (SILFn->isBare() || SILFn->isThunk() ||
                        SILFn->isTransparent()))) {
    // Bare functions and thunks should not have any line numbers. This
    // is especially important for shared functions like reabstraction
    // thunk helpers, where DS->Loc is an arbitrary location of whichever use
    // was emitted first.
    L = SILLocation::getCompilerGeneratedDebugLoc();
  } else {
    L = decodeDebugLoc(DS->Loc);
    ScopeLine = L.Line;
    if (!DS->Loc.isDebugInfoLoc())
      L = decodeSourceLoc(DS->Loc.getSourceLoc());
  }

  auto Line = L.Line;
  auto File = getOrCreateFile(L.Filename);
  llvm::DIScope *Scope = MainModule;
  if (SILFn && SILFn->getDeclContext())
    Scope = getOrCreateContext(SILFn->getDeclContext()->getParent());

  // We know that main always comes from MainFile.
  if (LinkageName == SWIFT_ENTRY_POINT_FUNCTION) {
    File = MainFile;
    Line = 1;
    Name = LinkageName;
  }

  CanSILFunctionType FnTy = getFunctionType(SILTy);
  auto Params = Opts.DebugInfoLevel > IRGenDebugInfoLevel::LineTables
                    ? createParameterTypes(SILTy, DeclCtx, GE)
                    : nullptr;
  llvm::DISubroutineType *DIFnTy = DBuilder.createSubroutineType(Params);
  llvm::DITemplateParameterArray TemplateParameters = nullptr;
  llvm::DISubprogram *Decl = nullptr;

  // Various flags.
  bool IsLocalToUnit = Fn ? Fn->hasInternalLinkage() : true;
  bool IsDefinition = true;
  llvm::DINode::DIFlags Flags = llvm::DINode::FlagZero;
  // Mark everything that is not visible from the source code (i.e.,
  // does not have a Swift name) as artificial, so the debugger can
  // ignore it. Explicit closures are exempt from this rule. We also
  // make an exception for toplevel code, which, although it does not
  // have a Swift name, does appear prominently in the source code.
  // ObjC thunks should also not show up in the linetable, because we
  // never want to set a breakpoint there.
  if ((Name.empty() && LinkageName != SWIFT_ENTRY_POINT_FUNCTION &&
       !isExplicitClosure(SILFn)) ||
      (Rep == SILFunctionTypeRepresentation::ObjCMethod) ||
      isAllocatingConstructor(Rep, DeclCtx)) {
    Flags |= llvm::DINode::FlagArtificial;
    ScopeLine = 0;
  }

  if (FnTy &&
      FnTy->getRepresentation() == SILFunctionType::Representation::Block)
    Flags |= llvm::DINode::FlagAppleBlock;

  // Get the throws information.
  llvm::DITypeArray Error = nullptr;
  if (FnTy)
    if (auto ErrorInfo = FnTy->getOptionalErrorResult()) {
      auto DTI = DebugTypeInfo::getFromTypeInfo(
          nullptr, nullptr, ErrorInfo->getType(),
          IGM.getTypeInfo(IGM.silConv.getSILType(*ErrorInfo)));
      Error = DBuilder.getOrCreateArray({getOrCreateType(DTI)}).get();
    }

  // Construct the DISubprogram.
  llvm::DISubprogram *SP = DBuilder.createFunction(
      Scope, Name, LinkageName, File, Line, DIFnTy, IsLocalToUnit, IsDefinition,
      ScopeLine, Flags, Opts.shouldOptimize(), TemplateParameters, Decl, Error);

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

void IRGenDebugInfoImpl::emitArtificialFunction(IRBuilder &Builder,
                                                llvm::Function *Fn,
                                                SILType SILTy) {
  RegularLocation ALoc = RegularLocation::getAutoGeneratedLocation();
  const SILDebugScope *Scope = new (IGM.getSILModule()) SILDebugScope(ALoc);
  emitFunction(Scope, Fn, SILFunctionTypeRepresentation::Thin, SILTy);
  /// Reusing the current file would be wrong: An objc thunk, for example, could
  /// be triggered from any random location. Use a placeholder name instead.
  setCurrentLoc(Builder, Scope, ALoc);
}

void IRGenDebugInfoImpl::emitVariableDeclaration(
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

  if (Opts.DebugInfoLevel <= IRGenDebugInfoLevel::LineTables)
    return;

  // We cannot yet represent opened existentials.
  if (DbgTy.Type->hasOpenedExistential())
    return;

  if (!DbgTy.size)
    DbgTy.size = getStorageSize(IGM.DataLayout, Storage);

  auto *Scope = dyn_cast_or_null<llvm::DILocalScope>(getOrCreateScope(DS));
  assert(Scope && "variable has no local scope");
  auto Loc = getDebugLoc(*this, VarDecl);

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
      (ArgNo > 0) ? DBuilder.createParameterVariable(
                        Scope, Name, ArgNo, Unit, Line, DITy, Optimized, Flags)
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

    // There are variables without storage, such as "struct { func foo() {}
    // }". Emit them as constant 0.
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
      assert(OffsetInBits + SizeInBits <= getSizeInBits(Var) && "pars > totum");

      // Add the piece DWARF expression.
      Operands.push_back(llvm::dwarf::DW_OP_LLVM_fragment);
      Operands.push_back(OffsetInBits);
      Operands.push_back(SizeInBits);
    }
    emitDbgIntrinsic(Builder, Piece, Var, DBuilder.createExpression(Operands),
                     Line, Loc.Column, Scope, DS);
  }

  // Emit locationless intrinsic for variables that were optimized away.
  if (Storage.empty())
    emitDbgIntrinsic(Builder, llvm::ConstantInt::get(IGM.Int64Ty, 0), Var,
                     DBuilder.createExpression(), Line, Loc.Column, Scope, DS);
}

void IRGenDebugInfoImpl::emitDbgIntrinsic(
    IRBuilder &Builder, llvm::Value *Storage, llvm::DILocalVariable *Var,
    llvm::DIExpression *Expr, unsigned Line, unsigned Col,
    llvm::DILocalScope *Scope, const SILDebugScope *DS) {
  // Set the location/scope of the intrinsic.
  auto *InlinedAt = createInlinedAt(DS);
  auto DL = llvm::DebugLoc::get(Line, Col, Scope, InlinedAt);
  auto *BB = Builder.GetInsertBlock();

  // An alloca may only be described by exactly one dbg.declare.
  if (isa<llvm::AllocaInst>(Storage) && !llvm::FindDbgAddrUses(Storage).empty())
    return;

  // A dbg.declare is only meaningful if there is a single alloca for
  // the variable that is live throughout the function. With SIL
  // optimizations this is not guaranteed and a variable can end up in
  // two allocas (for example, one function inlined twice).
  if (auto *Alloca = dyn_cast<llvm::AllocaInst>(Storage)) {
    auto *ParentBB = Alloca->getParent();
    auto InsertBefore = std::next(Alloca->getIterator());
    if (InsertBefore != ParentBB->end())
      DBuilder.insertDeclare(Alloca, Var, Expr, DL, &*InsertBefore);
    else
      DBuilder.insertDeclare(Alloca, Var, Expr, DL, ParentBB);
    return;
  }

  // Insert a dbg.value at the current insertion point.
  DBuilder.insertDbgValueIntrinsic(Storage, Var, Expr, DL, BB);
}

void IRGenDebugInfoImpl::emitGlobalVariableDeclaration(
    llvm::GlobalVariable *Var, StringRef Name, StringRef LinkageName,
    DebugTypeInfo DbgTy, bool IsLocalToUnit, bool Indirect,
    Optional<SILLocation> Loc) {
  if (Opts.DebugInfoLevel <= IRGenDebugInfoLevel::LineTables)
    return;

  llvm::DIType *Ty = getOrCreateType(DbgTy);
  if (Ty->isArtificial() || Ty == InternalType || !Loc)
    // FIXME: Really these should be marked as artificial, but LLVM
    // currently has no support for flags to be put on global
    // variables. In the mean time, elide these variables, they
    // would confuse both the user and LLDB.
    return;

  auto L = getStartLocation(Loc);
  auto File = getOrCreateFile(L.Filename);

  // Emit it as global variable of the current module.
  llvm::DIExpression *Expr = nullptr;
  if (!Var)
    Expr = DBuilder.createConstantValueExpression(0);
  else if (Indirect)
    Expr =
        DBuilder.createExpression(ArrayRef<uint64_t>(llvm::dwarf::DW_OP_deref));
  auto *GV = DBuilder.createGlobalVariableExpression(
      MainModule, Name, LinkageName, File, L.Line, Ty, IsLocalToUnit, Expr);
  if (Var)
    Var->addDebugInfo(GV);
}

void IRGenDebugInfoImpl::emitTypeMetadata(IRGenFunction &IGF,
                                          llvm::Value *Metadata, unsigned Depth,
                                          unsigned Index, StringRef AssocType) {
  if (Opts.DebugInfoLevel <= IRGenDebugInfoLevel::LineTables)
    return;

  // Don't emit debug info in transparent functions.
  auto *DS = IGF.getDebugScope();
  if (!DS || DS->getInlinedFunction()->isTransparent())
    return;

  llvm::SmallString<8> Buf;
  static const char *Tau = u8"\u03C4_";
  llvm::raw_svector_ostream OS(Buf);
  OS << '$' << Tau << Depth << '_' << Index << AssocType;
  auto DbgTy = DebugTypeInfo::getMetadata(
      getMetadataType()->getDeclaredInterfaceType().getPointer(),
      Metadata->getType(), Size(CI.getTargetInfo().getPointerWidth(0)),
      Alignment(CI.getTargetInfo().getPointerAlign(0)));
  emitVariableDeclaration(IGF.Builder, Metadata, DbgTy, IGF.getDebugScope(),
                          nullptr, OS.str().str(), 0,
                          // swift.type is already a pointer type,
                          // having a shadow copy doesn't add another
                          // layer of indirection.
                          DirectValue, ArtificialValue);
}

SILLocation::DebugLoc IRGenDebugInfoImpl::decodeSourceLoc(SourceLoc SL) {
  auto &Cached = DebugLocCache[SL.getOpaquePointerValue()];
  if (Cached.Filename.empty())
    Cached = sanitizeCodeViewDebugLoc(SILLocation::decode(SL, SM));
  return Cached;
}

} // anonymous namespace

IRGenDebugInfo *IRGenDebugInfo::createIRGenDebugInfo(
    const IRGenOptions &Opts, ClangImporter &CI, IRGenModule &IGM,
    llvm::Module &M, StringRef MainOutputFilenameForDebugInfo) {
  return new IRGenDebugInfoImpl(Opts, CI, IGM, M,
                                MainOutputFilenameForDebugInfo);
}


IRGenDebugInfo::~IRGenDebugInfo() {}

// Forwarding to the private implementation.
void IRGenDebugInfo::finalize() {
  static_cast<IRGenDebugInfoImpl *>(this)->finalize();
}

void IRGenDebugInfo::setCurrentLoc(IRBuilder &Builder, const SILDebugScope *DS,
                                   SILLocation Loc) {
  static_cast<IRGenDebugInfoImpl *>(this)->setCurrentLoc(Builder, DS, Loc);
}

void IRGenDebugInfo::clearLoc(IRBuilder &Builder) {
  static_cast<IRGenDebugInfoImpl *>(this)->clearLoc(Builder);
}

void IRGenDebugInfo::pushLoc() {
  static_cast<IRGenDebugInfoImpl *>(this)->pushLoc();
}

void IRGenDebugInfo::popLoc() {
  static_cast<IRGenDebugInfoImpl *>(this)->popLoc();
}

void IRGenDebugInfo::setInlinedTrapLocation(IRBuilder &Builder,
                                            const SILDebugScope *Scope) {
  static_cast<IRGenDebugInfoImpl *>(this)->setInlinedTrapLocation(Builder,
                                                                  Scope);
}

void IRGenDebugInfo::setEntryPointLoc(IRBuilder &Builder) {
  static_cast<IRGenDebugInfoImpl *>(this)->setEntryPointLoc(Builder);
}

llvm::DIScope *IRGenDebugInfo::getEntryPointFn() {
  return static_cast<IRGenDebugInfoImpl *>(this)->getEntryPointFn();
}

llvm::DIScope *IRGenDebugInfo::getOrCreateScope(const SILDebugScope *DS) {
  return static_cast<IRGenDebugInfoImpl *>(this)->getOrCreateScope(DS);
}

void IRGenDebugInfo::emitImport(ImportDecl *D) {
  static_cast<IRGenDebugInfoImpl *>(this)->emitImport(D);
}

llvm::DISubprogram *
IRGenDebugInfo::emitFunction(const SILDebugScope *DS, llvm::Function *Fn,
                             SILFunctionTypeRepresentation Rep, SILType Ty,
                             DeclContext *DeclCtx,
                             GenericEnvironment *GE) {
  return static_cast<IRGenDebugInfoImpl *>(this)->emitFunction(DS, Fn, Rep, Ty,
                                                        DeclCtx);
}

llvm::DISubprogram *IRGenDebugInfo::emitFunction(SILFunction &SILFn,
                                                 llvm::Function *Fn) {
  return static_cast<IRGenDebugInfoImpl *>(this)->emitFunction(SILFn, Fn);
}

void IRGenDebugInfo::emitArtificialFunction(IRBuilder &Builder,
                                            llvm::Function *Fn,
                                            SILType SILTy) {
  static_cast<IRGenDebugInfoImpl *>(this)->emitArtificialFunction(Builder,
                                                                  Fn, SILTy);
}

void IRGenDebugInfo::emitVariableDeclaration(
    IRBuilder &Builder, ArrayRef<llvm::Value *> Storage, DebugTypeInfo Ty,
    const SILDebugScope *DS, ValueDecl *VarDecl, StringRef Name,
    unsigned ArgNo, IndirectionKind Indirection,
      ArtificialKind Artificial) {
  static_cast<IRGenDebugInfoImpl *>(this)->emitVariableDeclaration(
      Builder, Storage, Ty, DS, VarDecl, Name, ArgNo, Indirection, Artificial);
}

void IRGenDebugInfo::emitDbgIntrinsic(IRBuilder &Builder, llvm::Value *Storage,
                                      llvm::DILocalVariable *Var,
                                      llvm::DIExpression *Expr, unsigned Line,
                                      unsigned Col, llvm::DILocalScope *Scope,
                                      const SILDebugScope *DS) {
  static_cast<IRGenDebugInfoImpl *>(this)->emitDbgIntrinsic(
      Builder, Storage, Var, Expr, Line, Col, Scope, DS);
}

void IRGenDebugInfo::emitGlobalVariableDeclaration(
    llvm::GlobalVariable *Storage, StringRef Name, StringRef LinkageName,
    DebugTypeInfo DebugType, bool IsLocalToUnit, bool Indirect,
    Optional<SILLocation> Loc) {
  static_cast<IRGenDebugInfoImpl *>(this)->emitGlobalVariableDeclaration(
      Storage, Name, LinkageName, DebugType, IsLocalToUnit, Indirect, Loc);
}

void IRGenDebugInfo::emitTypeMetadata(IRGenFunction &IGF, llvm::Value *Metadata,
                                      unsigned Depth, unsigned Index,
                                      StringRef AssocType) {
  static_cast<IRGenDebugInfoImpl *>(this)->emitTypeMetadata(
      IGF, Metadata, Depth, Index, AssocType);
}

llvm::DIBuilder &IRGenDebugInfo::getBuilder() {
  return static_cast<IRGenDebugInfoImpl *>(this)->getBuilder();
}

SILLocation::DebugLoc IRGenDebugInfo::decodeSourceLoc(SourceLoc SL) {
  return static_cast<IRGenDebugInfoImpl *>(this)->decodeSourceLoc(SL);
}

AutoRestoreLocation::AutoRestoreLocation(IRGenDebugInfo *DI, IRBuilder &Builder)
    : DI(DI), Builder(Builder) {
  if (DI)
    SavedLocation = Builder.getCurrentDebugLocation();
}

/// Autorestore everything back to normal.
AutoRestoreLocation::~AutoRestoreLocation() {
  if (DI)
    Builder.SetCurrentDebugLocation(SavedLocation);
}

ArtificialLocation::ArtificialLocation(const SILDebugScope *DS,
                                       IRGenDebugInfo *DI, IRBuilder &Builder)
    : AutoRestoreLocation(DI, Builder) {
  if (DI) {
    unsigned Line = 0;
    auto *Scope = DI->getOrCreateScope(DS);
    if (static_cast<IRGenDebugInfoImpl *>(DI)->getDebugInfoFormat() ==
        IRGenDebugInfoFormat::CodeView) {
      // In CodeView, line zero is not an artificial line location and so we
      // try to use the location of the scope.
      if (auto *LB = dyn_cast<llvm::DILexicalBlock>(Scope))
        Line = LB->getLine();
      else if (auto *SP = dyn_cast<llvm::DISubprogram>(Scope))
        Line = SP->getLine();
    }
    auto DL = llvm::DebugLoc::get(Line, 0, Scope);
    Builder.SetCurrentDebugLocation(DL);
  }
}

PrologueLocation::PrologueLocation(IRGenDebugInfo *DI, IRBuilder &Builder)
    : AutoRestoreLocation(DI, Builder) {
  if (DI)
    DI->clearLoc(Builder);
}
