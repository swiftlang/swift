//===--- Index.cpp --------------------------------------------------------===//
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

#include "swift/Index/Index.h"

#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Module.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/SourceEntityWalker.h"
#include "swift/AST/Types.h"
#include "swift/AST/USRGeneration.h"
#include "swift/Basic/SourceManager.h"
#include "swift/Basic/StringExtras.h"
#include "llvm/ADT/APInt.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/FileSystem.h"

using namespace swift;
using namespace swift::index;

static bool
printArtificialName(const swift::AbstractStorageDecl *ASD, AccessorKind AK, llvm::raw_ostream &OS) {
  switch (AK) {
  case AccessorKind::IsGetter:
    OS << "getter:" << ASD->getFullName();
    return false;
  case AccessorKind::IsSetter:
    OS << "setter:" << ASD->getFullName();
    return false;
  case AccessorKind::IsDidSet:
    OS << "didSet:" << ASD->getFullName();
    return false;
  case AccessorKind::IsWillSet:
    OS << "willSet:" << ASD->getFullName() ;
    return false;

  case AccessorKind::NotAccessor:
  case AccessorKind::IsMaterializeForSet:
  case AccessorKind::IsAddressor:
  case AccessorKind::IsMutableAddressor:
    return true;
  }

  llvm_unreachable("Unhandled AccessorKind in switch.");
}

static bool printDisplayName(const swift::ValueDecl *D, llvm::raw_ostream &OS) {
  if (!D->hasName() && !isa<ParamDecl>(D)) {
    auto *FD = dyn_cast<FuncDecl>(D);
    if (!FD || FD->getAccessorKind() == AccessorKind::NotAccessor)
      return true;
    return printArtificialName(FD->getAccessorStorageDecl(), FD->getAccessorKind(), OS);
  }

  OS << D->getFullName();
  return false;
}

namespace {
// Adapter providing a common interface for a SourceFile/Module.
class SourceFileOrModule {
  llvm::PointerUnion<SourceFile *, ModuleDecl *> SFOrMod;

public:
  SourceFileOrModule(SourceFile &SF) : SFOrMod(&SF) {}
  SourceFileOrModule(ModuleDecl &Mod) : SFOrMod(&Mod) {}

  SourceFile *getAsSourceFile() const {
    return SFOrMod.dyn_cast<SourceFile *>();
  }

  ModuleDecl *getAsModule() const { return SFOrMod.dyn_cast<ModuleDecl *>(); }

  ModuleDecl &getModule() const {
    if (auto SF = SFOrMod.dyn_cast<SourceFile *>())
      return *SF->getParentModule();
    return *SFOrMod.get<ModuleDecl *>();
  }

  ArrayRef<FileUnit *> getFiles() const {
    return SFOrMod.is<SourceFile *>() ? *SFOrMod.getAddrOfPtr1()
                                      : SFOrMod.get<ModuleDecl *>()->getFiles();
  }

  StringRef getFilename() const {
    if (SourceFile *SF = SFOrMod.dyn_cast<SourceFile *>())
      return SF->getFilename();
    return SFOrMod.get<ModuleDecl *>()->getModuleFilename();
  }

  void
  getImportedModules(SmallVectorImpl<ModuleDecl::ImportedModule> &Modules) const {
    if (SourceFile *SF = SFOrMod.dyn_cast<SourceFile *>()) {
      SF->getImportedModules(Modules, ModuleDecl::ImportFilter::All);
    } else {
      SFOrMod.get<ModuleDecl *>()->getImportedModules(Modules,
                                                  ModuleDecl::ImportFilter::All);
    }
  }
};

class IndexSwiftASTWalker : public SourceEntityWalker {
  IndexDataConsumer &IdxConsumer;
  SourceManager &SrcMgr;
  unsigned BufferID;
  bool enableWarnings;

  bool IsModuleFile = false;
  bool isSystemModule = false;
  struct Entity {
    Decl *D;
    SymbolInfo SymInfo;
    SymbolRoleSet Roles;
    SmallVector<SourceLoc, 6> RefsToSuppress;
  };
  SmallVector<Entity, 6> EntitiesStack;
  SmallVector<Expr *, 8> ExprStack;
  bool Cancelled = false;

  struct NameAndUSR {
    StringRef USR;
    StringRef name;
  };
  typedef llvm::PointerIntPair<Decl *, 3> DeclAccessorPair;
  llvm::DenseMap<Decl *, NameAndUSR> nameAndUSRCache;
  llvm::DenseMap<DeclAccessorPair, NameAndUSR> accessorNameAndUSRCache;
  StringScratchSpace stringStorage;

  bool getNameAndUSR(ValueDecl *D, ExtensionDecl *ExtD,
                     StringRef &name, StringRef &USR) {
    auto &result = nameAndUSRCache[ExtD ? (Decl*)ExtD : D];
    if (result.USR.empty()) {
      SmallString<128> storage;
      {
        llvm::raw_svector_ostream OS(storage);
        if (ExtD) {
          if (ide::printExtensionUSR(ExtD, OS))
            return true;
        } else {
          if (ide::printDeclUSR(D, OS))
            return true;
        }
        result.USR = stringStorage.copyString(OS.str());
      }

      storage.clear();
      {
        llvm::raw_svector_ostream OS(storage);
        printDisplayName(D, OS);
        result.name = stringStorage.copyString(OS.str());
      }
    }

    name = result.name;
    USR = result.USR;
    return false;
  }

  bool getPseudoAccessorNameAndUSR(AbstractStorageDecl *D, AccessorKind AK, StringRef &Name, StringRef &USR) {
    assert(AK != AccessorKind::NotAccessor);
    assert(static_cast<int>(AK) < 0x111 && "AccessorKind too big for pair");
    DeclAccessorPair key(D, static_cast<int>(AK));
    auto &result = accessorNameAndUSRCache[key];
    if (result.USR.empty()) {
      SmallString<128> storage;
      {
        llvm::raw_svector_ostream OS(storage);
        if(ide::printAccessorUSR(D, AK, OS))
          return true;
        result.USR = stringStorage.copyString(OS.str());
      }

      storage.clear();
      {
        llvm::raw_svector_ostream OS(storage);
        printArtificialName(D, AK, OS);
        result.name = stringStorage.copyString(OS.str());
      }
    }

    Name = result.name;
    USR = result.USR;
    return false;
  }

  bool addRelation(IndexSymbol &Info, SymbolRoleSet RelationRoles, Decl *D) {
    assert(D);
    auto Match = std::find_if(Info.Relations.begin(), Info.Relations.end(),
                              [D](IndexRelation R) { return R.decl == D; });
    if (Match != Info.Relations.end()) {
      Match->roles |= RelationRoles;
      Info.roles |= RelationRoles;
      return false;
    }

    StringRef Name, USR;
    SymbolInfo SymInfo = getSymbolInfoForDecl(D);

    if (SymInfo.Kind == SymbolKind::Unknown)
      return true;
    if (auto *ExtD = dyn_cast<ExtensionDecl>(D)) {
      NominalTypeDecl *NTD = ExtD->getExtendedType()->getAnyNominal();
      if (getNameAndUSR(NTD, ExtD, Name, USR))
        return true;
    } else {
      if (getNameAndUSR(cast<ValueDecl>(D), /*ExtD=*/nullptr, Name, USR))
        return true;
    }

    Info.Relations.push_back(IndexRelation(RelationRoles, D, SymInfo, Name, USR));
    Info.roles |= RelationRoles;
    return false;
  }

public:
  IndexSwiftASTWalker(IndexDataConsumer &IdxConsumer, ASTContext &Ctx,
                      unsigned BufferID)
      : IdxConsumer(IdxConsumer), SrcMgr(Ctx.SourceMgr), BufferID(BufferID),
        enableWarnings(IdxConsumer.enableWarnings()) {}
  ~IndexSwiftASTWalker() override { assert(Cancelled || EntitiesStack.empty()); }

  void visitModule(ModuleDecl &Mod, StringRef Hash);

private:
  bool visitImports(SourceFileOrModule Mod,
                    llvm::SmallPtrSet<ModuleDecl *, 16> &Visited);

  bool handleSourceOrModuleFile(SourceFileOrModule SFOrMod, StringRef KnownHash,
                                bool &HashIsKnown);

  bool walkToDeclPre(Decl *D, CharSourceRange Range) override {
    // Do not handle unavailable decls.
    if (AvailableAttr::isUnavailable(D))
      return false;
    if (FuncDecl *FD = dyn_cast<FuncDecl>(D)) {
      if (FD->isAccessor() && getParentDecl() != FD->getAccessorStorageDecl())
        return false; // already handled as part of the var decl.
    }
    if (ValueDecl *VD = dyn_cast<ValueDecl>(D)) {
      if (!report(VD))
        return false;
      if (SubscriptDecl *SD = dyn_cast<SubscriptDecl>(VD)) {
        // Avoid indexing the indices, only walk the getter/setter.
        if (SD->getGetter())
          if (SourceEntityWalker::walk(cast<Decl>(SD->getGetter())))
            return false;
        if (SD->getSetter())
          if (SourceEntityWalker::walk(cast<Decl>(SD->getSetter())))
            return false;
        if (SD->hasAddressors()) {
          if (auto FD = SD->getAddressor())
            SourceEntityWalker::walk(cast<Decl>(FD));
          if (Cancelled)
            return false;
          if (auto FD = SD->getMutableAddressor())
            SourceEntityWalker::walk(cast<Decl>(FD));
        }
        walkToDeclPost(D);
        return false; // already walked what we needed.
      }
    }
    if (ExtensionDecl *ED = dyn_cast<ExtensionDecl>(D))
      return reportExtension(ED);
    return true;
  }

  bool walkToDeclPost(Decl *D) override {
    if (Cancelled)
      return false;

    if (getParentDecl() == D)
      return finishCurrentEntity();

    return true;
  }

  bool walkToExprPre(Expr *E) override {
    if (Cancelled)
      return false;
    ExprStack.push_back(E);
    return true;
  }

  bool walkToExprPost(Expr *E) override {
    if (Cancelled)
      return false;
    assert(ExprStack.back() == E);
    ExprStack.pop_back();
    return true;
  }

  bool visitDeclReference(ValueDecl *D, CharSourceRange Range,
                          TypeDecl *CtorTyRef, ExtensionDecl *ExtTyRef, Type T,
                          ReferenceMetaData Data) override {
    SourceLoc Loc = Range.getStart();

    if (isRepressed(Loc) || Loc.isInvalid())
      return true;

    IndexSymbol Info;
    if (CtorTyRef)
      if (!reportRef(CtorTyRef, Loc, Info))
        return false;
    if (!reportRef(D, Loc, Info))
      return false;

    return true;
  }

  Decl *getParentDecl() {
    if (!EntitiesStack.empty())
      return EntitiesStack.back().D;
    return nullptr;
  }

  void repressRefAtLoc(SourceLoc Loc) {
    if (Loc.isInvalid()) return;
    assert(!EntitiesStack.empty());
    EntitiesStack.back().RefsToSuppress.push_back(Loc);
  }

  bool isRepressed(SourceLoc Loc) {
    if (EntitiesStack.empty() || Loc.isInvalid())
      return false;
    auto &Suppressed = EntitiesStack.back().RefsToSuppress;
    return std::find(Suppressed.begin(), Suppressed.end(), Loc) != Suppressed.end();

  }

  Expr *getContainingExpr(size_t index) {
    if (ExprStack.size() > index)
      return ExprStack.end()[-(index + 1)];
    return nullptr;
  }

  Expr *getCurrentExpr() {
    return ExprStack.empty() ? nullptr : ExprStack.back();
  }

  Expr *getParentExpr() {
    return getContainingExpr(1);
  }


  bool report(ValueDecl *D);
  bool reportExtension(ExtensionDecl *D);
  bool reportRef(ValueDecl *D, SourceLoc Loc, IndexSymbol &Info);

  bool startEntity(Decl *D, IndexSymbol &Info);
  bool startEntityDecl(ValueDecl *D);

  bool reportRelatedRef(ValueDecl *D, SourceLoc Loc, bool isImplicit, SymbolRoleSet Relations, Decl *Related);
  bool reportRelatedTypeRef(const TypeLoc &Ty, SymbolRoleSet Relations, Decl *Related);
  bool reportInheritedTypeRefs(ArrayRef<TypeLoc> Inherited, Decl *Inheritee);
  NominalTypeDecl *getTypeLocAsNominalTypeDecl(const TypeLoc &Ty);

  bool reportPseudoGetterDecl(VarDecl *D) {
    return reportPseudoAccessor(D, AccessorKind::IsGetter, /*IsRef=*/false,
                                D->getLoc());
  }
  bool reportPseudoSetterDecl(VarDecl *D) {
    return reportPseudoAccessor(D, AccessorKind::IsSetter, /*IsRef=*/false,
                                D->getLoc());
  }
  bool reportPseudoAccessor(AbstractStorageDecl *D, AccessorKind AccKind,
                            bool IsRef, SourceLoc Loc);

  bool finishCurrentEntity() {
    Entity CurrEnt = EntitiesStack.pop_back_val();
    assert(CurrEnt.SymInfo.Kind != SymbolKind::Unknown);
    if (!IdxConsumer.finishSourceEntity(CurrEnt.SymInfo, CurrEnt.Roles)) {
      Cancelled = true;
      return false;
    }
    return true;
  }

  bool initIndexSymbol(ValueDecl *D, SourceLoc Loc, bool IsRef,
                       IndexSymbol &Info);
  bool initIndexSymbol(ExtensionDecl *D, ValueDecl *ExtendedD, SourceLoc Loc,
                       IndexSymbol &Info);
  bool initFuncDeclIndexSymbol(FuncDecl *D, IndexSymbol &Info);
  bool initFuncRefIndexSymbol(ValueDecl *D, SourceLoc Loc, IndexSymbol &Info);
  bool initVarRefIndexSymbols(Expr *CurrentE, ValueDecl *D, SourceLoc Loc,
                              IndexSymbol &Info);

  std::pair<unsigned, unsigned> getLineCol(SourceLoc Loc) {
    if (Loc.isInvalid())
      return std::make_pair(0, 0);
    return SrcMgr.getLineAndColumn(Loc, BufferID);
  }

  bool shouldIndex(ValueDecl *D, bool IsRef) const {
    if (D->isImplicit())
      return false;
    if (isLocalSymbol(D) && (!isa<ParamDecl>(D) || IsRef))
      return false;
    if (D->isPrivateStdlibDecl())
      return false;

    return true;
  }

  void getModuleHash(SourceFileOrModule SFOrMod, llvm::raw_ostream &OS);
  llvm::hash_code hashModule(llvm::hash_code code, SourceFileOrModule SFOrMod);
  llvm::hash_code hashFileReference(llvm::hash_code code,
                                    SourceFileOrModule SFOrMod);
  void getRecursiveModuleImports(ModuleDecl &Mod,
                                 SmallVectorImpl<ModuleDecl *> &Imports);
  void collectRecursiveModuleImports(ModuleDecl &Mod,
                                     llvm::SmallPtrSet<ModuleDecl *, 16> &Visited);

  template <typename F>
  void warn(F log) {
    if (!enableWarnings)
      return;

    SmallString<128> warning;
    llvm::raw_svector_ostream OS(warning);
    log(OS);
  }

  // This maps a module to all its imports, recursively.
  llvm::DenseMap<ModuleDecl *, llvm::SmallVector<ModuleDecl *, 4>> ImportsMap;
};
} // anonymous namespace

void IndexSwiftASTWalker::visitModule(ModuleDecl &Mod, StringRef KnownHash) {
  SourceFile *SrcFile = nullptr;
  for (auto File : Mod.getFiles()) {
    if (auto SF = dyn_cast<SourceFile>(File)) {
      auto BufID = SF->getBufferID();
      if (BufID.hasValue() && *BufID == BufferID) {
        SrcFile = SF;
        break;
      }
    }
  }

  bool HashIsKnown;
  if (SrcFile != nullptr) {
    IsModuleFile = false;
    if (!handleSourceOrModuleFile(*SrcFile, KnownHash, HashIsKnown))
      return;
    if (HashIsKnown)
      return; // No need to report symbols.
    walk(*SrcFile);
  } else {
    IsModuleFile = true;
    isSystemModule = Mod.isSystemModule();
    if (!handleSourceOrModuleFile(Mod, KnownHash, HashIsKnown))
      return;
    if (HashIsKnown)
      return; // No need to report symbols.
    walk(Mod);
  }
}

bool IndexSwiftASTWalker::handleSourceOrModuleFile(SourceFileOrModule SFOrMod,
                                                   StringRef KnownHash,
                                                   bool &HashIsKnown) {
  // Common reporting for TU/module file.

  SmallString<32> HashBuf;
  {
    llvm::raw_svector_ostream HashOS(HashBuf);
    getModuleHash(SFOrMod, HashOS);
    StringRef Hash = HashOS.str();
    HashIsKnown = Hash == KnownHash;
    if (!IdxConsumer.recordHash(Hash, HashIsKnown))
      return false;
  }

  // We always report the dependencies, even if the hash is known.
  llvm::SmallPtrSet<ModuleDecl *, 16> Visited;
  return visitImports(SFOrMod, Visited);
}

bool IndexSwiftASTWalker::visitImports(
    SourceFileOrModule TopMod, llvm::SmallPtrSet<ModuleDecl *, 16> &Visited) {
  // Dependencies of the stdlib module (like SwiftShims module) are
  // implementation details.
  if (TopMod.getModule().isStdlibModule())
    return true;

  bool IsNew = Visited.insert(&TopMod.getModule()).second;
  if (!IsNew)
    return true;

  SmallVector<ModuleDecl::ImportedModule, 8> Imports;
  TopMod.getImportedModules(Imports);

  llvm::SmallPtrSet<ModuleDecl *, 8> Reported;
  for (auto Import : Imports) {
    ModuleDecl *Mod = Import.second;
    bool NewReport = Reported.insert(Mod).second;
    if (!NewReport)
      continue;

    // FIXME: Handle modules with multiple source files; these will fail on
    // getModuleFilename() (by returning an empty path). Note that such modules
    // may be heterogeneous.
    StringRef Path = Mod->getModuleFilename();
    if (Path.empty() || Path == TopMod.getFilename())
      continue; // this is a submodule.

    Optional<bool> IsClangModuleOpt;
    for (auto File : Mod->getFiles()) {
      switch (File->getKind()) {
      case FileUnitKind::Source:
      case FileUnitKind::Builtin:
      case FileUnitKind::Derived:
        break;
      case FileUnitKind::SerializedAST:
        assert(!IsClangModuleOpt.hasValue() &&
               "cannot handle multi-file modules");
        IsClangModuleOpt = false;
        break;
      case FileUnitKind::ClangModule:
        assert(!IsClangModuleOpt.hasValue() &&
               "cannot handle multi-file modules");
        IsClangModuleOpt = true;
        break;
      }
    }
    if (!IsClangModuleOpt.hasValue())
      continue;
    bool IsClangModule = *IsClangModuleOpt;

    StringRef Hash;
    SmallString<32> HashBuf;
    if (!IsClangModule) {
      llvm::raw_svector_ostream HashOS(HashBuf);
      getModuleHash(*Mod, HashOS);
      Hash = HashOS.str();
    }

    if (!IdxConsumer.startDependency(Mod->getName().str(), Path, IsClangModule,
                                     Mod->isSystemModule(), Hash))
      return false;
    if (!IsClangModule)
      if (!visitImports(*Mod, Visited))
        return false;
    if (!IdxConsumer.finishDependency(IsClangModule))
      return false;
  }

  return true;
}

bool IndexSwiftASTWalker::startEntity(Decl *D, IndexSymbol &Info) {
  switch (IdxConsumer.startSourceEntity(Info)) {
    case swift::index::IndexDataConsumer::Abort:
      Cancelled = true;
      LLVM_FALLTHROUGH;
    case swift::index::IndexDataConsumer::Skip:
      return false;
    case swift::index::IndexDataConsumer::Continue:
      EntitiesStack.push_back({D, Info.symInfo, Info.roles, {}});
      return true;
  }

  llvm_unreachable("Unhandled IndexDataConsumer in switch.");
}

bool IndexSwiftASTWalker::startEntityDecl(ValueDecl *D) {
  if (!shouldIndex(D, /*IsRef=*/false))
    return false;

  SourceLoc Loc = D->getLoc();
  if (Loc.isInvalid() && !IsModuleFile)
    return false;

  IndexSymbol Info;
  if (auto FD = dyn_cast<FuncDecl>(D)) {
    if (initFuncDeclIndexSymbol(FD, Info))
      return false;
  } else {
    if (initIndexSymbol(D, Loc, /*IsRef=*/false, Info))
      return false;
  }

  if (auto Overridden = D->getOverriddenDecl()) {
    if (addRelation(Info, (SymbolRoleSet) SymbolRole::RelationOverrideOf, Overridden))
      return false;
  }
  // FIXME: This is quite expensive and not worth the cost for indexing purposes
  // of system modules. Revisit if this becomes more efficient.
  if (!isSystemModule) {
    for (auto Conf : D->getSatisfiedProtocolRequirements()) {
      if (addRelation(Info, (SymbolRoleSet) SymbolRole::RelationOverrideOf, Conf))
        return false;
    }
  }

  if (auto Parent = getParentDecl()) {
    if (auto ParentVD = dyn_cast<ValueDecl>(Parent)) {
      SymbolRoleSet RelationsToParent = (SymbolRoleSet)SymbolRole::RelationChildOf;
      if (Info.symInfo.SubKind == SymbolSubKind::AccessorGetter ||
          Info.symInfo.SubKind == SymbolSubKind::AccessorSetter ||
          (Info.symInfo.SubKind >= SymbolSubKind::SwiftAccessorWillSet &&
           Info.symInfo.SubKind <= SymbolSubKind::SwiftAccessorMutableAddressor))
        RelationsToParent |= (SymbolRoleSet)SymbolRole::RelationAccessorOf;
      if (addRelation(Info, RelationsToParent, ParentVD))
        return false;

    } else if (auto ParentED = dyn_cast<ExtensionDecl>(Parent)) {
      if (ParentED->getExtendedType()->getAnyNominal()) {
        if (addRelation(Info, (SymbolRoleSet) SymbolRole::RelationChildOf, ParentED))
          return false;
      }
    }
  }

  return startEntity(D, Info);
}

bool IndexSwiftASTWalker::reportRelatedRef(ValueDecl *D, SourceLoc Loc, bool isImplicit,
                                           SymbolRoleSet Relations, Decl *Related) {
  if (!shouldIndex(D, /*IsRef=*/true))
    return true;

  IndexSymbol Info;
  if (addRelation(Info, Relations, Related))
    return true;
  if (isImplicit)
    Info.roles |= (unsigned)SymbolRole::Implicit;

  // don't report this ref again when visitDeclReference reports it
  repressRefAtLoc(Loc);

  if(!reportRef(D, Loc, Info)) {
    Cancelled = true;
    return false;
  }

  return !Cancelled;
}

bool IndexSwiftASTWalker::reportInheritedTypeRefs(ArrayRef<TypeLoc> Inherited, Decl *Inheritee) {
  for (auto Base : Inherited) {
    if(!reportRelatedTypeRef(Base, (SymbolRoleSet) SymbolRole::RelationBaseOf, Inheritee))
      return false;
  }
  return true;
}

bool IndexSwiftASTWalker::reportRelatedTypeRef(const TypeLoc &Ty, SymbolRoleSet Relations, Decl *Related) {

  if (IdentTypeRepr *T = dyn_cast_or_null<IdentTypeRepr>(Ty.getTypeRepr())) {
    auto Comps = T->getComponentRange();
    SourceLoc IdLoc = Comps.back()->getIdLoc();
    NominalTypeDecl *NTD = nullptr;
    bool isImplicit = false;
    if (auto *VD = Comps.back()->getBoundDecl()) {
      if (auto *TAD = dyn_cast<TypeAliasDecl>(VD)) {
        IndexSymbol Info;
        if (!reportRef(TAD, IdLoc, Info))
          return false;
        if (auto Ty = TAD->getUnderlyingTypeLoc().getType()) {
          NTD = Ty->getAnyNominal();
          isImplicit = true;
        }
      } else {
        NTD = dyn_cast<NominalTypeDecl>(VD);
      }
    }
    if (NTD) {
      if (!reportRelatedRef(NTD, IdLoc, isImplicit, Relations, Related))
        return false;
    }
    return true;
  }

  if (Ty.getType()) {
    if (auto nominal = Ty.getType()->getAnyNominal())
      if (!reportRelatedRef(nominal, Ty.getLoc(), /*isImplicit=*/false, Relations, Related))
        return false;
  }
  return true;
}

bool IndexSwiftASTWalker::reportPseudoAccessor(AbstractStorageDecl *D,
                                               AccessorKind AccKind, bool IsRef,
                                               SourceLoc Loc) {
  if (!shouldIndex(D, IsRef))
    return true; // continue walking.

  auto updateInfo = [this, D, AccKind](IndexSymbol &Info) {
    if (getPseudoAccessorNameAndUSR(D, AccKind, Info.name, Info.USR))
      return true;
    Info.symInfo.Kind = SymbolKind::Function;
    Info.symInfo.SubKind = getSubKindForAccessor(AccKind);
    Info.roles |= (SymbolRoleSet)SymbolRole::Implicit;
    Info.group = "";
    return false;
  };

  if (IsRef) {
    IndexSymbol Info;

    // initFuncRefIndexSymbol uses the top of the entities stack as the caller,
    // but in this case the top of the stack is the referenced
    // AbstractStorageDecl.
    assert(getParentDecl() == D);
    auto PreviousTop = EntitiesStack.pop_back_val();
    bool initFailed = initFuncRefIndexSymbol(D, Loc, Info);
    EntitiesStack.push_back(PreviousTop);

    if (initFailed)
      return true; // continue walking.
    if (updateInfo(Info))
      return true;

    if (!IdxConsumer.startSourceEntity(Info) || !IdxConsumer.finishSourceEntity(Info.symInfo, Info.roles))
      Cancelled = true;
  } else {
    IndexSymbol Info;
    if (initIndexSymbol(D, Loc, IsRef, Info))
      return true; // continue walking.
    if (updateInfo(Info))
      return true;
    if (addRelation(Info, (SymbolRoleSet) SymbolRole::RelationAccessorOf, D))
      return true;

    if (!IdxConsumer.startSourceEntity(Info) || !IdxConsumer.finishSourceEntity(Info.symInfo, Info.roles))
      Cancelled = true;
  }
  return !Cancelled;
}

NominalTypeDecl *
IndexSwiftASTWalker::getTypeLocAsNominalTypeDecl(const TypeLoc &Ty) {
  if (Type T = Ty.getType())
    return T->getAnyNominal();
  if (IdentTypeRepr *T = dyn_cast_or_null<IdentTypeRepr>(Ty.getTypeRepr())) {
    auto Comp = T->getComponentRange().back();
    if (auto NTD = dyn_cast_or_null<NominalTypeDecl>(Comp->getBoundDecl()))
      return NTD;
  }
  return nullptr;
}

bool IndexSwiftASTWalker::reportExtension(ExtensionDecl *D) {
  // Use the 'End' token of the range, in case it is a compound name, e.g.
  //   extension A.B {}
  // we want the location of 'B' token.
  SourceLoc Loc = D->getExtendedTypeLoc().getSourceRange().End;
  if (!D->getExtendedType())
    return true;
  NominalTypeDecl *NTD = D->getExtendedType()->getAnyNominal();
  if (!NTD)
    return true;
  if (!shouldIndex(NTD, /*IsRef=*/false))
    return true;

  IndexSymbol Info;
  if (initIndexSymbol(D, NTD, Loc, Info))
    return true;

  if (!startEntity(D, Info))
    return false;

  if (!reportRelatedRef(NTD, Loc, /*isImplicit=*/false,
                        (SymbolRoleSet)SymbolRole::RelationExtendedBy, D))
      return false;
  if (!reportInheritedTypeRefs(D->getInherited(), D))
      return false;

  return true;
}

bool IndexSwiftASTWalker::report(ValueDecl *D) {
  if (startEntityDecl(D)) {
    // Pass accessors.
    if (auto VarD = dyn_cast<VarDecl>(D)) {
      if (!VarD->getGetter() && !VarD->getSetter()) {
        // No actual getter or setter, pass 'pseudo' accessors.
        // We create accessor entities so we can implement the functionality
        // of libclang, which reports implicit method property accessor
        // declarations, invocations, and overrides for properties.
        // Note that an ObjC class subclassing from a Swift class, may still
        // be able to override its non-computed-property-accessors via a
        // method.
        if (!reportPseudoGetterDecl(VarD))
          return false;
        if (!reportPseudoSetterDecl(VarD))
          return false;
      } else {
        if (auto FD = VarD->getGetter())
          SourceEntityWalker::walk(cast<Decl>(FD));
        if (Cancelled)
          return false;
        if (auto FD = VarD->getSetter())
          SourceEntityWalker::walk(cast<Decl>(FD));
        if (Cancelled)
          return false;
        if (VarD->hasObservers()) {
          if (auto FD = VarD->getWillSetFunc())
            SourceEntityWalker::walk(cast<Decl>(FD));
          if (Cancelled)
            return false;
          if (auto FD = VarD->getDidSetFunc())
            SourceEntityWalker::walk(cast<Decl>(FD));
          if (Cancelled)
            return false;
        }
        if (VarD->hasAddressors()) {
          if (auto FD = VarD->getAddressor())
            SourceEntityWalker::walk(cast<Decl>(FD));
          if (Cancelled)
            return false;
          if (auto FD = VarD->getMutableAddressor())
            SourceEntityWalker::walk(cast<Decl>(FD));
        }
      }
    } else if (auto NTD = dyn_cast<NominalTypeDecl>(D)) {
      if (!reportInheritedTypeRefs(NTD->getInherited(), NTD))
        return false;
    }
  }

  return !Cancelled;
}

static bool hasUsefulRoleInSystemModule(SymbolRoleSet roles) {
  return roles & ((SymbolRoleSet)SymbolRole::Definition |
  (SymbolRoleSet)SymbolRole::Declaration |
  (SymbolRoleSet)SymbolRole::RelationChildOf |
  (SymbolRoleSet)SymbolRole::RelationBaseOf |
  (SymbolRoleSet)SymbolRole::RelationOverrideOf |
  (SymbolRoleSet)SymbolRole::RelationExtendedBy |
  (SymbolRoleSet)SymbolRole::RelationAccessorOf |
  (SymbolRoleSet)SymbolRole::RelationIBTypeOf);
}

bool IndexSwiftASTWalker::reportRef(ValueDecl *D, SourceLoc Loc,
                                    IndexSymbol &Info) {
  if (!shouldIndex(D, /*IsRef=*/true))
    return true; // keep walking

  if (isa<AbstractFunctionDecl>(D)) {
    if (initFuncRefIndexSymbol(D, Loc, Info))
      return true;
  } else if (isa<AbstractStorageDecl>(D)) {
    if (initVarRefIndexSymbols(getCurrentExpr(), D, Loc, Info))
      return true;
  } else {
    if (initIndexSymbol(D, Loc, /*IsRef=*/true, Info))
      return true;
  }

  if (isSystemModule && !hasUsefulRoleInSystemModule(Info.roles))
    return true;

  if (!startEntity(D, Info))
    return true;

  // Report the accessors that were utilized.
  if (AbstractStorageDecl *ASD = dyn_cast<AbstractStorageDecl>(D)) {
    bool UsesGetter = Info.roles & (SymbolRoleSet)SymbolRole::Read;
    bool UsesSetter = Info.roles & (SymbolRoleSet)SymbolRole::Write;

    if (UsesGetter)
      if (!reportPseudoAccessor(ASD, AccessorKind::IsGetter, /*IsRef=*/true,
                                Loc))
        return false;
    if (UsesSetter)
      if (!reportPseudoAccessor(ASD, AccessorKind::IsSetter, /*IsRef=*/true,
                                Loc))
        return false;
  }

  return finishCurrentEntity();
}

bool IndexSwiftASTWalker::initIndexSymbol(ValueDecl *D, SourceLoc Loc,
                                          bool IsRef, IndexSymbol &Info) {
  assert(D);
  Info.decl = D;
  Info.symInfo = getSymbolInfoForDecl(D);
  if (Info.symInfo.Kind == SymbolKind::Unknown)
    return true;

  // Cannot be extension, which is not a ValueDecl.

  if (IsRef) {
    Info.roles |= (unsigned)SymbolRole::Reference;
    auto Parent = getParentDecl();
    if (Parent && isa<AbstractFunctionDecl>(Parent))
      addRelation(Info, (unsigned)SymbolRole::RelationContainedBy, Parent);
  } else {
    Info.roles |= (unsigned)SymbolRole::Definition;
  }

  if (getNameAndUSR(D, /*ExtD=*/nullptr, Info.name, Info.USR))
    return true;

  std::tie(Info.line, Info.column) = getLineCol(Loc);
  if (!IsRef) {
    if (auto Group = D->getGroupName())
      Info.group = Group.getValue();
  }
  return false;
}

bool IndexSwiftASTWalker::initIndexSymbol(ExtensionDecl *ExtD, ValueDecl *ExtendedD,
                                          SourceLoc Loc, IndexSymbol &Info) {
  assert(ExtD && ExtendedD);
  Info.decl = ExtendedD;
  Info.symInfo = getSymbolInfoForDecl(ExtD);
  if (Info.symInfo.Kind == SymbolKind::Unknown)
    return true;

  Info.roles |= (unsigned)SymbolRole::Definition;

  if (getNameAndUSR(ExtendedD, ExtD, Info.name, Info.USR))
    return true;

  std::tie(Info.line, Info.column) = getLineCol(Loc);
  if (auto Group = ExtD->getGroupName())
    Info.group = Group.getValue();
  return false;
}

static NominalTypeDecl *getNominalParent(ValueDecl *D) {
  Type Ty = D->getDeclContext()->getDeclaredTypeOfContext();
  if (!Ty)
    return nullptr;
  return Ty->getAnyNominal();
}

bool IndexSwiftASTWalker::initFuncDeclIndexSymbol(FuncDecl *D,
                                                  IndexSymbol &Info) {
  if (initIndexSymbol(D, D->getLoc(), /*IsRef=*/false, Info))
    return true;

  if (D->getAttrs().hasAttribute<IBActionAttr>()) {
    // Relate with type of the first parameter using RelationIBTypeOf.
    if (D->getParameterLists().size() >= 2) {
      auto paramList = D->getParameterList(1);
      if (!paramList->getArray().empty()) {
        auto param = paramList->get(0);
        if (auto nominal = param->getType()->getAnyNominal()) {
          addRelation(Info, (SymbolRoleSet) SymbolRole::RelationIBTypeOf, nominal);
        }
      }
    }
  }

  if (auto Group = D->getGroupName())
    Info.group = Group.getValue();
  return false;
}

static bool isSuperRefExpr(Expr *E) {
  if (!E)
    return false;
  if (isa<SuperRefExpr>(E))
    return true;
  if (auto LoadE = dyn_cast<LoadExpr>(E))
    return isSuperRefExpr(LoadE->getSubExpr());
  return false;
}

static bool isDynamicCall(Expr *BaseE, ValueDecl *D) {
  // The call is 'dynamic' if the method is not of a struct and the
  // receiver is not 'super'. Note that if the receiver is 'super' that
  // does not mean that the call is statically determined (an extension
  // method may have injected itself in the super hierarchy).
  // For our purposes 'dynamic' means that the method call cannot invoke
  // a method in a subclass.
  auto TyD = getNominalParent(D);
  if (!TyD)
    return false;
  if (isa<StructDecl>(TyD))
    return false;
  if (isSuperRefExpr(BaseE))
    return false;
  if (BaseE->getType()->is<MetatypeType>())
    return false;

  return true;
}

static bool isBeingCalled(Expr *Target, Expr *Parent, Expr *GrandParent) {
    if (!Target || !Parent || !isa<ApplyExpr>(Parent))
      return false;

    if (!isa<SelfApplyExpr>(Parent))
      return cast<ApplyExpr>(Parent)->getFn() == Target;

  return GrandParent && isa<CallExpr>(GrandParent) &&
    cast<CallExpr>(GrandParent)->getFn() == Parent;
}

bool IndexSwiftASTWalker::initFuncRefIndexSymbol(ValueDecl *D, SourceLoc Loc,
                                                 IndexSymbol &Info) {

  if (initIndexSymbol(D, Loc, /*IsRef=*/true, Info))
    return true;

  Expr *CurrentE = getCurrentExpr();
  if (!CurrentE)
    return false;

  Expr *ParentE = getParentExpr();

  // FIXME: the below check maintains existing indexing behavior with
  // pseudo/accessor output but seems incorrect. E.g otherGlobal in:
  // let global = otherGlobal
  // will not have a parent expression so no accessor call is reported
  if (isa<AbstractStorageDecl>(D) && !ParentE)
    return true;

  if (!isa<AbstractStorageDecl>(D) &&
      !isBeingCalled(CurrentE, ParentE, getContainingExpr(2)))
    return false;


  Info.roles |= (unsigned)SymbolRole::Call;
  if (auto *Caller = dyn_cast_or_null<AbstractFunctionDecl>(getParentDecl())) {
    if (addRelation(Info, (SymbolRoleSet) SymbolRole::RelationCalledBy, Caller))
      return true;
  }

  Expr *BaseE = nullptr;
  if (auto DotE = dyn_cast_or_null<DotSyntaxCallExpr>(ParentE))
    BaseE = DotE->getBase();
  else if (auto MembE = dyn_cast<MemberRefExpr>(CurrentE))
    BaseE = MembE->getBase();
  else if (auto SubsE = dyn_cast<SubscriptExpr>(CurrentE))
    BaseE = SubsE->getBase();

  if (!BaseE || BaseE == CurrentE)
    return false;

  if (Type ReceiverTy = BaseE->getType()) {
    if (auto LVT = ReceiverTy->getAs<LValueType>())
      ReceiverTy = LVT->getObjectType();
    else if (auto MetaT = ReceiverTy->getAs<MetatypeType>())
      ReceiverTy = MetaT->getInstanceType();

    if (auto TyD = ReceiverTy->getAnyNominal()) {
      StringRef unused;
      if (addRelation(Info, (SymbolRoleSet) SymbolRole::RelationReceivedBy, TyD))
        return true;
      if (isDynamicCall(BaseE, D))
        Info.roles |= (unsigned)SymbolRole::Dynamic;
    }
  }

  return false;
}

bool IndexSwiftASTWalker::initVarRefIndexSymbols(Expr *CurrentE, ValueDecl *D, SourceLoc Loc, IndexSymbol &Info) {

  if (initIndexSymbol(D, Loc, /*IsRef=*/true, Info))
    return true;

  if (!CurrentE)
    return false;

  AccessKind Kind = CurrentE->hasLValueAccessKind() ? CurrentE->getLValueAccessKind() : AccessKind::Read;
  switch (Kind) {
  case swift::AccessKind::Read:
    Info.roles |= (unsigned)SymbolRole::Read;
    break;
  case swift::AccessKind::ReadWrite:
    Info.roles |= (unsigned)SymbolRole::Read;
    LLVM_FALLTHROUGH;
  case swift::AccessKind::Write:
    Info.roles |= (unsigned)SymbolRole::Write;
  }
  
  return false;
}

llvm::hash_code
IndexSwiftASTWalker::hashFileReference(llvm::hash_code code,
                                       SourceFileOrModule SFOrMod) {
  StringRef Filename = SFOrMod.getFilename();
  if (Filename.empty())
    return code;

  // FIXME: FileManager for swift ?

  llvm::sys::fs::file_status Status;
  if (std::error_code Ret = llvm::sys::fs::status(Filename, Status)) {
    // Failure to read the file, just use filename to recover.
    warn([&](llvm::raw_ostream &OS) {
      OS << "failed to stat file: " << Filename << " (" << Ret.message() << ')';
    });
    return hash_combine(code, Filename);
  }

  // Don't use inode because it can easily change when you update the repository
  // even though the file is supposed to be the same (same size/time).
  code = hash_combine(code, Filename);
  auto mtime = Status.getLastModificationTime().time_since_epoch().count();
  return hash_combine(code, Status.getSize(), mtime);
}

llvm::hash_code IndexSwiftASTWalker::hashModule(llvm::hash_code code,
                                                SourceFileOrModule SFOrMod) {
  code = hashFileReference(code, SFOrMod);

  SmallVector<ModuleDecl *, 16> Imports;
  getRecursiveModuleImports(SFOrMod.getModule(), Imports);
  for (auto Import : Imports)
    code = hashFileReference(code, *Import);

  return code;
}

void IndexSwiftASTWalker::getRecursiveModuleImports(
    ModuleDecl &Mod, SmallVectorImpl<ModuleDecl *> &Imports) {
  auto It = ImportsMap.find(&Mod);
  if (It != ImportsMap.end()) {
    Imports.append(It->second.begin(), It->second.end());
    return;
  }

  llvm::SmallPtrSet<ModuleDecl *, 16> Visited;
  collectRecursiveModuleImports(Mod, Visited);
  Visited.erase(&Mod);

  warn([&Imports](llvm::raw_ostream &OS) {
    std::for_each(Imports.begin(), Imports.end(), [&OS](ModuleDecl *M) {
      if (M->getModuleFilename().empty()) {
        std::string Info = "swift::ModuleDecl with empty file name!! \nDetails: \n";
        Info += "  name: ";
        Info += M->getName().get();
        Info += "\n";

        auto Files = M->getFiles();
        std::for_each(Files.begin(), Files.end(), [&](FileUnit *FU) {
          Info += "  file unit: ";

          switch (FU->getKind()) {
          case FileUnitKind::Builtin:
            Info += "builtin";
            break;
          case FileUnitKind::Derived:
            Info += "derived";
            break;
          case FileUnitKind::Source:
            Info += "source, file=\"";
            Info += cast<SourceFile>(FU)->getFilename();
            Info += "\"";
            break;
          case FileUnitKind::SerializedAST:
            Info += "serialized ast, file=\"";
            Info += cast<LoadedFile>(FU)->getFilename();
            Info += "\"";
            break;
          case FileUnitKind::ClangModule:
            Info += "clang module, file=\"";
            Info += cast<LoadedFile>(FU)->getFilename();
            Info += "\"";
          }

          Info += "\n";
        });

        OS << "swift::ModuleDecl with empty file name! " << Info << "\n";
      }
    });
  });

  Imports.append(Visited.begin(), Visited.end());
  std::sort(Imports.begin(), Imports.end(), [](ModuleDecl *LHS, ModuleDecl *RHS) {
    return LHS->getModuleFilename() < RHS->getModuleFilename();
  });

  // Cache it.
  ImportsMap[&Mod].append(Imports.begin(), Imports.end());
}

void IndexSwiftASTWalker::collectRecursiveModuleImports(
    ModuleDecl &TopMod, llvm::SmallPtrSet<ModuleDecl *, 16> &Visited) {

  bool IsNew = Visited.insert(&TopMod).second;
  if (!IsNew)
    return;

  // Pure Clang modules are tied to their dependencies, no need to look into its
  // imports.
  // FIXME: What happens if the clang module imports a swift module ? So far
  // the assumption is that the path to the swift module will be fixed, so no
  // need to hash the clang module.
  // FIXME: This is a bit of a hack.
  if (TopMod.getFiles().size() == 1)
    if (TopMod.getFiles().front()->getKind() == FileUnitKind::ClangModule)
      return;

  auto It = ImportsMap.find(&TopMod);
  if (It != ImportsMap.end()) {
    Visited.insert(It->second.begin(), It->second.end());
    return;
  }

  SmallVector<ModuleDecl::ImportedModule, 8> Imports;
  TopMod.getImportedModules(Imports, ModuleDecl::ImportFilter::All);

  for (auto Import : Imports) {
    collectRecursiveModuleImports(*Import.second, Visited);
  }
}

void IndexSwiftASTWalker::getModuleHash(SourceFileOrModule Mod,
                                        llvm::raw_ostream &OS) {
  // FIXME: Use a longer hash string to minimize possibility for conflicts.
  llvm::hash_code code = hashModule(0, Mod);
  OS << llvm::APInt(64, code).toString(36, /*Signed=*/false);
}

//===----------------------------------------------------------------------===//
// Indexing entry points
//===----------------------------------------------------------------------===//

void index::indexSourceFile(SourceFile *SF, StringRef hash,
                            IndexDataConsumer &consumer) {
  assert(SF);
  unsigned bufferID = SF->getBufferID().getValue();
  IndexSwiftASTWalker walker(consumer, SF->getASTContext(), bufferID);
  walker.visitModule(*SF->getParentModule(), hash);
  consumer.finish();
}

void index::indexModule(ModuleDecl *module, StringRef hash,
                        IndexDataConsumer &consumer) {
  assert(module);
  IndexSwiftASTWalker walker(consumer, module->getASTContext(),
                             /*bufferID*/ -1);
  walker.visitModule(*module, hash);
  consumer.finish();
}
