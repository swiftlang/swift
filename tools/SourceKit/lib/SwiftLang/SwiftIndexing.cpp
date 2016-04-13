//===--- SwiftIndexing.cpp ------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "SwiftASTManager.h"
#include "SwiftLangSupport.h"
#include "SourceKit/Support/Logging.h"
#include "SourceKit/Support/Tracing.h"
#include "SourceKit/Support/UIdent.h"

#include "swift/AST/SourceEntityWalker.h"
#include "swift/Basic/SourceManager.h"
#include "swift/Frontend/Frontend.h"
#include "swift/Frontend/PrintingDiagnosticConsumer.h"
#include "swift/Serialization/SerializedModuleLoader.h"
// This is included only for createLazyResolver(). Move to different header ?
#include "swift/Sema/IDETypeChecking.h"

#include "llvm/ADT/Hashing.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/raw_ostream.h"

using namespace SourceKit;
using namespace swift;

static UIdent KindImportModuleClang("source.lang.swift.import.module.clang");
static UIdent KindImportModuleSwift("source.lang.swift.import.module.swift");
static UIdent KindImportSourceFile("source.lang.swift.import.sourcefile");

namespace {

// Adapter providing a common interface for a SourceFile/Module.
class SourceFileOrModule {
  llvm::PointerUnion<SourceFile *, Module *> SFOrMod;

public:
  SourceFileOrModule(SourceFile &SF) : SFOrMod(&SF) { }
  SourceFileOrModule(Module &Mod) : SFOrMod(&Mod) { }

  SourceFile *getAsSourceFile() const {
    return SFOrMod.dyn_cast<SourceFile*>();
  }

  Module *getAsModule() const {
    return SFOrMod.dyn_cast<Module*>();
  }

  Module &getModule() const {
    if (auto SF = SFOrMod.dyn_cast<SourceFile*>())
      return *SF->getParentModule();
    return *SFOrMod.get<Module*>();
  }
  
  ArrayRef<FileUnit *> getFiles() const {
    return SFOrMod.is<SourceFile*>() ? *SFOrMod.getAddrOfPtr1()
                                     : SFOrMod.get<Module*>()->getFiles();
  }

  StringRef getFilename() const {
    if (SourceFile *SF = SFOrMod.dyn_cast<SourceFile*>())
      return SF->getFilename();
    return SFOrMod.get<Module*>()->getModuleFilename();
  }

  void getImportedModules(
      SmallVectorImpl<Module::ImportedModule> &Modules) const {
    if (SourceFile *SF = SFOrMod.dyn_cast<SourceFile*>()) {
      SF->getImportedModules(Modules, Module::ImportFilter::All);
    } else {
      SFOrMod.get<Module*>()->getImportedModules(Modules,
                                                 Module::ImportFilter::All);
    }
  }
};

struct IndexSymbol {
  enum TypeKind { Base, FuncDecl, CallReference };
  TypeKind entityType = Base;

  SymbolKind kind;
  SymbolSubKind subKind = SymbolSubKind::None;
  bool isRef;
  llvm::SmallString<32> name;
  llvm::SmallString<64> USR;
  llvm::SmallString<16> group;
  unsigned line = 0;
  unsigned column = 0;

  IndexSymbol() = default;

protected:
  IndexSymbol(TypeKind TK) : entityType(TK) {}
};

struct FuncDeclIndexSymbol : public IndexSymbol {
  bool IsTestCandidate = false;

  FuncDeclIndexSymbol() : IndexSymbol(FuncDecl) {}
};

struct CallRefIndexSymbol : public IndexSymbol {
  llvm::SmallString<64> ReceiverUSR;
  bool IsDynamic = false;

  CallRefIndexSymbol() : IndexSymbol(CallReference) {}
};

class IndexDataConsumer {
  virtual void anchor();

public:
  virtual ~IndexDataConsumer() {}

  virtual void failed(StringRef error) = 0;
  virtual bool recordHash(StringRef hash, bool isKnown) = 0;
  virtual bool startDependency(SymbolKind kind, StringRef name, StringRef path,
                               bool isSystem, StringRef hash) = 0;
  virtual bool finishDependency(SymbolKind kind) = 0;
  virtual bool startSourceEntity(const IndexSymbol &symbol) = 0;
  virtual bool recordRelatedEntity(const IndexSymbol &symbol) = 0;
  virtual bool finishSourceEntity(SymbolKind kind, SymbolSubKind subKind,
                                  bool isRef) = 0;
};

void IndexDataConsumer::anchor() {}

static UIdent getUIDForDependencyKind(SymbolKind depKind) {
  switch (depKind) {
  case SymbolKind::Module:
    return KindImportModuleSwift;
  case SymbolKind::ClangModule:
    return KindImportModuleClang;
  case SymbolKind::SourceFile:
    return KindImportSourceFile;
  default:
    return UIdent();
  }
}

class SKIndexDataConsumer : public IndexDataConsumer {
public:
  SKIndexDataConsumer(IndexingConsumer &C) : impl(C) {}

private:
  void failed(StringRef error) override { impl.failed(error); }

  bool recordHash(StringRef hash, bool isKnown) override {
    return impl.recordHash(hash, isKnown);
  }

  bool startDependency(SymbolKind kind, StringRef name, StringRef path,
                       bool isSystem, StringRef hash) override {
    auto kindUID = getUIDForDependencyKind(kind);
    return impl.startDependency(kindUID, name, path, isSystem, hash);
  }

  bool finishDependency(SymbolKind kind) override {
    return impl.finishDependency(getUIDForDependencyKind(kind));
  }

  bool startSourceEntity(const IndexSymbol &symbol) override {
    return withEntityInfo(symbol, [this](const EntityInfo &info) {
      return impl.startSourceEntity(info);
    });
  }

  bool recordRelatedEntity(const IndexSymbol &symbol) override {
    return withEntityInfo(symbol, [this](const EntityInfo &info) {
      return impl.recordRelatedEntity(info);
    });
  }

  bool finishSourceEntity(SymbolKind kind, SymbolSubKind subKind,
                          bool isRef) override {
    auto UID = SwiftLangSupport::getUIDForSymbol(kind, subKind, isRef);
    return impl.finishSourceEntity(UID);
  }

  template <typename F>
  bool withEntityInfo(const IndexSymbol &symbol, F func) {
    auto initEntity = [](EntityInfo &info, const IndexSymbol &symbol) {
      info.Kind = SwiftLangSupport::getUIDForSymbol(symbol.kind, symbol.subKind,
                                                    symbol.isRef);
      info.Name = symbol.name;
      info.USR = symbol.USR;
      info.Group = symbol.group;
      info.Line = symbol.line;
      info.Column = symbol.column;
    };

    switch (symbol.entityType) {
    case IndexSymbol::Base: {
      EntityInfo info;
      initEntity(info, symbol);
      return func(info);
    }
    case IndexSymbol::FuncDecl: {
      FuncDeclEntityInfo info;
      initEntity(info, symbol);
      info.IsTestCandidate =
          static_cast<const FuncDeclIndexSymbol &>(symbol).IsTestCandidate;
      return func(info);
    }
    case IndexSymbol::CallReference: {
      CallRefEntityInfo info;
      initEntity(info, symbol);
      auto call = static_cast<const CallRefIndexSymbol &>(symbol);
      info.ReceiverUSR = call.ReceiverUSR;
      info.IsDynamic = call.IsDynamic;
      return func(info);
    }
    }
    llvm_unreachable("unexpected symbol kind");
  }

private:
  IndexingConsumer &impl;
};

static SymbolKind getFuncSymbolKind(const FuncDecl *FD) {
  if (FD->isAccessor())
    return SymbolKind::Accessor;

  if (auto *op = FD->getOperatorDecl()) {
    switch (op->getKind()) {
    case DeclKind::PrefixOperator:  return SymbolKind::PrefixOperator;
    case DeclKind::PostfixOperator: return SymbolKind::PostfixOperator;
    case DeclKind::InfixOperator:   return SymbolKind::InfixOperator;
    default:
      llvm_unreachable("unexpected operator kind");
    }
  }

  if (FD->getDeclContext()->isTypeContext()) {
    if (FD->isStatic()) {
      if (FD->getCorrectStaticSpelling() == StaticSpellingKind::KeywordClass)
        return SymbolKind::ClassMethod;
      return SymbolKind::StaticMethod;
    }
    return SymbolKind::InstanceMethod;
  }

  return SymbolKind::Function;
}

static SymbolKind getVarSymbolKind(const VarDecl *VD) {
  auto *DC = VD->getDeclContext();
  if (DC->isTypeContext()) {
    if (VD->isStatic()) {
      if (VD->getCorrectStaticSpelling() == StaticSpellingKind::KeywordClass)
        return SymbolKind::ClassProperty;
      return SymbolKind::StaticProperty;
    }
    return SymbolKind::InstanceProperty;
  }

  if (DC->isLocalContext())
    return SymbolKind::LocalVariable;
  return SymbolKind::GlobalVariable;
}

static SymbolKind getSymbolKindForDecl(const Decl *D) {
  switch (D->getKind()) {
  case DeclKind::Enum:             return SymbolKind::Enum;
  case DeclKind::Struct:           return SymbolKind::Struct;
  case DeclKind::Class:            return SymbolKind::Class;
  case DeclKind::Protocol:         return SymbolKind::Protocol;
  case DeclKind::Extension:        return SymbolKind::Extension;
  case DeclKind::TypeAlias:        return SymbolKind::TypeAlias;
  case DeclKind::AssociatedType:   return SymbolKind::AssociatedType;
  case DeclKind::GenericTypeParam: return SymbolKind::GenericTypeParam;
  case DeclKind::EnumElement:      return SymbolKind::EnumElement;
  case DeclKind::Subscript:        return SymbolKind::Subscript;
  case DeclKind::Constructor:      return SymbolKind::Constructor;
  case DeclKind::Destructor:       return SymbolKind::Destructor;
  case DeclKind::Param:            return SymbolKind::ParamVariable;

  case DeclKind::Func:
    return getFuncSymbolKind(cast<FuncDecl>(D));
  case DeclKind::Var:
    return getVarSymbolKind(cast<VarDecl>(D));

  default:
    return SymbolKind::Unknown;
  }
}

class IndexSwiftASTWalker : public SourceEntityWalker {
  IndexDataConsumer &IdxConsumer;
  SourceManager &SrcMgr;
  unsigned BufferID;

  bool IsModuleFile = false;
  bool isSystemModule = false;
  struct Entity {
    Decl *D;
    SymbolKind Kind;
    SymbolSubKind SubKind;
    bool IsRef;
  };
  SmallVector<Entity, 6> EntitiesStack;
  SmallVector<Expr *, 8> ExprStack;
  bool Cancelled = false;

public:
  IndexSwiftASTWalker(IndexDataConsumer &IdxConsumer, ASTContext &Ctx,
                      unsigned BufferID)
      : IdxConsumer(IdxConsumer), SrcMgr(Ctx.SourceMgr), BufferID(BufferID) {}
  ~IndexSwiftASTWalker() {
    assert(Cancelled || EntitiesStack.empty());
  }

  void visitModule(Module &Mod, StringRef Hash);

private:
  bool visitImports(SourceFileOrModule Mod,
                    llvm::SmallPtrSet<Module *, 16> &Visited);

  bool handleSourceOrModuleFile(SourceFileOrModule SFOrMod,
                                StringRef KnownHash, bool &HashIsKnown);

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
                          TypeDecl *CtorTyRef, Type T) override {
    SourceLoc Loc = Range.getStart();
    if (CtorTyRef)
      if (!reportRef(CtorTyRef, Loc))
        return false;
    if (!reportRef(D, Loc))
      return false;

    return true;
  }

  Decl *getParentDecl() {
    if (!EntitiesStack.empty())
      return EntitiesStack.back().D;
    return nullptr;
  }

  Expr *getParentExpr() {
    if (ExprStack.size() >= 2)
      return ExprStack.end()[-2];
    return nullptr;
  }

  bool reportExtension(ExtensionDecl *D);

  bool report(ValueDecl *D);
  bool reportRef(ValueDecl *D, SourceLoc Loc);

  bool startEntityDecl(ValueDecl *D);
  bool startEntityRef(ValueDecl *D, SourceLoc Loc);
  bool startEntity(ValueDecl *D, const IndexSymbol &Info);

  bool passRelated(ValueDecl *D, SourceLoc Loc);
  bool passInheritedTypes(ArrayRef<TypeLoc> Inherited);
  bool passRelatedType(const TypeLoc &Ty);
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
    assert(CurrEnt.Kind != SymbolKind::Unknown);
    if (!IdxConsumer.finishSourceEntity(CurrEnt.Kind, CurrEnt.SubKind,
                                        CurrEnt.IsRef)) {
      Cancelled = true;
      return false;
    }
    return true;
  }

  bool initIndexSymbol(ValueDecl *D, SourceLoc Loc, bool IsRef,
                       IndexSymbol &Info);
  bool initFuncDeclIndexSymbol(ValueDecl *D, FuncDeclIndexSymbol &Info);
  bool initCallRefIndexSymbol(Expr *CurrentE, Expr *ParentE, ValueDecl *D,
                              SourceLoc Loc, CallRefIndexSymbol &Info);

  std::pair<unsigned, unsigned> getLineCol(SourceLoc Loc) {
    if (Loc.isInvalid())
      return std::make_pair(0, 0);
    return SrcMgr.getLineAndColumn(Loc, BufferID);
  }

  bool shouldIndex(ValueDecl *D) const {
    if (D->isImplicit())
      return false;
    if (isLocal(D))
      return false;
    if (D->isPrivateStdlibDecl())
      return false;

    return true;
  }

  bool isLocal(ValueDecl *D) const {
    return D->getDeclContext()->getLocalContext();
  }

  void getModuleHash(SourceFileOrModule SFOrMod, llvm::raw_ostream &OS);
  llvm::hash_code hashModule(llvm::hash_code code, SourceFileOrModule SFOrMod);
  void getRecursiveModuleImports(Module &Mod,
                                 SmallVectorImpl<Module *> &Imports);
  void collectRecursiveModuleImports(Module &Mod,
                                     llvm::SmallPtrSet<Module *, 16> &Visited);

  // This maps a module to all its imports, recursively.
  llvm::DenseMap<Module *, llvm::SmallVector<Module *, 4>> ImportsMap;
};
} // anonymous namespace

void IndexSwiftASTWalker::visitModule(Module &Mod, StringRef KnownHash) {
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
  llvm::SmallPtrSet<Module *, 16> Visited;
  return visitImports(SFOrMod, Visited);
}

bool IndexSwiftASTWalker::visitImports(SourceFileOrModule TopMod,
                                     llvm::SmallPtrSet<Module *, 16> &Visited) {
  // Dependencies of the stdlib module (like SwiftShims module) are
  // implementation details.
  if (TopMod.getModule().isStdlibModule())
    return true;

  bool IsNew = Visited.insert(&TopMod.getModule()).second;
  if (!IsNew)
    return true;

  SmallVector<Module::ImportedModule, 8> Imports;
  TopMod.getImportedModules(Imports);

  llvm::SmallPtrSet<Module *, 8> Reported;
  for (auto Import : Imports) {
    Module *Mod = Import.second;
    bool NewReport = Reported.insert(Mod).second;
    if (!NewReport)
      continue;

    // FIXME: Handle modules with multiple source files; these will fail on
    // getModuleFilename() (by returning an empty path). Note that such modules
    // may be heterogeneous.
    StringRef Path = Mod->getModuleFilename();
    if (Path.empty() || Path == TopMod.getFilename())
      continue; // this is a submodule.

    SymbolKind ImportKind = SymbolKind::Unknown;
    for (auto File : Mod->getFiles()) {
      switch (File->getKind()) {
      case FileUnitKind::Source:
        assert(ImportKind == SymbolKind::Unknown &&
               "cannot handle multi-file modules");
        ImportKind = SymbolKind::SourceFile;
        break;
      case FileUnitKind::Builtin:
      case FileUnitKind::Derived:
        break;
      case FileUnitKind::SerializedAST:
        assert(ImportKind == SymbolKind::Unknown &&
               "cannot handle multi-file modules");
        ImportKind = SymbolKind::Module;
        break;
      case FileUnitKind::ClangModule:
        assert(ImportKind == SymbolKind::Unknown &&
               "cannot handle multi-file modules");
        ImportKind = SymbolKind::ClangModule;
        break;
      }
    }
    if (ImportKind == SymbolKind::Unknown)
      continue;

    StringRef Hash;
    SmallString<32> HashBuf;
    if (ImportKind != SymbolKind::ClangModule) {
      llvm::raw_svector_ostream HashOS(HashBuf);
      getModuleHash(*Mod, HashOS);
      Hash = HashOS.str();
    }

    if (!IdxConsumer.startDependency(ImportKind, Mod->getName().str(), Path,
                                     Mod->isSystemModule(), Hash))
      return false;
    if (ImportKind != SymbolKind::ClangModule)
      if (!visitImports(*Mod, Visited))
        return false;
    if (!IdxConsumer.finishDependency(ImportKind))
      return false;
  }

  return true;
}

bool IndexSwiftASTWalker::startEntityDecl(ValueDecl *D) {
  if (!shouldIndex(D))
    return false;

  SourceLoc Loc = D->getLoc();
  if (Loc.isInvalid() && !IsModuleFile)
    return false;

  if (isa<FuncDecl>(D)) {
    FuncDeclIndexSymbol Info;
    if (initFuncDeclIndexSymbol(D, Info))
      return false;

    return startEntity(D, Info);

  } else {
    IndexSymbol Info;
    if (initIndexSymbol(D, Loc, /*isRef=*/false, Info))
      return false;

    return startEntity(D, Info);
  }
}

bool IndexSwiftASTWalker::startEntityRef(ValueDecl *D, SourceLoc Loc) {
  if (!shouldIndex(D))
    return false;

  if (Loc.isInvalid())
    return false;

  if (isa<AbstractFunctionDecl>(D)) {
    CallRefIndexSymbol Info;
    if (initCallRefIndexSymbol(ExprStack.back(), getParentExpr(), D, Loc, Info))
      return false;

    return startEntity(D, Info);

  } else {
    IndexSymbol Info;
    if (initIndexSymbol(D, Loc, /*isRef=*/true, Info))
      return false;

    return startEntity(D, Info);
  }
}

bool IndexSwiftASTWalker::startEntity(ValueDecl *D, const IndexSymbol &Info) {
  if (!IdxConsumer.startSourceEntity(Info)) {
    Cancelled = true;
    return false;
  }

  EntitiesStack.push_back({D, Info.kind, Info.subKind, Info.isRef});
  return true;
}

bool IndexSwiftASTWalker::passRelated(ValueDecl *D, SourceLoc Loc) {
  if (!shouldIndex(D))
    return false;

  IndexSymbol Info;
  if (initIndexSymbol(D, Loc, /*isRef=*/true, Info))
    return false;

  if (!IdxConsumer.recordRelatedEntity(Info)) {
    Cancelled = true;
    return false;
  }
  return true;
}

bool IndexSwiftASTWalker::passInheritedTypes(ArrayRef<TypeLoc> Inherited) {
  for (auto Base : Inherited) {
    passRelatedType(Base);
  }
  return true;
}

bool IndexSwiftASTWalker::passRelatedType(const TypeLoc &Ty) {
  if (IdentTypeRepr *T = dyn_cast_or_null<IdentTypeRepr>(Ty.getTypeRepr())) {
    auto Comps = T->getComponentRange();
    if (auto NTD = dyn_cast_or_null<NominalTypeDecl>(
                     Comps.back()->getBoundDecl())) {
      if (!passRelated(NTD, Comps.back()->getIdLoc()))
        return false;
    }

    return true;
  }

  if (Ty.getType()) {
    if (auto nominal = dyn_cast_or_null<NominalTypeDecl>(
                         Ty.getType()->getDirectlyReferencedTypeDecl()))
      if (!passRelated(nominal, Ty.getLoc()))
        return false;
  }

  return true;
}

static SymbolSubKind getSubKindForAccessor(AccessorKind AK) {
  switch (AK) {
  case AccessorKind::NotAccessor: return SymbolSubKind::None;
  case AccessorKind::IsGetter:    return SymbolSubKind::AccessorGetter;
  case AccessorKind::IsSetter:    return SymbolSubKind::AccessorSetter;
  case AccessorKind::IsWillSet:   return SymbolSubKind::AccessorWillSet;
  case AccessorKind::IsDidSet:    return SymbolSubKind::AccessorDidSet;
  case AccessorKind::IsMaterializeForSet:
    return SymbolSubKind::AccessorMaterializeForSet;
  case AccessorKind::IsAddressor: return SymbolSubKind::AccessorAddressor;
  case AccessorKind::IsMutableAddressor:
    return SymbolSubKind::AccessorMutableAddressor;
  }
}

bool IndexSwiftASTWalker::reportPseudoAccessor(AbstractStorageDecl *D,
                                               AccessorKind AccKind,
                                               bool IsRef, SourceLoc Loc) {
  if (!shouldIndex(D))
    return true; // continue walking.

  auto handleInfo = [this, D, AccKind](IndexSymbol &Info) {
    Info.kind = SymbolKind::Accessor;
    Info.subKind = getSubKindForAccessor(AccKind);
    Info.name.clear();
    Info.USR.clear();
    Info.group.clear();
    llvm::raw_svector_ostream OS(Info.USR);
    SwiftLangSupport::printAccessorUSR(D, AccKind, OS);

    if (!IdxConsumer.startSourceEntity(Info)) {
      Cancelled = true;
      return false;
    }
    if (!IdxConsumer.finishSourceEntity(Info.kind, Info.subKind, Info.isRef)) {
      Cancelled = true;
      return false;
    }
    return true;
  };

  if (IsRef) {
    CallRefIndexSymbol Info;
    if (initCallRefIndexSymbol(ExprStack.back(), getParentExpr(), D, Loc, Info))
      return true; // continue walking.

    return handleInfo(Info);

  } else {
    IndexSymbol Info;
    if (initIndexSymbol(D, Loc, IsRef, Info))
      return true; // continue walking.

    return handleInfo(Info);
  }
}

NominalTypeDecl *
IndexSwiftASTWalker::getTypeLocAsNominalTypeDecl(const TypeLoc &Ty) {
  if (Type T = Ty.getType())
    return dyn_cast_or_null<NominalTypeDecl>(T->getDirectlyReferencedTypeDecl());
  if (IdentTypeRepr *T = dyn_cast_or_null<IdentTypeRepr>(Ty.getTypeRepr())) {
    auto Comp = T->getComponentRange().back();
    if (auto NTD = dyn_cast_or_null<NominalTypeDecl>(Comp->getBoundDecl()))
      return NTD;
  }
  return nullptr;
}


bool IndexSwiftASTWalker::reportExtension(ExtensionDecl *D) {
  SourceLoc Loc = D->getExtendedTypeLoc().getSourceRange().Start;
  if (!D->getExtendedType())
    return true;
  NominalTypeDecl *NTD = D->getExtendedType()->getAnyNominal();
  if (!NTD)
    return true;
  if (!shouldIndex(NTD))
    return true;

  IndexSymbol Info;
  if (initIndexSymbol(NTD, Loc, /*isRef=*/false, Info))
    return true;

  Info.kind = getSymbolKindForDecl(D);
  if (isa<StructDecl>(NTD))
    Info.subKind = SymbolSubKind::ExtensionOfStruct;
  else if (isa<ClassDecl>(NTD))
    Info.subKind = SymbolSubKind::ExtensionOfClass;
  else if (isa<EnumDecl>(NTD))
    Info.subKind = SymbolSubKind::ExtensionOfEnum;
  else if (isa<ProtocolDecl>(NTD))
    Info.subKind = SymbolSubKind::ExtensionOfProtocol;

  assert(Info.subKind != SymbolSubKind::None);

  if (!IdxConsumer.startSourceEntity(Info)) {
    Cancelled = true;
    return false;
  }

  passInheritedTypes(D->getInherited());
  if (Cancelled)
    return false;

  EntitiesStack.push_back({D, Info.kind, Info.subKind, Info.isRef});
  return true;
}

bool IndexSwiftASTWalker::report(ValueDecl *D) {
  if (startEntityDecl(D)) {
    if (TypeDecl *TD = dyn_cast<NominalTypeDecl>(D))
      passInheritedTypes(TD->getInherited());
    if (Cancelled)
      return false;
    if (auto Overridden = D->getOverriddenDecl()) {
      passRelated(Overridden, SourceLoc());
      if (Cancelled)
        return false;
    }
    for (auto Conf: D->getSatisfiedProtocolRequirements()) {
      passRelated(Conf, SourceLoc());
      if (Cancelled)
        return false;
    }

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
    }
  }

  return !Cancelled;
}

bool IndexSwiftASTWalker::reportRef(ValueDecl *D, SourceLoc Loc) {
  if (startEntityRef(D, Loc)) {
    // Report the accessors that were utilized.
    if (isa<AbstractStorageDecl>(D) && getParentExpr()) {
      bool UsesGetter = false;
      bool UsesSetter = false;
      Expr *CurrE = ExprStack.back();
      Expr *Parent = getParentExpr();
      bool isLValue = !CurrE->getType().isNull() &&
                      CurrE->getType()->is<LValueType>();
      if (isa<LoadExpr>(Parent) || !isLValue) {
        UsesGetter = true;
      } else if (isa<AssignExpr>(Parent)) {
        UsesSetter = true;
      } else {
        UsesGetter = UsesSetter = true;
      }

      AbstractStorageDecl *ASD = cast<AbstractStorageDecl>(D);
      if (UsesGetter)
        if (!reportPseudoAccessor(ASD, AccessorKind::IsGetter, /*IsRef=*/true, Loc))
          return false;
      if (UsesSetter)
        if (!reportPseudoAccessor(ASD, AccessorKind::IsSetter, /*IsRef=*/true, Loc))
          return false;
    }

    assert(EntitiesStack.back().D == D);
    return finishCurrentEntity();
  }

  return !Cancelled;
}

bool IndexSwiftASTWalker::initIndexSymbol(ValueDecl *D, SourceLoc Loc,
                                          bool IsRef, IndexSymbol &Info) {
  assert(D);
  Info.kind = getSymbolKindForDecl(D);
  if (Info.kind == SymbolKind::Unknown)
    return true;

  if (Info.kind == SymbolKind::Accessor)
    Info.subKind = getSubKindForAccessor(cast<FuncDecl>(D)->getAccessorKind());
  // Cannot be extension, which is not a ValueDecl.

  Info.isRef = IsRef;

  Info.name.clear();
  llvm::raw_svector_ostream NameOS(Info.name);
  SwiftLangSupport::printDisplayName(D, NameOS);

  llvm::raw_svector_ostream OS(Info.USR);
  if (SwiftLangSupport::printUSR(D, OS))
    return true;
  std::tie(Info.line, Info.column) = getLineCol(Loc);
  if (auto Group = D->getGroupName()) {
    Info.group.clear();
    Info.group.append(Group.getValue());
  }
  return false;
}

static NominalTypeDecl *getNominalParent(ValueDecl *D) {
  Type Ty = D->getDeclContext()->getDeclaredTypeOfContext();
  if (!Ty)
    return nullptr;
  return Ty->getAnyNominal();
}

static bool isTestCandidate(ValueDecl *D) {
  if (!D->hasName())
    return false;

  // A 'test candidate' is a class instance method that returns void, has no
  // parameters and starts with 'test'.
  // FIXME: Also test if it is ObjC exportable ?
  if (auto FD = dyn_cast<FuncDecl>(D)) {
    if (FD->isStatic())
      return false;
    if (!D->getDeclContext()->isTypeContext())
      return false;
    auto NTD = getNominalParent(D);
    if (!NTD)
      return false;
    Type RetTy = FD->getResultType();
    if (FD->getParameterLists().size() != 2)
      return false;
    auto paramList = FD->getParameterList(1);
    if (RetTy && RetTy->isVoid() && isa<ClassDecl>(NTD) &&
        paramList->size() == 0 && FD->getName().str().startswith("test"))
      return true;
  }

  return false;
}

bool IndexSwiftASTWalker::initFuncDeclIndexSymbol(ValueDecl *D,
                                                  FuncDeclIndexSymbol &Info) {
  if (initIndexSymbol(D, D->getLoc(), /*IsRef=*/false, Info))
    return true;

  Info.IsTestCandidate = isTestCandidate(D);
  if (auto Group = D->getGroupName()) {
    Info.group.clear();
    Info.group.append(Group.getValue());
  }
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

bool IndexSwiftASTWalker::initCallRefIndexSymbol(Expr *CurrentE, Expr *ParentE,
                                                 ValueDecl *D, SourceLoc Loc,
                                                 CallRefIndexSymbol &Info) {
  if (!ParentE)
    return true;

  if (initIndexSymbol(D, Loc, /*IsRef=*/true, Info))
    return true;

  Expr *BaseE = nullptr;
  if (auto DotE = dyn_cast<DotSyntaxCallExpr>(ParentE))
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
      llvm::raw_svector_ostream OS(Info.ReceiverUSR);
      SwiftLangSupport::printUSR(TyD, OS);

      Info.IsDynamic = isDynamicCall(BaseE, D);
    }
  }

  return false;
}

static llvm::hash_code hashFileReference(llvm::hash_code code,
                                         SourceFileOrModule SFOrMod) {
  StringRef Filename = SFOrMod.getFilename();
  if (Filename.empty())
    return code;

  // FIXME: FileManager for swift ?

  llvm::sys::fs::file_status Status;
  if (std::error_code Ret = llvm::sys::fs::status(Filename, Status)) {
    // Failure to read the file, just use filename to recover.
    LOG_WARN_FUNC("failed to stat file: " << Filename
                  << " (" << Ret.message() << ')');
    return hash_combine(code, Filename);
  }

  // Don't use inode because it can easily change when you update the repository
  // even though the file is supposed to be the same (same size/time).
  code = hash_combine(code, Filename);
  return hash_combine(code, Status.getSize(),
                      Status.getLastModificationTime().toEpochTime());
}

llvm::hash_code IndexSwiftASTWalker::hashModule(llvm::hash_code code,
                                                SourceFileOrModule SFOrMod) {
  code = hashFileReference(code, SFOrMod);

  SmallVector<Module *, 16> Imports;
  getRecursiveModuleImports(SFOrMod.getModule(), Imports);
  for (auto Import : Imports)
    code = hashFileReference(code, *Import);

  return code;
}

void IndexSwiftASTWalker::getRecursiveModuleImports(Module &Mod,
                                           SmallVectorImpl<Module *> &Imports) {
  auto It = ImportsMap.find(&Mod);
  if (It != ImportsMap.end()) {
    Imports.append(It->second.begin(), It->second.end());
    return;
  }

  llvm::SmallPtrSet<Module *, 16> Visited;
  collectRecursiveModuleImports(Mod, Visited);
  Visited.erase(&Mod);

  if (Logger::isLoggingEnabledForLevel(Logger::Level::Warning)) {
    std::for_each(Imports.begin(), Imports.end(), [](Module *M) {
      if (M->getModuleFilename().empty()) {
        std::string Info = "swift::Module with empty file name!! \nDetails: \n";
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

        LOG_WARN_FUNC("swift::Module with empty file name! " << Info);
      }
    });
  }

  Imports.append(Visited.begin(), Visited.end());
  std::sort(Imports.begin(), Imports.end(), [](Module *LHS, Module *RHS) {
    return LHS->getModuleFilename() < RHS->getModuleFilename();
  });

  // Cache it.
  ImportsMap[&Mod].append(Imports.begin(), Imports.end());
}

void IndexSwiftASTWalker::collectRecursiveModuleImports(
    Module &TopMod,
    llvm::SmallPtrSet<Module *, 16> &Visited) {

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

  SmallVector<Module::ImportedModule, 8> Imports;
  TopMod.getImportedModules(Imports, Module::ImportFilter::All);

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


static void indexModule(llvm::MemoryBuffer *Input,
                        StringRef ModuleName,
                        StringRef Hash,
                        IndexingConsumer &IdxConsumer,
                        CompilerInstance &CI,
                        ArrayRef<const char *> Args) {
  trace::TracedOperation TracedOp;
  if (trace::enabled()) {
    trace::SwiftInvocation SwiftArgs;
    SwiftArgs.Args.Args.assign(Args.begin(), Args.end());
    SwiftArgs.Args.PrimaryFile = Input->getBufferIdentifier();
    SwiftArgs.addFile(Input->getBufferIdentifier(), Input->getBuffer());
    trace::StringPairs OpArgs;
    OpArgs.push_back(std::make_pair("ModuleName", ModuleName));
    OpArgs.push_back(std::make_pair("Hash", Hash));
    TracedOp.start(trace::OperationKind::IndexModule, SwiftArgs, OpArgs);
  }

  ASTContext &Ctx = CI.getASTContext();
  std::unique_ptr<SerializedModuleLoader> Loader;
  Module *Mod = nullptr;
  if (ModuleName == Ctx.StdlibModuleName.str()) {
    Mod = Ctx.getModule({ {Ctx.StdlibModuleName, SourceLoc()} });
  } else {
    Loader = SerializedModuleLoader::create(Ctx);
    auto Buf = std::unique_ptr<llvm::MemoryBuffer>(
        llvm::MemoryBuffer::getMemBuffer(Input->getBuffer(),
                                         Input->getBufferIdentifier()));

    // FIXME: These APIs allocate memory on the ASTContext, meaning it may not
    // be freed for a long time.
    Mod = Module::create(Ctx.getIdentifier(ModuleName), Ctx);
    // Indexing is not using documentation now, so don't open the module
    // documentation file.
    // FIXME: refactor the frontend to provide an easy way to figure out the
    // correct filename here.
    auto FUnit = Loader->loadAST(*Mod, None, std::move(Buf), nullptr);

    // FIXME: Not knowing what went wrong is pretty bad. loadModule() should be
    // more modular, rather than emitting diagnostics itself.
    if (!FUnit) {
      IdxConsumer.failed("failed to load module");
      return;
    }
  }

  // Setup a typechecker for protocol conformance resolving.
  OwnedResolver TypeResolver = createLazyResolver(Ctx);

  SKIndexDataConsumer IdxDataConsumer(IdxConsumer);
  IndexSwiftASTWalker Walker(IdxDataConsumer, Ctx, /*BufferID=*/-1);
  Walker.visitModule(*Mod, Hash);
}


//===----------------------------------------------------------------------===//
// IndexSource
//===----------------------------------------------------------------------===//

void trace::initTraceInfo(trace::SwiftInvocation &SwiftArgs,
                          StringRef InputFile,
                          ArrayRef<const char *> Args) {
  SwiftArgs.Args.Args.assign(Args.begin(), Args.end());
  SwiftArgs.Args.PrimaryFile = InputFile;
}

void trace::initTraceFiles(trace::SwiftInvocation &SwiftArgs,
                           swift::CompilerInstance &CI) {
  auto &SM = CI.getSourceMgr();
  auto Ids = CI.getInputBufferIDs();
  std::for_each(Ids.begin(), Ids.end(),
                [&] (unsigned Id) {
                  auto Buf = SM.getLLVMSourceMgr().getMemoryBuffer(Id);
                  SwiftArgs.addFile(Buf->getBufferIdentifier(),
                                    Buf->getBuffer());
                });
}

void SwiftLangSupport::indexSource(StringRef InputFile,
                                   IndexingConsumer &IdxConsumer,
                                   ArrayRef<const char *> Args,
                                   StringRef Hash) {
  std::string Error;
  auto InputBuf = ASTMgr->getMemoryBuffer(InputFile, Error);
  if (!InputBuf) {
    IdxConsumer.failed(Error);
    return;
  }

  StringRef Filename = llvm::sys::path::filename(InputFile);
  StringRef FileExt = llvm::sys::path::extension(Filename);

  bool IsModuleIndexing = (FileExt == ".swiftmodule" || FileExt == ".pcm");
  CompilerInstance CI;
  // Display diagnostics to stderr.
  PrintingDiagnosticConsumer PrintDiags;
  CI.addDiagnosticConsumer(&PrintDiags);

  CompilerInvocation Invocation;
  bool Failed = getASTManager().initCompilerInvocation(Invocation, Args,
                                                       CI.getDiags(),
                    /*PrimaryFile=*/IsModuleIndexing ? StringRef() : InputFile,
                                                       Error);
  if (Failed) {
    IdxConsumer.failed(Error);
    return;
  }

  if (IsModuleIndexing) {
    if (CI.setup(Invocation))
      return;
    bool IsClangModule = (FileExt == ".pcm");
    if (IsClangModule) {
      IdxConsumer.failed("Clang module files are not supported");
      return;
    }

    indexModule(InputBuf.get(), llvm::sys::path::stem(Filename),
                Hash, IdxConsumer, CI, Args);
    return;
  }

  if (Invocation.getInputFilenames().empty()) {
    IdxConsumer.failed("no input filenames specified");
    return;
  }

  if (CI.setup(Invocation))
    return;

  trace::TracedOperation TracedOp;
  if (trace::enabled()) {
    trace::SwiftInvocation SwiftArgs;
    trace::initTraceInfo(SwiftArgs, InputFile, Args);
    trace::initTraceFiles(SwiftArgs, CI);
    TracedOp.start(trace::OperationKind::IndexSource, SwiftArgs);
  }

  CI.performSema();

  // NOTE: performSema() may end up with some gruesome error preventing it from
  // setting primary file correctly
  if (!CI.getPrimarySourceFile()) {
    IdxConsumer.failed("no primary source file found");
    return;
  }

  // Setup a typechecker for protocol conformance resolving.
  OwnedResolver TypeResolver = createLazyResolver(CI.getASTContext());

  unsigned BufferID = CI.getPrimarySourceFile()->getBufferID().getValue();
  SKIndexDataConsumer IdxDataConsumer(IdxConsumer);
  IndexSwiftASTWalker Walker(IdxDataConsumer, CI.getASTContext(), BufferID);
  Walker.visitModule(*CI.getMainModule(), Hash);
}
