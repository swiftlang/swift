//===--- LookupVisibleDecls - Swift Name Lookup Routines ------------------===//
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
// This file implements the lookupVisibleDecls interface for visiting named
// declarations.
//
//===----------------------------------------------------------------------===//


#include "swift/AST/NameLookup.h"
#include "swift/AST/AST.h"
#include "swift/AST/ASTVisitor.h"

using namespace swift;

VisibleDeclConsumer::~VisibleDeclConsumer() {
  // Anchor the vtable.
}

static void DoGlobalExtensionLookup(Type BaseType,
                                    VisibleDeclConsumer &Consumer,
                                    ArrayRef<ValueDecl*> BaseMembers,
                                    Module *CurModule,
                                    Module *BaseModule,
                                    bool IsTypeLookup) {
  bool CurModuleHasTypeDecl = false;
  llvm::SmallPtrSet<CanType, 8> CurModuleTypes;

  // FIXME: Hack to avoid searching Clang modules more than once.
  bool searchedClangModule = isa<ClangModule>(CurModule);

  // Find all extensions in this module.
  for (ExtensionDecl *ED : CurModule->lookupExtensions(BaseType)) {
    for (Decl *Member : ED->getMembers()) {
      if (ValueDecl *VD = dyn_cast<ValueDecl>(Member)) {
        Consumer.foundDecl(VD);
        if (!IsTypeLookup)
          CurModuleTypes.insert(VD->getType()->getCanonicalType());
        CurModuleHasTypeDecl |= isa<TypeDecl>(VD);
      }
    }
  }

  if (BaseModule == CurModule) {
    for (ValueDecl *VD : BaseMembers) {
      Consumer.foundDecl(VD);
      if (!IsTypeLookup)
        CurModuleTypes.insert(VD->getType()->getCanonicalType());
      CurModuleHasTypeDecl |= isa<TypeDecl>(VD);
    }
  }

  // The builtin module has no imports.
  if (isa<BuiltinModule>(CurModule)) return;

  // The Clang module loader handles transitive lookups itself... but is that
  // what we want?
  if (isa<ClangModule>(CurModule)) return;

  // If we find a type in the current module, don't look into any
  // imported modules.
  if (CurModuleHasTypeDecl) return;

  TranslationUnit &TU = *cast<TranslationUnit>(CurModule);

  // Otherwise, check our imported extensions as well.
  // FIXME: Implement DAG-based shadowing rules.
  llvm::SmallPtrSet<Module *, 16> Visited;
  for (auto &ImpEntry : TU.getImportedModules()) {
    if (!Visited.insert(ImpEntry.second))
      continue;

    // FIXME: Don't search Clang modules more than once.
    if (isa<ClangModule>(ImpEntry.second)) {
      if (searchedClangModule)
        continue;

      searchedClangModule = true;
    }

    for (ExtensionDecl *ED : ImpEntry.second->lookupExtensions(BaseType)) {
      for (Decl *Member : ED->getMembers()) {
        if (ValueDecl *VD = dyn_cast<ValueDecl>(Member)) {
          if (IsTypeLookup || isa<TypeDecl>(VD) ||
              !CurModuleTypes.count(VD->getType()->getCanonicalType()))
            Consumer.foundDecl(VD);
        }
      }
    }
  }

  if (BaseModule != CurModule) {
    for (ValueDecl *VD : BaseMembers) {
      if (IsTypeLookup || isa<TypeDecl>(VD) ||
          !CurModuleTypes.count(VD->getType()->getCanonicalType())) {
        Consumer.foundDecl(VD);
      }
    }
  }
}

namespace {
  typedef llvm::SmallPtrSet<TypeDecl *, 8> VisitedSet;
}

static void doMemberLookup(Type BaseTy,
                           VisibleDeclConsumer &Consumer,
                           Module &M,
                           bool IsTypeLookup,
                           bool OnlyInstanceMembers,
                           VisitedSet &Visited);
static void lookupTypeMembers(Type BaseType,
                              VisibleDeclConsumer &Consumer,
                              Module &M,
                              bool IsTypeLookup);


static void lookupVisibleMemberDecls(Type BaseTy,
                                     VisibleDeclConsumer &Consumer,
                                     Module &M,
                                     bool IsTypeLookup) {
  VisitedSet Visited;
  doMemberLookup(BaseTy, Consumer, M,
                 IsTypeLookup,
                 /*OnlyInstanceMembers=*/!IsTypeLookup,
                 Visited);
}

/// doIt - Lookup a member 'Name' in 'BaseTy' within the context
/// of a given module 'M'.  This operation corresponds to a standard "dot" 
/// lookup operation like "a.b" where 'this' is the type of 'a'.  This
/// operation is only valid after name binding.
///
/// \param OnlyInstanceMembers Only instance members should be found by
/// name lookup.
static void doMemberLookup(Type BaseTy,
                           VisibleDeclConsumer &Consumer,
                           Module &M,
                           bool IsTypeLookup,
                           bool OnlyInstanceMembers,
                           VisitedSet &Visited) {
  typedef MemberLookupResult Result;
  
  // Just look through l-valueness.  It doesn't affect name lookup.
  BaseTy = BaseTy->getRValueType();

  // Type check metatype references, as in "some_type.some_member".  These are
  // special and can't have extensions.
  if (MetaTypeType *MTT = BaseTy->getAs<MetaTypeType>()) {
    // The metatype represents an arbitrary named type: dig through to the
    // declared type to see what we're dealing with.
    Type Ty = MTT->getInstanceType();

    // Just perform normal dot lookup on the type with the specified
    // member name to see if we find extensions or anything else.  For example,
    // type SomeTy.SomeMember can look up static functions, and can even look
    // up non-static functions as well (thus getting the address of the member).
    doMemberLookup(Ty, Consumer, M, IsTypeLookup,
                   /*OnlyInstanceMembers=*/false, Visited);
    return;
  }
  
  // Lookup module references, as on some_module.some_member.  These are
  // special and can't have extensions.
  if (ModuleType *MT = BaseTy->getAs<ModuleType>()) {
    MT->getModule()->lookupVisibleDecls(Module::AccessPathTy(), Consumer,
                                        NLKind::QualifiedLookup);
    return;
  }

  // If the base is a protocol, see if this is a reference to a declared
  // protocol member.
  if (ProtocolType *PT = BaseTy->getAs<ProtocolType>()) {
    if (!Visited.insert(PT->getDecl()))
      return;
      
    for (auto Inherited : PT->getDecl()->getInherited())
      doMemberLookup(Inherited.getType(), Consumer, M, IsTypeLookup,
                     OnlyInstanceMembers, Visited);
    
    for (auto Member : PT->getDecl()->getMembers()) {
      if (ValueDecl *VD = dyn_cast<ValueDecl>(Member)) {
        if (isa<VarDecl>(VD) || isa<SubscriptDecl>(VD) || isa<FuncDecl>(VD)) {
          if (OnlyInstanceMembers && !VD->isInstanceMember())
            continue;

          Consumer.foundDecl(VD);
        } else {
          assert(isa<TypeDecl>(VD) && "Unhandled protocol member");
          Consumer.foundDecl(VD);
        }
      }
    }
    return;
  }
  
  // If the base is a protocol composition, see if this is a reference to a
  // declared protocol member in any of the protocols.
  if (auto PC = BaseTy->getAs<ProtocolCompositionType>()) {
    for (auto Proto : PC->getProtocols())
      doMemberLookup(Proto, Consumer, M, IsTypeLookup,
                     OnlyInstanceMembers, Visited);
    return;
  }

  // Check to see if any of an archetype's requirements have the member.
  if (ArchetypeType *Archetype = BaseTy->getAs<ArchetypeType>()) {
    for (auto Proto : Archetype->getConformsTo())
      doMemberLookup(Proto->getDeclaredType(), Consumer, M, IsTypeLookup,
                     OnlyInstanceMembers, Visited);

    if (auto superclass = Archetype->getSuperclass())
      doMemberLookup(superclass, Consumer, M, IsTypeLookup,
                     OnlyInstanceMembers, Visited);
    return;
  }

  do {
    // Look in for members of a nominal type.
    SmallVector<ValueDecl*, 8> ExtensionMethods;
    lookupTypeMembers(BaseTy, Consumer, M, IsTypeLookup);

    for (ValueDecl *VD : ExtensionMethods) {
      assert((isa<VarDecl>(VD) || isa<SubscriptDecl>(VD)) &&
             "Unexpected extension member");
      Consumer.foundDecl(VD);
    }

    // If we have a class type, look into its base class.
    ClassDecl *CurClass = nullptr;
    if (auto CT = BaseTy->getAs<ClassType>())
      CurClass = CT->getDecl();
    else if (auto BGT = BaseTy->getAs<BoundGenericType>())
      CurClass = dyn_cast<ClassDecl>(BGT->getDecl());
    else if (UnboundGenericType *UGT = BaseTy->getAs<UnboundGenericType>())
      CurClass = dyn_cast<ClassDecl>(UGT->getDecl());

    if (CurClass && CurClass->hasBaseClass()) {
      BaseTy = CurClass->getBaseClass();
    } else {
      break;
    }
  } while (1);

  // FIXME: Weed out overridden methods.
}

static void lookupTypeMembers(Type BaseType, VisibleDeclConsumer &Consumer,
                              Module &M, bool IsTypeLookup) {
  NominalTypeDecl *D;
  ArrayRef<ValueDecl*> BaseMembers;
  SmallVector<ValueDecl*, 2> BaseMembersStorage;
  if (BoundGenericType *BGT = BaseType->getAs<BoundGenericType>()) {
    BaseType = BGT->getDecl()->getDeclaredType();
    D = BGT->getDecl();
  } else if (UnboundGenericType *UGT = BaseType->getAs<UnboundGenericType>()) {
    D = UGT->getDecl();
  } else if (NominalType *NT = BaseType->getAs<NominalType>()) {
    D = NT->getDecl();
  } else {
    return;
  }

  for (Decl* Member : D->getMembers()) {
    if (ValueDecl *VD = dyn_cast<ValueDecl>(Member)) {
      BaseMembersStorage.push_back(VD);
    }
  }
  if (D->getGenericParams())
    for (auto param : *D->getGenericParams())
      BaseMembersStorage.push_back(param.getDecl());
  BaseMembers = BaseMembersStorage;

  DeclContext *DC = D->getDeclContext();
  while (!DC->isModuleContext())
    DC = DC->getParent();

  DoGlobalExtensionLookup(BaseType, Consumer, BaseMembers, &M,
                          cast<Module>(DC), IsTypeLookup);
}

namespace {

struct FindLocalVal : public StmtVisitor<FindLocalVal> {
  SourceLoc Loc;
  VisibleDeclConsumer &Consumer;

  FindLocalVal(SourceLoc Loc, VisibleDeclConsumer &Consumer)
    : Loc(Loc), Consumer(Consumer) {}

  bool IntersectsRange(SourceRange R) {
    return R.Start.Value.getPointer() <= Loc.Value.getPointer() &&
           R.End.Value.getPointer() >= Loc.Value.getPointer();
  }

  void checkValueDecl(ValueDecl *D) {
    Consumer.foundDecl(D);
  }

  void checkPattern(Pattern *Pat) {
    switch (Pat->getKind()) {
    case PatternKind::Tuple:
      for (auto &field : cast<TuplePattern>(Pat)->getFields())
        checkPattern(field.getPattern());
      return;
    case PatternKind::Paren:
      return checkPattern(cast<ParenPattern>(Pat)->getSubPattern());
    case PatternKind::Typed:
      return checkPattern(cast<TypedPattern>(Pat)->getSubPattern());
    case PatternKind::Named:
      return checkValueDecl(cast<NamedPattern>(Pat)->getDecl());
    // Handle non-vars.
    case PatternKind::Any:
      return;
    }
  }

  void checkGenericParams(GenericParamList *Params) {
    if (!Params)
      return;

    for (auto P : *Params)
      checkValueDecl(P.getDecl());
  }

  void checkTranslationUnit(TranslationUnit *TU) {
    for (Decl *D : TU->Decls) {
      if (TopLevelCodeDecl *TLCD = dyn_cast<TopLevelCodeDecl>(D)) {
        if (Stmt *S = TLCD->getBody().dyn_cast<Stmt*>())
          visit(S);
      }
    }
  }

  void visitBreakStmt(BreakStmt *) {}
  void visitContinueStmt(ContinueStmt *) {}
  void visitAssignStmt(AssignStmt *) {}
  void visitReturnStmt(ReturnStmt *) {}
  void visitIfStmt(IfStmt * S) {
    visit(S->getThenStmt());
    if (S->getElseStmt())
      visit(S->getElseStmt());
  }
  void visitWhileStmt(WhileStmt *S) {
    visit(S->getBody());
  }
  void visitDoWhileStmt(DoWhileStmt *S) {
    visit(S->getBody());
  }

  void visitForStmt(ForStmt *S) {
    if (!IntersectsRange(S->getSourceRange()))
      return;
    visit(S->getBody());
    for (Decl *D : S->getInitializerVarDecls()) {
      if (ValueDecl *VD = dyn_cast<ValueDecl>(D))
        checkValueDecl(VD);
    }
  }
  void visitForEachStmt(ForEachStmt *S) {
    if (!IntersectsRange(S->getSourceRange()))
      return;
    visit(S->getBody());
    checkPattern(S->getPattern());
  }
  void visitBraceStmt(BraceStmt *S) {
    if (!IntersectsRange(S->getSourceRange()))
      return;
    for (auto elem : S->getElements()) {
      if (Stmt *S = elem.dyn_cast<Stmt*>())
        visit(S);
    }
    for (auto elem : S->getElements()) {
      if (Decl *D = elem.dyn_cast<Decl*>()) {
        if (ValueDecl *VD = dyn_cast<ValueDecl>(D))
          checkValueDecl(VD);
      }
    }
  }
  
  void visitSwitchStmt(SwitchStmt *S) {
    if (!IntersectsRange(S->getSourceRange()))
      return;
    for (CaseStmt *C : S->getCases()) {
      visit(C);
    }
  }
  
  void visitCaseStmt(CaseStmt *S) {
    if (!IntersectsRange(S->getSourceRange()))
      return;

    // TODO: Check patterns in pattern-matching case.
    visit(S->getBody());
  }
};
  
} // end anonymous namespace

void swift::lookupVisibleDecls(VisibleDeclConsumer &Consumer,
                               DeclContext *DC,
                               SourceLoc Loc,
                               bool IsTypeLookup) {
  DeclContext *ModuleDC = DC;
  while (!ModuleDC->isModuleContext())
    ModuleDC = ModuleDC->getParent();

  Module &M = *cast<Module>(ModuleDC);

  // If we are inside of a method, check to see if there are any ivars in scope,
  // and if so, whether this is a reference to one of them.
  while (!DC->isModuleContext()) {
    ValueDecl *BaseDecl = 0;
    ValueDecl *MetaBaseDecl = 0;
    GenericParamList *GenericParams = nullptr;
    Type ExtendedType;
    if (FuncExpr *FE = dyn_cast<FuncExpr>(DC)) {
      // Look for local variables; normally, the parser resolves these
      // for us, but it can't do the right thing inside local types.
      if (Loc.isValid()) {
        FindLocalVal(Loc, Consumer).visit(FE->getBody());
      }

      FuncDecl *FD = FE->getDecl();
      if (FD && FD->getExtensionType()) {
        ExtendedType = FD->getExtensionType();
        BaseDecl = FD->getImplicitThisDecl();
        if (NominalType *NT = ExtendedType->getAs<NominalType>())
          MetaBaseDecl = NT->getDecl();
        else if (auto UGT = ExtendedType->getAs<UnboundGenericType>())
          MetaBaseDecl = UGT->getDecl();
        DC = DC->getParent();

        if (FD->isStatic())
          ExtendedType = MetaTypeType::get(ExtendedType, M.getASTContext());
      }

      // Look in the generic parameters after checking our local declaration.
      if (FD)
        GenericParams = FD->getGenericParams();

    } else if (ExtensionDecl *ED = dyn_cast<ExtensionDecl>(DC)) {
      ExtendedType = ED->getExtendedType();
      if (NominalType *NT = ExtendedType->getAs<NominalType>())
        BaseDecl = NT->getDecl();
      else if (auto UGT = ExtendedType->getAs<UnboundGenericType>())
        BaseDecl = UGT->getDecl();
      MetaBaseDecl = BaseDecl;
    } else if (NominalTypeDecl *ND = dyn_cast<NominalTypeDecl>(DC)) {
      ExtendedType = ND->getDeclaredType();
      BaseDecl = ND;
      MetaBaseDecl = BaseDecl;
    } else if (ConstructorDecl *CD = dyn_cast<ConstructorDecl>(DC)) {
      // Look for local variables; normally, the parser resolves these
      // for us, but it can't do the right thing inside local types.
      if (Loc.isValid()) {
        FindLocalVal(Loc, Consumer).visit(CD->getBody());
      }

      BaseDecl = CD->getImplicitThisDecl();
      ExtendedType = CD->getDeclContext()->getDeclaredTypeOfContext();
      if (NominalType *NT = ExtendedType->getAs<NominalType>())
        MetaBaseDecl = NT->getDecl();
      else if (auto UGT = ExtendedType->getAs<UnboundGenericType>())
        MetaBaseDecl = UGT->getDecl();
      DC = DC->getParent();
    } else if (DestructorDecl *DD = dyn_cast<DestructorDecl>(DC)) {
      // Look for local variables; normally, the parser resolves these
      // for us, but it can't do the right thing inside local types.
      if (Loc.isValid()) {
        FindLocalVal(Loc, Consumer).visit(CD->getBody());
      }

      BaseDecl = DD->getImplicitThisDecl();
      ExtendedType = DD->getDeclContext()->getDeclaredTypeOfContext();
      if (NominalType *NT = ExtendedType->getAs<NominalType>())
        MetaBaseDecl = NT->getDecl();
      else if (auto UGT = ExtendedType->getAs<UnboundGenericType>())
        MetaBaseDecl = UGT->getDecl();
      DC = DC->getParent();
    }

    if (BaseDecl) {
      lookupVisibleMemberDecls(ExtendedType, Consumer, M, IsTypeLookup);
    }

    // Check the generic parameters for something with the given name.
    if (GenericParams) {
      FindLocalVal(Loc, Consumer).checkGenericParams(GenericParams);
    }

    DC = DC->getParent();
  }

  if (Loc.isValid()) {
    if (TranslationUnit *TU = dyn_cast<TranslationUnit>(&M)) {
      // Look for local variables in top-level code; normally, the parser
      // resolves these for us, but it can't do the right thing for
      // local types.
      FindLocalVal(Loc, Consumer).checkTranslationUnit(TU);
    }
  }

  // Track whether we've already searched the Clang modules.
  // FIXME: This is a weird hack. We either need to filter within the
  // Clang module importer, or we need to change how this works.
  bool searchedClangModule = false;
  
  // Do a local lookup within the current module.
  M.lookupVisibleDecls(Module::AccessPathTy(), Consumer,
                       NLKind::UnqualifiedLookup);
  searchedClangModule = isa<ClangModule>(&M);

  // The builtin module has no imports.
  if (isa<BuiltinModule>(M)) return;
  
  TranslationUnit &TU = cast<TranslationUnit>(M);

  // Scrape through all of the imports looking for additional results.
  // FIXME: Implement DAG-based shadowing rules.
  llvm::SmallPtrSet<Module *, 16> Visited;
  for (auto &ImpEntry : TU.getImportedModules()) {
    if (!Visited.insert(ImpEntry.second))
      continue;

    // FIXME: Only searching Clang modules once.
    if (isa<ClangModule>(ImpEntry.second)) {
      if (searchedClangModule)
        continue;

      searchedClangModule = true;
    }

    ImpEntry.second->lookupVisibleDecls(ImpEntry.first, Consumer,
                                        NLKind::UnqualifiedLookup);
  }

  // Look for a module with the given name.
  // FIXME: Modules aren't ValueDecls
  //Consumer.foundDecl(&M);
  //for (const auto &ImpEntry : TU.getImportedModules())
  //  Consumer.foundDecl(&ImpEntry.second);
}

void swift::lookupVisibleDecls(VisibleDeclConsumer &Consumer, Type BaseTy,
                               bool IsTypeLookup) {
  swift::NominalTypeDecl *ntd = nullptr;
  
  if (LValueType *lvt = BaseTy->getAs<LValueType>())
    ntd = lvt->getObjectType()->getNominalOrBoundGenericNominal();
  else if (MetaTypeType *mtt = BaseTy->getAs<MetaTypeType>())
    ntd = mtt->getInstanceType()->getNominalOrBoundGenericNominal();
  else
    ntd = BaseTy->getNominalOrBoundGenericNominal();

  if (!ntd)
    return;
  
  DeclContext *ModuleDC = ntd->getDeclContext();
  while (!ModuleDC->isModuleContext())
    ModuleDC = ModuleDC->getParent();
  
  Module &M = *cast<Module>(ModuleDC);

  lookupVisibleMemberDecls(BaseTy, Consumer, M, IsTypeLookup);
}
