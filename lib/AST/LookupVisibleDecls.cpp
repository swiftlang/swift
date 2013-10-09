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

#include "ModuleNameLookup.h"
#include "swift/Basic/SourceManager.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/AST.h"
#include "swift/AST/ASTVisitor.h"
#include "swift/Basic/STLExtras.h"

using namespace swift;

VisibleDeclConsumer::~VisibleDeclConsumer() {
  // Anchor the vtable.
}

namespace {
enum class LookupKind {
  /// An unqualified lookup of all visible decls in a DeclContext.
  Unqualified,

  /// Lookup of all visible members of a given object (of non-metatype type).
  Qualified,

  /// Lookup of all visible members of a given metatype.
  QualifiedOnMetatype
};
} // unnamed namespace

static bool isDeclVisibleInLookupMode(ValueDecl *Member, LookupKind LK) {
  if (auto *FD = dyn_cast<FuncDecl>(Member)) {
    // Can not call static functions on non-metatypes.
    if (LK != LookupKind::QualifiedOnMetatype && FD->isStatic())
      return false;
  }
  if (LK == LookupKind::QualifiedOnMetatype && isa<VarDecl>(Member)) {
    // FIXME: static variables
    return false;
  }
  if (LK == LookupKind::Qualified &&
      (isa<TypeAliasDecl>(Member) || isa<AssociatedTypeDecl>(Member) ||
       isa<GenericTypeParamDecl>(Member))) {
    // Can only access nested typealiases with unqualified lookup or on
    // metatypes.
    // FIXME: other nominal types?  rdar://14489286
    return false;
  }

  return true;
}

static void DoGlobalExtensionLookup(Type BaseType,
                                    VisibleDeclConsumer &Consumer,
                                    ArrayRef<ValueDecl*> BaseMembers,
                                    const Module *CurModule,
                                    LookupKind LK,
                                    DeclVisibilityKind Reason,
                                    LazyResolver *TypeResolver) {
  SmallVector<ValueDecl *, 4> found;
  
  auto nominal = BaseType->getAnyNominal();
  if (!nominal)
    return;

  // Add the members from the type itself to the list of results.
  for (auto member : BaseMembers) {
    if (isDeclVisibleInLookupMode(member, LK))
      found.push_back(member);
  }

  // Look in each extension of this type.
  for (auto extension : nominal->getExtensions()) {
    for (auto member : extension->getMembers()) {
      if (auto VD = dyn_cast<ValueDecl>(member))
        if (isDeclVisibleInLookupMode(VD, LK))
          found.push_back(VD);
    }
  }

  // Handle shadowing.
  removeShadowedDecls(found, CurModule, TypeResolver);

  // Report the declarations we found to the consumer.
  for (auto VD : found)
    Consumer.foundDecl(VD, Reason);
}

/// \brief Enumerate immediate members of the type \c BaseType and its
/// extensions, as seen from the context \c CurrDC.
///
/// Don't do lookup into superclasses or implemented protocols.
static void lookupTypeMembers(Type BaseType, VisibleDeclConsumer &Consumer,
                              const DeclContext *CurrDC,
                              LookupKind LK, DeclVisibilityKind Reason,
                              LazyResolver *TypeResolver) {
  NominalTypeDecl *D = BaseType->getAnyNominal();

  bool LookupFromChildDeclContext = false;
  const DeclContext *TempDC = CurrDC;
  while (!TempDC->isModuleContext()) {
    if (TempDC == D) {
      LookupFromChildDeclContext = true;
      break;
    }
    TempDC = TempDC->getParent();
  }

  SmallVector<ValueDecl*, 2> BaseMembers;

  if (LookupFromChildDeclContext) {
    // Current decl context is contained inside 'D', so generic parameters
    // are visible.
    if (D->getGenericParams())
      for (auto Param : *D->getGenericParams())
        BaseMembers.push_back(Param.getDecl());
  }

  for (Decl *Member : D->getMembers()) {
    if (ValueDecl *VD = dyn_cast<ValueDecl>(Member)) {
      if (LK == LookupKind::QualifiedOnMetatype && isa<VarDecl>(VD))
        continue;

      BaseMembers.push_back(VD);
    }
  }
  DoGlobalExtensionLookup(BaseType, Consumer, BaseMembers,
                          CurrDC->getParentModule(), LK, Reason, TypeResolver);
}

/// Enumerate DynamicLookup declarations as seen from context \c CurrDC.
static void doDynamicLookup(VisibleDeclConsumer &Consumer,
                            const DeclContext *CurrDC, LookupKind LK) {
  class DynamicLookupConsumer : public VisibleDeclConsumer {
    VisibleDeclConsumer &ChainedConsumer;
    LookupKind LK;
    std::set<std::pair<Identifier, CanType>> FunctionsReported;
    std::set<CanType> SubscriptsReported;
    std::set<std::pair<Identifier, CanType>> PropertiesReported;

  public:
    explicit DynamicLookupConsumer(VisibleDeclConsumer &ChainedConsumer,
                                   LookupKind LK)
        : ChainedConsumer(ChainedConsumer), LK(LK) {}

    void foundDecl(ValueDecl *D, DeclVisibilityKind Reason) override {
      // If the declaration has an override, name lookup will also have found
      // the overridden method.  Skip this declaration, because we prefer the
      // overridden method.
      if (D->getOverriddenDecl())
        return;

      // Check if we already reported a decl with the same signature.
      if (auto *FD = dyn_cast<FuncDecl>(D)) {
        assert(FD->getImplicitSelfDecl() && "should not find free functions");
        (void)FD;

        // Get the type without the first uncurry level with 'self'.
        CanType T = D->getType()
                        ->castTo<AnyFunctionType>()
                        ->getResult()
                        ->getCanonicalType();

        auto Signature = std::make_pair(D->getName(), T);
        if (FunctionsReported.count(Signature))
          return;
        FunctionsReported.insert(Signature);
      } else if (isa<SubscriptDecl>(D)) {
        auto Signature = D->getType()->getCanonicalType();
        if (SubscriptsReported.count(Signature))
          return;
        SubscriptsReported.insert(Signature);
      } else if (isa<VarDecl>(D)) {
        auto Signature =
            std::make_pair(D->getName(), D->getType()->getCanonicalType());
        if (PropertiesReported.count(Signature))
          return;
        PropertiesReported.insert(Signature);
      } else {
        llvm_unreachable("unhandled decl kind");
      }

      if (isDeclVisibleInLookupMode(D, LK))
        ChainedConsumer.foundDecl(D, DeclVisibilityKind::DynamicLookup);
    }
  };

  DynamicLookupConsumer ConsumerWrapper(Consumer, LK);

  CurrDC->getParentModule()->forAllVisibleModules(
      Module::AccessPathTy(), [&](Module::ImportedModule Import) {
        Import.second->lookupClassMembers(Import.first, ConsumerWrapper);
      });
}

namespace {
  typedef llvm::SmallPtrSet<TypeDecl *, 8> VisitedSet;
}

static DeclVisibilityKind getReasonForSuper(DeclVisibilityKind Reason) {
  switch (Reason) {
  case DeclVisibilityKind::MemberOfCurrentNominal:
  case DeclVisibilityKind::MemberOfSuper:
    return DeclVisibilityKind::MemberOfSuper;

  case DeclVisibilityKind::MemberOfOutsideNominal:
    return DeclVisibilityKind::MemberOfOutsideNominal;

  default:
    llvm_unreachable("should not see this kind");
  }
}

static void lookupVisibleMemberDeclsImpl(
    Type BaseTy, VisibleDeclConsumer &Consumer, const DeclContext *CurrDC,
    LookupKind LK, DeclVisibilityKind Reason, LazyResolver *TypeResolver,
    VisitedSet &Visited) {
  // Just look through l-valueness.  It doesn't affect name lookup.
  BaseTy = BaseTy->getRValueType();

  // Handle metatype references, as in "some_type.some_member".  These are
  // special and can't have extensions.
  if (MetaTypeType *MTT = BaseTy->getAs<MetaTypeType>()) {
    // The metatype represents an arbitrary named type: dig through to the
    // declared type to see what we're dealing with.
    Type Ty = MTT->getInstanceType();

    // Just perform normal dot lookup on the type see if we find extensions or
    // anything else.  For example, type SomeTy.SomeMember can look up static
    // functions, and can even look up non-static functions as well (thus
    // getting the address of the member).
    lookupVisibleMemberDeclsImpl(Ty, Consumer, CurrDC,
                                 LookupKind::QualifiedOnMetatype, Reason,
                                 TypeResolver, Visited);
    return;
  }

  // Lookup module references, as on some_module.some_member.  These are
  // special and can't have extensions.
  if (ModuleType *MT = BaseTy->getAs<ModuleType>()) {
    MT->getModule()->lookupVisibleDecls(Module::AccessPathTy(), Consumer,
                                        NLKind::QualifiedLookup);
    return;
  }

  // If the base is a protocol, enumerate its members.
  if (ProtocolType *PT = BaseTy->getAs<ProtocolType>()) {
    if (PT->getDecl()->isSpecificProtocol(KnownProtocolKind::DynamicLookup)) {
      // Handle DynamicLookup in a special way.
      doDynamicLookup(Consumer, CurrDC, LK);
      return;
    }
    if (!Visited.insert(PT->getDecl()))
      return;

    for (auto Proto : PT->getDecl()->getProtocols())
      lookupVisibleMemberDeclsImpl(Proto->getDeclaredType(), Consumer, CurrDC,
                                   LK, getReasonForSuper(Reason), TypeResolver,
                                   Visited);

    lookupTypeMembers(PT, Consumer, CurrDC, LK, Reason, TypeResolver);
    return;
  }

  // If the base is a protocol composition, enumerate members of the protocols.
  if (auto PC = BaseTy->getAs<ProtocolCompositionType>()) {
    for (auto Proto : PC->getProtocols())
      lookupVisibleMemberDeclsImpl(Proto, Consumer, CurrDC, LK, Reason,
                                   TypeResolver, Visited);
    return;
  }

  // Enumerate members of archetype's requirements.
  if (ArchetypeType *Archetype = BaseTy->getAs<ArchetypeType>()) {
    for (auto Proto : Archetype->getConformsTo())
      lookupVisibleMemberDeclsImpl(Proto->getDeclaredType(), Consumer, CurrDC,
                                   LK, getReasonForSuper(Reason),
                                   TypeResolver, Visited);

    if (auto superclass = Archetype->getSuperclass())
      lookupVisibleMemberDeclsImpl(superclass, Consumer, CurrDC, LK,
                                   getReasonForSuper(Reason), TypeResolver,
                                   Visited);
    return;
  }

  do {
    // Look in for members of a nominal type.
    lookupTypeMembers(BaseTy, Consumer, CurrDC, LK, Reason, TypeResolver);

    // If we have a class type, look into its superclass.
    ClassDecl *CurClass = nullptr;
    if (auto CT = BaseTy->getAs<ClassType>())
      CurClass = CT->getDecl();
    else if (auto BGT = BaseTy->getAs<BoundGenericType>())
      CurClass = dyn_cast<ClassDecl>(BGT->getDecl());
    else if (UnboundGenericType *UGT = BaseTy->getAs<UnboundGenericType>())
      CurClass = dyn_cast<ClassDecl>(UGT->getDecl());

    if (CurClass && CurClass->hasSuperclass()) {
      BaseTy = CurClass->getSuperclass();
      Reason = getReasonForSuper(Reason);
    } else {
      break;
    }
  } while (1);

  // FIXME: Weed out overridden methods.
}

/// \brief Enumerate all members in \c BaseTy (including members of extensions,
/// superclasses and implemented protocols), as seen from the context \c CurrDC.
///
/// This operation corresponds to a standard "dot" lookup operation like "a.b"
/// where 'self' is the type of 'a'.  This operation is only valid after name
/// binding.
static void lookupVisibleMemberDecls(Type BaseTy,
                                     VisibleDeclConsumer &Consumer,
                                     const DeclContext *CurrDC,
                                     LookupKind LK,
                                     DeclVisibilityKind Reason,
                                     LazyResolver *TypeResolver) {
  VisitedSet Visited;
  lookupVisibleMemberDeclsImpl(BaseTy, Consumer, CurrDC, LK, Reason,
                               TypeResolver, Visited);
}

namespace {

struct FindLocalVal : public StmtVisitor<FindLocalVal> {
  const SourceManager &SM;
  SourceLoc Loc;
  VisibleDeclConsumer &Consumer;

  FindLocalVal(const SourceManager &SM, SourceLoc Loc,
               VisibleDeclConsumer &Consumer)
      : SM(SM), Loc(Loc), Consumer(Consumer) {}

  bool isReferencePointInRange(SourceRange R) {
    return SM.rangeContainsTokenLoc(R, Loc);
  }

  void checkValueDecl(ValueDecl *D, DeclVisibilityKind Reason) {
    Consumer.foundDecl(D, Reason);
  }

  void checkPattern(const Pattern *Pat, DeclVisibilityKind Reason) {
    switch (Pat->getKind()) {
    case PatternKind::Tuple:
      for (auto &field : cast<TuplePattern>(Pat)->getFields())
        checkPattern(field.getPattern(), Reason);
      return;
    case PatternKind::Paren:
      return checkPattern(cast<ParenPattern>(Pat)->getSubPattern(), Reason);
    case PatternKind::Typed:
      return checkPattern(cast<TypedPattern>(Pat)->getSubPattern(), Reason);
    case PatternKind::Named:
      return checkValueDecl(cast<NamedPattern>(Pat)->getDecl(), Reason);
    case PatternKind::NominalType: {
      for (auto &elt : cast<NominalTypePattern>(Pat)->getElements())
        checkPattern(elt.getSubPattern(), Reason);
      return;
    }
    case PatternKind::EnumElement: {
      auto *OP = cast<EnumElementPattern>(Pat);
      if (OP->hasSubPattern())
        checkPattern(OP->getSubPattern(), Reason);
      return;
    }
    case PatternKind::Var:
      return checkPattern(cast<VarPattern>(Pat)->getSubPattern(), Reason);
    // Handle non-vars.
    case PatternKind::Isa:
    case PatternKind::Expr:
    case PatternKind::Any:
      return;
    }
  }

  void checkGenericParams(GenericParamList *Params,
                          DeclVisibilityKind Reason) {
    if (!Params)
      return;

    for (auto P : *Params)
      checkValueDecl(P.getDecl(), Reason);
  }

  void checkTranslationUnit(const TranslationUnit *TU) {
    for (Decl *D : TU->MainSourceFile->Decls)
      if (TopLevelCodeDecl *TLCD = dyn_cast<TopLevelCodeDecl>(D))
        visit(TLCD->getBody());
  }

  void visitBreakStmt(BreakStmt *) {}
  void visitContinueStmt(ContinueStmt *) {}
  void visitFallthroughStmt(FallthroughStmt *) {}
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
    if (!isReferencePointInRange(S->getSourceRange()))
      return;
    visit(S->getBody());
    for (Decl *D : S->getInitializerVarDecls()) {
      if (ValueDecl *VD = dyn_cast<ValueDecl>(D))
        checkValueDecl(VD, DeclVisibilityKind::LocalVariable);
    }
  }
  void visitForEachStmt(ForEachStmt *S) {
    if (!isReferencePointInRange(S->getSourceRange()))
      return;
    visit(S->getBody());
    checkPattern(S->getPattern(), DeclVisibilityKind::LocalVariable);
  }
  void visitBraceStmt(BraceStmt *S) {
    if (!isReferencePointInRange(S->getSourceRange()))
      return;
    for (auto elem : S->getElements()) {
      if (Stmt *S = elem.dyn_cast<Stmt*>())
        visit(S);
    }
    for (auto elem : S->getElements()) {
      if (Decl *D = elem.dyn_cast<Decl*>()) {
        if (ValueDecl *VD = dyn_cast<ValueDecl>(D))
          checkValueDecl(VD, DeclVisibilityKind::LocalVariable);
      }
    }
  }
  
  void visitSwitchStmt(SwitchStmt *S) {
    if (!isReferencePointInRange(S->getSourceRange()))
      return;
    for (CaseStmt *C : S->getCases()) {
      visit(C);
    }
  }
  
  void visitCaseStmt(CaseStmt *S) {
    if (!isReferencePointInRange(S->getSourceRange()))
      return;
    for (auto Label : S->getCaseLabels()) {
      for (auto P : Label->getPatterns())
        checkPattern(P, DeclVisibilityKind::LocalVariable);
    }
    visit(S->getBody());
  }
};
  
} // end anonymous namespace

void swift::lookupVisibleDecls(VisibleDeclConsumer &Consumer,
                               const DeclContext *DC,
                               LazyResolver *TypeResolver,
                               SourceLoc Loc) {
  const Module &M = *DC->getParentModule();
  const SourceManager &SM = DC->getASTContext().SourceMgr;
  auto Reason = DeclVisibilityKind::MemberOfCurrentNominal;

  // If we are inside of a method, check to see if there are any ivars in scope,
  // and if so, whether this is a reference to one of them.
  while (!DC->isModuleContext()) {
    const ValueDecl *BaseDecl = nullptr;
    const ValueDecl *MetaBaseDecl = nullptr;
    GenericParamList *GenericParams = nullptr;
    Type ExtendedType;
    if (auto *AFD = dyn_cast<AbstractFunctionDecl>(DC)) {
      // Look for local variables; normally, the parser resolves these
      // for us, but it can't do the right thing inside local types.
      // FIXME: when we can parse and typecheck the function body partially for
      // code completion, AFD->getBody() check can be removed.
      if (Loc.isValid() && AFD->getBody()) {
        FindLocalVal(SM, Loc, Consumer).visit(AFD->getBody());
      }

      for (auto *P : AFD->getBodyParamPatterns())
        FindLocalVal(SM, Loc, Consumer)
            .checkPattern(P, DeclVisibilityKind::FunctionParameter);

      // Constructors and destructors don't have 'self' in parameter patterns.
      if (isa<ConstructorDecl>(AFD) || isa<DestructorDecl>(AFD))
        Consumer.foundDecl(AFD->getImplicitSelfDecl(),
                           DeclVisibilityKind::FunctionParameter);

      if (AFD->getExtensionType()) {
        ExtendedType = AFD->getExtensionType();
        BaseDecl = AFD->getImplicitSelfDecl();
        MetaBaseDecl = ExtendedType->getAnyNominal();
        DC = DC->getParent();

        if (auto *FD = dyn_cast<FuncDecl>(AFD))
          if (FD->isStatic())
            ExtendedType = MetaTypeType::get(ExtendedType, M.getASTContext());
      }

      // Look in the generic parameters after checking our local declaration.
      GenericParams = AFD->getGenericParams();
    } else if (auto ACE = dyn_cast<AbstractClosureExpr>(DC)) {
      if (Loc.isValid()) {
        auto CE = cast<ClosureExpr>(ACE);
        FindLocalVal(SM, Loc, Consumer).visit(CE->getBody());
      }
    } else if (auto ED = dyn_cast<ExtensionDecl>(DC)) {
      ExtendedType = ED->getExtendedType();
      BaseDecl = ExtendedType->getNominalOrBoundGenericNominal();
      MetaBaseDecl = BaseDecl;
    } else if (auto ND = dyn_cast<NominalTypeDecl>(DC)) {
      ExtendedType = ND->getDeclaredType();
      BaseDecl = ND;
      MetaBaseDecl = BaseDecl;
    }

    if (BaseDecl) {
      ::lookupVisibleMemberDecls(ExtendedType, Consumer, DC,
                                 LookupKind::Unqualified, Reason,
                                 TypeResolver);
    }

    // Check the generic parameters for something with the given name.
    if (GenericParams) {
      FindLocalVal(SM, Loc, Consumer)
          .checkGenericParams(GenericParams,
                              DeclVisibilityKind::GenericParameter);
    }

    DC = DC->getParent();
    Reason = DeclVisibilityKind::MemberOfOutsideNominal;
  }

  if (auto TU = dyn_cast<TranslationUnit>(&M)) {
    if (Loc.isValid()) {
      // Look for local variables in top-level code; normally, the parser
      // resolves these for us, but it can't do the right thing for
      // local types.
      FindLocalVal(SM, Loc, Consumer).checkTranslationUnit(TU);
    }

    auto &cached = TU->getCachedVisibleDecls();
    if (!cached.empty()) {
      for (auto result : cached)
        Consumer.foundDecl(result, DeclVisibilityKind::VisibleAtTopLevel);
      return;
    }
  }

  using namespace namelookup;
  SmallVector<ValueDecl *, 0> moduleResults;
  auto &mutableM = const_cast<Module&>(M);
  lookupVisibleDeclsInModule(&mutableM, {}, moduleResults,
                             NLKind::QualifiedLookup,
                             ResolutionKind::Overloadable,
                             TypeResolver);
  for (auto result : moduleResults)
    Consumer.foundDecl(result, DeclVisibilityKind::VisibleAtTopLevel);

  if (auto TU = dyn_cast<TranslationUnit>(&M)) {
    TU->cacheVisibleDecls(std::move(moduleResults));
  }
}

void swift::lookupVisibleMemberDecls(VisibleDeclConsumer &Consumer, Type BaseTy,
                                     const DeclContext *CurrDC,
                                     LazyResolver *TypeResolver) {
  ::lookupVisibleMemberDecls(BaseTy, Consumer, CurrDC, LookupKind::Qualified,
                             DeclVisibilityKind::MemberOfCurrentNominal,
                             TypeResolver);
}
