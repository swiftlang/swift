//===--- NameLookup.cpp - Swift Name Lookup Routines ----------------------===//
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
// This file implements interfaces for performing name lookup.
//
//===----------------------------------------------------------------------===//

#include "ModuleNameLookup.h"
#include "swift/Basic/SourceManager.h"
#include "swift/AST/LazyResolver.h"
#include "swift/AST/DebuggerClient.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/AST.h"
#include "swift/AST/ASTVisitor.h"
#include "swift/Basic/Fallthrough.h"
#include "swift/Basic/STLExtras.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/TinyPtrVector.h"

using namespace swift;

void DebuggerClient::anchor() {}

template <typename Fn>
static void forAllVisibleModules(const DeclContext *DC, const Fn &fn) {
  DeclContext *moduleScope = DC->getModuleScopeContext();
  if (auto file = dyn_cast<FileUnit>(moduleScope))
    file->forAllVisibleModules(fn);
  else
    cast<Module>(moduleScope)->forAllVisibleModules(Module::AccessPathTy(), fn);
}


void swift::removeShadowedDecls(SmallVectorImpl<ValueDecl*> &decls,
                                const Module *curModule,
                                LazyResolver *typeResolver) {
  // Category declarations by their signatures.
  llvm::SmallDenseMap<std::pair<CanType, Identifier>,
                      llvm::TinyPtrVector<ValueDecl *>>
    CollidingDeclGroups;

  /// Objective-C initializers are tracked by their context type and
  /// full name.
  llvm::SmallDenseMap<std::pair<CanType, DeclName>, 
                      llvm::TinyPtrVector<ConstructorDecl *>>
    ObjCCollidingConstructors;
  bool anyCollisions = false;
  for (auto decl : decls) {
    // Determine the signature of this declaration.
    // FIXME: the canonical type makes a poor signature, because we don't
    // canonicalize away default arguments and don't canonicalize polymorphic
    // types well.
    CanType signature;

    // FIXME: Egregious hack to avoid failing when there are no declared types.
    if (!decl->hasType() || isa<TypeAliasDecl>(decl) || isa<AbstractTypeParamDecl>(decl)) {
      // FIXME: Pass this down instead of getting it from the ASTContext.
      if (typeResolver)
        typeResolver->resolveDeclSignature(decl);
      if (!decl->hasType())
        continue;
    }

    signature = decl->getType()->getCanonicalType();

    // If we've seen a declaration with this signature before, note it.
    auto &knownDecls =
        CollidingDeclGroups[std::make_pair(signature, decl->getName())];
    if (!knownDecls.empty())
      anyCollisions = true;

    knownDecls.push_back(decl);

    // Specifically keep track of Objective-C initializers, which can come from
    // either init methods or factory methods.
    if (decl->hasClangNode()) {
      if (auto ctor = dyn_cast<ConstructorDecl>(decl)) {
        auto ctorSignature
          = std::make_pair(ctor->getExtensionType()->getCanonicalType(),
                           decl->getFullName());
        auto &knownCtors = ObjCCollidingConstructors[ctorSignature];
        if (!knownCtors.empty())
          anyCollisions = true;
        knownCtors.push_back(ctor);
      }
    }
  }

  // If there were no signature collisions, there is nothing to do.
  if (!anyCollisions)
    return;

  // Determine the set of declarations that are shadowed by other declarations.
  llvm::SmallPtrSet<ValueDecl *, 4> shadowed;
  for (auto &collidingDecls : CollidingDeclGroups) {
    // If only one declaration has this signature, it isn't shadowed by
    // anything.
    if (collidingDecls.second.size() == 1)
      continue;

    // Compare each declaration to every other declaration. This is
    // unavoidably O(n^2) in the number of declarations, but because they
    // all have the same signature, we expect n to remain small.
    for (unsigned firstIdx = 0, n = collidingDecls.second.size();
         firstIdx != n; ++firstIdx) {
      auto firstDecl = collidingDecls.second[firstIdx];
      auto firstDC = firstDecl->getDeclContext();
      auto firstModule = firstDecl->getModuleContext();
      for (unsigned secondIdx = firstIdx + 1; secondIdx != n; ++secondIdx) {
        // Determine whether one module takes precedence over another.
        auto secondDecl = collidingDecls.second[secondIdx];
        auto secondModule = secondDecl->getModuleContext();
         
        // If one declaration is available and the other is not, prefer the
        // available one.
        if (firstDecl->getAttrs().isUnavailable() !=
              secondDecl->getAttrs().isUnavailable()) {
         if (firstDecl->getAttrs().isUnavailable()) {
           shadowed.insert(firstDecl);
           break;
         } else {
           shadowed.insert(secondDecl);
           continue;
         }
        }
         
        // If the first and second declarations are in the same module,
        // prefer one in the type itself vs. one in an extension.
        // FIXME: Should redeclaration checking prevent this from happening?
        if (firstModule == secondModule) {
          auto secondDC = secondDecl->getDeclContext();

          // If both declarations are in extensions, or both are in the
          // type definition itself, there's nothing we can do.
          if (isa<ExtensionDecl>(firstDC) == isa<ExtensionDecl>(secondDC))
            continue;

          // If the second declaration is in an extension, it is shadowed
          // by the first declaration. 
          if (isa<ExtensionDecl>(secondDC)) {
            shadowed.insert(secondDecl);
            continue;
          }

          // If the first declaration is in an extension, it is shadowed by
          // the second declaration. There is no point in continuing to compare
          // the first declaration to others.
          shadowed.insert(firstDecl);
          break;
        }

        // Prefer declarations in the current module over those in another
        // module.
        // FIXME: This is a hack. We should query a (lazily-built, cached)
        // module graph to determine shadowing.
        if ((firstModule == curModule) == (secondModule == curModule))
          continue;

        // If the first module is the current module, the second declaration
        // is shadowed by the first.
        if (firstModule == curModule) {
          shadowed.insert(secondDecl);
          continue;
        }

        // Otherwise, the first declaration is shadowed by the second. There is
        // no point in continuing to compare the first declaration to others.
        shadowed.insert(firstDecl);
        break;
      }
    }
  }
  
  // Check for collisions among Objective-C initializers. When such collisions
  // exist, we pick the
  for (const auto &colliding : ObjCCollidingConstructors) {
    if (colliding.second.size() == 1)
      continue;

    // Find the "best" constructor kind with this signature.
    CtorInitializerKind bestKind = colliding.second[0]->getInitKind();
    for (auto ctor : colliding.second) {
      auto kind = ctor->getInitKind();
      if (static_cast<unsigned>(kind) < static_cast<unsigned>(bestKind))
        bestKind = kind;
    }

    // Shadow any initializers with a worse kind.
    for (auto ctor : colliding.second) {
      auto kind = ctor->getInitKind();
      if (static_cast<unsigned>(kind) > static_cast<unsigned>(bestKind))
        shadowed.insert(ctor);
    }
  }

  // If none of the declarations were shadowed, we're done.
  if (shadowed.empty())
    return;

  // Remove shadowed declarations from the list of declarations.
  decls.erase(std::remove_if(decls.begin(), decls.end(),
                             [&](ValueDecl *vd) {
                               return shadowed.count(vd) > 0;
                             }),
              decls.end());
}

struct FindLocalVal : public StmtVisitor<FindLocalVal> {
  const SourceManager &SM;
  SourceLoc Loc;
  DeclName Name;
  ValueDecl *MatchingValue;

  FindLocalVal(const SourceManager &SM, SourceLoc Loc, DeclName Name)
      : SM(SM), Loc(Loc), Name(Name), MatchingValue(nullptr) {}

  bool IntersectsRange(SourceRange R) {
    return SM.rangeContainsTokenLoc(R, Loc);
  }

  void checkValueDecl(ValueDecl *D) {
    if (D->getFullName().matchesRef(Name)) {
      assert(!MatchingValue);
      MatchingValue = D;
    }
  }

  void checkPattern(const Pattern *Pat) {
    switch (Pat->getKind()) {
    case PatternKind::Tuple:
      for (auto &field : cast<TuplePattern>(Pat)->getFields())
        checkPattern(field.getPattern());
      return;
    case PatternKind::Paren:
    case PatternKind::Typed:
    case PatternKind::Var:
      return checkPattern(Pat->getSemanticsProvidingPattern());

    case PatternKind::Named:
      return checkValueDecl(cast<NamedPattern>(Pat)->getDecl());
    case PatternKind::NominalType: {
      for (const auto &elt : cast<NominalTypePattern>(Pat)->getElements())
        checkPattern(elt.getSubPattern());
      return;
    }
    case PatternKind::EnumElement: {
      auto *OP = cast<EnumElementPattern>(Pat);
      if (OP->hasSubPattern())
        checkPattern(OP->getSubPattern());
      return;
    }
    // Handle non-vars.
    case PatternKind::Isa:
    case PatternKind::Expr:
    case PatternKind::Any:
      return;
    }
  }

  void checkGenericParams(GenericParamList *Params) {
    if (!Params)
      return;

    for (auto P : *Params) {
      checkValueDecl(P.getDecl());
    }
  }

  void checkSourceFile(const SourceFile &SF) {
    for (Decl *D : SF.Decls) {
      if (TopLevelCodeDecl *TLCD = dyn_cast<TopLevelCodeDecl>(D))
        visit(TLCD->getBody());
    }
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
  void visitIfConfigStmt(IfConfigStmt * S) {
    // Active members are attached to the enclosing declaration, so there's no
    // need to walk anything within.
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
    if (MatchingValue)
      return;
    for (Decl *D : S->getInitializerVarDecls()) {
      if (ValueDecl *VD = dyn_cast<ValueDecl>(D))
        checkValueDecl(VD);
    }
  }
  void visitForEachStmt(ForEachStmt *S) {
    if (!IntersectsRange(S->getSourceRange()))
      return;
    visit(S->getBody());
    if (MatchingValue)
      return;
    checkPattern(S->getPattern());
  }
  void visitBraceStmt(BraceStmt *S) {
    // We'll want to look inside any active config blocks, even if the ranges
    // do not intersect.  (This is to accurately model how an active config
    // block "shares" its enclosing scope.)
    if (!IntersectsRange(S->getSourceRange()) &&
        (!S->isConfigBlock() || S->isInactiveConfigBlock()))
      return;
    for (auto elem : S->getElements()) {
      if (Stmt *S = elem.dyn_cast<Stmt*>())
        visit(S);
    }
    if (MatchingValue)
      return;
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
    for (const auto &CLI : S->getCaseLabelItems()) {
      auto *P = CLI.getPattern();
      if (!IntersectsRange(P->getSourceRange()))
        checkPattern(P);
    }
    visit(S->getBody());
  }
};


UnqualifiedLookup::UnqualifiedLookup(DeclName Name, DeclContext *DC,
                                     LazyResolver *TypeResolver,
                                     SourceLoc Loc, bool IsTypeLookup) {
  typedef UnqualifiedLookupResult Result;

  Module &M = *DC->getParentModule();
  ASTContext &Ctx = M.getASTContext();
  const SourceManager &SM = Ctx.SourceMgr;

  // Never perform local lookup for operators.
  if (Name.isOperator())
    DC = DC->getModuleScopeContext();

  // If we are inside of a method, check to see if there are any ivars in scope,
  // and if so, whether this is a reference to one of them.
  while (!DC->isModuleScopeContext()) {
    ValueDecl *BaseDecl = 0;
    ValueDecl *MetaBaseDecl = 0;
    GenericParamList *GenericParams = nullptr;
    Type ExtendedType;
    if (auto *AFD = dyn_cast<AbstractFunctionDecl>(DC)) {
      // Look for local variables; normally, the parser resolves these
      // for us, but it can't do the right thing inside local types.
      // FIXME: when we can parse and typecheck the function body partially for
      // code completion, AFD->getBody() check can be removed.
      if (Loc.isValid() && AFD->getBody()) {
        FindLocalVal localVal(SM, Loc, Name);
        localVal.visit(AFD->getBody());
        if (!localVal.MatchingValue) {
          for (Pattern *P : AFD->getBodyParamPatterns())
            localVal.checkPattern(P);
        }
        if (localVal.MatchingValue) {
          Results.push_back(Result::getLocalDecl(localVal.MatchingValue));
          return;
        }
      }

      if (AFD->getExtensionType()) {
        ExtendedType = AFD->getExtensionType();
        BaseDecl = AFD->getImplicitSelfDecl();
        MetaBaseDecl = ExtendedType->getAnyNominal();
        DC = DC->getParent();

        if (auto *FD = dyn_cast<FuncDecl>(AFD))
          if (FD->isStatic())
            ExtendedType = MetatypeType::get(ExtendedType);

        // If we're not in the body of the function, the base declaration
        // is the nominal type, not 'self'.
        if (Loc.isValid() &&
            AFD->getBodySourceRange().isValid() &&
            !SM.rangeContainsTokenLoc(AFD->getBodySourceRange(), Loc)) {
          BaseDecl = MetaBaseDecl;
        }
      }

      // Look in the generic parameters after checking our local declaration.
      GenericParams = AFD->getGenericParams();
    } else if (auto *ACE = dyn_cast<AbstractClosureExpr>(DC)) {
      // Look for local variables; normally, the parser resolves these
      // for us, but it can't do the right thing inside local types.
      if (Loc.isValid()) {
        if (auto *CE = dyn_cast<ClosureExpr>(ACE)) {
          FindLocalVal localVal(SM, Loc, Name);
          localVal.visit(CE->getBody());
          if (!localVal.MatchingValue) {
            localVal.checkPattern(CE->getParams());
          }
          if (localVal.MatchingValue) {
            Results.push_back(Result::getLocalDecl(localVal.MatchingValue));
            return;
          }
        }
      }
    } else if (ExtensionDecl *ED = dyn_cast<ExtensionDecl>(DC)) {
      ExtendedType = ED->getExtendedType();
      BaseDecl = ExtendedType->getNominalOrBoundGenericNominal();
      MetaBaseDecl = BaseDecl;
    } else if (NominalTypeDecl *ND = dyn_cast<NominalTypeDecl>(DC)) {
      ExtendedType = ND->getDeclaredType();
      BaseDecl = ND;
      MetaBaseDecl = BaseDecl;
    } else if (auto I = dyn_cast<DefaultArgumentInitializer>(DC)) {
      // In a default argument, skip immediately out of both the
      // initializer and the function.
      DC = I->getParent()->getParent();
      continue;
    }

    // Check the generic parameters for something with the given name.
    if (GenericParams) {
      FindLocalVal localVal(SM, Loc, Name);
      localVal.checkGenericParams(GenericParams);

      if (localVal.MatchingValue) {
        Results.push_back(Result::getLocalDecl(localVal.MatchingValue));
        return;
      }
    }

    if (BaseDecl) {
      if (TypeResolver)
        TypeResolver->resolveDeclSignature(BaseDecl);

      SmallVector<ValueDecl *, 4> Lookup;
      DC->lookupQualified(ExtendedType, Name, NL_UnqualifiedDefault,
                          TypeResolver, Lookup);
      bool isMetatypeType = ExtendedType->is<AnyMetatypeType>();
      bool FoundAny = false;
      for (auto Result : Lookup) {
        // If we're looking into an instance, skip static functions.
        if (!isMetatypeType &&
            isa<FuncDecl>(Result) &&
            cast<FuncDecl>(Result)->isStatic())
          continue;

        // Classify this declaration.
        FoundAny = true;

        // Types are local or metatype members.
        if (auto TD = dyn_cast<TypeDecl>(Result)) {
          if (isa<GenericTypeParamDecl>(TD))
            Results.push_back(Result::getLocalDecl(Result));
          else
            Results.push_back(Result::getMetatypeMember(MetaBaseDecl, Result));
          continue;
        }

        // Functions are either metatype members or member functions.
        if (auto FD = dyn_cast<FuncDecl>(Result)) {
          if (FD->isStatic()) {
            if (isMetatypeType)
              Results.push_back(Result::getMetatypeMember(BaseDecl, Result));
          } else {
            Results.push_back(Result::getMemberFunction(BaseDecl, Result));
          }
          continue;
        }

        if (isa<EnumElementDecl>(Result)) {
          Results.push_back(Result::getMetatypeMember(MetaBaseDecl, Result));
          continue;
        }

        // Archetype members
        if (ExtendedType->is<ArchetypeType>()) {
          Results.push_back(Result::getArchetypeMember(BaseDecl, Result));
          continue;
        }

        // Existential members.
        if (ExtendedType->isExistentialType()) {
          Results.push_back(Result::getExistentialMember(BaseDecl, Result));
          continue;
        }

        // Everything else is a member property.
        Results.push_back(Result::getMemberProperty(BaseDecl, Result));
      }

      if (FoundAny)
        return;

      // Check the generic parameters for something with the given name.
      auto nominal = isMetatypeType
                       ? ExtendedType->castTo<AnyMetatypeType>()
                           ->getInstanceType()->getAnyNominal()
                       : ExtendedType->getAnyNominal();
      if (nominal && nominal->getGenericParams()) {
        FindLocalVal localVal(SM, Loc, Name);
        localVal.checkGenericParams(nominal->getGenericParams());

        if (localVal.MatchingValue) {
          Results.push_back(Result::getLocalDecl(localVal.MatchingValue));
          return;
        }
      }
    }

    DC = DC->getParent();
  }

  if (auto SF = dyn_cast<SourceFile>(DC)) {
    if (Loc.isValid()) {
      // Look for local variables in top-level code; normally, the parser
      // resolves these for us, but it can't do the right thing for
      // local types.
      FindLocalVal localVal(SM, Loc, Name);
      localVal.checkSourceFile(*SF);
      if (localVal.MatchingValue) {
        Results.push_back(Result::getLocalDecl(localVal.MatchingValue));
        return;
      }
    }
  }

  // Add private imports to the extra search list.
  SmallVector<Module::ImportedModule, 8> extraImports;
  if (auto FU = dyn_cast<FileUnit>(DC))
    FU->getImportedModules(extraImports, Module::ImportFilter::Private);

  DebuggerClient *DebugClient = M.getDebugClient();
  // TODO: Does the debugger client care about compound names?
  if (Name.isSimpleName()
      && DebugClient && DebugClient->lookupOverrides(Name.getBaseName(), DC,
                                                   Loc, IsTypeLookup, Results))
    return;

  using namespace namelookup;
  SmallVector<ValueDecl *, 8> CurModuleResults;
  auto resolutionKind =
    IsTypeLookup ? ResolutionKind::TypesOnly : ResolutionKind::Overloadable;
  lookupInModule(&M, {}, Name, CurModuleResults, NLKind::UnqualifiedLookup,
                 resolutionKind, TypeResolver, extraImports);

  for (auto VD : CurModuleResults)
    Results.push_back(Result::getModuleMember(VD));

  // If we've found something, we're done.
  if (!Results.empty())
    return;

  // Look for a module with the given name.
  if (Name.isSimpleName(M.Name)) {
    Results.push_back(Result::getModuleName(&M));
    return;
  }

  forAllVisibleModules(DC, [&](const Module::ImportedModule &import) -> bool {
    if (Name.isSimpleName(import.second->Name)) {
      Results.push_back(Result::getModuleName(import.second));
      return false;
    }
    return true;
  });

  if (Name.isSimpleName() && DebugClient && !Results.size())
    DebugClient->lookupFallbacks(Name.getBaseName(), DC, Loc, IsTypeLookup,
                                 Results);
}

Optional<UnqualifiedLookup>
UnqualifiedLookup::forModuleAndName(ASTContext &C,
                                    StringRef Mod, StringRef Name) {
  auto foundModule = C.LoadedModules.find(Mod);
  if (foundModule == C.LoadedModules.end())
    return Nothing;

  Module *m = foundModule->second;
  return UnqualifiedLookup(C.getIdentifier(Name), m, nullptr);
}

TypeDecl* UnqualifiedLookup::getSingleTypeResult() {
  if (Results.size() != 1 || !Results.back().hasValueDecl() ||
      !isa<TypeDecl>(Results.back().getValueDecl()))
    return nullptr;
  return cast<TypeDecl>(Results.back().getValueDecl());
}

#pragma mark Member lookup table

void LazyMemberLoader::anchor() {}

/// Lookup table used to store members of a nominal type (and its extensions)
/// for fast retrieval.
class swift::MemberLookupTable {
  /// The last extension that was included within the member lookup table's
  /// results.
  ExtensionDecl *LastExtensionIncluded = nullptr;

  /// The type of the internal lookup table.
  typedef llvm::DenseMap<DeclName, llvm::TinyPtrVector<ValueDecl *>>
    LookupTable;

  /// Lookup table mapping names to the set of declarations with that name.
  LookupTable Lookup;

public:
  /// Create a new member lookup table.
  explicit MemberLookupTable(ASTContext &ctx);

  /// Destroy the lookup table.
  void destroy();

  /// Update a lookup table with members from newly-added extensions.
  void updateLookupTable(NominalTypeDecl *nominal);

  /// \brief Add the given member to the lookup tabke.
  void addMember(Decl *members);

  /// \brief Add the given members to the lookup table.
  void addMembers(DeclRange members);

  /// \brief The given extension has been extended with new members; add them
  /// if appropriate.
  void addExtensionMembers(NominalTypeDecl *nominal,
                           ExtensionDecl *ext,
                           DeclRange members);

  /// Iterator into the lookup table.
  typedef LookupTable::iterator iterator;

  iterator begin() { return Lookup.begin(); }
  iterator end() { return Lookup.end(); }

  iterator find(DeclName name) {
    return Lookup.find(name);
  }

  // Only allow allocation of member lookup tables using the allocator in
  // ASTContext or by doing a placement new.
  void *operator new(size_t Bytes, ASTContext &C,
                     unsigned Alignment = alignof(MemberLookupTable)) {
    return C.Allocate(Bytes, Alignment);
  }
  void *operator new(size_t Bytes, void *Mem) {
    assert(Mem);
    return Mem;
  }
};

/// Class member lookup table, which is a member lookup table with a second
/// table for lookup based on Objective-C selector.
class swift::ObjCMemberLookupTable
        : public llvm::DenseMap<ObjCSelector, llvm::TinyPtrVector<ValueDecl *>>
{
public:
  void destroy() {
    this->~ObjCMemberLookupTable();
  }

  // Only allow allocation of member lookup tables using the allocator in
  // ASTContext or by doing a placement new.
  void *operator new(size_t Bytes, ASTContext &C,
                     unsigned Alignment = alignof(MemberLookupTable)) {
    return C.Allocate(Bytes, Alignment);
  }
  void *operator new(size_t Bytes, void *Mem) {
    assert(Mem);
    return Mem;
  }
};

MemberLookupTable::MemberLookupTable(ASTContext &ctx) {
  // Register a cleanup with the ASTContext to call the lookup table
  // destructor.
  ctx.addCleanup([this]() {
    this->destroy();
  });
}

void MemberLookupTable::addMember(Decl *member) {
  // Only value declarations matter.
  auto vd = dyn_cast<ValueDecl>(member);
  if (!vd)
    return;

  // Unnamed entities cannot be found by name lookup.
  if (!vd->hasName())
    return;

  // If this declaration is already in the lookup table, don't add it
  // again.
  if (vd->ValueDeclBits.AlreadyInLookupTable) {
    return;
  }
  vd->ValueDeclBits.AlreadyInLookupTable = true;

  // Add this declaration to the lookup set under its compound name and simple
  // name.
  vd->getFullName().addToLookupTable(Lookup, vd);
}

void MemberLookupTable::addMembers(DeclRange members) {
  for (auto member : members) {
    addMember(member);
  }
}

void MemberLookupTable::addExtensionMembers(NominalTypeDecl *nominal,
                                            ExtensionDecl *ext,
                                            DeclRange members) {
  // We have not processed any extensions yet, so there's nothing to do.
  if (!LastExtensionIncluded)
    return;

  // If this extension shows up in the list of extensions not yet included
  // in the lookup table, there's nothing to do.
  for (auto notIncluded = LastExtensionIncluded->NextExtension.getPointer();
       notIncluded;
       notIncluded = notIncluded->NextExtension.getPointer()) {
    if (notIncluded == ext)
      return;
  }

  // Add the new members to the lookup table.
  addMembers(members);
}

void MemberLookupTable::updateLookupTable(NominalTypeDecl *nominal) {
  // If the last extension we included is the same as the last known extension,
  // we're already up-to-date.
  if (LastExtensionIncluded == nominal->LastExtension)
    return;

  // Add members from each of the extensions that we have not yet visited.
  for (auto next = LastExtensionIncluded
                     ? LastExtensionIncluded->NextExtension.getPointer()
                     : nominal->FirstExtension;
       next;
       (LastExtensionIncluded = next,next = next->NextExtension.getPointer())) {
    addMembers(next->getMembers());
  }
}

void MemberLookupTable::destroy() {
  this->~MemberLookupTable();
}

void NominalTypeDecl::forceDelayedMemberDecls() {
  if (!hasDelayedMemberDecls())
    return;

  // Grab the delayed members and then empty the list so we don't recurse.
  auto delayedMembers = DelayedMembers;
  DelayedMembers = {};

  for (auto delayedDeclCreator : delayedMembers) {
    addMember(delayedDeclCreator());
  }
}

void NominalTypeDecl::forceDelayedProtocolDecls() {
  if (!hasDelayedProtocolDecls())
    return;
  
  SmallVector<ProtocolDecl *, 4> protocols;
  
  // Copy over any non-delayed protocols.
  if (Protocols.size()) {
    protocols.append(Protocols.begin(), Protocols.end());
    Protocols = {};
  }
  
  for (auto delayedProtocolCreator : DelayedProtocols) {
    protocols.push_back(delayedProtocolCreator());
  }
  
  setProtocols((getASTContext()).AllocateCopy(
                                       llvm::makeArrayRef(protocols)));
  
  // Set the delayed protocol list to empty so we don't attempt to re-force it
  // later.
  DelayedProtocols = {};
}

ArrayRef<ProtocolDecl *>
NominalTypeDecl::getProtocols(bool forceDelayedMembers) const {
  if (forceDelayedMembers)
    const_cast<NominalTypeDecl*>(this)->forceDelayedProtocolDecls();
  
  return Protocols;
}

void NominalTypeDecl::addedMember(Decl *member) {
  // If we have a lookup table, add the new member to it.
  if (LookupTable.getPointer()) {
    LookupTable.getPointer()->addMember(member);
  }
}

void ExtensionDecl::addedMember(Decl *member) {
  if (NextExtension.getInt()) {
    auto nominal = getExtendedType()->getAnyNominal();
    if (nominal->LookupTable.getPointer()) {
      // Make sure we have the complete list of extensions.
      // FIXME: This is completely unnecessary. We want to determine whether
      // our own extension has already been included in the lookup table.
      (void)nominal->getExtensions();

      nominal->LookupTable.getPointer()->addMember(member);
    }
  }
}

void NominalTypeDecl::prepareLookupTable() {
  // If we haven't allocated the lookup table yet, do so now.
  if (!LookupTable.getPointer()) {
    auto &ctx = getASTContext();
    LookupTable.setPointer(new (ctx) MemberLookupTable(ctx));
  }

  // If we haven't walked the member list yet to update the lookup
  // table, do so now.
  if (!LookupTable.getInt()) {
    // Note that we'll have walked the members now.
    LookupTable.setInt(true);

    // Add the members of the nominal declaration to the table.
    LookupTable.getPointer()->addMembers(getMembers());
  }

  // Update the lookup table to introduce members from extensions.
  LookupTable.getPointer()->updateLookupTable(this);
}

void NominalTypeDecl::makeMemberVisible(ValueDecl *member) {
  if (!LookupTable.getPointer()) {
    auto &ctx = getASTContext();
    LookupTable.setPointer(new (ctx) MemberLookupTable(ctx));
  }
  
  LookupTable.getPointer()->addMember(member);
}

ArrayRef<ValueDecl *> NominalTypeDecl::lookupDirect(DeclName name) {
  // Make sure we have the complete list of members (in this nominal and in all
  // extensions).
  for (auto E : getExtensions())
    (void)E->getMembers();
  (void)getMembers();

  prepareLookupTable();

  // Look for the declarations with this name.
  auto known = LookupTable.getPointer()->find(name);
  if (known == LookupTable.getPointer()->end())
    return { };

  // We found something; return it.
  return { known->second.begin(), known->second.size() };
}

void ClassDecl::createObjCMemberLookup() {
  assert(!ObjCMemberLookup && "Already have an Objective-C member table");
  auto &ctx = getASTContext();
  ObjCMemberLookup = new (ctx) ObjCMemberLookupTable();

  // Register a cleanup with the ASTContext to call the lookup table
  // destructor.
  ctx.addCleanup([this]() {
    this->ObjCMemberLookup->destroy();
  });
}

ArrayRef<ValueDecl *> ClassDecl::lookupDirect(ObjCSelector selector) {
  if (!ObjCMemberLookup) {
    createObjCMemberLookup();
  }

  auto known = ObjCMemberLookup->find(selector);
  if (known != ObjCMemberLookup->end())
    return { known->second.begin(), known->second.end() };

  // FIXME: Callback to perform lazy lookup of selectors.
  return { };
}

void ClassDecl::recordObjCMember(ValueDecl *vd) {
  if (!ObjCMemberLookup) {
    createObjCMemberLookup();
  }

  assert(vd->isObjC() && "Not an Objective-C member");

  // For variables and subscripts, record the getter and setter separately.
  if (auto storageDecl = dyn_cast<AbstractStorageDecl>(vd)) {
    auto getter = storageDecl->getGetter();
    (*ObjCMemberLookup)[getter->getObjCSelector()].push_back(getter);

    if (auto setter = storageDecl->getSetter())
      (*ObjCMemberLookup)[setter->getObjCSelector()].push_back(setter);
    return;
  }

  // For methods and initializers, record just the method or initializer.
  auto afd = cast<AbstractFunctionDecl>(vd);
  (*ObjCMemberLookup)[afd->getObjCSelector()].push_back(afd);
}

bool DeclContext::lookupQualified(Type type,
                                  DeclName name,
                                  unsigned options,
                                  LazyResolver *typeResolver,
                                  SmallVectorImpl<ValueDecl *> &decls) const {
  if (type->is<ErrorType>())
    return false;

  // Look through lvalue and inout types.
  type = type->getLValueOrInOutObjectType();

  // Look through metatypes.
  if (auto metaTy = type->getAs<AnyMetatypeType>())
    type = metaTy->getInstanceType();

  // Look through DynamicSelf.
  if (auto dynamicSelf = type->getAs<DynamicSelfType>())
    type = dynamicSelf->getSelfType();

  // Look for module references.
  if (auto moduleTy = type->getAs<ModuleType>()) {
    Module *module = moduleTy->getModule();
    // Perform the lookup in all imports of this module.
    forAllVisibleModules(this,
                         [&](const Module::ImportedModule &import) -> bool {
      using namespace namelookup;
      if (import.second != module)
        return true;
      lookupInModule(import.second, import.first, name, decls,
                     NLKind::QualifiedLookup, ResolutionKind::Overloadable,
                     typeResolver);
      // If we're able to do an unscoped lookup, we see everything. No need
      // to keep going.
      return !import.first.empty();
    });

    std::sort(decls.begin(), decls.end());
    auto afterUnique = std::unique(decls.begin(), decls.end());
    decls.erase(afterUnique, decls.end());
    return !decls.empty();
  }

  // The set of nominal type declarations we should (and have) visited.
  SmallVector<NominalTypeDecl *, 4> stack;
  llvm::SmallPtrSet<NominalTypeDecl *, 4> visited;

  // Handle nominal types.
  bool wantProtocolMembers = false;
  bool wantLookupInAllClasses = false;
  if (auto nominal = type->getAnyNominal()) {
    visited.insert(nominal);
    stack.push_back(nominal);
    
    wantProtocolMembers = (options & NL_ProtocolMembers) &&
                          !isa<ProtocolDecl>(nominal);

    // If we want dynamic lookup and we're searching in the
    // AnyObject protocol, note this for later.
    if (options & NL_DynamicLookup) {
      if (auto proto = dyn_cast<ProtocolDecl>(nominal)) {
        if (proto->isSpecificProtocol(KnownProtocolKind::AnyObject))
          wantLookupInAllClasses = true;
      }
    }
  }
  // Handle archetypes
  else if (auto archetypeTy = type->getAs<ArchetypeType>()) {
    // Look in the protocols to which the archetype conforms (always).
    for (auto proto : archetypeTy->getConformsTo())
      if (visited.insert(proto))
        stack.push_back(proto);

    // If requested, look into the superclasses of this archetype.
    if (options & NL_VisitSupertypes) {
      if (auto superclassTy = archetypeTy->getSuperclass()) {
        if (auto superclassDecl = superclassTy->getAnyNominal()) {
          if (visited.insert(superclassDecl)) {
            stack.push_back(superclassDecl);

            wantProtocolMembers = (options & NL_ProtocolMembers) &&
                                  !isa<ProtocolDecl>(superclassDecl);
          }
        }
      }
    }
  }
  // Handle protocol compositions.
  else if (auto compositionTy = type->getAs<ProtocolCompositionType>()) {
    SmallVector<ProtocolDecl *, 4> protocols;
    if (compositionTy->isExistentialType(protocols)) {
      for (auto proto : protocols) {
        if (visited.insert(proto)) {
          stack.push_back(proto);

          // If we want dynamic lookup and this is the AnyObject
          // protocol, note this for later.
          if ((options & NL_DynamicLookup) &&
              proto->isSpecificProtocol(KnownProtocolKind::AnyObject))
            wantLookupInAllClasses = true;
        }
      }
    }
  }

  // Allow filtering of the visible declarations based on various
  // criteria.
  bool onlyCompleteObjectInits = false;
  auto isAcceptableDecl = [&](NominalTypeDecl *current, Decl *decl) -> bool {
    // Filter out designated initializers, if requested.
    if (onlyCompleteObjectInits) {
      if (auto ctor = dyn_cast<ConstructorDecl>(decl)) {
        if (!ctor->isInheritable())
          return false;
      } else {
        return false;
      }
    }

    // Ignore stub implementations.
    if (auto ctor = dyn_cast<ConstructorDecl>(decl)) {
      if (ctor->hasStubImplementation())
        return false;
    }

    return true;
  };

  // Visit all of the nominal types we know about, discovering any others
  // we need along the way.
  auto &ctx = getASTContext();
  llvm::DenseMap<ProtocolDecl *, ProtocolConformance *> knownConformances;
  while (!stack.empty()) {
    auto current = stack.back();
    stack.pop_back();

    // Make sure we've resolved implicit constructors, if we need them.
    if (name.getBaseName() == ctx.Id_init && typeResolver)
      typeResolver->resolveImplicitConstructors(current);

    // Look for results within the current nominal type and its extensions.
    bool currentIsProtocol = isa<ProtocolDecl>(current);
    for (auto decl : current->lookupDirect(name)) {
      // Resolve the declaration signature when we find the
      // declaration.
      if (typeResolver && !decl->hasType()) {
        typeResolver->resolveDeclSignature(decl);

        if (!decl->hasType())
          continue;
      }

      if (isAcceptableDecl(current, decl))
        decls.push_back(decl);
    }

    // If we're not supposed to visit our supertypes, we're done.
    if ((options & NL_VisitSupertypes) == 0)
      continue;

    // Visit superclass.
    if (auto classDecl = dyn_cast<ClassDecl>(current)) {
      // If we're looking for initializers, only look at the superclass if the
      // current class permits inheritance. Even then, only find complete
      // object initializers.
      if (name.getBaseName() == ctx.Id_init) {
        if (classDecl->inheritsSuperclassInitializers(typeResolver) &&
            !classDecl->hasClangNode())
          onlyCompleteObjectInits = true;
        else
          continue;
      }

      if (auto superclassType = classDecl->getSuperclass())
        if (auto superclassDecl = superclassType->getClassOrBoundGenericClass())
          if (visited.insert(superclassDecl))
            stack.push_back(superclassDecl);
    }

    // If we're not looking at a protocol and we don't we're not supposed to
    // visit the protocols that this type conforms to, skip the next step.
    if (!wantProtocolMembers && !currentIsProtocol)
      continue;

    // Local function object used to add protocols to the stack.
    auto addProtocols = [&](ArrayRef<ProtocolDecl *> protocols) {
      for (auto proto : protocols) {
        if (visited.insert(proto)) {
          stack.push_back(proto);
        }
      }
    };

    // Add protocols from the current type.
    addProtocols(current->getProtocols());

    // Add protocols from the extensions of the current type.
    for (auto ext : current->getExtensions()) {
      addProtocols(ext->getProtocols());
    }
  }

  // If we want to perform lookup into all classes, do so now.
  if (wantLookupInAllClasses) {
    // Collect all of the visible declarations.
    SmallVector<ValueDecl *, 4> allDecls;
    forAllVisibleModules(this, [&](Module::ImportedModule import) {
      import.second->lookupClassMember(import.first, name, allDecls);
    });

    // For each declaration whose context is not something we've
    // already visited above, add it to the list of declarations.
    llvm::SmallPtrSet<ValueDecl *, 4> knownDecls;
    for (auto decl : allDecls) {
      // If the declaration has an override, name lookup will also have
      // found the overridden method. Skip this declaration, because we
      // prefer the overridden method.
      if (decl->getOverriddenDecl())
        continue;

      auto dc = decl->getDeclContext();
      auto nominal = dyn_cast<NominalTypeDecl>(dc);
      if (!nominal) {
        auto ext = cast<ExtensionDecl>(dc);
        nominal = ext->getExtendedType()->getAnyNominal();
        assert(nominal && "Couldn't find nominal type?");
      }

      // If we didn't visit this nominal type above, add this
      // declaration to the list.
      if (!visited.count(nominal) && knownDecls.insert(decl))
        decls.push_back(decl);
    }
  }

  // If we're supposed to remove overridden declarations, do so now.
  if (options & NL_RemoveOverridden) {
    // Find all of the overridden declarations.
    llvm::SmallPtrSet<ValueDecl*, 8> overridden;
    for (auto decl : decls) {
      while (auto overrides = decl->getOverriddenDecl()) {
        overridden.insert(overrides);

        // Because initializers from Objective-C base classes have greater
        // visibility than initializers written in Swift classes, we can
        // have a "break" in the set of declarations we found, where
        // C.init overrides B.init overrides A.init, but only C.init and
        // A.init are in the chain. Make sure we still remove A.init from the
        // set in this case.
        if (name.getBaseName() == ctx.Id_init) {
          decl = overrides;
          continue;
        }

        break;
      }
    }

    // If any methods were overridden, remove them from the results.
    if (!overridden.empty()) {
      decls.erase(std::remove_if(decls.begin(), decls.end(),
                                 [&](ValueDecl *decl) -> bool {
                                   return overridden.count(decl);
                                 }),
                  decls.end());
    }
  }

  // If we're supposed to remove shadowed/hidden declarations, do so now.
  if (options & NL_RemoveNonVisible) {
    removeShadowedDecls(decls, getParentModule(), typeResolver);
  }

  // We're done. Report success/failure.
  return !decls.empty();
}
