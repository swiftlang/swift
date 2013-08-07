
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

#include "swift/Basic/SourceManager.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/AST.h"
#include "swift/AST/ASTVisitor.h"
#include "swift/Basic/STLExtras.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/TinyPtrVector.h"

using namespace swift;

void swift::removeShadowedDecls(SmallVectorImpl<ValueDecl*> &decls,
                                const Module *curModule) {
  // Category declarations by their signatures.
  llvm::SmallDenseMap<std::pair<CanType, Identifier>,
                      llvm::TinyPtrVector<ValueDecl *>>
    CollidingDeclGroups;
  bool anyCollisions = false;
  for (auto decl : decls) {
    // Determine the signature of this declaration.
    // FIXME: the canonical type makes a poor signature, because we don't
    // canonicalize away default arguments and don't canonicalize polymorphic
    // types well.
    CanType signature;
    if (isa<TypeDecl>(decl))
      signature = cast<TypeDecl>(decl)->getDeclaredType()->getCanonicalType();
    else
      signature = decl->getType()->getCanonicalType();

    // If we've seen a declaration with this signature before, note it.
    auto &knownDecls = CollidingDeclGroups[std::make_pair(signature, decl->getName())];
    if (!knownDecls.empty())
      anyCollisions = true;

    knownDecls.push_back(decl);
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
  Identifier Name;
  ValueDecl *MatchingValue;

  FindLocalVal(const SourceManager &SM, SourceLoc Loc, Identifier Name)
      : SM(SM), Loc(Loc), Name(Name), MatchingValue(nullptr) {}

  bool IntersectsRange(SourceRange R) {
    return SM.rangeContainsLoc(R, Loc);
  }

  void checkValueDecl(ValueDecl *D) {
    if (D->getName() == Name) {
      assert(!MatchingValue);
      MatchingValue = D;
    }
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
    case PatternKind::NominalType:
      return checkPattern(cast<NominalTypePattern>(Pat)->getSubPattern());
    case PatternKind::UnionElement: {
      auto *OP = cast<UnionElementPattern>(Pat);
      if (OP->hasSubPattern())
        checkPattern(OP->getSubPattern());
      return;
    }
    case PatternKind::Var:
      return checkPattern(cast<VarPattern>(Pat)->getSubPattern());
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

    for (auto P : *Params)
      checkValueDecl(P.getDecl());
  }

  void checkTranslationUnit(TranslationUnit *TU) {
    for (Decl *D : TU->Decls) {
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
    if (!IntersectsRange(S->getSourceRange()))
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
    
    // TODO: Check patterns in pattern-matching case.
    visit(S->getBody());
  }
};

UnqualifiedLookup::UnqualifiedLookup(Identifier Name, DeclContext *DC,
                                     SourceLoc Loc, bool IsTypeLookup) {
  typedef UnqualifiedLookupResult Result;

  Module &M = *DC->getParentModule();
  const SourceManager &SM = DC->getASTContext().SourceMgr;

  // Never perform local lookup for operators.
  if (Name.isOperator())
    DC = &M;

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
      // FIXME: when we can parse and typecheck the function body partially for
      // code completion, FE->getBody() check can be removed.
      if (Loc.isValid() && FE->getBody()) {
        FindLocalVal localVal(SM, Loc, Name);
        localVal.visit(FE->getBody());
        if (!localVal.MatchingValue) {
          for (Pattern *P : FE->getBodyParamPatterns())
            localVal.checkPattern(P);
        }
        if (localVal.MatchingValue) {
          Results.push_back(Result::getLocalDecl(localVal.MatchingValue));
          return;
        }
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
    } else if (PipeClosureExpr *CE = dyn_cast<PipeClosureExpr>(DC)) {
      // Look for local variables; normally, the parser resolves these
      // for us, but it can't do the right thing inside local types.
      if (Loc.isValid()) {
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
    } else if (ExtensionDecl *ED = dyn_cast<ExtensionDecl>(DC)) {
      ExtendedType = ED->getExtendedType();
      BaseDecl = ExtendedType->getNominalOrBoundGenericNominal();
      MetaBaseDecl = BaseDecl;
    } else if (NominalTypeDecl *ND = dyn_cast<NominalTypeDecl>(DC)) {
      ExtendedType = ND->getDeclaredType();
      BaseDecl = ND;
      MetaBaseDecl = BaseDecl;
    } else if (ConstructorDecl *CD = dyn_cast<ConstructorDecl>(DC)) {
      // Look for local variables; normally, the parser resolves these
      // for us, but it can't do the right thing inside local types.
      if (Loc.isValid()) {
        FindLocalVal localVal(SM, Loc, Name);
        localVal.visit(CD->getBody());
        if (!localVal.MatchingValue)
          localVal.checkPattern(CD->getArguments());
        if (localVal.MatchingValue) {
          Results.push_back(Result::getLocalDecl(localVal.MatchingValue));
          return;
        }
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
        FindLocalVal localVal(SM, Loc, Name);
        localVal.visit(CD->getBody());
        if (!localVal.MatchingValue)
          localVal.checkPattern(CD->getArguments());
        if (localVal.MatchingValue) {
          Results.push_back(Result::getLocalDecl(localVal.MatchingValue));
          return;
        }
      }

      BaseDecl = DD->getImplicitThisDecl();
      ExtendedType = DD->getDeclContext()->getDeclaredTypeOfContext();
      if (NominalType *NT = ExtendedType->getAs<NominalType>())
        MetaBaseDecl = NT->getDecl();
      else if (auto UGT = ExtendedType->getAs<UnboundGenericType>())
        MetaBaseDecl = UGT->getDecl();
      DC = DC->getParent();
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
      SmallVector<ValueDecl *, 4> Lookup;
      M.lookupQualified(ExtendedType, Name, NL_UnqualifiedDefault, Lookup);
      bool isMetatypeType = ExtendedType->is<MetaTypeType>();
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
          auto TAD = dyn_cast<TypeAliasDecl>(TD);
          if (TAD && TAD->isGenericParameter())
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

        if (isa<UnionElementDecl>(Result)) {
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
                       ? ExtendedType->castTo<MetaTypeType>()
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

  if (Loc.isValid()) {
    if (TranslationUnit *TU = dyn_cast<TranslationUnit>(&M)) {
      // Look for local variables in top-level code; normally, the parser
      // resolves these for us, but it can't do the right thing for
      // local types.
      FindLocalVal localVal(SM, Loc, Name);
      localVal.checkTranslationUnit(TU);
      if (localVal.MatchingValue) {
        Results.push_back(Result::getLocalDecl(localVal.MatchingValue));
        return;
      }
    }
  }

  // Track whether we've already searched the Clang modules.
  // FIXME: This is a weird hack. We either need to filter within the
  // Clang module importer, or we need to change how this works.
  bool searchedClangModule = false;
  
  // Do a local lookup within the current module.
  llvm::SmallVector<ValueDecl*, 4> CurModuleResults;
  M.lookupValue(Module::AccessPathTy(), Name, NLKind::UnqualifiedLookup,
                CurModuleResults);
  searchedClangModule = M.getContextKind() == DeclContextKind::ClangModule;
  for (ValueDecl *VD : CurModuleResults)
    if (!IsTypeLookup || isa<TypeDecl>(VD))
      Results.push_back(Result::getModuleMember(VD));

  llvm::SmallPtrSet<CanType, 8> CurModuleTypes;
  for (ValueDecl *VD : CurModuleResults) {
    // If we find a type in the current module, don't look into any
    // imported modules.
    if (isa<TypeDecl>(VD))
      return;
    if (!IsTypeLookup)
      CurModuleTypes.insert(VD->getType()->getCanonicalType());
  }

  // Scrape through all of the imports looking for additional results.
  M.forAllVisibleModules(Nothing,
                         makeStackLambda(
    [&](const Module::ImportedModule &ImpEntry) {
      // FIXME: Only searching Clang modules once.
      if (ImpEntry.first.empty() &&
          ImpEntry.second->getContextKind() == DeclContextKind::ClangModule) {
        if (searchedClangModule)
          return;

        searchedClangModule = true;
      }

      SmallVector<ValueDecl*, 8> ImportedModuleResults;
      ImpEntry.second->lookupValue(ImpEntry.first, Name,
                                   NLKind::UnqualifiedLookup,
                                   ImportedModuleResults);
      for (ValueDecl *VD : ImportedModuleResults) {
        if ((!IsTypeLookup || isa<TypeDecl>(VD)) &&
            !CurModuleTypes.count(VD->getType()->getCanonicalType())) {
          Results.push_back(Result::getModuleMember(VD));
          CurModuleTypes.insert(VD->getType()->getCanonicalType());
        }
      }
    }
  ));

  // If we've found something, we're done.
  if (!Results.empty())
    return;

  // Look for a module with the given name.
  if (Name == M.Name) {
    Results.push_back(Result::getModuleName(&M));
    return;
  }

  M.forAllVisibleModules(Nothing,
                         makeStackLambda(
    [&](const Module::ImportedModule &ImpEntry) -> bool {
      if (ImpEntry.second->Name == Name) {
        Results.push_back(Result::getModuleName(ImpEntry.second));
        return false;
      }
      return true;
    }
  ));

  auto last = std::unique(Results.begin(), Results.end(),
              [](const Result &LHS, const Result &RHS) {
    return LHS.getNamedModule() == RHS.getNamedModule();
  });
  Results.erase(last, Results.end());
}

Optional<UnqualifiedLookup>
UnqualifiedLookup::forModuleAndName(ASTContext &C,
                                    StringRef Mod, StringRef Name) {
  auto foundModule = C.LoadedModules.find(Mod);
  if (foundModule == C.LoadedModules.end())
    return Nothing;

  Module *m = foundModule->second;
  return UnqualifiedLookup(C.getIdentifier(Name), m);
}

TypeDecl* UnqualifiedLookup::getSingleTypeResult() {
  if (Results.size() != 1 || !Results.back().hasValueDecl() ||
      !isa<TypeDecl>(Results.back().getValueDecl()))
    return nullptr;
  return cast<TypeDecl>(Results.back().getValueDecl());
}

#pragma mark Member lookup table

/// Lookup table used to store members of a nominal type (and its extensions)
/// for fast retrieval.
class swift::MemberLookupTable {
  /// The last extension that was included within the member lookup table's
  /// results.
  ExtensionDecl *LastExtensionIncluded = nullptr;

  /// The type of the internal lookup table.
  typedef llvm::DenseMap<Identifier, llvm::TinyPtrVector<ValueDecl *>>
    LookupTable;

  /// Lookup table mapping names to the set of declarations with that name.
  LookupTable Lookup;

public:
  /// Create a new member lookup table for the given nominal type.
  explicit MemberLookupTable(NominalTypeDecl *nominal);

  /// Update a lookup table with members from newly-added extensions.
  void updateLookupTable(NominalTypeDecl *nominal);

  /// \brief Add the given members to the lookup table.
  void addMembers(ArrayRef<Decl *> members);

  /// \brief The given extension has been extended with new members; add them
  /// if appropriate.
  void addExtensionMembers(NominalTypeDecl *nominal,
                           ExtensionDecl *ext,
                           ArrayRef<Decl *> members);

  /// Iterator into the lookup table.
  typedef LookupTable::iterator iterator;

  iterator begin() { return Lookup.begin(); }
  iterator end() { return Lookup.end(); }

  iterator find(Identifier name) {
    return Lookup.find(name);
  }
};

MemberLookupTable::MemberLookupTable(NominalTypeDecl *nominal) {
  // Add the members of the nominal declaration to the table.
  addMembers(nominal->getMembers());

  // Update the lookup table to introduce members from extensions.
  updateLookupTable(nominal);
}

void MemberLookupTable::addMembers(ArrayRef<Decl *> members) {
  for (auto member : members) {
    // Only value declarations matter.
    auto vd = dyn_cast<ValueDecl>(member);
    if (!vd)
      continue;

    // Unnamed entities cannot be found by name lookup.
    if (vd->getName().empty())
      continue;

    // Add this declaration to the lookup set.
    Lookup[vd->getName()].push_back(vd);
  }
}

void MemberLookupTable::addExtensionMembers(NominalTypeDecl *nominal,
                                            ExtensionDecl *ext,
                                            ArrayRef<Decl *> members) {
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
       LastExtensionIncluded = next, next = next->NextExtension.getPointer()) {
    addMembers(next->getMembers());
  }
}

void NominalTypeDecl::setMembers(ArrayRef<Decl*> M, SourceRange B) {
  // If we have already constructed a lookup table and we are adding members,
  // add them to the lookup table.
  if (LookupTable && M.size() > Members.size()) {
    // Make sure we have the complete list of extensions.
    (void)getExtensions();
    
    LookupTable->addMembers(M.slice(Members.size()));
  }

  Members = M;
  Braces = B;
}

ArrayRef<ValueDecl *> NominalTypeDecl::lookupDirect(Identifier name) {
  // Make sure we have the complete list of extensions.
  (void)getExtensions();

  if (!LookupTable) {
    // Create the lookup table.
    auto &ctx = getASTContext();
    void *mem = ctx.Allocate(sizeof(MemberLookupTable),
                             alignof(MemberLookupTable));
    LookupTable = new (mem) MemberLookupTable(this);

    // Register a cleanup with the ASTContext to call the lookup table
    // destructor.
    ctx.addCleanup([this]() {
      this->LookupTable->~MemberLookupTable();
    });
  } else {
    // Update the lookup table, if any new extension have come into existence.
    LookupTable->updateLookupTable(this);
  }

  // Look for the declarations with this name.
  auto known = LookupTable->find(name);
  if (known == LookupTable->end())
    return { };

  // We found something; return it.
  return { known->second.begin(), known->second.size() };
}

bool Module::lookupQualified(Type type,
                             Identifier name,
                             unsigned options,
                             SmallVectorImpl<ValueDecl *> &decls) {
  if (type->is<ErrorType>()) {
    return false;
  }

  // Look through lvalue types.
  if (auto lvalueTy = type->getAs<LValueType>()) {
    return lookupQualified(lvalueTy->getObjectType(), name, options, decls);
  }

  // Look through metatypes.
  if (auto metaTy = type->getAs<MetaTypeType>()) {
    return lookupQualified(metaTy->getInstanceType(), name, options, decls);
  }

  // Look for module references.
  if (auto moduleTy = type->getAs<ModuleType>()) {
    Module *module = moduleTy->getModule();
    module->lookupValue(Module::AccessPathTy(), name,
                        NLKind::QualifiedLookup, decls);

    // Prefer decls from the module itself, rather than imported modules.
    if (!decls.empty())
      return true;

    // Track whether we've already searched the Clang modules.
    // FIXME: This is a weird hack. We either need to filter within the
    // Clang module importer, or we need to change how this works.
    bool searchedClangModule =
      module->getContextKind() == DeclContextKind::ClangModule;

    module->forAllVisibleModules(Nothing,
                                 makeStackLambda(
      [&](const Module::ImportedModule &ImpEntry) {
        // FIXME: Only searching Clang modules once.
        if (ImpEntry.first.empty() &&
            ImpEntry.second->getContextKind() == DeclContextKind::ClangModule) {
          if (searchedClangModule)
            return;

          searchedClangModule = true;
        }

        // FIXME: Is the re-exported lookup really unqualified? We do want it
        // to ignore the Builtin module, but no one should be re-exporting that.
        ImpEntry.second->lookupValue(ImpEntry.first, name,
                                     NLKind::UnqualifiedLookup, decls);
      }
    ));
    return !decls.empty();
  }

  // The set of nominal type declarations we should (and have) visited.
  llvm::SmallVector<NominalTypeDecl *, 4> stack;
  llvm::SmallPtrSet<NominalTypeDecl *, 4> visited;

  // Handle nominal types.
  bool wantDefaultImplementations = false;
  if (auto nominal = type->getAnyNominal()) {
    visited.insert(nominal);
    stack.push_back(nominal);

    wantDefaultImplementations = (options & NL_DefaultDefinitions) &&
                                 !isa<ProtocolDecl>(nominal);
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

            wantDefaultImplementations = (options & NL_DefaultDefinitions) &&
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
        if (visited.insert(proto))
          stack.push_back(proto);
      }
    }
  }

  // Visit all of the nominal types we know about, discovering any others
  // we need along the way.
  llvm::DenseMap<ProtocolDecl *, ProtocolConformance *> knownConformances;
  while (!stack.empty()) {
    auto current = stack.back();
    stack.pop_back();

    // Look for results within the current nominal type and its extensions.
    for (auto results : current->lookupDirect(name))
      decls.push_back(results);

    // If we're not supposed to visit our supertypes, we're done.
    if ((options & NL_VisitSupertypes) == 0)
      continue;

    // Visit superclass.
    if (auto classDecl = dyn_cast<ClassDecl>(current)) {
      if (auto superclassType = classDecl->getSuperclass())
        if (auto superclassDecl = superclassType->getClassOrBoundGenericClass())
          if (visited.insert(superclassDecl))
            stack.push_back(superclassDecl);
      continue;
    }

    // If we're not looking at a protocol and we don't we're not supposed to visit the protocols that this type conforms to,
    // skip the next step.
    bool currentIsProtocol = isa<ProtocolDecl>(current);
    if (!wantDefaultImplementations && !currentIsProtocol)
      continue;

    // Local function object used to add protocols to the stack.
    auto addProtocols = [&](ArrayRef<ProtocolDecl *> protocols,
                            ArrayRef<ProtocolConformance *> conformances) {
      unsigned protoIndex = 0;
      for (auto proto : protocols) {
        // If our current declaration isn't a protocol, only consider this
        // inherited protocol if we have protocol-conformance information.
        if (!currentIsProtocol) {
          if (conformances.size() <= protoIndex || !conformances[protoIndex]) {
            ++protoIndex;
            continue;
          }

          // Record conformances for this protocol and every protocol it
          // inherits, recursively.
          SmallVector<std::pair<ProtocolDecl *, ProtocolConformance *>, 4>
            conformsStack;
          if (knownConformances.find(proto) == knownConformances.end())
            conformsStack.push_back({proto, conformances[protoIndex]});
          while (!conformsStack.empty()) {
            ProtocolConformance *conformance = nullptr;
            llvm::tie(proto, conformance) = conformsStack.back();
            conformsStack.pop_back();

            // Record this conformance.
            knownConformances[proto] = conformances[protoIndex];

            // Push inherited conformances.
            for (auto inherited : conformance->getInheritedConformances()) {
              if (knownConformances.find(inherited.first)
                    == knownConformances.end())
                conformsStack.push_back(inherited);
            }
          }
        }

        if (visited.insert(proto)) {
          stack.push_back(proto);
        }
        ++protoIndex;
      }
    };

    // Add protocols from the current type.
    addProtocols(current->getProtocols(), current->getConformances());

    // Add protocols from the extensions of the current type.
    for (auto ext : current->getExtensions()) {
      addProtocols(ext->getProtocols(), ext->getConformances());
    }
  }

  // If we're supposed to remove overridden declarations, do so now.
  if (options & NL_RemoveOverridden) {
    // Find all of the overridden declarations.
    llvm::SmallPtrSet<ValueDecl*, 8> overridden;
    for (auto decl : decls) {
      // FIXME: Generalize this.
      if (auto protocol = dyn_cast<ProtocolDecl>(decl->getDeclContext())) {
        // A declaration found in a protocol is a requirement that is
        // overridden by the witness satisfies that requirement, unless we
        // used a default definition or otherwise inferred the witness.
        auto knownProtocol = knownConformances.find(protocol);
        if (knownProtocol != knownConformances.end() &&
            !knownProtocol->second->usesDefaultDefinition(decl)) {
          overridden.insert(decl);
        }
      } else if (auto fd = dyn_cast<FuncDecl>(decl)) {
        if (fd->getOverriddenDecl())
          overridden.insert(fd->getOverriddenDecl());
      } else if (auto vd = dyn_cast<VarDecl>(decl)) {
        if (vd->getOverriddenDecl())
          overridden.insert(vd->getOverriddenDecl());
      } else if (auto sd = dyn_cast<SubscriptDecl>(decl)) {
        if (sd->getOverriddenDecl())
          overridden.insert(sd->getOverriddenDecl());
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
    removeShadowedDecls(decls, this);
  }

  // We're done. Report success/failure.
  return !decls.empty();
}

void ExtensionDecl::setMembers(ArrayRef<Decl*> M, SourceRange B) {
  // If this is a resolved extension and we're adding members to it,
  // we may have to update the extended type's lookup table.
  if (NextExtension.getInt() && M.size() > Members.size()) {
    auto nominal = getExtendedType()->getAnyNominal();
    if (nominal->LookupTable) {
      // Make sure we have the complete list of extensions.
      (void)nominal->getExtensions();

      nominal->LookupTable->addExtensionMembers(nominal, this,
                                                M.slice(Members.size()));
    }
  }

  Members = M;
  Braces = B;
}

