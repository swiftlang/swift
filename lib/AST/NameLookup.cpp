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


#include "swift/AST/NameLookup.h"
#include "swift/AST/AST.h"
#include "swift/AST/ASTVisitor.h"

using namespace swift;

static void DoGlobalExtensionLookup(Type BaseType, Identifier Name,
                                    ArrayRef<ValueDecl*> BaseMembers,
                                    Module *CurModule,
                                    Module *BaseModule,
                                    SmallVectorImpl<ValueDecl*> &Result) {
  bool CurModuleHasTypeDecl = false;
  llvm::SmallPtrSet<CanType, 8> CurModuleTypes;

  bool NameBindingLookup = CurModule->ASTStage == Module::Parsed;

  // Find all extensions in this module.
  for (ExtensionDecl *ED : CurModule->lookupExtensions(BaseType)) {
    for (Decl *Member : ED->getMembers()) {
      if (ValueDecl *VD = dyn_cast<ValueDecl>(Member)) {
        if (VD->getName() == Name) {
          Result.push_back(VD);
          if (!NameBindingLookup)
            CurModuleTypes.insert(VD->getType()->getCanonicalType());
          CurModuleHasTypeDecl |= isa<MetaTypeType>(VD->getType());
        }
      }
    }
  }

  if (BaseModule == CurModule) {
    for (ValueDecl *VD : BaseMembers) {
      if (VD->getName() == Name) {
        Result.push_back(VD);
        if (!NameBindingLookup)
          CurModuleTypes.insert(VD->getType()->getCanonicalType());
        CurModuleHasTypeDecl |= isa<MetaTypeType>(VD->getType());
      }
    }
  }

  // The builtin module has no imports.
  if (isa<BuiltinModule>(CurModule)) return;

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
    
    for (ExtensionDecl *ED : ImpEntry.second->lookupExtensions(BaseType)) {
      for (Decl *Member : ED->getMembers()) {
        if (ValueDecl *VD = dyn_cast<ValueDecl>(Member)) {
          if (VD->getName() == Name &&
              (NameBindingLookup || isa<TypeDecl>(VD) ||
               !CurModuleTypes.count(VD->getType()->getCanonicalType()))) {
            Result.push_back(VD);
          }
        }
      }
    }
  }

  if (BaseModule != CurModule) {
    for (ValueDecl *VD : BaseMembers) {
      if (VD->getName() == Name &&
          (NameBindingLookup || isa<TypeDecl>(VD) ||
           !CurModuleTypes.count(VD->getType()->getCanonicalType()))) {
        Result.push_back(VD);
      }
    }
  }
}

MemberLookup::MemberLookup(Type BaseTy, Identifier Name, Module &M) {
  MemberName = Name;
  VisitedSet Visited;
  doIt(BaseTy, M, Visited);
}

/// doIt - Lookup a member 'Name' in 'BaseTy' within the context
/// of a given module 'M'.  This operation corresponds to a standard "dot" 
/// lookup operation like "a.b" where 'this' is the type of 'a'.  This
/// operation is only valid after name binding.
void MemberLookup::doIt(Type BaseTy, Module &M, VisitedSet &Visited) {
  typedef MemberLookupResult Result;
  
  // Just look through l-valueness.  It doesn't affect name lookup.
  if (LValueType *LV = BaseTy->getAs<LValueType>())
    BaseTy = LV->getObjectType();

  // Type check metatype references, as in "some_type.some_member".  These are
  // special and can't have extensions.
  if (MetaTypeType *MTT = BaseTy->getAs<MetaTypeType>()) {
    // The metatype represents an arbitrary named type: dig through to the
    // declared type to see what we're dealing with.
    Type Ty = MTT->getTypeDecl()->getDeclaredType();

    // Just perform normal dot lookup on the type with the specified
    // member name to see if we find extensions or anything else.  For example,
    // type SomeTy.SomeMember can look up static functions, and can even look
    // up non-static functions as well (thus getting the address of the member).
    doIt(Ty, M, Visited);
    return;
  }
  
  // Lookup module references, as on some_module.some_member.  These are
  // special and can't have extensions.
  if (ModuleType *MT = BaseTy->getAs<ModuleType>()) {
    SmallVector<ValueDecl*, 8> Decls;
    MT->getModule()->lookupValue(Module::AccessPathTy(), MemberName,
                                 NLKind::QualifiedLookup, Decls);
    for (ValueDecl *VD : Decls)
      Results.push_back(Result::getMetatypeMember(VD));
    return;
  }

  // If the base is a protocol, see if this is a reference to a declared
  // protocol member.
  if (ProtocolType *PT = BaseTy->getAs<ProtocolType>()) {
    if (!Visited.insert(PT->getDecl()))
      return;
      
    for (auto Inherited : PT->getDecl()->getInherited())
      doIt(Inherited, M, Visited);
    
    for (auto Member : PT->getDecl()->getMembers()) {
      if (ValueDecl *VD = dyn_cast<ValueDecl>(Member)) {
        if (VD->getName() != MemberName) continue;
        if (isa<VarDecl>(VD) || isa<SubscriptDecl>(VD) || isa<FuncDecl>(VD)) {
          Results.push_back(Result::getExistentialMember(VD));
        } else {
          assert(isa<TypeDecl>(VD) && "Unhandled protocol member");
          Results.push_back(Result::getMetatypeMember(VD));
        }
      }
    }
    return;
  }
  
  // If the base is a protocol composition, see if this is a reference to a
  // declared protocol member in any of the protocols.
  if (auto PC = BaseTy->getAs<ProtocolCompositionType>()) {
    for (auto Proto : PC->getProtocols())
      doIt(Proto, M, Visited);
    return;
  }
    
  // Check to see if this is a reference to a tuple field.
  if (TupleType *TT = BaseTy->getAs<TupleType>()) {
    // If the field name exists, we win.  Otherwise, if the field name is a
    // dollarident like $4, process it as a field index.
    int FieldNo = TT->getNamedElementId(MemberName);
    if (FieldNo != -1) {
      Results.push_back(MemberLookupResult::getTupleElement(FieldNo));
    } else {
      StringRef NameStr = MemberName.str();
      if (NameStr.startswith("$")) {
        unsigned Value = 0;
        if (!NameStr.substr(1).getAsInteger(10, Value) &&
            Value < TT->getFields().size())
          Results.push_back(MemberLookupResult::getTupleElement(Value));
      }
    }
    return;
  }

  // Check to see if any of an archetype's requirements have the member.
  if (ArchetypeType *Archetype = BaseTy->getAs<ArchetypeType>()) {
    for (auto Proto : Archetype->getConformsTo())
      doIt(Proto, M, Visited);

    // Change existential members to archetype members, since we're in an
    // archetype.
    for (auto &Result : Results) {
      if (Result.Kind == MemberLookupResult::ExistentialMember)
        Result.Kind = MemberLookupResult::ArchetypeMember;
    }
    return;
  }

  // Look in any extensions that add methods to the base type.
  SmallVector<ValueDecl*, 8> ExtensionMethods;
  lookupMembers(BaseTy, M, ExtensionMethods);

  for (ValueDecl *VD : ExtensionMethods) {
    if (TypeDecl *TAD = dyn_cast<TypeDecl>(VD)) {
      Results.push_back(Result::getMetatypeMember(TAD));
      continue;
    }
    if (FuncDecl *FD = dyn_cast<FuncDecl>(VD)) {
      if (FD->isStatic())
        Results.push_back(Result::getMetatypeMember(FD));
      else
        Results.push_back(Result::getMemberFunction(FD));
      continue;
    }
    if (OneOfElementDecl *OOED = dyn_cast<OneOfElementDecl>(VD)) {
      Results.push_back(Result::getMetatypeMember(OOED));
      continue;
    }
    assert((isa<VarDecl>(VD) || isa<SubscriptDecl>(VD)) &&
           "Unexpected extension member");
    Results.push_back(Result::getMemberProperty(VD));
  }
}

void MemberLookup::lookupMembers(Type BaseType, Module &M,
                                 SmallVectorImpl<ValueDecl*> &Result) {
  assert(Results.empty() &&
         "This expects that the input list is empty, could be generalized");

  TypeDecl *D;
  ArrayRef<ValueDecl*> BaseMembers;
  SmallVector<ValueDecl*, 2> BaseMembersStorage;
  if (StructType *ST = BaseType->getAs<StructType>()) {
    D = ST->getDecl();
    for (Decl* Member : ST->getDecl()->getMembers()) {
      if (ValueDecl *VD = dyn_cast<ValueDecl>(Member))
        BaseMembersStorage.push_back(VD);
    }
    BaseMembers = BaseMembersStorage;
  } else if (ClassType *CT = BaseType->getAs<ClassType>()) {
    D = CT->getDecl();
    for (Decl* Member : CT->getDecl()->getMembers()) {
      if (ValueDecl *VD = dyn_cast<ValueDecl>(Member))
        BaseMembersStorage.push_back(VD);
    }
    BaseMembers = BaseMembersStorage;
  } else if (OneOfType *OOT = BaseType->getAs<OneOfType>()) {
    D = OOT->getDecl();
    for (Decl* Member : OOT->getDecl()->getMembers()) {
      if (ValueDecl *VD = dyn_cast<ValueDecl>(Member))
        BaseMembersStorage.push_back(VD);
    }
    BaseMembers = BaseMembersStorage;
  } else {
    return;
  }

  DeclContext *DC = D->getDeclContext();
  while (!DC->isModuleContext())
    DC = DC->getParent();

  DoGlobalExtensionLookup(BaseType, MemberName, BaseMembers, &M,
                          cast<Module>(DC), Result);
}

static Type makeSimilarLValue(Type objectType, Type lvalueType,
                              ASTContext &Context) {
  LValueType::Qual qs = cast<LValueType>(lvalueType)->getQualifiers();
  return LValueType::get(objectType, qs, Context);
}

static Expr *buildTupleElementExpr(Expr *Base, SourceLoc DotLoc,
                                   SourceLoc NameLoc, unsigned FieldIndex,
                                   ASTContext &Context) {
  Type BaseTy = Base->getType();
  bool IsLValue = false;
  if (LValueType *LV = BaseTy->getAs<LValueType>()) {
    IsLValue = true;
    BaseTy = LV->getObjectType();
  }
  
  Type FieldType = BaseTy->castTo<TupleType>()->getElementType(FieldIndex);
  if (IsLValue)
    FieldType = makeSimilarLValue(FieldType, Base->getType(), Context);
  
  if (DotLoc.isValid())
    return new (Context) SyntacticTupleElementExpr(Base, DotLoc, FieldIndex,
                                                   NameLoc, FieldType);
  
  return new (Context) ImplicitThisTupleElementExpr(Base, FieldIndex, NameLoc,
                                                    FieldType);
}


/// createResultAST - Build an AST to represent this lookup, with the
/// specified base expression.
Expr *MemberLookup::createResultAST(Expr *Base, SourceLoc DotLoc, 
                                    SourceLoc NameLoc, ASTContext &Context) {
  assert(isSuccess() && "Can't create a result if we didn't find anything");
         
  // Handle the case when we found exactly one result.
  if (Results.size() == 1) {
    MemberLookupResult R = Results[0];
    bool IsMetatypeBase = Base->getType()->is<MetaTypeType>();

    switch (R.Kind) {
    case MemberLookupResult::TupleElement:
      if (IsMetatypeBase)
        break;
      return buildTupleElementExpr(Base, DotLoc, NameLoc, R.TupleFieldNo,
                                   Context);
    case MemberLookupResult::MemberFunction: {
      if (IsMetatypeBase) {
        Expr *RHS = new (Context) DeclRefExpr(R.D, NameLoc,
                                              R.D->getTypeOfReference());
        return new (Context) DotSyntaxBaseIgnoredExpr(Base, DotLoc, RHS);
      }
      Expr *Fn = new (Context) DeclRefExpr(R.D, NameLoc,
                                           R.D->getTypeOfReference());
      return new (Context) DotSyntaxCallExpr(Fn, DotLoc, Base);
    }
    case MemberLookupResult::MemberProperty: {
      if (IsMetatypeBase) 
        break;
      VarDecl *Var = cast<VarDecl>(R.D);
      return new (Context) MemberRefExpr(Base, DotLoc, Var, NameLoc);
    }
    case MemberLookupResult::MetatypeMember: {
      Expr *RHS = new (Context) DeclRefExpr(R.D, NameLoc,
                                            R.D->getTypeOfReference());
      return new (Context) DotSyntaxBaseIgnoredExpr(Base, DotLoc, RHS);
    }
    case MemberLookupResult::ExistentialMember:
      return new (Context) ExistentialMemberRefExpr(Base, DotLoc, R.D, NameLoc);
    case MemberLookupResult::ArchetypeMember:
      return new (Context) ArchetypeMemberRefExpr(Base, DotLoc, R.D, NameLoc);
    }

    Expr *BadExpr = new (Context) UnresolvedDotExpr(Base, DotLoc,
                                                    MemberName, NameLoc);
    return BadExpr;
  }
  
  // If we have an ambiguous result, build an overload set.
  SmallVector<ValueDecl*, 8> ResultSet;
    
  // This is collecting a mix of static and normal functions. We won't know
  // until after overload resolution whether we actually need 'this'.
  for (MemberLookupResult X : Results) {
    assert(X.Kind != MemberLookupResult::TupleElement);
    ResultSet.push_back(X.D);
  }
  
  return OverloadedMemberRefExpr::createWithCopy(Base, DotLoc, ResultSet,
                                                 NameLoc);
}

ConstructorLookup::ConstructorLookup(Type BaseType, Module &M) {
  NominalType *NT = BaseType->getAs<NominalType>();
  if (!NT)
    return;

  NominalTypeDecl *D = NT->getDecl();
  SmallVector<ValueDecl*, 16> BaseMembers;
  if (StructDecl *SD = dyn_cast<StructDecl>(D)) {
    for (Decl* Member : SD->getMembers()) {
      if (ValueDecl *VD = dyn_cast<ValueDecl>(Member))
        BaseMembers.push_back(VD);
    }
  } else if (OneOfDecl *OOD = dyn_cast<OneOfDecl>(D)) {
    for (Decl* Member : OOD->getMembers()) {
      // FIXME: We shouldn't be injecting OneOfElementDecls into the results
      // like this.
      if (OneOfElementDecl *OOED = dyn_cast<OneOfElementDecl>(Member))
        Results.push_back(OOED);
      else if (ValueDecl *VD = dyn_cast<ValueDecl>(Member))
        BaseMembers.push_back(VD);
    }
  } else if (ClassDecl *CD = dyn_cast<ClassDecl>(D)) {
    for (Decl* Member : CD->getMembers()) {
      if (ValueDecl *VD = dyn_cast<ValueDecl>(Member))
        BaseMembers.push_back(VD);
    }
  } else {
    return;
  }

  Identifier Constructor = M.Ctx.getIdentifier("constructor");
  DeclContext *DC = D->getDeclContext();
  if (!DC->isModuleContext()) {
    for (ValueDecl *VD : BaseMembers) {
      if (VD->getName() == Constructor)
        Results.push_back(VD);
    }
    return;
  }

  DoGlobalExtensionLookup(BaseType, Constructor, BaseMembers, &M,
                          cast<Module>(DC), Results);
}

struct FindLocalVal : public StmtVisitor<FindLocalVal> {
  SourceLoc Loc;
  Identifier Name;
  ValueDecl *MatchingValue;

  FindLocalVal(SourceLoc Loc, Identifier Name)
    : Loc(Loc), Name(Name), MatchingValue(nullptr) {}

  bool IntersectsRange(SourceRange R) {
    return R.Start.Value.getPointer() <= Loc.Value.getPointer() &&
           R.End.Value.getPointer() >= Loc.Value.getPointer();
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
  void visitSemiStmt(SemiStmt *) {}
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
};

UnqualifiedLookup::UnqualifiedLookup(Identifier Name, DeclContext *DC,
                                     SourceLoc Loc) {
  typedef UnqualifiedLookupResult Result;

  DeclContext *ModuleDC = DC;
  while (!ModuleDC->isModuleContext())
    ModuleDC = ModuleDC->getParent();

  Module &M = *cast<Module>(ModuleDC);

  // If we are inside of a method, check to see if there are any ivars in scope,
  // and if so, whether this is a reference to one of them.
  while (!DC->isModuleContext()) {
    ValueDecl *BaseDecl = 0;
    GenericParamList *GenericParams = nullptr;
    Type ExtendedType;
    if (FuncExpr *FE = dyn_cast<FuncExpr>(DC)) {
      // Look for local variables; normally, the parser resolves these
      // for us, but it can't do the right thing inside local types.
      if (Loc.isValid()) {
        FindLocalVal localVal(Loc, Name);
        localVal.visit(FE->getBody());
        if (!localVal.MatchingValue) {
          for (Pattern *P : FE->getParamPatterns())
            localVal.checkPattern(P);
        }
        if (localVal.MatchingValue) {
          Results.push_back(Result::getLocalDecl(localVal.MatchingValue));
          return;
        }
      }

      FuncDecl *FD = FE->getDecl();
      if (FD && FD->getExtensionType() && !FD->isStatic()) {
        ExtendedType = FD->getExtensionType();
        BaseDecl = FD->getImplicitThisDecl();
        DC = DC->getParent();
      }

      // Look in the generic parameters after checking our local declaration.
      if (FD)
        GenericParams = FD->getGenericParams();

    } else if (ExtensionDecl *ED = dyn_cast<ExtensionDecl>(DC)) {
      ExtendedType = ED->getExtendedType();
      if (NominalType *NT = ExtendedType->getAs<NominalType>())
        BaseDecl = NT->getDecl();
    } else if (NominalTypeDecl *ND = dyn_cast<NominalTypeDecl>(DC)) {
      ExtendedType = ND->getDeclaredType();
      BaseDecl = ND;
    } else if (ConstructorDecl *CD = dyn_cast<ConstructorDecl>(DC)) {
      // Look for local variables; normally, the parser resolves these
      // for us, but it can't do the right thing inside local types.
      if (Loc.isValid()) {
        FindLocalVal localVal(Loc, Name);
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
      DC = DC->getParent();
    }

    if (BaseDecl) {
      MemberLookup Lookup(ExtendedType, Name, M);
      
      for (auto Result : Lookup.Results) {
        switch (Result.Kind) {
        case MemberLookupResult::MemberProperty:
          Results.push_back(Result::getMemberProperty(BaseDecl, Result.D));
          break;
        case MemberLookupResult::MemberFunction:
          Results.push_back(Result::getMemberFunction(BaseDecl, Result.D));
          break;
        case MemberLookupResult::MetatypeMember:
          Results.push_back(Result::getMetatypeMember(BaseDecl, Result.D));
          break;
        case MemberLookupResult::ExistentialMember:
          Results.push_back(Result::getExistentialMember(BaseDecl, Result.D));
          break;
        case MemberLookupResult::ArchetypeMember:
          Results.push_back(Result::getArchetypeMember(BaseDecl, Result.D));
          break;
        case MemberLookupResult::TupleElement:
          llvm_unreachable("Can't have context with tuple type");
        }
      }
      if (Lookup.isSuccess())
        return;
    }

    // Check the generic parameters for something with the given name.
    if (GenericParams) {
      FindLocalVal localVal(Loc, Name);
      localVal.checkGenericParams(GenericParams);

      if (localVal.MatchingValue) {
        Results.push_back(Result::getLocalDecl(localVal.MatchingValue));
        return;
      }
    }

    DC = DC->getParent();
  }

  if (Loc.isValid()) {
    if (TranslationUnit *TU = dyn_cast<TranslationUnit>(&M)) {
      // Look for local variables in top-level code; normally, the parser
      // resolves these for us, but it can't do the right thing for
      // local types.
      FindLocalVal localVal(Loc, Name);
      localVal.checkTranslationUnit(TU);
      if (localVal.MatchingValue) {
        Results.push_back(Result::getLocalDecl(localVal.MatchingValue));
        return;
      }
    }
  }

  // Do a local lookup within the current module.
  llvm::SmallVector<ValueDecl*, 4> CurModuleResults;
  M.lookupValue(Module::AccessPathTy(), Name, NLKind::UnqualifiedLookup,
                CurModuleResults);
  for (ValueDecl *VD : CurModuleResults)
    Results.push_back(Result::getModuleMember(VD));

  // The builtin module has no imports.
  if (isa<BuiltinModule>(M)) return;
  
  TranslationUnit &TU = cast<TranslationUnit>(M);

  bool NameBindingLookup = TU.ASTStage == Module::Parsed;
  llvm::SmallPtrSet<CanType, 8> CurModuleTypes;
  for (ValueDecl *VD : CurModuleResults) {
    // If we find a type in the current module, don't look into any
    // imported modules.
    if (isa<TypeDecl>(VD))
      return;
    if (!NameBindingLookup)
      CurModuleTypes.insert(VD->getType()->getCanonicalType());
  }

  // Scrape through all of the imports looking for additional results.
  // FIXME: Implement DAG-based shadowing rules.
  llvm::SmallPtrSet<Module *, 16> Visited;
  for (auto &ImpEntry : TU.getImportedModules()) {
    if (!Visited.insert(ImpEntry.second))
      continue;

    SmallVector<ValueDecl*, 8> ImportedModuleResults;
    ImpEntry.second->lookupValue(ImpEntry.first, Name, NLKind::UnqualifiedLookup,
                                 ImportedModuleResults);
    for (ValueDecl *VD : ImportedModuleResults) {
      if (NameBindingLookup || isa<TypeDecl>(VD) ||
          !CurModuleTypes.count(VD->getType()->getCanonicalType())) {
        Results.push_back(Result::getModuleMember(VD));
      }
    }
  }

  for (const auto &ImpEntry : TU.getImportedModules())
    if (ImpEntry.second->Name == Name) {
      Results.push_back(Result::getModuleName(ImpEntry.second));
      break;
    }
}


TypeDecl* UnqualifiedLookup::getSingleTypeResult() {
  if (Results.size() != 1 || !Results.back().hasValueDecl() ||
      !isa<TypeDecl>(Results.back().getValueDecl()))
    return nullptr;
  return cast<TypeDecl>(Results.back().getValueDecl());
}
