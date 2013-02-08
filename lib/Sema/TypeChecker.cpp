//===--- TypeChecker.cpp - Type Checking ----------------------------------===//
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
// This file implements the swift::performTypeChecking entry point for
// semantic analysis.
//
//===----------------------------------------------------------------------===//

#include "swift/Subsystems.h"
#include "TypeChecker.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/ASTVisitor.h"
#include "swift/AST/Attr.h"
#include "swift/AST/ExprHandle.h"
#include "swift/AST/Identifier.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/PrettyStackTrace.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/TinyPtrVector.h"
#include "llvm/ADT/Twine.h"

using namespace swift;

/// \brief Check for circular inheritance of protocols.
///
/// \param Path The circular path through the protocol inheritance hierarchy,
/// which will be constructed (backwards) if there is in fact a circular path.
///
/// \returns True if there was circular inheritance, false otherwise.
bool checkProtocolCircularity(TypeChecker &TC, ProtocolDecl *Proto,
                              llvm::SmallPtrSet<ProtocolDecl *, 16> &Visited,
                              llvm::SmallPtrSet<ProtocolDecl *, 16> &Local,
                              llvm::SmallVectorImpl<ProtocolDecl *> &Path) {
  for (auto InheritedTy : Proto->getInherited()) {
    SmallVector<ProtocolDecl *, 4> InheritedProtos;
    if (!InheritedTy.getType()->isExistentialType(InheritedProtos))
      continue;
    
    for (auto InheritedProto : InheritedProtos) {
      if (Visited.count(InheritedProto)) {
        // We've seen this protocol as part of another protocol search;
        // it's not circular.
        continue;
      }

      // Whether we've seen this protocol before in our search or visiting it
      // detects a circularity, record it in the path and abort.
      if (!Local.insert(InheritedProto) ||
          checkProtocolCircularity(TC, InheritedProto, Visited, Local, Path)) {
        Path.push_back(InheritedProto);
        return true;
      }
    }
  }
  
  return false;
}

static unsigned getNumArgs(ValueDecl *value) {
  if (!isa<FuncDecl>(value)) return ~0U;

  AnyFunctionType *fnTy = value->getType()->castTo<AnyFunctionType>();
  if (value->getDeclContext()->isTypeContext())
    fnTy = fnTy->getResult()->castTo<AnyFunctionType>();
  Type argTy = fnTy->getInput();
  if (auto tuple = argTy->getAs<TupleType>()) {
    return tuple->getFields().size();
  } else {
    return 1;
  }
}

static bool matchesDeclRefKind(ValueDecl *value, DeclRefKind refKind) {
  if (value->getType()->is<ErrorType>())
    return true;

  switch (refKind) {
  // An ordinary reference doesn't ignore anything.
  case DeclRefKind::Ordinary:
    return true;

  // A binary-operator reference only honors FuncDecls with a certain type.
  case DeclRefKind::BinaryOperator:
    return (getNumArgs(value) == 2);

  case DeclRefKind::PrefixOperator:
    return (!value->getAttrs().isPostfix() && getNumArgs(value) == 1);

  case DeclRefKind::PostfixOperator:
    return (value->getAttrs().isPostfix() && getNumArgs(value) == 1);    
  }
  llvm_unreachable("bad declaration reference kind");
}

/// BindName - Bind an UnresolvedDeclRefExpr by performing name lookup and
/// returning the resultant expression.  Context is the DeclContext used
/// for the lookup.
static Expr *BindName(UnresolvedDeclRefExpr *UDRE, DeclContext *Context,
                      TypeChecker &TC) {
  // Process UnresolvedDeclRefExpr by doing an unqualified lookup.
  Identifier Name = UDRE->getName();
  SourceLoc Loc = UDRE->getLoc();

  // Perform standard value name lookup.
  UnqualifiedLookup Lookup(Name, Context);

  if (!Lookup.isSuccess()) {
    TC.diagnose(Loc, diag::use_unresolved_identifier, Name);
    return new (TC.Context) ErrorExpr(Loc);
  }

  // FIXME: Need to refactor the way we build an AST node from a lookup result!

  if (Lookup.Results.size() == 1 &&
      Lookup.Results[0].Kind == UnqualifiedLookupResult::ModuleName) {
    ModuleType *MT = ModuleType::get(Lookup.Results[0].getNamedModule());
    return new (TC.Context) ModuleExpr(Loc, MT);
  }

  bool AllDeclRefs = true;
  SmallVector<ValueDecl*, 4> ResultValues;
  for (auto Result : Lookup.Results) {
    switch (Result.Kind) {
    case UnqualifiedLookupResult::ModuleMember:
    case UnqualifiedLookupResult::LocalDecl: {
      ValueDecl *D = Result.getValueDecl();
      if (matchesDeclRefKind(D, UDRE->getRefKind()))
        ResultValues.push_back(D);
      break;
    }
    case UnqualifiedLookupResult::MemberProperty:
    case UnqualifiedLookupResult::MemberFunction:
    case UnqualifiedLookupResult::MetatypeMember:
    case UnqualifiedLookupResult::ExistentialMember:
    case UnqualifiedLookupResult::ArchetypeMember:
    case UnqualifiedLookupResult::MetaArchetypeMember:
    case UnqualifiedLookupResult::ModuleName:
      AllDeclRefs = false;
      break;
    }
  }
  if (AllDeclRefs) {
    // Diagnose uses of operators that found no matching candidates.
    if (ResultValues.empty()) {
      assert(UDRE->getRefKind() != DeclRefKind::Ordinary);
      TC.diagnose(Loc, diag::use_nonmatching_operator, Name,
                  UDRE->getRefKind() == DeclRefKind::BinaryOperator ? 0 :
                  UDRE->getRefKind() == DeclRefKind::PrefixOperator ? 1 : 2);
      return new (TC.Context) ErrorExpr(Loc);
    }

    return TC.buildRefExpr(ResultValues, Loc);
  }

  ResultValues.clear();
  bool AllMemberRefs = true;
  ValueDecl *Base = 0;
  for (auto Result : Lookup.Results) {
    switch (Result.Kind) {
    case UnqualifiedLookupResult::MemberProperty:
    case UnqualifiedLookupResult::MemberFunction:
    case UnqualifiedLookupResult::MetatypeMember:
    case UnqualifiedLookupResult::ExistentialMember:
      ResultValues.push_back(Result.getValueDecl());
      if (Base && Result.getBaseDecl() != Base) {
        AllMemberRefs = false;
        break;
      }
      Base = Result.getBaseDecl();
      break;
    case UnqualifiedLookupResult::ModuleMember:
    case UnqualifiedLookupResult::LocalDecl:
    case UnqualifiedLookupResult::ModuleName:
      AllMemberRefs = false;
      break;
    case UnqualifiedLookupResult::MetaArchetypeMember:
    case UnqualifiedLookupResult::ArchetypeMember:
      // FIXME: We need to extend OverloadedMemberRefExpr to deal with this.
      llvm_unreachable("Archetype members in overloaded member references");
      break;
    }
  }

  if (AllMemberRefs) {
    Expr *BaseExpr;
    if (auto NTD = dyn_cast<NominalTypeDecl>(Base)) {
      Type BaseTy = MetaTypeType::get(NTD->getDeclaredTypeInContext(),
                                      TC.Context);
      BaseExpr = new (TC.Context) MetatypeExpr(nullptr, Loc, BaseTy);
    } else {
      BaseExpr = new (TC.Context) DeclRefExpr(Base, Loc,
                                              Base->getTypeOfReference());
    }
    return TC.buildMemberRefExpr(BaseExpr, SourceLoc(), ResultValues, Loc);
  }

  llvm_unreachable("Can't represent lookup result");
}

static void overrideDecl(TypeChecker &TC,
                         llvm::SmallPtrSet<ValueDecl*, 16> &OverriddenDecls,
                         ValueDecl *MemberVD, ValueDecl *OverriddenDecl) {
  if (!OverriddenDecls.insert(OverriddenDecl)) {
    TC.diagnose(MemberVD->getLoc(),
                diag::override_multiple_decls_derived);
    return;
  }
  if (OverriddenDecl->getDeclContext()->getContextKind() ==
      DeclContextKind::ExtensionDecl) {
    if (!OverriddenDecl->hasClangNode()) {
      TC.diagnose(MemberVD->getLoc(), diag::override_decl_extension);
      return;
    }
  }
  if (auto FD = dyn_cast<FuncDecl>(MemberVD)) {
    FD->setOverriddenDecl(cast<FuncDecl>(OverriddenDecl));
  } else if (auto VD = dyn_cast<VarDecl>(MemberVD)) {
    VD->setOverriddenDecl(cast<VarDecl>(OverriddenDecl));
  } else if (auto SD = dyn_cast<SubscriptDecl>(MemberVD)) {
    SD->setOverriddenDecl(cast<SubscriptDecl>(OverriddenDecl));
  } else {
    llvm_unreachable("Unexpected decl");
  }
}

static Type getBaseMemberDeclType(TypeChecker &TC,
                                  ValueDecl *D, Type Base) {
  ClassDecl *BaseDecl =
      D->getDeclContext()->getDeclaredTypeInContext()
      ->getClassOrBoundGenericClass();
  while (Base->getClassOrBoundGenericClass() != BaseDecl)
    Base = TC.getSuperClassOf(Base);
  return TC.substMemberTypeWithBase(D->getType(), nullptr, Base);
}

static void checkClassOverrides(TypeChecker &TC, ClassDecl *CD) {
  if (!CD->hasBaseClass())
    return;

  // The overall process of checking a method which might override
  // a base class method with the same name has three phases:
  // 1. If there's an exact match, the method overrides
  // the base class method.
  // 2. If there's a subtyping relationship with a single method not yet
  // overriden, the method overrides that base class method.  It's an error
  // if there are multiple potential base class methods.
  // 3. If all the base class methods with a given name have been overridden,
  // the method introduces a new overload.

  Type Base = CD->getBaseClass();
  llvm::DenseMap<Identifier, std::vector<ValueDecl*>> FoundDecls;
  llvm::SmallPtrSet<ValueDecl*, 16> ExactOverriddenDecls;
  llvm::SmallVector<ValueDecl*, 16> PossiblyOverridingDecls;
  for (Decl *MemberD : CD->getMembers()) {
    ValueDecl *MemberVD = dyn_cast<ValueDecl>(MemberD);
    if (!MemberVD)
      continue;
    if (isa<DestructorDecl>(MemberVD) || isa<ConstructorDecl>(MemberVD))
      continue;
    if (MemberVD->getName().empty())
      continue;
    auto FoundDeclResult = FoundDecls.insert({ MemberVD->getName(),
                                               std::vector<ValueDecl*>() });
    auto& CurDecls = FoundDeclResult.first->second;
    if (FoundDeclResult.second) {
      MemberLookup Lookup(Base, MemberVD->getName(), TC.TU);
      for (auto BaseMember : Lookup.Results)
        CurDecls.push_back(BaseMember.D);
    }
    if (!CurDecls.empty()) {
      if (isa<TypeDecl>(MemberVD)) {
        // Duplicate type declaration; this gets diagnosed earlier.
        // FIXME: That doesn't actually happen at the moment.
        continue;
      }

      ValueDecl *OverriddenDecl = nullptr;

      // First, check for an exact type match.
      for (unsigned i = 0, e = CurDecls.size(); i != e; ++i) {
        if (isa<TypeDecl>(CurDecls[i])) {
          // Overriding type declaration; this gets diagnosed earlier.
          // FIXME: That doesn't actually happen at the moment.
          break;
        }
        if (MemberVD->getKind() != CurDecls[i]->getKind())
          continue;
        bool isTypeEqual = false;
        Type BaseMemberTy = getBaseMemberDeclType(TC, CurDecls[i], Base);
        if (isa<FuncDecl>(MemberVD)) {
          AnyFunctionType *MemberFTy =
              MemberVD->getType()->getAs<AnyFunctionType>();
          AnyFunctionType *OtherFTy = BaseMemberTy->castTo<AnyFunctionType>();
          if (MemberFTy && OtherFTy)
            isTypeEqual = MemberFTy->getResult()->isEqual(OtherFTy->getResult());
        } else {
          isTypeEqual = MemberVD->getType()->isEqual(BaseMemberTy);
        }
        if (isTypeEqual) {
          if (CurDecls[i]->getDeclContext() == MemberVD->getDeclContext()) {
            // Duplicate declaration, diagnosed elsewhere
            // FIXME: That doesn't actually happen at the moment.
            continue;
          }
          if (OverriddenDecl) {
            // Two decls from the base class have the same type; this will
            // trigger an error elsewhere.
            // FIXME: That doesn't actually happen at the moment.
            continue;
          }
          OverriddenDecl = CurDecls[i];
        }
      }

      if (OverriddenDecl) {
        overrideDecl(TC, ExactOverriddenDecls, MemberVD, OverriddenDecl);
      } else {
        PossiblyOverridingDecls.push_back(MemberVD);
      }
    }
  }

  llvm::SmallVector<ValueDecl*, 16> PossiblyOverloadingDecls;
  llvm::SmallPtrSet<ValueDecl*, 16> SubtypeOverriddenDecls;
  for (auto MemberVD : PossiblyOverridingDecls) {
    // Then, check for a subtyping relationship.
    auto& CurDecls = FoundDecls[MemberVD->getName()];
    ValueDecl *OverriddenDecl = nullptr;
    for (unsigned i = 0, e = CurDecls.size(); i != e; ++i) {
      if (CurDecls[i]->getDeclContext() == MemberVD->getDeclContext()) {
        // We ignore sub-typing on the same class.
        continue;
      }
      if (ExactOverriddenDecls.count(CurDecls[i]))
        continue;
      if (MemberVD->getKind() != CurDecls[i]->getKind())
        continue;
      bool isSubtype = false;
      Type BaseMemberTy = getBaseMemberDeclType(TC, CurDecls[i], Base);
      if (isa<FuncDecl>(MemberVD)) {
        AnyFunctionType *MemberFTy =
            MemberVD->getType()->getAs<AnyFunctionType>();
        AnyFunctionType *OtherFTy = BaseMemberTy->getAs<AnyFunctionType>();
        if (MemberFTy && OtherFTy) {
          bool Trivial;
          isSubtype = TC.isSubtypeOf(MemberFTy->getResult(),
                                     OtherFTy->getResult(),
                                     Trivial) && Trivial;
        }
      } else {
        bool Trivial;
        isSubtype = TC.isSubtypeOf(MemberVD->getType(),
                                   BaseMemberTy,
                                   Trivial) && Trivial;
      }
      if (isSubtype) {
        if (OverriddenDecl) {
          TC.diagnose(MemberVD->getLoc(),
                      diag::override_multiple_decls_base);
          continue;
        }
        OverriddenDecl = CurDecls[i];
      }
      if (OverriddenDecl) {
        overrideDecl(TC, SubtypeOverriddenDecls, MemberVD, OverriddenDecl);
      } else {
        PossiblyOverloadingDecls.push_back(MemberVD);
      }
    }
  }

  for (auto MemberVD : PossiblyOverloadingDecls) {
    // Check if this is a new overload.
    auto& CurDecls = FoundDecls[MemberVD->getName()];
    for (unsigned i = 0, e = CurDecls.size(); i != e; ++i) {
      if (CurDecls[i]->getDeclContext() == MemberVD->getDeclContext()) {
        // We ignore sub-typing on the same class.
        continue;
      }
      if (ExactOverriddenDecls.count(CurDecls[i]))
        continue;
      if (SubtypeOverriddenDecls.count(CurDecls[i]))
        continue;
      TC.diagnose(MemberVD->getLoc(), diag::overload_base_decl);
      break;
    }
  }
}


/// performTypeChecking - Once parsing and namebinding are complete, these
/// walks the AST to resolve types and diagnose problems therein.
///
/// FIXME: This should be moved out to somewhere else.
void swift::performTypeChecking(TranslationUnit *TU, unsigned StartElem) {
  TypeChecker TC(*TU);

  struct ExprPrePassWalker : private ASTWalker {
    TypeChecker &TC;

    ExprPrePassWalker(TypeChecker &TC) : TC(TC) {}
    
    /// CurDeclContexts - This is the stack of DeclContexts that
    /// we're nested in.
    SmallVector<DeclContext*, 4> CurDeclContexts;

    // FuncExprs - This is a list of all the FuncExprs we need to analyze, in
    // an appropriate order.
    SmallVector<FuncExprLike, 32> FuncExprs;

    virtual bool walkToDeclPre(Decl *D) {
      if (NominalTypeDecl *NTD = dyn_cast<NominalTypeDecl>(D))
        CurDeclContexts.push_back(NTD);
      else if (ExtensionDecl *ED = dyn_cast<ExtensionDecl>(D))
        CurDeclContexts.push_back(ED);
      else if (ConstructorDecl *CD = dyn_cast<ConstructorDecl>(D))
        CurDeclContexts.push_back(CD);
      else if (DestructorDecl *DD = dyn_cast<DestructorDecl>(D))
        CurDeclContexts.push_back(DD);

      if (FuncDecl *FD = dyn_cast<FuncDecl>(D)) {
        // If this is an instance method with a body, set the type of its
        // implicit 'this' variable.
        // FIXME: This is only necessary because we do name-binding for
        // DeclRefs too early.
        if (Type ThisTy = FD->computeThisType())
          FD->getImplicitThisDecl()->setType(ThisTy);
      }

      if (ConstructorDecl *CD = dyn_cast<ConstructorDecl>(D))
        FuncExprs.push_back(CD);
      if (DestructorDecl *DD = dyn_cast<DestructorDecl>(D))
        FuncExprs.push_back(DD);
      return true;
    }
    
    virtual bool walkToDeclPost(Decl *D) {
      if (isa<NominalTypeDecl>(D)) {
        assert(CurDeclContexts.back() == cast<NominalTypeDecl>(D) &&
               "Context misbalance");
        CurDeclContexts.pop_back();
      } else if (isa<ExtensionDecl>(D)) {
        assert(CurDeclContexts.back() == cast<ExtensionDecl>(D) &&
               "Context misbalance");
        CurDeclContexts.pop_back();
      } else if (isa<ConstructorDecl>(D)) {
        assert(CurDeclContexts.back() == cast<ConstructorDecl>(D) &&
               "Context misbalance");
        CurDeclContexts.pop_back();
      } else if (isa<DestructorDecl>(D)) {
        assert(CurDeclContexts.back() == cast<DestructorDecl>(D) &&
               "Context misbalance");
        CurDeclContexts.pop_back();
      }

      return true;
    }

    bool walkToExprPre(Expr *E) {
      if (FuncExpr *FE = dyn_cast<FuncExpr>(E))
        FuncExprs.push_back(FE);

      if (CapturingExpr *CE = dyn_cast<CapturingExpr>(E))
        CurDeclContexts.push_back(CE);

      return true;
    }

    Expr *walkToExprPost(Expr *E) {
      if (UnresolvedDeclRefExpr *UDRE = dyn_cast<UnresolvedDeclRefExpr>(E)) {
        return BindName(UDRE, CurDeclContexts.back(),
                        TC);
      }

      if (isa<CapturingExpr>(E)) {
        assert(CurDeclContexts.back() == cast<CapturingExpr>(E) &&
               "Context misbalance");
        CurDeclContexts.pop_back();
      }

      return E;
    }

    Expr *doWalk(Expr *E, DeclContext *DC) {
      CurDeclContexts.push_back(DC);
      E = E->walk(*this);
      CurDeclContexts.pop_back();
      return E;
    }

    void doWalk(Decl *D) {
      CurDeclContexts.push_back(D->getDeclContext());
      D->walk(*this);
      CurDeclContexts.pop_back();
    }
  };
  ExprPrePassWalker prePass(TC);

  // Validate the conformance types of all of the protocols in the translation
  // unit. This includes inherited protocols, associated types with
  // requirements, and (FIXME:) conformance requirements in requires clauses.
  for (unsigned i = StartElem, e = TU->Decls.size(); i != e; ++i) {
    Decl *D = TU->Decls[i];
    if (auto Proto = dyn_cast<ProtocolDecl>(D))
      TC.preCheckProtocol(Proto);
  }

  // Type check the top-level elements of the translation unit.
  for (unsigned i = StartElem, e = TU->Decls.size(); i != e; ++i) {
    Decl *D = TU->Decls[i];
    if (isa<TopLevelCodeDecl>(D))
      continue;

    TC.typeCheckDecl(D, /*isFirstPass*/true);
  }

  // Check for explicit conformance to protocols and for circularity in
  // protocol definitions.
  {
    // FIXME: This check should be in TypeCheckDecl.
    llvm::SmallPtrSet<ProtocolDecl *, 16> VisitedProtocols;
    for (auto D : TU->Decls) {
      if (auto Protocol = dyn_cast<ProtocolDecl>(D)) {
        // Check for circular protocol definitions.
        llvm::SmallPtrSet<ProtocolDecl *, 16> LocalVisited;
        llvm::SmallVector<ProtocolDecl *, 4> Path;
        if (VisitedProtocols.count(Protocol) == 0) {
          LocalVisited.insert(Protocol);
          if (checkProtocolCircularity(TC, Protocol, VisitedProtocols,
                                       LocalVisited, Path)) {
            llvm::SmallString<128> PathStr;
            PathStr += "'";
            PathStr += Protocol->getName().str();
            PathStr += "'";
            for (unsigned I = Path.size(); I != 0; --I) {
              PathStr += " -> '";
              PathStr += Path[I-1]->getName().str();
              PathStr += "'";
            }
            
            TC.diagnose(Protocol->getLoc(), diag::circular_protocol_def,
                        PathStr);
            for (unsigned I = Path.size(); I != 1; --I) {
              TC.diagnose(Path[I-1], diag::protocol_here,
                          Path[I-1]->getName());
            }
          }
          
          VisitedProtocols.insert(LocalVisited.begin(), LocalVisited.end());
        }
      }
    }
  }

  // Check for correct overriding in classes.  We have to do this after the
  // first pass so we have valid types, but before the second pass because
  // it affects name lookup.  The loop here is a bit complicated because we
  // can't check a class before we've checked its base.
  {
    llvm::SmallPtrSet<ClassDecl *, 16> CheckedClasses;
    llvm::SmallVector<ClassDecl *, 16> QueuedClasses;
    for (unsigned i = 0, e = StartElem; i != e; ++i) {
      ClassDecl *CD = dyn_cast<ClassDecl>(TU->Decls[i]);
      if (CD)
        CheckedClasses.insert(CD);
    }
    for (unsigned i = StartElem, e = TU->Decls.size(); i != e; ++i) {
      ClassDecl *CD = dyn_cast<ClassDecl>(TU->Decls[i]);
      if (!CD)
        continue;
      if (!CheckedClasses.insert(CD))
        continue;
      QueuedClasses.push_back(CD);
      while (!QueuedClasses.empty()) {
        CD = QueuedClasses.back();
        if (CD->hasBaseClass()) {
          ClassDecl *Base = CD->getBaseClass()->getClassOrBoundGenericClass();
          if (CheckedClasses.insert(Base)) {
            QueuedClasses.push_back(Base);
            continue;
          }
        }
        checkClassOverrides(TC, CD);
        QueuedClasses.pop_back();
      }
    }
  }

  // At this point, we can perform general name lookup into any type.

  // We don't know the types of all the global declarations in the first
  // pass, which means we can't completely analyze everything. Perform the
  // second pass now.

  // Check default arguments in types.
  for (auto TypeAndContext : TU->getTypesWithDefaultValues()) {
    TupleType *TT = TypeAndContext.first;
    for (unsigned i = 0, e = TT->getFields().size(); i != e; ++i) {
      const TupleTypeElt& Elt = TT->getFields()[i];
      if (Elt.hasInit()) {
        // Perform global name-binding etc. for all tuple default values.
        // FIXME: This screws up the FuncExprs list for FuncExprs in a
        // default value; conceptually, we should be appending to the list
        // in source order.
        ExprHandle *init = Elt.getInit();
        Expr *initExpr = prePass.doWalk(init->getExpr(), TypeAndContext.second);
        init->setExpr(initExpr);

        if (TT->hasCanonicalTypeComputed()) {
          // If we already examined a tuple in the first pass, we didn't
          // get a chance to type-check it; do that now.
          if (!TC.typeCheckExpression(initExpr, Elt.getType()))
            init->setExpr(initExpr);
        }
      }
    }
  }
  TU->clearTypesWithDefaultValues();

  // Check default arguments in patterns.
  // FIXME: This is an ugly hack to keep default arguments working for the
  // moment; I don't really want to invest more time into them until we're
  // sure how they are acutally supposed to work.
  struct PatternDefaultArgChecker : public ASTWalker {
    TypeChecker &TC;
    ExprPrePassWalker &PrePass;

    PatternDefaultArgChecker(TypeChecker &TC,
                             ExprPrePassWalker &PrePass)
      : TC(TC), PrePass(PrePass) {}

    void visitPattern(Pattern *P, DeclContext *DC) {
      switch (P->getKind()) {
      case PatternKind::Tuple:
        for (auto &field : cast<TuplePattern>(P)->getFields()) {
          if (field.getInit() && field.getPattern()->hasType()) {
            Expr *e = field.getInit()->getExpr();
            e = PrePass.doWalk(e, DC);
            TC.typeCheckExpression(e, field.getPattern()->getType());
            field.getInit()->setExpr(e);
          }
        }
        return;
      case PatternKind::Paren:
        return visitPattern(cast<ParenPattern>(P)->getSubPattern(), DC);
      case PatternKind::Typed:
      case PatternKind::Named:
      case PatternKind::Any:
        return;
      }
      llvm_unreachable("bad pattern kind!");
    }

    virtual bool walkToDeclPre(Decl *D) {
      if (ConstructorDecl *CD = dyn_cast<ConstructorDecl>(D)) {
        if (CD->getArguments())
          visitPattern(CD->getArguments(), D->getDeclContext());
      } else if (SubscriptDecl *SD = dyn_cast<SubscriptDecl>(D)) {
        visitPattern(SD->getIndices(), D->getDeclContext());
      } else if (FuncDecl *FD = dyn_cast<FuncDecl>(D)) {
        for (Pattern *p : FD->getBody()->getArgParamPatterns())
          visitPattern(p, D->getDeclContext());
      }
      return true;
    }
  };
  PatternDefaultArgChecker pdac(TC, prePass);
  for (unsigned i = StartElem, e = TU->Decls.size(); i != e; ++i)
    TU->Decls[i]->walk(pdac);

  for (unsigned i = StartElem, e = TU->Decls.size(); i != e; ++i) {
    Decl *D = TU->Decls[i];
    if (TopLevelCodeDecl *TLCD = dyn_cast<TopLevelCodeDecl>(D)) {
      // Immediately perform global name-binding etc.
      prePass.doWalk(TLCD);
      TC.typeCheckTopLevelCodeDecl(TLCD);
    } else {
      prePass.doWalk(D);
      TC.typeCheckDecl(D, /*isFirstPass*/false);
    }
    if (TU->Kind == TranslationUnit::Repl && !TC.Context.hadError())
      if (PatternBindingDecl *PBD = dyn_cast<PatternBindingDecl>(D))
        TC.REPLCheckPatternBinding(PBD);
  }

  // Check overloaded vars/funcs.
  // FIXME: This is quadratic time for TUs with multiple chunks.
  // FIXME: Can we make this more efficient?
  // FIXME: This check should be earlier to avoid ambiguous overload
  // errors etc.
  llvm::DenseMap<Identifier, TinyPtrVector<ValueDecl*>> CheckOverloads;
  for (unsigned i = 0, e = TU->Decls.size(); i != e; ++i) {
    if (ValueDecl *VD = dyn_cast<ValueDecl>(TU->Decls[i])) {
      // FIXME: I'm not sure this check is really correct.
      if (VD->getName().empty())
        continue;
      if (VD->getType()->is<ErrorType>() || VD->getType()->isUnresolvedType())
        continue;
      auto &PrevOv = CheckOverloads[VD->getName()];
      if (i >= StartElem) {
        for (ValueDecl *PrevD : PrevOv) {
          if (PrevD->getType()->isEqual(VD->getType())) {
            TC.diagnose(VD->getStartLoc(), diag::invalid_redecl);
            TC.diagnose(PrevD, diag::invalid_redecl_prev,
                        VD->getName());
          }
        }
      }
      PrevOv.push_back(VD);
    }
  }

  unsigned currentFuncExpr = 0;
  do {
    // Type check the body of each of the FuncExpr in turn.  Note that outside
    // FuncExprs must be visited before nested FuncExprs for type-checking to
    // work correctly.
    for (unsigned n = prePass.FuncExprs.size(); currentFuncExpr != n;
         ++currentFuncExpr) {
      auto func = prePass.FuncExprs[currentFuncExpr];

      if (ConstructorDecl *CD = func.dyn_cast<ConstructorDecl*>()) {
        TC.typeCheckConstructorBody(CD);
        continue;
      }
      if (DestructorDecl *DD = func.dyn_cast<DestructorDecl*>()) {
        TC.typeCheckDestructorBody(DD);
        continue;
      }
      FuncExpr *FE = func.get<FuncExpr*>();
      PrettyStackTraceExpr StackEntry(TC.Context, "type-checking", FE);

      TC.typeCheckFunctionBody(FE);
    }

    // Type-check any externally-sourced definitions and types.
    // FIXME: This is O(N^2), because we're not tracking new definitions.
    for (auto imported : TC.Context.LoadedClangModules) {
      for (auto &def : imported->getExternalDefinitions()) {
        switch (def.getStage()) {
        case ExternalDefinition::TypeChecked:
          continue;

        case ExternalDefinition::NameBound:
          break;
        }

        auto decl = def.getDecl();
        if (auto constructor = dyn_cast<ConstructorDecl>(decl)) {
          prePass.FuncExprs.push_back(constructor);
          def.setStage(ExternalDefinition::TypeChecked);
          continue;
        }
        if (auto destructor = dyn_cast<DestructorDecl>(decl)) {
          prePass.FuncExprs.push_back(destructor);
          def.setStage(ExternalDefinition::TypeChecked);
          continue;
        }
        if (auto func = dyn_cast<FuncDecl>(decl)) {
          prePass.FuncExprs.push_back(func->getBody());
          def.setStage(ExternalDefinition::TypeChecked);
          continue;
        }

        llvm_unreachable("Unknown externally-sourced definition");
      }
      for (Type t : imported->getTypes())
        TC.validateTypeSimple(t);
    }
  } while (currentFuncExpr < prePass.FuncExprs.size());
  
  // Verify that we've checked types correctly.
  TU->ASTStage = TranslationUnit::TypeChecked;
  verify(TU);
}
