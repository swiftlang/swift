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

TypeChecker::TypeChecker(TranslationUnit &TU, DiagnosticEngine &Diags)
  : TU(TU), Context(TU.Ctx),
    Diags(Diags)
{
  Context.addMutationListener(*this);

  // Validate any external types already part of the translation unit.
  for (auto type : Context.ExternalTypes)
    validateTypeSimple(type);
  
  // Add any external definitions already part of the translation unit.
  // Note: the underlying vector can get reallocated during this traversal.
  // We don't want to pick up the new external definitions.
  unsigned n = Context.ExternalDefinitions.size();
  for (unsigned i = Context.LastCheckedExternalDefinition; i != n; ++i)
    handleExternalDecl(Context.ExternalDefinitions[i]);
}

TypeChecker::~TypeChecker() {
  Context.removeMutationListener(*this);
}

void TypeChecker::handleExternalDecl(Decl *decl) {
  if (auto structDecl = dyn_cast<StructDecl>(decl)) {
    addImplicitConstructors(structDecl);
  }
}

void TypeChecker::addedExternalDecl(Decl *decl) {
  handleExternalDecl(decl);
  Context.ExternalDefinitions.insert(decl);
}

void TypeChecker::addedExternalType(Type type) {
  Context.ExternalTypes.push_back(type);
}

ProtocolDecl *TypeChecker::getProtocol(SourceLoc loc, KnownProtocolKind kind) {
  // Check whether we've already looked for and cached this protocol.
  unsigned index = (unsigned)kind;
  assert(index < numKnownProtocols && "Number of known protocols is wrong");
  if (knownProtocols[index])
    return knownProtocols[index];

  // Look for the protocol by name.
  Identifier name;
  switch (kind) {
  case KnownProtocolKind::ArrayBound:
    name = Context.getIdentifier("ArrayBound");
    break;

  case KnownProtocolKind::ArrayLiteralConvertible:
    name = Context.getIdentifier("ArrayLiteralConvertible");
    break;

  case KnownProtocolKind::BuiltinCharacterLiteralConvertible:
    name = Context.getIdentifier("BuiltinCharacterLiteralConvertible");
    break;

  case KnownProtocolKind::BuiltinFloatLiteralConvertible:
    name = Context.getIdentifier("BuiltinFloatLiteralConvertible");
    break;

  case KnownProtocolKind::BuiltinIntegerLiteralConvertible:
    name = Context.getIdentifier("BuiltinIntegerLiteralConvertible");
    break;

  case KnownProtocolKind::BuiltinStringLiteralConvertible:
    name = Context.getIdentifier("BuiltinStringLiteralConvertible");
    break;

  case KnownProtocolKind::CharacterLiteralConvertible:
    name = Context.getIdentifier("CharacterLiteralConvertible");
    break;

  case KnownProtocolKind::DictionaryLiteralConvertible:
    name = Context.getIdentifier("DictionaryLiteralConvertible");
    break;

  case KnownProtocolKind::Enumerable:
    name = Context.getIdentifier("Enumerable");
    break;

  case KnownProtocolKind::Enumerator:
    name = Context.getIdentifier("Enumerator");
    break;

  case KnownProtocolKind::FloatLiteralConvertible:
    name = Context.getIdentifier("FloatLiteralConvertible");
    break;

  case KnownProtocolKind::IntegerLiteralConvertible:
    name = Context.getIdentifier("IntegerLiteralConvertible");
    break;

  case KnownProtocolKind::LogicValue:
    name = Context.getIdentifier("LogicValue");
    break;

  case KnownProtocolKind::StringInterpolationConvertible:
    name = Context.getIdentifier("StringInterpolationConvertible");
    break;

  case KnownProtocolKind::StringLiteralConvertible:
    name = Context.getIdentifier("StringLiteralConvertible");
    break;
  }

  UnqualifiedLookup global(name, &TU);
  knownProtocols[index]
    = dyn_cast_or_null<ProtocolDecl>(global.getSingleTypeResult());

  // Complain if the protocol is completely missing.
  if (!knownProtocols[index]) {
    diagnose(loc, diag::missing_protocol, name);
  }

  return knownProtocols[index];
}

ProtocolDecl *TypeChecker::getLiteralProtocol(Expr *expr) {
  if (isa<ArrayExpr>(expr)) {
    return getProtocol(expr->getLoc(),
                       KnownProtocolKind::ArrayLiteralConvertible);
  }
  if (isa<DictionaryExpr>(expr)) {
    return getProtocol(expr->getLoc(),
                       KnownProtocolKind::DictionaryLiteralConvertible);
  }
  if (!isa<LiteralExpr>(expr))
    return nullptr;
  if (isa<IntegerLiteralExpr>(expr)) {
    return getProtocol(expr->getLoc(),
                       KnownProtocolKind::IntegerLiteralConvertible);
  }
  if (isa<FloatLiteralExpr>(expr)) {
    return getProtocol(expr->getLoc(),
                       KnownProtocolKind::FloatLiteralConvertible);
  }
  if (isa<CharacterLiteralExpr>(expr)) {
    return getProtocol(expr->getLoc(),
                       KnownProtocolKind::CharacterLiteralConvertible);
  }
  if (isa<StringLiteralExpr>(expr)) {
    return getProtocol(expr->getLoc(),
                       KnownProtocolKind::StringLiteralConvertible);
  }
  if (isa<InterpolatedStringLiteralExpr>(expr)) {
    return getProtocol(expr->getLoc(),
                       KnownProtocolKind::StringInterpolationConvertible);
  }
  
  llvm_unreachable("Unhandled literal kind");
}

Type TypeChecker::lookupBoolType() {
  return boolType.cache([&]{
    UnqualifiedLookup boolLookup(Context.getIdentifier("Bool"), &TU);
    if (!boolLookup.isSuccess()) {
      diagnose(SourceLoc(), diag::bool_type_broken);
      return Type();
    }
    TypeDecl *tyDecl = boolLookup.getSingleTypeResult();
    
    if (!tyDecl) {
      diagnose(SourceLoc(), diag::bool_type_broken);
      return Type();
    }
    
    return tyDecl->getDeclaredType();
  });
}

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
    return new (TC.Context) UnresolvedDotExpr(BaseExpr, SourceLoc(), Name, Loc);
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
  if ((OverriddenDecl->getDeclContext()->isExtensionContext() ||
       MemberVD->getDeclContext()->isExtensionContext()) &&
      !OverriddenDecl->isObjC()) {
    TC.diagnose(MemberVD->getLoc(), diag::override_decl_extension,
                OverriddenDecl->getDeclContext()->isExtensionContext());
    TC.diagnose(OverriddenDecl->getLoc(), diag::overridden_here);
    return;
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

/// \brief Check the given set of members for any members that override a member
/// of a base class (or its extensions).
///
/// \param TC The type checker.
///
/// \param CD The class for which we're checking overrides.
///
/// \param Members The set of members for which we are checking overrides.
static void checkClassOverrides(TypeChecker &TC, ClassDecl *CD,
                                ArrayRef<Decl *> Members) {
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
  auto BaseMetaTy = MetaTypeType::get(Base, TC.Context);
  llvm::DenseMap<Identifier, std::vector<ValueDecl*>> FoundDecls;
  llvm::SmallPtrSet<ValueDecl*, 16> ExactOverriddenDecls;
  llvm::SmallVector<ValueDecl*, 16> PossiblyOverridingDecls;
  for (Decl *MemberD : Members) {
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
      for (auto BaseMember : TC.lookupMember(BaseMetaTy, MemberVD->getName(),
                                             /*isTypeLookup=*/false))
        CurDecls.push_back(BaseMember);
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
      // Give [objc] decls a pass, because 'foo:' and 'foo:bar:' can coexist.
      if (MemberVD->isObjC() || CurDecls[i]->isObjC())
        continue;
      TC.diagnose(MemberVD->getLoc(), diag::overload_base_decl);
      break;
    }
  }
}

namespace {
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

    std::pair<bool, Expr *> walkToExprPre(Expr *E) override {
      if (FuncExpr *FE = dyn_cast<FuncExpr>(E))
        FuncExprs.push_back(FE);

      if (CapturingExpr *CE = dyn_cast<CapturingExpr>(E))
        CurDeclContexts.push_back(CE);

      return { true, E } ;
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
} // end anonymous namespace

/// performTypeChecking - Once parsing and namebinding are complete, these
/// walks the AST to resolve types and diagnose problems therein.
///
/// FIXME: This should be moved out to somewhere else.
void swift::performTypeChecking(TranslationUnit *TU, unsigned StartElem) {

  
  // Make sure that name binding has been completed before doing any type
  // checking.
  performNameBinding(TU, StartElem);
  
  TypeChecker TC(*TU);

  ExprPrePassWalker prePass(TC);

  // FIXME: Resolve the types of extensions first, so that each nominal type
  // has the complete chain of extensions associated with it. We need to be
  // able to more lazily resolve types for this to work.

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
    enum ClassState { CheckNone, CheckExtensions, CheckAll };
    llvm::DenseMap<ClassDecl *, ClassState> CheckedClasses;
    llvm::SmallVector<ClassDecl *, 16> QueuedClasses;
    for (unsigned i = 0, e = StartElem; i != e; ++i) {
      ClassDecl *CD = dyn_cast<ClassDecl>(TU->Decls[i]);
      if (CD)
        CheckedClasses[CD] = CheckNone;
    }

    // Find all of the classes and class extensions.
    llvm::DenseMap<ClassDecl *, llvm::SmallVector<ExtensionDecl *, 2>>
      Extensions;
    for (unsigned i = StartElem, e = TU->Decls.size(); i != e; ++i) {
      // We found a class; record it.
      if (auto classDecl = dyn_cast<ClassDecl>(TU->Decls[i])) {
        if (CheckedClasses.find(classDecl) == CheckedClasses.end()) {
          QueuedClasses.push_back(classDecl);
          CheckedClasses[classDecl] = CheckAll;
        }
        continue;
      }

      auto ext = dyn_cast<ExtensionDecl>(TU->Decls[i]);
      if (!ext)
        continue;

      auto classDecl = ext->getExtendedType()->getClassOrBoundGenericClass();
      if (!classDecl)
        continue;

      Extensions[classDecl].push_back(ext);
      if (CheckedClasses[classDecl] == CheckNone) {
        QueuedClasses.push_back(classDecl);
        ClassState &State = CheckedClasses[classDecl];
        State = std::max(State, CheckExtensions);
      }
    }

    // Check overrides in all of the classes and their extensions.
    while (!QueuedClasses.empty()) {
      auto classDecl = QueuedClasses.back();
      if (classDecl->hasBaseClass()) {
        ClassDecl *Base = classDecl->getBaseClass()->getClassOrBoundGenericClass();
        if (CheckedClasses[Base] != CheckNone) {
          QueuedClasses.push_back(Base);
          continue;
        }
      }
      if (CheckedClasses[classDecl] == CheckAll) {
        checkClassOverrides(TC, classDecl, classDecl->getMembers());
      }
      CheckedClasses[classDecl] = CheckNone;
      
      QueuedClasses.pop_back();

      // Check overrides in extensions of this class.
      auto knownExts = Extensions.find(classDecl);
      if (knownExts == Extensions.end())
        continue;

      for (auto ext : knownExts->second) {
        checkClassOverrides(TC, classDecl, ext->getMembers());
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
        if (init->alreadyChecked())
          continue;

        Expr *initExpr = prePass.doWalk(init->getExpr(), TypeAndContext.second);
        init->setExpr(initExpr, false);

        if (TT->hasCanonicalTypeComputed()) {
          // If we already examined a tuple in the first pass, we didn't
          // get a chance to type-check it; do that now.
          // FIXME: Bogus DeclContext
          if (!TC.typeCheckExpression(initExpr, TU, Elt.getType()))
            init->setExpr(initExpr, true);
        }
      }
    }
  }
  TU->clearTypesWithDefaultValues();

  // Check default arguments in patterns.
  // FIXME: This is an ugly hack to keep default arguments working for the
  // moment; I don't really want to invest more time into them until we're
  // sure how they are actually supposed to work.
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
          if (field.getInit() && field.getPattern()->hasType() &&
              !field.getInit()->alreadyChecked()) {
            Expr *e = field.getInit()->getExpr();
            e = PrePass.doWalk(e, DC);
            TC.typeCheckExpression(e, DC, field.getPattern()->getType());
            field.getInit()->setExpr(e, true);
          }
        }
        return;
      case PatternKind::Paren:
        return visitPattern(cast<ParenPattern>(P)->getSubPattern(), DC);
      case PatternKind::Typed:
      case PatternKind::Named:
      case PatternKind::Any:
        return;
          
#define PATTERN(Id, Parent)
#define REFUTABLE_PATTERN(Id, Parent) case PatternKind::Id:
#include "swift/AST/PatternNodes.def"
        llvm_unreachable("pattern can't appear in argument list!");
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
  }

  // Define any pending implicitly declarations.
  TC.definePendingImplicitDecls();
  prePass.FuncExprs.append(TC.implicitlyDefinedFunctions.begin(),
                           TC.implicitlyDefinedFunctions.end());
  TC.implicitlyDefinedFunctions.clear();

  // If we're in REPL mode, inject temporary result variables and other stuff
  // that the REPL needs to synthesize.
  if (TU->Kind == TranslationUnit::REPL && !TC.Context.hadError())
    TC.processREPLTopLevel(StartElem);
  
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
  unsigned currentExternalDef = TC.Context.LastCheckedExternalDefinition;
  unsigned currentExternalType = 0;
  do {
    // Validate any externally-created types.
    for (unsigned n = TC.Context.ExternalTypes.size(); currentExternalType != n;
         ++currentExternalType) {
      TC.validateTypeSimple(TC.Context.ExternalTypes[currentExternalType]);
    }

    // Type check the body of each of the FuncExpr in turn.  Note that outside
    // FuncExprs must be visited before nested FuncExprs for type-checking to
    // work correctly.
    unsigned previousFuncExpr = currentFuncExpr;
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

    // Compute captures for the function expressions we visited, in the
    // opposite order of type checking. i.e., the nested FuncExprs will be
    // visited before the outer FuncExprs.
    for (unsigned i = currentFuncExpr; i > previousFuncExpr; --i) {
      auto func = prePass.FuncExprs[i-1].dyn_cast<FuncExpr *>();
      if (!func)
        continue;

      TC.computeCaptures(func);
    }

    for (unsigned n = TC.Context.ExternalDefinitions.size();
                  currentExternalDef != n;
         ++currentExternalDef) {
      auto decl = TC.Context.ExternalDefinitions[currentExternalDef];

      if (auto constructor = dyn_cast<ConstructorDecl>(decl)) {
        TC.typeCheckConstructorBody(constructor);
        continue;
      }
      if (auto destructor = dyn_cast<DestructorDecl>(decl)) {
        TC.typeCheckDestructorBody(destructor);
        continue;
      }
      if (auto func = dyn_cast<FuncDecl>(decl)) {
        FuncExpr *FE = func->getBody();
        PrettyStackTraceExpr StackEntry(TC.Context, "type-checking", FE);
        TC.typeCheckFunctionBody(FE);
        continue;
      }
       if (isa<StructDecl>(decl) || isa<ProtocolDecl>(decl)) {
         // Type decls should already be typed by the ClangImporter and don't
         // need additional typechecking.
         continue;
      }
      llvm_unreachable("Unhandled external definition kind");
    }

    // Define any pending implicitly declarations.
    TC.definePendingImplicitDecls();
    prePass.FuncExprs.append(TC.implicitlyDefinedFunctions.begin(),
                             TC.implicitlyDefinedFunctions.end());
    TC.implicitlyDefinedFunctions.clear();
  } while (currentFuncExpr < prePass.FuncExprs.size() ||
           currentExternalDef < TC.Context.ExternalDefinitions.size() ||
           currentExternalType < TC.Context.ExternalTypes.size());

  // FIXME: Horrible hack. Store this somewhere more sane.
  TC.Context.LastCheckedExternalDefinition = currentExternalDef;

  // Verify that we've checked types correctly.
  TU->ASTStage = TranslationUnit::TypeChecked;
  verify(TU);
}

/// performTypeLocChecking - recursively validate the specified type.  This is
/// used when dealing with partial translation units (e.g. SIL parsing).
bool swift::performTypeLocChecking(TranslationUnit *TU, TypeLoc &T) {
  return TypeChecker(*TU).validateType(T);
}


bool swift::typeCheckCompletionContextExpr(TranslationUnit *TU,
                                           Expr *&parsedExpr) {
  // Set up a diagnostics engine that swallows diagnostics.
  NullDiagnosticConsumer completionConsumer;
  DiagnosticEngine diags(TU->Ctx.SourceMgr, completionConsumer);
  
  TypeChecker TC(*TU, diags);
  
  parsedExpr = ExprPrePassWalker(TC).doWalk(parsedExpr, TU);
  if (!parsedExpr
      || isa<ErrorExpr>(parsedExpr)
      || (parsedExpr->getType() && parsedExpr->getType()->is<ErrorType>()))
    return false;
  TC.typeCheckExpression(parsedExpr, TU);
  TU->ASTStage = TranslationUnit::TypeChecked;
  
  return parsedExpr && !isa<ErrorExpr>(parsedExpr)
                    && parsedExpr->getType()
                    && !parsedExpr->getType()->is<ErrorType>();
}
