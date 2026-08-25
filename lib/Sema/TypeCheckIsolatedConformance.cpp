//==- TypeCheckIsolatedConformance.cpp - Type Checking for Access Control -==//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file implements isolated conformance checking.
//
//===----------------------------------------------------------------------===//

#include "MiscDiagnostics.h"
#include "TypeCheckConcurrency.h"
#include "TypeChecker.h"
#include "swift/AST/ASTVisitor.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/Decl.h"
#include "swift/AST/DeclContext.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Type.h"
#include "swift/AST/TypeCheckRequests.h"
#include "swift/AST/TypeRepr.h"

using namespace swift;

namespace {

static void
forAllRequirementTypes(WhereClauseOwner &&source,
                       llvm::function_ref<void(Type, TypeRepr *)> callback) {
  std::move(source).visitRequirements(
      TypeResolutionStage::Interface,
      [&](const Requirement &req, RequirementRepr *reqRepr) {
        switch (req.getKind()) {
        case RequirementKind::SameShape:
        case RequirementKind::Conformance:
        case RequirementKind::SameType:
        case RequirementKind::Superclass:
          callback(req.getFirstType(),
                   RequirementRepr::getFirstTypeRepr(reqRepr));
          callback(req.getSecondType(),
                   RequirementRepr::getSecondTypeRepr(reqRepr));
          break;

        case RequirementKind::Layout:
          callback(req.getFirstType(),
                   RequirementRepr::getFirstTypeRepr(reqRepr));
          break;
        }
        return false;
      });
}

class IsolatedConformanceUseChecker
    : public DeclVisitor<IsolatedConformanceUseChecker> {
  void checkType(Type type, const TypeRepr *typeRepr, const Decl *context) {
    if (!type || (type && type->hasError()))
      return;

    TypeChecker::checkIsolatedConformancesInType(
        type, typeRepr ? typeRepr->getLoc() : context->getLoc());
  }

  void checkGenericParams(const GenericContext *ownerCtx,
                          const ValueDecl *ownerDecl) {
    if (!ownerCtx->isGenericContext())
      return;

    if (auto params = ownerCtx->getGenericParams()) {
      for (auto param : *params) {
        auto inheritedEntries = param->getInherited().getEntries();
        if (inheritedEntries.empty())
          continue;
        assert(inheritedEntries.size() == 1);
        auto inherited = inheritedEntries.front();
        checkType(inherited.getType(), inherited.getTypeRepr(), ownerDecl);
      }
    }

    if (ownerCtx->getTrailingWhereClause()) {
      forAllRequirementTypes(
          WhereClauseOwner(const_cast<GenericContext *>(ownerCtx)),
          [&](Type type, TypeRepr *typeRepr) {
            checkType(type, typeRepr, ownerDecl);
          });
    }
  }

public:
  explicit IsolatedConformanceUseChecker() {}

  void checkGlobalActor(Decl *D) {
    auto globalActor = D->getGlobalActorAttr();
    if (!globalActor)
      return;

    // Skip `@MainActor` since it doesn't have any generic parameters.
    if (globalActor->second->isMainActor())
      return;

    auto customAttr = globalActor->first;
    checkType(customAttr->getType(), customAttr->getTypeRepr(), D);
  }

  void visit(Decl *D) {
    DeclVisitor<IsolatedConformanceUseChecker>::visit(D);
    checkGlobalActor(D);
  }

  // Force all kinds to be handled at a lower level.
  void visitDecl(Decl *D) = delete;
  void visitValueDecl(ValueDecl *D) = delete;

#define UNREACHABLE(KIND, REASON)                                              \
  void visit##KIND##Decl(KIND##Decl *D) { llvm_unreachable(REASON); }
  UNREACHABLE(Import, "not applicable")
  UNREACHABLE(TopLevelCode, "not applicable")
  UNREACHABLE(Module, "not applicable")
  UNREACHABLE(Missing, "not applicable")
  UNREACHABLE(Using, "not applicable")

  UNREACHABLE(Param, "handled by the enclosing declaration")
  UNREACHABLE(GenericTypeParam, "handled by the enclosing declaration")
  UNREACHABLE(MissingMember, "handled by the enclosing declaration")
  UNREACHABLE(MacroExpansion, "handled by the enclosing declaration")
#undef UNREACHABLE

#define UNINTERESTING(KIND)                                                    \
  void visit##KIND##Decl(KIND##Decl *D) {}

  UNINTERESTING(PrefixOperator)
  UNINTERESTING(PostfixOperator)
  UNINTERESTING(InfixOperator)
  UNINTERESTING(EnumCase)
  UNINTERESTING(Destructor)
  UNINTERESTING(Accessor) // Handled by the Var or Subscript.
  UNINTERESTING(OpaqueType)
  UNINTERESTING(PrecedenceGroup)

  // Handled at the PatternBinding level; if the pattern has a simple
  // "name: TheType" form, we can get better results by diagnosing the TypeRepr.
  UNINTERESTING(Var)

  /// \see visitPatternBindingDecl
  void checkNamedPattern(const NamedPattern *NP,
                         const llvm::DenseSet<const VarDecl *> &seenVars) {
    const VarDecl *theVar = NP->getDecl();

    // Only check the type of individual variables if we didn't check an
    // enclosing TypedPattern.
    if (seenVars.count(theVar))
      return;

    checkType(theVar->getValueInterfaceType(), /*typeRepr*/ nullptr, theVar);

    for (auto attr : theVar->getAttachedPropertyWrappers()) {
      checkType(attr->getType(), attr->getTypeRepr(), theVar);
    }
  }

  /// \see visitPatternBindingDecl
  void checkTypedPattern(PatternBindingDecl *PBD, const TypedPattern *TP,
                         llvm::DenseSet<const VarDecl *> &seenVars) {
    // FIXME: We need to figure out if this is a stored or computed property,
    // so we pull out some random VarDecl in the pattern. They're all going to
    // be the same, but still, ick.
    const VarDecl *anyVar = nullptr;
    TP->forEachVariable([&](VarDecl *V) {
      seenVars.insert(V);
      anyVar = V;
    });

    checkType(TP->hasType() ? TP->getType() : Type(), TP->getTypeRepr(),
              anyVar ? (Decl *)anyVar : (Decl *)PBD);

    // Check the property wrapper types.
    if (anyVar) {
      for (auto attr : anyVar->getAttachedPropertyWrappers()) {
        checkType(attr->getType(), attr->getTypeRepr(), anyVar);
      }

      if (auto attr = anyVar->getAttachedResultBuilder()) {
        checkType(anyVar->getResultBuilderType(), attr->getTypeRepr(), anyVar);
      }
    }
  }

  void visitPatternBindingDecl(PatternBindingDecl *PBD) {
    llvm::DenseSet<const VarDecl *> seenVars;
    for (auto idx : range(PBD->getNumPatternEntries())) {
      PBD->getPattern(idx)->forEachNode([&](const Pattern *P) {
        if (auto *NP = dyn_cast<NamedPattern>(P)) {
          checkNamedPattern(NP, seenVars);
          return;
        }

        auto *TP = dyn_cast<TypedPattern>(P);
        if (!TP)
          return;
        checkTypedPattern(PBD, TP, seenVars);
      });
      seenVars.clear();
    }
  }

  void visitTypeAliasDecl(TypeAliasDecl *TAD) {
    checkGenericParams(TAD, TAD);
    checkType(TAD->getUnderlyingType(), TAD->getUnderlyingTypeRepr(), TAD);
  }

  void visitAssociatedTypeDecl(AssociatedTypeDecl *assocType) {
    for (TypeLoc requirement : assocType->getInherited().getEntries()) {
      checkType(requirement.getType(), requirement.getTypeRepr(), assocType);
    }
    checkType(assocType->getDefaultDefinitionType(),
              assocType->getDefaultDefinitionTypeRepr(), assocType);

    if (assocType->getTrailingWhereClause()) {
      forAllRequirementTypes(assocType, [&](Type type, TypeRepr *typeRepr) {
        checkType(type, typeRepr, assocType);
      });
    }
  }

  void visitNominalTypeDecl(const NominalTypeDecl *nominal) {
    checkGenericParams(nominal, nominal);

    for (TypeLoc inherited : nominal->getInherited().getEntries()) {
      checkType(inherited.getType(), inherited.getTypeRepr(), nominal);
    }
  }

  void visitProtocolDecl(ProtocolDecl *proto) {
    for (TypeLoc requirement : proto->getInherited().getEntries()) {
      checkType(requirement.getType(), requirement.getTypeRepr(), proto);
    }

    if (proto->getTrailingWhereClause()) {
      forAllRequirementTypes(proto, [&](Type type, TypeRepr *typeRepr) {
        checkType(type, typeRepr, proto);
      });
    }
  }

  void visitSubscriptDecl(SubscriptDecl *SD) {
    checkGenericParams(SD, SD);

    for (auto &P : *SD->getIndices()) {
      checkType(P->getInterfaceType(), P->getTypeRepr(), SD);
    }
    checkType(SD->getElementInterfaceType(), SD->getElementTypeRepr(), SD);
  }

  void visitAbstractFunctionDecl(AbstractFunctionDecl *fn) {
    checkGenericParams(fn, fn);

    for (auto *P : *fn->getParameters()) {
      auto wrapperAttrs = P->getAttachedPropertyWrappers();
      for (auto index : indices(wrapperAttrs)) {
        auto wrapperType = P->getAttachedPropertyWrapperType(index);
        checkType(wrapperType, wrapperAttrs[index]->getTypeRepr(), fn);
      }

      if (auto attr = P->getAttachedResultBuilder())
        checkType(P->getResultBuilderType(), attr->getTypeRepr(), fn);

      checkType(P->getInterfaceType(), P->getTypeRepr(), fn);
    }

    if (auto thrownTypeRepr = fn->getThrownTypeRepr()) {
      checkType(fn->getThrownInterfaceType(), thrownTypeRepr, fn);
    }
  }

  void visitFuncDecl(FuncDecl *FD) {
    visitAbstractFunctionDecl(FD);
    checkType(FD->getResultInterfaceType(), FD->getResultTypeRepr(), FD);

    if (auto attr = FD->getAttachedResultBuilder()) {
      checkType(FD->getResultBuilderType(), attr->getTypeRepr(), FD);
    }
  }

  void visitEnumElementDecl(EnumElementDecl *EED) {
    if (!EED->hasAssociatedValues())
      return;

    for (auto &P : *EED->getParameterList())
      checkType(P->getInterfaceType(), P->getTypeRepr(), EED);
  }

  void visitMacroDecl(MacroDecl *MD) {
    checkGenericParams(MD, MD);

    if (MD->parameterList) {
      for (auto P : *MD->parameterList) {
        checkType(P->getInterfaceType(), P->getTypeRepr(), MD);
      }
    }
    checkType(MD->getResultInterfaceType(), MD->resultType.getTypeRepr(), MD);
  }

  void checkConstrainedExtensionRequirements(ExtensionDecl *ED) {
    if (!ED->getTrailingWhereClause())
      return;

    forAllRequirementTypes(ED, [&](Type type, TypeRepr *typeRepr) {
      checkType(type, typeRepr, ED);
    });
  }

  void visitExtensionDecl(ExtensionDecl *ED) {
    auto extendedType = ED->getExtendedNominal();
    assert(extendedType && "valid extension with no extended type?");
    if (!extendedType)
      return;

    for (TypeLoc inherited : ED->getInherited().getEntries())
      checkType(inherited.getType(), inherited.getTypeRepr(), ED);

    checkType(ED->getExtendedType(), ED->getExtendedTypeRepr(), ED);

    checkConstrainedExtensionRequirements(ED);
  }
};

class ExprIsolatedConformanceUseChecker : public BaseDiagnosticWalker {
  DeclContext *DC;

  void checkType(Type T, TypeRepr *TR, SourceLoc loc) {
    TypeChecker::checkIsolatedConformancesInType(T, TR ? TR->getLoc() : loc);
  }

public:
  ExprIsolatedConformanceUseChecker(DeclContext *DC) : DC(DC) {}
  PreWalkResult<Expr *> walkToExprPre(Expr *E) override {
    if (auto T = dyn_cast<TypeExpr>(E)) {
      if (!T->isImplicit()) {
        checkType(T->getType(), T->getTypeRepr(), E->getLoc());
      }
    }

    if (auto CE = dyn_cast<ClosureExpr>(E)) {
      for (auto *param : *CE->getParameters()) {
        checkType(param->getInterfaceType(), param->getTypeRepr(), E->getLoc());
      }
      checkType(CE->getResultType(),
                CE->hasExplicitResultType() ? CE->getExplicitResultTypeRepr()
                                            : nullptr,
                E->getLoc());
    }

    if (AbstractClosureExpr *closure = dyn_cast<AbstractClosureExpr>(E)) {
      // Walk into the closure body as well.
      closure->getBody()->walk(*this);
      return Action::SkipChildren(E);
    }

    if (auto CE = dyn_cast<ExplicitCastExpr>(E)) {
      checkType(CE->getCastType(), CE->getCastTypeRepr(), E->getLoc());
    }

    return Action::Continue(E);
  }

  PreWalkResult<Stmt *> walkToStmtPre(Stmt *S) override {
    // We need to recursively call diagnoseInvalidIsolatedConformancesUses
    // for any sub-expressions in the statement.
    class StmtRecurseWalker : public BaseDiagnosticWalker {
      DeclContext *DC;

    public:
      StmtRecurseWalker(DeclContext *DC) : DC(DC) {}

      PreWalkResult<Expr *> walkToExprPre(Expr *E) override {
        diagnoseInvalidIsolatedConformancesUses(E, DC);
        return Action::SkipNode(E);
      }
    };
    StmtRecurseWalker W(DC);
    S->walk(W);
    return Action::SkipNode(S);
  }
};
} // end namespace

void TypeChecker::checkIsolatedConfromancesInDecl(Decl *D) {
  IsolatedConformanceUseChecker checker;
  checker.visit(D);
}

void TypeChecker::checkIsolatedConformancesInType(Type type, SourceLoc loc) {
  if (!type || type->hasError() || loc.isInvalid())
    return;

  class IsolatedConformanceInTypeWalker : public TypeWalker {
    SourceLoc loc;

  public:
    explicit IsolatedConformanceInTypeWalker(SourceLoc loc) : loc(loc) {}

    Action walkToTypePre(Type ty) override {
      auto boundGeneric = ty->getAs<BoundGenericType>();
      if (!boundGeneric)
        return Action::Continue;

      auto *decl = boundGeneric->getDecl();
      auto signature = decl->getGenericSignature();
      if (!signature)
        return Action::Continue;

      auto substitutions =
          QuerySubstitutionMap{boundGeneric->getContextSubstitutionMap()};
      const auto result = TypeChecker::checkIsolatedConformancesForDiagnostics(
          signature, signature.getRequirements(), substitutions);
      if (result.getKind() == CheckRequirementsResult::RequirementFailure) {
        TypeChecker::diagnoseRequirementFailure(
            result.getRequirementFailureInfo(), loc, decl->getLoc(), ty,
            signature.getGenericParams(), substitutions);
      }

      return Action::Continue;
    }
  };

  type.walk(IsolatedConformanceInTypeWalker(loc));
}

CheckGenericArgumentsResult
TypeChecker::checkIsolatedConformancesForDiagnostics(
    GenericSignature signature, ArrayRef<Requirement> requirements,
    TypeSubstitutionFn substitutions) {
  if (!signature)
    return CheckGenericArgumentsResult::createSuccess();

  for (const auto &req : requirements) {
    if (req.getKind() != RequirementKind::Conformance)
      continue;

    // Dig out the original type parameter for the requirement.
    // FIXME: req might not be the right pre-substituted requirement,
    // if this came from a conditional requirement.
    auto prohibits =
        signature->prohibitsIsolatedConformance(req.getFirstType());
    if (!prohibits)
      continue;

    auto substReq = req.subst(substitutions, LookUpConformanceInModule());

    SmallVector<Requirement, 2> subReqs;
    SmallVector<ProtocolConformanceRef, 2> isolatedConformances;
    (void)substReq.checkRequirement(subReqs, /*allowMissing=*/true,
                                    &isolatedConformances);
    if (isolatedConformances.empty())
      continue;

    return CheckGenericArgumentsResult::createIsolatedConformanceFailure(
        req, substReq,
        TinyPtrVector<ProtocolConformanceRef>(isolatedConformances),
        prohibits->second);
  }

  return CheckGenericArgumentsResult::createSuccess();
}

void swift::diagnoseInvalidIsolatedConformancesUses(const Expr *E,
                                                    const DeclContext *DC) {
  ExprIsolatedConformanceUseChecker checker(const_cast<DeclContext *>(DC));
  const_cast<Expr *>(E)->walk(checker);
}
