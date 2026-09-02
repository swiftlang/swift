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
#include "TypeCheckDeclInterface.h"
#include "TypeChecker.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/Decl.h"
#include "swift/AST/DeclContext.h"
#include "swift/AST/Expr.h"
#include "swift/AST/SubstitutionMap.h"
#include "swift/AST/Type.h"
#include "swift/AST/TypeCheckRequests.h"
#include "swift/AST/TypeRepr.h"

using namespace swift;

namespace {

class IsolatedConformanceUseChecker
    : public DeclInterfaceTypeChecker<IsolatedConformanceUseChecker> {
public:
  explicit IsolatedConformanceUseChecker() {}

  void checkType(Type type, const TypeRepr *typeRepr, const Decl *context) {
    if (!type || (type && type->hasError()))
      return;

    TypeChecker::checkIsolatedConformancesInType(
        type, typeRepr ? typeRepr->getLoc() : context->getLoc());
  }

  void checkConstrainedExtensionRequirements(ExtensionDecl *ED) {
    if (!ED->getTrailingWhereClause())
      return;

    WhereClauseOwner(ED).forAllRequirementTypes(
        [&](Type type, TypeRepr *typeRepr) { checkType(type, typeRepr, ED); });
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
      if (auto *TA = dyn_cast<TypeAliasType>(ty.getPointer())) {
        check(TA->getDecl(), TA, TA->getGenericSignature(),
              TA->getSubstitutionMap());
      }

      auto boundGeneric = ty->getAs<BoundGenericType>();
      if (!boundGeneric)
        return Action::Continue;

      auto *decl = boundGeneric->getDecl();
      auto signature = decl->getGenericSignature();
      if (!signature)
        return Action::Continue;

      check(decl, ty, signature, boundGeneric->getContextSubstitutionMap()); 
      return Action::Continue;
    }

    void check(Decl *source, Type type, GenericSignature signature,
               SubstitutionMap substitutions) {
      QuerySubstitutionMap subs{substitutions};
      const auto result = TypeChecker::checkIsolatedConformancesForDiagnostics(
          signature, signature.getRequirements(), subs);
      if (result.getKind() == CheckRequirementsResult::RequirementFailure) {
        TypeChecker::diagnoseRequirementFailure(
            result.getRequirementFailureInfo(), loc, source->getLoc(), type,
            signature.getGenericParams(), subs);
      }
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
