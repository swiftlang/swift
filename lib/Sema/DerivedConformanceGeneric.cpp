//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  This file implements implicit derivation of the CaseIterable protocol.
//
//===----------------------------------------------------------------------===//

// clang-format off
#include "TypeChecker.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Stmt.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Types.h"
#include "swift/AST/ParameterList.h"
#include "llvm/Support/raw_ostream.h"
#include "DerivedConformances.h"
// clang-format on

using namespace swift;

namespace {

Type computeRepresentation(ASTContext &ctx, NominalTypeDecl *type) {
  auto gpModule = ctx.getLoadedModule(ctx.Id_GenericProgramming);
  if (!gpModule)
    return nullptr;

  StructDecl *productStruct = nullptr;
  SmallVector<ValueDecl *, 1> productResults;
  gpModule->lookupValue(ctx.Id_Product, NLKind::UnqualifiedLookup,
                        productResults);
  for (auto productResult0 : productResults) {
    if (auto productResult = dyn_cast<StructDecl>(productResult0)) {
      productStruct = productResult;
    }
  }

  // TODO: Update the doc.
  // 0 -> Void
  // 1 -> Int
  // 2 -> Product<Int, Int>
  // 3 -> Product<Int, Product<Int, Int>>
  SmallVector<Type, 4> propTypesImpl;
  for (auto prop : type->getStoredProperties()) {
    propTypesImpl.push_back(prop->getType());
  }
  ArrayRef<Type> propTypes = propTypesImpl;
  if (propTypes.size() == 0) {
    return ctx.getVoidDecl()->getDeclaredInterfaceType();
  } else if (propTypes.size() == 1) {
    return propTypes[0];
  } else {
    auto result =
        BoundGenericType::get(productStruct, Type(), propTypes.take_back(2));
    propTypes = propTypes.drop_back(2);
    while (propTypes.size() > 0) {
      result = BoundGenericType::get(productStruct, Type(),
                                     {propTypes.back(), result});
      propTypes = propTypes.drop_back(1);
    }
    return result;
  }
}

} // namespace

bool DerivedConformance::canDeriveGeneric(NominalTypeDecl *type) {
  return true;
}

Type DerivedConformance::deriveGeneric(AssociatedTypeDecl *requirement) {
  if (checkAndDiagnoseDisallowedContext(requirement))
    return nullptr;

  if (!canDeriveGeneric(Nominal))
    return nullptr;

  if (requirement->getName() == Context.Id_Representation) {
    return computeRepresentation(Context, Nominal);
  }

  return nullptr;
}

ValueDecl *DerivedConformance::deriveGeneric(ValueDecl *requirement) {
  if (checkAndDiagnoseDisallowedContext(requirement))
    return nullptr;

  if (!canDeriveGeneric(Nominal))
    return nullptr;

  if (isa<ConstructorDecl>(requirement)) {
    auto param = new (Context)
        ParamDecl(SourceLoc(), SourceLoc(), Context.Id_representation,
                  SourceLoc(), Context.Id_representation, Nominal);
    param->setSpecifier(ParamSpecifier::Default);
    param->setInterfaceType(computeRepresentation(Context, Nominal));
    auto paramList = ParameterList::create(Context, param);

    SmallVector<ASTNode, 4> stmts;
    Expr *base = new (Context) DeclRefExpr(ConcreteDeclRef(param), DeclNameLoc(), /*Implicit=*/true);
    for (auto prop : Nominal->getStoredProperties()) {
      auto lhs = UnresolvedDeclRefExpr::createImplicit(Context, prop->getName());
      Expr* rhs;
      if (prop != Nominal->getStoredProperties().back()) {
        rhs = UnresolvedDotExpr::createImplicit(Context, base, Context.Id_first);
        base = UnresolvedDotExpr::createImplicit(Context, base, Context.Id_second);
      } else {
        rhs = base;
      }
      auto assign = new (Context) AssignExpr(lhs, SourceLoc(), rhs, /*Implicit=*/true);
      stmts.push_back(assign);
    }
    auto body = BraceStmt::create(Context, SourceLoc(), stmts, SourceLoc(),
                                  /*implicit=*/true);
    // auto body = [](AbstractFunctionDecl *decl,
    //                void *context) -> std::pair<BraceStmt *, bool> {
    //   ASTContext &ctx = decl->getASTContext();
    //   return {..., /*isTypeChecked=*/true};
    // };

    DeclName name(Context, DeclBaseName::createConstructor(), paramList);
    auto *ctor = new (Context)
        ConstructorDecl(name, Nominal->getLoc(),
                        /*Failable=*/false, /*FailabilityLoc=*/SourceLoc(),
                        /*Throws=*/false, /*ThrowsLoc=*/SourceLoc(), paramList,
                        /*GenericParams=*/nullptr, Nominal);
    ctor->setImplicit();
    ctor->copyFormalAccessFrom(Nominal, /*sourceIsParentContext*/ true);
    ctor->setBody(body);
    // ctor->setBodySynthesizer(body);

    addMembersToConformanceContext({ctor});
    return ctor;
  }

  return nullptr;
}
