//===--- DerivedConformanceParameterAggregate.cpp -------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file implements explicit derivation of the ParameterAggregate protocol
// for a nominal type.
//
//===----------------------------------------------------------------------===//

#include "CodeSynthesis.h"
#include "TypeChecker.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Module.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/Pattern.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/Stmt.h"
#include "swift/AST/Types.h"
#include "DerivedConformances.h"

using namespace swift;

// Return the "parameter type" corresponding to a ValueDecl.
// If the decl conforms to ParameterAggregate, return the `Parameter` associated
// type. Otherwise, directly return the decl's type.
static Type getParameterType(TypeChecker &TC, ValueDecl *decl) {
  auto *paramAggProto =
      TC.Context.getProtocol(KnownProtocolKind::ParameterAggregate);
  auto conf = TC.conformsToProtocol(decl->getInterfaceType(), paramAggProto,
                                    decl->getDeclContext(),
                                    ConformanceCheckFlags::InExpression);
  if (!conf)
    return decl->getInterfaceType();
  Type parameterType = ProtocolConformanceRef::getTypeWitnessByName(
      decl->getInterfaceType(), *conf, TC.Context.Id_Parameter, &TC);
  assert(parameterType && "'Parameter' associated type not found");
  return parameterType;
}

static Type deriveParameterAggregate_Parameter(TypeChecker &TC,
                                               NominalTypeDecl *nominal) {
  if (nominal->getMembers().empty())
    return Type();
  // If all stored properties have the same type, return that type.
  // Otherwise, the `Parameter` type cannot be derived.
  Type sameMemberType;
  for (auto member : nominal->getMembers()) {
    auto varDecl = dyn_cast<VarDecl>(member);
    if (!varDecl || varDecl->isStatic() || !varDecl->hasStorage())
      continue;
    auto parameterType = getParameterType(TC, varDecl);
    if (!sameMemberType) {
      sameMemberType = parameterType;
      continue;
    }
    if (!parameterType->isEqual(sameMemberType))
      return Type();
  }
  return sameMemberType;
}

bool DerivedConformance::canDeriveParameterAggregate(TypeChecker &TC,
                                                     NominalTypeDecl *nominal) {
  return bool(deriveParameterAggregate_Parameter(TC, nominal));
}

// Add @_fixed_layout attribute to type conforming to `ParameterAggregate`, if
// necessary.
void addFixedLayoutAttrIfNeeded(TypeChecker &TC, NominalTypeDecl *nominal) {
  // If nominal already has @_fixed_layout, return.
  if (nominal->getAttrs().hasAttribute<FixedLayoutAttr>()) return;
  auto access = nominal->getEffectiveAccess();
  // If nominal does not have at least internal access, return.
  if (access < AccessLevel::Internal)
    return;
  // If nominal is internal, it should have the @usableFromInline attribute.
  if (access == AccessLevel::Internal &&
      !nominal->getAttrs().hasAttribute<UsableFromInlineAttr>()) {
    nominal->getAttrs().add(
      new (TC.Context) UsableFromInlineAttr(/*Implicit*/ true));
  }
  // Add the @_fixed_layout attribute to the nominal.
  nominal->getAttrs().add(
    new (TC.Context) FixedLayoutAttr(/*Implicit*/ true));
}

static TypeAliasDecl *getParameterTypeAliasDecl(NominalTypeDecl *nominal) {
  auto &ctx = nominal->getASTContext();
  TypeAliasDecl *parameterDecl = nullptr;
  for (auto memberDecl : nominal->getMembers()) {
    auto typealiasDecl = dyn_cast<TypeAliasDecl>(memberDecl);
    if (!typealiasDecl || typealiasDecl->getName() != ctx.Id_Parameter)
      continue;
    parameterDecl = typealiasDecl;
    break;
  }
  return parameterDecl;
}

static void
deriveBodyParameterAggregate_update(AbstractFunctionDecl *updateDecl) {
  auto *nominal = updateDecl->getDeclContext()->getSelfNominalTypeDecl();
  auto &C = nominal->getASTContext();

  auto *selfDecl = updateDecl->getImplicitSelfDecl();
  Expr *selfDRE =
      new (C) DeclRefExpr(selfDecl, DeclNameLoc(), /*Implicit*/ true);

  auto *gradientsDecl = updateDecl->getParameters()->get(0);
  auto *updaterDecl = updateDecl->getParameters()->get(1);
  Expr *gradientsDRE =
      new (C) DeclRefExpr(gradientsDecl, DeclNameLoc(), /*Implicit*/ true);
  Expr *updaterDRE =
      new (C) DeclRefExpr(updaterDecl, DeclNameLoc(), /*Implicit*/ true);

  // Return the member with the same name as a target VarDecl.
  auto getMatchingMember = [&](VarDecl *target) -> VarDecl * {
    for (auto member : nominal->getMembers()) {
      auto *varDecl = dyn_cast<VarDecl>(member);
      if (!varDecl)
        continue;
      if (varDecl->getName() == target->getName())
        return varDecl;
    }
    assert(false && "Could not find matching 'ParameterAggregate' member");
    return nullptr;
  };

  auto *paramAggProto = C.getProtocol(KnownProtocolKind::ParameterAggregate);
  auto lookup = paramAggProto->lookupDirect(C.getIdentifier("update"));
  assert(lookup.size() == 1 && "Broken 'ParameterAggregate' protocol");
  auto updateRequirement = lookup[0];

  // Return an "update call" expression for a member `x`.
  auto createUpdateCallExpr = [&](VarDecl *member) -> Expr * {
    auto module = nominal->getModuleContext();
    auto confRef = module->lookupConformance(member->getType(), paramAggProto);

    auto *memberExpr = new (C) MemberRefExpr(selfDRE, SourceLoc(), member,
                                             DeclNameLoc(), /*Implicit*/ true);
    auto *gradientsMemberExpr = new (C) MemberRefExpr(
        gradientsDRE, SourceLoc(), getMatchingMember(member), DeclNameLoc(),
        /*Implicit*/ true);

    // If member does not conform to ParameterAggregate, apply updater to member
    // directly: `updater(&x, gradients.x)`.
    if (!confRef) {
      auto *inoutExpr = new (C) InOutExpr(SourceLoc(), memberExpr,
                                          member->getType(), /*Implicit*/ true);
      return CallExpr::createImplicit(C, updaterDRE,
                                      {inoutExpr, gradientsMemberExpr}, {});
    }

    // Otherwise, if member does conform to ParameterAggregate, call the
    // member's `update` method:
    // `x.update(withGradients: gradients.x, updater)`.
    auto conf = confRef->getConcrete();
    auto paramUpdateDecl = conf->getWitnessDecl(updateRequirement, nullptr);
    auto updateDRE =
        new (C) DeclRefExpr(paramUpdateDecl, DeclNameLoc(), /*Implicit*/ true);
    auto updateCallExpr =
        new (C) DotSyntaxCallExpr(updateDRE, SourceLoc(), memberExpr);
    updateCallExpr->setImplicit();
    return CallExpr::createImplicit(
        C, updateCallExpr, {gradientsMemberExpr, updaterDRE},
        {C.getIdentifier("withGradients"), Identifier()});
  };

  SmallVector<ASTNode, 2> updateCallNodes;
  for (auto member : nominal->getMembers()) {
    auto varDecl = dyn_cast<VarDecl>(member);
    if (!varDecl || varDecl->isStatic() || !varDecl->hasStorage())
      continue;
    auto *call = createUpdateCallExpr(varDecl);
    updateCallNodes.push_back(call);
  }
  updateDecl->setBody(
      BraceStmt::create(C, SourceLoc(), updateCallNodes, SourceLoc(),
                        /*Implicit*/ true));
}

// Synthesize the `update(withGradients:_:)` function declaration.
static ValueDecl *deriveParameterAggregate_update(DerivedConformance &derived) {
  auto nominal = derived.Nominal;
  auto parentDC = derived.getConformanceContext();
  auto &C = derived.TC.Context;

  auto selfDecl = ParamDecl::createSelf(SourceLoc(), nominal,
                                        /*isStatic*/ false, /*isInOut*/ true);
  auto parametersType = nominal->getDeclaredTypeInContext();
  auto parametersInterfaceType = nominal->getDeclaredInterfaceType();

  auto parameterDecl = getParameterTypeAliasDecl(nominal);
  auto parameterType = parameterDecl->getDeclaredInterfaceType();
  assert(nominal && parametersType && "'Parameters' decl unresolved");
  assert(parameterDecl && "'Parameter' decl unresolved");

  auto gradientsDecl =
      new (C) ParamDecl(VarDecl::Specifier::Default, SourceLoc(), SourceLoc(),
                        C.getIdentifier("withGradients"), SourceLoc(),
                        C.getIdentifier("gradients"), parentDC);
  gradientsDecl->setInterfaceType(parametersInterfaceType);

  auto inoutFlag = ParameterTypeFlags().withInOut(true);
  auto updaterDecl = new (C) ParamDecl(
      VarDecl::Specifier::Default, SourceLoc(), SourceLoc(), Identifier(),
      SourceLoc(), C.getIdentifier("updater"), parentDC);
  FunctionType::Param updaterInputTypes[] = {
    FunctionType::Param(parameterType, Identifier(), inoutFlag),
    FunctionType::Param(parameterType)
  };
  auto updaterType = FunctionType::get(updaterInputTypes,
                                       TupleType::getEmpty(C),
                                       FunctionType::ExtInfo().withNoEscape());
  updaterDecl->setInterfaceType(updaterType);

  ParameterList *params =
      ParameterList::create(C, {gradientsDecl, updaterDecl});

  DeclName updateDeclName(C, C.getIdentifier("update"), params);
  auto updateDecl = FuncDecl::create(
      C, SourceLoc(), StaticSpellingKind::None, SourceLoc(), updateDeclName,
      SourceLoc(), /*Throws*/ false, SourceLoc(), nullptr, selfDecl, params,
      TypeLoc::withoutLoc(TupleType::getEmpty(C)), nominal);
  updateDecl->setImplicit();
  updateDecl->setSelfAccessKind(SelfAccessKind::Mutating);
  updateDecl->setBodySynthesizer(deriveBodyParameterAggregate_update);

  if (auto env = parentDC->getGenericEnvironmentOfContext())
    updateDecl->setGenericEnvironment(env);
  updateDecl->computeType();
  updateDecl->copyFormalAccessFrom(nominal, /*sourceIsParentContext*/ true);
  updateDecl->setValidationToChecked();

  derived.addMembersToConformanceContext({ updateDecl });
  C.addSynthesizedDecl(updateDecl);

  return updateDecl;
}

ValueDecl *
DerivedConformance::deriveParameterAggregate(ValueDecl *requirement) {
  if (requirement->getBaseName() == TC.Context.getIdentifier("update")) {
    addFixedLayoutAttrIfNeeded(TC, Nominal);
    return deriveParameterAggregate_update(*this);
  }
  TC.diagnose(requirement->getLoc(),
              diag::broken_parameter_aggregate_requirement);
  return nullptr;
}

Type DerivedConformance::deriveParameterAggregate(
    AssociatedTypeDecl *requirement) {
  if (requirement->getBaseName() == TC.Context.Id_Parameter) {
    addFixedLayoutAttrIfNeeded(TC, Nominal);
    return deriveParameterAggregate_Parameter(TC, Nominal);
  }
  TC.diagnose(requirement->getLoc(),
              diag::broken_parameter_aggregate_requirement);
  return nullptr;
}
