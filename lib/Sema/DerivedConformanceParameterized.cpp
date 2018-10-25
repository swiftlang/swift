//===--- DerivedConformanceParameterized.cpp - Derived Parameterized ------===//
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
// SWIFT_ENABLE_TENSORFLOW
//
// This file implements explicit derivation of the Parameterized protocol for a
// nominal type.
//
//===----------------------------------------------------------------------===//

// TODO:
// - Support synthesis when non-synthesized `Parameters` struct does not have
//   implicit memberwise initializer. Currently, user-defined memberwise
//   initializers do not work.

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

// Represents the possible outcomes of checking whether or not the `Parameters`
// struct exists.
enum ParametersStructStatus {
  Valid,
  Invalid,
  DoesNotExist
};

// Return the "parameter type" corresponding to a ValueDecl.
// If the decl conforms to Parameterized, return the `Parameters` associated
// type. Otherwise, directly return the decl's type.
static Type getParameterType(ValueDecl *decl) {
  auto &C = decl->getASTContext();
  auto *module = decl->getModuleContext();
  auto *parameterizedProto = C.getProtocol(KnownProtocolKind::Parameterized);
  auto conf =
      module->lookupConformance(decl->getInterfaceType(), parameterizedProto);
  if (!conf)
    return decl->getInterfaceType();
  Type parametersType = ProtocolConformanceRef::getTypeWitnessByName(
      decl->getInterfaceType(), *conf, C.Id_Parameters, nullptr);
  assert(parametersType && "'Parameters' associated type not found");
  return parametersType;
}

// Return true if `parametersDecl` is a valid `Parameters` struct for a nominal
// type.
static bool isValidParametersStruct(NominalTypeDecl *nominal,
                                    StructDecl *parametersDecl) {
  // Add all stored properties of the `Parameters` struct to a map.
  // Also, check that `Parameters` struct has a memberwise initializer.
  llvm::DenseMap<Identifier, VarDecl *> parameters;
  ConstructorDecl *memberwiseInitDecl = nullptr;
  for (auto member : parametersDecl->getMembers()) {
    // Find memberwise initializer.
    if (!memberwiseInitDecl) {
      auto initDecl = dyn_cast<ConstructorDecl>(member);
      if (initDecl && initDecl->isMemberwiseInitializer())
        memberwiseInitDecl = initDecl;
    }
    // Add `Parameters` struct stored properties to map.
    auto varDecl = dyn_cast<VarDecl>(member);
    if (!varDecl || varDecl->isStatic() || !varDecl->hasStorage())
      continue;
    parameters[varDecl->getName()] = varDecl;
  }
  if (!memberwiseInitDecl)
    return false;

  SmallVector<VarDecl *, 8> tfParamDecls;
  nominal->getAllTFParameters(tfParamDecls);
  // If there's a parameter count mismatch, return false.
  if (tfParamDecls.size() != parameters.size())
    return false;

  // Check that each parameter of the nominal type maps to a stored property in
  // the `Parameters` struct.
  for (auto parameter : tfParamDecls) {
    auto it = parameters.find(parameter->getName());
    if (it == parameters.end() ||
        !it->second->getType()->isEqual(getParameterType(parameter))) {
      return false;
    }
  }
  return true;
}

static std::pair<StructDecl *, ParametersStructStatus>
getParametersStructDecl(NominalTypeDecl *nominal) {
  auto &ctx = nominal->getASTContext();
  ParametersStructStatus status = DoesNotExist;
  StructDecl *parametersDecl = nullptr;

  for (auto memberDecl : nominal->getMembers()) {
    auto structDecl = dyn_cast<StructDecl>(memberDecl);
    if (!structDecl || structDecl->getName() != ctx.Id_Parameters)
      continue;
    parametersDecl = structDecl;
    if (isValidParametersStruct(nominal, structDecl))
      return std::make_pair(parametersDecl, Valid);
    else
      status = Invalid;
  }
  return std::make_pair(parametersDecl, status);
}

// Return the protocol requirement with the specified name.
static ValueDecl *getProtocolRequirement(ProtocolDecl *proto, Identifier name) {
  auto lookup = proto->lookupDirect(name);
  assert(lookup.size() == 1 && "Ambiguous protocol requirement");
  return lookup[0];
}

static void derivedBody_allParametersGetter(AbstractFunctionDecl *getterDecl) {
  auto *nominal = getterDecl->getDeclContext()->getSelfNominalTypeDecl();
  auto &C = nominal->getASTContext();

  auto *parametersDecl = getParametersStructDecl(nominal).first;
  assert(parametersDecl && "'Parameters' struct not found but should exist");
  ConstructorDecl *parametersInitDecl = nullptr;
  for (auto member : parametersDecl->getMembers()) {
    auto initDecl = dyn_cast<ConstructorDecl>(member);
    if (!initDecl || !initDecl->isMemberwiseInitializer())
      continue;
    assert(!parametersInitDecl && "'Parameters' initializer already found");
    parametersInitDecl = initDecl;
  }
  assert(parametersInitDecl && "'Parameters' implicit initializer not found");

  auto *selfDecl = getterDecl->getImplicitSelfDecl();
  auto *selfDRE =
      new (C) DeclRefExpr(selfDecl, DeclNameLoc(), /*Implicit*/ true);

  auto *initDRE =
      new (C) DeclRefExpr(parametersInitDecl, DeclNameLoc(), /*Implicit*/ true);
  initDRE->setFunctionRefKind(FunctionRefKind::SingleApply);

  auto parametersType =
      nominal->mapTypeIntoContext(parametersDecl->getDeclaredInterfaceType());
  Expr *baseExpr = TypeExpr::createImplicit(parametersType, C);
  auto *initExpr = new (C) ConstructorRefCallExpr(initDRE, baseExpr);
  initExpr->setThrows(false);
  initExpr->setImplicit();

  auto *parameterizedProto = C.getProtocol(KnownProtocolKind::Parameterized);
  auto allParametersReq =
    getProtocolRequirement(parameterizedProto, C.Id_allParameters);

  auto getUnderlyingParameter = [&](Expr *expr, VarDecl *param) -> Expr * {
    auto module = nominal->getModuleContext();
    auto confRef =
        module->lookupConformance(param->getType(), parameterizedProto);
    if (!confRef)
      return expr;
    auto conf = confRef->getConcrete();
    auto allParamsDecl = conf->getWitnessDecl(allParametersReq, nullptr);
    return new (C) MemberRefExpr(expr, SourceLoc(), allParamsDecl,
                                 DeclNameLoc(), /*Implicit*/ true);
  };

  SmallVector<Expr *, 2> members;
  SmallVector<Identifier, 2> memberNames;

  // The `Parameters` struct initializer may take parameters in a different
  // order than marked in the nominal type. Thus, mapping parameters to their
  // name is necessary to maintain correctness.
  llvm::DenseMap<Identifier, VarDecl *> parameterMap;
  SmallVector<VarDecl *, 8> parameters;
  nominal->getAllTFParameters(parameters);
  for (auto *param : parameters)
    parameterMap[param->getName()] = param;

  for (auto initParam : *parametersInitDecl->getParameters()) {
    auto param = parameterMap[initParam->getName()];
    Expr *member = new (C) MemberRefExpr(selfDRE, SourceLoc(), param,
                                         DeclNameLoc(), /*Implicit*/ true);
    member->setType(param->getType());
    member = getUnderlyingParameter(member, param);
    members.push_back(member);
    memberNames.push_back(param->getName());
  }
  Expr *callExpr = CallExpr::createImplicit(C, initExpr, members, memberNames);

  ASTNode returnStmt = new (C) ReturnStmt(SourceLoc(), callExpr, true);
  getterDecl->setBody(
      BraceStmt::create(C, SourceLoc(), returnStmt, SourceLoc(), true));
}

static void derivedBody_allParametersSetter(AbstractFunctionDecl *setterDecl) {
  auto *nominal = setterDecl->getDeclContext()->getSelfNominalTypeDecl();
  auto &C = nominal->getASTContext();

  auto *selfDecl = setterDecl->getImplicitSelfDecl();
  Expr *selfDRE =
      new (C) DeclRefExpr(selfDecl, DeclNameLoc(), /*Implicit*/ true);

  auto *parametersDecl = getParametersStructDecl(nominal).first;
  assert(parametersDecl && "'Parameters' struct not found but should exist");
  auto *newValueDecl = setterDecl->getParameters()->get(0);
  Expr *newValueDRE =
      new (C) DeclRefExpr(newValueDecl, DeclNameLoc(), /*Implicit*/ true);

  auto *parameterizedProto = C.getProtocol(KnownProtocolKind::Parameterized);
  auto allParametersReq =
    getProtocolRequirement(parameterizedProto, C.Id_allParameters);

  // Returns the underlying parameter of a VarDecl `x`.
  // If `x` conforms to `Parameterized`, return `x.allParameters`.
  // Otherwise, return `x`.
  auto getUnderlyingParameter = [&](VarDecl *param) -> ValueDecl * {
    auto module = nominal->getModuleContext();
    auto confRef =
        module->lookupConformance(param->getType(), parameterizedProto);
    if (!confRef)
      return param;
    auto conf = confRef->getConcrete();
    auto allParamsDecl = conf->getWitnessDecl(allParametersReq, nullptr);
    return allParamsDecl;
  };

  // Map `Parameters` struct members to their names for efficient lookup.
  llvm::DenseMap<Identifier, VarDecl *> parametersMembers;
  for (auto member : parametersDecl->getMembers()) {
    auto *varDecl = dyn_cast<VarDecl>(member);
    if (!varDecl || varDecl->isStatic() || !varDecl->hasStorage())
      continue;
    parametersMembers[varDecl->getName()] = varDecl;
  }

  SmallVector<ASTNode, 2> assignNodes;
  SmallVector<VarDecl *, 8> tfParamDecls;
  nominal->getAllTFParameters(tfParamDecls);
  for (auto param : tfParamDecls) {
    Expr *lhs;
    auto lhsParam = getUnderlyingParameter(param);
    if (param == lhsParam) {
      lhs = new (C) MemberRefExpr(selfDRE, SourceLoc(), param, DeclNameLoc(),
                                  /*Implicit*/ true);
    } else {
      auto *paramDRE = new (C) MemberRefExpr(selfDRE, SourceLoc(), param,
                                             DeclNameLoc(), /*Implicit*/ true);
      lhs = new (C) MemberRefExpr(paramDRE, SourceLoc(), lhsParam,
                                  DeclNameLoc(), /*Implicit*/ true);
    }
    auto *rhs = new (C) MemberRefExpr(newValueDRE, SourceLoc(),
                                      parametersMembers[param->getName()],
                                      DeclNameLoc(), /*Implicit*/ true);
    auto *assignExpr = new (C) AssignExpr(lhs, SourceLoc(), rhs,
                                          /*Implicit*/ true);
    assignNodes.push_back(assignExpr);
  }

  setterDecl->setBody(
      BraceStmt::create(C, SourceLoc(), assignNodes, SourceLoc(), true));
}

static ValueDecl *
deriveParameterized_allParameters(DerivedConformance &derived) {
  auto nominal = derived.Nominal;
  auto &TC = derived.TC;
  auto &C = TC.Context;

  StructDecl *parametersDecl;
  ParametersStructStatus status;
  std::tie(parametersDecl, status) = getParametersStructDecl(nominal);
  switch (status) {
  case DoesNotExist:
    TC.diagnose(derived.ConformanceDecl->getLoc(),
                diag::parameterized_no_parameters_struct,
                derived.getProtocolType());
    return nullptr;
  case Invalid:
    TC.diagnose(parametersDecl, diag::parameterized_invalid_parameters_struct,
                derived.getProtocolType());
    return nullptr;
  case Valid:
    break;
  }

  auto returnInterfaceTy = parametersDecl->getDeclaredInterfaceType();
  auto returnTy = nominal->mapTypeIntoContext(returnInterfaceTy);

  VarDecl *allParamsDecl;
  PatternBindingDecl *pbDecl;
  std::tie(allParamsDecl, pbDecl) = derived.declareDerivedProperty(
      C.Id_allParameters, returnInterfaceTy, returnTy, /*isStatic*/ false,
      /*isFinal*/ true);

  auto *getterDecl =
      derived.declareDerivedPropertyGetter(derived.TC, allParamsDecl, returnTy);
  getterDecl->setBodySynthesizer(&derivedBody_allParametersGetter);

  auto *setterDecl =
      derived.declareDerivedPropertySetter(derived.TC, allParamsDecl, returnTy);
  setterDecl->setBodySynthesizer(&derivedBody_allParametersSetter);

  allParamsDecl->setAccessors(StorageImplInfo::getMutableComputed(),
                              SourceLoc(), {getterDecl, setterDecl},
                              SourceLoc());

  derived.addMembersToConformanceContext(
      {getterDecl, setterDecl, allParamsDecl, pbDecl});

  addExpectedOpaqueAccessorsToStorage(TC, allParamsDecl);
  triggerAccessorSynthesis(TC, allParamsDecl);

  return allParamsDecl;
}

ValueDecl *DerivedConformance::deriveParameterized(ValueDecl *requirement) {
  if (requirement->getBaseName() == TC.Context.Id_allParameters)
    return deriveParameterized_allParameters(*this);
  TC.diagnose(requirement->getLoc(), diag::broken_parameterized_requirement);
  return nullptr;
}

static Type deriveParameterized_Parameters(DerivedConformance &derived) {
  auto &TC = derived.TC;
  auto parentDC = derived.getConformanceContext();
  auto nominal = derived.Nominal;
  auto &C = nominal->getASTContext();

  auto *paramGroupProto = C.getProtocol(KnownProtocolKind::ParameterGroup);
  auto paramGroupType = TypeLoc::withoutLoc(paramGroupProto->getDeclaredType());
  auto *parametersDecl =
      new (C) StructDecl(SourceLoc(), C.Id_Parameters, SourceLoc(),
                         /*Inherited*/ {}, /*GenericParams*/ {}, parentDC);
  parametersDecl->setImplicit();
  parametersDecl->copyFormalAccessFrom(nominal, /*sourceIsParentContext*/ true);
  TC.validateDecl(parametersDecl);

  auto *parameterizedProto = C.getProtocol(KnownProtocolKind::Parameterized);
  SmallVector<VarDecl *, 8> tfParamDecls;
  nominal->getAllTFParameters(tfParamDecls);
  for (auto *parameter : tfParamDecls) {
    Type newParameterType;
    auto conf =
        TC.conformsToProtocol(parameter->getType(), parameterizedProto,
                              parentDC, ConformanceCheckFlags::InExpression);
    if (conf) {
      newParameterType = ProtocolConformanceRef::getTypeWitnessByName(
          parameter->getType(), *conf, C.Id_Parameters, &TC);
    } else {
      newParameterType = parameter->getType();
    }

    auto newParameter =
        new (C) VarDecl(parameter->isStatic(), parameter->getSpecifier(),
                        parameter->isCaptureList(), /*NameLoc*/ SourceLoc(),
                        parameter->getName(), parametersDecl);
    // NOTE: `newParameter` is not marked as implicit here, because that affects
    // memberwise initializer synthesis.
    newParameter->setInterfaceType(newParameterType);
    parametersDecl->addMember(newParameter);
    newParameter->copyFormalAccessFrom(parameter,
                                       /*sourceIsParentContext*/ true);
    newParameter->setValidationToChecked();
    newParameter->setSetterAccess(parameter->getFormalAccess());
    C.addSynthesizedDecl(newParameter);
  }
  parametersDecl->setValidationToChecked();

  // Add conformance to the ParameterGroup protocol, if possible.
  // The ParameterGroup protocol requirements will be derived.
  if (DerivedConformance::canDeriveParameterGroup(parametersDecl)) {
    TypeLoc inherited[1] = {paramGroupType};
    parametersDecl->setInherited(C.AllocateCopy(inherited));
  }

  // The implicit memberwise constructor must be explicitly created so that it
  // can called when synthesizing the `allParameters` getter. Normally, the
  // memberwise constructor is synthesized during SILGen, which is too late.
  auto *initDecl = createImplicitConstructor(
      TC, parametersDecl, ImplicitConstructorKind::Memberwise);
  parametersDecl->addMember(initDecl);
  C.addSynthesizedDecl(initDecl);

  // After memberwise initializer is synthesized, mark members as implicit.
  for (auto member : parametersDecl->getMembers()) {
    auto varDecl = dyn_cast<VarDecl>(member);
    if (!varDecl || varDecl->isStatic() || !varDecl->hasStorage())
      continue;
    varDecl->setImplicit();
  }

  derived.addMembersToConformanceContext({parametersDecl});
  C.addSynthesizedDecl(parametersDecl);

  auto parametersType = parametersDecl->getDeclaredInterfaceType();
  return derived.getConformanceContext()->mapTypeIntoContext(parametersType);
}

Type DerivedConformance::deriveParameterized(AssociatedTypeDecl *requirement) {
  if (requirement->getBaseName() == TC.Context.Id_Parameters)
    return deriveParameterized_Parameters(*this);
  TC.diagnose(requirement->getLoc(), diag::broken_parameterized_requirement);
  return nullptr;
}
