//===--- DerivedConformanceVectorProtocol.cpp -----------------------------===//
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
// This file implements explicit derivation of the VectorProtocol protocol for
// struct types.
//
//===----------------------------------------------------------------------===//

#include "CodeSynthesis.h"
#include "TypeChecker.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/AST/GenericSignature.h"
#include "swift/AST/Module.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/Pattern.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/Stmt.h"
#include "swift/AST/Types.h"
#include "DerivedConformances.h"

using namespace swift;

// Return the protocol requirement with the specified name.
// TODO: Move function to shared place for use with other derived conformances.
static ValueDecl *getProtocolRequirement(ProtocolDecl *proto, Identifier name) {
  auto lookup = proto->lookupDirect(name);
  // Erase declarations that are not protocol requirements.
  // This is important for removing default implementations of the same name.
  llvm::erase_if(lookup, [](ValueDecl *v) {
    return !isa<ProtocolDecl>(v->getDeclContext()) ||
           !v->isProtocolRequirement();
  });
  assert(lookup.size() == 1 && "Ambiguous protocol requirement");
  return lookup.front();
}

// Return true if given nominal type has a `let` stored with an initial value.
// TODO: Move function to shared place for use with other derived conformances.
static bool hasLetStoredPropertyWithInitialValue(NominalTypeDecl *nominal) {
  return llvm::any_of(nominal->getStoredProperties(), [&](VarDecl *v) {
    return v->isLet() && v->hasInitialValue();
  });
}

// Return the `VectorSpaceScalar` associated type for the given `ValueDecl` if
// it conforms to `VectorProtocol` in the given context. Otherwise, return
// `nullptr`.
static Type getVectorProtocolVectorSpaceScalarAssocType(
    VarDecl *varDecl, DeclContext *DC) {
  auto &C = varDecl->getASTContext();
  auto *vectorProto = C.getProtocol(KnownProtocolKind::VectorProtocol);
  if (!varDecl->hasInterfaceType())
    C.getLazyResolver()->resolveDeclSignature(varDecl);
  if (!varDecl->hasInterfaceType())
    return nullptr;
  auto varType = DC->mapTypeIntoContext(varDecl->getValueInterfaceType());
  auto conf = TypeChecker::conformsToProtocol(varType, vectorProto, DC, None);
  if (!conf)
    return nullptr;
  return conf->getTypeWitnessByName(varType, C.Id_VectorSpaceScalar);
}

// Return the `VectorSpaceScalar` associated type for the given nominal type in
// the given context, or `nullptr` if `VectorSpaceScalar` cannot be derived.
static Type deriveVectorProtocol_VectorSpaceScalar(NominalTypeDecl *nominal,
                                                   DeclContext *DC) {
  auto &C = DC->getASTContext();
  // Nominal type must be a struct. (Zero stored properties is okay.)
  if (!isa<StructDecl>(nominal))
    return nullptr;
  // If all stored properties conform to `VectorProtocol` and have the same
  // `VectorSpaceScalar` associated type, return that `VectorSpaceScalar`
  // associated type. Otherwise, the `VectorSpaceScalar` type cannot be derived.
  Type sameScalarType;
  for (auto member : nominal->getStoredProperties()) {
    if (!member->hasInterfaceType())
      C.getLazyResolver()->resolveDeclSignature(member);
    if (!member->hasInterfaceType())
      return nullptr;
    auto scalarType = getVectorProtocolVectorSpaceScalarAssocType(member, DC);
    // If stored property does not conform to `VectorProtocol`, return nullptr.
    if (!scalarType)
      return nullptr;
    // If same `VectorSpaceScalar` type has not been set, set it for the first
    // time.
    if (!sameScalarType) {
      sameScalarType = scalarType;
      continue;
    }
    // If stored property `VectorSpaceScalar` types do not match, return
    // nullptr.
    if (!scalarType->isEqual(sameScalarType))
      return nullptr;
  }
  return sameScalarType;
}

bool DerivedConformance::canDeriveVectorProtocol(NominalTypeDecl *nominal,
                                                 DeclContext *DC) {
  // Must not have any `let` stored properties with an initial value.
  // - This restriction may be lifted later with support for "true" memberwise
  //   initializers that initialize all stored properties, including initial
  //   value information.
  if (hasLetStoredPropertyWithInitialValue(nominal))
    return false;
  // Must be able to derive `VectorSpaceScalar` associated type.
  return bool(deriveVectorProtocol_VectorSpaceScalar(nominal, DC));
}

// Synthesize body for a `VectorProtocol` method requirement.
static void deriveBodyVectorProtocol_method(AbstractFunctionDecl *funcDecl,
                                            Identifier methodName,
                                            Identifier methodParamLabel) {
  auto *parentDC = funcDecl->getParent();
  auto *nominal = parentDC->getSelfNominalTypeDecl();
  auto &C = nominal->getASTContext();

  // Create memberwise initializer: `Nominal.init(...)`.
  auto *memberwiseInitDecl = nominal->getEffectiveMemberwiseInitializer();
  assert(memberwiseInitDecl && "Memberwise initializer must exist");
  auto *initDRE =
      new (C) DeclRefExpr(memberwiseInitDecl, DeclNameLoc(), /*Implicit*/ true);
  initDRE->setFunctionRefKind(FunctionRefKind::SingleApply);
  auto *nominalTypeExpr = TypeExpr::createForDecl(SourceLoc(), nominal,
                                                  funcDecl, /*Implicit*/ true);
  auto *initExpr = new (C) ConstructorRefCallExpr(initDRE, nominalTypeExpr);

  // Get method protocol requirement.
  auto *vectorProto = C.getProtocol(KnownProtocolKind::VectorProtocol);
  auto *methodReq = getProtocolRequirement(vectorProto, methodName);

  // Get references to `self` and parameter declarations.
  auto *selfDecl = funcDecl->getImplicitSelfDecl();
  auto *selfDRE =
      new (C) DeclRefExpr(selfDecl, DeclNameLoc(), /*Implicit*/ true);
  auto *paramDecl = funcDecl->getParameters()->get(0);
  auto *paramDRE =
      new (C) DeclRefExpr(paramDecl, DeclNameLoc(), /*Implicit*/ true);

  // Create call expression applying a member method to the parameter.
  // Format: `<member>.method(<parameter>)`.
  // Example: `x.scaled(by: scalar)`.
  auto createMemberMethodCallExpr = [&](VarDecl *member) -> Expr * {
    auto *module = nominal->getModuleContext();
    auto memberType =
        parentDC->mapTypeIntoContext(member->getValueInterfaceType());
    auto confRef = module->lookupConformance(memberType, vectorProto);
    assert(confRef && "Member does not conform to `VectorNumeric`");

    // Get member type's method, e.g. `Member.scaled(by:)`.
    // Use protocol requirement declaration for the method by default: this
    // will be dynamically dispatched.
    ValueDecl *memberMethodDecl = methodReq;
    // If conformance reference is concrete, then use concrete witness
    // declaration for the operator.
    if (confRef->isConcrete()) {
      if (auto *concreteMemberMethodDecl =
              confRef->getConcrete()->getWitnessDecl(methodReq,
                                                     C.getLazyResolver()))
        memberMethodDecl = concreteMemberMethodDecl;
      assert(memberMethodDecl);
    }
    assert(memberMethodDecl && "Member method declaration must exist");
    auto memberMethodDRE =
        new (C) DeclRefExpr(memberMethodDecl, DeclNameLoc(), /*Implicit*/ true);
    memberMethodDRE->setFunctionRefKind(FunctionRefKind::SingleApply);

    // Create reference to member method: `x.scaled(by:)`.
    auto memberExpr =
        new (C) MemberRefExpr(selfDRE, SourceLoc(), member, DeclNameLoc(),
                              /*Implicit*/ true);
    auto memberMethodExpr =
        new (C) DotSyntaxCallExpr(memberMethodDRE, SourceLoc(), memberExpr);

    // Create expression: `x.scaled(by: scalar)`.
    return CallExpr::createImplicit(C, memberMethodExpr, {paramDRE},
                                    {methodParamLabel});
  };

  // Create array of member method call expressions.
  llvm::SmallVector<Expr *, 2> memberMethodCallExprs;
  llvm::SmallVector<Identifier, 2> memberNames;
  for (auto *member : nominal->getStoredProperties()) {
    memberMethodCallExprs.push_back(createMemberMethodCallExpr(member));
    memberNames.push_back(member->getName());
  }
  // Call memberwise initializer with member method call expressions.
  auto *callExpr =
      CallExpr::createImplicit(C, initExpr, memberMethodCallExprs, memberNames);
  ASTNode returnStmt = new (C) ReturnStmt(SourceLoc(), callExpr, true);
  funcDecl->setBody(
      BraceStmt::create(C, SourceLoc(), returnStmt, SourceLoc(), true));
}

// Synthesize body for `scaled(by:)`.
static void deriveBodyVectorProtocol_scalarMultiply(
    AbstractFunctionDecl *funcDecl, void *) {
  auto &C = funcDecl->getASTContext();
  deriveBodyVectorProtocol_method(funcDecl, C.Id_scaled, C.getIdentifier("by"));
}

// Synthesize function declaration for a `VectorProtocol` method requirement.
static ValueDecl *deriveVectorProtocol_method(
    DerivedConformance &derived, Identifier methodName, Identifier argumentName,
    Identifier parameterName, Type parameterType, Type returnType,
    AbstractFunctionDecl::BodySynthesizer bodySynthesizer) {
  auto nominal = derived.Nominal;
  auto &TC = derived.TC;
  auto &C = derived.TC.Context;
  auto parentDC = derived.getConformanceContext();

  auto *param =
      new (C) ParamDecl(VarDecl::Specifier::Default, SourceLoc(), SourceLoc(),
                        argumentName, SourceLoc(), parameterName, parentDC);
  param->setInterfaceType(parameterType);
  ParameterList *params = ParameterList::create(C, {param});

  DeclName declName(C, methodName, params);
  auto funcDecl = FuncDecl::create(C, SourceLoc(), StaticSpellingKind::None,
                                   SourceLoc(), declName, SourceLoc(),
                                   /*Throws*/ false, SourceLoc(),
                                   /*GenericParams*/ nullptr, params,
                                   TypeLoc::withoutLoc(returnType), parentDC);
  funcDecl->setImplicit();
  funcDecl->setBodySynthesizer(bodySynthesizer.Fn, bodySynthesizer.Context);

  if (auto env = parentDC->getGenericEnvironmentOfContext())
    funcDecl->setGenericEnvironment(env);
  funcDecl->computeType();
  funcDecl->copyFormalAccessFrom(nominal, /*sourceIsParentContext*/ true);
  funcDecl->setValidationToChecked();

  derived.addMembersToConformanceContext({funcDecl});
  C.addSynthesizedDecl(funcDecl);

  // Returned nominal type must define a memberwise initializer.
  // Add memberwise initializer if necessary.
  if (!nominal->getEffectiveMemberwiseInitializer()) {
    // The implicit memberwise constructor must be explicitly created so that
    // it can called in `VectorProtocol` methods. Normally, the memberwise
    // constructor is synthesized during SILGen, which is too late.
    auto *initDecl = createImplicitConstructor(
        TC, nominal, ImplicitConstructorKind::Memberwise);
    nominal->addMember(initDecl);
    C.addSynthesizedDecl(initDecl);
  }

  return funcDecl;
}

// Synthesize the `scaled(by:)` function declaration.
static ValueDecl *deriveVectorProtocol_scaled(DerivedConformance &derived) {
  auto &C = derived.TC.Context;
  auto *nominal = derived.Nominal;
  auto *parentDC = derived.getConformanceContext();

  auto selfInterfaceType = parentDC->getDeclaredInterfaceType();
  auto scalarType = deriveVectorProtocol_VectorSpaceScalar(nominal, parentDC)
      ->mapTypeOutOfContext();

  return deriveVectorProtocol_method(
      derived, C.Id_scaled, C.getIdentifier("by"), C.getIdentifier("scalar"),
      scalarType, selfInterfaceType,
      {deriveBodyVectorProtocol_scalarMultiply, nullptr});
}

ValueDecl *DerivedConformance::deriveVectorProtocol(ValueDecl *requirement) {
  // Diagnose conformances in disallowed contexts.
  if (checkAndDiagnoseDisallowedContext(requirement))
    return nullptr;
  if (requirement->getBaseName() == TC.Context.Id_scaled)
    return deriveVectorProtocol_scaled(*this);
  TC.diagnose(requirement->getLoc(), diag::broken_vector_protocol_requirement);
  return nullptr;
}

Type DerivedConformance::deriveVectorProtocol(AssociatedTypeDecl *requirement) {
  // Diagnose conformances in disallowed contexts.
  if (checkAndDiagnoseDisallowedContext(requirement))
    return nullptr;
  if (requirement->getBaseName() == TC.Context.Id_VectorSpaceScalar)
    return deriveVectorProtocol_VectorSpaceScalar(
        Nominal, getConformanceContext());
  TC.diagnose(requirement->getLoc(), diag::broken_vector_protocol_requirement);
  return nullptr;
}
