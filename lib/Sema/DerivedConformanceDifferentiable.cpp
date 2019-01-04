//===--- DerivedConformanceDifferentiable.cpp - Derived Differentiable ----===//
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
// This file implements explicit derivation of the Differentiable protocol for
// struct types.
//
//===----------------------------------------------------------------------===//

#include "CodeSynthesis.h"
#include "TypeChecker.h"
#include "swift/AST/AutoDiff.h"
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

static Identifier
getVectorSpaceIdentifier(AutoDiffAssociatedVectorSpaceKind kind,
                         ASTContext &C) {
  switch (kind) {
  case AutoDiffAssociatedVectorSpaceKind::Tangent:
    return C.Id_TangentVector;
  case AutoDiffAssociatedVectorSpaceKind::Cotangent:
    return C.Id_CotangentVector;
  }
}

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

// Get the stored properties of a nominal type that are relevant for
// differentiation, except the ones tagged `@noDerivative`.
static void
getStoredPropertiesForDifferentiation(NominalTypeDecl *nominal,
                                      SmallVectorImpl<VarDecl *> &result) {
  for (auto *vd : nominal->getStoredProperties()) {
    if (vd->getAttrs().hasAttribute<NoDerivativeAttr>())
      continue;
    result.push_back(vd);
  }
}

bool DerivedConformance::canDeriveDifferentiable(NominalTypeDecl *nominal,
                                                 DeclContext *DC) {
  // Nominal type must be a struct. (Zero stored properties is okay.)
  auto *structDecl = dyn_cast<StructDecl>(nominal);
  if (!structDecl)
    return false;
  auto &C = nominal->getASTContext();
  auto *lazyResolver = C.getLazyResolver();
  auto *diffableProto = C.getProtocol(KnownProtocolKind::Differentiable);
  auto *addArithProto = C.getProtocol(KnownProtocolKind::AdditiveArithmetic);

  // Nominal type must not customize `TangentVector` or `CotangentVector` to
  // anything other than `Self`.
  // Otherwise, synthesis is semantically unsupported.
  auto tangentDecls = nominal->lookupDirect(C.Id_TangentVector);
  auto cotangentDecls = nominal->lookupDirect(C.Id_CotangentVector);
  auto isValidVectorSpaceCandidate = [&](ValueDecl *v) {
    if (!v->hasInterfaceType())
      lazyResolver->resolveDeclSignature(v);
    if (!v->hasInterfaceType())
      return false;
    return v->isImplicit() ||
           (v->getInterfaceType()->isEqual(nominal->getInterfaceType()) &&
            TypeChecker::conformsToProtocol(nominal->getDeclaredInterfaceType(),
                                            addArithProto, DC,
                                            ConformanceCheckFlags::Used));
  };
  auto invalidTangentDecls =
      llvm::partition(tangentDecls, isValidVectorSpaceCandidate);
  auto invalidCotangentDecls =
      llvm::partition(cotangentDecls, isValidVectorSpaceCandidate);

  auto validTangentDeclCount =
      std::distance(tangentDecls.begin(), invalidTangentDecls);
  auto invalidTangentDeclCount =
      std::distance(invalidTangentDecls, tangentDecls.end());
  auto validCotangentDeclCount =
      std::distance(cotangentDecls.begin(), invalidCotangentDecls);
  auto invalidCotangentDeclCount =
      std::distance(invalidCotangentDecls, cotangentDecls.end());

  // There cannot be any invalid vector space types.
  // There can be at most one valid vector space type.
  if (invalidTangentDeclCount != 0 || invalidCotangentDeclCount != 0 ||
      validTangentDeclCount > 1 || validCotangentDeclCount > 1)
    return false;

  // All stored properties not marked with `@noDerivative` must conform to
  // `Differentiable`.
  SmallVector<VarDecl *, 16> diffProperties;
  getStoredPropertiesForDifferentiation(structDecl, diffProperties);
  return llvm::all_of(diffProperties, [&](VarDecl *v) {
    if (!v->hasType())
      lazyResolver->resolveDeclSignature(v);
    if (!v->hasType())
      return false;
    auto declType = v->getType()->hasArchetype()
                        ? v->getType()
                        : DC->mapTypeIntoContext(v->getType());
    auto conf = TypeChecker::conformsToProtocol(declType, diffableProto, DC,
                                                ConformanceCheckFlags::Used);
    return (bool)conf;
  });
}

// Get the specified vector space associated type for the given declaration.
// TODO: Generalize and move function to shared place for use with other derived
// conformances.
static Type getVectorSpaceType(VarDecl *decl, DeclContext *DC,
                               AutoDiffAssociatedVectorSpaceKind kind) {
  auto &C = decl->getASTContext();
  auto *diffableProto = C.getProtocol(KnownProtocolKind::Differentiable);
  auto declType = decl->getType()->hasArchetype()
                      ? decl->getType()
                      : DC->mapTypeIntoContext(decl->getType());
  C.getLazyResolver()->resolveDeclSignature(decl);
  auto conf = TypeChecker::conformsToProtocol(declType, diffableProto, DC,
                                              ConformanceCheckFlags::Used);
  if (!conf)
    return Type();
  auto vectorSpaceId = getVectorSpaceIdentifier(kind, C);
  Type vectorSpaceType = ProtocolConformanceRef::getTypeWitnessByName(
      declType, *conf, vectorSpaceId, C.getLazyResolver());
  assert(vectorSpaceType &&
         "Differentiable protocol vector space type not found");
  return vectorSpaceType;
}

// Attempt to find a vector space associated type for the given nominal type.
static StructDecl *
getVectorSpaceStructDecl(NominalTypeDecl *nominal,
                         AutoDiffAssociatedVectorSpaceKind kind) {
  auto module = nominal->getModuleContext();
  auto lookupConformance = LookUpConformanceInModule(module);
  auto vectorSpace =
      nominal->getDeclaredInterfaceType()->getAutoDiffAssociatedVectorSpace(
          kind, lookupConformance);
  if (!vectorSpace)
    return nullptr;
  auto vectorSpaceStructDecl =
      dyn_cast<StructDecl>(vectorSpace->getType()->getAnyNominal());
  if (!vectorSpaceStructDecl)
    return nullptr;
  return vectorSpaceStructDecl;
}

// Get memberwise initializer for a nominal type.
static ConstructorDecl *getMemberwiseInitializer(NominalTypeDecl *nominal) {
  ConstructorDecl *memberwiseInitDecl = nullptr;
  auto ctorDecls = nominal->lookupDirect(DeclBaseName::createConstructor());
  for (auto decl : ctorDecls) {
    auto ctorDecl = dyn_cast<ConstructorDecl>(decl);
    if (!ctorDecl)
      continue;
    // Continue if:
    // - Constructor is not a memberwise initializer.
    // - Constructor is implicit and takes no arguments, and nominal has no
    //   stored properties. This is ad-hoc and accepts empt struct
    //   constructors generated via `TypeChecker::defineDefaultConstructor`.
    if (!ctorDecl->isMemberwiseInitializer() &&
        !(nominal->getStoredProperties().empty() && ctorDecl->isImplicit() &&
          ctorDecl->getParameters()->size() == 0))
      continue;
    assert(!memberwiseInitDecl && "Memberwise initializer already found");
    memberwiseInitDecl = ctorDecl;
  }
  return memberwiseInitDecl;
}

// Synthesize body for a `Differentiable` method requirement.
static void deriveBodyDifferentiable_method(AbstractFunctionDecl *funcDecl,
                                            Identifier methodName,
                                            Identifier methodParamLabel) {
  auto *nominal = funcDecl->getDeclContext()->getSelfNominalTypeDecl();
  auto &C = nominal->getASTContext();

  // Create memberwise initializer for the returned nominal type:
  // `Nominal.init(...)`.
  auto retNominalInterfaceType =
      funcDecl->getMethodInterfaceType()->getAs<AnyFunctionType>()->getResult();
  auto *retNominal = retNominalInterfaceType->getAnyNominal();
  auto retNominalType = funcDecl->mapTypeIntoContext(retNominalInterfaceType);
  auto *retNominalTypeExpr = TypeExpr::createImplicit(retNominalType, C);
  auto *memberwiseInitDecl = getMemberwiseInitializer(retNominal);
  assert(memberwiseInitDecl && "Memberwise initializer must exist");
  auto *initDRE =
      new (C) DeclRefExpr(memberwiseInitDecl, DeclNameLoc(), /*Implicit*/ true);
  initDRE->setFunctionRefKind(FunctionRefKind::SingleApply);
  auto *initExpr = new (C) ConstructorRefCallExpr(initDRE, retNominalTypeExpr);

  // Get method protocol requirement.
  auto *diffProto = C.getProtocol(KnownProtocolKind::Differentiable);
  auto *methodReq = getProtocolRequirement(diffProto, methodName);

  // Get references to `self` and parameter declarations.
  auto *selfDecl = funcDecl->getImplicitSelfDecl();
  auto *selfDRE =
      new (C) DeclRefExpr(selfDecl, DeclNameLoc(), /*Implicit*/ true);
  auto *paramDecl = funcDecl->getParameters()->get(0);
  auto *paramDRE =
      new (C) DeclRefExpr(paramDecl, DeclNameLoc(), /*Implicit*/ true);

  // If this is the `tangentVector(from:)` method and the `TangentVector` and
  // `CotangentVector` types are identical, simply return the parameter
  // `cotangent` expression. This is more efficient than constructing a new
  // `TangentVector` instance, which is unnecessary.
  if (methodName == C.Id_tangentVector &&
      retNominalInterfaceType->isEqual(paramDecl->getInterfaceType())) {
    ASTNode returnStmt = new (C) ReturnStmt(SourceLoc(), paramDRE, true);
    funcDecl->setBody(
        BraceStmt::create(C, SourceLoc(), returnStmt, SourceLoc(), true));
    return;
  }

  // Create call expression applying a member method to a parameter member.
  // Format: `<member>.method(<parameter>.<member>)`.
  // Example: `x.moved(along: direction.x)`.
  auto createMemberMethodCallExpr = [&](VarDecl *retNominalMember) -> Expr * {
    // Find `Self` member corresponding to member from returned nominal type.
    VarDecl *selfMember = nullptr;
    for (auto candidate : nominal->getStoredProperties()) {
      if (candidate->getName() == retNominalMember->getName()) {
        selfMember = candidate;
        break;
      }
    }
    assert(selfMember && "Could not find corresponding self member");
    // If member has `@noDerivative`, create direct reference to member.
    if (retNominalMember->getAttrs().hasAttribute<NoDerivativeAttr>()) {
      return new (C)
          MemberRefExpr(selfDRE, SourceLoc(), selfMember, DeclNameLoc(),
                        /*Implicit*/ true);
    }
    // Otherwise, construct member method call.
    auto module = nominal->getModuleContext();
    auto confRef = module->lookupConformance(selfMember->getType(), diffProto);
    assert(confRef && "Member does not conform to 'Differentiable'");

    // Get member type's method, e.g. `Member.moved(along:)`.
    // Use protocol requirement declaration for the method by default: this
    // will be dynamically dispatched.
    ValueDecl *memberMethodDecl = methodReq;
    // If conformance reference is concrete, then use concrete witness
    // declaration for the operator.
    if (confRef->isConcrete())
      if (auto methodDecl =
              confRef->getConcrete()->getWitnessDecl(methodReq, nullptr))
        memberMethodDecl = methodDecl;
    assert(memberMethodDecl && "Member method declaration must exist");
    auto memberMethodDRE =
        new (C) DeclRefExpr(memberMethodDecl, DeclNameLoc(), /*Implicit*/ true);
    memberMethodDRE->setFunctionRefKind(FunctionRefKind::SingleApply);

    // Create reference to member method: `x.moved(along:)`.
    auto memberExpr =
        new (C) MemberRefExpr(selfDRE, SourceLoc(), selfMember, DeclNameLoc(),
                              /*Implicit*/ true);
    auto memberMethodExpr =
        new (C) DotSyntaxCallExpr(memberMethodDRE, SourceLoc(), memberExpr);

    // Create reference to parameter member: `direction.x`.
    VarDecl *paramMember = nullptr;
    auto paramNominal = paramDecl->getType()->getAnyNominal();
    assert(paramNominal && "Parameter should have a nominal type");
    // Find parameter member corresponding to returned nominal member.
    for (auto candidate : paramNominal->getStoredProperties()) {
      if (candidate->getName() == retNominalMember->getName()) {
        paramMember = candidate;
        break;
      }
    }
    assert(paramMember && "Could not find corresponding parameter member");
    auto paramMemberExpr =
        new (C) MemberRefExpr(paramDRE, SourceLoc(), paramMember, DeclNameLoc(),
                              /*Implicit*/ true);
    // Create expression: `x.moved(along: direction.x)`.
    return CallExpr::createImplicit(C, memberMethodExpr, {paramMemberExpr},
                                    {methodParamLabel});
  };

  // Create array of member method call expressions.
  llvm::SmallVector<Expr *, 2> memberMethodCallExprs;
  llvm::SmallVector<Identifier, 2> memberNames;
  for (auto member : retNominal->getStoredProperties()) {
    // Initialized `let` properties don't get an argument in memberwise
    // initializers.
    if (member->isLet() && member->getParentInitializer())
      continue;
    memberMethodCallExprs.push_back(createMemberMethodCallExpr(member));
    memberNames.push_back(member->getName());
  }
  // Call memberwise initialier with member method call expressions.
  auto *callExpr =
      CallExpr::createImplicit(C, initExpr, memberMethodCallExprs, memberNames);
  ASTNode returnStmt = new (C) ReturnStmt(SourceLoc(), callExpr, true);
  funcDecl->setBody(
      BraceStmt::create(C, SourceLoc(), returnStmt, SourceLoc(), true));
}

// Synthesize body for `moved(along:)`.
static void deriveBodyDifferentiable_moved(AbstractFunctionDecl *funcDecl) {
  auto &C = funcDecl->getASTContext();
  deriveBodyDifferentiable_method(funcDecl, C.Id_moved,
                                  C.getIdentifier("along"));
}

// Synthesize body for `tangentVector(from:)`.
static void
deriveBodyDifferentiable_tangentVector(AbstractFunctionDecl *funcDecl) {
  auto &C = funcDecl->getASTContext();
  deriveBodyDifferentiable_method(funcDecl, C.Id_tangentVector,
                                  C.getIdentifier("from"));
}

// Synthesize function declaration for a `Differentiable` method requirement.
static ValueDecl *deriveDifferentiable_method(
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
                                   /*GenericParams=*/nullptr, params,
                                   TypeLoc::withoutLoc(returnType), parentDC);
  funcDecl->setImplicit();
  funcDecl->setBodySynthesizer(bodySynthesizer);

  if (auto env = parentDC->getGenericEnvironmentOfContext())
    funcDecl->setGenericEnvironment(env);
  funcDecl->computeType();
  funcDecl->copyFormalAccessFrom(nominal, /*sourceIsParentContext*/ true);
  funcDecl->setValidationToChecked();

  derived.addMembersToConformanceContext({funcDecl});
  C.addSynthesizedDecl(funcDecl);

  // Returned nominal type must define a memberwise initializer.
  // Add memberwise initializer if necessary.
  auto returnNominal = returnType->getAnyNominal();
  assert(returnNominal && "Return type must be a nominal type");
  if (!getMemberwiseInitializer(returnNominal)) {
    // The implicit memberwise constructor must be explicitly created so that
    // it can called in `Differentiable` methods. Normally, the memberwise
    // constructor is synthesized during SILGen, which is too late.
    auto *initDecl = createImplicitConstructor(
        TC, returnNominal, ImplicitConstructorKind::Memberwise);
    returnNominal->addMember(initDecl);
    C.addSynthesizedDecl(initDecl);
  }

  return funcDecl;
}

// Synthesize the `moved(along:)` function declaration.
static ValueDecl *deriveDifferentiable_moved(DerivedConformance &derived) {
  auto nominal = derived.Nominal;
  auto &C = derived.TC.Context;
  auto parentDC = derived.getConformanceContext();
  auto selfInterfaceType = parentDC->getDeclaredInterfaceType();

  StructDecl *tangentDecl = getVectorSpaceStructDecl(
      nominal, AutoDiffAssociatedVectorSpaceKind::Tangent);
  assert(tangentDecl && "'TangentVector' struct must exist");
  auto tangentType = tangentDecl->getDeclaredInterfaceType();

  return deriveDifferentiable_method(
      derived, C.Id_moved, C.getIdentifier("along"),
      C.getIdentifier("direction"), tangentType, selfInterfaceType,
      deriveBodyDifferentiable_moved);
}

// Synthesize the `tangentVector(from:)` function declaration.
static ValueDecl *
deriveDifferentiable_tangentVector(DerivedConformance &derived) {
  auto nominal = derived.Nominal;
  auto &C = derived.TC.Context;

  StructDecl *tangentDecl = getVectorSpaceStructDecl(
      nominal, AutoDiffAssociatedVectorSpaceKind::Tangent);
  assert(tangentDecl && "'TangentVector' struct must exist");
  auto tangentType = tangentDecl->getDeclaredInterfaceType();

  StructDecl *cotangentDecl = getVectorSpaceStructDecl(
      nominal, AutoDiffAssociatedVectorSpaceKind::Cotangent);
  assert(cotangentDecl && "'CotangentVector' struct must exist");
  auto cotangentType = cotangentDecl->getDeclaredInterfaceType();

  return deriveDifferentiable_method(
      derived, C.Id_tangentVector, C.getIdentifier("from"),
      C.getIdentifier("cotangent"), cotangentType, tangentType,
      deriveBodyDifferentiable_tangentVector);
}

// Return associated vector space struct for a nominal type, if it exists.
// If not, synthesize the vector space struct.
static StructDecl *
getOrSynthesizeVectorSpaceStruct(DerivedConformance &derived,
                                 AutoDiffAssociatedVectorSpaceKind kind) {
  auto &TC = derived.TC;
  auto parentDC = derived.getConformanceContext();
  auto nominal = derived.Nominal;
  auto &C = nominal->getASTContext();

  // If vector space struct already exists, return it.
  auto vectorSpaceId = getVectorSpaceIdentifier(kind, C);
  auto lookup = nominal->lookupDirect(vectorSpaceId);
  assert(lookup.size() < 2);
  if (lookup.size() == 1) {
    auto structDecl = dyn_cast<StructDecl>(lookup.front());
    assert(structDecl && "Expected lookup result to be a struct");
    return structDecl;
  }

  // Otherwise, synthesize a new vector space struct.
  auto *diffableProto = C.getProtocol(KnownProtocolKind::Differentiable);
  auto diffableType = TypeLoc::withoutLoc(diffableProto->getDeclaredType());
  auto *addArithProto = C.getProtocol(KnownProtocolKind::AdditiveArithmetic);
  auto addArithType = TypeLoc::withoutLoc(addArithProto->getDeclaredType());
  auto *kpiProto = C.getProtocol(KnownProtocolKind::KeyPathIterable);
  auto kpiType = TypeLoc::withoutLoc(kpiProto->getDeclaredType());
  TypeLoc inherited[3] = {diffableType, addArithType, kpiType};
  auto *structDecl = new (C) StructDecl(SourceLoc(), vectorSpaceId, SourceLoc(),
                                        /*Inherited*/ C.AllocateCopy(inherited),
                                        /*GenericParams*/ {}, parentDC);
  structDecl->setImplicit();
  structDecl->copyFormalAccessFrom(nominal, /*sourceIsParentContext*/ true);
  structDecl->getAttrs().add(new (C)
                                 FieldwiseProductSpaceAttr(/*Implicit*/ true));

  // Add members to vector space struct.
  for (auto *member : nominal->getStoredProperties()) {
    // Skip members with `@noDerivative`.
    if (member->getAttrs().hasAttribute<NoDerivativeAttr>())
      continue;
    auto memberAssocType = getVectorSpaceType(member, nominal, kind);
    auto newMember = new (C) VarDecl(
        member->isStatic(), member->getSpecifier(), member->isCaptureList(),
        /*NameLoc*/ SourceLoc(), member->getName(), structDecl);
    // NOTE: `newMember` is not marked as implicit here, because that affects
    // memberwise initializer synthesis.

    auto memberAssocInterfaceType = memberAssocType->hasArchetype()
                                        ? memberAssocType->mapTypeOutOfContext()
                                        : memberAssocType;
    newMember->setInterfaceType(memberAssocInterfaceType);
    newMember->setType(memberAssocType);
    structDecl->addMember(newMember);
    newMember->copyFormalAccessFrom(member, /*sourceIsParentContext*/ true);
    newMember->setValidationToChecked();
    newMember->setSetterAccess(member->getFormalAccess());
    C.addSynthesizedDecl(newMember);
  }

  // The implicit memberwise constructor must be explicitly created so that it
  // can called in `AdditiveArithmetic` and `Differentiable` methods. Normally,
  // the memberwise constructor is synthesized during SILGen, which is too late.
  auto *initDecl = createImplicitConstructor(
      TC, structDecl, ImplicitConstructorKind::Memberwise);
  structDecl->addMember(initDecl);
  C.addSynthesizedDecl(initDecl);

  // After memberwise initializer is synthesized, mark members as implicit.
  for (auto member : structDecl->getStoredProperties())
    member->setImplicit();

  derived.addMembersToConformanceContext({structDecl});
  C.addSynthesizedDecl(structDecl);

  return structDecl;
}

// Synthesize a vector space associated type (either 'TangentVector' or
// 'CotangentVector').
static Type
deriveDifferentiable_VectorSpace(DerivedConformance &derived,
                                 AutoDiffAssociatedVectorSpaceKind kind) {
  auto &TC = derived.TC;
  auto parentDC = derived.getConformanceContext();
  auto nominal = derived.Nominal;
  auto &C = nominal->getASTContext();

  // Check if all members have vector space associated types equal to `Self`.
  SmallVector<VarDecl *, 16> diffProperties;
  getStoredPropertiesForDifferentiation(nominal, diffProperties);
  bool allMembersVectorSpaceEqualsSelf =
      llvm::all_of(diffProperties, [&](VarDecl *member) {
        auto memberAssocType = getVectorSpaceType(member, nominal, kind);
        return member->getType()->isEqual(memberAssocType);
      });

  // Check if nominal type conforms to `AdditiveArithmetic`.
  // This is important because nominal type must conform to `AdditiveArithmetic`
  // in order to be a valid vector space type.
  auto *addArithProto = C.getProtocol(KnownProtocolKind::AdditiveArithmetic);
  auto nominalConformsToAddArith =
      TC.conformsToProtocol(nominal->getDeclaredInterfaceType(), addArithProto,
                            parentDC, ConformanceCheckFlags::Used);
  // Return `Self` if conditions are met.
  if (allMembersVectorSpaceEqualsSelf && nominalConformsToAddArith) {
    return parentDC->mapTypeIntoContext(nominal->getDeclaredInterfaceType());
  }

  // Get or synthesize both `TangentVector` and `CotangentVector` structs at
  // once. Synthesizing both structs is necessary in order to correctly set
  // their mutually recursive associated types.
  auto tangentStruct = getOrSynthesizeVectorSpaceStruct(
      derived, AutoDiffAssociatedVectorSpaceKind::Tangent);
  auto cotangentStruct = getOrSynthesizeVectorSpaceStruct(
      derived, AutoDiffAssociatedVectorSpaceKind::Cotangent);

  // Add a typealias declaration with the given name and underlying target
  // struct type to the source struct.
  auto addVectorSpaceAliasDecl = [&](Identifier name, StructDecl *source,
                                     StructDecl *target) {
    auto lookup = source->lookupDirect(name);
    assert(lookup.size() < 2 &&
           "Expected at most one vector space named member");
    // If implicit typealias with the given name already exists in source
    // struct, return it.
    if (lookup.size() == 1) {
      auto existingAlias = dyn_cast<TypeAliasDecl>(lookup.front());
      assert(existingAlias && existingAlias->isImplicit() &&
             "Expected lookup result to be an implicit typealias");
      return;
    }

    // Otherwise, create a new typealias.
    auto aliasDecl = new (C)
        TypeAliasDecl(SourceLoc(), SourceLoc(), name, SourceLoc(), {}, source);
    aliasDecl->setUnderlyingType(target->getDeclaredInterfaceType());
    aliasDecl->setImplicit();
    aliasDecl->getAttrs().add(new (C)
                                  FieldwiseProductSpaceAttr(/*Implicit*/ true));
    if (auto env = source->getGenericEnvironmentOfContext())
      aliasDecl->setGenericEnvironment(env);
    source->addMember(aliasDecl);
    aliasDecl->copyFormalAccessFrom(source, /*sourceIsParentContext*/ true);
    aliasDecl->setValidationToChecked();
    TC.validateDecl(aliasDecl);
    C.addSynthesizedDecl(aliasDecl);
  };

  // Add vector space typealiases for both vector space structs.
  addVectorSpaceAliasDecl(C.Id_TangentVector, tangentStruct, tangentStruct);
  addVectorSpaceAliasDecl(C.Id_TangentVector, cotangentStruct, cotangentStruct);
  addVectorSpaceAliasDecl(C.Id_CotangentVector, tangentStruct, cotangentStruct);
  addVectorSpaceAliasDecl(C.Id_CotangentVector, cotangentStruct, tangentStruct);

  TC.validateDecl(tangentStruct);
  TC.validateDecl(cotangentStruct);

  // TODO: Enable "canDeriveAdditiveArithmetic" assertions.
  // Blocked by SR-9595 (bug regarding mututally recursive associated types).
  /*
  assert(
      DerivedConformance::canDeriveAdditiveArithmetic(tangentStruct, nominal) &&
      "Should be able to derive 'AdditiveArithmetic'");
  assert(DerivedConformance::canDeriveAdditiveArithmetic(cotangentStruct,
                                                         nominal) &&
         "Should be able to derive 'AdditiveArithmetic'");
  */
  assert(DerivedConformance::canDeriveDifferentiable(tangentStruct, nominal) &&
         "Should be able to derive 'Differentiable'");
  assert(
      DerivedConformance::canDeriveDifferentiable(cotangentStruct, nominal) &&
      "Should be able to derive 'Differentiable'");

  // Return the requested vector space struct type.
  switch (kind) {
  case AutoDiffAssociatedVectorSpaceKind::Tangent:
    return parentDC->mapTypeIntoContext(
        tangentStruct->getDeclaredInterfaceType());
  case AutoDiffAssociatedVectorSpaceKind::Cotangent:
    return parentDC->mapTypeIntoContext(
        cotangentStruct->getDeclaredInterfaceType());
  }
}

ValueDecl *DerivedConformance::deriveDifferentiable(ValueDecl *requirement) {
  if (requirement->getBaseName() == TC.Context.Id_moved)
    return deriveDifferentiable_moved(*this);
  if (requirement->getBaseName() == TC.Context.Id_tangentVector)
    return deriveDifferentiable_tangentVector(*this);
  TC.diagnose(requirement->getLoc(), diag::broken_differentiable_requirement);
  return nullptr;
}

Type DerivedConformance::deriveDifferentiable(AssociatedTypeDecl *requirement) {
  if (requirement->getBaseName() == TC.Context.Id_TangentVector) {
    return deriveDifferentiable_VectorSpace(
        *this, AutoDiffAssociatedVectorSpaceKind::Tangent);
  }
  if (requirement->getBaseName() == TC.Context.Id_CotangentVector) {
    return deriveDifferentiable_VectorSpace(
        *this, AutoDiffAssociatedVectorSpaceKind::Cotangent);
  }
  TC.diagnose(requirement->getLoc(), diag::broken_differentiable_requirement);
  return nullptr;
}
