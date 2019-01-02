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
// differentiation.
// - If the nominal conforms to `Parameterized`, return only the stored
//   properties marked with `@TFParameter`.
// - Otherwise, return all stored properties.
static SmallVector<VarDecl *, 4>
getStoredPropertiesForDifferentiation(NominalTypeDecl *nominal) {
  auto &C = nominal->getASTContext();
  auto *parameterizedProto = C.getProtocol(KnownProtocolKind::Parameterized);
  SmallVector<VarDecl *, 4> storedProperties;
  if (TypeChecker::conformsToProtocol(
          nominal->getDeclaredInterfaceType(), parameterizedProto,
          nominal->getDeclContext(), ConformanceCheckFlags::Used)) {
    nominal->getAllTFParameters(storedProperties);
  } else {
    storedProperties.append(nominal->getStoredProperties().begin(),
                            nominal->getStoredProperties().end());
  }
  return storedProperties;
}

bool DerivedConformance::canDeriveDifferentiable(NominalTypeDecl *nominal) {
  // Nominal type must be a struct. (Zero stored properties is okay.)
  auto *structDecl = dyn_cast<StructDecl>(nominal);
  if (!structDecl)
    return false;
  auto &C = nominal->getASTContext();
  auto *lazyResolver = C.getLazyResolver();
  auto *diffableProto = C.getProtocol(KnownProtocolKind::Differentiable);

  // Nominal type must not customize `TangentVector` or `CotangentVector` to
  // anything other than `Self`.
  // Otherwise, synthesis is semantically unsupported.
  auto tangentDecls = nominal->lookupDirect(C.Id_TangentVector);
  auto cotangentDecls = nominal->lookupDirect(C.Id_CotangentVector);
  auto isValidVectorSpaceDecl = [&](ValueDecl *v) {
    if (!v->hasInterfaceType())
      lazyResolver->resolveDeclSignature(v);
    if (!v->hasInterfaceType())
      return false;
    return v->isImplicit() ||
           v->getInterfaceType()->isEqual(nominal->getInterfaceType());
  };
  auto invalidTangentDecls =
      llvm::partition(tangentDecls, isValidVectorSpaceDecl);
  auto invalidCotangentDecls =
      llvm::partition(cotangentDecls, isValidVectorSpaceDecl);

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

  // If there are no valid vector space types, `Self` must conform to either:
  // - `VectorNumeric`. Vector space types will be set to `Self`.
  // - `Parameterized`. Vector space types will be set to `Parameters` member
  //   struct type.
  // TODO(dan-zheng): Lift this restriction.
  if (validTangentDeclCount == 0 || validCotangentDeclCount == 0) {
    auto *vectorNumericProto = C.getProtocol(KnownProtocolKind::VectorNumeric);
    auto *parameterizedProto = C.getProtocol(KnownProtocolKind::Parameterized);
    if (!TypeChecker::conformsToProtocol(
            nominal->getDeclaredInterfaceType(), vectorNumericProto,
            nominal->getDeclContext(), ConformanceCheckFlags::Used) &&
        !TypeChecker::conformsToProtocol(
            nominal->getDeclaredInterfaceType(), parameterizedProto,
            nominal->getDeclContext(), ConformanceCheckFlags::Used))
      return false;
  }

  // All stored properties must conform to `Differentiable`.
  // Currently, all stored properties must also have
  // `Self == TangentVector == CotangentVector`.
  // TODO(dan-zheng): Lift this restriction.
  return llvm::all_of(
      getStoredPropertiesForDifferentiation(structDecl), [&](VarDecl *v) {
        if (!v->hasType())
          lazyResolver->resolveDeclSignature(v);
        if (!v->hasType())
          return false;
        auto conf = TypeChecker::conformsToProtocol(
            v->getType(), diffableProto, v->getDeclContext(),
            ConformanceCheckFlags::Used);
        if (!conf)
          return false;

        Type memberTangentType = ProtocolConformanceRef::getTypeWitnessByName(
            v->getType(), *conf, C.Id_TangentVector, lazyResolver);
        Type memberCotangentType = ProtocolConformanceRef::getTypeWitnessByName(
            v->getType(), *conf, C.Id_CotangentVector, lazyResolver);
        return memberTangentType->isEqual(v->getType()) &&
               memberCotangentType->isEqual(v->getType());
      });
}

// Get the specified vector space associated type for the given declaration.
// TODO: Generalize and move function to shared place for use with other derived
// conformances.
static Type getVectorSpaceType(ValueDecl *decl,
                               AutoDiffAssociatedVectorSpaceKind kind) {
  auto &C = decl->getASTContext();
  auto *diffableProto = C.getProtocol(KnownProtocolKind::Differentiable);
  auto declType =
      decl->getDeclContext()->mapTypeIntoContext(decl->getInterfaceType());
  auto conf = TypeChecker::conformsToProtocol(declType, diffableProto,
                                              decl->getDeclContext(),
                                              ConformanceCheckFlags::Used);
  if (!conf)
    return Type();
  auto vectorSpaceId = getVectorSpaceIdentifier(kind, C);
  Type vectorSpaceType = ProtocolConformanceRef::getTypeWitnessByName(
      declType, *conf, vectorSpaceId, C.getLazyResolver());
  assert(vectorSpaceType &&
         "Differentiable protocol vector space type not found");
  return vectorSpaceType->mapTypeOutOfContext();
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
  auto parameterizedProto = C.getProtocol(KnownProtocolKind::Parameterized);
  auto retNominalIsParameterized = TypeChecker::conformsToProtocol(
      retNominal->getDeclaredInterfaceType(), parameterizedProto,
      retNominal->getDeclContext(), ConformanceCheckFlags::Used);

  auto createMemberMethodCallExpr = [&](VarDecl *member) -> Expr * {
    auto module = nominal->getModuleContext();
    auto confRef = module->lookupConformance(member->getType(), diffProto);
    // If the returned nominal is `Parameterized` and the member does not have
    // `@TFParameter`, create direct reference to member.
    if (retNominalIsParameterized &&
        !member->getAttrs().hasAttribute<TFParameterAttr>()) {
      return new (C) MemberRefExpr(selfDRE, SourceLoc(), member, DeclNameLoc(),
                                   /*Implicit*/ true);
    }
    assert(confRef && "Member does not conform to 'Differentiable'");

    // Get member type's method, e.g. `Member.moved(along:)`.
    // Use protocol requirement declaration for the method by default: this
    // will be dynamically dispatched.
    ValueDecl *memberMethodDecl = methodReq;
    // If conformance reference is concrete, then use concrete witness
    // declaration for the operator.
    if (confRef->isConcrete())
      memberMethodDecl =
          confRef->getConcrete()->getWitnessDecl(methodReq, nullptr);
    auto memberMethodDRE =
        new (C) DeclRefExpr(memberMethodDecl, DeclNameLoc(), /*Implicit*/ true);
    memberMethodDRE->setFunctionRefKind(FunctionRefKind::SingleApply);

    // Create reference to member method: `x.moved(along:)`.
    auto memberExpr =
        new (C) MemberRefExpr(selfDRE, SourceLoc(), member, DeclNameLoc(),
                              /*Implicit*/ true);
    auto memberMethodExpr =
        new (C) DotSyntaxCallExpr(memberMethodDRE, SourceLoc(), memberExpr);

    // Create reference to parameter member: `direction.x`.
    VarDecl *paramMember = nullptr;
    auto paramNominal = paramDecl->getType()->getAnyNominal();
    assert(paramNominal && "Parameter should have a nominal type");
    // Find parameter member corresponding to nominal member.
    // TODO: make more efficient using `lookupDirect` or a map of members.
    for (auto candidate : paramNominal->getStoredProperties()) {
      if (candidate->getName() == member->getName() &&
          candidate->getType()->isEqual(member->getType())) {
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

// Synthesize a vector space associated type (either 'TangentVector' or
// 'CotangentVector').
static Type
deriveDifferentiable_VectorSpace(DerivedConformance &derived,
                                 AutoDiffAssociatedVectorSpaceKind kind) {
  auto &TC = derived.TC;
  auto parentDC = derived.getConformanceContext();
  auto nominal = derived.Nominal;
  auto &C = nominal->getASTContext();

  // TODO: Check if nominal type conforms to `Parameterized` in addition to
  // `Differentiable`. If so, return the associated `Parameters` struct.

  // Check if all members have vector space associated types equal to `Self`.
  bool allMembersVectorSpaceEqualsSelf = llvm::all_of(
      getStoredPropertiesForDifferentiation(nominal), [&](VarDecl *member) {
        auto memberAssocType =
            nominal->mapTypeIntoContext(getVectorSpaceType(member, kind));
        return member->getType()->isEqual(memberAssocType);
      });
  // Check if nominal type conforms to `VectorNumeric`.
  // This is important because nominal type must conform to `VectorNumeric` in
  // order to be a valid vector space type.
  auto *vectorNumericProto = C.getProtocol(KnownProtocolKind::VectorNumeric);
  auto nominalConformsToVectorNumeric = TC.conformsToProtocol(
      nominal->getDeclaredInterfaceType(), vectorNumericProto, parentDC,
      ConformanceCheckFlags::Used);
  // Return `Self` if conditions are met.
  if (allMembersVectorSpaceEqualsSelf && nominalConformsToVectorNumeric) {
    return parentDC->mapTypeIntoContext(nominal->getDeclaredInterfaceType());
  }

  // Otherwise, synthesis is not currently supported.
  // assert(false && "Could not derive vector space associated type");
  return nullptr;
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
