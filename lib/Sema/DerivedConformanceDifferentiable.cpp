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

// TODO:
// - Support synthesis when non-synthesized `TangentVector` or `CotangentVector`
//   struct does not have implicit memberwise initializer. Currently,
//   user-defined memberwise initializers do not work.

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

// Represents the possible outcomes of checking whether the `TangentVector` or
// `CotangentVector` struct exists.
enum VectorSpaceStructStatus {
  Valid,
  Invalid,
  DoesNotExist
};

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
  lookup.erase(std::remove_if(lookup.begin(), lookup.end(),
                              [](ValueDecl *v) {
                                return !isa<ProtocolDecl>(
                                           v->getDeclContext()) ||
                                       !v->isProtocolRequirement();
                              }),
               lookup.end());
  assert(lookup.size() == 1 && "Ambiguous protocol requirement");
  return lookup[0];
}

bool DerivedConformance::canDeriveDifferentiable(NominalTypeDecl *nominal) {
  // Nominal type must be a struct with at least one stored property.
  auto *structDecl = dyn_cast<StructDecl>(nominal);
  if (!structDecl || structDecl->getStoredProperties().empty())
    return false;
  auto &C = nominal->getASTContext();
  auto *lazyResolver = C.getLazyResolver();
  auto *diffableProto = C.getProtocol(KnownProtocolKind::Differentiable);

  // Nominal type must conform to `VectorNumeric`.
  // TODO: Lift this restriction.
  auto *vectorNumericProto = C.getProtocol(KnownProtocolKind::VectorNumeric);
  if (!TypeChecker::conformsToProtocol(
          nominal->getDeclaredInterfaceType(), vectorNumericProto,
          nominal->getDeclContext(), ConformanceCheckFlags::Used))
    return false;

  // All stored properties must conform to `Differentiable`.
  // Currently, all stored properties must also have
  // `Self == TangentVector == CotangentVector`.
  // TODO: Lift this restriction.
  return llvm::all_of(structDecl->getStoredProperties(), [&](VarDecl *v) {
    if (!v->hasType())
      lazyResolver->resolveDeclSignature(v);
    if (!v->hasType())
      return false;
    auto conf = TypeChecker::conformsToProtocol(v->getType(), diffableProto,
                                                v->getDeclContext(),
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

// Return true if `vectorSpaceDecl` is a valid vector space struct for the given
// nominal type.
static bool isValidVectorSpaceStruct(NominalTypeDecl *nominal,
                                     StructDecl *vectorSpaceDecl) {
  // Add all stored properties of the vector space struct to a map.
  // Also, check that vector space struct has a memberwise initializer.
  llvm::DenseMap<Identifier, VarDecl *> members;
  ConstructorDecl *memberwiseInitDecl = nullptr;
  for (auto member : vectorSpaceDecl->getMembers()) {
    // Find memberwise initializer.
    if (!memberwiseInitDecl) {
      auto initDecl = dyn_cast<ConstructorDecl>(member);
      if (initDecl && initDecl->isMemberwiseInitializer())
        memberwiseInitDecl = initDecl;
    }
    // Add `members` struct stored properties to map.
    auto varDecl = dyn_cast<VarDecl>(member);
    if (!varDecl || varDecl->isStatic() || !varDecl->hasStorage())
      continue;
    members[varDecl->getName()] = varDecl;
  }
  if (!memberwiseInitDecl)
    return false;

  // Check that each member of the nominal type maps to a stored property in
  // the vector space struct.
  for (auto member : nominal->getStoredProperties()) {
    auto it = members.find(member->getName());
    if (it == members.end() ||
        !it->second->getType()->isEqual(member->getType())) {
      return false;
    }
  }
  return true;
}

// Attempt to find a vector space associated type for the given nominal type.
static std::pair<StructDecl *, VectorSpaceStructStatus>
getVectorSpaceStructDecl(NominalTypeDecl *nominal,
                         AutoDiffAssociatedVectorSpaceKind kind) {
  auto &ctx = nominal->getASTContext();
  auto vectorSpaceId = getVectorSpaceIdentifier(kind, ctx);

  VectorSpaceStructStatus status = DoesNotExist;
  StructDecl *vectorSpaceStructDecl = nullptr;

  for (auto member : nominal->getMembers()) {
    // If member is a typealias declaration with matching name and underlying
    // struct type, use the struct type.
    if (auto aliasDecl = dyn_cast<TypeAliasDecl>(member)) {
      if (aliasDecl->getName() != vectorSpaceId)
        continue;
      auto underlyingType = aliasDecl->getUnderlyingTypeLoc().getType();
      vectorSpaceStructDecl =
          dyn_cast<StructDecl>(underlyingType->getAnyNominal());
    }
    // If member is a struct declaration with matching name, use it.
    else if (auto structDecl = dyn_cast<StructDecl>(member)) {
      if (structDecl->getName() != vectorSpaceId)
        continue;
      vectorSpaceStructDecl = structDecl;
    }
    if (!vectorSpaceStructDecl)
      continue;
    if (isValidVectorSpaceStruct(nominal, vectorSpaceStructDecl))
      return std::make_pair(vectorSpaceStructDecl, Valid);
    else
      status = Invalid;
  }
  return std::make_pair(vectorSpaceStructDecl, status);
}

// Get memberwise initializer for a nominal type.
static ConstructorDecl *getMemberwiseInitializer(NominalTypeDecl *nominal) {
  ConstructorDecl *memberwiseInitDecl = nullptr;
  for (auto member : nominal->getMembers()) {
    // Find memberwise initializer.
    if (!memberwiseInitDecl) {
      auto initDecl = dyn_cast<ConstructorDecl>(member);
      if (!initDecl || !initDecl->isMemberwiseInitializer())
        continue;
      assert(!memberwiseInitDecl && "Memberwise initializer already found");
      memberwiseInitDecl = initDecl;
    }
  }
  return memberwiseInitDecl;
}

// Synthesize body for a `Differentiable` method requirement.
static void deriveBodyDifferentiable_method(AbstractFunctionDecl *funcDecl,
                                            Identifier methodName,
                                            Identifier methodParamLabel) {
  // NominalTypeDecl *returnedNominal) {
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

  // Create call expression applying a member method to a parameter member.
  // Format: `<member>.method(<parameter>.<member>)`.
  // Example: `x.moved(along: direction.x)`.
  auto createMemberMethodCallExpr = [&](VarDecl *member) -> Expr * {
    auto module = nominal->getModuleContext();
    auto confRef = module->lookupConformance(member->getType(), diffProto);
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
  for (auto member : nominal->getStoredProperties()) {
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

// Synthesize the `moved(along:)` function declaration.
static ValueDecl *deriveDifferentiable_moved(DerivedConformance &derived) {
  auto nominal = derived.Nominal;
  auto &TC = derived.TC;
  auto &C = derived.TC.Context;
  auto parentDC = derived.getConformanceContext();
  auto selfInterfaceType = parentDC->getDeclaredInterfaceType();

  StructDecl *tangentDecl;
  VectorSpaceStructStatus tangentStatus;
  std::tie(tangentDecl, tangentStatus) = getVectorSpaceStructDecl(
      nominal, AutoDiffAssociatedVectorSpaceKind::Tangent);
  switch (tangentStatus) {
  case DoesNotExist:
    TC.diagnose(derived.ConformanceDecl->getLoc(),
                diag::differentiable_no_vector_space_struct,
                derived.getProtocolType(), C.Id_TangentVector);
    return nullptr;
  case Invalid:
    TC.diagnose(tangentDecl, diag::differentiable_invalid_vector_space_struct,
                derived.getProtocolType(), C.Id_TangentVector);
    return nullptr;
  case Valid:
    break;
  }
  auto tangentType = tangentDecl->getDeclaredInterfaceType();

  auto *param =
      new (C) ParamDecl(VarDecl::Specifier::Default, SourceLoc(), SourceLoc(),
                        C.getIdentifier("along"), SourceLoc(),
                        C.getIdentifier("direction"), parentDC);
  param->setInterfaceType(tangentType);
  ParameterList *params = ParameterList::create(C, {param});

  DeclName declName(C, C.Id_moved, params);
  auto funcDecl =
      FuncDecl::create(C, SourceLoc(), StaticSpellingKind::None, SourceLoc(),
                       declName, SourceLoc(),
                       /*Throws*/ false, SourceLoc(),
                       /*GenericParams=*/nullptr, params,
                       TypeLoc::withoutLoc(selfInterfaceType), parentDC);
  funcDecl->setImplicit();
  funcDecl->setBodySynthesizer(deriveBodyDifferentiable_moved);

  if (auto env = parentDC->getGenericEnvironmentOfContext())
    funcDecl->setGenericEnvironment(env);
  funcDecl->computeType();
  funcDecl->copyFormalAccessFrom(nominal, /*sourceIsParentContext*/ true);
  funcDecl->setValidationToChecked();

  derived.addMembersToConformanceContext({funcDecl});
  C.addSynthesizedDecl(funcDecl);

  return funcDecl;
}

// Synthesize the `tangentVector(from:)` function declaration.
static ValueDecl *
deriveDifferentiable_tangentVector(DerivedConformance &derived) {
  auto nominal = derived.Nominal;
  auto &TC = derived.TC;
  auto &C = derived.TC.Context;
  auto parentDC = derived.getConformanceContext();

  StructDecl *tangentDecl;
  VectorSpaceStructStatus tangentStatus;
  std::tie(tangentDecl, tangentStatus) = getVectorSpaceStructDecl(
      nominal, AutoDiffAssociatedVectorSpaceKind::Tangent);
  switch (tangentStatus) {
  case DoesNotExist:
    TC.diagnose(derived.ConformanceDecl->getLoc(),
                diag::differentiable_no_vector_space_struct,
                derived.getProtocolType(), C.Id_TangentVector);
    return nullptr;
  case Invalid:
    TC.diagnose(tangentDecl, diag::differentiable_invalid_vector_space_struct,
                derived.getProtocolType(), C.Id_TangentVector);
    return nullptr;
  case Valid:
    break;
  }
  auto tangentType = tangentDecl->getDeclaredInterfaceType();

  StructDecl *cotangentDecl;
  VectorSpaceStructStatus cotangentStatus;
  std::tie(cotangentDecl, cotangentStatus) = getVectorSpaceStructDecl(
      nominal, AutoDiffAssociatedVectorSpaceKind::Cotangent);
  switch (cotangentStatus) {
  case DoesNotExist:
    TC.diagnose(derived.ConformanceDecl->getLoc(),
                diag::differentiable_no_vector_space_struct,
                derived.getProtocolType(), C.Id_CotangentVector);
    return nullptr;
  case Invalid:
    TC.diagnose(cotangentDecl, diag::differentiable_invalid_vector_space_struct,
                derived.getProtocolType(), C.Id_CotangentVector);
    return nullptr;
  case Valid:
    break;
  }
  auto cotangentType = cotangentDecl->getDeclaredInterfaceType();

  auto *param =
      new (C) ParamDecl(VarDecl::Specifier::Default, SourceLoc(), SourceLoc(),
                        C.getIdentifier("from"), SourceLoc(),
                        C.getIdentifier("cotangent"), parentDC);
  param->setInterfaceType(cotangentType);
  ParameterList *params = ParameterList::create(C, {param});

  DeclName declName(C, C.Id_tangentVector, params);
  auto funcDecl = FuncDecl::create(C, SourceLoc(), StaticSpellingKind::None,
                                   SourceLoc(), declName, SourceLoc(),
                                   /*Throws*/ false, SourceLoc(),
                                   /*GenericParams=*/nullptr, params,
                                   TypeLoc::withoutLoc(tangentType), parentDC);
  funcDecl->setImplicit();
  funcDecl->setBodySynthesizer(deriveBodyDifferentiable_tangentVector);

  if (auto env = parentDC->getGenericEnvironmentOfContext())
    funcDecl->setGenericEnvironment(env);
  funcDecl->computeType();
  funcDecl->copyFormalAccessFrom(nominal, /*sourceIsParentContext*/ true);
  funcDecl->setValidationToChecked();

  derived.addMembersToConformanceContext({funcDecl});
  C.addSynthesizedDecl(funcDecl);

  return funcDecl;
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
  bool allMembersVectorSpaceEqualsSelf =
      llvm::all_of(nominal->getStoredProperties(), [&](VarDecl *member) {
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
  if (allMembersVectorSpaceEqualsSelf && nominalConformsToVectorNumeric)
    return nominal->getDeclaredInterfaceType();

  // Otherwise, synthesis is not currently supported.
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
    auto rawType = deriveDifferentiable_VectorSpace(
        *this, AutoDiffAssociatedVectorSpaceKind::Tangent);
    return getConformanceContext()->mapTypeIntoContext(rawType);
  }
  if (requirement->getBaseName() == TC.Context.Id_CotangentVector) {
    auto rawType = deriveDifferentiable_VectorSpace(
        *this, AutoDiffAssociatedVectorSpaceKind::Cotangent);
    return getConformanceContext()->mapTypeIntoContext(rawType);
  }
  TC.diagnose(requirement->getLoc(), diag::broken_differentiable_requirement);
  return nullptr;
}
