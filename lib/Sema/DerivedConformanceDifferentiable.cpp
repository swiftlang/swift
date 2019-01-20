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

  // Nominal type must not customize `TangentVector`, `CotangentVector`, or
  // `AllDifferentiableVariables` to anything other than `Self`.
  // Otherwise, synthesis is semantically unsupported.
  auto tangentDecls = nominal->lookupDirect(C.Id_TangentVector);
  auto cotangentDecls = nominal->lookupDirect(C.Id_CotangentVector);
  auto allDiffableVarsDecls =
      nominal->lookupDirect(C.Id_AllDifferentiableVariables);
  auto isValidAssociatedTypeCandidate = [&](ValueDecl *v) {
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
      llvm::partition(tangentDecls, isValidAssociatedTypeCandidate);
  auto invalidCotangentDecls =
      llvm::partition(cotangentDecls, isValidAssociatedTypeCandidate);
  auto invalidAllDiffableVarsDecls =
      llvm::partition(allDiffableVarsDecls, isValidAssociatedTypeCandidate);

  auto validTangentDeclCount =
      std::distance(tangentDecls.begin(), invalidTangentDecls);
  auto invalidTangentDeclCount =
      std::distance(invalidTangentDecls, tangentDecls.end());
  auto validCotangentDeclCount =
      std::distance(cotangentDecls.begin(), invalidCotangentDecls);
  auto invalidCotangentDeclCount =
      std::distance(invalidCotangentDecls, cotangentDecls.end());
  auto validAllDiffableVarsDeclCount =
      std::distance(allDiffableVarsDecls.begin(), invalidAllDiffableVarsDecls);
  auto invalidAllDiffableVarsDeclCount =
      std::distance(invalidAllDiffableVarsDecls, allDiffableVarsDecls.end());

  // There cannot be any invalid associated types. There can be at most one
  // valid associated type.
  if (invalidTangentDeclCount != 0 ||
      invalidCotangentDeclCount != 0 ||
      invalidAllDiffableVarsDeclCount != 0 ||
      validTangentDeclCount > 1 ||
      validCotangentDeclCount > 1 ||
      validAllDiffableVarsDeclCount > 1)
    return false;

  // All stored properties not marked with `@noDerivative`:
  // - Must conform to `Differentiable`.
  // - Must not have any `let` stored properties with an initial value.
  //   - This restriction may be lifted later with support for "true" memberwise
  //     initializers that initialize all stored properties, including initial
  //     value information.
  SmallVector<VarDecl *, 16> diffProperties;
  getStoredPropertiesForDifferentiation(structDecl, diffProperties);
  return llvm::all_of(diffProperties, [&](VarDecl *v) {
    if (v->isLet() && v->hasInitialValue())
      return false;
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

// Get the specified associated type for the given declaration.
// TODO: Generalize and move function to shared place for use with other derived
// conformances.
static Type getAssociatedType(VarDecl *decl, DeclContext *DC, Identifier id) {
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
  Type associatedType = ProtocolConformanceRef::getTypeWitnessByName(
      declType, *conf, id, C.getLazyResolver());
  assert(associatedType && "Differentiable protocol associated type not found");
  return associatedType;
}

// Attempt to find an associated type for the given nominal type.
static StructDecl *
getAssociatedStructDecl(NominalTypeDecl *nominal, Identifier id) {
  auto module = nominal->getModuleContext();
  // Differentiable protocol.
  auto &C = nominal->getASTContext();
  auto *diffableProtocol = C.getProtocol(KnownProtocolKind::Differentiable);
  assert(diffableProtocol && "Differentiable protocol not found");
  auto associatedTypeLookup = diffableProtocol->lookupDirect(id);
  assert(associatedTypeLookup.size() == 1);
  auto *dependentType = DependentMemberType::get(
      diffableProtocol->getDeclaredInterfaceType(),
      cast<AssociatedTypeDecl>(associatedTypeLookup.front()));
  if (auto assocTy =
          dependentType->substBaseType(nominal->getDeclaredInterfaceType(),
                                       LookUpConformanceInModule(module)))
    return assocTy->getStructOrBoundGenericStruct();
  return nullptr;
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
  auto *memberwiseInitDecl = retNominal->getMemberwiseInitializer();
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
  if (!returnNominal->getMemberwiseInitializer()) {
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

  auto *tangentDecl = getAssociatedStructDecl(nominal, C.Id_TangentVector);
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

  auto *tangentDecl = getAssociatedStructDecl(nominal, C.Id_TangentVector);
  assert(tangentDecl && "'TangentVector' struct must exist");
  auto tangentType = tangentDecl->getDeclaredInterfaceType();

  auto *cotangentDecl = getAssociatedStructDecl(nominal, C.Id_CotangentVector);
  assert(cotangentDecl && "'CotangentVector' struct must exist");
  auto cotangentType = cotangentDecl->getDeclaredInterfaceType();

  return deriveDifferentiable_method(
      derived, C.Id_tangentVector, C.getIdentifier("from"),
      C.getIdentifier("cotangent"), cotangentType, tangentType,
      deriveBodyDifferentiable_tangentVector);
}

// Returns the underlying `allDifferentiableVariables` of a VarDecl `x`.
// If `x` conforms to `Differentiable`, return `allDifferentiableVariables`.
// Otherwise, return `x`.
static ValueDecl *getUnderlyingAllDiffableVariables(ModuleDecl *module,
                                                    VarDecl *varDecl) {
  auto &C = module->getASTContext();
  auto *diffableProto = C.getProtocol(KnownProtocolKind::Differentiable);
  auto allDiffableVarsReq =
      getProtocolRequirement(diffableProto, C.Id_allDifferentiableVariables);
  auto confRef =
      module->lookupConformance(varDecl->getType(), diffableProto);
  if (!confRef)
    return varDecl;
  auto conf = confRef->getConcrete();
  auto allDiffableVarsDecl =
     conf->getWitnessDecl(allDiffableVarsReq, /*lazyResolver*/ nullptr);
  return allDiffableVarsDecl;
}

// Synthesize getter body for `allDifferentiableVariables` computed property.
static void
derivedBody_allDifferentiableVariablesGetter(AbstractFunctionDecl *getterDecl) {
  auto *nominal = getterDecl->getDeclContext()->getSelfNominalTypeDecl();
  auto &C = nominal->getASTContext();
  auto *module = nominal->getModuleContext();

  auto *allDiffableVarsStruct =
      getAssociatedStructDecl(nominal, C.Id_AllDifferentiableVariables);
  assert(allDiffableVarsStruct &&
         "'AllDifferentiableVariables' struct not found but should exist");
  auto *allDiffableVarsInitDecl =
      allDiffableVarsStruct->getMemberwiseInitializer();
  assert(allDiffableVarsInitDecl &&
         "'AllDifferentiableVariables' memberwise initializer not found");

  auto *selfDecl = getterDecl->getImplicitSelfDecl();
  auto *selfDRE =
      new (C) DeclRefExpr(selfDecl, DeclNameLoc(), /*Implicit*/ true);

  auto *initDRE = new (C) DeclRefExpr(allDiffableVarsInitDecl, DeclNameLoc(),
                                      /*Implicit*/ true);
  initDRE->setFunctionRefKind(FunctionRefKind::SingleApply);

  auto allDiffableVarsType = nominal->mapTypeIntoContext(
      allDiffableVarsStruct->getDeclaredInterfaceType());
  Expr *baseExpr = TypeExpr::createImplicit(allDiffableVarsType, C);
  auto *initExpr = new (C) ConstructorRefCallExpr(initDRE, baseExpr);
  initExpr->setThrows(false);
  initExpr->setImplicit();

  SmallVector<Expr *, 2> members;
  SmallVector<Identifier, 2> memberNames;

  llvm::DenseMap<Identifier, VarDecl *> diffPropertyMap;
  SmallVector<VarDecl *, 8> diffProperties;
  getStoredPropertiesForDifferentiation(nominal, diffProperties);
  for (auto *member : diffProperties)
    diffPropertyMap[member->getName()] = member;

  for (auto initParam : *allDiffableVarsInitDecl->getParameters()) {
    auto member = diffPropertyMap[initParam->getName()];
    Expr *memberExpr = new (C) MemberRefExpr(selfDRE, SourceLoc(), member,
                                             DeclNameLoc(), /*Implicit*/ true);
    memberExpr->setType(member->getType());
    auto memberAllDiffableVarsDecl =
        getUnderlyingAllDiffableVariables(module, member);
    if (member != memberAllDiffableVarsDecl) {
      memberExpr = new (C) MemberRefExpr(memberExpr, SourceLoc(),
                                         memberAllDiffableVarsDecl,
                                         DeclNameLoc(), /*Implicit*/ true);
    }
    members.push_back(memberExpr);
    memberNames.push_back(member->getName());
  }
  Expr *callExpr = CallExpr::createImplicit(C, initExpr, members, memberNames);

  ASTNode returnStmt = new (C) ReturnStmt(SourceLoc(), callExpr, true);
  getterDecl->setBody(
      BraceStmt::create(C, SourceLoc(), returnStmt, SourceLoc(), true));
}

// Synthesize setter body for `allDifferentiableVariables` computed property.
static void
derivedBody_allDifferentiableVariablesSetter(AbstractFunctionDecl *setterDecl) {
  auto *nominal = setterDecl->getDeclContext()->getSelfNominalTypeDecl();
  auto &C = nominal->getASTContext();
  auto *module = nominal->getModuleContext();

  auto *selfDecl = setterDecl->getImplicitSelfDecl();
  Expr *selfDRE =
      new (C) DeclRefExpr(selfDecl, DeclNameLoc(), /*Implicit*/ true);

  auto *allDiffableVarsStruct =
      getAssociatedStructDecl(nominal, C.Id_AllDifferentiableVariables);
  assert(allDiffableVarsStruct &&
         "'AllDifferentiableVariables' struct not found but should exist");
  auto *newValueDecl = setterDecl->getParameters()->get(0);
  Expr *newValueDRE =
      new (C) DeclRefExpr(newValueDecl, DeclNameLoc(), /*Implicit*/ true);

  // Map `AllDifferentiableVariables` struct members to their names for
  // efficient lookup.
  llvm::DenseMap<Identifier, VarDecl *> diffPropertyMap;
  for (auto member : allDiffableVarsStruct->getStoredProperties())
    diffPropertyMap[member->getName()] = member;

  SmallVector<ASTNode, 2> assignExprs;
  SmallVector<VarDecl *, 8> diffProperties;
  getStoredPropertiesForDifferentiation(nominal, diffProperties);
  for (auto *member : diffProperties) {
    // Skip immutable members.
    if (member->isLet())
      continue;
    // Create lhs: either `self.x` or `self.x.allDifferentiableVariables`.
    auto lhsAllDiffableVars = getUnderlyingAllDiffableVariables(module, member);
    Expr *lhs;
    if (member == lhsAllDiffableVars) {
      lhs = new (C) MemberRefExpr(selfDRE, SourceLoc(), member, DeclNameLoc(),
                                  /*Implicit*/ true);
    } else {
      auto *paramDRE = new (C) MemberRefExpr(selfDRE, SourceLoc(), member,
                                             DeclNameLoc(), /*Implicit*/ true);
      lhs = new (C) MemberRefExpr(paramDRE, SourceLoc(), lhsAllDiffableVars,
                                  DeclNameLoc(), /*Implicit*/ true);
    }
    // Create rhs: `newValue.x`.
    auto *rhs = new (C) MemberRefExpr(newValueDRE, SourceLoc(),
                                      diffPropertyMap[member->getName()],
                                      DeclNameLoc(), /*Implicit*/ true);
    // Create assign expression.
    auto *assignExpr = new (C) AssignExpr(lhs, SourceLoc(), rhs,
                                          /*Implicit*/ true);
    assignExprs.push_back(assignExpr);
  }

  setterDecl->setBody(
      BraceStmt::create(C, SourceLoc(), assignExprs, SourceLoc(), true));
}

// Synthesize `allDifferentiableVariables` computed property declaration.
static ValueDecl *
deriveDifferentiable_allDifferentiableVariables(DerivedConformance &derived) {
  auto nominal = derived.Nominal;
  auto &TC = derived.TC;
  auto &C = TC.Context;

  // Get `AllDifferentiableVariables` struct.
  auto allDiffableVarsStruct =
      getAssociatedStructDecl(nominal, C.Id_AllDifferentiableVariables);

  auto returnInterfaceTy = allDiffableVarsStruct->getDeclaredInterfaceType();
  auto returnTy = nominal->mapTypeIntoContext(returnInterfaceTy);

  VarDecl *allDiffableVarsDecl;
  PatternBindingDecl *pbDecl;
  std::tie(allDiffableVarsDecl, pbDecl) = derived.declareDerivedProperty(
      C.Id_allDifferentiableVariables, returnInterfaceTy, returnTy,
      /*isStatic*/ false, /*isFinal*/ true);

  auto *getterDecl = derived.declareDerivedPropertyGetter(
      derived.TC, allDiffableVarsDecl, returnTy);
  getterDecl->setBodySynthesizer(&derivedBody_allDifferentiableVariablesGetter);

  auto *setterDecl = derived.declareDerivedPropertySetter(
      derived.TC, allDiffableVarsDecl, returnTy);
  setterDecl->setBodySynthesizer(&derivedBody_allDifferentiableVariablesSetter);

  allDiffableVarsDecl->setAccessors(StorageImplInfo::getMutableComputed(),
                                    SourceLoc(), {getterDecl, setterDecl},
                                    SourceLoc());

  derived.addMembersToConformanceContext(
      {getterDecl, setterDecl, allDiffableVarsDecl, pbDecl});

  addExpectedOpaqueAccessorsToStorage(TC, allDiffableVarsDecl);
  triggerAccessorSynthesis(TC, allDiffableVarsDecl);

  return allDiffableVarsDecl;
}

// Return associated `TangentVector`, `CotangentVector`, or
// `AllDifferentiableVariables` struct for a nominal type, if it exists.
// If not, synthesize the struct.
static StructDecl *
getOrSynthesizeSingleAssociatedStruct(DerivedConformance &derived,
                                      Identifier id) {
  auto &TC = derived.TC;
  auto parentDC = derived.getConformanceContext();
  auto nominal = derived.Nominal;
  auto &C = nominal->getASTContext();

  assert(id == C.Id_TangentVector || id == C.Id_CotangentVector ||
         id == C.Id_AllDifferentiableVariables);

  // If the associated struct already exists, return it.
  auto lookup = nominal->lookupDirect(id);
  assert(lookup.size() < 2);
  if (lookup.size() == 1) {
    auto structDecl = dyn_cast<StructDecl>(lookup.front());
    assert(structDecl && "Expected lookup result to be a struct");
    return structDecl;
  }

  // Otherwise, synthesize a new struct. The struct must conform to
  // `Differentiable`.
  auto *diffableProto = C.getProtocol(KnownProtocolKind::Differentiable);
  auto diffableType = TypeLoc::withoutLoc(diffableProto->getDeclaredType());
  auto *addArithProto = C.getProtocol(KnownProtocolKind::AdditiveArithmetic);
  auto addArithType = TypeLoc::withoutLoc(addArithProto->getDeclaredType());
  auto *vecNumProto = C.getProtocol(KnownProtocolKind::VectorNumeric);
  auto vecNumType = TypeLoc::withoutLoc(vecNumProto->getDeclaredType());
  auto *kpIterableProto = C.getProtocol(KnownProtocolKind::KeyPathIterable);
  auto kpIterableType = TypeLoc::withoutLoc(kpIterableProto->getDeclaredType());
  SmallVector<TypeLoc, 3> inherited {diffableType};
  // If the associated type is `TangentVector` or `CotangentVector`, make it
  // also conform to `AdditiveArithmetic`.
  if (id == C.Id_TangentVector || id == C.Id_CotangentVector)
    inherited.push_back(addArithType);

  // Cache original members and their associated types for later use.
  SmallVector<VarDecl *, 8> diffProperties;
  getStoredPropertiesForDifferentiation(nominal, diffProperties);

  auto canDerive = [&](ProtocolDecl *proto) {
      return !diffProperties.empty() && llvm::all_of(
          diffProperties, [&](VarDecl *var) {
            return TC.conformsToProtocol(getAssociatedType(var, nominal, id),
                                         proto, nominal,
                                         ConformanceCheckFlags::Used);
          });
  };

  // If the associated struct is `AllDifferentiableVariables`, conform it to:
  // - `AdditiveArithmetic`, if all members of the parent conform to
  //   `AdditiveArithmetic`.
  // - `KeyPathIterable`, if the parent conforms to to `KeyPathIterable`.
  if (id == C.Id_AllDifferentiableVariables) {
    if (canDerive(addArithProto))
      inherited.push_back(addArithType);
    if (TC.conformsToProtocol(nominal->getDeclaredInterfaceType(),
                              kpIterableProto, parentDC,
                              ConformanceCheckFlags::Used))
      inherited.push_back(kpIterableType);
  }
  // If all members also conform to `VectorNumeric` with the same `Scalar` type,
  // make the associated struct conform to `VectorNumeric` instead of just
  // `AdditiveArithmetic`.
  if (canDerive(vecNumProto))
    inherited.push_back(vecNumType);

  auto *structDecl = new (C) StructDecl(SourceLoc(), id, SourceLoc(),
                                        /*Inherited*/ C.AllocateCopy(inherited),
                                        /*GenericParams*/ {}, parentDC);
  structDecl->getAttrs().add(
      new (C) FieldwiseDifferentiableAttr(/*implicit*/ true));
  structDecl->setImplicit();
  structDecl->copyFormalAccessFrom(nominal, /*sourceIsParentContext*/ true);

  // Add members to associated struct.
  for (auto *member : diffProperties) {
    // Add this member's corresponding associated type to the parent's
    // associated struct.
    auto newMember = new (C) VarDecl(
        member->isStatic(), member->getSpecifier(), member->isCaptureList(),
        /*NameLoc*/ SourceLoc(), member->getName(), structDecl);
    // NOTE: `newMember` is not marked as implicit here, because that affects
    // memberwise initializer synthesis.

    auto memberAssocType = getAssociatedType(member, nominal, id);
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

    // Now that this member is in the associated type, it should be marked
    // `@differentiable` so that the differentiation transform will synthesize
    // associated functions for it. We only add this to public stored
    // properties, because their access outside the module will go through a
    // call to the getter.
    if (member->getEffectiveAccess() > AccessLevel::Internal &&
        !member->getAttrs().hasAttribute<DifferentiableAttr>()) {
      auto *diffableAttr = DifferentiableAttr::create(
          C, /*implicit*/ true, SourceLoc(), SourceLoc(), {}, None,
          None, nullptr);
      member->getAttrs().add(diffableAttr);
      auto *getterType =
          member->getGetter()->getInterfaceType()->castTo<AnyFunctionType>();
      AutoDiffParameterIndicesBuilder builder(getterType);
      builder.setParameter(0);
      diffableAttr->setParameterIndices(builder.build(C));
    }
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

// Add a typealias declaration with the given name and underlying target
// struct type to the source nominal type.
static void addAssociatedTypeAliasDecl(Identifier name,
                                       NominalTypeDecl *source,
                                       StructDecl *target,
                                       TypeChecker &TC) {
  auto &C = TC.Context;
  auto lookup = source->lookupDirect(name);
  assert(lookup.size() < 2 &&
         "Expected at most one associated type named member");
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
  if (auto env = source->getGenericEnvironmentOfContext())
    aliasDecl->setGenericEnvironment(env);
  source->addMember(aliasDecl);
  aliasDecl->copyFormalAccessFrom(source, /*sourceIsParentContext*/ true);
  aliasDecl->setValidationToChecked();
  TC.validateDecl(aliasDecl);
  C.addSynthesizedDecl(aliasDecl);
};

// Get or synthesize all associated struct types: 'TangentVector',
// 'CotangentVector', and 'AllDifferentiableVariables'.
// Return the type corresponding to the given identifier.
static Type
getOrSynthesizeAssociatedStructType(DerivedConformance &derived,
                                    Identifier id) {
  auto &TC = derived.TC;
  auto parentDC = derived.getConformanceContext();
  auto nominal = derived.Nominal;
  auto &C = nominal->getASTContext();

  // Otherwise, get or synthesize `TangentVector`, `CotangentVector`, and
  // `AllDifferentiableVariables` structs at once. Synthesizing all three
  // structs at once is necessary in order to correctly set their mutually
  // recursive associated types.
  auto tangentStruct =
      getOrSynthesizeSingleAssociatedStruct(derived, C.Id_TangentVector);
  auto cotangentStruct =
      getOrSynthesizeSingleAssociatedStruct(derived, C.Id_CotangentVector);
  auto allDiffableVarsStruct = getOrSynthesizeSingleAssociatedStruct(derived,
      C.Id_AllDifferentiableVariables);

  // Add associated typealiases for structs.
  addAssociatedTypeAliasDecl(C.Id_TangentVector,
                             tangentStruct, tangentStruct, TC);
  addAssociatedTypeAliasDecl(C.Id_TangentVector,
                             cotangentStruct, cotangentStruct, TC);
  addAssociatedTypeAliasDecl(C.Id_TangentVector,
                             allDiffableVarsStruct, tangentStruct, TC);

  addAssociatedTypeAliasDecl(C.Id_CotangentVector,
                             tangentStruct, cotangentStruct, TC);
  addAssociatedTypeAliasDecl(C.Id_CotangentVector,
                             cotangentStruct, tangentStruct, TC);
  addAssociatedTypeAliasDecl(C.Id_CotangentVector,
                             allDiffableVarsStruct, cotangentStruct, TC);

  addAssociatedTypeAliasDecl(C.Id_AllDifferentiableVariables,
                             allDiffableVarsStruct, allDiffableVarsStruct, TC);
  addAssociatedTypeAliasDecl(C.Id_AllDifferentiableVariables,
                             tangentStruct, tangentStruct, TC);
  addAssociatedTypeAliasDecl(C.Id_AllDifferentiableVariables,
                             cotangentStruct, cotangentStruct, TC);

  TC.validateDecl(tangentStruct);
  TC.validateDecl(cotangentStruct);
  TC.validateDecl(allDiffableVarsStruct);

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
  assert(DerivedConformance::canDeriveDifferentiable(
      tangentStruct, nominal) && "Should be able to derive 'Differentiable'");
  assert(DerivedConformance::canDeriveDifferentiable(
      cotangentStruct, nominal) && "Should be able to derive 'Differentiable'");
  assert(DerivedConformance::canDeriveDifferentiable(
      allDiffableVarsStruct, nominal) &&
          "Should be able to derive 'Differentiable'");

  // Return the requested associated struct type.
  StructDecl *requestedStructDecl = nullptr;
  if (id == C.Id_TangentVector)
    requestedStructDecl = tangentStruct;
  else if (id == C.Id_CotangentVector)
    requestedStructDecl = cotangentStruct;
  else if (id == C.Id_AllDifferentiableVariables)
    requestedStructDecl = allDiffableVarsStruct;
  else
    llvm_unreachable("Unknown 'Differentiable' associated type identifier");
  return parentDC->mapTypeIntoContext(
      requestedStructDecl->getDeclaredInterfaceType());
}

// Synthesize an associated struct type ('TangentVector', 'CotangentVector', or
// 'AllDifferentiableVariables').
static Type
deriveDifferentiable_AssociatedStruct(DerivedConformance &derived,
                                      Identifier id) {
  auto &TC = derived.TC;
  auto parentDC = derived.getConformanceContext();
  auto nominal = derived.Nominal;
  auto &C = nominal->getASTContext();

  // Since associated types will be derived, we make this struct a fieldwise
  // differentiable type.
  nominal->getAttrs().add(
      new (C) FieldwiseDifferentiableAttr(/*implicit*/ true));

  // Get all stored properties for differentation.
  SmallVector<VarDecl *, 16> diffProperties;
  getStoredPropertiesForDifferentiation(nominal, diffProperties);

  // Check whether at least one `@noDerivative` stored property exists.
  unsigned numStoredProperties =
      std::distance(nominal->getStoredProperties().begin(),
                    nominal->getStoredProperties().end());
  bool hasNoDerivativeStoredProp = diffProperties.size() != numStoredProperties;

  // Check conditions for returning `Self`.
  // - No `@noDerivative` stored properties exist.
  // - All stored properties must have specified associated type equal to
  //   `Self`.
  // - If associated type is `TangentVector` or `CotangentVector`, parent type
  //   must also conform to `AdditiveArithmetic`.
  bool allMembersAssocTypeEqualsSelf =
      llvm::all_of(diffProperties, [&](VarDecl *member) {
        auto memberAssocType = getAssociatedType(member, nominal, id);
        return member->getType()->isEqual(memberAssocType);
      });

  auto *addArithProto = C.getProtocol(KnownProtocolKind::AdditiveArithmetic);
  auto nominalConformsToAddArith =
      TC.conformsToProtocol(nominal->getDeclaredInterfaceType(), addArithProto,
                            parentDC, ConformanceCheckFlags::Used);

  // Return `Self` if conditions are met.
  if (!hasNoDerivativeStoredProp &&
      (id == C.Id_AllDifferentiableVariables ||
       (allMembersAssocTypeEqualsSelf && nominalConformsToAddArith))) {
    auto selfType =
        parentDC->mapTypeIntoContext(nominal->getDeclaredInterfaceType());
    auto *aliasDecl = new (C) TypeAliasDecl(SourceLoc(), SourceLoc(), id,
                                            SourceLoc(), {}, nominal);
    aliasDecl->setUnderlyingType(selfType);
    aliasDecl->setImplicit();
    nominal->addMember(aliasDecl);
    aliasDecl->copyFormalAccessFrom(nominal, /*sourceIsParentContext*/ true);
    aliasDecl->setValidationToChecked();
    TC.validateDecl(aliasDecl);
    C.addSynthesizedDecl(aliasDecl);
    return selfType;
  }

  // Otherwise, check if all stored properties have all `Differentiable`
  // protocol associated types equal to `Self`:
  // `Self == TangentVector == CotangentVector == AllDifferentiableVariables`.
  bool allMembersAssocTypesEqualsSelf =
      llvm::all_of(diffProperties, [&](VarDecl *member) {
        auto tangentType =
            getAssociatedType(member, nominal, C.Id_TangentVector);
        auto cotangentType =
            getAssociatedType(member, nominal, C.Id_CotangentVector);
        auto allDiffableVarsType =
            getAssociatedType(member, nominal, C.Id_AllDifferentiableVariables);
        return member->getType()->isEqual(tangentType) &&
               member->getType()->isEqual(cotangentType) &&
               member->getType()->isEqual(allDiffableVarsType);
      });

  // If the following conditions hold:
  // - No `@noDerivative stored properties exist.
  // - All stored properties have all `Differentiable` protocol associated
  //    types equal to `Self`.
  // Then get or synthesize `AllDifferentiableVariables` struct and let
  // `TangentVector` and `CotangentVector` alias to it.
  if (allMembersAssocTypesEqualsSelf) {
    auto allDiffableVarsStruct = getOrSynthesizeSingleAssociatedStruct(
        derived, C.Id_AllDifferentiableVariables);
    addAssociatedTypeAliasDecl(C.Id_AllDifferentiableVariables,
                               allDiffableVarsStruct, allDiffableVarsStruct, TC);
    addAssociatedTypeAliasDecl(C.Id_TangentVector,
                               allDiffableVarsStruct, allDiffableVarsStruct, TC);
    addAssociatedTypeAliasDecl(C.Id_CotangentVector,
                               allDiffableVarsStruct, allDiffableVarsStruct, TC);
    addAssociatedTypeAliasDecl(C.Id_TangentVector,
                               nominal, allDiffableVarsStruct, TC);
    addAssociatedTypeAliasDecl(C.Id_CotangentVector,
                               nominal, allDiffableVarsStruct, TC);
    TC.validateDecl(allDiffableVarsStruct);
    return parentDC->mapTypeIntoContext(
        allDiffableVarsStruct->getDeclaredInterfaceType());
  }

  // Otherwise, get or synthesize associated struct type.
  return getOrSynthesizeAssociatedStructType(derived, id);
}

ValueDecl *DerivedConformance::deriveDifferentiable(ValueDecl *requirement) {
  if (requirement->getBaseName() == TC.Context.Id_moved)
    return deriveDifferentiable_moved(*this);
  if (requirement->getBaseName() == TC.Context.Id_tangentVector)
    return deriveDifferentiable_tangentVector(*this);
  if (requirement->getBaseName() == TC.Context.Id_allDifferentiableVariables)
    return deriveDifferentiable_allDifferentiableVariables(*this);
  TC.diagnose(requirement->getLoc(), diag::broken_differentiable_requirement);
  return nullptr;
}

Type DerivedConformance::deriveDifferentiable(AssociatedTypeDecl *requirement) {
  if (requirement->getBaseName() == TC.Context.Id_TangentVector)
    return deriveDifferentiable_AssociatedStruct(
        *this, TC.Context.Id_TangentVector);
  if (requirement->getBaseName() == TC.Context.Id_CotangentVector)
    return deriveDifferentiable_AssociatedStruct(
        *this, TC.Context.Id_CotangentVector);
  if (requirement->getBaseName() == TC.Context.Id_AllDifferentiableVariables)
    return deriveDifferentiable_AssociatedStruct(
        *this, TC.Context.Id_AllDifferentiableVariables);
  TC.diagnose(requirement->getLoc(), diag::broken_differentiable_requirement);
  return nullptr;
}
