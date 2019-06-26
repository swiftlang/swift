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
  assert(lookup.size() <= 1 && "Ambiguous protocol requirement");
  return lookup.front();
}

// Get the stored properties of a nominal type that are relevant for
// differentiation, except the ones tagged `@noDerivative`.
static void
getStoredPropertiesForDifferentiation(NominalTypeDecl *nominal,
                                      DeclContext *DC,
                                      SmallVectorImpl<VarDecl *> &result) {
  auto &C = nominal->getASTContext();
  auto *diffableProto = C.getProtocol(KnownProtocolKind::Differentiable);
  for (auto *vd : nominal->getStoredProperties()) {
    if (vd->getAttrs().hasAttribute<NoDerivativeAttr>())
      continue;
    if (vd->isLet())
      continue;
    if (!vd->hasInterfaceType())
      C.getLazyResolver()->resolveDeclSignature(vd);
    if (!vd->hasInterfaceType())
      continue;
    auto varType = DC->mapTypeIntoContext(vd->getValueInterfaceType());
    if (!TypeChecker::conformsToProtocol(varType, diffableProto, nominal,
                                         None))
      continue;
    result.push_back(vd);
  }
}

// Convert the given `ValueDecl` to a `StructDecl` if it is a `StructDecl` or a
// `TypeDecl` with an underlying struct type. Otherwise, return `nullptr`.
static StructDecl *convertToStructDecl(ValueDecl *v) {
  if (auto *structDecl = dyn_cast<StructDecl>(v))
    return structDecl;
  auto *typeDecl = dyn_cast<TypeDecl>(v);
  if (!typeDecl)
    return nullptr;
  return dyn_cast_or_null<StructDecl>(
      typeDecl->getDeclaredInterfaceType()->getAnyNominal());
}

// Get the `Differentiable` protocol associated type for the given `VarDecl`.
// TODO: Generalize and move function to shared place for use with other derived
// conformances.
static Type getAssociatedType(VarDecl *decl, DeclContext *DC, Identifier id) {
  auto &C = decl->getASTContext();
  auto *diffableProto = C.getProtocol(KnownProtocolKind::Differentiable);
  if (!decl->hasInterfaceType())
    C.getLazyResolver()->resolveDeclSignature(decl);
  auto varType = DC->mapTypeIntoContext(decl->getValueInterfaceType());
  auto conf = TypeChecker::conformsToProtocol(varType, diffableProto, DC,
                                              None);
  if (!conf)
    return nullptr;
  Type assocType = conf->getTypeWitnessByName(varType, id);
  return assocType;
}

// Get the `Differentiable` protocol associated struct for the given nominal
// `DeclContext`. Asserts that the associated struct type exists.
static StructDecl *getAssociatedStructDecl(DeclContext *DC, Identifier id) {
  assert(DC->getSelfNominalTypeDecl() && "Must be a nominal `DeclContext`");
  auto &C = DC->getASTContext();
  auto *diffableProto = C.getProtocol(KnownProtocolKind::Differentiable);
  assert(diffableProto && "`Differentiable` protocol not found");
  auto conf = TypeChecker::conformsToProtocol(DC->getSelfTypeInContext(),
                                              diffableProto,
                                              DC, None);
  assert(conf && "Nominal must conform to `Differentiable`");
  Type assocType = conf->getTypeWitnessByName(DC->getSelfTypeInContext(), id);
  assert(assocType && "`Differentiable` protocol associated type not found");
  auto *structDecl = dyn_cast<StructDecl>(assocType->getAnyNominal());
  assert(structDecl && "Associated type must be a struct type");
  return structDecl;
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

  // Nominal type must not customize `TangentVector` or
  // `AllDifferentiableVariables` to anything other than `Self`.
  // Otherwise, synthesis is semantically unsupported.
  auto tangentDecls = nominal->lookupDirect(C.Id_TangentVector);
  auto allDiffableVarsDecls =
      nominal->lookupDirect(C.Id_AllDifferentiableVariables);
  auto nominalTypeInContext =
      DC->mapTypeIntoContext(nominal->getDeclaredInterfaceType());

  auto isValidAssocTypeCandidate =
      [&](ValueDecl *v, bool checkAdditiveArithmetic = false) -> StructDecl * {
    // Valid candidate must be a struct or a typealias to a struct.
    auto *structDecl = convertToStructDecl(v);
    if (!structDecl)
      return nullptr;
    // Valid candidate must either:
    // 1. Be implicit (previously synthesized).
    if (structDecl->isImplicit())
      return structDecl;
    // 2. Equal nominal's implicit parent.
    //    This can occur during mutually recursive constraints. Example:
    //   `X == X.TangentVector`.
    if (nominal->isImplicit() && structDecl == nominal->getDeclContext() &&
        TypeChecker::conformsToProtocol(structDecl->getDeclaredInterfaceType(),
                                        diffableProto, DC,
                                        None))
      return structDecl;
    // 3. Equal nominal (and conform to `AdditiveArithmetic` if flag is true).
    if (structDecl == nominal) {
      if (!checkAdditiveArithmetic)
        return structDecl;
      // Check conformance to `AdditiveArithmetic`.
      if (TypeChecker::conformsToProtocol(nominalTypeInContext, addArithProto,
                                          DC, None))
        return structDecl;
    }
    // Otherwise, candidate is invalid.
    return nullptr;
  };

  auto invalidTangentDecls = llvm::partition(tangentDecls, [&](ValueDecl *v) {
    return isValidAssocTypeCandidate(v, /*checkAdditiveArithmetic*/ true);
  });
  auto invalidAllDiffableVarsDecls =
      llvm::partition(allDiffableVarsDecls, isValidAssocTypeCandidate);

  auto validTangentDeclCount =
      std::distance(tangentDecls.begin(), invalidTangentDecls);
  auto invalidTangentDeclCount =
      std::distance(invalidTangentDecls, tangentDecls.end());
  auto validAllDiffableVarsDeclCount =
      std::distance(allDiffableVarsDecls.begin(), invalidAllDiffableVarsDecls);
  auto invalidAllDiffableVarsDeclCount =
      std::distance(invalidAllDiffableVarsDecls, allDiffableVarsDecls.end());

  // There cannot be any invalid associated types. There can be at most one
  // valid associated type.
  if (invalidTangentDeclCount != 0 ||
      invalidAllDiffableVarsDeclCount != 0 ||
      validTangentDeclCount > 1 ||
      validAllDiffableVarsDeclCount > 1)
    return false;

  // All stored properties not marked with `@noDerivative`:
  // - Must conform to `Differentiable`.
  // - Must not have any `let` stored properties with an initial value.
  //   - This restriction may be lifted later with support for "true" memberwise
  //     initializers that initialize all stored properties, including initial
  //     value information.
  SmallVector<VarDecl *, 16> diffProperties;
  getStoredPropertiesForDifferentiation(structDecl, DC, diffProperties);
  return llvm::all_of(diffProperties, [&](VarDecl *v) {
    if (!v->hasInterfaceType())
      lazyResolver->resolveDeclSignature(v);
    if (!v->hasInterfaceType())
      return false;
    auto varType = DC->mapTypeIntoContext(v->getValueInterfaceType());
    return (bool)TypeChecker::conformsToProtocol(varType, diffableProto, DC,
                                                 None);
  });
}

// Synthesize body for a `Differentiable` method requirement.
static void deriveBodyDifferentiable_method(AbstractFunctionDecl *funcDecl,
                                            Identifier methodName,
                                            Identifier methodParamLabel) {
  auto *parentDC = funcDecl->getParent();
  auto *nominal = parentDC->getSelfNominalTypeDecl();
  auto &C = nominal->getASTContext();

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

  SmallVector<VarDecl *, 8> diffProperties;
  getStoredPropertiesForDifferentiation(nominal, parentDC, diffProperties);

  // Create call expression applying a member method to a parameter member.
  // Format: `<member>.method(<parameter>.<member>)`.
  // Example: `x.move(along: direction.x)`.
  auto createMemberMethodCallExpr = [&](VarDecl *member) -> Expr * {
    auto *module = nominal->getModuleContext();
    auto memberType =
        parentDC->mapTypeIntoContext(member->getValueInterfaceType());
    auto confRef = module->lookupConformance(memberType, diffProto);
    assert(confRef && "Member does not conform to `Differentiable`");

    // Get member type's method, e.g. `Member.move(along:)`.
    // Use protocol requirement declaration for the method by default: this
    // will be dynamically dispatched.
    ValueDecl *memberMethodDecl = methodReq;
    // If conformance reference is concrete, then use concrete witness
    // declaration for the operator.
    if (confRef->isConcrete())
      memberMethodDecl = confRef->getConcrete()->getWitnessDecl(
          methodReq, C.getLazyResolver());
    assert(memberMethodDecl && "Member method declaration must exist");
    auto memberMethodDRE =
        new (C) DeclRefExpr(memberMethodDecl, DeclNameLoc(), /*Implicit*/ true);
    memberMethodDRE->setFunctionRefKind(FunctionRefKind::SingleApply);

    // Create reference to member method: `x.move(along:)`.
    auto memberExpr =
        new (C) MemberRefExpr(selfDRE, SourceLoc(), member, DeclNameLoc(),
                              /*Implicit*/ true);
    auto memberMethodExpr =
        new (C) DotSyntaxCallExpr(memberMethodDRE, SourceLoc(), memberExpr);

    // Create reference to parameter member: `direction.x`.
    VarDecl *paramMember = nullptr;
    auto *paramNominal = paramDecl->getType()->getAnyNominal();
    assert(paramNominal && "Parameter should have a nominal type");
    // Find parameter member corresponding to returned nominal member.
    for (auto *candidate : paramNominal->getStoredProperties()) {
      if (candidate->getName() == member->getName()) {
        paramMember = candidate;
        break;
      }
    }
    assert(paramMember && "Could not find corresponding parameter member");
    auto *paramMemberExpr =
        new (C) MemberRefExpr(paramDRE, SourceLoc(), paramMember, DeclNameLoc(),
                              /*Implicit*/ true);
    // Create expression: `x.move(along: direction.x)`.
    return CallExpr::createImplicit(C, memberMethodExpr, {paramMemberExpr},
                                    {methodParamLabel});
  };

  // Create array of member method call expressions.
  llvm::SmallVector<ASTNode, 2> memberMethodCallExprs;
  llvm::SmallVector<Identifier, 2> memberNames;
  for (auto *member : diffProperties) {
    memberMethodCallExprs.push_back(createMemberMethodCallExpr(member));
    memberNames.push_back(member->getName());
  }
  funcDecl->setBody(BraceStmt::create(
      C, SourceLoc(), memberMethodCallExprs, SourceLoc(), true));
}

// Synthesize body for `move(along:)`.
static void deriveBodyDifferentiable_move(AbstractFunctionDecl *funcDecl,
                                           void *) {
  auto &C = funcDecl->getASTContext();
  deriveBodyDifferentiable_method(funcDecl, C.Id_move,
                                  C.getIdentifier("along"));
}

// Synthesize function declaration for a `Differentiable` method requirement.
static ValueDecl *deriveDifferentiable_method(
    DerivedConformance &derived, Identifier methodName, Identifier argumentName,
    Identifier parameterName, Type parameterType, Type returnType,
    AbstractFunctionDecl::BodySynthesizer bodySynthesizer) {
  auto *nominal = derived.Nominal;
  auto &C = derived.TC.Context;
  auto *parentDC = derived.getConformanceContext();

  auto *param =
      new (C) ParamDecl(VarDecl::Specifier::Default, SourceLoc(), SourceLoc(),
                        argumentName, SourceLoc(), parameterName, parentDC);
  param->setInterfaceType(parameterType);
  ParameterList *params = ParameterList::create(C, {param});

  DeclName declName(C, methodName, params);
  auto *funcDecl = FuncDecl::create(C, SourceLoc(), StaticSpellingKind::None,
                                    SourceLoc(), declName, SourceLoc(),
                                    /*Throws*/ false, SourceLoc(),
                                    /*GenericParams=*/nullptr, params,
                                    TypeLoc::withoutLoc(returnType), parentDC);
  funcDecl->setSelfAccessKind(SelfAccessKind::Mutating);
  funcDecl->setImplicit();
  funcDecl->setBodySynthesizer(bodySynthesizer.Fn, bodySynthesizer.Context);

  if (auto *env = parentDC->getGenericEnvironmentOfContext())
    funcDecl->setGenericEnvironment(env);
  funcDecl->computeType();
  funcDecl->copyFormalAccessFrom(nominal, /*sourceIsParentContext*/ true);
  funcDecl->setValidationToChecked();

  derived.addMembersToConformanceContext({funcDecl});
  C.addSynthesizedDecl(funcDecl);

  return funcDecl;
}

// Synthesize the `move(along:)` function declaration.
static ValueDecl *deriveDifferentiable_move(DerivedConformance &derived) {
  auto &C = derived.TC.Context;
  auto *parentDC = derived.getConformanceContext();

  auto *tangentDecl = getAssociatedStructDecl(parentDC, C.Id_TangentVector);
  auto tangentType = tangentDecl->getDeclaredInterfaceType();

  return deriveDifferentiable_method(
      derived, C.Id_move, C.getIdentifier("along"),
      C.getIdentifier("direction"), tangentType, C.TheEmptyTupleType,
      {deriveBodyDifferentiable_move, nullptr});
}

// Return the underlying `allDifferentiableVariables` of a VarDecl `x`.
// If `x` conforms to `Differentiable`, return `allDifferentiableVariables`.
// Otherwise, return `x`.
static ValueDecl *getUnderlyingAllDiffableVariables(DeclContext *DC,
                                                    VarDecl *varDecl) {
  auto *module = DC->getParentModule();
  auto &C = module->getASTContext();
  auto *diffableProto = C.getProtocol(KnownProtocolKind::Differentiable);
  auto *allDiffableVarsReq =
      getProtocolRequirement(diffableProto, C.Id_allDifferentiableVariables);
  if (!varDecl->hasInterfaceType())
    C.getLazyResolver()->resolveDeclSignature(varDecl);
  auto varType = DC->mapTypeIntoContext(varDecl->getValueInterfaceType());
  auto confRef = module->lookupConformance(varType, diffableProto);
  if (!confRef)
    return varDecl;
  // Use protocol requirement as a default for abstract conformances.
  // If conformance is concrete, get concrete witness declaration instead.
  ValueDecl *allDiffableVarsDecl = allDiffableVarsReq;
  if (confRef->isConcrete())
    allDiffableVarsDecl = confRef->getConcrete()->getWitnessDecl(
        allDiffableVarsReq, C.getLazyResolver());
  return allDiffableVarsDecl;
}

// Synthesize getter body for `allDifferentiableVariables` computed property.
static void derivedBody_allDifferentiableVariablesGetter(
    AbstractFunctionDecl *getterDecl, void *) {
  auto *parentDC = getterDecl->getParent();
  auto *nominal = parentDC->getSelfNominalTypeDecl();
  auto &C = nominal->getASTContext();

  auto *allDiffableVarsStruct =
      getAssociatedStructDecl(parentDC, C.Id_AllDifferentiableVariables);
  auto *allDiffableVarsInitDecl =
      allDiffableVarsStruct->getEffectiveMemberwiseInitializer();
  assert(allDiffableVarsInitDecl &&
         "'AllDifferentiableVariables' memberwise initializer not found");

  auto *selfDecl = getterDecl->getImplicitSelfDecl();
  auto *selfDRE =
      new (C) DeclRefExpr(selfDecl, DeclNameLoc(), /*Implicit*/ true);

  auto *initDRE = new (C) DeclRefExpr(allDiffableVarsInitDecl, DeclNameLoc(),
                                      /*Implicit*/ true);
  initDRE->setFunctionRefKind(FunctionRefKind::SingleApply);

  auto allDiffableVarsType = parentDC->mapTypeIntoContext(
      allDiffableVarsStruct->getDeclaredInterfaceType());
  Expr *baseExpr = TypeExpr::createImplicit(allDiffableVarsType, C);
  auto *initExpr = new (C) ConstructorRefCallExpr(initDRE, baseExpr);
  initExpr->setThrows(false);
  initExpr->setImplicit();

  SmallVector<Expr *, 2> members;
  SmallVector<Identifier, 2> memberNames;

  llvm::DenseMap<Identifier, VarDecl *> diffPropertyMap;
  SmallVector<VarDecl *, 8> diffProperties;
  getStoredPropertiesForDifferentiation(nominal, parentDC, diffProperties);
  for (auto *member : diffProperties)
    diffPropertyMap[member->getName()] = member;

  for (auto initParam : *allDiffableVarsInitDecl->getParameters()) {
    auto member = diffPropertyMap[initParam->getName()];
    Expr *memberExpr = new (C) MemberRefExpr(selfDRE, SourceLoc(), member,
                                             DeclNameLoc(), /*Implicit*/ true);
    member->setInterfaceType(member->getValueInterfaceType());
    auto *memberAllDiffableVarsDecl =
        getUnderlyingAllDiffableVariables(parentDC, member);
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
static void derivedBody_allDifferentiableVariablesSetter(
    AbstractFunctionDecl *setterDecl, void *) {
  auto *parentDC = setterDecl->getParent();
  auto *nominal = parentDC->getSelfNominalTypeDecl();
  auto &C = nominal->getASTContext();

  auto *selfDecl = setterDecl->getImplicitSelfDecl();
  Expr *selfDRE =
      new (C) DeclRefExpr(selfDecl, DeclNameLoc(), /*Implicit*/ true);

  auto *allDiffableVarsStruct =
      getAssociatedStructDecl(parentDC, C.Id_AllDifferentiableVariables);
  auto *newValueDecl = setterDecl->getParameters()->get(0);
  Expr *newValueDRE =
      new (C) DeclRefExpr(newValueDecl, DeclNameLoc(), /*Implicit*/ true);

  // Map `AllDifferentiableVariables` struct members to their names for
  // efficient lookup.
  llvm::DenseMap<Identifier, VarDecl *> diffPropertyMap;
  for (auto *member : allDiffableVarsStruct->getStoredProperties())
    diffPropertyMap[member->getName()] = member;

  SmallVector<ASTNode, 2> assignExprs;
  SmallVector<VarDecl *, 8> diffProperties;
  getStoredPropertiesForDifferentiation(nominal, parentDC, diffProperties);
  for (auto *member : diffProperties) {
    // Skip immutable members.
    if (member->isLet())
      continue;
    // Create lhs: either `self.x` or `self.x.allDifferentiableVariables`.
    auto *lhsAllDiffableVars =
         getUnderlyingAllDiffableVariables(parentDC, member);
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
  auto *parentDC = derived.getConformanceContext();
  auto &TC = derived.TC;
  auto &C = TC.Context;

  // Get `AllDifferentiableVariables` struct.
  auto *allDiffableVarsStruct =
      getAssociatedStructDecl(parentDC, C.Id_AllDifferentiableVariables);

  auto returnInterfaceTy = allDiffableVarsStruct->getDeclaredInterfaceType();
  auto returnTy = parentDC->mapTypeIntoContext(returnInterfaceTy);

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

  addExpectedOpaqueAccessorsToStorage(allDiffableVarsDecl, C);
  triggerAccessorSynthesis(TC, allDiffableVarsDecl);

  return allDiffableVarsDecl;
}

// Return associated `TangentVector` or `AllDifferentiableVariables` struct for
// a nominal type, if it exists.
// If not, synthesize the struct. Also return a Boolean value that indicates
// whether synthesis occurred.
static std::pair<StructDecl *, bool>
getOrSynthesizeSingleAssociatedStruct(DerivedConformance &derived,
                                      Identifier id) {
  auto &TC = derived.TC;
  auto *parentDC = derived.getConformanceContext();
  auto *nominal = derived.Nominal;
  auto &C = nominal->getASTContext();

  assert(id == C.Id_TangentVector || id == C.Id_AllDifferentiableVariables);

  // If the associated struct already exists, return it.
  auto lookup = nominal->lookupDirect(id);
  assert(lookup.size() < 2 &&
         "Expected at most one associated type named member");
  if (lookup.size() == 1) {
    auto *structDecl = convertToStructDecl(lookup.front());
    assert(structDecl && "Expected lookup result to be a struct");
    return {structDecl, false};
  }

  // Otherwise, synthesize a new struct. The struct must conform to
  // `Differentiable`.
  auto *diffableProto = C.getProtocol(KnownProtocolKind::Differentiable);
  auto diffableType = TypeLoc::withoutLoc(diffableProto->getDeclaredType());
  auto *addArithProto = C.getProtocol(KnownProtocolKind::AdditiveArithmetic);
  auto addArithType = TypeLoc::withoutLoc(addArithProto->getDeclaredType());
  auto *pointMulProto =
      C.getProtocol(KnownProtocolKind::PointwiseMultiplicative);
  auto pointMulType = TypeLoc::withoutLoc(pointMulProto->getDeclaredType());
  auto *mathProto = C.getProtocol(KnownProtocolKind::ElementaryFunctions);
  auto mathType = TypeLoc::withoutLoc(mathProto->getDeclaredType());
  auto *vectorProto = C.getProtocol(KnownProtocolKind::VectorProtocol);
  auto vectorType = TypeLoc::withoutLoc(vectorProto->getDeclaredType());
  auto *kpIterableProto = C.getProtocol(KnownProtocolKind::KeyPathIterable);
  auto kpIterableType = TypeLoc::withoutLoc(kpIterableProto->getDeclaredType());

  SmallVector<TypeLoc, 4> inherited{diffableType};

  // Cache original members and their associated types for later use.
  SmallVector<VarDecl *, 8> diffProperties;
  getStoredPropertiesForDifferentiation(nominal, parentDC, diffProperties);

  // If the associated type is `TangentVector`, make it also conform to
  // `AdditiveArithmetic`.
  if (id == C.Id_TangentVector)
    inherited.push_back(addArithType);

  // Associated struct can derive `AdditiveArithmetic` if the associated types
  // of all stored properties conform to `AdditiveArithmetic`.
  bool canDeriveAdditiveArithmetic =
      llvm::all_of(diffProperties, [&](VarDecl *vd) {
        return TC.conformsToProtocol(getAssociatedType(vd, parentDC, id),
                                     addArithProto, parentDC, None);
      });

  // Associated struct can derive `PointwiseMultiplicative` if the associated
  // types of all stored properties conform to `PointwiseMultiplicative`.
  bool canDerivePointwiseMultiplicative =
      llvm::all_of(diffProperties, [&](VarDecl *vd) {
        return TC.conformsToProtocol(getAssociatedType(vd, parentDC, id),
                                     pointMulProto, parentDC, None);
      });

  // Associated struct can derive `ElementaryFunctions` if the associated types
  // of all stored properties conform to `ElementaryFunctions`.
  bool canDeriveElementaryFunctions =
      llvm::all_of(diffProperties, [&](VarDecl *vd) {
        return TC.conformsToProtocol(getAssociatedType(vd, parentDC, id),
                                     mathProto, parentDC, None);
      });

  // Associated struct can derive `VectorProtocol` if the associated types of
  // all members conform to `VectorProtocol` and share the same
  // `VectorSpaceScalar` type.
  Type sameScalarType;
  bool canDeriveVectorProtocol =
      canDeriveAdditiveArithmetic && !diffProperties.empty() &&
      llvm::all_of(diffProperties, [&](VarDecl *vd) {
        auto conf = TC.conformsToProtocol(getAssociatedType(vd, parentDC, id),
                                          vectorProto, nominal, None);
        if (!conf)
          return false;
        auto scalarType =
            conf->getTypeWitnessByName(vd->getType(), C.Id_VectorSpaceScalar);
        if (!sameScalarType) {
          sameScalarType = scalarType;
          return true;
        }
        return scalarType->isEqual(sameScalarType);
      });

  // If the associated struct is `AllDifferentiableVariables`, conform it to:
  // - `AdditiveArithmetic`, if all members of the parent conform to
  //   `AdditiveArithmetic`.
  // - `KeyPathIterable`, if the parent conforms to to `KeyPathIterable`.
  if (id == C.Id_AllDifferentiableVariables) {
    if (canDeriveAdditiveArithmetic)
      inherited.push_back(addArithType);
    if (TC.conformsToProtocol(nominal->getDeclaredInterfaceType(),
                              kpIterableProto, parentDC,
                              None))
      inherited.push_back(kpIterableType);
  }
  // If all members conform to `PointwiseMultiplicative`, make the associated
  // struct conform to `PointwiseMultiplicative`.
  if (canDerivePointwiseMultiplicative)
    inherited.push_back(pointMulType);
  // If all members conform to `ElementaryFunctions`, make the associated struct
  // conform to `ElementaryFunctions`.
  if (canDeriveElementaryFunctions)
    inherited.push_back(mathType);
  // If all members also conform to `VectorProtocol` with the same `Scalar`
  // type, make the associated struct conform to `VectorProtocol` instead of
  // just `AdditiveArithmetic`.
  if (canDeriveVectorProtocol)
    inherited.push_back(vectorType);

  auto *structDecl = new (C) StructDecl(SourceLoc(), id, SourceLoc(),
                                        /*Inherited*/ C.AllocateCopy(inherited),
                                        /*GenericParams*/ {}, parentDC);
  structDecl->setImplicit();
  structDecl->copyFormalAccessFrom(nominal, /*sourceIsParentContext*/ true);

  // Add members to associated struct.
  for (auto *member : diffProperties) {
    // Add this member's corresponding associated type to the parent's
    // associated struct.
    auto *newMember = new (C) VarDecl(
        member->isStatic(), member->getSpecifier(), member->isCaptureList(),
        /*NameLoc*/ SourceLoc(), member->getName(), structDecl);
    // NOTE: `newMember` is not marked as implicit here, because that affects
    // memberwise initializer synthesis.

    auto memberAssocType = getAssociatedType(member, parentDC, id);
    auto memberAssocInterfaceType = memberAssocType->hasArchetype()
                                        ? memberAssocType->mapTypeOutOfContext()
                                        : memberAssocType;
    auto memberAssocContextualType =
        parentDC->mapTypeIntoContext(memberAssocInterfaceType);
    newMember->setInterfaceType(memberAssocInterfaceType);
    newMember->setType(memberAssocContextualType);
    Pattern *memberPattern =
        new (C) NamedPattern(newMember, /*implicit*/ true);
    memberPattern->setType(memberAssocContextualType);
    memberPattern = TypedPattern::createImplicit(
        C, memberPattern, memberAssocContextualType);
    memberPattern->setType(memberAssocContextualType);
    auto *memberBinding = PatternBindingDecl::createImplicit(
        C, StaticSpellingKind::None, memberPattern, /*initExpr*/ nullptr,
        structDecl);
    structDecl->addMember(newMember);
    structDecl->addMember(memberBinding);
    newMember->copyFormalAccessFrom(member, /*sourceIsParentContext*/ true);
    newMember->setValidationToChecked();
    newMember->setSetterAccess(member->getFormalAccess());
    addExpectedOpaqueAccessorsToStorage(newMember, C);
    C.addSynthesizedDecl(newMember);
    C.addSynthesizedDecl(memberBinding);

    // Now that this member is in the associated type, it should be marked
    // `@differentiable` so that the differentiation transform will synthesize
    // associated functions for it. We only add this to public stored
    // properties, because their access outside the module will go through a
    // call to the getter.
    if (member->getEffectiveAccess() > AccessLevel::Internal &&
        !member->getAttrs().hasAttribute<DifferentiableAttr>()) {
      // If getter does not exist, trigger synthesis and compute type.
      if (!member->getGetter())
        addExpectedOpaqueAccessorsToStorage(member, C);
      if (!member->getGetter()->hasInterfaceType())
        TC.resolveDeclSignature(member->getGetter());
      // If member or its getter already has a `@differentiable` attribute,
      // continue.
      if (member->getAttrs().hasAttribute<DifferentiableAttr>() ||
          member->getGetter()->getAttrs().hasAttribute<DifferentiableAttr>())
        continue;
      ArrayRef<Requirement> requirements;
      // If the parent declaration context is an extension, the nominal type may
      // conditionally conform to `Differentiable`. Use the conditional
      // conformance requirements in getter `@differentiable` attributes.
      if (auto *extDecl = dyn_cast<ExtensionDecl>(parentDC->getAsDecl()))
        requirements = extDecl->getGenericRequirements();
      auto *diffableAttr = DifferentiableAttr::create(
          C, /*implicit*/ true, SourceLoc(), SourceLoc(),
          /*linear*/ false, {}, None, None, requirements);
      member->getAttrs().add(diffableAttr);
      // Compute getter parameter indices.
      auto *getterType =
          member->getGetter()->getInterfaceType()->castTo<AnyFunctionType>();
      AutoDiffParameterIndicesBuilder builder(getterType);
      builder.setParameter(0);
      diffableAttr->setParameterIndices(builder.build(C));
    }
  }

  // If nominal type has `@_fixed_layout` attribute, mark associated struct as
  // `@_fixed_layout` as well.
  if (nominal->getAttrs().hasAttribute<FixedLayoutAttr>())
    structDecl->addFixedLayoutAttr();

  // The implicit memberwise constructor must be explicitly created so that it
  // can called in `AdditiveArithmetic` and `Differentiable` methods. Normally,
  // the memberwise constructor is synthesized during SILGen, which is too late.
  auto *initDecl = createImplicitConstructor(
      TC, structDecl, ImplicitConstructorKind::Memberwise);
  structDecl->addMember(initDecl);
  C.addSynthesizedDecl(initDecl);

  // After memberwise initializer is synthesized, mark members as implicit.
  for (auto *member : structDecl->getStoredProperties())
    member->setImplicit();

  derived.addMembersToConformanceContext({structDecl});
  C.addSynthesizedDecl(structDecl);

  return {structDecl, true};
}

// Add a typealias declaration with the given name and underlying target
// struct type to the given source nominal declaration context.
static void addAssociatedTypeAliasDecl(Identifier name,
                                       DeclContext *sourceDC,
                                       StructDecl *target,
                                       TypeChecker &TC) {
  auto &C = TC.Context;
  auto *nominal = sourceDC->getSelfNominalTypeDecl();
  assert(nominal && "Expected `DeclContext` to be a nominal type");
  auto lookup = nominal->lookupDirect(name);
  assert(lookup.size() < 2 &&
         "Expected at most one associated type named member");
  // If implicit type declaration with the given name already exists in source
  // struct, return it.
  if (lookup.size() == 1) {
    auto existingTypeDecl = dyn_cast<TypeDecl>(lookup.front());
    assert(existingTypeDecl && existingTypeDecl->isImplicit() &&
           "Expected lookup result to be an implicit type declaration");
    return;
  }
  // Otherwise, create a new typealias.
  auto *aliasDecl = new (C)
      TypeAliasDecl(SourceLoc(), SourceLoc(), name, SourceLoc(), {}, sourceDC);
  aliasDecl->setUnderlyingType(target->getDeclaredInterfaceType());
  aliasDecl->setImplicit();
  if (auto env = sourceDC->getGenericEnvironmentOfContext())
    aliasDecl->setGenericEnvironment(env);
  cast<IterableDeclContext>(sourceDC->getAsDecl())->addMember(aliasDecl);
  aliasDecl->copyFormalAccessFrom(nominal, /*sourceIsParentContext*/ true);
  aliasDecl->setValidationToChecked();
  TC.validateDecl(aliasDecl);
  C.addSynthesizedDecl(aliasDecl);
};

// Diagnose stored properties in the nominal that do not have an explicit
// `@noDerivative` attribute, but either:
// - Do not conform to `Differentiable`.
// - Are a `let` stored property.
// Emit a warning and a fixit so that users will make the attribute explicit.
static void checkAndDiagnoseImplicitNoDerivative(TypeChecker &TC,
                                                 NominalTypeDecl *nominal,
                                                 DeclContext* DC) {
  auto *diffableProto =
      TC.Context.getProtocol(KnownProtocolKind::Differentiable);
  for (auto *vd : nominal->getStoredProperties()) {
    if (!vd->hasInterfaceType())
      TC.resolveDeclSignature(vd);
    if (!vd->hasInterfaceType())
      continue;
    auto varType = DC->mapTypeIntoContext(vd->getValueInterfaceType());
    if (vd->getAttrs().hasAttribute<NoDerivativeAttr>())
      continue;
    // Check whether to diagnose stored property.
    bool conformsToDifferentiable =
        TC.conformsToProtocol(varType, diffableProto, nominal,
                              None).hasValue();
    // If stored property should not be diagnosed, continue.
    if (conformsToDifferentiable && !vd->isLet())
      continue;
    // Otherwise, add an implicit `@noDerivative` attribute.
    vd->getAttrs().add(
        new (TC.Context) NoDerivativeAttr(/*Implicit*/ true));
    auto loc = vd->getAttributeInsertionLoc(/*forModifier*/ false);
    assert(loc.isValid() && "Expected valid source location");
    // If nominal type can conform to `AdditiveArithmetic`, suggest conforming
    // adding a conformance to `AdditiveArithmetic`.
    // `Differentiable` protocol requirements all have default implementations
    // when `Self` conforms to `AdditiveArithmetic`, so `Differentiable`
    // derived conformances will no longer be necessary.
    bool nominalCanDeriveAdditiveArithmetic =
        DerivedConformance::canDeriveAdditiveArithmetic(nominal, DC);
    if (!conformsToDifferentiable) {
      TC.diagnose(loc,
                  diag::differentiable_nondiff_type_implicit_noderivative_fixit,
                  vd->getName(), nominal->getName(),
                  nominalCanDeriveAdditiveArithmetic)
          .fixItInsert(loc, "@noDerivative ");
      continue;
    }
    TC.diagnose(loc,
                diag::differentiable_let_property_implicit_noderivative_fixit,
                vd->getName(), nominal->getName(),
                nominalCanDeriveAdditiveArithmetic)
        .fixItInsert(loc, "@noDerivative ");

  }
}

// Get or synthesize all associated struct types: `TangentVector`,
// and `AllDifferentiableVariables`.
// Return the type corresponding to the given identifier.
static Type
getOrSynthesizeAssociatedStructType(DerivedConformance &derived,
                                    Identifier id) {
  auto &TC = derived.TC;
  auto *parentDC = derived.getConformanceContext();
  auto *nominal = derived.Nominal;
  auto &C = nominal->getASTContext();

  // Get or synthesize `AllDifferentiableVariables` and `TangentVector` structs
  // at once. Synthesizing all three structs at once is necessary in order to
  // correctly set their mutually recursive associated types.
  auto allDiffableVarsStructSynthesis =
      getOrSynthesizeSingleAssociatedStruct(derived,
                                            C.Id_AllDifferentiableVariables);
  auto *allDiffableVarsStruct = allDiffableVarsStructSynthesis.first;
  if (!allDiffableVarsStruct)
    return nullptr;
  bool freshlySynthesized = allDiffableVarsStructSynthesis.second;

  auto tangentStructSynthesis =
      getOrSynthesizeSingleAssociatedStruct(derived, C.Id_TangentVector);
  auto *tangentStruct = tangentStructSynthesis.first;
  if (!tangentStruct)
    return nullptr;
  freshlySynthesized |= tangentStructSynthesis.second;

  // When all structs are freshly synthesized, we check emit warnings for
  // implicit `@noDerivative` members. Checking for fresh synthesis is necessary
  // because `getOrSynthesizeAssociatedStructType` will be called multiple times
  // during synthesis.
  if (freshlySynthesized)
    checkAndDiagnoseImplicitNoDerivative(TC, nominal, parentDC);

  // Add associated typealiases for structs.
  addAssociatedTypeAliasDecl(C.Id_TangentVector,
                             tangentStruct, tangentStruct, TC);
  addAssociatedTypeAliasDecl(C.Id_TangentVector,
                             allDiffableVarsStruct, tangentStruct, TC);

  addAssociatedTypeAliasDecl(C.Id_AllDifferentiableVariables,
                             allDiffableVarsStruct, allDiffableVarsStruct, TC);
  addAssociatedTypeAliasDecl(C.Id_AllDifferentiableVariables,
                             tangentStruct, tangentStruct, TC);

  TC.validateDecl(allDiffableVarsStruct);
  TC.validateDecl(tangentStruct);

  // Sanity checks for synthesized structs.
  assert(DerivedConformance::canDeriveAdditiveArithmetic(tangentStruct,
                                                         parentDC) &&
         "Should be able to derive `AdditiveArithmetic`");
  assert(DerivedConformance::canDeriveDifferentiable(
      tangentStruct, parentDC) && "Should be able to derive `Differentiable`");
  assert(DerivedConformance::canDeriveDifferentiable(
      allDiffableVarsStruct, parentDC) &&
          "Should be able to derive `Differentiable`");

  // Return the requested associated struct type.
  StructDecl *requestedStructDecl = nullptr;
  if (id == C.Id_TangentVector)
    requestedStructDecl = tangentStruct;
  else if (id == C.Id_AllDifferentiableVariables)
    requestedStructDecl = allDiffableVarsStruct;
  else
    llvm_unreachable("Unknown `Differentiable` associated type identifier");
  return parentDC->mapTypeIntoContext(
      requestedStructDecl->getDeclaredInterfaceType());
}

// Synthesize an associated struct type (`TangentVector` or
// `AllDifferentiableVariables`).
static Type
deriveDifferentiable_AssociatedStruct(DerivedConformance &derived,
                                      Identifier id) {
  auto &TC = derived.TC;
  auto *parentDC = derived.getConformanceContext();
  auto *nominal = derived.Nominal;
  auto &C = nominal->getASTContext();

  // Get all stored properties for differentation.
  SmallVector<VarDecl *, 16> diffProperties;
  getStoredPropertiesForDifferentiation(nominal, parentDC, diffProperties);

  // If any member has an invalid associated type, return nullptr.
  for (auto *member : diffProperties)
    if (!getAssociatedType(member, parentDC, id))
      return nullptr;

  // Prevent re-synthesis during repeated calls.
  // FIXME: Investigate why this is necessary to prevent duplicate synthesis.
  auto lookup = nominal->lookupDirect(id);
  if (lookup.size() == 1)
    if (auto *structDecl = convertToStructDecl(lookup.front()))
      if (structDecl->isImplicit())
        return structDecl->getDeclaredInterfaceType();

  // Check whether at least one `@noDerivative` stored property exists.
  unsigned numStoredProperties =
      std::distance(nominal->getStoredProperties().begin(),
                    nominal->getStoredProperties().end());
  bool hasNoDerivativeStoredProp = diffProperties.size() != numStoredProperties;

  // Check conditions for returning `Self`.
  // - No `@noDerivative` stored properties exist.
  // - All stored properties must have specified associated type equal to
  //   `Self`.
  // - If associated type is `TangentVector`, parent type must also conform to
  //   `AdditiveArithmetic`.
  bool allMembersAssocTypeEqualsSelf =
      llvm::all_of(diffProperties, [&](VarDecl *member) {
        auto memberAssocType = getAssociatedType(member, parentDC, id);
        return member->getType()->isEqual(memberAssocType);
      });

  auto *addArithProto = C.getProtocol(KnownProtocolKind::AdditiveArithmetic);
  auto nominalConformsToAddArith =
      TC.conformsToProtocol(parentDC->getSelfTypeInContext(), addArithProto,
                            parentDC, None);

  // Return `Self` if conditions are met.
  if (!hasNoDerivativeStoredProp &&
      (id == C.Id_AllDifferentiableVariables ||
       (allMembersAssocTypeEqualsSelf && nominalConformsToAddArith))) {
    auto selfType = parentDC->getSelfTypeInContext();
    auto *aliasDecl = new (C) TypeAliasDecl(SourceLoc(), SourceLoc(), id,
                                            SourceLoc(), {}, parentDC);
    aliasDecl->setUnderlyingType(selfType);
    aliasDecl->setImplicit();
    aliasDecl->copyFormalAccessFrom(nominal, /*sourceIsParentContext*/ true);
    aliasDecl->setValidationToChecked();
    TC.validateDecl(aliasDecl);
    derived.addMembersToConformanceContext({aliasDecl});
    C.addSynthesizedDecl(aliasDecl);
    return selfType;
  }

  // Otherwise, check if all stored properties have all `Differentiable`
  // protocol associated types equal to each other:
  // `TangentVector == AllDifferentiableVariables`.
  bool allMembersAssocTypesEqualsSelf =
      llvm::all_of(diffProperties, [&](VarDecl *member) {
        auto tangentType =
            getAssociatedType(member, parentDC, C.Id_TangentVector);
        auto allDiffableVarsType =
            getAssociatedType(member, parentDC, C.Id_AllDifferentiableVariables);
        return tangentType->isEqual(allDiffableVarsType);
      });

  // If all stored properties (excluding ones with `@noDerivative`) have all
  // `Differentiable` protocol associated types equal to `Self`, then get or
  // synthesize `AllDifferentiableVariables` struct and let `TangentVector`
  // alias to it.
  if (allMembersAssocTypesEqualsSelf) {
    auto allDiffableVarsStructSynthesis = getOrSynthesizeSingleAssociatedStruct(
        derived, C.Id_AllDifferentiableVariables);
    auto *allDiffableVarsStruct = allDiffableVarsStructSynthesis.first;
    auto freshlySynthesized = allDiffableVarsStructSynthesis.second;
    // When the struct is freshly synthesized, we check emit warnings for
    // implicit `@noDerivative` members. Checking for fresh synthesis is
    // necessary because this code path will be executed called multiple times
    // during synthesis.
    if (freshlySynthesized)
      checkAndDiagnoseImplicitNoDerivative(TC, nominal, parentDC);
    addAssociatedTypeAliasDecl(C.Id_AllDifferentiableVariables,
        allDiffableVarsStruct, allDiffableVarsStruct, TC);
    addAssociatedTypeAliasDecl(C.Id_TangentVector,
        allDiffableVarsStruct, allDiffableVarsStruct, TC);
    addAssociatedTypeAliasDecl(C.Id_TangentVector,
        parentDC, allDiffableVarsStruct, TC);
    TC.validateDecl(allDiffableVarsStruct);
    return parentDC->mapTypeIntoContext(
        allDiffableVarsStruct->getDeclaredInterfaceType());
  }

  // Otherwise, get or synthesize associated struct type.
  return getOrSynthesizeAssociatedStructType(derived, id);
}

ValueDecl *DerivedConformance::deriveDifferentiable(ValueDecl *requirement) {
  // Diagnose conformances in disallowed contexts.
  if (checkAndDiagnoseDisallowedContext(requirement))
    return nullptr;
  if (requirement->getBaseName() == TC.Context.Id_move)
    return deriveDifferentiable_move(*this);
  if (requirement->getBaseName() == TC.Context.Id_allDifferentiableVariables)
    return deriveDifferentiable_allDifferentiableVariables(*this);
  TC.diagnose(requirement->getLoc(), diag::broken_differentiable_requirement);
  return nullptr;
}

Type DerivedConformance::deriveDifferentiable(AssociatedTypeDecl *requirement) {
  // Diagnose conformances in disallowed contexts.
  if (checkAndDiagnoseDisallowedContext(requirement))
    return nullptr;
  if (requirement->getBaseName() == TC.Context.Id_TangentVector)
    return deriveDifferentiable_AssociatedStruct(
        *this, TC.Context.Id_TangentVector);
  if (requirement->getBaseName() == TC.Context.Id_AllDifferentiableVariables)
    return deriveDifferentiable_AssociatedStruct(
        *this, TC.Context.Id_AllDifferentiableVariables);
  TC.diagnose(requirement->getLoc(), diag::broken_differentiable_requirement);
  return nullptr;
}
