//===--- DerivedConformanceAdditiveArithmeticVectorNumeric.cpp ------------===//
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
// This file implements explicit derivation of the AdditiveArithmetic and
// VectorNumeric protocols for struct types.
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

// Represents synthesizable math operators.
enum MathOperator {
  // `+(Self, Self)`, `AdditiveArithmetic` requirement
  Add,
  // `-(Self, Self)`, `AdditiveArithmetic` requirement
  Subtract,
  // `*(Scalar, Self)`, `VectorNumeric` requirement
  ScalarMultiply
};

static StringRef getMathOperatorName(MathOperator op) {
  switch (op) {
  case Add:
    return "+";
  case Subtract:
    return "-";
  case ScalarMultiply:
    return "*";
  }
}

// Return the protocol associated with a math operator.
static ProtocolDecl *getAssociatedProtocol(MathOperator op, ASTContext &C) {
  switch (op) {
  case Add:
  case Subtract:
    return C.getProtocol(KnownProtocolKind::AdditiveArithmetic);
  case ScalarMultiply:
    return C.getProtocol(KnownProtocolKind::VectorNumeric);
  }
}

// Return the protocol requirement with the specified name.
static ValueDecl *getProtocolRequirement(ProtocolDecl *proto, Identifier name) {
  auto lookup = proto->lookupDirect(name);
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

// Return the `Scalar` associated type for a ValueDecl if it conforms to
// `VectorNumeric` in the given context.
// If the decl does not conform to `VectorNumeric`, return a null `Type`.
static Type getVectorNumericScalarAssocType(VarDecl *decl, DeclContext *DC) {
  auto &C = decl->getASTContext();
  auto *vectorNumericProto = C.getProtocol(KnownProtocolKind::VectorNumeric);
  if (!decl->hasType())
    C.getLazyResolver()->resolveDeclSignature(decl);
  if (!decl->hasType())
    return Type();
  auto declType = decl->getType()->hasArchetype()
                      ? decl->getType()
                      : DC->mapTypeIntoContext(decl->getType());
  auto conf = TypeChecker::conformsToProtocol(declType, vectorNumericProto, DC,
                                              ConformanceCheckFlags::Used);
  if (!conf)
    return Type();
  Type scalarType = ProtocolConformanceRef::getTypeWitnessByName(
      declType, *conf, C.Id_Scalar, C.getLazyResolver());
  assert(scalarType && "'Scalar' associated type not found");
  return scalarType;
}

static Type deriveVectorNumeric_Scalar(NominalTypeDecl *nominal,
                                       DeclContext *DC) {
  // Nominal type must be a struct. (Zero stored properties is okay.)
  auto *structDecl = dyn_cast<StructDecl>(nominal);
  auto &C = nominal->getASTContext();
  if (!structDecl)
    return Type();
  // If all stored properties conform to `VectorNumeric` and have the same
  // `Scalar` associated type, return that `Scalar` associated type.
  // Otherwise, the `Scalar` type cannot be derived.
  Type sameScalarType;
  for (auto member : structDecl->getStoredProperties()) {
    if (!member->hasInterfaceType())
      C.getLazyResolver()->resolveDeclSignature(member);
    if (!member->hasInterfaceType())
      return Type();
    auto scalarType = getVectorNumericScalarAssocType(member, DC);
    // If stored property does not conform to `VectorNumeric`, return null
    // `Type`.
    if (!scalarType)
      return Type();
    // If same `Scalar` type has not been set, set it for the first time.
    if (!sameScalarType) {
      sameScalarType = scalarType;
      continue;
    }
    // If stored property `Scalar` types do not match, return null `Type`.
    if (!scalarType->isEqual(sameScalarType))
      return Type();
  }
  return sameScalarType;
}

bool DerivedConformance::canDeriveAdditiveArithmetic(NominalTypeDecl *nominal,
                                                     DeclContext *DC) {
  // Nominal type must be a struct. (Zero stored properties is okay.)
  auto *structDecl = dyn_cast<StructDecl>(nominal);
  if (!structDecl)
    return false;
  // All stored properties must conform to `AdditiveArithmetic`.
  auto &C = nominal->getASTContext();
  auto *addArithProto = C.getProtocol(KnownProtocolKind::AdditiveArithmetic);
  return llvm::all_of(structDecl->getStoredProperties(), [&](VarDecl *v) {
    if (!v->hasInterfaceType() || !v->getType())
      C.getLazyResolver()->resolveDeclSignature(v);
    if (!v->hasInterfaceType() || !v->getType())
      return false;
    auto declType = v->getType()->hasArchetype()
                        ? v->getType()
                        : DC->mapTypeIntoContext(v->getType());
    return (bool)TypeChecker::conformsToProtocol(declType, addArithProto, DC,
                                                 ConformanceCheckFlags::Used);
  });
}

bool DerivedConformance::canDeriveVectorNumeric(NominalTypeDecl *nominal,
                                                DeclContext *DC) {
  return bool(deriveVectorNumeric_Scalar(nominal, DC));
}

// Synthesize body for the given math operator.
static void deriveBodyMathOperator(AbstractFunctionDecl *funcDecl,
                                   MathOperator op) {
  auto *nominal = funcDecl->getDeclContext()->getSelfNominalTypeDecl();
  auto &C = nominal->getASTContext();

  // Create memberwise initializer: `Nominal.init(...)`.
  auto *memberwiseInitDecl = getMemberwiseInitializer(nominal);
  auto *initDRE =
      new (C) DeclRefExpr(memberwiseInitDecl, DeclNameLoc(), /*Implicit*/ true);
  initDRE->setFunctionRefKind(FunctionRefKind::SingleApply);
  auto *nominalTypeExpr = TypeExpr::createForDecl(SourceLoc(), nominal,
                                                  funcDecl, /*Implicit*/ true);
  auto *initExpr = new (C) ConstructorRefCallExpr(initDRE, nominalTypeExpr);

  // Get operator protocol requirement.
  auto *proto = getAssociatedProtocol(op, C);
  auto operatorId = C.getIdentifier(getMathOperatorName(op));
  auto *operatorReq = getProtocolRequirement(proto, operatorId);

  // Create reference to operator parameters: lhs and rhs.
  auto params = funcDecl->getParameters();
  auto *lhsDRE =
      new (C) DeclRefExpr(params->get(0), DeclNameLoc(), /*Implicit*/ true);
  auto *rhsDRE =
      new (C) DeclRefExpr(params->get(1), DeclNameLoc(), /*Implicit*/ true);

  // Create expression combining lhs and rhs members using member operator.
  auto createMemberOpExpr = [&](VarDecl *member) -> Expr * {
    auto module = nominal->getModuleContext();
    auto memberType = member->getType()->hasArchetype()
                          ? member->getType()
                          : nominal->mapTypeIntoContext(
                                member->getType()->mapTypeOutOfContext());
    auto confRef = module->lookupConformance(memberType, proto);
    assert(confRef && "Member does not conform to math protocol");

    // Get member type's math operator, e.g. `Member.+`.
    // Use protocol requirement declaration for the operator by default: this
    // will be dynamically dispatched.
    ValueDecl *memberOpDecl = operatorReq;
    // If conformance reference is concrete, then use concrete witness
    // declaration for the operator.
    if (confRef->isConcrete())
      if (auto opDecl =
              confRef->getConcrete()->getWitnessDecl(operatorReq, nullptr))
        memberOpDecl = opDecl;
    assert(memberOpDecl && "Member operator declaration must exist");
    auto memberOpDRE =
        new (C) DeclRefExpr(memberOpDecl, DeclNameLoc(), /*Implicit*/ true);
    auto *memberTypeExpr = TypeExpr::createImplicit(member->getType(), C);
    auto memberOpExpr =
        new (C) DotSyntaxCallExpr(memberOpDRE, SourceLoc(), memberTypeExpr);

    // Create lhs argument.
    // For `AdditiveArithmetic` operators: use `lhs.member`.
    // For `VectorNumeric.*`: use `lhs` directly.
    Expr *lhsArg = nullptr;
    switch (op) {
    case Add:
    case Subtract:
      lhsArg = new (C) MemberRefExpr(lhsDRE, SourceLoc(), member, DeclNameLoc(),
                                     /*Implicit*/ true);
      break;
    case ScalarMultiply:
      lhsArg = lhsDRE;
      break;
    }
    // Create rhs argument: `rhs.member`.
    auto *rhsArg = new (C) MemberRefExpr(rhsDRE, SourceLoc(), member,
                                         DeclNameLoc(), /*Implicit*/ true);
    // Create expression `lhsArg <op> rhsArg`.
    auto *memberOpArgs =
        TupleExpr::create(C, SourceLoc(), {lhsArg, rhsArg}, {}, {}, SourceLoc(),
                          /*HasTrailingClosure*/ false,
                          /*Implicit*/ true);
    auto *memberOpCallExpr =
        new (C) BinaryExpr(memberOpExpr, memberOpArgs, /*Implicit*/ true);
    return memberOpCallExpr;
  };

  // Create array of member operator call expressions.
  llvm::SmallVector<Expr *, 2> memberOpExprs;
  llvm::SmallVector<Identifier, 2> memberNames;
  for (auto member : nominal->getStoredProperties()) {
    memberOpExprs.push_back(createMemberOpExpr(member));
    memberNames.push_back(member->getName());
  }
  // Call memberwise initialier with member operator call expressions.
  auto *callExpr =
      CallExpr::createImplicit(C, initExpr, memberOpExprs, memberNames);
  ASTNode returnStmt = new (C) ReturnStmt(SourceLoc(), callExpr, true);
  funcDecl->setBody(
      BraceStmt::create(C, SourceLoc(), returnStmt, SourceLoc(), true));
}

// Synthesize body for `AdditiveArithmetic.+` operator.
static void deriveBodyAdditiveArithmetic_add(AbstractFunctionDecl *funcDecl) {
  deriveBodyMathOperator(funcDecl, Add);
}

// Synthesize body for `AdditiveArithmetic.-` operator.
static void
deriveBodyAdditiveArithmetic_subtract(AbstractFunctionDecl *funcDecl) {
  deriveBodyMathOperator(funcDecl, Subtract);
}

// Synthesize body for `VectorNumeric.*` operator.
static void
deriveBodyVectorNumeric_scalarMultiply(AbstractFunctionDecl *funcDecl) {
  deriveBodyMathOperator(funcDecl, ScalarMultiply);
}

// Synthesize the function declaration for the given math operator.
static ValueDecl *deriveMathOperator(DerivedConformance &derived,
                                     MathOperator op) {
  auto nominal = derived.Nominal;
  auto &C = derived.TC.Context;
  auto parentDC = derived.getConformanceContext();
  auto selfInterfaceType = parentDC->getDeclaredInterfaceType();

  // Return tuple of the lhs and rhs parameter types for the given math
  // operator.
  auto getParameterTypes = [&](MathOperator op) -> std::pair<Type, Type> {
    switch (op) {
    case Add:
    case Subtract:
      return std::make_pair(selfInterfaceType, selfInterfaceType);
    case ScalarMultiply:
      return std::make_pair(
          deriveVectorNumeric_Scalar(nominal, parentDC)->mapTypeOutOfContext(),
          selfInterfaceType);
    }
  };

  // Create parameter declaration with the given name and type.
  auto createParamDecl = [&](StringRef name, Type type) -> ParamDecl * {
    auto *param = new (C)
        ParamDecl(VarDecl::Specifier::Default, SourceLoc(), SourceLoc(),
                  Identifier(), SourceLoc(), C.getIdentifier(name), parentDC);
    param->setInterfaceType(type);
    return param;
  };

  auto paramTypes = getParameterTypes(op);
  ParameterList *params =
      ParameterList::create(C, {createParamDecl("lhs", paramTypes.first),
                                createParamDecl("rhs", paramTypes.second)});

  Identifier operatorId = C.getIdentifier(getMathOperatorName(op));
  DeclName operatorDeclName(C, operatorId, params);
  auto operatorDecl =
      FuncDecl::create(C, SourceLoc(), StaticSpellingKind::KeywordStatic,
                       SourceLoc(), operatorDeclName, SourceLoc(),
                       /*Throws*/ false, SourceLoc(),
                       /*GenericParams=*/nullptr, params,
                       TypeLoc::withoutLoc(selfInterfaceType), parentDC);
  operatorDecl->setImplicit();
  switch (op) {
  case Add:
    operatorDecl->setBodySynthesizer(deriveBodyAdditiveArithmetic_add);
    break;
  case Subtract:
    operatorDecl->setBodySynthesizer(deriveBodyAdditiveArithmetic_subtract);
    break;
  case ScalarMultiply:
    operatorDecl->setBodySynthesizer(deriveBodyVectorNumeric_scalarMultiply);
    break;
  }
  if (auto env = parentDC->getGenericEnvironmentOfContext())
    operatorDecl->setGenericEnvironment(env);
  operatorDecl->computeType();
  operatorDecl->copyFormalAccessFrom(nominal, /*sourceIsParentContext*/ true);
  operatorDecl->setValidationToChecked();

  derived.addMembersToConformanceContext({operatorDecl});
  C.addSynthesizedDecl(operatorDecl);

  return operatorDecl;
}

// Synthesize body for the `AdditiveArithmetic.zero` computed property getter.
static void deriveBodyAdditiveArithmetic_zero(AbstractFunctionDecl *funcDecl) {
  auto *nominal = funcDecl->getDeclContext()->getSelfNominalTypeDecl();
  auto &C = nominal->getASTContext();

  auto *memberwiseInitDecl = getMemberwiseInitializer(nominal);
  auto *initDRE =
      new (C) DeclRefExpr(memberwiseInitDecl, DeclNameLoc(), /*Implicit*/ true);
  initDRE->setFunctionRefKind(FunctionRefKind::SingleApply);

  auto *nominalTypeExpr = TypeExpr::createForDecl(SourceLoc(), nominal,
                                                  funcDecl, /*Implicit*/ true);
  auto *initExpr = new (C) ConstructorRefCallExpr(initDRE, nominalTypeExpr);

  auto *addArithProto = C.getProtocol(KnownProtocolKind::AdditiveArithmetic);
  auto *zeroReq = getProtocolRequirement(addArithProto, C.Id_zero);

  auto createMemberZeroExpr = [&](VarDecl *member) -> Expr * {
    auto *memberTypeExpr = TypeExpr::createImplicit(member->getType(), C);
    auto module = nominal->getModuleContext();
    auto confRef = module->lookupConformance(member->getType(), addArithProto);
    assert(confRef && "Member does not conform to 'AdditiveArithmetic'");
    // If conformance reference is not concrete, then concrete witness
    // declaration for `zero` cannot be resolved. Return reference to `zero`
    // protocol requirement: this will be dynamically dispatched.
    if (!confRef->isConcrete()) {
      return new (C) MemberRefExpr(memberTypeExpr, SourceLoc(), zeroReq,
                                   DeclNameLoc(), /*Implicit*/ true);
    }
    // Otherwise, return reference to concrete witness declaration for `zero`.
    auto conf = confRef->getConcrete();
    auto zeroDecl = conf->getWitnessDecl(zeroReq, nullptr);
    return new (C) MemberRefExpr(memberTypeExpr, SourceLoc(), zeroDecl,
                                 DeclNameLoc(), /*Implicit*/ true);
  };

  // Create array of `member.zero` expressions.
  llvm::SmallVector<Expr *, 2> memberZeroExprs;
  llvm::SmallVector<Identifier, 2> memberNames;
  for (auto member : nominal->getStoredProperties()) {
    memberZeroExprs.push_back(createMemberZeroExpr(member));
    memberNames.push_back(member->getName());
  }
  // Call memberwise initialier with member zero expressions.
  auto *callExpr =
      CallExpr::createImplicit(C, initExpr, memberZeroExprs, memberNames);
  ASTNode returnStmt = new (C) ReturnStmt(SourceLoc(), callExpr, true);
  funcDecl->setBody(
      BraceStmt::create(C, SourceLoc(), returnStmt, SourceLoc(), true));
}

// Synthesize the static property declaration for `AdditiveArithmetic.zero`.
static ValueDecl *deriveAdditiveArithmetic_zero(DerivedConformance &derived) {
  auto nominal = derived.Nominal;
  auto &TC = derived.TC;
  auto &C = TC.Context;

  // The implicit memberwise constructor must be explicitly created so that it
  // can called when synthesizing the `zero` property getter. Normally, the
  // memberwise constructor is synthesized during SILGen, which is too late.
  if (!getMemberwiseInitializer(nominal)) {
    auto *initDecl = createImplicitConstructor(
        TC, nominal, ImplicitConstructorKind::Memberwise);
    nominal->addMember(initDecl);
    C.addSynthesizedDecl(initDecl);
  }

  auto returnInterfaceTy = nominal->getDeclaredInterfaceType();
  auto returnTy =
      derived.getConformanceContext()->mapTypeIntoContext(returnInterfaceTy);

  // Create `zero` static property declaration.
  VarDecl *zeroDecl;
  PatternBindingDecl *pbDecl;
  std::tie(zeroDecl, pbDecl) = derived.declareDerivedProperty(
      C.Id_zero, returnInterfaceTy, returnTy, /*isStatic*/ true,
      /*isFinal*/ true);

  // Create `zero` getter.
  auto *getterDecl =
      derived.declareDerivedPropertyGetter(TC, zeroDecl, returnTy);
  getterDecl->setBodySynthesizer(deriveBodyAdditiveArithmetic_zero);
  zeroDecl->setAccessors(StorageImplInfo::getImmutableComputed(), SourceLoc(),
                         {getterDecl}, SourceLoc());
  derived.addMembersToConformanceContext({getterDecl, zeroDecl, pbDecl});

  return zeroDecl;
}

ValueDecl *
DerivedConformance::deriveAdditiveArithmetic(ValueDecl *requirement) {
  if (requirement->getBaseName() == TC.Context.getIdentifier("+")) {
    return deriveMathOperator(*this, Add);
  }
  if (requirement->getBaseName() == TC.Context.getIdentifier("-")) {
    return deriveMathOperator(*this, Subtract);
  }
  if (requirement->getBaseName() == TC.Context.Id_zero) {
    return deriveAdditiveArithmetic_zero(*this);
  }
  TC.diagnose(requirement->getLoc(),
              diag::broken_additive_arithmetic_requirement);
  return nullptr;
}

ValueDecl *DerivedConformance::deriveVectorNumeric(ValueDecl *requirement) {
  if (requirement->getBaseName() == TC.Context.getIdentifier("*")) {
    return deriveMathOperator(*this, ScalarMultiply);
  }
  TC.diagnose(requirement->getLoc(), diag::broken_vector_numeric_requirement);
  return nullptr;
}

Type DerivedConformance::deriveVectorNumeric(AssociatedTypeDecl *requirement) {
  if (requirement->getBaseName() == TC.Context.Id_Scalar) {
    return deriveVectorNumeric_Scalar(Nominal, getConformanceContext());
  }
  TC.diagnose(requirement->getLoc(), diag::broken_vector_numeric_requirement);
  return nullptr;
}
