//===--- DerivedConformanceMathProtocols.cpp ------------------------------===//
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
// This file implements explicit derivation of mathematical protocols for struct
// types: AdditiveArithmetic and PointwiseMultiplicative.
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
  // `+(Self, Self)`: AdditiveArithmetic
  Add,
  // `-(Self, Self)`: AdditiveArithmetic
  Subtract,
  // `.*(Self, Self)`: PointwiseMultiplicative
  Multiply
};

static StringRef getMathOperatorName(MathOperator op) {
  switch (op) {
  case Add:
    return "+";
  case Subtract:
    return "-";
  case Multiply:
    return ".*";
  }
}

static KnownProtocolKind getKnownProtocolKind(MathOperator op) {
  switch (op) {
  case Add:
  case Subtract:
    return KnownProtocolKind::AdditiveArithmetic;
  case Multiply:
    return KnownProtocolKind::PointwiseMultiplicative;
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

// Get the effective memberwise initializer of the given nominal type, or create
// it if it does not exist.
static ConstructorDecl *getOrCreateEffectiveMemberwiseInitializer(
    TypeChecker &TC, NominalTypeDecl *nominal) {
  auto &C = nominal->getASTContext();
  if (auto *initDecl = nominal->getEffectiveMemberwiseInitializer())
    return initDecl;
  auto *initDecl = createImplicitConstructor(
      TC, nominal, ImplicitConstructorKind::Memberwise);
  nominal->addMember(initDecl);
  C.addSynthesizedDecl(initDecl);
  return initDecl;
}

// Return true if given nominal type has a `let` stored with an initial value.
// TODO: Move function to shared place for use with other derived conformances.
static bool hasLetStoredPropertyWithInitialValue(NominalTypeDecl *nominal) {
  return llvm::any_of(nominal->getStoredProperties(), [&](VarDecl *v) {
    return v->isLet() && v->hasInitialValue();
  });
}

static bool canDeriveMathProtocol(KnownProtocolKind knownProtoKind,
                                  NominalTypeDecl *nominal, DeclContext *DC) {
  // Nominal type must be a struct. (No stored properties is okay.)
  auto *structDecl = dyn_cast<StructDecl>(nominal);
  if (!structDecl)
    return false;
  // Must not have any `let` stored properties with an initial value.
  // - This restriction may be lifted later with support for "true" memberwise
  //   initializers that initialize all stored properties, including initial
  //   value information.
  if (hasLetStoredPropertyWithInitialValue(nominal))
    return false;
  // All stored properties must conform to `AdditiveArithmetic`.
  auto &C = nominal->getASTContext();
  auto *proto = C.getProtocol(knownProtoKind);
  return llvm::all_of(structDecl->getStoredProperties(), [&](VarDecl *v) {
    if (!v->hasInterfaceType())
      C.getLazyResolver()->resolveDeclSignature(v);
    if (!v->hasInterfaceType())
      return false;
    auto varType = DC->mapTypeIntoContext(v->getValueInterfaceType());
    return (bool)TypeChecker::conformsToProtocol(varType, proto, DC, None);
  });
}

bool DerivedConformance::canDeriveAdditiveArithmetic(NominalTypeDecl *nominal,
                                                     DeclContext *DC) {
  return canDeriveMathProtocol(KnownProtocolKind::AdditiveArithmetic,
                               nominal, DC);
}

bool DerivedConformance::canDerivePointwiseMultiplicative(NominalTypeDecl *nominal,
                                                          DeclContext *DC) {
  return canDeriveMathProtocol(KnownProtocolKind::PointwiseMultiplicative,
                               nominal, DC);
}

// Synthesize body for math operator.
static void deriveBodyMathOperator(AbstractFunctionDecl *funcDecl,
                                   MathOperator op) {
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

  // Get operator protocol requirement.
  auto *proto = C.getProtocol(getKnownProtocolKind(op));
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
    auto memberType =
        parentDC->mapTypeIntoContext(member->getValueInterfaceType());
    auto confRef = module->lookupConformance(memberType, proto);
    assert(confRef && "Member does not conform to math protocol");

    // Get member type's math operator, e.g. `Member.+`.
    // Use protocol requirement declaration for the operator by default: this
    // will be dynamically dispatched.
    ValueDecl *memberOpDecl = operatorReq;
    // If conformance reference is concrete, then use concrete witness
    // declaration for the operator.
    if (confRef->isConcrete())
      if (auto *concreteMemberMethodDecl =
              confRef->getConcrete()->getWitnessDecl(operatorReq,
                                                     C.getLazyResolver()))
        memberOpDecl = concreteMemberMethodDecl;
    assert(memberOpDecl && "Member operator declaration must exist");
    auto memberOpDRE =
        new (C) DeclRefExpr(memberOpDecl, DeclNameLoc(), /*Implicit*/ true);
    auto *memberTypeExpr = TypeExpr::createImplicit(memberType, C);
    auto memberOpExpr =
        new (C) DotSyntaxCallExpr(memberOpDRE, SourceLoc(), memberTypeExpr);

    // Create expression `lhs.member <op> rhs.member`.
    Expr *lhsArg = new (C) MemberRefExpr(lhsDRE, SourceLoc(), member,
                                         DeclNameLoc(), /*Implicit*/ true);
    auto *rhsArg = new (C) MemberRefExpr(rhsDRE, SourceLoc(), member,
                                         DeclNameLoc(), /*Implicit*/ true);
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
  // Call memberwise initializer with member operator call expressions.
  auto *callExpr =
      CallExpr::createImplicit(C, initExpr, memberOpExprs, memberNames);
  ASTNode returnStmt = new (C) ReturnStmt(SourceLoc(), callExpr, true);
  funcDecl->setBody(
      BraceStmt::create(C, SourceLoc(), returnStmt, SourceLoc(), true));
}

// Synthesize function declaration for the given math operator.
static ValueDecl *deriveMathOperator(DerivedConformance &derived,
                                     MathOperator op) {
  auto nominal = derived.Nominal;
  auto parentDC = derived.getConformanceContext();
  auto &C = derived.TC.Context;
  auto selfInterfaceType = parentDC->getDeclaredInterfaceType();

  // Create parameter declaration with the given name and type.
  auto createParamDecl = [&](StringRef name, Type type) -> ParamDecl * {
    auto *param = new (C)
        ParamDecl(VarDecl::Specifier::Default, SourceLoc(), SourceLoc(),
                  Identifier(), SourceLoc(), C.getIdentifier(name), parentDC);
    param->setInterfaceType(type);
    return param;
  };

  ParameterList *params =
      ParameterList::create(C, {createParamDecl("lhs", selfInterfaceType),
                                createParamDecl("rhs", selfInterfaceType)});

  auto operatorId = C.getIdentifier(getMathOperatorName(op));
  DeclName operatorDeclName(C, operatorId, params);
  auto operatorDecl =
      FuncDecl::create(C, SourceLoc(), StaticSpellingKind::KeywordStatic,
                       SourceLoc(), operatorDeclName, SourceLoc(),
                       /*Throws*/ false, SourceLoc(),
                       /*GenericParams=*/nullptr, params,
                       TypeLoc::withoutLoc(selfInterfaceType), parentDC);
  operatorDecl->setImplicit();
  auto bodySynthesizer = [](AbstractFunctionDecl *funcDecl, void *ctx) {
    auto op = (MathOperator) reinterpret_cast<intptr_t>(ctx);
    deriveBodyMathOperator(funcDecl, op);
  };
  operatorDecl->setBodySynthesizer(bodySynthesizer, (void *) op);
  if (auto env = parentDC->getGenericEnvironmentOfContext())
    operatorDecl->setGenericEnvironment(env);
  operatorDecl->computeType();
  operatorDecl->copyFormalAccessFrom(nominal, /*sourceIsParentContext*/ true);
  operatorDecl->setValidationToChecked();

  derived.addMembersToConformanceContext({operatorDecl});
  C.addSynthesizedDecl(operatorDecl);

  return operatorDecl;
}

// Synthesize body for a computed property getter.
static void deriveBodyMathPropertyGetter(
    AbstractFunctionDecl *funcDecl, ProtocolDecl *proto, ValueDecl *reqDecl) {
  auto *parentDC = funcDecl->getParent();
  auto *nominal = parentDC->getSelfNominalTypeDecl();
  auto &C = nominal->getASTContext();

  auto *memberwiseInitDecl = nominal->getEffectiveMemberwiseInitializer();
  assert(memberwiseInitDecl && "Memberwise initializer must exist");
  auto *initDRE =
      new (C) DeclRefExpr(memberwiseInitDecl, DeclNameLoc(), /*Implicit*/ true);
  initDRE->setFunctionRefKind(FunctionRefKind::SingleApply);

  auto *nominalTypeExpr = TypeExpr::createForDecl(SourceLoc(), nominal,
                                                  funcDecl, /*Implicit*/ true);
  auto *initExpr = new (C) ConstructorRefCallExpr(initDRE, nominalTypeExpr);

  auto createMemberMathPropertyExpr = [&](VarDecl *member) -> Expr * {
    auto memberType =
        parentDC->mapTypeIntoContext(member->getValueInterfaceType());
    Expr *memberExpr = nullptr;
    // If the property is static, create a type expression: `Member`.
    if (reqDecl->isStatic()) {
      memberExpr = TypeExpr::createImplicit(memberType, C);
    }
    // If the property is not static, create a member ref expression:
    // `self.member`.
    else {
      auto *selfDecl = funcDecl->getImplicitSelfDecl();
      auto *selfDRE =
          new (C) DeclRefExpr(selfDecl, DeclNameLoc(), /*Implicit*/ true);
      memberExpr =
         new (C) MemberRefExpr(selfDRE, SourceLoc(), member, DeclNameLoc(),
                               /*Implicit*/ true);
    }
    auto module = nominal->getModuleContext();
    auto confRef = module->lookupConformance(memberType, proto);
    assert(confRef && "Member does not conform to math protocol");
    // If conformance reference is not concrete, then concrete witness
    // declaration for property cannot be resolved. Return reference to
    // protocol requirement: this will be dynamically dispatched.
    if (!confRef->isConcrete()) {
      return new (C) MemberRefExpr(memberExpr, SourceLoc(), reqDecl,
                                   DeclNameLoc(), /*Implicit*/ true);
    }
    // Otherwise, return reference to concrete witness declaration.
    auto conf = confRef->getConcrete();
    auto witnessDecl = conf->getWitnessDecl(reqDecl, C.getLazyResolver());
    return new (C) MemberRefExpr(memberExpr, SourceLoc(), witnessDecl,
                                 DeclNameLoc(), /*Implicit*/ true);
  };

  // Create array of `member.<property>` expressions.
  llvm::SmallVector<Expr *, 2> memberPropExprs;
  llvm::SmallVector<Identifier, 2> memberNames;
  for (auto member : nominal->getStoredProperties()) {
    memberPropExprs.push_back(createMemberMathPropertyExpr(member));
    memberNames.push_back(member->getName());
  }
  // Call memberwise initializer with member property expressions.
  auto *callExpr =
      CallExpr::createImplicit(C, initExpr, memberPropExprs, memberNames);
  ASTNode returnStmt = new (C) ReturnStmt(SourceLoc(), callExpr, true);
  funcDecl->setBody(
      BraceStmt::create(C, SourceLoc(), returnStmt, SourceLoc(), true));
}

// Synthesize body for the `AdditiveArithmetic.zero` computed property getter.
static void deriveBodyAdditiveArithmetic_zero(AbstractFunctionDecl *funcDecl,
                                              void *) {
  auto &C = funcDecl->getASTContext();
  auto *addArithProto = C.getProtocol(KnownProtocolKind::AdditiveArithmetic);
  auto *zeroReq = getProtocolRequirement(addArithProto, C.Id_zero);
  deriveBodyMathPropertyGetter(funcDecl, addArithProto, zeroReq);
}

// Synthesize body for the `PointwiseMultiplicative.reciprocal` computed
// property getter.
static void
deriveBodyPointwiseMultiplicative_reciprocal(AbstractFunctionDecl *funcDecl,
                                             void *) {
  auto &C = funcDecl->getASTContext();
  auto *pointMulProto =
      C.getProtocol(KnownProtocolKind::PointwiseMultiplicative);
  auto *reciprocalReq = getProtocolRequirement(pointMulProto, C.Id_reciprocal);
  deriveBodyMathPropertyGetter(funcDecl, pointMulProto, reciprocalReq);
}

// Synthesize a protocol property declaration.
static ValueDecl *
deriveMathProperty(DerivedConformance &derived, Identifier propertyName,
                   bool isStatic,
                   AbstractFunctionDecl::BodySynthesizer bodySynthesizer) {
  auto *nominal = derived.Nominal;
  auto *parentDC = derived.getConformanceContext();
  auto &TC = derived.TC;

  auto returnInterfaceTy = nominal->getDeclaredInterfaceType();
  auto returnTy = parentDC->mapTypeIntoContext(returnInterfaceTy);

  // Create property declaration.
  VarDecl *propDecl;
  PatternBindingDecl *pbDecl;
  std::tie(propDecl, pbDecl) = derived.declareDerivedProperty(
      propertyName, returnInterfaceTy, returnTy, /*isStatic*/ isStatic,
      /*isFinal*/ true);

  // Create property getter.
  auto *getterDecl =
      derived.declareDerivedPropertyGetter(TC, propDecl, returnTy);
  getterDecl->setBodySynthesizer(bodySynthesizer.Fn, bodySynthesizer.Context);
  propDecl->setAccessors(StorageImplInfo::getImmutableComputed(), SourceLoc(),
                         {getterDecl}, SourceLoc());
  derived.addMembersToConformanceContext({getterDecl, propDecl, pbDecl});

  return propDecl;
}

// Synthesize the static property declaration for `AdditiveArithmetic.zero`.
static ValueDecl *deriveAdditiveArithmetic_zero(DerivedConformance &derived) {
  auto &C = derived.TC.Context;
  return deriveMathProperty(derived, C.Id_zero, /*isStatic*/ true,
                            {deriveBodyAdditiveArithmetic_zero, nullptr});
}

// Synthesize the instance property declaration for
// `PointwiseMultiplicative.reciprocal`.
static ValueDecl *
derivePointwiseMultiplicative_reciprocal(DerivedConformance &derived) {
  auto &C = derived.TC.Context;
  return deriveMathProperty(
      derived, C.Id_reciprocal, /*isStatic*/ false,
      {deriveBodyPointwiseMultiplicative_reciprocal, nullptr});
}

ValueDecl *
DerivedConformance::deriveAdditiveArithmetic(ValueDecl *requirement) {
  // Diagnose conformances in disallowed contexts.
  if (checkAndDiagnoseDisallowedContext(requirement))
    return nullptr;
  // Create memberwise initializer for nominal type if it doesn't already exist.
  getOrCreateEffectiveMemberwiseInitializer(TC, Nominal);
  if (requirement->getBaseName() == TC.Context.getIdentifier("+"))
    return deriveMathOperator(*this, Add);
  if (requirement->getBaseName() == TC.Context.getIdentifier("-"))
    return deriveMathOperator(*this, Subtract);
  if (requirement->getBaseName() == TC.Context.Id_zero)
    return deriveAdditiveArithmetic_zero(*this);
  TC.diagnose(requirement->getLoc(),
              diag::broken_additive_arithmetic_requirement);
  return nullptr;
}

ValueDecl *
DerivedConformance::derivePointwiseMultiplicative(ValueDecl *requirement) {
  // Diagnose conformances in disallowed contexts.
  if (checkAndDiagnoseDisallowedContext(requirement))
    return nullptr;
  // Create memberwise initializer for nominal type if it doesn't already exist.
  getOrCreateEffectiveMemberwiseInitializer(TC, Nominal);
  if (requirement->getBaseName() == TC.Context.getIdentifier(".*"))
    return deriveMathOperator(*this, Multiply);
  if (requirement->getBaseName() == TC.Context.Id_reciprocal)
    return derivePointwiseMultiplicative_reciprocal(*this);
  TC.diagnose(requirement->getLoc(),
              diag::broken_pointwise_multiplicative_requirement);
  return nullptr;
}
