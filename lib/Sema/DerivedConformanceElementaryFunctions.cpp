//===--- DerivedConformanceElementaryFunctions.cpp ------------------------===//
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
// This file implements explicit derivation of the ElementaryFunctions protocol
// for struct types.
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

// Represents synthesizable `ElementaryFunction` protocol requirements.
enum ElementaryFunction {
#define ELEMENTARY_FUNCTION(ID, NAME) ID,
#include "DerivedConformanceElementaryFunctions.def"
#undef ELEMENTARY_FUNCTION
};

static StringRef getElementaryFunctionName(ElementaryFunction op) {
  switch (op) {
#define ELEMENTARY_FUNCTION(ID, NAME) case ElementaryFunction::ID: return NAME;
#include "DerivedConformanceElementaryFunctions.def"
#undef ELEMENTARY_FUNCTION
  }
}

// Return the protocol requirement with the specified name.
// TODO: Move function to shared place for use with other derived conformances.
static ValueDecl *getProtocolRequirement(ProtocolDecl *proto, Identifier name) {
  auto lookup = proto->lookupDirect(name);
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

// Return the `ElementaryFunction` protocol requirement corresponding to the
// given elementary function.
static ValueDecl *getElementaryFunctionRequirement(
    ASTContext &C, ElementaryFunction op) {
  auto *mathProto = C.getProtocol(KnownProtocolKind::ElementaryFunctions);
  auto operatorId = C.getIdentifier(getElementaryFunctionName(op));
  switch (op) {
#define ELEMENTARY_FUNCTION_UNARY(ID, NAME) \
  case ID: \
    return getProtocolRequirement(mathProto, operatorId);
#include "DerivedConformanceElementaryFunctions.def"
#undef ELEMENTARY_FUNCTION_UNARY
  case Root:
    return getProtocolRequirement(mathProto, operatorId);
  case Pow:
  case PowInt:
    auto lookup = mathProto->lookupDirect(operatorId);
    lookup.erase(std::remove_if(lookup.begin(), lookup.end(),
                                [](ValueDecl *v) {
                                  return !isa<ProtocolDecl>(
                                             v->getDeclContext()) ||
                                         !v->isProtocolRequirement();
                                }),
                 lookup.end());
    assert(lookup.size() == 2 && "Expected two 'pow' functions");
    auto *powFuncDecl = cast<FuncDecl>(lookup.front());
    auto secondParamType =
        powFuncDecl->getParameters()->get(1)->getInterfaceType();
    if (secondParamType->getAnyNominal() == C.getIntDecl())
      return op == PowInt ? lookup.front() : lookup[1];
    else
      return op == PowInt ? lookup[1] : lookup.front();
  }
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

bool DerivedConformance::canDeriveElementaryFunctions(NominalTypeDecl *nominal,
                                                      DeclContext *DC) {
  // Nominal type must be a struct. (Zero stored properties is okay.)
  auto *structDecl = dyn_cast<StructDecl>(nominal);
  if (!structDecl)
    return false;
  // Must not have any `let` stored properties with an initial value.
  // - This restriction may be lifted later with support for "true" memberwise
  //   initializers that initialize all stored properties, including initial
  //   value information.
  if (hasLetStoredPropertyWithInitialValue(nominal))
    return false;
  // All stored properties must conform to `ElementaryFunctions`.
  auto &C = nominal->getASTContext();
  auto *mathProto = C.getProtocol(KnownProtocolKind::ElementaryFunctions);
  return llvm::all_of(structDecl->getStoredProperties(), [&](VarDecl *v) {
    if (!v->hasInterfaceType())
      C.getLazyResolver()->resolveDeclSignature(v);
    if (!v->hasInterfaceType())
      return false;
    auto varType = DC->mapTypeIntoContext(v->getValueInterfaceType());
    return (bool)TypeChecker::conformsToProtocol(varType, mathProto, DC, None);
  });
}

// Synthesize body for the given `ElementaryFunction` protocol requirement.
static std::pair<BraceStmt *, bool>
deriveBodyElementaryFunction(AbstractFunctionDecl *funcDecl,
                             ElementaryFunction op) {
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
  auto *mathProto = C.getProtocol(KnownProtocolKind::ElementaryFunctions);
  auto *operatorReq = getElementaryFunctionRequirement(C, op);

  // Create reference(s) to operator parameters: one for unary functions and two
  // for binary functions.
  auto params = funcDecl->getParameters();
  auto *firstParamDRE =
      new (C) DeclRefExpr(params->get(0), DeclNameLoc(), /*Implicit*/ true);
  Expr *secondParamDRE = nullptr;
  if (params->size() == 2)
    secondParamDRE =
        new (C) DeclRefExpr(params->get(1), DeclNameLoc(), /*Implicit*/ true);

  // Create call expression combining lhs and rhs members using member operator.
  auto createMemberOpCallExpr = [&](VarDecl *member) -> Expr * {
    auto module = nominal->getModuleContext();
    auto memberType =
        parentDC->mapTypeIntoContext(member->getValueInterfaceType());
    auto confRef = module->lookupConformance(memberType, mathProto);
    assert(confRef && "Member does not conform to math protocol");

    // Get member type's elementary function, e.g. `Member.cos`.
    // Use protocol requirement declaration for the operator by default: this
    // will be dynamically dispatched.
    ValueDecl *memberOpDecl = operatorReq;
    // If conformance reference is concrete, then use concrete witness
    // declaration for the operator.
    if (confRef->isConcrete())
      memberOpDecl = confRef->getConcrete()->getWitnessDecl(
          operatorReq);
    assert(memberOpDecl && "Member operator declaration must exist");
    auto memberOpDRE =
        new (C) DeclRefExpr(memberOpDecl, DeclNameLoc(), /*Implicit*/ true);
    auto *memberTypeExpr = TypeExpr::createImplicit(memberType, C);
    auto memberOpExpr =
        new (C) DotSyntaxCallExpr(memberOpDRE, SourceLoc(), memberTypeExpr);

    // - For unary ops, create expression:
    //   `<op>(x.member)`.
    // - For `pow(_ x: Self, _ y: Self)`, create expression:
    //   `<op>(x.member, y.member)`.
    // - For `pow(_ x: Self, _ n: Int)` and `root(_ x: Self, n: Int)`, create:
    //   `<op>(x.member, n)`.
    Expr *firstArg = new (C) MemberRefExpr(firstParamDRE, SourceLoc(), member,
                                         DeclNameLoc(), /*Implicit*/ true);
    Expr *secondArg = nullptr;
    if (secondParamDRE) {
      if (op == PowInt || op == Root)
        secondArg = secondParamDRE;
      else
        secondArg = new (C) MemberRefExpr(secondParamDRE, SourceLoc(), member,
                                          DeclNameLoc(), /*Implicit*/ true);
    }
    SmallVector<Expr *, 2> memberOpArgs{firstArg};
    if (secondArg)
      memberOpArgs.push_back(secondArg);
    SmallVector<Identifier, 2> memberOpArgLabels(memberOpArgs.size());
    auto *memberOpCallExpr = CallExpr::createImplicit(
        C, memberOpExpr, memberOpArgs, memberOpArgLabels);
    return memberOpCallExpr;
  };

  // Create array of member operator call expressions.
  llvm::SmallVector<Expr *, 2> memberOpCallExprs;
  llvm::SmallVector<Identifier, 2> memberNames;
  for (auto member : nominal->getStoredProperties()) {
    memberOpCallExprs.push_back(createMemberOpCallExpr(member));
    memberNames.push_back(member->getName());
  }
  // Call memberwise initializer with member operator call expressions.
  auto *callExpr =
      CallExpr::createImplicit(C, initExpr, memberOpCallExprs, memberNames);
  ASTNode returnStmt = new (C) ReturnStmt(SourceLoc(), callExpr, true);
  auto* braceStmt = BraceStmt::create(C, SourceLoc(), returnStmt, SourceLoc(), true);
  return std::pair<BraceStmt *, bool>(braceStmt, false);
}

#define ELEMENTARY_FUNCTION(ID, NAME)                                   \
static std::pair<BraceStmt *, bool> deriveBodyElementaryFunctions_##ID( \
  AbstractFunctionDecl *funcDecl, void *) {                             \
  return deriveBodyElementaryFunction(funcDecl, ID);                    \
}
#include "DerivedConformanceElementaryFunctions.def"
#undef ELEMENTARY_FUNCTION

// Synthesize function declaration for the given math operator.
static ValueDecl *deriveElementaryFunction(DerivedConformance &derived,
ElementaryFunction op) {
  auto nominal = derived.Nominal;
  auto parentDC = derived.getConformanceContext();
  auto &C = derived.TC.Context;
  auto selfInterfaceType = parentDC->getDeclaredInterfaceType();

  // Create parameter declaration with the given name and type.
  auto createParamDecl = [&](StringRef name, Type type) -> ParamDecl * {
    auto *param = new (C)
        ParamDecl(ParamDecl::Specifier::Default, SourceLoc(), SourceLoc(),
                  Identifier(), SourceLoc(), C.getIdentifier(name), parentDC);
    param->setInterfaceType(type);
    return param;
  };

  ParameterList *params = nullptr;

  switch (op) {
#define ELEMENTARY_FUNCTION_UNARY(ID, NAME)                                    \
  case ID:                                                                     \
    params =                                                                   \
        ParameterList::create(C, {createParamDecl("x", selfInterfaceType)});   \
    break;
#include "DerivedConformanceElementaryFunctions.def"
#undef ELEMENTARY_FUNCTION_UNARY
  case Pow:
    params =
        ParameterList::create(C, {createParamDecl("x", selfInterfaceType),
                                  createParamDecl("y", selfInterfaceType)});
    break;
  case PowInt:
  case Root:
    params = ParameterList::create(
         C, {createParamDecl("x", selfInterfaceType),
             createParamDecl("n", C.getIntDecl()->getDeclaredInterfaceType())});
      break;
  }

  auto operatorId = C.getIdentifier(getElementaryFunctionName(op));
  DeclName operatorDeclName(C, operatorId, params);
  auto operatorDecl =
      FuncDecl::create(C, SourceLoc(), StaticSpellingKind::KeywordStatic,
                       SourceLoc(), operatorDeclName, SourceLoc(),
                       /*Throws*/ false, SourceLoc(),
                       /*GenericParams*/ nullptr, params,
                       TypeLoc::withoutLoc(selfInterfaceType), parentDC);
  operatorDecl->setImplicit();
  switch (op) {
#define ELEMENTARY_FUNCTION(ID, NAME)                                          \
  case ID:                                                                     \
    operatorDecl->setBodySynthesizer(deriveBodyElementaryFunctions_##ID,       \
                                     nullptr);                                 \
    break;
#include "DerivedConformanceElementaryFunctions.def"
#undef ELEMENTARY_FUNCTION
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

ValueDecl *
DerivedConformance::deriveElementaryFunctions(ValueDecl *requirement) {
  // Diagnose conformances in disallowed contexts.
  if (checkAndDiagnoseDisallowedContext(requirement))
    return nullptr;
  // Create memberwise initializer for nominal type if it doesn't already exist.
  getOrCreateEffectiveMemberwiseInitializer(TC, Nominal);
#define ELEMENTARY_FUNCTION_UNARY(ID, NAME)                                    \
  if (requirement->getBaseName() == TC.Context.getIdentifier(NAME))            \
    return deriveElementaryFunction(*this, ID);
#include "DerivedConformanceElementaryFunctions.def"
#undef ELEMENTARY_FUNCTION_UNARY
  if (requirement->getBaseName() == TC.Context.getIdentifier("root"))
    return deriveElementaryFunction(*this, Root);
  if (requirement->getBaseName() == TC.Context.getIdentifier("pow")) {
    auto *powFuncDecl = cast<FuncDecl>(requirement);
    return powFuncDecl->getParameters()->get(1)->getName().str() == "n"
        ? deriveElementaryFunction(*this, PowInt)
        : deriveElementaryFunction(*this, Pow);
  }
  TC.diagnose(requirement->getLoc(),
              diag::broken_elementary_functions_requirement);
  return nullptr;
}
