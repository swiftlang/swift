//===--- DerivedConformanceAdditiveArithmetic.cpp -------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2018 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file implements explicit derivation of the AdditiveArithmetic protocol
// for struct types.
//
// Currently, this is gated by a frontend flag:
// `-enable-experimental-additive-arithmetic-derivation`.
//
// Swift Evolution pitch thread:
// https://forums.swift.org/t/additivearithmetic-conformance-synthesis-for-structs/26159
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
};

static StringRef getMathOperatorName(MathOperator op) {
  switch (op) {
  case Add:
    return "+";
  case Subtract:
    return "-";
  }
  llvm_unreachable("invalid math operator kind");
}

static ValueDecl *getFatalErrorFn(ASTContext &C) {
  auto fatalErrorLookupRes = llvm::SmallVector<ValueDecl*, 1>{};
  C.lookupInSwiftModule("fatalError", fatalErrorLookupRes);
  assert((fatalErrorLookupRes.size() == 1) && "Exactly one fatalError Decl must exist");
  return fatalErrorLookupRes.front();
}

bool DerivedConformance::canDeriveAdditiveArithmetic(NominalTypeDecl *nominal,
                                                     DeclContext *DC) {
  // Experimental `AdditiveArithmetic` derivation must be enabled.
  if (auto *SF = DC->getParentSourceFile())
    if (!isAdditiveArithmeticConformanceDerivationEnabled(*SF))
      return false;
  // Nominal type must be an enum or struct.
  auto &C = nominal->getASTContext();
  auto *proto = C.getProtocol(KnownProtocolKind::AdditiveArithmetic);

  if (auto *enumDecl = dyn_cast<EnumDecl>(nominal))
    return DerivedConformance::allAssociatedValuesConformToProtocol(DC, enumDecl, proto);

  if (auto *structDecl = dyn_cast<StructDecl>(nominal)) {
    // Must not have any `let` stored properties with an initial value.
    // - This restriction may be lifted later with support for "true" memberwise
    //   initializers that initialize all stored properties, including initial
    //   value information.
    if (hasLetStoredPropertyWithInitialValue(nominal))
      return false;
    // All stored properties must conform to `AdditiveArithmetic`.
    return llvm::all_of(structDecl->getStoredProperties(), [&](VarDecl *v) {
      if (v->getInterfaceType()->hasError())
        return false;
      auto varType = DC->mapTypeIntoContext(v->getValueInterfaceType());
      return (bool)TypeChecker::conformsToProtocol(varType, proto, DC);
    });
  }
  return false;
}

// E.g. 'ValueType.+'
static MemberRefExpr *getMathOperatorForValue(VarDecl *value, MathOperator op,
                                              DeclContext *parentDC) {
  auto nominal = parentDC->getSelfNominalTypeDecl();
  auto &C = nominal->getASTContext();

  // Get operator protocol requirement.
  auto proto = C.getProtocol(KnownProtocolKind::AdditiveArithmetic);
  auto operatorId = C.getIdentifier(getMathOperatorName(op));
  auto operatorReq = getProtocolRequirement(proto, operatorId);

  auto module = nominal->getModuleContext();
  auto valueType =
    parentDC->mapTypeIntoContext(value->getValueInterfaceType());

  auto confRef = module->lookupConformance(valueType, proto);
  assert(confRef && "Value does not conform to math protocol");

  // Get value type's math operator, e.g. `ValueType.+`.
  // Use protocol requirement declaration for the operator by default: this
  // will be dynamically dispatched.
  ValueDecl *valueOpDecl = operatorReq;

  // If conformance reference is concrete, then use concrete witness
  // declaration for the operator.
  if (confRef.isConcrete())
    if (auto *concreteValueMethodDecl =
            confRef.getConcrete()->getWitnessDecl(operatorReq))
      valueOpDecl = concreteValueMethodDecl;
  assert(valueOpDecl && "Value operator declaration must exist");
  auto *valueTypeExpr = TypeExpr::createImplicit(valueType, C);
  auto memberOpExpr =
      new (C) MemberRefExpr(valueTypeExpr, SourceLoc(), valueOpDecl,
                            DeclNameLoc(), /*Implicit*/ true);
  return memberOpExpr;
}

// Synthesize body for math operator for an enum.
static std::pair<BraceStmt *, bool>
deriveBodyMathOperator_enum(AbstractFunctionDecl *funcDecl, MathOperator op) {
  auto parentDC = funcDecl->getParent();
  auto nominal = parentDC->getSelfNominalTypeDecl();
  auto enumDecl = cast<EnumDecl>(nominal);
  auto &C = nominal->getASTContext();

  // 'lhs' and 'rhs' parameters.
  auto params = funcDecl->getParameters();
  auto lhs = params->get(0);
  auto rhs = params->get(1);

  auto enumType = lhs->getType();

  // Create case statement for every enum element:
  // 'case (.<elem>(let l0, let l1, ...), .<elem>(let r0, let r1, ...)):
  //   return .<elem>(l0 + r0, l1 + r1, ...)'
  auto cases = llvm::SmallVector<ASTNode, 6>{};
  auto createCaseForEnumElement = [&] (EnumElementDecl *elemDecl) {
    // Create '.<elem>(let l0, let l1, ...)' pattern
    auto lhsPayloadVars = llvm::SmallVector<VarDecl*, 3>{};
    auto lhsSubpattern = DerivedConformance::enumElementPayloadSubpattern(elemDecl, 'l', funcDecl,
                                                                          lhsPayloadVars);
    auto lhsElemPat = new (C) EnumElementPattern(TypeExpr::createImplicit(enumType, C),
                                                  SourceLoc(), DeclNameLoc(), DeclNameRef(),
                                                  elemDecl, lhsSubpattern);
    lhsElemPat->setImplicit();

    auto numAsocVars = lhsPayloadVars.size();

    // Create '.<elem>(let r0, let r1, ...)' pattern
    auto rhsPayloadVars = SmallVector<VarDecl*, 3>{};
    auto rhsSubpattern = DerivedConformance::enumElementPayloadSubpattern(elemDecl, 'r', funcDecl,
                                                                          rhsPayloadVars);
    auto rhsElemPat = new (C) EnumElementPattern(TypeExpr::createImplicit(enumType, C),
                                                  SourceLoc(), DeclNameLoc(),
                                                  DeclNameRef(), elemDecl, rhsSubpattern);
    rhsElemPat->setImplicit();

    assert((numAsocVars == rhsPayloadVars.size()) && "Lhs and Rhs must have same size");

    // Create 'case (.<elem>(let l0, let l1, ...), .<elem>(let r0, let r1, ...)):' label
    auto patternElements = { TuplePatternElt(lhsElemPat), TuplePatternElt(rhsElemPat) };
    auto caseTuplePattern = TuplePattern::createImplicit(C, patternElements);
    caseTuplePattern->setImplicit();
    auto labelItem = CaseLabelItem(caseTuplePattern);

    // Create <op>s of associated values: 'l0 <op> r0', 'l1 <op> r1', ...
    auto asocValExprs = llvm::SmallVector<Expr*, 3>{};
    // Creates 'li <op> ri' expr
    auto createOpExpr = [&] (auto lhsAndRhsDecls) {
      VarDecl *lhsVal;
      VarDecl *rhsVal;
      std::tie(lhsVal, rhsVal) = lhsAndRhsDecls;
      auto lhsExpr = new (C) DeclRefExpr(lhsVal, DeclNameLoc(),
                                                        /*implicit*/true);
      auto rhsExpr = new (C) DeclRefExpr(rhsVal, DeclNameLoc(),
                                        /*implicit*/true);
      // Get 'ValueType.+' operator
      auto opExpr = getMathOperatorForValue(lhsVal, op, parentDC);

      auto opArgs = TupleExpr::create(C, SourceLoc(), {lhsExpr, rhsExpr},
                                      {}, {}, SourceLoc(),
                                      /*hasTrailingClosure*/ false,
                                      /*implicit*/ true);
      return new (C) BinaryExpr(opExpr, opArgs, /*implicit*/ true);
    };
    llvm::transform(llvm::zip(lhsPayloadVars, rhsPayloadVars),
                    std::back_inserter(asocValExprs),
                    createOpExpr);

    // Create '.<elem>(l0 <op> r0, l1 <op> r1, ...)' expr for return
    auto elemRef = new (C) DeclRefExpr(elemDecl, DeclNameLoc(), /*Implicit*/ true);
    elemRef->setFunctionRefKind(FunctionRefKind::SingleApply);
    auto nominalTypeExpr = TypeExpr::createImplicitForDecl(
        DeclNameLoc(), nominal, funcDecl,
        funcDecl->mapTypeIntoContext(nominal->getInterfaceType()));
    // '.<elem>' expr
    auto dotExpr = new (C) MemberRefExpr(nominalTypeExpr, SourceLoc(), elemDecl, DeclNameLoc(), true);
    Expr *enumInit;
    if (numAsocVars > 0)
      // '.<elem>(...)'
      enumInit = CallExpr::createImplicit(C, dotExpr, asocValExprs, {});
    else
      // Use '.<elem>' instead of (incorrect) '.<elem>()' when the element
      // has no associated values
      enumInit = dotExpr;
    auto returnStmt = new (C) ReturnStmt(SourceLoc(), enumInit, true);
    auto caseBody = BraceStmt::create(C, SourceLoc(), ASTNode(returnStmt), SourceLoc(), true);

    // Copy all pattern vars ('l0', 'l1', ..., 'r0', 'r1', ...) for CaseStmt
    auto caseBodyVarDecls = C.Allocate<VarDecl *>(2 * numAsocVars);
    auto copyDecl = [&] (VarDecl *oldDecl) {
      auto *newDecl = new (C) VarDecl(/*IsStatic*/ false, oldDecl->getIntroducer(),
                                      /*IsCaptureList*/ false, oldDecl->getNameLoc(),
                                      oldDecl->getName(), oldDecl->getDeclContext());
      newDecl->setHasNonPatternBindingInit();
      newDecl->setImplicit();
      return newDecl;
    };
    llvm::transform(llvm::concat<VarDecl *>(lhsPayloadVars, rhsPayloadVars),
                    std::begin(caseBodyVarDecls),
                    copyDecl);

    return CaseStmt::create(C, CaseParentKind::Switch, SourceLoc(),
                            labelItem, SourceLoc(), SourceLoc(), caseBody, caseBodyVarDecls);
  };
  llvm::transform(enumDecl->getAllElements(),
                  std::back_inserter(cases),
                  createCaseForEnumElement);

  // Create 'default: fatalError()' case for mismatching enum values
  // (only necessary if the enum has more than one element)
  if (cases.size() > 1) {
    auto defaultPattern = AnyPattern::createImplicit(C);
    auto defaultItem = CaseLabelItem::getDefault(defaultPattern);
    auto fatalErrorDecl = getFatalErrorFn(C);
    auto fatalErrorDRE = new (C) DeclRefExpr(fatalErrorDecl, DeclNameLoc(),
                                             /*implicit*/ true);
    auto errorMsg = new (C) StringLiteralExpr("Enum cases mismatch", SourceRange(),
                                              /*implicit*/ true);
    auto callExpr = CallExpr::createImplicit(C, fatalErrorDRE, {errorMsg}, {});
    auto body = BraceStmt::create(C, SourceLoc(), ASTNode(callExpr),
                                  SourceLoc());
    cases.push_back(CaseStmt::create(C, CaseParentKind::Switch, SourceLoc(),
                    defaultItem, SourceLoc(), SourceLoc(),
                    body, /*case body var decls*/ None));
  }

  // Create 'switch (lhs, rhs) { <case statements> }'
  auto lhsRef = new (C) DeclRefExpr(lhs, DeclNameLoc(), /*implicit*/true);
  auto rhsRef = new (C) DeclRefExpr(rhs, DeclNameLoc(), /*implicit*/true);
  auto lhsRhsExpr = TupleExpr::create(C, SourceLoc(), { lhsRef, rhsRef }, {}, {},
                                      SourceLoc(), /*hasTrailingClosure*/ false,
                                      /*implicit*/ true);
  auto switchStmt = SwitchStmt::create(LabeledStmtInfo(), SourceLoc(), lhsRhsExpr,
                                      SourceLoc(), cases, SourceLoc(), C);

  auto body = BraceStmt::create(C, SourceLoc(), ASTNode(switchStmt), SourceLoc());
  return { body, false };
}

// Synthesize body for math operator.
static std::pair<BraceStmt *, bool>
deriveBodyMathOperator_struct(AbstractFunctionDecl *funcDecl, MathOperator op) {
  auto *parentDC = funcDecl->getParent();
  auto *nominal = parentDC->getSelfNominalTypeDecl();
  assert(isa<StructDecl>(nominal) && "Must be a struct");
  auto &C = nominal->getASTContext();

  // Create memberwise initializer: `Nominal.init(...)`.
  auto *memberwiseInitDecl = nominal->getEffectiveMemberwiseInitializer();
  assert(memberwiseInitDecl && "Memberwise initializer must exist");
  auto *initDRE =
      new (C) DeclRefExpr(memberwiseInitDecl, DeclNameLoc(), /*Implicit*/ true);
  initDRE->setFunctionRefKind(FunctionRefKind::SingleApply);
  auto *nominalTypeExpr = TypeExpr::createImplicitForDecl(
      DeclNameLoc(), nominal, funcDecl,
      funcDecl->mapTypeIntoContext(nominal->getInterfaceType()));
  auto *initExpr = new (C) ConstructorRefCallExpr(initDRE, nominalTypeExpr);

  // Create reference to operator parameters: lhs and rhs.
  auto params = funcDecl->getParameters();

  // Create expression combining lhs and rhs members using member operator.
  auto createMemberOpExpr = [&](VarDecl *member) -> Expr * {
    // Get member type's math operator, e.g. `Member.+`.
    auto memberOpExpr = getMathOperatorForValue(member, op, parentDC);

    // Create expression `lhs.member <op> rhs.member`.
    // NOTE(TF-1054): create new `DeclRefExpr`s per loop iteration to avoid
    // `ConstraintSystem::resolveOverload` error.
    auto *lhsDRE =
        new (C) DeclRefExpr(params->get(0), DeclNameLoc(), /*Implicit*/ true);
    auto *rhsDRE =
        new (C) DeclRefExpr(params->get(1), DeclNameLoc(), /*Implicit*/ true);
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
  return std::pair<BraceStmt *, bool>(
      BraceStmt::create(C, SourceLoc(), returnStmt, SourceLoc(), true), false);
}

// Synthesize function declaration for the given math operator.
static ValueDecl *deriveMathOperator(DerivedConformance &derived,
                                     MathOperator op) {
  auto nominal = derived.Nominal;
  auto parentDC = derived.getConformanceContext();
  auto &C = derived.Context;
  auto selfInterfaceType = parentDC->getDeclaredInterfaceType();

  // Create parameter declaration with the given name and type.
  auto createParamDecl = [&](StringRef name, Type type) -> ParamDecl * {
    auto *param =
        new (C) ParamDecl(SourceLoc(), SourceLoc(), Identifier(), SourceLoc(),
                          C.getIdentifier(name), parentDC);
    param->setSpecifier(ParamDecl::Specifier::Default);
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
  if (isa<StructDecl>(nominal)) {
    auto bodySynthesizer = [](AbstractFunctionDecl *funcDecl,
                              void *ctx) -> std::pair<BraceStmt *, bool> {
      auto op = (MathOperator) reinterpret_cast<intptr_t>(ctx);
      return deriveBodyMathOperator_struct(funcDecl, op);
    };
    operatorDecl->setBodySynthesizer(bodySynthesizer, (void *)op);
  }
  else if (isa<EnumDecl>(nominal)) {
    auto bodySynthesizer = [](AbstractFunctionDecl *funcDecl,
                              void *ctx) -> std::pair<BraceStmt *, bool> {
      auto op = (MathOperator) reinterpret_cast<intptr_t>(ctx);
      return deriveBodyMathOperator_enum(funcDecl, op);
    };
    operatorDecl->setBodySynthesizer(bodySynthesizer, (void *)op);
  }
  else
    llvm_unreachable("Must be a struct or enum");
  operatorDecl->setGenericSignature(parentDC->getGenericSignatureOfContext());
  operatorDecl->copyFormalAccessFrom(nominal, /*sourceIsParentContext*/ true);

  derived.addMembersToConformanceContext({operatorDecl});
  return operatorDecl;
}

// Synthesize body for a property computed property getter.
static std::pair<BraceStmt *, bool>
deriveBodyPropertyGetter(AbstractFunctionDecl *funcDecl, ProtocolDecl *proto,
                         ValueDecl *reqDecl) {
  auto *parentDC = funcDecl->getParent();
  auto *nominal = parentDC->getSelfNominalTypeDecl();
  auto &C = nominal->getASTContext();

  if (isa<EnumDecl>(nominal)) {
    auto fatalErrorDecl = getFatalErrorFn(C);
    auto fatalErrorDRE = new (C) DeclRefExpr(fatalErrorDecl, DeclNameLoc(),
                                             /*implicit*/ true);
    auto errorMsg = new (C) StringLiteralExpr("Cannot create zero enum", SourceRange(),
                                              /*implicit*/ true);
    auto callExpr = CallExpr::createImplicit(C, fatalErrorDRE, {errorMsg}, {});
    auto braceStmt = BraceStmt::create(C, SourceLoc(), {callExpr}, SourceLoc(), true);
    return { braceStmt, false };
  }

  auto *memberwiseInitDecl = nominal->getEffectiveMemberwiseInitializer();
  assert(memberwiseInitDecl && "Memberwise initializer must exist");
  auto *initDRE =
      new (C) DeclRefExpr(memberwiseInitDecl, DeclNameLoc(), /*Implicit*/ true);
  initDRE->setFunctionRefKind(FunctionRefKind::SingleApply);

  auto *nominalTypeExpr = TypeExpr::createImplicitForDecl(
      DeclNameLoc(), nominal, funcDecl,
      funcDecl->mapTypeIntoContext(nominal->getInterfaceType()));
  auto *initExpr = new (C) ConstructorRefCallExpr(initDRE, nominalTypeExpr);

  auto createMemberPropertyExpr = [&](VarDecl *member) -> Expr * {
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
    auto *module = nominal->getModuleContext();
    auto confRef = module->lookupConformance(memberType, proto);
    assert(confRef && "Member does not conform to `AdditiveArithmetic`");
    // If conformance reference is not concrete, then concrete witness
    // declaration for property cannot be resolved. Return reference to
    // protocol requirement: this will be dynamically dispatched.
    if (!confRef.isConcrete()) {
      return new (C) MemberRefExpr(memberExpr, SourceLoc(), reqDecl,
                                   DeclNameLoc(), /*Implicit*/ true);
    }
    // Otherwise, return reference to concrete witness declaration.
    auto conf = confRef.getConcrete();
    auto *witnessDecl = conf->getWitnessDecl(reqDecl);
    return new (C) MemberRefExpr(memberExpr, SourceLoc(), witnessDecl,
                                 DeclNameLoc(), /*Implicit*/ true);
  };

  // Create array of `member.<property>` expressions.
  llvm::SmallVector<Expr *, 2> memberPropExprs;
  llvm::SmallVector<Identifier, 2> memberNames;
  for (auto member : nominal->getStoredProperties()) {
    memberPropExprs.push_back(createMemberPropertyExpr(member));
    memberNames.push_back(member->getName());
  }
  // Call memberwise initializer with member property expressions.
  auto *callExpr =
      CallExpr::createImplicit(C, initExpr, memberPropExprs, memberNames);
  ASTNode returnStmt = new (C) ReturnStmt(SourceLoc(), callExpr, true);
  auto *braceStmt =
      BraceStmt::create(C, SourceLoc(), returnStmt, SourceLoc(), true);
  return std::pair<BraceStmt *, bool>(braceStmt, false);
}

// Synthesize body for the `AdditiveArithmetic.zero` computed property getter.
static std::pair<BraceStmt *, bool>
deriveBodyAdditiveArithmetic_zero(AbstractFunctionDecl *funcDecl, void *) {
  auto &C = funcDecl->getASTContext();
  auto *addArithProto = C.getProtocol(KnownProtocolKind::AdditiveArithmetic);
  auto *zeroReq = getProtocolRequirement(addArithProto, C.Id_zero);
  return deriveBodyPropertyGetter(funcDecl, addArithProto, zeroReq);
}

// Synthesize the static property declaration for `AdditiveArithmetic.zero`.
static ValueDecl *deriveAdditiveArithmetic_zero(DerivedConformance &derived) {
  auto &C = derived.Context;
  auto *nominal = derived.Nominal;
  auto *parentDC = derived.getConformanceContext();

  auto returnInterfaceTy = nominal->getDeclaredInterfaceType();
  auto returnTy = parentDC->mapTypeIntoContext(returnInterfaceTy);

  // Create property declaration.
  VarDecl *propDecl;
  PatternBindingDecl *pbDecl;
  std::tie(propDecl, pbDecl) = derived.declareDerivedProperty(
      C.Id_zero, returnInterfaceTy, returnTy, /*isStatic*/ true,
      /*isFinal*/ true);

  // Create property getter.
  auto *getterDecl =
      derived.addGetterToReadOnlyDerivedProperty(propDecl, returnTy);
  getterDecl->setBodySynthesizer(deriveBodyAdditiveArithmetic_zero, nullptr);

  derived.addMembersToConformanceContext({propDecl, pbDecl});
  return propDecl;
}

ValueDecl *
DerivedConformance::deriveAdditiveArithmetic(ValueDecl *requirement) {
  // Diagnose conformances in disallowed contexts.
  if (checkAndDiagnoseDisallowedContext(requirement))
    return nullptr;
  if (requirement->getBaseName() == Context.getIdentifier("+"))
    return deriveMathOperator(*this, Add);
  if (requirement->getBaseName() == Context.getIdentifier("-"))
    return deriveMathOperator(*this, Subtract);
  if (requirement->getBaseName() == Context.Id_zero)
    return deriveAdditiveArithmetic_zero(*this);
  Context.Diags.diagnose(requirement->getLoc(),
                         diag::broken_additive_arithmetic_requirement);
  return nullptr;
}
