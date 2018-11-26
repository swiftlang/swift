//===--- DerivedConformanceRawRepresentable.cpp - Derived RawRepresentable ===//
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
//  This file implements implicit derivation of the RawRepresentable protocol
//  for an enum.
//
//===----------------------------------------------------------------------===//

#include "TypeChecker.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Stmt.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Pattern.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/Types.h"
#include "llvm/ADT/APInt.h"
#include "DerivedConformances.h"

using namespace swift;

static LiteralExpr *cloneRawLiteralExpr(ASTContext &C, LiteralExpr *expr) {
  LiteralExpr *clone;
  if (auto intLit = dyn_cast<IntegerLiteralExpr>(expr)) {
    clone = new (C) IntegerLiteralExpr(intLit->getDigitsText(), expr->getLoc(),
                                       /*implicit*/ true);
    if (intLit->isNegative())
      cast<IntegerLiteralExpr>(clone)->setNegative(expr->getLoc());
  } else if (isa<NilLiteralExpr>(expr)) {
    clone = new (C) NilLiteralExpr(expr->getLoc());
  } else if (auto stringLit = dyn_cast<StringLiteralExpr>(expr)) {
    clone = new (C) StringLiteralExpr(stringLit->getValue(), expr->getLoc());
  } else if (auto floatLit = dyn_cast<FloatLiteralExpr>(expr)) {
    clone = new (C) FloatLiteralExpr(floatLit->getDigitsText(), expr->getLoc(),
                                     /*implicit*/ true);
    if (floatLit->isNegative())
      cast<FloatLiteralExpr>(clone)->setNegative(expr->getLoc());
  } else {
    llvm_unreachable("invalid raw literal expr");
  }
  clone->setImplicit();
  return clone;
}

static Type deriveRawRepresentable_Raw(DerivedConformance &derived) {
  // enum SomeEnum : SomeType {
  //   @derived
  //   typealias Raw = SomeType
  // }
  auto rawInterfaceType = cast<EnumDecl>(derived.Nominal)->getRawType();
  return derived.getConformanceContext()->mapTypeIntoContext(rawInterfaceType);
}

static void deriveBodyRawRepresentable_raw(AbstractFunctionDecl *toRawDecl) {
  // enum SomeEnum : SomeType {
  //   case A = 111, B = 222
  //   @derived
  //   var raw: SomeType {
  //     switch self {
  //     case A:
  //       return 111
  //     case B:
  //       return 222
  //     }
  //   }
  // }

  auto parentDC = toRawDecl->getDeclContext();
  ASTContext &C = parentDC->getASTContext();

  auto enumDecl = parentDC->getSelfEnumDecl();

  Type rawTy = enumDecl->getRawType();
  assert(rawTy);
  rawTy = toRawDecl->mapTypeIntoContext(rawTy);

#ifndef NDEBUG
  for (auto elt : enumDecl->getAllElements()) {
    assert(elt->getTypeCheckedRawValueExpr() &&
           "Enum element has no literal - missing a call to checkEnumRawValues()");
    assert(elt->getTypeCheckedRawValueExpr()->getType()->isEqual(rawTy));
  }
#endif

  if (enumDecl->isObjC()) {
    // Special case: ObjC enums are represented by their raw value, so just use
    // a bitcast.

    // return unsafeBitCast(self, to: RawType.self)
    DeclName name(C, C.getIdentifier("unsafeBitCast"), {Identifier(), C.Id_to});
    auto functionRef = new (C) UnresolvedDeclRefExpr(name,
                                                     DeclRefKind::Ordinary,
                                                     DeclNameLoc());
    auto selfRef = DerivedConformance::createSelfDeclRef(toRawDecl);
    auto bareTypeExpr = TypeExpr::createImplicit(rawTy, C);
    auto typeExpr = new (C) DotSelfExpr(bareTypeExpr, SourceLoc(), SourceLoc());
    auto call = CallExpr::createImplicit(C, functionRef, {selfRef, typeExpr},
                                         {Identifier(), C.Id_to});
    auto returnStmt = new (C) ReturnStmt(SourceLoc(), call);
    auto body = BraceStmt::create(C, SourceLoc(), ASTNode(returnStmt),
                                  SourceLoc());
    toRawDecl->setBody(body);
    return;
  }

  Type enumType = parentDC->getDeclaredTypeInContext();

  SmallVector<ASTNode, 4> cases;
  for (auto elt : enumDecl->getAllElements()) {
    auto pat = new (C) EnumElementPattern(TypeLoc::withoutLoc(enumType),
                                          SourceLoc(), SourceLoc(),
                                          Identifier(), elt, nullptr);
    pat->setImplicit();

    auto labelItem = CaseLabelItem(pat);

    auto returnExpr = cloneRawLiteralExpr(C, elt->getRawValueExpr());
    auto returnStmt = new (C) ReturnStmt(SourceLoc(), returnExpr);

    auto body = BraceStmt::create(C, SourceLoc(),
                                  ASTNode(returnStmt), SourceLoc());

    cases.push_back(CaseStmt::create(C, SourceLoc(), labelItem,
                                     /*HasBoundDecls=*/false, SourceLoc(),
                                     SourceLoc(), body));
  }

  auto selfRef = DerivedConformance::createSelfDeclRef(toRawDecl);
  auto switchStmt = SwitchStmt::create(LabeledStmtInfo(), SourceLoc(), selfRef,
                                       SourceLoc(), cases, SourceLoc(), C);
  auto body = BraceStmt::create(C, SourceLoc(),
                                ASTNode(switchStmt),
                                SourceLoc());
  toRawDecl->setBody(body);
}

static void maybeMarkAsInlinable(DerivedConformance &derived,
                                 AbstractFunctionDecl *afd) {
  ASTContext &C = derived.TC.Context;
  auto parentDC = derived.getConformanceContext();
  if (parentDC->getParentModule()->getResilienceStrategy() !=
      ResilienceStrategy::Resilient) {
    AccessScope access =
        afd->getFormalAccessScope(nullptr,
                                  /*treatUsableFromInlineAsPublic*/true);
    if (auto *attr = afd->getAttrs().getAttribute<UsableFromInlineAttr>())
      attr->setInvalid();
    if (access.isPublic())
      afd->getAttrs().add(new (C) InlinableAttr(/*implicit*/false));
  }
}

static VarDecl *deriveRawRepresentable_raw(DerivedConformance &derived) {
  ASTContext &C = derived.TC.Context;

  auto enumDecl = cast<EnumDecl>(derived.Nominal);
  auto parentDC = derived.getConformanceContext();
  auto rawInterfaceType = enumDecl->getRawType();
  auto rawType = parentDC->mapTypeIntoContext(rawInterfaceType);

  // Define the property.
  VarDecl *propDecl;
  PatternBindingDecl *pbDecl;
  std::tie(propDecl, pbDecl) = derived.declareDerivedProperty(
      C.Id_rawValue, rawInterfaceType, rawType, /*isStatic=*/false,
      /*isFinal=*/false);

  // Define the getter.
  auto getterDecl = DerivedConformance::addGetterToReadOnlyDerivedProperty(
      derived.TC, propDecl, rawType);
  getterDecl->setBodySynthesizer(&deriveBodyRawRepresentable_raw);

  // If the containing module is not resilient, make sure clients can get at
  // the raw value without function call overhead.
  maybeMarkAsInlinable(derived, getterDecl);

  derived.addMembersToConformanceContext({getterDecl, propDecl, pbDecl});

  return propDecl;
}

static void
deriveBodyRawRepresentable_init(AbstractFunctionDecl *initDecl) {
  // enum SomeEnum : SomeType {
  //   case A = 111, B = 222
  //   @derived
  //   init?(rawValue: SomeType) {
  //     switch rawValue {
  //     case 111:
  //       self = .A
  //     case 222:
  //       self = .B
  //     default:
  //       return nil
  //     }
  //   }
  // }
  
  auto parentDC = initDecl->getDeclContext();
  ASTContext &C = parentDC->getASTContext();

  auto nominalTypeDecl = parentDC->getSelfNominalTypeDecl();
  auto enumDecl = cast<EnumDecl>(nominalTypeDecl);

  Type rawTy = enumDecl->getRawType();
  assert(rawTy);
  rawTy = initDecl->mapTypeIntoContext(rawTy);

#ifndef NDEBUG
  for (auto elt : enumDecl->getAllElements()) {
    assert(elt->getTypeCheckedRawValueExpr() &&
           "Enum element has no literal - missing a call to checkEnumRawValues()");
    assert(elt->getTypeCheckedRawValueExpr()->getType()->isEqual(rawTy));
  }
#endif

  bool isStringEnum =
    (rawTy->getNominalOrBoundGenericNominal() == C.getStringDecl());
  llvm::SmallVector<Expr *, 16> stringExprs;

  Type enumType = parentDC->getDeclaredTypeInContext();

  auto selfDecl = cast<ConstructorDecl>(initDecl)->getImplicitSelfDecl();
  
  SmallVector<ASTNode, 4> cases;
  unsigned Idx = 0;
  for (auto elt : enumDecl->getAllElements()) {
    LiteralExpr *litExpr = cloneRawLiteralExpr(C, elt->getRawValueExpr());
    if (isStringEnum) {
      // In case of a string enum we are calling the _findStringSwitchCase
      // function from the library and switching on the returned Int value.
      stringExprs.push_back(litExpr);
      litExpr = IntegerLiteralExpr::createFromUnsigned(C, Idx); 
    }
    auto litPat = new (C) ExprPattern(litExpr, /*isResolved*/ true,
                                      nullptr, nullptr);
    litPat->setImplicit();

    auto labelItem = CaseLabelItem(litPat);

    auto eltRef = new (C) DeclRefExpr(elt, DeclNameLoc(), /*implicit*/true);
    auto metaTyRef = TypeExpr::createImplicit(enumType, C);
    auto valueExpr = new (C) DotSyntaxCallExpr(eltRef, SourceLoc(), metaTyRef);
    
    auto selfRef = new (C) DeclRefExpr(selfDecl, DeclNameLoc(),
                                       /*implicit*/true,
                                       AccessSemantics::DirectToStorage);

    auto assignment = new (C) AssignExpr(selfRef, SourceLoc(), valueExpr,
                                         /*implicit*/ true);
    
    auto body = BraceStmt::create(C, SourceLoc(),
                                  ASTNode(assignment), SourceLoc());

    cases.push_back(CaseStmt::create(C, SourceLoc(), labelItem,
                                     /*HasBoundDecls=*/false, SourceLoc(),
                                     SourceLoc(), body));
    Idx++;
  }

  auto anyPat = new (C) AnyPattern(SourceLoc());
  anyPat->setImplicit();
  auto dfltLabelItem = CaseLabelItem::getDefault(anyPat);

  auto dfltReturnStmt = new (C) FailStmt(SourceLoc(), SourceLoc());
  auto dfltBody = BraceStmt::create(C, SourceLoc(),
                                    ASTNode(dfltReturnStmt), SourceLoc());
  cases.push_back(CaseStmt::create(C, SourceLoc(), dfltLabelItem,
                                   /*HasBoundDecls=*/false, SourceLoc(),
                                   SourceLoc(), dfltBody));

  auto rawDecl = initDecl->getParameters()->get(0);
  auto rawRef = new (C) DeclRefExpr(rawDecl, DeclNameLoc(), /*implicit*/true);
  Expr *switchArg = rawRef;
  if (isStringEnum) {
    // Call _findStringSwitchCase with an array of strings as argument.
    auto *Fun = new (C) UnresolvedDeclRefExpr(
                  C.getIdentifier("_findStringSwitchCase"),
                  DeclRefKind::Ordinary, DeclNameLoc());
    auto *strArray = ArrayExpr::create(C, SourceLoc(), stringExprs, {},
                                       SourceLoc());;
    Identifier tableId = C.getIdentifier("cases");
    Identifier strId = C.getIdentifier("string");
    auto *Args = TupleExpr::createImplicit(C, {strArray, rawRef},
                                              {tableId, strId});
    auto *CallExpr = CallExpr::create(C, Fun, Args, {}, {}, false, false);
    switchArg = CallExpr;
  }
  auto switchStmt = SwitchStmt::create(LabeledStmtInfo(), SourceLoc(), switchArg,
                                       SourceLoc(), cases, SourceLoc(), C);
  auto body = BraceStmt::create(C, SourceLoc(),
                                ASTNode(switchStmt),
                                SourceLoc());
  initDecl->setBody(body);
}

static ConstructorDecl *
deriveRawRepresentable_init(DerivedConformance &derived) {
  auto &tc = derived.TC;
  ASTContext &C = tc.Context;

  auto enumDecl = cast<EnumDecl>(derived.Nominal);
  auto parentDC = derived.getConformanceContext();
  auto rawInterfaceType = enumDecl->getRawType();
  auto rawType = parentDC->mapTypeIntoContext(rawInterfaceType);

  auto equatableProto = tc.getProtocol(enumDecl->getLoc(),
                                       KnownProtocolKind::Equatable);
  assert(equatableProto);
  assert(tc.conformsToProtocol(rawType, equatableProto, enumDecl, None));
  (void)equatableProto;
  (void)rawType;

  auto *rawDecl = new (C)
      ParamDecl(VarDecl::Specifier::Default, SourceLoc(), SourceLoc(),
                C.Id_rawValue, SourceLoc(), C.Id_rawValue, parentDC);
  rawDecl->setInterfaceType(rawInterfaceType);
  rawDecl->setImplicit();
  auto paramList = ParameterList::createWithoutLoc(rawDecl);
  
  DeclName name(C, DeclBaseName::createConstructor(), paramList);
  
  auto initDecl =
    new (C) ConstructorDecl(name, SourceLoc(),
                            /*Failability=*/ OTK_Optional,
                            /*FailabilityLoc=*/SourceLoc(),
                            /*Throws=*/false, /*ThrowsLoc=*/SourceLoc(),
                            paramList,
                            /*GenericParams=*/nullptr, parentDC);
  
  initDecl->setImplicit();
  initDecl->setBodySynthesizer(&deriveBodyRawRepresentable_init);

  // Compute the interface type of the initializer.
  if (auto env = parentDC->getGenericEnvironmentOfContext())
    initDecl->setGenericEnvironment(env);
  initDecl->computeType();

  initDecl->copyFormalAccessFrom(enumDecl, /*sourceIsParentContext*/true);
  initDecl->setValidationToChecked();

  // If the containing module is not resilient, make sure clients can construct
  // an instance without function call overhead.
  maybeMarkAsInlinable(derived, initDecl);

  C.addSynthesizedDecl(initDecl);

  derived.addMembersToConformanceContext({initDecl});
  return initDecl;
}

static bool canSynthesizeRawRepresentable(DerivedConformance &derived) {
  auto enumDecl = cast<EnumDecl>(derived.Nominal);
  auto &tc = derived.TC;

  // Validate the enum and its raw type.
  tc.validateDecl(enumDecl);

  // It must have a valid raw type.
  Type rawType = enumDecl->getRawType();
  if (!rawType)
    return false;
  auto parentDC = cast<DeclContext>(derived.ConformanceDecl);
  rawType       = parentDC->mapTypeIntoContext(rawType);

  auto inherited = enumDecl->getInherited();
  if (!inherited.empty() && inherited.front().wasValidated() &&
      inherited.front().isError())
    return false;

  // The raw type must be Equatable, so that we have a suitable ~= for
  // synthesized switch statements.
  auto equatableProto =
      tc.getProtocol(enumDecl->getLoc(), KnownProtocolKind::Equatable);
  if (!equatableProto)
    return false;

  if (!tc.conformsToProtocol(rawType, equatableProto, enumDecl, None))
    return false;
  
  // There must be enum elements.
  if (enumDecl->getAllElements().empty())
    return false;

  // Have the type-checker validate that:
  // - the enum elements all have the same type
  // - they all match the enum type
  for (auto elt : enumDecl->getAllElements()) {
    tc.validateDecl(elt);
    if (elt->isInvalid()) {
      return false;
    }
  }

  // If it meets all of those requirements, we can synthesize RawRepresentable conformance.
  return true;
}

ValueDecl *DerivedConformance::deriveRawRepresentable(ValueDecl *requirement) {

  // We can only synthesize RawRepresentable for enums.
  if (!isa<EnumDecl>(Nominal))
    return nullptr;

  // Check other preconditions for synthesized conformance.
  if (!canSynthesizeRawRepresentable(*this))
    return nullptr;

  if (requirement->getBaseName() == TC.Context.Id_rawValue)
    return deriveRawRepresentable_raw(*this);

  if (requirement->getBaseName() == DeclBaseName::createConstructor())
    return deriveRawRepresentable_init(*this);

  TC.diagnose(requirement->getLoc(),
              diag::broken_raw_representable_requirement);
  return nullptr;
}

Type DerivedConformance::deriveRawRepresentable(AssociatedTypeDecl *assocType) {

  // We can only synthesize RawRepresentable for enums.
  if (!isa<EnumDecl>(Nominal))
    return nullptr;

  // Check other preconditions for synthesized conformance.
  if (!canSynthesizeRawRepresentable(*this))
    return nullptr;

  if (assocType->getName() == TC.Context.Id_RawValue) {
    return deriveRawRepresentable_Raw(*this);
  }

  TC.diagnose(assocType->getLoc(), diag::broken_raw_representable_requirement);
  return nullptr;
}
