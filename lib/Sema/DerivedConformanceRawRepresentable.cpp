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
#include "DerivedConformances.h"

using namespace swift;
using namespace DerivedConformance;

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

static Type deriveRawRepresentable_Raw(TypeChecker &tc, Decl *parentDecl,
                                       EnumDecl *enumDecl) {
  // enum SomeEnum : SomeType {
  //   @derived
  //   typealias Raw = SomeType
  // }
  auto rawInterfaceType = enumDecl->getRawType();
  return cast<DeclContext>(parentDecl)->mapTypeIntoContext(rawInterfaceType);
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

  auto enumDecl = parentDC->getAsEnumOrEnumExtensionContext();

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

  Type enumType = parentDC->getDeclaredTypeInContext();

  SmallVector<CaseStmt*, 4> cases;
  for (auto elt : enumDecl->getAllElements()) {
    auto pat = new (C) EnumElementPattern(TypeLoc::withoutLoc(enumType),
                                          SourceLoc(), SourceLoc(),
                                          Identifier(), elt, nullptr);
    pat->setImplicit();

    auto labelItem =
      CaseLabelItem(/*IsDefault=*/false, pat, SourceLoc(), nullptr);

    auto returnExpr = cloneRawLiteralExpr(C, elt->getRawValueExpr());
    auto returnStmt = new (C) ReturnStmt(SourceLoc(), returnExpr);

    auto body = BraceStmt::create(C, SourceLoc(),
                                  ASTNode(returnStmt), SourceLoc());

    cases.push_back(CaseStmt::create(C, SourceLoc(), labelItem,
                                     /*HasBoundDecls=*/false, SourceLoc(),
                                     body));
  }

  auto selfRef = createSelfDeclRef(toRawDecl);
  auto switchStmt = SwitchStmt::create(LabeledStmtInfo(), SourceLoc(), selfRef,
                                       SourceLoc(), cases, SourceLoc(), C);
  auto body = BraceStmt::create(C, SourceLoc(),
                                ASTNode(switchStmt),
                                SourceLoc());
  toRawDecl->setBody(body);
}

static VarDecl *deriveRawRepresentable_raw(TypeChecker &tc,
                                           Decl *parentDecl,
                                           EnumDecl *enumDecl) {
  ASTContext &C = tc.Context;
  
  auto parentDC = cast<DeclContext>(parentDecl);
  auto rawInterfaceType = enumDecl->getRawType();
  auto rawType = parentDC->mapTypeIntoContext(rawInterfaceType);
  // Define the getter.
  auto getterDecl = declareDerivedPropertyGetter(tc, parentDecl, enumDecl,
                                                 rawInterfaceType,
                                                 rawType,
                                                 /*isStatic=*/false,
                                                 /*isFinal=*/false);
  getterDecl->setBodySynthesizer(&deriveBodyRawRepresentable_raw);

  // Define the property.
  VarDecl *propDecl;
  PatternBindingDecl *pbDecl;
  std::tie(propDecl, pbDecl)
    = declareDerivedReadOnlyProperty(tc, parentDecl, enumDecl,
                                     C.Id_rawValue,
                                     rawInterfaceType,
                                     rawType,
                                     getterDecl,
                                     /*isStatic=*/false,
                                     /*isFinal=*/false);
  
  auto dc = cast<IterableDeclContext>(parentDecl);
  dc->addMember(getterDecl);
  dc->addMember(propDecl);
  dc->addMember(pbDecl);

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

  auto nominalTypeDecl = parentDC->getAsNominalTypeOrNominalTypeExtensionContext();
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

  Type enumType = parentDC->getDeclaredTypeInContext();

  auto selfDecl = cast<ConstructorDecl>(initDecl)->getImplicitSelfDecl();
  
  SmallVector<CaseStmt*, 4> cases;
  for (auto elt : enumDecl->getAllElements()) {
    auto litExpr = cloneRawLiteralExpr(C, elt->getRawValueExpr());
    auto litPat = new (C) ExprPattern(litExpr, /*isResolved*/ true,
                                      nullptr, nullptr);
    litPat->setImplicit();

    auto labelItem =
      CaseLabelItem(/*IsDefault=*/false, litPat, SourceLoc(), nullptr);

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
                                     body));
  }

  auto anyPat = new (C) AnyPattern(SourceLoc());
  anyPat->setImplicit();
  auto dfltLabelItem =
    CaseLabelItem(/*IsDefault=*/true, anyPat, SourceLoc(), nullptr);

  auto dfltReturnStmt = new (C) FailStmt(SourceLoc(), SourceLoc());
  auto dfltBody = BraceStmt::create(C, SourceLoc(),
                                    ASTNode(dfltReturnStmt), SourceLoc());
  cases.push_back(CaseStmt::create(C, SourceLoc(), dfltLabelItem,
                                   /*HasBoundDecls=*/false, SourceLoc(),
                                   dfltBody));

  auto rawDecl = initDecl->getParameterList(1)->get(0);
  auto rawRef = new (C) DeclRefExpr(rawDecl, DeclNameLoc(), /*implicit*/true);
  auto switchStmt = SwitchStmt::create(LabeledStmtInfo(), SourceLoc(), rawRef,
                                       SourceLoc(), cases, SourceLoc(), C);
  auto body = BraceStmt::create(C, SourceLoc(),
                                ASTNode(switchStmt),
                                SourceLoc());
  initDecl->setBody(body);
}

static ConstructorDecl *deriveRawRepresentable_init(TypeChecker &tc,
                                                    Decl *parentDecl,
                                                    EnumDecl *enumDecl) {
  ASTContext &C = tc.Context;
  
  auto parentDC = cast<DeclContext>(parentDecl);
  auto rawInterfaceType = enumDecl->getRawType();
  auto rawType = parentDC->mapTypeIntoContext(rawInterfaceType);

  auto equatableProto = tc.getProtocol(enumDecl->getLoc(),
                                       KnownProtocolKind::Equatable);
  assert(equatableProto);
  assert(tc.conformsToProtocol(rawType, equatableProto, enumDecl, None));
  (void)equatableProto;

  auto *selfDecl = ParamDecl::createSelf(SourceLoc(), parentDC,
                                         /*static*/false, /*inout*/true);

  auto *rawDecl = new (C) ParamDecl(/*IsLet*/true, SourceLoc(), SourceLoc(),
                                    C.Id_rawValue, SourceLoc(),
                                    C.Id_rawValue, rawType, parentDC);
  rawDecl->setInterfaceType(rawInterfaceType);
  rawDecl->setImplicit();
  auto paramList = ParameterList::createWithoutLoc(rawDecl);
  
  DeclName name(C, C.Id_init, paramList);
  
  auto initDecl =
    new (C) ConstructorDecl(name, SourceLoc(),
                            /*Failability=*/ OTK_Optional,
                            /*FailabilityLoc=*/SourceLoc(),
                            /*Throws=*/false, /*ThrowsLoc=*/SourceLoc(),
                            selfDecl, paramList,
                            /*GenericParams=*/nullptr, parentDC);
  
  initDecl->setImplicit();
  initDecl->setBodySynthesizer(&deriveBodyRawRepresentable_init);

  // Compute the type of the initializer.
  TupleTypeElt element(rawType, C.Id_rawValue);
  TupleTypeElt interfaceElement(rawInterfaceType, C.Id_rawValue);
  auto interfaceArgType = TupleType::get(interfaceElement, C);

  // Compute the interface type of the initializer.
  Type retInterfaceType
    = OptionalType::get(parentDC->getDeclaredInterfaceType());
  Type interfaceType = FunctionType::get(interfaceArgType, retInterfaceType);
  Type selfInterfaceType = initDecl->computeInterfaceSelfType();
  Type selfInitializerInterfaceType
    = initDecl->computeInterfaceSelfType(/*init*/ true);

  Type allocIfaceType;
  Type initIfaceType;
  if (auto sig = parentDC->getGenericSignatureOfContext()) {
    initDecl->setGenericEnvironment(parentDC->getGenericEnvironmentOfContext());

    allocIfaceType = GenericFunctionType::get(sig, selfInterfaceType,
                                              interfaceType,
                                              FunctionType::ExtInfo());
    initIfaceType = GenericFunctionType::get(sig, selfInitializerInterfaceType,
                                             interfaceType,
                                             FunctionType::ExtInfo());
  } else {
    allocIfaceType = FunctionType::get(selfInterfaceType,
                                       interfaceType);
    initIfaceType = FunctionType::get(selfInitializerInterfaceType,
                                      interfaceType);
  }
  initDecl->setInterfaceType(allocIfaceType);
  initDecl->setInitializerInterfaceType(initIfaceType);
  initDecl->setAccessibility(std::max(Accessibility::Internal,
                                      enumDecl->getFormalAccess()));

  // If the enum was not imported, the derived conformance is either from the
  // enum itself or an extension, in which case we will emit the declaration
  // normally.
  if (enumDecl->hasClangNode())
    tc.Context.addExternalDecl(initDecl);

  cast<IterableDeclContext>(parentDecl)->addMember(initDecl);
  return initDecl;
}

static bool canSynthesizeRawRepresentable(TypeChecker &tc, Decl *parentDecl,
                                          EnumDecl *enumDecl) {

  // It must have a valid raw type.
  Type rawType = enumDecl->getRawType();
  if (!rawType)
    return false;
  auto parentDC = cast<DeclContext>(parentDecl);
  rawType       = parentDC->mapTypeIntoContext(rawType);

  if (!enumDecl->getInherited().empty() &&
      enumDecl->getInherited().front().isError())
    return false;

  // The raw type must be Equatable, so that we have a suitable ~= for
  // synthesized switch statements.
  auto equatableProto = tc.getProtocol(enumDecl->getLoc(),
                                       KnownProtocolKind::Equatable);
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

ValueDecl *DerivedConformance::deriveRawRepresentable(TypeChecker &tc,
                                                      Decl *parentDecl,
                                                      NominalTypeDecl *type,
                                                      ValueDecl *requirement) {

  // We can only synthesize RawRepresentable for enums.
  auto enumDecl = dyn_cast<EnumDecl>(type);
  if (!enumDecl)
    return nullptr;

  // Check other preconditions for synthesized conformance.
  if (!canSynthesizeRawRepresentable(tc, parentDecl, enumDecl))
    return nullptr;

  if (requirement->getName() == tc.Context.Id_rawValue)
    return deriveRawRepresentable_raw(tc, parentDecl, enumDecl);
  
  if (requirement->getName() == tc.Context.Id_init)
    return deriveRawRepresentable_init(tc, parentDecl, enumDecl);
  
  tc.diagnose(requirement->getLoc(),
              diag::broken_raw_representable_requirement);
  return nullptr;
}

Type DerivedConformance::deriveRawRepresentable(TypeChecker &tc,
                                                Decl *parentDecl,
                                                NominalTypeDecl *type,
                                                AssociatedTypeDecl *assocType) {

  // We can only synthesize RawRepresentable for enums.
  auto enumDecl = dyn_cast<EnumDecl>(type);
  if (!enumDecl)
    return nullptr;

  // Check other preconditions for synthesized conformance.
  if (!canSynthesizeRawRepresentable(tc, parentDecl, enumDecl))
    return nullptr;

  if (assocType->getName() == tc.Context.Id_RawValue) {
    return deriveRawRepresentable_Raw(tc, parentDecl, enumDecl);
  }
  
  tc.diagnose(assocType->getLoc(),
              diag::broken_raw_representable_requirement);
  return nullptr;
}
