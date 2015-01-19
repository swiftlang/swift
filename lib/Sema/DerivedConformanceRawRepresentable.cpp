//===--- DerivedConformanceRawRepresentable.cpp - Derived RawRepresentable ===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  This file implements implicit derivation of the RawRepresentable protocol
//  for an enum.
//
//===----------------------------------------------------------------------===//

#include "TypeChecker.h"
#include "llvm/Support/raw_ostream.h"
#include "swift/AST/ArchetypeBuilder.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Stmt.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Types.h"
#include "DerivedConformances.h"

using namespace swift;
using namespace DerivedConformance;

void DerivedConformance::_insertOperatorDecl(NominalTypeDecl *scope,
                                             Decl *member) {
  // Find the module.
  auto &C = scope->getASTContext();
  auto mod = scope->getModuleContext();

  // Add it to the module in a DerivedFileUnit.
  mod->getDerivedFileUnit().addDerivedDecl(cast<FuncDecl>(member));

  // Add it as a derived global decl to the nominal type.
  auto oldDerived = scope->getDerivedGlobalDecls();
  auto oldSize = std::distance(oldDerived.begin(), oldDerived.end());
  auto newDerived = C.Allocate<Decl*>(oldSize + 1);
  
  std::move(oldDerived.begin(), oldDerived.end(), newDerived.begin());
  newDerived[oldSize] = member;
  
  scope->setDerivedGlobalDecls(newDerived);
}

static LiteralExpr *cloneRawLiteralExpr(ASTContext &C, LiteralExpr *expr) {
  LiteralExpr *clone;
  if (auto intLit = dyn_cast<IntegerLiteralExpr>(expr)) {
    clone = new (C) IntegerLiteralExpr(intLit->getDigitsText(), SourceLoc(),
                                       /*implicit*/ true);
    if (intLit->isNegative())
      cast<IntegerLiteralExpr>(clone)->setNegative(SourceLoc());
  } else if (isa<NilLiteralExpr>(expr)) {
    clone = new (C) NilLiteralExpr(SourceLoc());
  } else if (auto charLit = dyn_cast<CharacterLiteralExpr>(expr)) {
    clone = new (C) CharacterLiteralExpr(charLit->getValue(), SourceLoc());
  } else if (auto stringLit = dyn_cast<StringLiteralExpr>(expr)) {
    clone = new (C) StringLiteralExpr(stringLit->getValue(), SourceLoc());
  } else if (auto floatLit = dyn_cast<FloatLiteralExpr>(expr)) {
    clone = new (C) FloatLiteralExpr(floatLit->getDigitsText(), SourceLoc(),
                                     /*implicit*/ true);
    if (floatLit->isNegative())
      cast<FloatLiteralExpr>(clone)->setNegative(SourceLoc());
  } else {
    llvm_unreachable("invalid raw literal expr");
  }
  clone->setImplicit();
  return clone;
}

static TypeDecl *deriveRawRepresentable_Raw(TypeChecker &tc,
                                            EnumDecl *enumDecl) {
  // enum SomeEnum : SomeType {
  //   @derived
  //   typealias Raw = SomeType
  // }
  ASTContext &C = tc.Context;

  auto rawInterfaceType = enumDecl->getRawType();
  auto rawType = ArchetypeBuilder::mapTypeIntoContext(enumDecl,
                                                      rawInterfaceType);
  auto rawTypeDecl = new (C) TypeAliasDecl(SourceLoc(),
                                   C.Id_RawValue,
                                   SourceLoc(),
                                   TypeLoc::withoutLoc(rawType),
                                   enumDecl);
  rawTypeDecl->setImplicit();
  rawTypeDecl->setType(rawType);
  rawTypeDecl->setInterfaceType(rawInterfaceType);
  rawTypeDecl->setAccessibility(enumDecl->getAccessibility());
  enumDecl->addMember(rawTypeDecl);
  return rawTypeDecl;
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
  auto enumDecl = cast<EnumDecl>(toRawDecl->getDeclContext());

  Type rawTy = enumDecl->getRawType();
  assert(rawTy);
  for (auto elt : enumDecl->getAllElements()) {
    if (!elt->getTypeCheckedRawValueExpr() ||
        !elt->getTypeCheckedRawValueExpr()->getType()->isEqual(rawTy)) {
      return;
    }
  }

  ASTContext &C = enumDecl->getASTContext();
  Type enumType = enumDecl->getDeclaredTypeInContext();

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

  Pattern *curriedArgs = toRawDecl->getBodyParamPatterns().front();
  auto selfPattern =
    cast<NamedPattern>(curriedArgs->getSemanticsProvidingPattern());
  auto selfDecl = selfPattern->getDecl();
  auto selfRef = new (C) DeclRefExpr(selfDecl, SourceLoc(), /*implicit*/true);
  auto switchStmt = SwitchStmt::create(LabeledStmtInfo(), SourceLoc(), selfRef,
                                       SourceLoc(), cases, SourceLoc(), C);
  auto body = BraceStmt::create(C, SourceLoc(),
                                ASTNode(switchStmt),
                                SourceLoc());
  toRawDecl->setBody(body);
}

static VarDecl *deriveRawRepresentable_raw(TypeChecker &tc,
                                           EnumDecl *enumDecl) {
  ASTContext &C = tc.Context;
  
  auto rawInterfaceType = enumDecl->getRawType();
  auto rawType = ArchetypeBuilder::mapTypeIntoContext(enumDecl,
                                                      rawInterfaceType);
  Type enumType = enumDecl->getDeclaredTypeInContext();
  
  // Define the getter.
  VarDecl *selfDecl = new (C) ParamDecl(/*IsLet*/true,
                                        SourceLoc(),
                                        Identifier(),
                                        SourceLoc(),
                                        C.Id_self,
                                        enumType,
                                        enumDecl);
  selfDecl->setImplicit();
  Pattern *selfParam = new (C) NamedPattern(selfDecl, /*implicit*/ true);
  selfParam->setType(enumType);
  selfParam = new (C) TypedPattern(selfParam, TypeLoc::withoutLoc(enumType));
  selfParam->setType(enumType);
  Pattern *methodParam = TuplePattern::create(C, SourceLoc(),{},SourceLoc());
  methodParam->setType(TupleType::getEmpty(tc.Context));
  Pattern *params[] = {selfParam, methodParam};
  
  FuncDecl *getterDecl =
      FuncDecl::create(C, SourceLoc(), StaticSpellingKind::None, SourceLoc(),
                       DeclName(), SourceLoc(), nullptr, Type(),
                       params, TypeLoc::withoutLoc(rawType), enumDecl);
  getterDecl->setImplicit();
  getterDecl->setBodySynthesizer(&deriveBodyRawRepresentable_raw);

  // Compute the type of the getter.
  GenericParamList *genericParams = nullptr;
  Type type = FunctionType::get(TupleType::getEmpty(tc.Context), rawType);
  Type selfType = getterDecl->computeSelfType(&genericParams);
  if (genericParams)
    type = PolymorphicFunctionType::get(selfType, type, genericParams);
  else
    type = FunctionType::get(selfType, type);
  getterDecl->setType(type);
  getterDecl->setBodyResultType(rawType);

  // Compute the interface type of the getter.
  Type interfaceType = FunctionType::get(TupleType::getEmpty(tc.Context),
                                         rawInterfaceType);
  Type selfInterfaceType = getterDecl->computeInterfaceSelfType(false);
  if (auto sig = enumDecl->getGenericSignatureOfContext())
    interfaceType = GenericFunctionType::get(sig, selfInterfaceType,
                                             interfaceType,
                                             FunctionType::ExtInfo());
  else
    interfaceType = type;
  getterDecl->setInterfaceType(interfaceType);
  getterDecl->setAccessibility(enumDecl->getAccessibility());

  if (enumDecl->hasClangNode())
    tc.implicitlyDefinedFunctions.push_back(getterDecl);

  // Define the property.
  VarDecl *propDecl = new (C) VarDecl(/*static*/ false,
                                      /*let*/ false,
                                      SourceLoc(), C.Id_rawValue,
                                      rawType, enumDecl);
  propDecl->setImplicit();
  propDecl->makeComputed(SourceLoc(), getterDecl, nullptr, nullptr,
                         SourceLoc());
  propDecl->setAccessibility(enumDecl->getAccessibility());
  propDecl->setInterfaceType(rawInterfaceType);
  
  Pattern *propPat = new (C) NamedPattern(propDecl, /*implicit*/ true);
  propPat->setType(rawType);
  propPat = new (C) TypedPattern(propPat, TypeLoc::withoutLoc(rawType),
                                 /*implicit*/ true);
  
  auto pbDecl = new (C) PatternBindingDecl(SourceLoc(),
                                           StaticSpellingKind::None,
                                           SourceLoc(), propPat, nullptr,
                                           /*cond*/ false, enumDecl);
  pbDecl->setImplicit();
  
  enumDecl->addMember(getterDecl);
  enumDecl->addMember(propDecl);
  enumDecl->addMember(pbDecl);
  return propDecl;
}

static void
deriveBodyRawRepresentable_init(AbstractFunctionDecl *initDecl) {
  // enum SomeEnum : SomeType {
  //   case A = 111, B = 222
  //   @derived
  //   init?(_ raw: SomeType) {
  //     switch raw {
  //     case 111:
  //       self = .A
  //     case 222:
  //       self = .B
  //     default:
  //       return nil
  //     }
  //   }
  // }
  
  auto enumDecl = cast<EnumDecl>(initDecl->getDeclContext());

  Type rawTy = enumDecl->getRawType();
  assert(rawTy);
  for (auto elt : enumDecl->getAllElements()) {
    if (!elt->getTypeCheckedRawValueExpr() ||
        !elt->getTypeCheckedRawValueExpr()->getType()->isEqual(rawTy)) {
      return;
    }
  }

  ASTContext &C = enumDecl->getASTContext();
  Type enumType = enumDecl->getDeclaredTypeInContext();

  auto selfDecl = cast<ConstructorDecl>(initDecl)->getImplicitSelfDecl();
  
  SmallVector<CaseStmt*, 4> cases;
  for (auto elt : enumDecl->getAllElements()) {
    auto litExpr = cloneRawLiteralExpr(C, elt->getRawValueExpr());
    auto litPat = new (C) ExprPattern(litExpr, /*isResolved*/ true,
                                      nullptr, nullptr);
    litPat->setImplicit();

    auto labelItem =
      CaseLabelItem(/*IsDefault=*/false, litPat, SourceLoc(), nullptr);

    auto eltRef = new (C) DeclRefExpr(elt, SourceLoc(), /*implicit*/true);
    auto metaTyRef = TypeExpr::createImplicit(enumType, C);
    auto valueExpr = new (C) DotSyntaxCallExpr(eltRef, SourceLoc(), metaTyRef);
    
    auto selfRef = new (C) DeclRefExpr(selfDecl, SourceLoc(), /*implicit*/true,
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

  Pattern *args = initDecl->getBodyParamPatterns().back();
  auto rawArgPattern = cast<NamedPattern>(args->getSemanticsProvidingPattern());
  auto rawDecl = rawArgPattern->getDecl();
  auto rawRef = new (C) DeclRefExpr(rawDecl, SourceLoc(), /*implicit*/true);
  auto switchStmt = SwitchStmt::create(LabeledStmtInfo(), SourceLoc(), rawRef,
                                       SourceLoc(), cases, SourceLoc(), C);
  auto body = BraceStmt::create(C, SourceLoc(),
                                ASTNode(switchStmt),
                                SourceLoc());
  initDecl->setBody(body);
}

static ConstructorDecl *deriveRawRepresentable_init(TypeChecker &tc,
                                                    EnumDecl *enumDecl) {
  ASTContext &C = tc.Context;
  
  auto rawInterfaceType = enumDecl->getRawType();
  auto rawType = ArchetypeBuilder::mapTypeIntoContext(enumDecl,
                                                      rawInterfaceType);

  // Make sure that the raw type is Equatable. We need it to ensure that we have
  // a suitable ~= for the switch.
  auto equatableProto = tc.getProtocol(enumDecl->getLoc(),
                                       KnownProtocolKind::Equatable);
  if (!equatableProto)
    return nullptr;

  if (!tc.conformsToProtocol(rawType, equatableProto, enumDecl,
                             /*inExpression=*/false)) {
    SourceLoc loc = enumDecl->getInherited()[0].getSourceRange().Start;
    tc.diagnose(loc, diag::enum_raw_type_not_equatable, rawType);
    return nullptr;
  }

  Type enumType = enumDecl->getDeclaredTypeInContext();
  VarDecl *selfDecl = new (C) ParamDecl(/*IsLet*/false,
                                        SourceLoc(),
                                        Identifier(),
                                        SourceLoc(),
                                        C.Id_self,
                                        enumType,
                                        enumDecl);
  selfDecl->setImplicit();
  Pattern *selfParam = new (C) NamedPattern(selfDecl, /*implicit*/ true);
  selfParam->setType(enumType);
  selfParam = new (C) TypedPattern(selfParam,
                                   TypeLoc::withoutLoc(enumType));
  selfParam->setType(enumType);
  selfParam->setImplicit();

  VarDecl *rawDecl = new (C) ParamDecl(/*IsVal*/true,
                                       SourceLoc(),
                                       C.Id_rawValue,
                                       SourceLoc(),
                                       C.Id_rawValue,
                                       rawType,
                                       enumDecl);
  rawDecl->setImplicit();
  Pattern *rawParam = new (C) NamedPattern(rawDecl, /*implicit*/ true);
  rawParam->setType(rawType);
  rawParam = new (C) TypedPattern(rawParam, TypeLoc::withoutLoc(rawType));
  rawParam->setType(rawType);
  rawParam->setImplicit();
  rawParam = new (C) ParenPattern(SourceLoc(), rawParam, SourceLoc());
  rawParam->setType(rawType);
  rawParam->setImplicit();
  
  auto retTy = OptionalType::get(enumType);
  DeclName name(C, C.Id_init, { C.Id_rawValue });
  
  auto initDecl = new (C) ConstructorDecl(name, SourceLoc(),
                                          /*failability*/ OTK_Optional,
                                          SourceLoc(),
                                          selfParam,
                                          rawParam,
                                          nullptr,
                                          enumDecl);
  
  initDecl->setImplicit();
  initDecl->setBodySynthesizer(&deriveBodyRawRepresentable_init);

  // Compute the type of the initializer.
  GenericParamList *genericParams = nullptr;
  
  TupleTypeElt element(rawType, C.Id_rawValue);
  auto argType = TupleType::get(element, C);
  
  Type type = FunctionType::get(argType, retTy);
  Type selfType = initDecl->computeSelfType(&genericParams);
  Type selfMetatype = MetatypeType::get(selfType->getInOutObjectType());
  
  Type allocType;
  Type initType;
  if (genericParams) {
    allocType = PolymorphicFunctionType::get(selfMetatype, type, genericParams);
    initType = PolymorphicFunctionType::get(selfType, type, genericParams);
  } else {
    allocType = FunctionType::get(selfMetatype, type);
    initType = FunctionType::get(selfType, type);
  }
  initDecl->setType(allocType);
  initDecl->setInitializerType(initType);

  // Compute the interface type of the initializer.
  Type retInterfaceType
    = OptionalType::get(enumDecl->getDeclaredInterfaceType());
  Type interfaceType = FunctionType::get(rawInterfaceType, retInterfaceType);
  Type selfInterfaceType = initDecl->computeInterfaceSelfType(/*init*/ false);
  Type selfInitializerInterfaceType
    = initDecl->computeInterfaceSelfType(/*init*/ true);

  Type allocIfaceType;
  Type initIfaceType;
  if (auto sig = enumDecl->getGenericSignatureOfContext()) {
    allocIfaceType = GenericFunctionType::get(sig, selfInterfaceType,
                                              interfaceType,
                                              FunctionType::ExtInfo());
    initIfaceType = GenericFunctionType::get(sig, selfInitializerInterfaceType,
                                             interfaceType,
                                             FunctionType::ExtInfo());
  } else {
    allocIfaceType = allocType;
    initIfaceType = initType;
  }
  initDecl->setInterfaceType(allocIfaceType);
  initDecl->setInitializerInterfaceType(initIfaceType);
  initDecl->setAccessibility(enumDecl->getAccessibility());

  if (enumDecl->hasClangNode())
    tc.implicitlyDefinedFunctions.push_back(initDecl);

  enumDecl->addMember(initDecl);
  return initDecl;
}

ValueDecl *DerivedConformance::deriveRawRepresentable(TypeChecker &tc,
                                                      NominalTypeDecl *type,
                                                      ValueDecl *requirement) {
  // Check preconditions. These should already have been diagnosed by
  // type-checking but we may still get here after recovery.
  
  // The type must be an enum.
  auto enumDecl = dyn_cast<EnumDecl>(type);
  if (!enumDecl)
    return nullptr;
  
  // It must have a valid raw type.
  if (!enumDecl->hasRawType())
    return nullptr;
  if (!enumDecl->getInherited().empty() &&
      enumDecl->getInherited().front().isError())
    return nullptr;
  
  // There must be enum elements.
  if (enumDecl->getAllElements().empty())
    return nullptr;

  for (auto elt : enumDecl->getAllElements())
    tc.validateDecl(elt);

  if (requirement->getName() == tc.Context.Id_rawValue)
    return deriveRawRepresentable_raw(tc, enumDecl);
  
  if (requirement->getName() == tc.Context.Id_init)
    return deriveRawRepresentable_init(tc, enumDecl);

  if (requirement->getName() == tc.Context.Id_RawValue)
    return deriveRawRepresentable_Raw(tc, enumDecl);
  
  tc.diagnose(requirement->getLoc(),
              diag::broken_raw_representable_requirement);
  return nullptr;
}
