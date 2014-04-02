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

void DerivedConformance::_insertMemberDecl(NominalTypeDecl *scope, Decl *member)
{
  auto oldMembers = scope->getMembers();
  auto oldSize = oldMembers.size();
  auto newMembers = scope->getASTContext().Allocate<Decl*>(oldSize + 1);

  std::move(oldMembers.begin(), oldMembers.end(), newMembers.begin());
  newMembers[oldSize] = member;
  
  scope->setMembers(newMembers, scope->getBraces());
}

void DerivedConformance::_insertOperatorDecl(NominalTypeDecl *scope,
                                             Decl *member) {
  // Find the module.
  auto &C = scope->getASTContext();
  auto mod = scope->getModuleContext();

  // Add it to the module in a DerivedFileUnit.
  mod->getDerivedFileUnit().addDerivedDecl(cast<FuncDecl>(member));

  // Add it as a derived global decl to the nominal type.
  auto oldDerived = scope->getDerivedGlobalDecls();
  auto oldSize = oldDerived.size();
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
  } else if (auto charLit = dyn_cast<CharacterLiteralExpr>(expr)) {
    clone = new (C) CharacterLiteralExpr(charLit->getValue(), SourceLoc());
  } else if (auto stringLit = dyn_cast<StringLiteralExpr>(expr)) {
    clone = new (C) StringLiteralExpr(stringLit->getValue(), SourceLoc());
  } else if (auto floatLit = dyn_cast<FloatLiteralExpr>(expr)) {
    clone = new (C) FloatLiteralExpr(floatLit->getText(), SourceLoc(),
                                     /*implicit*/ true);
  } else {
    llvm_unreachable("invalid raw literal expr");
  }
  clone->setImplicit();
  return clone;
}

static TypeDecl *deriveRawRepresentable_RawType(TypeChecker &tc,
                                                EnumDecl *enumDecl) {
  // enum SomeEnum : SomeType {
  //   typealias [derived] RawType = SomeType
  // }
  ASTContext &C = tc.Context;

  auto rawInterfaceType = enumDecl->getRawType();
  auto rawType = ArchetypeBuilder::mapTypeIntoContext(enumDecl,
                                                      rawInterfaceType);
  auto rawTypeDecl = new (C) TypeAliasDecl(SourceLoc(),
                                   C.getIdentifier("RawType"),
                                   SourceLoc(),
                                   TypeLoc::withoutLoc(rawType),
                                   enumDecl);
  rawTypeDecl->setImplicit();
  rawTypeDecl->setType(rawType);
  rawTypeDecl->setInterfaceType(rawInterfaceType);
  return insertMemberDecl(enumDecl, rawTypeDecl);
}

static void deriveBodyRawRepresentable_toRaw(AbstractFunctionDecl *toRawDecl) {
  auto enumDecl = cast<EnumDecl>(toRawDecl->getDeclContext());
  Type enumType = enumDecl->getDeclaredTypeInContext();
  ASTContext &C = enumDecl->getASTContext();

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
  auto switchStmt = SwitchStmt::create(SourceLoc(), selfRef,
                                       SourceLoc(), cases, SourceLoc(), C);
  auto body = BraceStmt::create(C, SourceLoc(),
                                ASTNode(switchStmt),
                                SourceLoc());
  toRawDecl->setBody(body);
}

static FuncDecl *deriveRawRepresentable_toRaw(TypeChecker &tc,
                                              EnumDecl *enumDecl) {
  // enum SomeEnum : SomeType {
  //   case A = 111, B = 222
  //   func [derived] toRaw() -> SomeType {
  //     switch self {
  //     case A:
  //       return 111
  //     case B:
  //       return 222
  //     }
  //   }
  // }
  ASTContext &C = tc.Context;
  
  auto rawInterfaceType = enumDecl->getRawType();
  auto rawType = ArchetypeBuilder::mapTypeIntoContext(enumDecl,
                                                      rawInterfaceType);
  Type enumType = enumDecl->getDeclaredTypeInContext();
  
  VarDecl *selfDecl = new (C) VarDecl(/*static*/ false, /*IsLet*/true,
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
  
  FuncDecl *toRawDecl =
      FuncDecl::create(C, SourceLoc(), StaticSpellingKind::None, SourceLoc(),
                       C.getIdentifier("toRaw"), SourceLoc(), nullptr, Type(),
                       params, params, TypeLoc::withoutLoc(rawType), enumDecl);
  toRawDecl->setImplicit();
  toRawDecl->setBodySynthesizer(&deriveBodyRawRepresentable_toRaw);

  // Compute the type of toRaw().
  GenericParamList *genericParams = nullptr;
  Type type = FunctionType::get(TupleType::getEmpty(tc.Context), rawType);
  Type selfType = toRawDecl->computeSelfType(&genericParams);
  if (genericParams)
    type = PolymorphicFunctionType::get(selfType, type, genericParams);
  else
    type = FunctionType::get(selfType, type);
  toRawDecl->setType(type);
  toRawDecl->setBodyResultType(rawType);

  // Compute the interface type of toRaw();
  Type interfaceType = FunctionType::get(TupleType::getEmpty(tc.Context),
                                         rawInterfaceType);
  Type selfInterfaceType = toRawDecl->computeInterfaceSelfType(false);
  if (auto sig = enumDecl->getGenericSignatureOfContext())
    interfaceType = GenericFunctionType::get(sig, selfInterfaceType,
                                             interfaceType,
                                             FunctionType::ExtInfo());
  else
    interfaceType = type;
  toRawDecl->setInterfaceType(interfaceType);

  tc.implicitlyDefinedFunctions.push_back(toRawDecl);

  return insertMemberDecl(enumDecl, toRawDecl);
}

static void
deriveBodyRawRepresentable_frowRaw(AbstractFunctionDecl *fromRawDecl) {
  auto enumDecl = cast<EnumDecl>(fromRawDecl->getDeclContext());
  ASTContext &C = enumDecl->getASTContext();

  Type enumType = enumDecl->getDeclaredTypeInContext();
  Type enumMetaType = MetatypeType::get(enumType);

  SmallVector<CaseStmt*, 4> cases;
  for (auto elt : enumDecl->getAllElements()) {
    auto litExpr = cloneRawLiteralExpr(C, elt->getRawValueExpr());
    auto litPat = new (C) ExprPattern(litExpr, /*isResolved*/ true,
                                      nullptr, nullptr);
    litPat->setImplicit();

    auto labelItem =
      CaseLabelItem(/*IsDefault=*/false, litPat, SourceLoc(), nullptr);

    auto eltRef = new (C) DeclRefExpr(elt, SourceLoc(), /*implicit*/true);
    auto metaTyRef = new (C) MetatypeExpr(nullptr, SourceLoc(), enumMetaType);
    auto returnExpr = new (C) DotSyntaxCallExpr(eltRef, SourceLoc(), metaTyRef);
    auto returnStmt = new (C) ReturnStmt(SourceLoc(), returnExpr);

    auto body = BraceStmt::create(C, SourceLoc(),
                                  ASTNode(returnStmt), SourceLoc());

    cases.push_back(CaseStmt::create(C, SourceLoc(), labelItem,
                                     /*HasBoundDecls=*/false, SourceLoc(),
                                     body));
  }

  auto anyPat = new (C) AnyPattern(SourceLoc());
  anyPat->setImplicit();
  auto dfltLabelItem =
    CaseLabelItem(/*IsDefault=*/true, anyPat, SourceLoc(), nullptr);

  auto optionalRef = new (C) DeclRefExpr(C.getOptionalDecl(),
                                         SourceLoc(), /*implicit*/true);
  auto emptyArgs = new (C) TupleExpr(SourceLoc(), SourceLoc(),
                                     /*implicit*/ true);
  auto dfltReturnExpr = new (C) CallExpr(optionalRef, emptyArgs,
                                         /*implicit*/ true);
  auto dfltReturnStmt = new (C) ReturnStmt(SourceLoc(), dfltReturnExpr);
  auto dfltBody = BraceStmt::create(C, SourceLoc(),
                                    ASTNode(dfltReturnStmt), SourceLoc());
  cases.push_back(CaseStmt::create(C, SourceLoc(), dfltLabelItem,
                                   /*HasBoundDecls=*/false, SourceLoc(),
                                   dfltBody));

  Pattern *args = fromRawDecl->getBodyParamPatterns().back();
  auto rawArgPattern = cast<NamedPattern>(args->getSemanticsProvidingPattern());
  auto rawDecl = rawArgPattern->getDecl();
  auto rawRef = new (C) DeclRefExpr(rawDecl, SourceLoc(), /*implicit*/true);
  auto switchStmt = SwitchStmt::create(SourceLoc(), rawRef, SourceLoc(),
                                       cases, SourceLoc(), C);
  auto body = BraceStmt::create(C, SourceLoc(),
                                ASTNode(switchStmt),
                                SourceLoc());
  fromRawDecl->setBody(body);
}

static FuncDecl *deriveRawRepresentable_fromRaw(TypeChecker &tc,
                                                EnumDecl *enumDecl) {
  // enum SomeEnum : SomeType {
  //   case A = 111, B = 222
  //   @derived
  //   static func fromRaw(raw: SomeType) -> SomeEnum? {
  //     switch raw {
  //     case 111:
  //       return A
  //     case 222:
  //       return B
  //     default:
  //       return Optional()
  //     }
  //   }
  // }

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

  if (!tc.conformsToProtocol(rawType, equatableProto, enumDecl)) {
    SourceLoc loc = enumDecl->getInherited()[0].getSourceRange().Start;
    tc.diagnose(loc, diag::enum_raw_type_not_equatable, rawType);
    return nullptr;
  }

  Type enumType = enumDecl->getDeclaredTypeInContext();
  Type enumMetaType = MetatypeType::get(enumType);

  VarDecl *selfDecl = new (C) VarDecl(/*static*/ false, /*IsVal*/true,
                                      SourceLoc(),
                                      C.Id_self,
                                      enumMetaType,
                                      enumDecl);
  selfDecl->setImplicit();
  Pattern *selfParam = new (C) NamedPattern(selfDecl, /*implicit*/ true);
  selfParam->setType(enumMetaType);
  selfParam = new (C) TypedPattern(selfParam,
                                   TypeLoc::withoutLoc(enumMetaType));
  selfParam->setType(enumMetaType);
  selfParam->setImplicit();

  VarDecl *rawDecl = new (C) VarDecl(/*static*/ false, /*IsVal*/true,
                                     SourceLoc(),
                                     C.getIdentifier("raw"),
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
  
  Pattern *argParams[] = {selfParam->clone(C, Pattern::Implicit),
                          rawParam->clone(C, Pattern::Implicit)};
  Pattern *bodyParams[] = {selfParam, rawParam};
  auto retTy = OptionalType::get(enumType);
  auto fromRawDecl = FuncDecl::create(
      C, SourceLoc(), StaticSpellingKind::None, SourceLoc(),
      C.getIdentifier("fromRaw"), SourceLoc(), nullptr, Type(), argParams,
      bodyParams, TypeLoc::withoutLoc(retTy), enumDecl);
  fromRawDecl->setStatic();
  fromRawDecl->setImplicit();
  fromRawDecl->setBodySynthesizer(&deriveBodyRawRepresentable_frowRaw);

  // Compute the type of fromRaw().
  GenericParamList *genericParams = nullptr;
  Type type = FunctionType::get(rawType, retTy);
  Type selfType = fromRawDecl->computeSelfType(&genericParams);
  if (genericParams)
    type = PolymorphicFunctionType::get(selfType, type, genericParams);
  else
    type = FunctionType::get(selfType, type);
  fromRawDecl->setType(type);
  fromRawDecl->setBodyResultType(retTy);

  // Compute the interface type of fromRaw();
  Type retInterfaceType
    = OptionalType::get(enumDecl->getDeclaredInterfaceType());
  Type interfaceType = FunctionType::get(rawInterfaceType, retInterfaceType);
  Type selfInterfaceType = fromRawDecl->computeInterfaceSelfType(false);
  if (auto sig = enumDecl->getGenericSignatureOfContext())
    interfaceType = GenericFunctionType::get(sig, selfInterfaceType,
                                             interfaceType,
                                             FunctionType::ExtInfo());
  else
    interfaceType = type;
  fromRawDecl->setInterfaceType(interfaceType);

  tc.implicitlyDefinedFunctions.push_back(fromRawDecl);

  return insertMemberDecl(enumDecl, fromRawDecl);
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
  
  // It must have a raw type.
  auto rawType = enumDecl->getRawType();
  if (!rawType)
    return nullptr;
  
  // There must be enum elements, and they must all have type-checked raw
  // values.
  if (enumDecl->getAllElements().empty())
    return nullptr;

  // Map the interface type into the context of the enum.
  rawType = ArchetypeBuilder::mapTypeIntoContext(enumDecl, rawType);

  for (auto elt : enumDecl->getAllElements()) {
    tc.validateDecl(elt);
    if (!elt->getTypeCheckedRawValueExpr()
        || !elt->getTypeCheckedRawValueExpr()->getType()->isEqual(rawType))
      return nullptr;
  }
  
  // Start building the conforming decls.
  if (requirement->getName().str() == "toRaw")
    return deriveRawRepresentable_toRaw(tc, enumDecl);
  
  if (requirement->getName().str() == "fromRaw")
    return deriveRawRepresentable_fromRaw(tc, enumDecl);

  if (requirement->getName().str() == "RawType")
    return deriveRawRepresentable_RawType(tc, enumDecl);
  
  tc.diagnose(requirement->getLoc(),
              diag::broken_raw_representable_requirement);
  return nullptr;
}
