//===--- DerivedConformanceEquatableHashable.cpp - Derived Equatable & co -===//
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
//  This file implements implicit derivation of the Equatable and Hashable
//  protocols. (Comparable is similar enough in spirit that it would make
//  sense to live here too when we implement its derivation.)
//
//===----------------------------------------------------------------------===//

#include "TypeChecker.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Stmt.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Types.h"
#include "llvm/ADT/APInt.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/Support/raw_ostream.h"
#include "DerivedConformances.h"

using namespace swift;
using namespace DerivedConformance;

/// Common preconditions for Equatable and Hashable.
static bool canDeriveConformance(NominalTypeDecl *type) {
  // The type must be an enum.
  // TODO: Structs with Equatable/Hashable/Comparable members
  auto enumDecl = dyn_cast<EnumDecl>(type);
  if (!enumDecl)
    return false;
  
  // The enum must not have associated values.
  // TODO: Enums with Equatable/Hashable/Comparable payloads
  if (!enumDecl->hasOnlyCasesWithoutAssociatedValues())
    return false;
  
  return true;
}

/// Create AST statements which convert from an enum to an Int with a switch.
/// \p stmts The generated statements are appended to this vector.
/// \p parentDC Either an extension or the enum itself.
/// \p enumDecl The enum declaration.
/// \p enumVarDecl The enum input variable.
/// \p funcDecl The parent function.
/// \p indexName The name of the output variable.
/// \return A DeclRefExpr of the output variable (of type Int).
static DeclRefExpr *convertEnumToIndex(SmallVectorImpl<ASTNode> &stmts,
                                       DeclContext *parentDC,
                                       EnumDecl *enumDecl,
                                       VarDecl *enumVarDecl,
                                       AbstractFunctionDecl *funcDecl,
                                       const char *indexName) {
  ASTContext &C = enumDecl->getASTContext();
  Type enumType = enumVarDecl->getType();
  Type intType = C.getIntDecl()->getDeclaredType();

  auto indexVar = new (C) VarDecl(/*IsStatic*/false, /*IsLet*/false,
                                  /*IsCaptureList*/false, SourceLoc(),
                                  C.getIdentifier(indexName), intType,
                                  funcDecl);
  indexVar->setInterfaceType(intType);
  indexVar->setImplicit();

  // generate: var indexVar
  Pattern *indexPat = new (C) NamedPattern(indexVar, /*implicit*/ true);
  indexPat->setType(intType);
  indexPat = new (C) TypedPattern(indexPat, TypeLoc::withoutLoc(intType));
  indexPat->setType(intType);
  auto indexBind = PatternBindingDecl::create(C, SourceLoc(),
                                              StaticSpellingKind::None,
                                              SourceLoc(),
                                              indexPat, nullptr, funcDecl);

  unsigned index = 0;
  SmallVector<CaseStmt*, 4> cases;
  for (auto elt : enumDecl->getAllElements()) {
    // generate: case .<Case>:
    auto pat = new (C) EnumElementPattern(TypeLoc::withoutLoc(enumType),
                                          SourceLoc(), SourceLoc(),
                                          Identifier(), elt, nullptr);
    pat->setImplicit();

    auto labelItem = CaseLabelItem(/*IsDefault=*/false, pat, SourceLoc(),
                                   nullptr);

    // generate: indexVar = <index>
    llvm::SmallString<8> indexVal;
    APInt(32, index++).toString(indexVal, 10, /*signed*/ false);
    auto indexStr = C.AllocateCopy(indexVal);

    auto indexExpr = new (C) IntegerLiteralExpr(StringRef(indexStr.data(),
                                                indexStr.size()), SourceLoc(),
                                                /*implicit*/ true);
    auto indexRef = new (C) DeclRefExpr(indexVar, DeclNameLoc(),
                                        /*implicit*/true);
    auto assignExpr = new (C) AssignExpr(indexRef, SourceLoc(),
                                         indexExpr, /*implicit*/ true);
    auto body = BraceStmt::create(C, SourceLoc(), ASTNode(assignExpr),
                                  SourceLoc());
    cases.push_back(CaseStmt::create(C, SourceLoc(), labelItem,
                                     /*HasBoundDecls=*/false,
                                     SourceLoc(), body));
  }
  
  // generate: switch enumVar { }
  auto enumRef = new (C) DeclRefExpr(enumVarDecl, DeclNameLoc(),
                                      /*implicit*/true);
  auto switchStmt = SwitchStmt::create(LabeledStmtInfo(), SourceLoc(), enumRef,
                                       SourceLoc(), cases, SourceLoc(), C);
  
  stmts.push_back(indexBind);
  stmts.push_back(switchStmt);

  return new (C) DeclRefExpr(indexVar, DeclNameLoc(), /*implicit*/ true,
                             AccessSemantics::Ordinary, intType);
}

/// Derive the body for an '==' operator for an enum
static void deriveBodyEquatable_enum_eq(AbstractFunctionDecl *eqDecl) {
  auto parentDC = eqDecl->getDeclContext();
  ASTContext &C = parentDC->getASTContext();

  auto args = eqDecl->getParameterLists().back();
  auto aParam = args->get(0);
  auto bParam = args->get(1);

  auto boolTy = C.getBoolDecl()->getDeclaredType();

  auto enumDecl = cast<EnumDecl>(aParam->getType()->getAnyNominal());

  // Generate the conversion from the enums to integer indices.
  SmallVector<ASTNode, 6> statements;
  DeclRefExpr *aIndex = convertEnumToIndex(statements, parentDC, enumDecl,
                                           aParam, eqDecl, "index_a");
  DeclRefExpr *bIndex = convertEnumToIndex(statements, parentDC, enumDecl,
                                           bParam, eqDecl, "index_b");
  
  // Generate the compare of the indices.
  FuncDecl *cmpFunc = C.getEqualIntDecl();
  assert(cmpFunc && "should have a == for int as we already checked for it");
  
  auto fnType = cast<FunctionType>(cmpFunc->getInterfaceType()
      ->getCanonicalType());

  Expr *cmpFuncExpr;
  if (cmpFunc->getDeclContext()->isTypeContext()) {
    auto contextTy = cmpFunc->getDeclContext()->getSelfInterfaceType();
    Expr *base = TypeExpr::createImplicitHack(SourceLoc(), contextTy, C);
    Expr *ref = new (C) DeclRefExpr(cmpFunc, DeclNameLoc(), /*Implicit*/ true,
                                    AccessSemantics::Ordinary, fnType);

    fnType = cast<FunctionType>(fnType.getResult());
    cmpFuncExpr = new (C) DotSyntaxCallExpr(ref, SourceLoc(), base, fnType);
    cmpFuncExpr->setImplicit();
  } else {
    cmpFuncExpr = new (C) DeclRefExpr(cmpFunc, DeclNameLoc(),
                                      /*implicit*/ true,
                                      AccessSemantics::Ordinary,
                                      fnType);
  }

  auto tType = fnType.getInput();
  TupleExpr *abTuple = TupleExpr::create(C, SourceLoc(), { aIndex, bIndex },
                                         { }, { }, SourceLoc(),
                                         /*HasTrailingClosure*/ false,
                                         /*Implicit*/ true, tType);

  auto *cmpExpr = new (C) BinaryExpr(cmpFuncExpr, abTuple, /*implicit*/ true,
                                     boolTy);
  statements.push_back(new (C) ReturnStmt(SourceLoc(), cmpExpr));

  BraceStmt *body = BraceStmt::create(C, SourceLoc(), statements, SourceLoc());
  eqDecl->setBody(body);
}

/// Derive an '==' operator implementation for an enum.
static ValueDecl *
deriveEquatable_enum_eq(TypeChecker &tc, Decl *parentDecl, EnumDecl *enumDecl) {
  // enum SomeEnum<T...> {
  //   case A, B, C
  //
  //   @derived
  //   func ==(a: SomeEnum<T...>, b: SomeEnum<T...>) -> Bool {
  //     var index_a: Int
  //     switch a {
  //     case .A: index_a = 0
  //     case .B: index_a = 1
  //     case .C: index_a = 2
  //     }
  //     var index_b: Int
  //     switch b {
  //     case .A: index_b = 0
  //     case .B: index_b = 1
  //     case .C: index_b = 2
  //     }
  //     return index_a == index_b
  //   }
  
  ASTContext &C = tc.Context;
  
  auto parentDC = cast<DeclContext>(parentDecl);
  auto enumTy = parentDC->getDeclaredTypeInContext();
  auto enumIfaceTy = parentDC->getDeclaredInterfaceType();

  auto getParamDecl = [&](StringRef s) -> ParamDecl* {
    auto *param = new (C) ParamDecl(/*isLet*/true, SourceLoc(), SourceLoc(),
                                    Identifier(), SourceLoc(), C.getIdentifier(s),
                                    enumTy, parentDC);
    param->setInterfaceType(enumIfaceTy);
    return param;
  };

  auto selfDecl = ParamDecl::createSelf(SourceLoc(), parentDC,
                                        /*isStatic=*/true);
  
  ParameterList *params[] = {
    ParameterList::createWithoutLoc(selfDecl),
    ParameterList::create(C, {
        getParamDecl("a"),
        getParamDecl("b")
    })
  };

  auto boolTy = C.getBoolDecl()->getDeclaredType();

  DeclName name(C, C.Id_EqualsOperator, params[1]);
  auto eqDecl =
    FuncDecl::create(C, /*StaticLoc=*/SourceLoc(),
                     StaticSpellingKind::KeywordStatic,
                     /*FuncLoc=*/SourceLoc(), name, /*NameLoc=*/SourceLoc(),
                     /*Throws=*/false, /*ThrowsLoc=*/SourceLoc(),
                     /*AccessorKeywordLoc=*/SourceLoc(),
                     /*GenericParams=*/nullptr,
                     params,
                     TypeLoc::withoutLoc(boolTy),
                     parentDC);
  eqDecl->setImplicit();
  eqDecl->getAttrs().add(new (C) InfixAttr(/*implicit*/false));
  auto op = C.getStdlibModule()->lookupInfixOperator(C.Id_EqualsOperator);
  if (!op) {
    tc.diagnose(parentDecl->getLoc(),
                diag::broken_equatable_eq_operator);
    return nullptr;
  }
  if (!C.getEqualIntDecl()) {
    tc.diagnose(parentDecl->getLoc(), diag::no_equal_overload_for_int);
    return nullptr;
  }

  eqDecl->setOperatorDecl(op);
  eqDecl->setBodySynthesizer(&deriveBodyEquatable_enum_eq);

  // Compute the type.
  Type paramsTy = params[1]->getType(tc.Context);

  // Compute the interface type.
  Type interfaceTy;
  Type selfIfaceTy = eqDecl->computeInterfaceSelfType();
  if (auto genericSig = parentDC->getGenericSignatureOfContext()) {
    eqDecl->setGenericEnvironment(parentDC->getGenericEnvironmentOfContext());

    Type enumIfaceTy = parentDC->getDeclaredInterfaceType();
    TupleTypeElt ifaceParamElts[] = {
      enumIfaceTy, enumIfaceTy,
    };
    auto ifaceParamsTy = TupleType::get(ifaceParamElts, C);
    interfaceTy = FunctionType::get(ifaceParamsTy, boolTy,
                                    AnyFunctionType::ExtInfo());
    interfaceTy = GenericFunctionType::get(genericSig, selfIfaceTy, interfaceTy,
                                           AnyFunctionType::ExtInfo());
  } else {
    interfaceTy = FunctionType::get(paramsTy, boolTy);
    interfaceTy = FunctionType::get(selfIfaceTy, interfaceTy);
  }
  eqDecl->setInterfaceType(interfaceTy);

  // Since we can't insert the == operator into the same FileUnit as the enum,
  // itself, we have to give it at least internal access.
  eqDecl->setAccessibility(std::max(enumDecl->getFormalAccess(),
                                    Accessibility::Internal));

  // If the enum was not imported, the derived conformance is either from the
  // enum itself or an extension, in which case we will emit the declaration
  // normally.
  if (enumDecl->hasClangNode())
    tc.Context.addExternalDecl(eqDecl);
  
  // Add the operator to the parent scope.
  cast<IterableDeclContext>(parentDecl)->addMember(eqDecl);

  return eqDecl;
}

ValueDecl *DerivedConformance::deriveEquatable(TypeChecker &tc,
                                               Decl *parentDecl,
                                               NominalTypeDecl *type,
                                               ValueDecl *requirement) {
  // Check that we can actually derive Equatable for this type.
  if (!canDeriveConformance(type))
    return nullptr;

  // Build the necessary decl.
  if (requirement->getBaseName() == "==") {
    if (auto theEnum = dyn_cast<EnumDecl>(type))
      return deriveEquatable_enum_eq(tc, parentDecl, theEnum);
    else
      llvm_unreachable("todo");
  }
  tc.diagnose(requirement->getLoc(),
              diag::broken_equatable_requirement);
  return nullptr;
}

static void
deriveBodyHashable_enum_hashValue(AbstractFunctionDecl *hashValueDecl) {
  auto parentDC = hashValueDecl->getDeclContext();
  ASTContext &C = parentDC->getASTContext();

  auto enumDecl = parentDC->getAsEnumOrEnumExtensionContext();
  SmallVector<ASTNode, 3> statements;
  auto selfDecl = hashValueDecl->getImplicitSelfDecl();

  DeclRefExpr *indexRef = convertEnumToIndex(statements, parentDC, enumDecl,
                                             selfDecl, hashValueDecl, "index");
  
  auto memberRef = new (C) UnresolvedDotExpr(indexRef, SourceLoc(),
                                             C.Id_hashValue,
                                             DeclNameLoc(),
                                             /*implicit*/true);
  auto returnStmt = new (C) ReturnStmt(SourceLoc(), memberRef);
  statements.push_back(returnStmt);

  auto body = BraceStmt::create(C, SourceLoc(), statements, SourceLoc());
  hashValueDecl->setBody(body);
}

/// Derive a 'hashValue' implementation for an enum.
static ValueDecl *
deriveHashable_enum_hashValue(TypeChecker &tc, Decl *parentDecl,
                              EnumDecl *enumDecl) {
  // enum SomeEnum {
  //   case A, B, C
  //   @derived var hashValue: Int {
  //     var index: Int
  //     switch self {
  //     case A:
  //       index = 0
  //     case B:
  //       index = 1
  //     case C:
  //       index = 2
  //     }
  //     return index.hashValue
  //   }
  // }
  ASTContext &C = tc.Context;
  
  auto parentDC = cast<DeclContext>(parentDecl);
  Type intType = C.getIntDecl()->getDeclaredType();
  
  // We can't form a Hashable conformance if Int isn't Hashable or
  // ExpressibleByIntegerLiteral.
  if (!tc.conformsToProtocol(intType,C.getProtocol(KnownProtocolKind::Hashable),
                             enumDecl, None)) {
    tc.diagnose(enumDecl->getLoc(), diag::broken_int_hashable_conformance);
    return nullptr;
  }

  ProtocolDecl *intLiteralProto =
      C.getProtocol(KnownProtocolKind::ExpressibleByIntegerLiteral);
  if (!tc.conformsToProtocol(intType, intLiteralProto, enumDecl, None)) {
    tc.diagnose(enumDecl->getLoc(),
                diag::broken_int_integer_literal_convertible_conformance);
    return nullptr;
  }
  
  auto selfDecl = ParamDecl::createSelf(SourceLoc(), parentDC);
  
  ParameterList *params[] = {
    ParameterList::createWithoutLoc(selfDecl),
    ParameterList::createEmpty(C)
  };
  
  FuncDecl *getterDecl =
      FuncDecl::create(C, /*StaticLoc=*/SourceLoc(), StaticSpellingKind::None,
                       /*FuncLoc=*/SourceLoc(),
                       Identifier(), /*NameLoc=*/SourceLoc(),
                       /*Throws=*/false, /*ThrowsLoc=*/SourceLoc(),
                       /*AccessorKeywordLoc=*/SourceLoc(),
                       /*GenericParams=*/nullptr, params,
                       TypeLoc::withoutLoc(intType), parentDC);
  getterDecl->setImplicit();
  getterDecl->setBodySynthesizer(deriveBodyHashable_enum_hashValue);

  // Compute the type of hashValue().
  Type methodType = FunctionType::get(TupleType::getEmpty(tc.Context), intType);

  // Compute the interface type of hashValue().
  Type interfaceType;
  Type selfIfaceType = getterDecl->computeInterfaceSelfType();
  if (auto sig = parentDC->getGenericSignatureOfContext()) {
    getterDecl->setGenericEnvironment(parentDC->getGenericEnvironmentOfContext());
    interfaceType = GenericFunctionType::get(sig, selfIfaceType, methodType,
                                             AnyFunctionType::ExtInfo());
  } else
    interfaceType = FunctionType::get(selfIfaceType, methodType);
  
  getterDecl->setInterfaceType(interfaceType);
  getterDecl->setAccessibility(std::max(Accessibility::Internal,
                                        enumDecl->getFormalAccess()));

  // If the enum was not imported, the derived conformance is either from the
  // enum itself or an extension, in which case we will emit the declaration
  // normally.
  if (enumDecl->hasClangNode())
    tc.Context.addExternalDecl(getterDecl);

  // Create the property.
  VarDecl *hashValueDecl = new (C) VarDecl(/*IsStatic*/false, /*IsLet*/false,
                                           /*IsCaptureList*/false, SourceLoc(),
                                           C.Id_hashValue, intType, parentDC);
  hashValueDecl->setImplicit();
  hashValueDecl->setInterfaceType(intType);
  hashValueDecl->makeComputed(SourceLoc(), getterDecl,
                              nullptr, nullptr, SourceLoc());
  hashValueDecl->setAccessibility(getterDecl->getFormalAccess());

  Pattern *hashValuePat = new (C) NamedPattern(hashValueDecl, /*implicit*/true);
  hashValuePat->setType(intType);
  hashValuePat
    = new (C) TypedPattern(hashValuePat, TypeLoc::withoutLoc(intType),
                           /*implicit*/ true);
  hashValuePat->setType(intType);

  auto patDecl = PatternBindingDecl::create(C, SourceLoc(),
                                            StaticSpellingKind::None,
                                            SourceLoc(), hashValuePat, nullptr,
                                            parentDC);
  patDecl->setImplicit();

  auto dc = cast<IterableDeclContext>(parentDecl);
  dc->addMember(getterDecl);
  dc->addMember(hashValueDecl);
  dc->addMember(patDecl);
  return hashValueDecl;
}

ValueDecl *DerivedConformance::deriveHashable(TypeChecker &tc,
                                              Decl *parentDecl,
                                              NominalTypeDecl *type,
                                              ValueDecl *requirement) {
  // Check that we can actually derive Hashable for this type.
  if (!canDeriveConformance(type))
    return nullptr;
  
  // Build the necessary decl.
  if (requirement->getBaseName() == "hashValue") {
    if (auto theEnum = dyn_cast<EnumDecl>(type))
      return deriveHashable_enum_hashValue(tc, parentDecl, theEnum);
    else
      llvm_unreachable("todo");
  }
  tc.diagnose(requirement->getLoc(),
              diag::broken_hashable_requirement);
  return nullptr;
}
