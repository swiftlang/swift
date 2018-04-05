//===--- DerivedConformanceCodingKey.cpp - Derived CodingKey --------------===//
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
// This file implements explicit derivation of the CodingKey protocol for an
// enum.
//
//===----------------------------------------------------------------------===//

#include "TypeChecker.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/Pattern.h"
#include "swift/AST/Stmt.h"
#include "swift/AST/Types.h"
#include "DerivedConformances.h"

using namespace swift;
using namespace DerivedConformance;

/// Sets the body of the given function to `return nil`.
///
/// \param funcDecl The function whose body to set.
static void deriveNilReturn(AbstractFunctionDecl *funcDecl) {
  auto *parentDC = funcDecl->getDeclContext();
  auto &C = parentDC->getASTContext();

  auto *nilExpr = new (C) NilLiteralExpr(SourceLoc(), /*Implicit=*/true);
  auto *returnStmt = new (C) ReturnStmt(SourceLoc(), nilExpr);
  auto *body = BraceStmt::create(C, SourceLoc(), ASTNode(returnStmt),
                                 SourceLoc());
  funcDecl->setBody(body);
}

/// Sets the body of the given function to `return self.rawValue`.
///
/// \param funcDecl The function whose body to set.
static void deriveRawValueReturn(AbstractFunctionDecl *funcDecl) {
  auto *parentDC = funcDecl->getDeclContext();
  auto &C = parentDC->getASTContext();

  auto *selfRef = createSelfDeclRef(funcDecl);
  auto *memberRef = new (C) UnresolvedDotExpr(selfRef, SourceLoc(),
                                              C.Id_rawValue, DeclNameLoc(),
                                              /*Implicit=*/true);

  auto *returnStmt = new (C) ReturnStmt(SourceLoc(), memberRef);
  auto *body = BraceStmt::create(C, SourceLoc(), ASTNode(returnStmt),
                                 SourceLoc());
  funcDecl->setBody(body);
}

/// Sets the body of the given function to `self.init(rawValue:)`, passing along
/// the parameter of the given constructor.
///
/// \param initDecl The constructor whose body to set.
static void deriveRawValueInit(AbstractFunctionDecl *initDecl) {
  auto *parentDC = initDecl->getDeclContext();
  auto &C = parentDC->getASTContext();

  // Get the param from init({string,int}Value:). self is the first param in the
  // list; stringValue is the second.
  auto *valueParam = initDecl->getParameterList(1)->get(0);
  auto *valueParamExpr = new (C) DeclRefExpr(ConcreteDeclRef(valueParam),
                                             DeclNameLoc(), /*Implicit=*/true);

  // rawValue param to init(rawValue:)
  auto *rawValueDecl = new (C) ParamDecl(
      VarDecl::Specifier::Default, SourceLoc(), SourceLoc(), C.Id_rawValue,
      SourceLoc(), C.Id_rawValue, valueParam->getType(), parentDC);
  rawValueDecl->setInterfaceType(C.getIntDecl()->getDeclaredType());
  rawValueDecl->setImplicit();
  auto *paramList = ParameterList::createWithoutLoc(rawValueDecl);

  // init(rawValue:) constructor name
  DeclName ctorName(C, DeclBaseName::createConstructor(), paramList);

  // self.init(rawValue:) expr
  auto *selfRef = createSelfDeclRef(initDecl);
  auto *initExpr = new (C) UnresolvedDotExpr(selfRef, SourceLoc(), ctorName,
                                             DeclNameLoc(), /*Implicit=*/true);

  // Bind the value param in self.init(rawValue: {string,int}Value).
  Expr *args[1] = {valueParamExpr};
  Identifier argLabels[1] = {C.Id_rawValue};
  auto *callExpr = CallExpr::createImplicit(C, initExpr, C.AllocateCopy(args),
                                            C.AllocateCopy(argLabels));

  auto *body = BraceStmt::create(C, SourceLoc(), ASTNode(callExpr),
                                 SourceLoc());
  initDecl->setBody(body);
}

/// Synthesizes a constructor declaration with the given parameter name and
/// type.
///
/// \param tc The type checker to use in synthesizing the constructor.
///
/// \param parentDecl The parent declaration of the enum.
///
/// \param enumDecl The enum on which to synthesize the constructor.
///
/// \param paramType The type of the parameter.
///
/// \param paramName The name of the parameter.
///
/// \param synthesizer A lambda to call to set the constructor's body.
template <typename Synthesizer>
static ValueDecl *deriveInitDecl(TypeChecker &tc, Decl *parentDecl,
                                 EnumDecl *enumDecl, Type paramType,
                                 Identifier paramName,
                                 const Synthesizer &synthesizer) {
  auto &C = tc.Context;
  auto *parentDC = cast<DeclContext>(parentDecl);

  // rawValue
  auto *rawDecl =
      new (C) ParamDecl(VarDecl::Specifier::Default, SourceLoc(), SourceLoc(),
                        paramName, SourceLoc(), paramName, paramType, parentDC);
  rawDecl->setInterfaceType(paramType);
  rawDecl->setImplicit();

  // init(rawValue:) name
  auto *paramList = ParameterList::createWithoutLoc(rawDecl);
  DeclName name(C, DeclBaseName::createConstructor(), paramList);

  // init(rawValue:) decl
  auto *selfDecl = ParamDecl::createSelf(SourceLoc(), parentDC,
                                         /*static*/false, /*inout*/true);
  auto *initDecl =
    new (C) ConstructorDecl(name, SourceLoc(),
                            /*Failability=*/OTK_Optional,
                            /*FailabilityLoc=*/SourceLoc(),
                            /*Throws=*/false, /*ThrowsLoc=*/SourceLoc(),
                            selfDecl, paramList,
                            /*GenericParams=*/nullptr, parentDC);

  initDecl->setImplicit();

  // Synthesize the body.
  synthesizer(initDecl);

  // Compute the type of the initializer.
  TupleTypeElt element(paramType, paramName);
  TupleTypeElt interfaceElement(paramType, paramName);
  auto interfaceArgType = TupleType::get(interfaceElement, C);

  // Compute the interface type of the initializer.
  Type retInterfaceType =
      OptionalType::get(parentDC->getDeclaredInterfaceType());
  Type interfaceType = FunctionType::get(interfaceArgType, retInterfaceType);
  auto selfParam = computeSelfParam(initDecl);
  auto initSelfParam = computeSelfParam(initDecl, /*init*/ true);

  Type allocIfaceType;
  Type initIfaceType;
  if (auto sig = parentDC->getGenericSignatureOfContext()) {
    initDecl->setGenericEnvironment(parentDC->getGenericEnvironmentOfContext());

    allocIfaceType = GenericFunctionType::get(sig, {selfParam},
                                              interfaceType,
                                              FunctionType::ExtInfo());
    initIfaceType = GenericFunctionType::get(sig, {initSelfParam},
                                             interfaceType,
                                             FunctionType::ExtInfo());
  } else {
    allocIfaceType = FunctionType::get({selfParam},
                                       interfaceType, FunctionType::ExtInfo());
    initIfaceType = FunctionType::get({initSelfParam},
                                      interfaceType, FunctionType::ExtInfo());
  }
  initDecl->setInterfaceType(allocIfaceType);
  initDecl->setInitializerInterfaceType(initIfaceType);
  initDecl->setAccess(enumDecl->getFormalAccess());
  initDecl->setValidationStarted();

  tc.Context.addSynthesizedDecl(initDecl);

  cast<IterableDeclContext>(parentDecl)->addMember(initDecl);
  return initDecl;
}

/// Synthesizes a read-only computed property with a given type and name.
///
/// \param tc The type checker to use in synthesizing the property.
///
/// \param parentDecl The parent declaration of the enum.
///
/// \param enumDecl The enum on which to synthesize the property.
///
/// \param type The type of the property.
///
/// \param name The name of the property.
///
/// \param synthesizer A lambda to call to set the property's getter.
template <typename Synthesizer>
static ValueDecl *deriveProperty(TypeChecker &tc, Decl *parentDecl,
                                 EnumDecl *enumDecl, Type type, Identifier name,
                                 const Synthesizer &synthesizer) {
  // Define the property.
  VarDecl *propDecl;
  PatternBindingDecl *pbDecl;
  std::tie(propDecl, pbDecl)
    = declareDerivedProperty(tc, parentDecl, enumDecl, name, type, type,
                             /*isStatic=*/false, /*isFinal=*/false);

  // Define the getter.
  auto *getterDecl = addGetterToReadOnlyDerivedProperty(tc, propDecl, type);

  // Synthesize the body.
  synthesizer(getterDecl);

  auto *dc = cast<IterableDeclContext>(parentDecl);
  dc->addMember(getterDecl);
  dc->addMember(propDecl);
  dc->addMember(pbDecl);
  return propDecl;
}

/// Sets the body of the given function to return a string value based on
/// switching on `self`.
///
/// \param strValDecl The function whose body to set.
static void
deriveBodyCodingKey_enum_stringValue(AbstractFunctionDecl *strValDecl) {
  // enum SomeEnum {
  //   case A, B, C
  //   @derived var stringValue: String {
  //     switch self {
  //       case A:
  //         return "A"
  //       case B:
  //         return "B"
  //       case C:
  //         return "C"
  //     }
  //   }
  // }
  auto *parentDC = strValDecl->getDeclContext();
  auto &C = parentDC->getASTContext();

  auto *enumDecl = parentDC->getAsEnumOrEnumExtensionContext();
  Type enumType = parentDC->getDeclaredTypeInContext();

  BraceStmt *body = nullptr;
  auto elements = enumDecl->getAllElements();
  if (elements.empty() /* empty enum */) {
    // return ""
    auto *emptyStringExpr = new (C) StringLiteralExpr("", SourceRange(),
                                                      /*Implicit=*/true);
    auto *returnStmt = new (C) ReturnStmt(SourceLoc(), emptyStringExpr);
    body = BraceStmt::create(C, SourceLoc(), ASTNode(returnStmt),
                             SourceLoc());
  } else {
    SmallVector<ASTNode, 4> cases;
    for (auto *elt : elements) {
      auto *pat = new (C) EnumElementPattern(TypeLoc::withoutLoc(enumType),
                                             SourceLoc(), SourceLoc(),
                                             Identifier(), elt, nullptr);
      pat->setImplicit();

      auto labelItem = CaseLabelItem(pat);

      auto *caseValue = new (C) StringLiteralExpr(elt->getNameStr(),
                                                  SourceRange(),
                                                  /*Implicit=*/true);
      auto *returnStmt = new (C) ReturnStmt(SourceLoc(), caseValue);
      auto *caseBody = BraceStmt::create(C, SourceLoc(), ASTNode(returnStmt),
                                         SourceLoc());
      cases.push_back(CaseStmt::create(C, SourceLoc(), labelItem,
                                       /*HasBoundDecls=*/false, SourceLoc(),
                                       SourceLoc(), caseBody));
    }

    auto *selfRef = createSelfDeclRef(strValDecl);
    auto *switchStmt = SwitchStmt::create(LabeledStmtInfo(), SourceLoc(),
                                          selfRef, SourceLoc(), cases,
                                          SourceLoc(), C);
    body = BraceStmt::create(C, SourceLoc(), ASTNode(switchStmt), SourceLoc());
  }

  strValDecl->setBody(body);
}

/// Sets the body of the given constructor to initialize `self` based on the
/// value of the given string param.
///
/// \param initDecl The function whose body to set.
static void
deriveBodyCodingKey_init_stringValue(AbstractFunctionDecl *initDecl) {
  // enum SomeEnum {
  //   case A, B, C
  //   @derived init?(stringValue: String) {
  //     switch stringValue {
  //     case "A":
  //       self = .A
  //     case "B":
  //       self = .B
  //     case "C":
  //       self = .C
  //     default:
  //       return nil
  //     }
  //   }
  // }
  auto *parentDC = initDecl->getDeclContext();
  auto &C = parentDC->getASTContext();

  auto *enumDecl = parentDC->getAsEnumOrEnumExtensionContext();
  Type enumType = parentDC->getDeclaredTypeInContext();

  auto elements = enumDecl->getAllElements();
  if (elements.empty() /* empty enum */) {
    deriveNilReturn(initDecl);
    return;
  }

  auto *selfRef = createSelfDeclRef(initDecl);
  SmallVector<ASTNode, 4> cases;
  for (auto *elt : elements) {
    auto *litExpr = new (C) StringLiteralExpr(elt->getNameStr(), SourceRange(),
                                              /*Implicit=*/true);
    auto *litPat = new (C) ExprPattern(litExpr, /*IsResolved=*/true, nullptr,
                                       nullptr);
    litPat->setImplicit();

    auto labelItem = CaseLabelItem(litPat);

    auto *eltRef = new (C) DeclRefExpr(elt, DeclNameLoc(), /*Implicit=*/true);
    auto *metaTyRef = TypeExpr::createImplicit(enumType, C);
    auto *valueExpr = new (C) DotSyntaxCallExpr(eltRef, SourceLoc(), metaTyRef);

    auto *assignment = new (C) AssignExpr(selfRef, SourceLoc(), valueExpr,
                                          /*Implicit=*/true);

    auto *body = BraceStmt::create(C, SourceLoc(), ASTNode(assignment),
                                   SourceLoc());
    cases.push_back(CaseStmt::create(C, SourceLoc(), labelItem,
                                     /*HasBoundDecls=*/false, SourceLoc(),
                                     SourceLoc(), body));
  }

  auto *anyPat = new (C) AnyPattern(SourceLoc());
  anyPat->setImplicit();
  auto dfltLabelItem = CaseLabelItem::getDefault(anyPat);

  auto *dfltReturnStmt = new (C) FailStmt(SourceLoc(), SourceLoc());
  auto *dfltBody = BraceStmt::create(C, SourceLoc(), ASTNode(dfltReturnStmt),
                                     SourceLoc());
  cases.push_back(CaseStmt::create(C, SourceLoc(), dfltLabelItem,
                                   /*HasBoundDecls=*/false, SourceLoc(),
                                   SourceLoc(), dfltBody));

  auto *stringValueDecl = initDecl->getParameterList(1)->get(0);
  auto *stringValueRef = new (C) DeclRefExpr(stringValueDecl, DeclNameLoc(),
                                             /*Implicit=*/true);
  auto *switchStmt = SwitchStmt::create(LabeledStmtInfo(), SourceLoc(),
                                        stringValueRef, SourceLoc(), cases,
                                        SourceLoc(), C);
  auto *body = BraceStmt::create(C, SourceLoc(), ASTNode(switchStmt),
                                 SourceLoc());
  initDecl->setBody(body);
}

/// Returns whether the given enum is eligible for CodingKey synthesis.
///
/// \param tc The type checker to use in checking eligibility.
///
/// \param parentDecl The parent declaration of the enum.
///
/// \param enumDecl The enum to check.
static bool canSynthesizeCodingKey(TypeChecker &tc, Decl *parentDecl,
                                   EnumDecl *enumDecl) {
  // Validate the enum and its raw type.
  tc.validateDecl(enumDecl);

  // If the enum has a raw type (optional), it must be String or Int.
  Type rawType = enumDecl->getRawType();
  if (rawType) {
    auto *parentDC = cast<DeclContext>(parentDecl);
    rawType = parentDC->mapTypeIntoContext(rawType);

    auto &C = tc.Context;
    auto *nominal = rawType->getCanonicalType()->getAnyNominal();
    if (nominal != C.getStringDecl() && nominal != C.getIntDecl())
      return false;
  }

  auto inherited = enumDecl->getInherited();
  if (!inherited.empty() && inherited.front().wasValidated() &&
      inherited.front().isError())
    return false;

  // If it meets all of those requirements, we can synthesize CodingKey
  // conformance.
  return true;
}

ValueDecl *DerivedConformance::deriveCodingKey(TypeChecker &tc,
                                               Decl *parentDecl,
                                               NominalTypeDecl *type,
                                               ValueDecl *requirement) {

  // We can only synthesize CodingKey for enums.
  auto *enumDecl = dyn_cast<EnumDecl>(type);
  if (!enumDecl)
    return nullptr;

  // Check other preconditions for synthesized conformance.
  if (!canSynthesizeCodingKey(tc, parentDecl, enumDecl))
    return nullptr;

  auto &C = tc.Context;
  auto rawType = enumDecl->getRawType();
  auto name = requirement->getBaseName();
  if (name == C.Id_stringValue) {
    // Synthesize `var stringValue: String { get }`
    auto stringType = C.getStringDecl()->getDeclaredType();
    auto synth = [rawType, stringType](AbstractFunctionDecl *getterDecl) {
      if (rawType && rawType->isEqual(stringType)) {
        // enum SomeStringEnum : String {
        //   case A, B, C
        //   @derived var stringValue: String {
        //     return self.rawValue
        //   }
        getterDecl->setBodySynthesizer(&deriveRawValueReturn);
      } else {
        // enum SomeEnum {
        //   case A, B, C
        //   @derived var stringValue: String {
        //     switch self {
        //       case A:
        //         return "A"
        //       case B:
        //         return "B"
        //       case C:
        //         return "C"
        //     }
        //   }
        // }
        getterDecl->setBodySynthesizer(&deriveBodyCodingKey_enum_stringValue);
      }
    };

    return deriveProperty(tc, parentDecl, enumDecl, stringType,
                          C.Id_stringValue, synth);

  } else if (name == C.Id_intValue) {
    // Synthesize `var intValue: Int? { get }`
    auto intType = C.getIntDecl()->getDeclaredType();
    auto optionalIntType = OptionalType::get(intType);

    auto synth = [rawType, intType](AbstractFunctionDecl *getterDecl) {
      if (rawType && rawType->isEqual(intType)) {
        // enum SomeIntEnum : Int {
        //   case A = 1, B = 2, C = 3
        //   @derived var intValue: Int? {
        //     return self.rawValue
        //   }
        // }
        getterDecl->setBodySynthesizer(&deriveRawValueReturn);
      } else {
        // enum SomeEnum {
        //   case A, B, C
        //   @derived var intValue: Int? {
        //     return nil
        //   }
        // }
        getterDecl->setBodySynthesizer(&deriveNilReturn);
      }
    };

    return deriveProperty(tc, parentDecl, enumDecl, optionalIntType,
                          C.Id_intValue, synth);
  } else if (name == DeclBaseName::createConstructor()) {
    auto argumentNames = requirement->getFullName().getArgumentNames();
    if (argumentNames.size() == 1) {
      if (argumentNames[0] == C.Id_stringValue) {
        // Derive `init?(stringValue:)`
        auto stringType = C.getStringDecl()->getDeclaredType();
        auto synth = [rawType, stringType](AbstractFunctionDecl *initDecl) {
          if (rawType && rawType->isEqual(stringType)) {
            // enum SomeStringEnum : String {
            //   case A = "a", B = "b", C = "c"
            //   @derived init?(stringValue: String) {
            //     self.init(rawValue: stringValue)
            //   }
            // }
            initDecl->setBodySynthesizer(&deriveRawValueInit);
          } else {
            // enum SomeEnum {
            //   case A, B, C
            //   @derived init?(stringValue: String) {
            //     switch stringValue {
            //     case "A":
            //       self = .A
            //     case "B":
            //       self = .B
            //     case "C":
            //       self = .C
            //     default:
            //       return nil
            //     }
            //   }
            // }
            initDecl->setBodySynthesizer(&deriveBodyCodingKey_init_stringValue);
          }
        };

        return deriveInitDecl(tc, parentDecl, enumDecl, stringType,
                              C.Id_stringValue, synth);
      } else if (argumentNames[0] == C.Id_intValue) {
        // Synthesize `init?(intValue:)`
        auto intType = C.getIntDecl()->getDeclaredType();
        auto synthesizer = [rawType, intType](AbstractFunctionDecl *initDecl) {
          if (rawType && rawType->isEqual(intType)) {
            // enum SomeIntEnum : Int {
            //   case A = 1, B = 2, C = 3
            //   @derived init?(intValue: Int) {
            //     self.init(rawValue: intValue)
            //   }
            // }
            initDecl->setBodySynthesizer(&deriveRawValueInit);
          } else {
            // enum SomeEnum {
            //   case A, B, C
            //   @derived init?(intValue: Int) {
            //     return nil
            //   }
            // }
            initDecl->setBodySynthesizer(&deriveNilReturn);
          }
        };

        return deriveInitDecl(tc, parentDecl, enumDecl, intType, C.Id_intValue,
                              synthesizer);
      }
    }
  }

  tc.diagnose(requirement->getLoc(), diag::broken_coding_key_requirement);
  return nullptr;
}
