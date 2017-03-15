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

static void setNilReturnBody(AbstractFunctionDecl *funcDecl) {
  auto parentDC = funcDecl->getDeclContext();
  ASTContext &C = parentDC->getASTContext();

  auto nilExpr = new (C) NilLiteralExpr(SourceLoc(), /*Implicit=*/true);
  auto returnStmt = new (C) ReturnStmt(SourceLoc(), nilExpr);
  auto body = BraceStmt::create(C, SourceLoc(), ASTNode(returnStmt),
                                SourceLoc());
  funcDecl->setBody(body);
}

static void setRawValueReturnBody(AbstractFunctionDecl *funcDecl) {
  auto parentDC = funcDecl->getDeclContext();
  ASTContext &C = parentDC->getASTContext();

  auto selfRef = createSelfDeclRef(funcDecl);
  auto memberRef = new (C) UnresolvedDotExpr(selfRef, SourceLoc(),
                                             C.Id_rawValue, DeclNameLoc(),
                                             /*Implicit=*/true);

  auto returnStmt = new (C) ReturnStmt(SourceLoc(), memberRef);
  auto body = BraceStmt::create(C, SourceLoc(), ASTNode(returnStmt),
                                SourceLoc());
  funcDecl->setBody(body);
}

static void
deriveBodyCodingKey_enum_stringValue_raw(AbstractFunctionDecl *strValDecl) {
  // enum SomeStringEnum : String {
  //   case A = "a", B = "b", C = "c"
  //   @derived var stringValue: String? {
  //     return self.rawValue
  //   }
  // }

  // Guaranteed to have a rawValue because you cannot declare an enum with a
  // raw type but no cases.
  setRawValueReturnBody(strValDecl);
}

static void
deriveBodyCodingKey_enum_stringValue_switch(AbstractFunctionDecl *strValDecl) {
  // enum SomeEnum {
  //   case A, B, C
  //   @derived var stringValue: String? {
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
  auto parentDC = strValDecl->getDeclContext();
  ASTContext &C = parentDC->getASTContext();

  auto enumDecl = parentDC->getAsEnumOrEnumExtensionContext();
  Type enumType = parentDC->getDeclaredTypeInContext();

  auto elements = enumDecl->getAllElements();
  if (elements.empty() /* empty enum */) {
    setNilReturnBody(strValDecl);
    return;
  }

  SmallVector<CaseStmt*, 4> cases;
  for (auto elt : elements) {
    auto pat = new (C) EnumElementPattern(TypeLoc::withoutLoc(enumType),
                                          SourceLoc(), SourceLoc(),
                                          Identifier(), elt, nullptr);
    pat->setImplicit();

    auto labelItem =
      CaseLabelItem(/*IsDefault=*/false, pat, SourceLoc(), nullptr);

    auto returnExpr = new (C) StringLiteralExpr(elt->getNameStr(),
                                                SourceRange(),
                                                /*Implicit=*/true);
    auto returnStmt = new (C) ReturnStmt(SourceLoc(), returnExpr);
    auto body = BraceStmt::create(C, SourceLoc(), ASTNode(returnStmt),
                                  SourceLoc());
    cases.push_back(CaseStmt::create(C, SourceLoc(), labelItem,
                                     /*HasBoundDecls=*/false, SourceLoc(),
                                     body));
  }

  auto selfRef = createSelfDeclRef(strValDecl);
  auto switchStmt = SwitchStmt::create(LabeledStmtInfo(), SourceLoc(), selfRef,
                                       SourceLoc(), cases, SourceLoc(), C);
  auto body = BraceStmt::create(C, SourceLoc(), ASTNode(switchStmt),
                                SourceLoc());
  strValDecl->setBody(body);
}

static ValueDecl *
deriveCodingKey_enum_stringValue(TypeChecker &tc, Decl *parentDecl,
                                 EnumDecl *enumDecl) {
  ASTContext &C = tc.Context;
  auto parentDC = cast<DeclContext>(parentDecl);
  auto optionalStringType = OptionalType::get(OTK_Optional,
                                              tc.getStringType(parentDC));

  // Define the getter.
  auto getterDecl = declareDerivedPropertyGetter(tc, parentDecl, enumDecl,
                                                 optionalStringType,
                                                 optionalStringType,
                                                 /*isStatic=*/false,
                                                 /*isFinal=*/false);

  auto rawType = enumDecl->getRawType();
  if (rawType && rawType->isEqual(tc.getStringType(parentDC))) {
    getterDecl->setBodySynthesizer(&deriveBodyCodingKey_enum_stringValue_raw);
  } else {
    getterDecl->setBodySynthesizer(&deriveBodyCodingKey_enum_stringValue_switch);
  }

  // Define the property.
  VarDecl *propDecl;
  PatternBindingDecl *pbDecl;
  std::tie(propDecl, pbDecl)
    = declareDerivedReadOnlyProperty(tc, parentDecl, enumDecl,
                                     C.Id_stringValue,
                                     optionalStringType,
                                     optionalStringType,
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
deriveBodyCodingKey_enum_intValue_raw(AbstractFunctionDecl *intValDecl) {
  // enum SomeIntEnum : Int {
  //   case A = 1, B = 2, C = 3
  //   @derived var intValue: Int? {
  //     return self.rawValue
  //   }
  // }

  // Guaranteed to have a rawValue because you cannot declare an enum with a
  // raw type but no cases.
  setRawValueReturnBody(intValDecl);
}

void deriveBodyCodingKey_enum_intValue_nil(AbstractFunctionDecl *intValDecl) {
  // enum SomeEnum {
  //   case A, B, C
  //   @derived var intValue: Int? {
  //     return nil
  //   }
  // }
  setNilReturnBody(intValDecl);
}

static ValueDecl *
deriveCodingKey_enum_intValue(TypeChecker &tc, Decl *parentDecl,
                              EnumDecl *enumDecl) {
  ASTContext &C = tc.Context;
  auto parentDC = cast<DeclContext>(parentDecl);
  auto optionalIntType = OptionalType::get(OTK_Optional,
                                           tc.getIntType(parentDC));

  // Define the getter.
  auto getterDecl = declareDerivedPropertyGetter(tc, parentDecl, enumDecl,
                                                 optionalIntType,
                                                 optionalIntType,
                                                 /*isStatic=*/false,
                                                 /*isFinal=*/false);

  auto rawType = enumDecl->getRawType();
  if (rawType && rawType->isEqual(tc.getIntType(parentDC))) {
    getterDecl->setBodySynthesizer(&deriveBodyCodingKey_enum_intValue_raw);
  } else {
    getterDecl->setBodySynthesizer(&deriveBodyCodingKey_enum_intValue_nil);
  }

  // Define the property.
  VarDecl *propDecl;
  PatternBindingDecl *pbDecl;
  std::tie(propDecl, pbDecl)
    = declareDerivedReadOnlyProperty(tc, parentDecl, enumDecl,
                                     C.Id_intValue,
                                     optionalIntType,
                                     optionalIntType,
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
deriveBodyCodingKey_enum_init_stringValue_raw(AbstractFunctionDecl *initDecl) {
  // enum SomeStringEnum : String {
  //   case A = "a", B = "b", C = "c"
  //   @derived init?(stringValue: String) {
  //     self.init(rawValue: stringValue)
  //   }
  // }
  auto parentDC = initDecl->getDeclContext();
  ASTContext &C = parentDC->getASTContext();

  // rawValue param to init(rawValue:)
  auto rawValueDecl = new (C) ParamDecl(/*IsLet=*/true, SourceLoc(),
                                        SourceLoc(), C.Id_rawValue,
                                        SourceLoc(), C.Id_rawValue,
                                        C.getStringDecl()->getDeclaredType(),
                                        parentDC);
  rawValueDecl->setInterfaceType(C.getIntDecl()->getDeclaredType());
  rawValueDecl->setImplicit();
  auto paramList = ParameterList::createWithoutLoc(rawValueDecl);

  // init(rawValue:) constructor name
  DeclName ctorName(C, C.Id_init, paramList);

  auto selfRef = createSelfDeclRef(initDecl);

  // self.init(rawValue:) expr
  auto initExpr = new (C) UnresolvedDotExpr(selfRef, SourceLoc(),
                                            ctorName, DeclNameLoc(),
                                            /*Implicit=*/true);

  // Get the stringValue param from init(stringValue:). self is the first param
  // in the list; stringValue is the second.
  auto stringValueParam = initDecl->getParameterList(1)->get(0);
  auto stringValueExpr = new (C) DeclRefExpr(ConcreteDeclRef(stringValueParam),
                                          DeclNameLoc(),
                                          /*Implicit=*/true);

  // Bind the stringValue param in self.init(rawValue: stringValue).
  ArrayRef<Expr *> args{stringValueExpr};
  ArrayRef<Identifier> argLabels{C.Id_rawValue};
  auto callExpr = CallExpr::createImplicit(C, initExpr, args, argLabels);

  auto body = BraceStmt::create(C, SourceLoc(), ASTNode(callExpr), SourceLoc());
  initDecl->setBody(body);
}

static void
deriveBodyCodingKey_enum_init_stringValue_switch(AbstractFunctionDecl *
                                                 initDecl) {
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
  auto parentDC = initDecl->getDeclContext();
  ASTContext &C = parentDC->getASTContext();

  auto enumDecl = parentDC->getAsEnumOrEnumExtensionContext();
  Type enumType = parentDC->getDeclaredTypeInContext();

  auto elements = enumDecl->getAllElements();
  if (elements.empty() /* empty enum */) {
    setNilReturnBody(initDecl);
    return;
  }

  auto selfRef = createSelfDeclRef(initDecl);
  SmallVector<CaseStmt*, 4> cases;
  for (auto elt : elements) {
    auto litExpr = new (C) StringLiteralExpr(elt->getNameStr(),
                                             SourceRange(),
                                             /*Implicit=*/true);
    auto litPat = new (C) ExprPattern(litExpr, /*IsResolved=*/true,
                                      nullptr, nullptr);
    litPat->setImplicit();

    auto labelItem =
      CaseLabelItem(/*IsDefault=*/false, litPat, SourceLoc(), nullptr);

    auto eltRef = new (C) DeclRefExpr(elt, DeclNameLoc(), /*Implicit=*/true);
    auto metaTyRef = TypeExpr::createImplicit(enumType, C);
    auto valueExpr = new (C) DotSyntaxCallExpr(eltRef, SourceLoc(), metaTyRef);

    auto assignment = new (C) AssignExpr(selfRef, SourceLoc(), valueExpr,
                                         /*Implicit=*/true);

    auto body = BraceStmt::create(C, SourceLoc(), ASTNode(assignment),
                                  SourceLoc());
    cases.push_back(CaseStmt::create(C, SourceLoc(), labelItem,
                                     /*HasBoundDecls=*/false, SourceLoc(),
                                     body));
  }

  auto anyPat = new (C) AnyPattern(SourceLoc());
  anyPat->setImplicit();
  auto dfltLabelItem =
    CaseLabelItem(/*IsDefault=*/true, anyPat, SourceLoc(), nullptr);

  auto dfltReturnStmt = new (C) FailStmt(SourceLoc(), SourceLoc());
  auto dfltBody = BraceStmt::create(C, SourceLoc(), ASTNode(dfltReturnStmt),
                                    SourceLoc());
  cases.push_back(CaseStmt::create(C, SourceLoc(), dfltLabelItem,
                                   /*HasBoundDecls=*/false, SourceLoc(),
                                   dfltBody));

  auto stringValueDecl = initDecl->getParameterList(1)->get(0);
  auto stringValueRef = new (C) DeclRefExpr(stringValueDecl, DeclNameLoc(),
                                            /*Implicit=*/true);
  auto switchStmt = SwitchStmt::create(LabeledStmtInfo(), SourceLoc(),
                                       stringValueRef, SourceLoc(), cases,
                                       SourceLoc(), C);
  auto body = BraceStmt::create(C, SourceLoc(), ASTNode(switchStmt),
                                SourceLoc());
  initDecl->setBody(body);
}

static ValueDecl *
deriveCodingKey_enum_init_stringValue(TypeChecker &tc, Decl *parentDecl,
                                      EnumDecl *enumDecl) {
  ASTContext &C = tc.Context;
  auto parentDC = cast<DeclContext>(parentDecl);
  auto stringType = tc.getStringType(parentDC);

  auto *selfDecl = ParamDecl::createSelf(SourceLoc(), parentDC,
                                         /*static*/false, /*inout*/true);

  auto *rawDecl = new (C) ParamDecl(/*IsLet*/true, SourceLoc(), SourceLoc(),
                                    C.Id_stringValue, SourceLoc(),
                                    C.Id_stringValue, stringType, parentDC);
  rawDecl->setInterfaceType(stringType);
  rawDecl->setImplicit();
  auto paramList = ParameterList::createWithoutLoc(rawDecl);
  DeclName name(C, C.Id_init, paramList);

  auto initDecl =
    new (C) ConstructorDecl(name, SourceLoc(),
                            /*Failability=*/OTK_Optional,
                            /*FailabilityLoc=*/SourceLoc(),
                            /*Throws=*/false, /*ThrowsLoc=*/SourceLoc(),
                            selfDecl, paramList,
                            /*GenericParams=*/nullptr, parentDC);

  initDecl->setImplicit();

  auto rawType = enumDecl->getRawType();
  if (rawType && rawType->isEqual(stringType)) {
    initDecl->setBodySynthesizer(&deriveBodyCodingKey_enum_init_stringValue_raw);
  } else {
    initDecl->setBodySynthesizer(&deriveBodyCodingKey_enum_init_stringValue_switch);
  }

  // Compute the type of the initializer.
  TupleTypeElt element(stringType, C.Id_stringValue);
  TupleTypeElt interfaceElement(stringType, C.Id_stringValue);
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

static void deriveBodyCodingKey_enum_init_intValue_raw(AbstractFunctionDecl *
                                                       initDecl) {
  // enum SomeIntEnum : Int {
  //   case A = 1, B = 2, C = 3
  //   @derived init?(intValue: Int) {
  //     self.init(rawValue: intValue)
  //   }
  // }
  auto parentDC = initDecl->getDeclContext();
  ASTContext &C = parentDC->getASTContext();

  // rawValue param to init(rawValue:)
  auto rawValueDecl = new (C) ParamDecl(/*IsLet=*/true, SourceLoc(),
                                        SourceLoc(), C.Id_rawValue,
                                        SourceLoc(), C.Id_rawValue,
                                        C.getIntDecl()->getDeclaredType(),
                                        parentDC);
  rawValueDecl->setInterfaceType(C.getIntDecl()->getDeclaredType());
  rawValueDecl->setImplicit();
  auto paramList = ParameterList::createWithoutLoc(rawValueDecl);

  // init(rawValue:) constructor name
  DeclName ctorName(C, C.Id_init, paramList);

  auto selfRef = createSelfDeclRef(initDecl);

  // self.init(rawValue:) expr
  auto initExpr = new (C) UnresolvedDotExpr(selfRef, SourceLoc(),
                                            ctorName, DeclNameLoc(),
                                            /*Implicit=*/true);

  // Get the intValue param from init(intValue:). self is the first param
  // in the list; intValue is the second.
  auto intValueParam = initDecl->getParameterList(1)->get(0);
  auto intValueExpr = new (C) DeclRefExpr(ConcreteDeclRef(intValueParam),
                                          DeclNameLoc(),
                                          /*Implicit=*/true);

  // Bind the intValue param in self.init(rawValue: intValue).
  ArrayRef<Expr *> args{intValueExpr};
  ArrayRef<Identifier> argLabels{C.Id_rawValue};
  auto callExpr = CallExpr::createImplicit(C, initExpr, args, argLabels);

  auto body = BraceStmt::create(C, SourceLoc(), ASTNode(callExpr), SourceLoc());
  initDecl->setBody(body);
}

static void
deriveBodyCodingKey_enum_init_intValue_nil(AbstractFunctionDecl *initDecl) {
  // enum SomeEnum {
  //   case A, B, C
  //   @derived init?(intValue: Int) {
  //     return nil
  //   }
  // }
  setNilReturnBody(initDecl);
}

static ValueDecl *
deriveCodingKey_enum_init_intValue(TypeChecker &tc, Decl *parentDecl,
                                   EnumDecl *enumDecl) {
  ASTContext &C = tc.Context;
  auto parentDC = cast<DeclContext>(parentDecl);
  auto intType = tc.getIntType(parentDC);

  auto *selfDecl = ParamDecl::createSelf(SourceLoc(), parentDC,
                                         /*static*/false, /*inout*/true);

  auto *rawDecl = new (C) ParamDecl(/*IsLet*/true, SourceLoc(), SourceLoc(),
                                    C.Id_intValue, SourceLoc(),
                                    C.Id_intValue, intType, parentDC);
  rawDecl->setInterfaceType(intType);
  rawDecl->setImplicit();
  auto paramList = ParameterList::createWithoutLoc(rawDecl);
  DeclName name(C, C.Id_init, paramList);

  auto initDecl =
    new (C) ConstructorDecl(name, SourceLoc(),
                            /*Failability=*/OTK_Optional,
                            /*FailabilityLoc=*/SourceLoc(),
                            /*Throws=*/false, /*ThrowsLoc=*/SourceLoc(),
                            selfDecl, paramList,
                            /*GenericParams=*/nullptr, parentDC);

  initDecl->setImplicit();

  auto rawType = enumDecl->getRawType();
  if (rawType && rawType->isEqual(intType)) {
    initDecl->setBodySynthesizer(&deriveBodyCodingKey_enum_init_intValue_raw);
  } else {
    initDecl->setBodySynthesizer(&deriveBodyCodingKey_enum_init_intValue_nil);
  }

  // Compute the type of the initializer.
  TupleTypeElt element(intType, C.Id_intValue);
  TupleTypeElt interfaceElement(intType, C.Id_intValue);
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

static bool
canSynthesizeCodingKey(TypeChecker &tc, Decl *parentDecl, EnumDecl *enumDecl) {
  // If the enum has a raw type (optional), it must be String or Int.
  Type rawType = enumDecl->getRawType();
  if (rawType) {
    auto parentDC = cast<DeclContext>(parentDecl);
    rawType = parentDC->mapTypeIntoContext(rawType);

    ASTContext &C = tc.Context;
    auto nominal = rawType->getCanonicalType()->getAnyNominal();
    if (nominal != C.getStringDecl() && nominal != C.getIntDecl())
      return false;
  }

  if (!enumDecl->getInherited().empty() &&
      enumDecl->getInherited().front().isError())
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
  auto enumDecl = dyn_cast<EnumDecl>(type);
  if (!enumDecl)
    return nullptr;

  // Check other preconditions for synthesized conformance.
  if (!canSynthesizeCodingKey(tc, parentDecl, enumDecl))
    return nullptr;

   auto name = requirement->getName();
   if (name == tc.Context.Id_stringValue) {
     return deriveCodingKey_enum_stringValue(tc, parentDecl, enumDecl);
   } else if (name == tc.Context.Id_intValue) {
     return deriveCodingKey_enum_intValue(tc, parentDecl, enumDecl);
   } else if (name == tc.Context.Id_init) {
     auto argumentNames = requirement->getFullName().getArgumentNames();
     if (argumentNames.size() == 1) {
       if (argumentNames[0] == tc.Context.Id_stringValue)
         return deriveCodingKey_enum_init_stringValue(tc, parentDecl, enumDecl);
       else if (argumentNames[0] == tc.Context.Id_intValue)
         return deriveCodingKey_enum_init_intValue(tc, parentDecl, enumDecl);
     }
   }

  tc.diagnose(requirement->getLoc(),
              diag::broken_coding_key_requirement);
  return nullptr;
}
