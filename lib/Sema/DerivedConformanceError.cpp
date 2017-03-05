//===--- DerivedConformanceError.cpp - Derived Error ----------------------===//
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
//  This file implements implicit derivation of the Error
//  protocol.
//
//===----------------------------------------------------------------------===//

#include "TypeChecker.h"
#include "DerivedConformances.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Stmt.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Types.h"

using namespace swift;
using namespace DerivedConformance;

static void deriveBodyBridgedNSError_enum_nsErrorDomain(
              AbstractFunctionDecl *domainDecl) {
  // enum SomeEnum {
  //   @derived
  //   static var _nsErrorDomain: String {
  //     return "ModuleName.SomeEnum"
  //   }
  // }

  auto M = domainDecl->getParentModule();
  auto &C = M->getASTContext();
  auto TC = domainDecl->getInnermostTypeContext();
  auto ED = TC->getAsEnumOrEnumExtensionContext();

  std::string buffer = M->getNameStr();
  buffer += ".";
  buffer += ED->getNameStr();
  StringRef value(C.AllocateCopy(buffer));

  auto string = new (C) StringLiteralExpr(value, SourceRange(), /*implicit*/ true);
  auto ret = new (C) ReturnStmt(SourceLoc(), string, /*implicit*/ true);
  auto body = BraceStmt::create(C, SourceLoc(),
                                ASTNode(ret),
                                SourceLoc());
  domainDecl->setBody(body);
}

static ValueDecl *deriveBridgedNSError_enum_nsErrorDomain(TypeChecker &tc,
                                                          Decl *parentDecl,
                                                          EnumDecl *enumDecl) {
  // enum SomeEnum {
  //   @derived
  //   static var _nsErrorDomain: String {
  //     return "\(self)"
  //   }
  // }

  // Note that for @objc enums the format is assumed to be "MyModule.SomeEnum".
  // If this changes, please change PrintAsObjC as well.
  
  ASTContext &C = tc.Context;
  
  auto stringTy = C.getStringDecl()->getDeclaredType();

  // Define the getter.
  auto getterDecl = declareDerivedPropertyGetter(tc, parentDecl, enumDecl,
                                                 stringTy, stringTy,
                                                 /*isStatic=*/true,
                                                 /*isFinal=*/true);
  getterDecl->setBodySynthesizer(&deriveBodyBridgedNSError_enum_nsErrorDomain);
  
  // Define the property.
  VarDecl *propDecl;
  PatternBindingDecl *pbDecl;
  std::tie(propDecl, pbDecl)
    = declareDerivedReadOnlyProperty(tc, parentDecl, enumDecl,
                                     C.Id_nsErrorDomain,
                                     stringTy, stringTy,
                                     getterDecl, /*isStatic=*/true,
                                     /*isFinal=*/true);
  
  auto dc = cast<IterableDeclContext>(parentDecl);
  dc->addMember(getterDecl);
  dc->addMember(propDecl);
  dc->addMember(pbDecl);

  return propDecl;
}

ValueDecl *DerivedConformance::deriveBridgedNSError(TypeChecker &tc,
                                                    Decl *parentDecl,
                                                    NominalTypeDecl *type,
                                                    ValueDecl *requirement) {
  if (!isa<EnumDecl>(type))
    return nullptr;

  auto enumType = cast<EnumDecl>(type);

  if (requirement->getBaseName() == tc.Context.Id_nsErrorDomain)
    return deriveBridgedNSError_enum_nsErrorDomain(tc, parentDecl, enumType);

  tc.diagnose(requirement->getLoc(),
              diag::broken_errortype_requirement);
  return nullptr;
}
