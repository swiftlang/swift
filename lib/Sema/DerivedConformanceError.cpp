//===--- DerivedConformanceError.cpp - Derived Error ----------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  This file implements implicit derivation of the Error
//  protocol.
//
//===----------------------------------------------------------------------===//

#include "TypeChecker.h"
#include "DerivedConformances.h"
#include "swift/AST/ArchetypeBuilder.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Stmt.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Types.h"

using namespace swift;
using namespace DerivedConformance;

static void deriveBodyError_enum_code(AbstractFunctionDecl *codeDecl) {
  // enum SomeEnum {
  //   case A,B,C,D
  //
  //   @derived
  //   var code: Int {
  //     switch self {
  //     case A: return 0
  //     case B: return 1
  //     case C: return 2
  //     ...
  //     }
  //   }
  // }
  //
  // TODO: Some convenient way to override the code if that's desired.

  auto parentDC = codeDecl->getDeclContext();
  ASTContext &C = parentDC->getASTContext();

  auto enumDecl = parentDC->getAsEnumOrEnumExtensionContext();
  Type enumType = parentDC->getDeclaredTypeInContext();

  SmallVector<CaseStmt*, 4> cases;
  SmallString<11> strBuf;

  unsigned code = 0;
  for (auto elt : enumDecl->getAllElements()) {
    auto pat = new (C) EnumElementPattern(TypeLoc::withoutLoc(enumType),
                                          SourceLoc(), SourceLoc(),
                                          Identifier(), elt, nullptr);
    pat->setImplicit();
    
    auto labelItem =
      CaseLabelItem(/*IsDefault=*/false, pat, SourceLoc(), nullptr);

    {
      strBuf.clear();
      llvm::raw_svector_ostream os(strBuf);
      os << code;
    }
    
    auto codeStr = C.AllocateCopy(StringRef(strBuf));

    auto returnExpr = new (C) IntegerLiteralExpr(codeStr, SourceLoc(),
                                                 /*implicit*/ true);
    auto returnStmt = new (C) ReturnStmt(SourceLoc(), returnExpr,
                                         /*implicit*/ true);
    
    auto body = BraceStmt::create(C, SourceLoc(),
                                  ASTNode(returnStmt), SourceLoc());

    cases.push_back(CaseStmt::create(C, SourceLoc(), labelItem,
                                     /*HasBoundDecls=*/false, SourceLoc(),
                                     body));
    
    ++code;
  }
  
  Stmt *bodyStmt;
  // If the enum is empty, simply return zero. (It doesn't really matter, since
  // the enum can't be instantiated regardless.)
  if (cases.empty()) {
    static const char zero[] = "0";
    auto returnExpr = new (C) IntegerLiteralExpr(zero, SourceLoc(),
                                                 /*implicit*/ true);
    bodyStmt = new (C) ReturnStmt(SourceLoc(), returnExpr,
                                  /*implicit*/ true);
  } else {
    auto selfRef = createSelfDeclRef(codeDecl);
    bodyStmt = SwitchStmt::create(LabeledStmtInfo(), SourceLoc(), selfRef,
                                  SourceLoc(), cases, SourceLoc(), C);
  }
  auto body = BraceStmt::create(C, SourceLoc(),
                                ASTNode(bodyStmt),
                                SourceLoc());

  codeDecl->setBody(body);
}

static void deriveBodyError_zero_code(AbstractFunctionDecl *codeDecl) {
  // struct SomeStruct {
  //   @derived
  //   var code: Int { return 0 }
  // }
  //
  // TODO: Some convenient way to override the code if that's desired.

  auto parentDC = codeDecl->getDeclContext();
  ASTContext &C = parentDC->getASTContext();

  auto returnExpr = new (C) IntegerLiteralExpr("1", SourceLoc(),
                                               /*implicit*/ true);
  auto returnStmt = new (C) ReturnStmt(SourceLoc(), returnExpr,
                                       /*implicit*/ true);

  auto body = BraceStmt::create(C, SourceLoc(),
                                ASTNode(returnStmt), SourceLoc());

  codeDecl->setBody(body);
}

static ValueDecl *deriveError_code(TypeChecker &tc, Decl *parentDecl,
                                           NominalTypeDecl *nominal) {
  // enum SomeEnum {
  //   case A,B,C,D
  //
  //   @derived
  //   var code: Int {
  //     switch self {
  //     case A: return 0
  //     case B: return 1
  //     case C: return 2
  //     ...
  //     }
  //   }
  // }
  
  ASTContext &C = tc.Context;
  
  auto intTy = C.getIntDecl()->getDeclaredType();

  // Define the getter.
  auto getterDecl = declareDerivedPropertyGetter(tc, parentDecl, nominal,
                                                 intTy, intTy,
                                                 /*isStatic=*/false,
                                                 /*isFinal=*/true);
  if (isa<EnumDecl>(nominal))
    getterDecl->setBodySynthesizer(&deriveBodyError_enum_code);
  else
    getterDecl->setBodySynthesizer(&deriveBodyError_zero_code);

  // Define the property.
  VarDecl *propDecl;
  PatternBindingDecl *pbDecl;
  std::tie(propDecl, pbDecl)
    = declareDerivedReadOnlyProperty(tc, parentDecl, nominal, C.Id_code_,
                                     intTy, intTy, getterDecl,
                                     /*isStatic=*/false, /*isFinal=*/true);
  
  auto dc = cast<IterableDeclContext>(parentDecl);
  dc->addMember(getterDecl);
  dc->addMember(propDecl);
  dc->addMember(pbDecl);

  return propDecl;

}

ValueDecl *DerivedConformance::deriveError(TypeChecker &tc,
                                                   Decl *parentDecl,
                                                   NominalTypeDecl *type,
                                                   ValueDecl *requirement) {
  if (requirement->getName() == tc.Context.Id_code_)
    return deriveError_code(tc, parentDecl, type);
  
  tc.diagnose(requirement->getLoc(),
              diag::broken_errortype_requirement);
  return nullptr;
}

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

  if (requirement->getName() == tc.Context.Id_nsErrorDomain)
    return deriveBridgedNSError_enum_nsErrorDomain(tc, parentDecl, enumType);

  tc.diagnose(requirement->getLoc(),
              diag::broken_errortype_requirement);
  return nullptr;
}
