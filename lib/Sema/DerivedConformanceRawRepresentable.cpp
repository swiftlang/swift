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
#include "swift/AST/Decl.h"
#include "swift/AST/Stmt.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Types.h"
#include "DerivedConformances.h"

using namespace swift;
using namespace DerivedConformance;

LiteralExpr *cloneRawLiteralExpr(ASTContext &C, LiteralExpr *expr) {
  LiteralExpr *clone;
  if (auto intLit = dyn_cast<IntegerLiteralExpr>(expr)) {
    clone = new (C) IntegerLiteralExpr(intLit->getText(), SourceLoc(),
                                       /*implicit*/ true);
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

bool DerivedConformance::deriveRawRepresentable(TypeChecker &tc,
                                                NominalTypeDecl *type) {
  // Check preconditions. These should already have been diagnosed by
  // type-checking but we may still get here through recovery.
  
  // The type must be an enum.
  auto enumDecl = cast<EnumDecl>(type);
  if (!enumDecl)
    return true;
  
  // It must have a raw type.
  auto rawType = enumDecl->getRawType();
  if (!rawType)
    return true;
  
  // There must be enum elements, and they must all have type-checked raw
  // values.
  if (enumDecl->getAllElements().empty())
    return true;

  for (auto elt : enumDecl->getAllElements()) {
    if (!elt->getTypeCheckedRawValueExpr()
        || !elt->getTypeCheckedRawValueExpr()->getType()->isEqual(rawType))
      return true;
  }
  
  // Start building the conforming decls.
  
  auto &C = tc.Context;
  
  // Build the 'RawType' typealias.
  // enum SomeEnum : SomeType {
  //   typealias [derived] RawType = SomeType
  // }
  auto rawTypeDecl = new (C) TypeAliasDecl(SourceLoc(),
                                           C.getIdentifier("RawType"),
                                           SourceLoc(),
                                           TypeLoc::withoutLoc(rawType),
                                           enumDecl,
                                           {});
  rawTypeDecl->setImplicit();
  tc.typeCheckDecl(rawTypeDecl, /*FirstPass*/ true);

  // Build the 'toRaw' method.
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
  FuncDecl *toRawDecl;
  {
    VarDecl *selfDecl = new (C) VarDecl(SourceLoc(),
                                        C.getIdentifier("self"),
                                        Type(),
                                        enumDecl);
    selfDecl->setImplicit();
    Pattern *selfParam = new (C) NamedPattern(selfDecl, /*implicit*/ true);
    selfParam = new (C) TypedPattern(selfParam,
                     TypeLoc::withoutLoc(enumDecl->getDeclaredTypeInContext()));
    Pattern *methodParam = TuplePattern::create(C, SourceLoc(),{},SourceLoc());
    Pattern *params[] = {selfParam, methodParam};
    
    toRawDecl = FuncDecl::create(C, SourceLoc(), SourceLoc(),
                                 C.getIdentifier("toRaw"),
                                 SourceLoc(), nullptr, Type(),
                                 params, params,
                                 TypeLoc::withoutLoc(rawType), enumDecl);
    toRawDecl->setImplicit();
    
    selfDecl->setDeclContext(toRawDecl);
    
    SmallVector<CaseStmt*, 4> cases;
    for (auto elt : enumDecl->getAllElements()) {
      auto pat = new (C) EnumElementPattern(
                  TypeLoc::withoutLoc(enumDecl->getDeclaredTypeInContext()),
                  SourceLoc(), SourceLoc(), Identifier(), elt, nullptr);
      pat->setImplicit();
      
      auto label = CaseLabel::create(C, /*isDefault*/false,
                                     SourceLoc(),
                                     pat, SourceLoc(), nullptr, SourceLoc());

      auto returnExpr = cloneRawLiteralExpr(C, elt->getRawValueExpr());
      auto returnStmt = new (C) ReturnStmt(SourceLoc(), returnExpr);
      
      auto body = BraceStmt::create(C, SourceLoc(),
                            BraceStmt::ExprStmtOrDecl(returnStmt), SourceLoc());

      cases.push_back(CaseStmt::create(C, label, /*hasBoundDecls*/false, body));
    }
    auto selfRef = new (C) DeclRefExpr(selfDecl, SourceLoc(), /*implicit*/true);
    auto switchStmt = SwitchStmt::create(SourceLoc(), selfRef,
                                         SourceLoc(), cases, SourceLoc(), C);
    auto body = BraceStmt::create(C, SourceLoc(),
                                  BraceStmt::ExprStmtOrDecl(switchStmt),
                                  SourceLoc());
    toRawDecl->setBody(body);

    tc.typeCheckDecl(toRawDecl, /*FirstPass*/ true);
    bool error = tc.typeCheckFunctionBodyUntil(toRawDecl, SourceLoc());
    assert(!error); (void)error;
  }
  
  // Build the 'fromRaw' method.
  // enum SomeEnum : SomeType {
  //   case A = 111, B = 222
  //   static func [derived] fromRaw(raw: SomeType) -> SomeEnum? {
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
  FuncDecl *fromRawDecl;
  {
    VarDecl *selfDecl = new (C) VarDecl(SourceLoc(),
                                        C.getIdentifier("self"),
                                        Type(),
                                        enumDecl);
    selfDecl->setImplicit();
    Pattern *selfParam = new (C) NamedPattern(selfDecl, /*implicit*/ true);
    auto metaTy = MetaTypeType::get(enumDecl->getDeclaredTypeInContext(), C);
    selfParam = new (C) TypedPattern(selfParam, TypeLoc::withoutLoc(metaTy));
    selfParam->setImplicit();
    
    VarDecl *rawDecl = new (C) VarDecl(SourceLoc(),
                                       C.getIdentifier("raw"),
                                       Type(),
                                       enumDecl);
    rawDecl->setImplicit();
    Pattern *rawParam = new (C) NamedPattern(rawDecl, /*implicit*/ true);
    rawParam = new (C) TypedPattern(rawParam, TypeLoc::withoutLoc(rawType));
    rawParam->setImplicit();
    rawParam = new (C) ParenPattern(SourceLoc(), rawParam, SourceLoc());
    rawParam->setImplicit();

    Pattern *params[] = {selfParam, rawParam};
    auto retTy = OptionalType::get(enumDecl->getDeclaredTypeInContext(), C);
    fromRawDecl = FuncDecl::create(C, SourceLoc(), SourceLoc(),
                                   C.getIdentifier("fromRaw"),
                                   SourceLoc(), nullptr, Type(),
                                   params, params, TypeLoc::withoutLoc(retTy),
                                   enumDecl);
    fromRawDecl->setStatic();
    fromRawDecl->setImplicit();
    selfDecl->setDeclContext(fromRawDecl);
    rawDecl->setDeclContext(fromRawDecl);
    
    SmallVector<CaseStmt*, 4> cases;
    for (auto elt : enumDecl->getAllElements()) {
      auto litExpr = cloneRawLiteralExpr(C, elt->getRawValueExpr());
      auto litPat = new (C) ExprPattern(litExpr, /*isResolved*/ true,
                                        nullptr, nullptr);
      litPat->setImplicit();
      
      auto label = CaseLabel::create(C, /*isDefault*/false,
                                     SourceLoc(), litPat, SourceLoc(),
                                     nullptr, SourceLoc());
      
      auto eltRef = new (C) DeclRefExpr(elt, SourceLoc(), /*implicit*/true);
      auto metaTyRef = new (C) MetatypeExpr(nullptr, SourceLoc(), metaTy);
      auto returnExpr = new (C) DotSyntaxCallExpr(eltRef,SourceLoc(),metaTyRef);
      auto returnStmt = new (C) ReturnStmt(SourceLoc(), returnExpr);
      
      auto body = BraceStmt::create(C, SourceLoc(),
                          BraceStmt::ExprStmtOrDecl(returnStmt), SourceLoc());
      
      cases.push_back(CaseStmt::create(C, label, /*hasBoundDecls*/false, body));
    }
    
    auto anyPat = new (C) AnyPattern(SourceLoc());
    anyPat->setImplicit();
    auto dfltLabel = CaseLabel::create(C, /*isDefault*/true, SourceLoc(),
                                       anyPat, SourceLoc(), nullptr,
                                       SourceLoc());
    
    auto optionalRef = new (C) DeclRefExpr(C.getOptionalDecl(),
                                           SourceLoc(), /*implicit*/true);
    auto emptyArgs = new (C) TupleExpr(SourceLoc(), {}, nullptr, SourceLoc(),
                                       /*trailingClosure*/false,
                                       /*implicit*/ true);
    auto dfltReturnExpr = new (C) CallExpr(optionalRef, emptyArgs,
                                           /*implicit*/ true);
    auto dfltReturnStmt = new (C) ReturnStmt(SourceLoc(), dfltReturnExpr);
    auto dfltBody = BraceStmt::create(C, SourceLoc(),
                        BraceStmt::ExprStmtOrDecl(dfltReturnStmt), SourceLoc());
    cases.push_back(CaseStmt::create(C, dfltLabel, /*hasBoundDecls*/false,
                                     dfltBody));
    
    auto rawRef = new (C) DeclRefExpr(rawDecl, SourceLoc(), /*implicit*/true);
    auto switchStmt = SwitchStmt::create(SourceLoc(), rawRef, SourceLoc(),
                                         cases, SourceLoc(), C);
    auto body = BraceStmt::create(C, SourceLoc(),
                                  BraceStmt::ExprStmtOrDecl(switchStmt),
                                  SourceLoc());
    fromRawDecl->setBody(body);
 
    tc.typeCheckDecl(fromRawDecl, /*FirstPass*/ true);
    bool error = tc.typeCheckFunctionBodyUntil(fromRawDecl, SourceLoc());
    assert(!error); (void)error;
  }
  
  // Add the members to the type.
  SmallVector<Decl*, 8> newMembers(enumDecl->getMembers().begin(),
                                   enumDecl->getMembers().end());
  newMembers.push_back(rawTypeDecl);
  newMembers.push_back(fromRawDecl);
  newMembers.push_back(toRawDecl);
  
  enumDecl->setMembers(C.AllocateCopy(newMembers), enumDecl->getBraces());
  return false;
}
