//===--- DerivedConformances.cpp - Derived conformance utilities ----------===//
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

#include "TypeChecker.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Stmt.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Pattern.h"
#include "swift/AST/Types.h"
#include "DerivedConformances.h"

using namespace swift;
using namespace DerivedConformance;

void DerivedConformance::_insertOperatorDecl(ASTContext &C,
                                             IterableDeclContext *scope,
                                             Decl *member) {
  // Find the module.
  auto mod = member->getModuleContext();

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

DeclRefExpr *
DerivedConformance::createSelfDeclRef(AbstractFunctionDecl *fn) {
  ASTContext &C = fn->getASTContext();

  Pattern *curriedArgs = fn->getBodyParamPatterns().front();
  auto selfPattern =
    cast<NamedPattern>(curriedArgs->getSemanticsProvidingPattern());
  auto selfDecl = selfPattern->getDecl();
  return new (C) DeclRefExpr(selfDecl, SourceLoc(), /*implicit*/true);
}

FuncDecl *DerivedConformance::declareDerivedPropertyGetter(TypeChecker &tc,
                                                 NominalTypeDecl *typeDecl,
                                                 Type contextType,
                                                 Type propertyInterfaceType,
                                                 Type propertyContextType,
                                                 bool isStatic) {
  auto &C = tc.Context;

  Type selfType = contextType;
  if (isStatic)
    selfType = MetatypeType::get(selfType);

  VarDecl *selfDecl = new (C) ParamDecl(/*IsLet*/true,
                                        SourceLoc(),
                                        Identifier(),
                                        SourceLoc(),
                                        C.Id_self,
                                        selfType,
                                        typeDecl);
  selfDecl->setImplicit();
  Pattern *selfParam = new (C) NamedPattern(selfDecl, /*implicit*/ true);
  selfParam->setType(selfType);
  selfParam = new (C) TypedPattern(selfParam,
                                   TypeLoc::withoutLoc(selfType));
  selfParam->setType(selfType);
  Pattern *methodParam = TuplePattern::create(C, SourceLoc(),{},SourceLoc());
  methodParam->setType(TupleType::getEmpty(C));
  Pattern *params[] = {selfParam, methodParam};

  FuncDecl *getterDecl =
    FuncDecl::create(C, SourceLoc(), StaticSpellingKind::None, SourceLoc(),
                     DeclName(), SourceLoc(), SourceLoc(), nullptr, Type(),
                     params, TypeLoc::withoutLoc(propertyContextType),
                     typeDecl);
  getterDecl->setImplicit();
  getterDecl->setStatic(isStatic);

  // Compute the type of the getter.
  GenericParamList *genericParams = nullptr;
  Type type = FunctionType::get(TupleType::getEmpty(C),
                                propertyContextType);
  selfType = getterDecl->computeSelfType(&genericParams);
  if (genericParams)
    type = PolymorphicFunctionType::get(selfType, type, genericParams);
  else
    type = FunctionType::get(selfType, type);
  getterDecl->setType(type);
  getterDecl->setBodyResultType(propertyContextType);

  // Compute the interface type of the getter.
  Type interfaceType = FunctionType::get(TupleType::getEmpty(C),
                                         propertyInterfaceType);
  Type selfInterfaceType = getterDecl->computeInterfaceSelfType(false);
  if (auto sig = typeDecl->getGenericSignatureOfContext())
    interfaceType = GenericFunctionType::get(sig, selfInterfaceType,
                                             interfaceType,
                                             FunctionType::ExtInfo());
  else
    interfaceType = type;
  getterDecl->setInterfaceType(interfaceType);
  getterDecl->setAccessibility(typeDecl->getFormalAccess());

  if (typeDecl->hasClangNode())
    tc.implicitlyDefinedFunctions.push_back(getterDecl);

  return getterDecl;
}

std::pair<VarDecl *, PatternBindingDecl *>
DerivedConformance::declareDerivedReadOnlyProperty(TypeChecker &tc,
                                                   NominalTypeDecl *typeDecl,
                                                   Identifier name,
                                                   Type propertyInterfaceType,
                                                   Type propertyContextType,
                                                   FuncDecl *getterDecl,
                                                   bool isStatic) {
  auto &C = tc.Context;

  VarDecl *propDecl = new (C) VarDecl(isStatic,
                                      /*let*/ false,
                                      SourceLoc(), name,
                                      propertyContextType,
                                      typeDecl);
  propDecl->setImplicit();
  propDecl->makeComputed(SourceLoc(), getterDecl, nullptr, nullptr,
                         SourceLoc());
  propDecl->setAccessibility(typeDecl->getFormalAccess());
  propDecl->setInterfaceType(propertyInterfaceType);

  Pattern *propPat = new (C) NamedPattern(propDecl, /*implicit*/ true);
  propPat->setType(propertyContextType);
  propPat = new (C) TypedPattern(propPat,
                                 TypeLoc::withoutLoc(propertyContextType),
                                 /*implicit*/ true);

  auto pbDecl = PatternBindingDecl::create(C, SourceLoc(),
                                           StaticSpellingKind::None,
                                           SourceLoc(), propPat, nullptr,
                                           typeDecl);
  pbDecl->setImplicit();

  return {propDecl, pbDecl};
}
