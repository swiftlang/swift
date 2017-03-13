//===--- DerivedConformances.cpp - Derived conformance utilities ----------===//
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

ValueDecl *DerivedConformance::getDerivableRequirement(NominalTypeDecl *nominal,
                                                       ValueDecl *requirement) {
  // Note: whenever you update this function, also update
  // TypeChecker::deriveProtocolRequirement.
  ASTContext &ctx = nominal->getASTContext();
  auto name = requirement->getFullName();

  // Local function that retrieves the requirement with the same name as
  // the provided requirement, but within the given known protocol.
  auto getRequirement = [&](KnownProtocolKind kind) -> ValueDecl * {
    // Dig out the protocol.
    auto proto = ctx.getProtocol(kind);
    if (!proto) return nullptr;

    // Check whether this nominal type derives conformances to the
    if (!nominal->derivesProtocolConformance(proto)) return nullptr;

    // Retrieve the requirement.
    auto results = proto->lookupDirect(name);
    return results.empty() ? nullptr : results.front();
  };

  // Properties.
  if (isa<VarDecl>(requirement)) {
    // RawRepresentable.rawValue
    if (name.isSimpleName(ctx.Id_rawValue))
      return getRequirement(KnownProtocolKind::RawRepresentable);

    // Hashable.hashValue
    if (name.isSimpleName(ctx.Id_hashValue))
      return getRequirement(KnownProtocolKind::Hashable);

    // _BridgedNSError._nsErrorDomain
    if (name.isSimpleName(ctx.Id_nsErrorDomain))
      return getRequirement(KnownProtocolKind::BridgedNSError);

    return nullptr;
  }

  // Functions.
  if (auto func = dyn_cast<FuncDecl>(requirement)) {
    if (func->isOperator() && name.getBaseName().str() == "==")
      return getRequirement(KnownProtocolKind::Equatable);

    return nullptr;
  }

  // Initializers.
  if (isa<ConstructorDecl>(requirement)) {
    auto argumentNames = name.getArgumentNames();
    if (argumentNames.size() == 1 && argumentNames[0] == ctx.Id_rawValue)
      return getRequirement(KnownProtocolKind::RawRepresentable);

    return nullptr;
  }

  // Associated types.
  if (isa<AssociatedTypeDecl>(requirement)) {
    // RawRepresentable.RawValue
    if (name.isSimpleName(ctx.Id_RawValue))
      return getRequirement(KnownProtocolKind::RawRepresentable);

    return nullptr;
  }

  return nullptr;
}

DeclRefExpr *
DerivedConformance::createSelfDeclRef(AbstractFunctionDecl *fn) {
  ASTContext &C = fn->getASTContext();

  auto selfDecl = fn->getImplicitSelfDecl();
  return new (C) DeclRefExpr(selfDecl, DeclNameLoc(), /*implicit*/true);
}

FuncDecl *DerivedConformance::declareDerivedPropertyGetter(TypeChecker &tc,
                                                 Decl *parentDecl,
                                                 NominalTypeDecl *typeDecl,
                                                 Type propertyInterfaceType,
                                                 Type propertyContextType,
                                                 bool isStatic,
                                                 bool isFinal) {
  auto &C = tc.Context;
  auto parentDC = cast<DeclContext>(parentDecl);
  auto selfDecl = ParamDecl::createSelf(SourceLoc(), parentDC, isStatic);
  ParameterList *params[] = {
    ParameterList::createWithoutLoc(selfDecl),
    ParameterList::createEmpty(C)
  };
  
  FuncDecl *getterDecl =
    FuncDecl::create(C, /*StaticLoc=*/SourceLoc(), StaticSpellingKind::None,
                     /*FuncLoc=*/SourceLoc(), DeclName(), /*NameLoc=*/SourceLoc(),
                     /*Throws=*/false, /*ThrowsLoc=*/SourceLoc(),
                     /*AccessorKeywordLoc=*/SourceLoc(),
                     nullptr, params,
                     TypeLoc::withoutLoc(propertyInterfaceType), parentDC);
  getterDecl->setImplicit();
  getterDecl->setStatic(isStatic);

  // If this is supposed to be a final method, mark it as such.
  assert(isFinal || !parentDC->getAsClassOrClassExtensionContext());
  if (isFinal && parentDC->getAsClassOrClassExtensionContext() &&
      !getterDecl->isFinal())
    getterDecl->getAttrs().add(new (C) FinalAttr(/*IsImplicit=*/true));

  // Compute the interface type of the getter.
  Type interfaceType = FunctionType::get(TupleType::getEmpty(C),
                                         propertyInterfaceType);
  Type selfInterfaceType = getterDecl->computeInterfaceSelfType();
  if (auto sig = parentDC->getGenericSignatureOfContext()) {
    getterDecl->setGenericEnvironment(
        parentDC->getGenericEnvironmentOfContext());
    interfaceType = GenericFunctionType::get(sig, selfInterfaceType,
                                             interfaceType,
                                             FunctionType::ExtInfo());
  } else
    interfaceType = FunctionType::get(selfInterfaceType, interfaceType);
  getterDecl->setInterfaceType(interfaceType);
  getterDecl->setAccessibility(std::max(typeDecl->getFormalAccess(),
                                        Accessibility::Internal));

  // If the enum was not imported, the derived conformance is either from the
  // enum itself or an extension, in which case we will emit the declaration
  // normally.
  if (parentDecl->hasClangNode())
    tc.Context.addExternalDecl(getterDecl);

  return getterDecl;
}

std::pair<VarDecl *, PatternBindingDecl *>
DerivedConformance::declareDerivedReadOnlyProperty(TypeChecker &tc,
                                                   Decl *parentDecl,
                                                   NominalTypeDecl *typeDecl,
                                                   Identifier name,
                                                   Type propertyInterfaceType,
                                                   Type propertyContextType,
                                                   FuncDecl *getterDecl,
                                                   bool isStatic,
                                                   bool isFinal) {
  auto &C = tc.Context;
  auto parentDC = cast<DeclContext>(parentDecl);

  VarDecl *propDecl = new (C) VarDecl(/*IsStatic*/isStatic, /*IsLet*/false,
                                      /*IsCaptureList*/false, SourceLoc(), name,
                                      propertyContextType, parentDC);
  propDecl->setImplicit();
  propDecl->makeComputed(SourceLoc(), getterDecl, nullptr, nullptr,
                         SourceLoc());
  propDecl->setAccessibility(getterDecl->getFormalAccess());
  propDecl->setInterfaceType(propertyInterfaceType);

  // If this is supposed to be a final property, mark it as such.
  assert(isFinal || !parentDC->getAsClassOrClassExtensionContext());
  if (isFinal && parentDC->getAsClassOrClassExtensionContext() &&
      !propDecl->isFinal())
    propDecl->getAttrs().add(new (C) FinalAttr(/*IsImplicit=*/true));

  Pattern *propPat = new (C) NamedPattern(propDecl, /*implicit*/ true);
  propPat->setType(propertyContextType);
  propPat = new (C) TypedPattern(propPat,
                                 TypeLoc::withoutLoc(propertyContextType),
                                 /*implicit*/ true);

  auto pbDecl = PatternBindingDecl::create(C, SourceLoc(),
                                           StaticSpellingKind::None,
                                           SourceLoc(), propPat, nullptr,
                                           parentDC);
  pbDecl->setImplicit();

  return {propDecl, pbDecl};
}
