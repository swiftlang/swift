//===--- TypeCheckTypeWrapper.cpp - type wrappers -------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file implements semantic analysis for type wrappers.
//
//===----------------------------------------------------------------------===//
#include "TypeChecker.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/NameLookupRequests.h"
#include "swift/AST/Pattern.h"
#include "swift/AST/TypeCheckRequests.h"
#include "swift/Basic/LLVM.h"
#include "swift/Basic/SourceLoc.h"

using namespace swift;

/// Create a property declaration and inject it into the given type.
static VarDecl *injectProperty(NominalTypeDecl *parent, Identifier name,
                               Type type, VarDecl::Introducer introducer,
                               Expr *initializer = nullptr) {
  auto &ctx = parent->getASTContext();

  auto *var = new (ctx) VarDecl(/*isStatic=*/false, introducer,
                                /*nameLoc=*/SourceLoc(), name, parent);

  var->setImplicit();
  var->setSynthesized();
  var->copyFormalAccessFrom(parent, /*sourceIsParentContext=*/true);
  var->setInterfaceType(type);

  Pattern *pattern = NamedPattern::createImplicit(ctx, var);
  pattern->setType(type);

  pattern = TypedPattern::createImplicit(ctx, pattern, type);

  auto *PBD = PatternBindingDecl::createImplicit(ctx, StaticSpellingKind::None,
                                                 pattern, initializer, parent);

  parent->addMember(PBD);
  parent->addMember(var);

  return var;
}

NominalTypeDecl *NominalTypeDecl::getTypeWrapper() const {
  auto *mutableSelf = const_cast<NominalTypeDecl *>(this);
  return evaluateOrDefault(getASTContext().evaluator,
                           GetTypeWrapper{mutableSelf}, nullptr);
}

static void getTypeWrappers(
    NominalTypeDecl *decl,
    SmallVectorImpl<std::pair<CustomAttr *, NominalTypeDecl *>> &typeWrappers) {
  auto &ctx = decl->getASTContext();

  for (auto *attr : decl->getAttrs().getAttributes<CustomAttr>()) {
    auto *mutableAttr = const_cast<CustomAttr *>(attr);
    auto *nominal = evaluateOrDefault(
        ctx.evaluator, CustomAttrNominalRequest{mutableAttr, decl}, nullptr);

    if (!nominal)
      continue;

    auto *typeWrapper = nominal->getAttrs().getAttribute<TypeWrapperAttr>();
    if (typeWrapper && typeWrapper->isValid())
      typeWrappers.push_back({mutableAttr, nominal});
  }
}

NominalTypeDecl *GetTypeWrapper::evaluate(Evaluator &evaluator,
                                          NominalTypeDecl *decl) const {
  auto &ctx = decl->getASTContext();

  // Note that we don't actually care whether there are duplicates,
  // using the same type wrapper multiple times is still an error.
  SmallVector<std::pair<CustomAttr *, NominalTypeDecl *>, 2> typeWrappers;

  getTypeWrappers(decl, typeWrappers);

  if (typeWrappers.empty())
    return nullptr;

  if (typeWrappers.size() != 1) {
    ctx.Diags.diagnose(decl, diag::cannot_use_multiple_type_wrappers);
    return nullptr;
  }

  return typeWrappers.front().second;
}

Type GetTypeWrapperType::evaluate(Evaluator &evaluator,
                                  NominalTypeDecl *decl) const {
  SmallVector<std::pair<CustomAttr *, NominalTypeDecl *>, 2> typeWrappers;

  getTypeWrappers(decl, typeWrappers);

  if (typeWrappers.size() != 1)
    return Type();

  auto *typeWrapperAttr = typeWrappers.front().first;
  auto type = evaluateOrDefault(
      evaluator,
      CustomAttrTypeRequest{typeWrapperAttr, decl->getDeclContext(),
                            CustomAttrTypeKind::TypeWrapper},
      Type());

  if (!type || type->hasError()) {
    return ErrorType::get(decl->getASTContext());
  }
  return type;
}

NominalTypeDecl *
GetTypeWrapperStorage::evaluate(Evaluator &evaluator,
                                NominalTypeDecl *parent) const {
  if (!parent->hasTypeWrapper())
    return nullptr;

  auto &ctx = parent->getASTContext();

  auto *storage =
      new (ctx) StructDecl(/*StructLoc=*/SourceLoc(), ctx.Id_TypeWrapperStorage,
                           /*NameLoc=*/SourceLoc(),
                           /*Inheritted=*/{},
                           /*GenericParams=*/nullptr, parent);

  storage->setImplicit();
  storage->setSynthesized();
  storage->copyFormalAccessFrom(parent, /*sourceIsParentContext=*/true);

  parent->addMember(storage);

  return storage;
}

VarDecl *
GetTypeWrapperProperty::evaluate(Evaluator &evaluator,
                                 NominalTypeDecl *parent) const {
  auto &ctx = parent->getASTContext();

  auto *typeWrapper = parent->getTypeWrapper();
  if (!typeWrapper)
    return nullptr;

  auto *storage =
      evaluateOrDefault(ctx.evaluator, GetTypeWrapperStorage{parent}, nullptr);

  auto *typeWrapperType =
      evaluateOrDefault(ctx.evaluator, GetTypeWrapperType{parent}, Type())
          ->castTo<AnyGenericType>();
  assert(typeWrapperType);

  // $_storage: Wrapper<$Storage>
  auto propertyTy = BoundGenericType::get(
      typeWrapper, /*Parent=*/typeWrapperType->getParent(),
      /*genericArgs=*/{storage->getInterfaceType()->getMetatypeInstanceType()});

  return injectProperty(parent, ctx.Id_TypeWrapperProperty,
                        propertyTy, VarDecl::Introducer::Var);
}
