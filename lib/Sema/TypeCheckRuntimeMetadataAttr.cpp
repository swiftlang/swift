//===--- TypeCheckRuntimeMetadataAttr.cpp - type wrappers -----------------===//
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
// This file implements semantic analysis for @runtimeMetadata attributes.
//
//===----------------------------------------------------------------------===//

#include "TypeChecker.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/ASTMangler.h"
#include "swift/AST/Attr.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Initializer.h"
#include "swift/AST/NameLookupRequests.h"
#include "swift/AST/TypeCheckRequests.h"
#include "swift/Basic/LLVM.h"
#include "swift/Basic/SourceLoc.h"
#include "llvm/ADT/SmallString.h"

using namespace swift;

ArrayRef<CustomAttr *> ValueDecl::getRuntimeDiscoverableAttrs() const {
  auto *mutableSelf = const_cast<ValueDecl *>(this);
  return evaluateOrDefault(getASTContext().evaluator,
                           GetRuntimeDiscoverableAttributes{mutableSelf},
                           nullptr);
}

Expr *
ValueDecl::getRuntimeDiscoverableAttributeGenerator(CustomAttr *attr) const {
  auto *mutableSelf = const_cast<ValueDecl *>(this);
  return evaluateOrDefault(
      getASTContext().evaluator,
      SynthesizeRuntimeMetadataAttrGenerator{attr, mutableSelf}, nullptr);
}

Expr *SynthesizeRuntimeMetadataAttrGenerator::evaluate(
    Evaluator &evaluator, CustomAttr *attr, ValueDecl *attachedTo) const {
  auto &ctx = attachedTo->getASTContext();

  auto *attrTypeDecl = evaluateOrDefault(
      ctx.evaluator,
      CustomAttrNominalRequest{attr, attachedTo->getDeclContext()}, nullptr);

  if (!attrTypeDecl)
    return nullptr;

  assert(attrTypeDecl->getAttrs().hasAttribute<RuntimeMetadataAttr>());

  auto *initContext = new (ctx) RuntimeAttributeInitializer(attr, attachedTo);

  Expr *initArgument = nullptr;
  if (auto *nominal = dyn_cast<NominalTypeDecl>(attachedTo)) {
    // Registry attributes on protocols are only used for
    // inference on conforming types.
    if (isa<ProtocolDecl>(nominal))
      return nullptr;

    // Form an initializer call passing in the metatype
    auto *metatype = TypeExpr::createImplicit(nominal->getDeclaredType(), ctx);
    initArgument = new (ctx)
        DotSelfExpr(metatype, /*dot=*/SourceLoc(), /*self=*/SourceLoc());
  } else if (auto *func = dyn_cast<FuncDecl>(attachedTo)) {
    if (auto *nominal = func->getDeclContext()->getSelfNominalTypeDecl()) {
      auto *baseExpr =
          TypeExpr::createImplicit(nominal->getDeclaredInterfaceType(), ctx);

      // Form an initializer call passing in the function reference
      initArgument = new (ctx) MemberRefExpr(baseExpr, /*dotLoc=*/SourceLoc(),
                                             {func}, /*loc=*/DeclNameLoc(),
                                             /*Implicit=*/true);
    } else {
      initArgument = new (ctx)
          DeclRefExpr({func}, /*Loc=*/DeclNameLoc(), /*implicit=*/true);
    }
  } else {
    auto *var = cast<VarDecl>(attachedTo);
    assert(!var->isStatic());

    auto *keyPath =
        KeyPathExpr::createImplicit(ctx, /*backslashLoc=*/SourceLoc(),
                                    {KeyPathExpr::Component::forProperty(
                                        {var}, var->getValueInterfaceType(),
                                        /*Loc=*/SourceLoc())},
                                    /*endLoc=*/SourceLoc());

    // Build a type repr for base of the key path, since attribute
    // could be attached to an inner type, we need to go up decl
    // contexts and add every parent type.
    {
      SmallVector<ComponentIdentTypeRepr *, 2> baseNameComponents;

      auto *DC = var->getDeclContext();
      while (!DC->isModuleContext()) {
        auto *NTD = DC->getSelfNominalTypeDecl();
        // Only contiguous chains of nominals and extensions thereof.
        if (!NTD)
          break;

        auto *component = new (ctx) SimpleIdentTypeRepr(
            /*Loc=*/DeclNameLoc(), NTD->createNameRef());

        // Resolve the component right away, instead of
        // involving name lookup. This plays well with
        // the fact that initializer is anchored on a
        // source file.
        component->setValue(NTD, NTD->getDeclContext());

        baseNameComponents.push_back(component);
        DC = NTD->getDeclContext();
      }

      // Reverse the components to form a valid outer-to-inner name sequence.
      std::reverse(baseNameComponents.begin(), baseNameComponents.end());

      // Set the 'root' of the key path to the newly build base name.
      // We cannot do this via `parsedRoot` because it has strict
      // rules about leading-dot.
      TypeRepr *rootName = nullptr;
      if (baseNameComponents.size() == 1) {
        rootName = baseNameComponents.front();
      } else {
        rootName = CompoundIdentTypeRepr::create(ctx, baseNameComponents);
      }

      keyPath->setRootType(rootName);
    }

    initArgument = keyPath;
  }

  auto reprRange = SourceRange();
  if (auto *repr = attr->getTypeRepr())
    reprRange = repr->getSourceRange();

  auto attrTy = attrTypeDecl->getDeclaredInterfaceType();
  // Drop all of the generic parameters from the type and
  // let type-checker open them while solving.
  attrTy = attrTy.transform([&](Type type) -> Type {
    if (auto *BGT = type->getAs<BoundGenericType>()) {
      return UnboundGenericType::get(BGT->getDecl(), BGT->getParent(), ctx);
    }
    return type;
  });

  auto typeExpr = TypeExpr::createImplicitHack(reprRange.Start, attrTy, ctx);

  // Add the initializer argument at the front of the argument list
  SmallVector<Argument, 4> newArgs;
  newArgs.push_back({/*loc=*/SourceLoc(), ctx.Id_attachedTo, initArgument});
  if (auto *attrArgs = attr->getArgs())
    newArgs.append(attrArgs->begin(), attrArgs->end());

  ArgumentList *argList = ArgumentList::createImplicit(ctx, reprRange.Start,
                                                       newArgs, reprRange.End);
  Expr *init = CallExpr::createImplicit(ctx, typeExpr, argList);

  // result of generator is an optional always.
  Expr *result = CallExpr::createImplicit(
      ctx,
      new (ctx) DeclRefExpr({ctx.getOptionalDecl()}, /*Loc=*/DeclNameLoc(),
                            /*implicit=*/true),
      ArgumentList::forImplicitSingle(ctx, /*label=*/Identifier(), init));

  auto resultTy = TypeChecker::typeCheckExpression(result, initContext);
  if (!resultTy)
    return nullptr;

  TypeChecker::contextualizeInitializer(initContext, result);
  TypeChecker::checkInitializerEffects(initContext, result);

  return result;
}
