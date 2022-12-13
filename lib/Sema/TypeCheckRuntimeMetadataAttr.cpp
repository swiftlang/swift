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
using namespace constraints;

std::pair<BraceStmt *, Type>
ValueDecl::getRuntimeDiscoverableAttributeGenerator(CustomAttr *attr) const {
  auto *mutableSelf = const_cast<ValueDecl *>(this);
  auto *body = evaluateOrDefault(
      getASTContext().evaluator,
      SynthesizeRuntimeMetadataAttrGeneratorBody{attr, mutableSelf}, nullptr);
  if (!body)
    return std::make_pair(nullptr, Type());

  auto *init = evaluateOrDefault(
      getASTContext().evaluator,
      SynthesizeRuntimeMetadataAttrGenerator{attr, mutableSelf}, nullptr);
  assert(init);

  return std::make_pair(body, init->getType()->mapTypeOutOfContext());
}

Expr *SynthesizeRuntimeMetadataAttrGenerator::evaluate(
    Evaluator &evaluator, CustomAttr *attr, ValueDecl *attachedTo) const {
  auto &ctx = attachedTo->getASTContext();

  auto attrType = evaluateOrDefault(
      ctx.evaluator,
      CustomAttrTypeRequest{attr, attachedTo->getDeclContext(),
                            CustomAttrTypeKind::RuntimeMetadata},
      nullptr);

  if (!attrType)
    return nullptr;

  auto *attrTypeDecl = attrType->getAnyNominal();
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

  auto typeExpr = TypeExpr::createImplicitHack(reprRange.Start, attrType, ctx);

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

  // Note that the availability checking is disabled for generator expression
  // because availability checker cannot handle synthesized code without any
  // source information (and no declaration for context).
  auto resultTy = TypeChecker::typeCheckExpression(
      result, initContext, ContextualTypeInfo(),
      TypeCheckExprFlags::DisableExprAvailabilityChecking);
  if (!resultTy)
    return nullptr;

  TypeChecker::contextualizeInitializer(initContext, result);
  TypeChecker::checkInitializerEffects(initContext, result);

  return result;
}

BraceStmt *SynthesizeRuntimeMetadataAttrGeneratorBody::evaluate(
    Evaluator &evaluator, CustomAttr *attr, ValueDecl *attachedTo) const {
  auto &ctx = attachedTo->getASTContext();

  Expr *init = evaluateOrDefault(
      ctx.evaluator, SynthesizeRuntimeMetadataAttrGenerator{attr, attachedTo},
      nullptr);
  if (!init)
    return nullptr;

  SmallVector<ASTNode, 4> body;

  auto declAvailability = attachedTo->getAvailabilityForLinkage();
  auto attrAvailability = attachedTo->getRuntimeDiscoverableAttrTypeDecl(attr)
                              ->getAvailabilityForLinkage();
  declAvailability.intersectWith(attrAvailability);

  if (!declAvailability.isAlwaysAvailable()) {

    // if #available(...) {
    //  return <#initializer call#>
    // }
    // return nil

    auto availableOn = declAvailability.getOSVersion().getLowerEndpoint();
    auto *platformSpec = new (ctx) PlatformVersionConstraintAvailabilitySpec(
        targetPlatform(ctx.LangOpts), /*PlatformLoc=*/SourceLoc(), availableOn,
        availableOn, /*VersionSrcRange=*/SourceRange());

    auto *wildcardSpec =
        new (ctx) OtherPlatformAvailabilitySpec(/*StarLoc=*/SourceLoc());

    auto *availabilityInfo = PoundAvailableInfo::create(
        ctx, /*PoundLoc=*/SourceLoc(),
        /*LParenLoc=*/SourceLoc(), {platformSpec, wildcardSpec},
        /*RParenLoc=*/SourceLoc(),
        /*isUnavailability=*/false);
    { availabilityInfo->setAvailableRange(declAvailability.getOSVersion()); }

    NullablePtr<Stmt> thenStmt;
    {
      SmallVector<ASTNode, 4> thenBody;

      thenBody.push_back(new (ctx) ReturnStmt(/*loc=*/SourceLoc(), init,
                                              /*isImplicit=*/true));

      thenStmt = BraceStmt::create(ctx, /*lbloc=*/SourceLoc(), thenBody,
                                   /*rbloc=*/SourceLoc(), /*implicit=*/true);
    }

    NullablePtr<Stmt> elseStmt;
    {
      // return nil

      auto *nil =
          new (ctx) NilLiteralExpr(/*Loc=*/SourceLoc(), /*implicit=*/true);
      nil->setType(init->getType());

      auto *returnNil = new (ctx) ReturnStmt(/*loc=*/SourceLoc(), nil,
                                             /*isImplicit=*/true);

      elseStmt = BraceStmt::create(ctx, /*lbloc=*/SourceLoc(), {returnNil},
                                   /*rbloc=*/SourceLoc(), /*implicit=*/true);
    }

    auto *ifStmt = new (ctx) IfStmt(
        LabeledStmtInfo(), /*ifLoc=*/SourceLoc(),
        ctx.AllocateCopy(ArrayRef<StmtConditionElement>({availabilityInfo})),
        thenStmt.get(),
        /*ElseLoc=*/SourceLoc(), elseStmt.get(), /*Implicit=*/true);

    body.push_back(ifStmt);
  } else {
    // just `return <#initializer value#>`
    body.push_back(new (ctx) ReturnStmt(/*loc=*/SourceLoc(), init,
                                        /*isImplicit=*/true));
  }

  ASTNode braceStmt =
      BraceStmt::create(ctx, /*lbloc=*/attr->getLocation(), body,
                        /*rbloc=*/attr->getLocation(), /*implicit=*/true);

  return cast<BraceStmt>(braceStmt.get<Stmt *>());
}
