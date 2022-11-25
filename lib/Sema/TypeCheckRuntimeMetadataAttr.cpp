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
#include "swift/AST/NameLookupRequests.h"
#include "swift/AST/TypeCheckRequests.h"
#include "swift/Basic/LLVM.h"
#include "swift/Basic/SourceLoc.h"
#include "llvm/ADT/SmallString.h"

using namespace swift;

static BraceStmt *
deriveGeneratorBody(FuncDecl *generator,
                    std::pair<CustomAttr *, ValueDecl *> content) {
  auto &ctx = generator->getASTContext();

  // return nil
  ASTNode body = new (ctx) ReturnStmt(
      SourceLoc(),
      new (ctx) NilLiteralExpr(/*Loc=*/SourceLoc(), /*Implicit=*/true),
      /*isImplicit=*/true);

  return BraceStmt::create(ctx, /*lbloc=*/SourceLoc(), body,
                           /*rbloc=*/SourceLoc(), /*implicit=*/true);
}

FuncDecl *SynthesizeRuntimeMetadataAttrGenerator::evaluate(
    Evaluator &evaluator, CustomAttr *attr, ValueDecl *parent) const {
  auto &ctx = parent->getASTContext();

  auto *attrType = evaluateOrDefault(
      ctx.evaluator, CustomAttrNominalRequest{attr, parent->getDeclContext()},
      nullptr);

  if (!attrType)
    return nullptr;

  assert(attrType->getAttrs().hasAttribute<RuntimeMetadataAttr>());

  Mangle::ASTMangler mangler;

  SmallString<32> nameScratch;

  nameScratch.append("generator$");
  nameScratch.append(mangler.mangleAnyDecl(attrType, /*prefix=*/false));
  nameScratch.append("$");
  nameScratch.append(mangler.mangleAnyDecl(parent, /*prefix=*/false));

  DeclName generatorName(ctx, ctx.getIdentifier(nameScratch.str()),
                         /*argumentNames=*/ArrayRef<Identifier>());

  auto resultType = OptionalType::get(attrType->getInterfaceType());

  // () -> Optional<<#Attribute#>>
  auto *generator = FuncDecl::createImplicit(
      ctx, StaticSpellingKind::None, generatorName, /*NameLoc=*/SourceLoc(),
      /*Async=*/false,
      /*Throws=*/false,
      /*GenericParams=*/nullptr, ParameterList::createEmpty(ctx), resultType,
      /*DC=*/parent->getDeclContext()->getParentModule());

  generator->setSynthesized(true);
  generator->copyFormalAccessFrom(parent, /*sourceIsParentContext=*/false);

  ASTNode body = deriveGeneratorBody(generator, std::make_pair(attr, parent));
  if (!body)
    return nullptr;

  TypeChecker::typeCheckASTNode(body, generator);

  generator->setBody(cast<BraceStmt>(body.get<Stmt *>()),
                     FuncDecl::BodyKind::TypeChecked);

  return generator;
}
