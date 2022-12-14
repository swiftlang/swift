//===--- TypeWrapper.cpp - Type Traversal ---------------------------------===//
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
//  This file implements functionality related to type wrapper feature.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/TypeCheckRequests.h"
#include "swift/AST/TypeResolutionStage.h"
#include "swift/AST/TypeWrappers.h"

using namespace swift;

Optional<TypeWrapperInfo> NominalTypeDecl::getTypeWrapper() const {
  auto *mutableSelf = const_cast<NominalTypeDecl *>(this);
  return evaluateOrDefault(getASTContext().evaluator,
                           GetTypeWrapper{mutableSelf},
                           Optional<TypeWrapperInfo>());
}

VarDecl *ConstructorDecl::getLocalTypeWrapperStorageVar() const {
  auto &ctx = getASTContext();
  auto *mutableSelf = const_cast<ConstructorDecl *>(this);
  return evaluateOrDefault(
      ctx.evaluator, SynthesizeLocalVariableForTypeWrapperStorage{mutableSelf},
      nullptr);
}

bool VarDecl::isTypeWrapperLocalStorageForInitializer() const {
  if (auto *ctor =
          dyn_cast_or_null<ConstructorDecl>(getDeclContext()->getAsDecl())) {
    return this == ctor->getLocalTypeWrapperStorageVar();
  }
  return false;
}

bool UsesTypeWrapperFeature::evaluate(Evaluator &evaluator,
                                      NominalTypeDecl *decl) const {
  // This is a type wrapper type.
  if (decl->getAttrs().hasAttribute<TypeWrapperAttr>())
    return true;

  // This is a type wrapped type.
  if (decl->hasTypeWrapper())
    return true;

  // This type could be depending on a type wrapper feature
  // indirectly by conforming to a protocol with a type
  // wrapper attribute. To determine that we need to walk
  // protocol dependency chains and check each one.

  auto &ctx = decl->getASTContext();

  auto usesTypeWrapperFeature = [&](ProtocolDecl *protocol) {
    return evaluateOrDefault(ctx.evaluator, UsesTypeWrapperFeature{protocol},
                             false);
  };

  for (unsigned i : indices(decl->getInherited())) {
    auto inheritedType = evaluateOrDefault(
        ctx.evaluator,
        InheritedTypeRequest{decl, i, TypeResolutionStage::Interface}, Type());

    if (!(inheritedType && inheritedType->isConstraintType()))
      continue;

    if (auto *protocol =
        dyn_cast_or_null<ProtocolDecl>(inheritedType->getAnyNominal())) {
      if (usesTypeWrapperFeature(protocol))
        return true;
    }

    if (auto composition = inheritedType->getAs<ProtocolCompositionType>()) {
      for (auto member : composition->getMembers()) {
        if (auto *protocol =
                dyn_cast_or_null<ProtocolDecl>(member->getAnyNominal())) {
          if (usesTypeWrapperFeature(protocol))
            return true;
        }
      }
    }
  }

  return false;
}
