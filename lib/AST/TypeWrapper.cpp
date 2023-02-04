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

clang::PointerAuthQualifier VarDecl::getPointerAuthQualifier() const {
  if (auto *clangDecl = getClangDecl()) {
    if (auto *valueDecl = dyn_cast<clang::ValueDecl>(clangDecl)) {
      return valueDecl->getType().getPointerAuth();
    }
  }
  return clang::PointerAuthQualifier();
}
