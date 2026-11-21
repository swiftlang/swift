//===--- HiddenTypeLayout.cpp - Abstract member layout analysis -----------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/AST/HiddenTypeLayout.h"
#include "swift/AST/Decl.h"
#include "swift/AST/FileUnit.h"
#include <functional>

using namespace swift;

void swift::forEachRequiredAbstractMemberTypeLayout(
    llvm::ArrayRef<const FileUnit *> files,
    llvm::function_ref<void(
        const AbstractMemberTypeLayoutRequirement &)> callback) {
  std::function<void(Decl *)> processDecl = [&](Decl *decl) {
    if (auto *structDecl = dyn_cast<StructDecl>(decl)) {
      auto access = structDecl->getFormalAccessScope(
          /*useDC=*/nullptr, /*treatUsableFromInlineAsPublic=*/true);
      if (access.isPublicOrPackage()) {
        for (auto *property : structDecl->getStoredProperties()) {
          if (!property->isExposedToClients())
            callback({structDecl, property});
        }
      }
    }

    if (auto *iterable = dyn_cast<IterableDeclContext>(decl)) {
      for (auto *member : iterable->getAllMembers())
        processDecl(member);
    }
  };

  for (auto *file : files) {
    SmallVector<Decl *, 32> fileDecls;
    file->getTopLevelDeclsWithAuxiliaryDecls(fileDecls);
    for (auto *decl : fileDecls)
      processDecl(decl);
  }
}
