//===--- NameLookup.cpp - Name lookup utilities ---------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "NameLookup.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Module.h"
#include "llvm/ADT/SmallVector.h"
#include <algorithm>

using namespace swift;
using namespace rewriting;

void
swift::rewriting::lookupConcreteNestedType(
    NominalTypeDecl *decl,
    Identifier name,
    SmallVectorImpl<TypeDecl *> &concreteDecls) {
  SmallVector<ValueDecl *, 2> foundMembers;
  decl->getParentModule()->lookupQualified(
      decl, DeclNameRef(name),
      NL_QualifiedDefault | NL_OnlyTypes | NL_ProtocolMembers,
      foundMembers);
  for (auto member : foundMembers)
    concreteDecls.push_back(cast<TypeDecl>(member));
}

TypeDecl *
swift::rewriting::findBestConcreteNestedType(
    SmallVectorImpl<TypeDecl *> &concreteDecls) {
  if (concreteDecls.empty())
    return nullptr;

  return *std::min_element(concreteDecls.begin(), concreteDecls.end(),
                           [](TypeDecl *type1, TypeDecl *type2) {
                             return TypeDecl::compare(type1, type2) < 0;
                           });
}
