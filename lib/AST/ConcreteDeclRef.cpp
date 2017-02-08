//===--- ConcreteDeclRef.cpp - Reference to a concrete decl ---------------===//
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
//
// This file implements the ConcreteDeclRef class, which provides a reference to
// a declaration that is potentially specialized.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/ASTContext.h"
#include "swift/AST/ConcreteDeclRef.h"
#include "swift/AST/Decl.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/Types.h"
#include "llvm/Support/raw_ostream.h"
using namespace swift;

ConcreteDeclRef::SpecializedDeclRef *
ConcreteDeclRef::SpecializedDeclRef::create(
                                       ASTContext &ctx, ValueDecl *decl,
                                       SubstitutionList substitutions) {
  size_t size = totalSizeToAlloc<Substitution>(substitutions.size());
  void *memory = ctx.Allocate(size, alignof(SpecializedDeclRef));
  return new (memory) SpecializedDeclRef(decl, substitutions);
}

ConcreteDeclRef
ConcreteDeclRef::getOverriddenDecl(ASTContext &ctx,
                                   LazyResolver *resolver) const {
  auto *derivedDecl = getDecl();
  auto *baseDecl = derivedDecl->getOverriddenDecl();

  auto *baseSig = baseDecl->getInnermostDeclContext()
      ->getGenericSignatureOfContext();
  auto *derivedSig = derivedDecl->getInnermostDeclContext()
      ->getGenericSignatureOfContext();

  SmallVector<Substitution, 4> subs = {};
  if (baseSig) {
    Optional<SubstitutionMap> derivedSubMap;
    if (derivedSig)
      derivedSubMap = derivedSig->getSubstitutionMap(getSubstitutions());
    auto subMap = SubstitutionMap::getOverrideSubstitutions(
        baseDecl, derivedDecl, derivedSubMap, resolver);
    baseSig->getSubstitutions(subMap, subs);
  }
  return ConcreteDeclRef(ctx, baseDecl, subs);
}

void ConcreteDeclRef::dump(raw_ostream &os) {
  if (!getDecl()) {
    os << "**NULL**";
    return;
  }

  getDecl()->dumpRef(os);

  // If specialized, dump the substitutions.
  if (isSpecialized()) {
    os << " [with ";
    bool isFirst = true;
    for (const auto &sub : getSubstitutions()) {
      if (isFirst) {
        isFirst = false;
      } else {
        os << ", ";
      }

      os << sub.getReplacement().getString();

      if (sub.getConformances().size()) {
        os << '[';
        bool isFirst = true;
        for (auto &c : sub.getConformances()) {
          if (isFirst) {
            isFirst = false;
          } else {
            os << ", ";
          }

          if (c.isConcrete()) {
            c.getConcrete()->printName(os);
          } else {
            os << "abstract:" << c.getAbstract()->getName();
          }
        }
        os << ']';
      }
    }
    os << ']';
  }
}

void ConcreteDeclRef::dump() {
  dump(llvm::errs());
}
