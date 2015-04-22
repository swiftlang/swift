//===--- ConcreteDeclRef.cpp - Reference to a concrete decl -----*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file implements the ConcreteDeclRef class, which provides a reference to
// a declaration that is potentially specialized.
//===----------------------------------------------------------------------===//

#include "swift/AST/ASTContext.h"
#include "swift/AST/ConcreteDeclRef.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Types.h"
#include "llvm/Support/raw_ostream.h"
using namespace swift;

ConcreteDeclRef::SpecializedDeclRef *
ConcreteDeclRef::SpecializedDeclRef::create(
                                       ASTContext &ctx, ValueDecl *decl,
                                       ArrayRef<Substitution> substitutions) {
  unsigned size = sizeof(SpecializedDeclRef)
  + sizeof(Substitution) * substitutions.size();
  void *memory = ctx.Allocate(size, alignof(SpecializedDeclRef));
  return new (memory) SpecializedDeclRef(decl, substitutions);
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

      os << sub.getArchetype()->getFullName()
         << "=" << sub.getReplacement().getString();

      if (sub.getConformances().size()) {
        os << '[';
        bool isFirst = true;
        for (const auto *c : sub.getConformances()) {
          if (isFirst) {
            isFirst = false;
          } else {
            os << ", ";
          }

          if (c) {
            c->printName(os);
          } else {
            os << "nullptr";
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
