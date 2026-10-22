//===--- ScopeRestrictions.cpp - Scopes referenced from types -------------===//
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

#include "swift/AST/ScopeRestrictions.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Module.h"
#include "swift/AST/Types.h"
#include "llvm/Support/raw_ostream.h"

using namespace swift;

void ScopeRef::print(llvm::raw_ostream &os) const {
  switch (getKind()) {
  case Kind::Immortal:
    os << "immortal";
    return;
  case Kind::Param:
    os << "^" << getDepth().Value << "." << getIndex();
    return;
  }
  llvm_unreachable("unhandled kind");
}

void ScopeRef::dump() const {
  print(llvm::errs());
  llvm::errs() << "\n";
}

unsigned swift::getNumScopeParams(const NominalTypeDecl *decl) {
  if (!decl->getASTContext().LangOpts.hasFeature(Feature::ScopeRestrictions))
    return 0;
  if (!isa<StructDecl>(decl) && !isa<EnumDecl>(decl))
    return 0;
  // FIXME: Record scope parameters in serialized modules?
  if (!decl->getModuleContext()->isMainModule())
    return 0;
  // FIXME: Support scope parameters on generic types. Code that rebuilds a
  // nominal type with new generic arguments drops its scopes (e.g.
  // `stripConcurrency`, `TypeOfReference`, `SILTypeSubstitution`).
  if (decl->isGenericContext())
    return 0;
  // A conditionally non-escapable type is restricted by the scopes of its
  // generic arguments instead.
  if (decl->canBeEscapable() != TypeDecl::CanBeInvertible::Never)
    return 0;
  // TODO: Give a type one scope parameter per independently scoped stored
  // property.
  return 1;
}

bool swift::hasScopeUnsaturatedType(Type ty) {
  // Type walks don't look through typealiases.
  return ty->getCanonicalType().findIf([](Type ty) {
    auto *nominal = dyn_cast<NominalType>(ty.getPointer());
    return nominal && !nominal->hasScopeArgs() &&
           getNumScopeParams(nominal->getDecl()) != 0;
  });
}

Type swift::eraseScopes(Type ty) {
  return ty.transformRec([](TypeBase *t) -> std::optional<Type> {
    auto *nominal = dyn_cast<NominalType>(t);
    if (!nominal || !nominal->hasScopeArgs())
      return std::nullopt;
    return Type(NominalType::get(nominal->getDecl(), nominal->getParent(),
                                 nominal->getGenericArgs(),
                                 nominal->getASTContext()));
  });
}
