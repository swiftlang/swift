//===- LayoutConstraint.cpp - Layout constraints types and APIs -*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file implements APIs for layout constraints.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/Decl.h"
#include "swift/AST/Types.h"
#include "swift/AST/TypeRepr.h"
#include "llvm/ADT/StringSwitch.h"

namespace swift {

LayoutConstraintInfo getLayoutConstraintInfo(StringRef ID) {
  auto LayoutKind =
      llvm::StringSwitch<LayoutConstraintKind>(ID)
          .Case("_Trivial", LayoutConstraintKind::Trivial)
          .Case("_Trivial8", LayoutConstraintKind::Trivial8)
          .Case("_Trivial16", LayoutConstraintKind::Trivial16)
          .Case("_Trivial32", LayoutConstraintKind::Trivial32)
          .Case("_Trivial64", LayoutConstraintKind::Trivial64)
          .Case("_Trivial128", LayoutConstraintKind::Trivial128)
          .Case("_Trivial256", LayoutConstraintKind::Trivial256)
          .Case("_Trivial512", LayoutConstraintKind::Trivial512)
          .Case("_RefCountedObject", LayoutConstraintKind::RefCountedObject)
          .Default(LayoutConstraintKind::UnknownLayout);
  return LayoutConstraintInfo(LayoutKind);
}

/// Checks if a given TypeRepr is a layout constraint type.
bool isLayoutConstraintType(TypeRepr *TyR) {
  return getLayoutConstraintInfo(TyR).isKnownLayout();
}

/// Checks if a given TypeRepr is a layout constraint and returns this
/// constraint. If ID does not match any known layout constrains,
/// returns UnknownLayout.
LayoutConstraintInfo getLayoutConstraintInfo(TypeRepr *TyR) {
  auto SimpleIdentType = dyn_cast<SimpleIdentTypeRepr>(TyR);
  if (!SimpleIdentType)
    return LayoutConstraintInfo(LayoutConstraintKind::UnknownLayout);

  auto TypeName = SimpleIdentType->getIdentifier().str();
  return getLayoutConstraintInfo(TypeName);
}

/// Checks if a given Type is a layout constraint and returns this
/// constraint. If ID does not match any known layout constrains,
/// returns UnknownLayout.
LayoutConstraintInfo getLayoutConstraintInfo(Type Ty) {
  auto CanTy = Ty.getCanonicalTypeOrNull();
  if (auto Archetype = dyn_cast<ArchetypeType>(CanTy.getPointer())) {
    auto Protocols = Archetype->getConformsTo();
    for (auto Proto : Protocols) {
      auto LayoutInfo = getLayoutConstraintInfo(Proto->getDeclaredType());
      if (LayoutInfo.isKnownLayout())
        return LayoutInfo;
    }
    return LayoutConstraintInfo(LayoutConstraintKind::UnknownLayout);
  }

  auto *NTD = CanTy.getNominalOrBoundGenericNominal();
  if (!NTD)
    return LayoutConstraintInfo(LayoutConstraintKind::UnknownLayout);

  auto TypeName = NTD->getNameStr();
  return getLayoutConstraintInfo(TypeName);
}

} // end namespace swift
