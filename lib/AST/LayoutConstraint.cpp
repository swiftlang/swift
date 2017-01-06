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
#include "cctype"

namespace swift {

LayoutConstraintInfo getLayoutConstraintInfo(StringRef ID) {
  if (ID.consume_front("_Trivial")) {
    if (ID.empty())
      return LayoutConstraintInfo(LayoutConstraintKind::Trivial);
    auto kind = LayoutConstraintKind::TrivialOfExactSize;
    if (ID.consume_front("AtMost"))
      kind = LayoutConstraintKind::TrivialOfAtMostSize;
    auto Suffix = ID;
    // Try to parse the size.
    unsigned size = 0;
    ID = ID.drop_while([&](char c) -> bool {
      if (!isdigit(c))
        return false;
      auto newSize = size;
      newSize *= 10;
      newSize += (c - '0');
      if (newSize > (1 << 16))
        return false;
      size = newSize;
      return true;
    });
    if (!ID.empty() || !isdigit(Suffix[0])) {
      return LayoutConstraintInfo(LayoutConstraintKind::UnknownLayout);
    }
    return LayoutConstraintInfo(kind, size);
  }

  if (ID == "_RefCountedObject")
    return LayoutConstraintInfo(LayoutConstraintKind::RefCountedObject);

  if (ID == "_NativeRefCountedObject")
    return LayoutConstraintInfo(LayoutConstraintKind::NativeRefCountedObject);

  return LayoutConstraintInfo(LayoutConstraintKind::UnknownLayout);
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
  auto CanTy = Ty->getCanonicalType();
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
