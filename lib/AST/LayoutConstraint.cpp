//===--- LayoutConstraint.cpp - Layout constraints types and APIs ---------===//
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
// This file implements APIs for layout constraints.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Types.h"

namespace swift {

/// This helper function is typically used by the parser to
/// map a type name to a corresponding layout constraint if possible.
LayoutConstraintInfo getLayoutConstraintInfo(Identifier ID, ASTContext &Ctx) {
  if (ID == Ctx.Id_TrivialLayout)
    return LayoutConstraintInfo(LayoutConstraintKind::TrivialOfExactSize, 0, 0);

  if (ID == Ctx.Id_TrivialAtMostLayout)
    return LayoutConstraintInfo(LayoutConstraintKind::TrivialOfAtMostSize, 0,
                                0);

  if (ID == Ctx.Id_RefCountedObjectLayout)
    return LayoutConstraintInfo(LayoutConstraintKind::RefCountedObject);

  if (ID == Ctx.Id_NativeRefCountedObjectLayout)
    return LayoutConstraintInfo(LayoutConstraintKind::NativeRefCountedObject);

  return LayoutConstraintInfo(LayoutConstraintKind::UnknownLayout);
}

StringRef LayoutConstraintInfo::getName() const {
  return getName(getKind());
}

StringRef LayoutConstraintInfo::getName(LayoutConstraintKind Kind) {
  switch (Kind) {
  case LayoutConstraintKind::UnknownLayout:
    return "_UnknownLayout";
  case LayoutConstraintKind::RefCountedObject:
    return "_RefCountedObject";
  case LayoutConstraintKind::NativeRefCountedObject:
    return "_NativeRefCountedObject";
  case LayoutConstraintKind::Trivial:
    return "_Trivial";
  case LayoutConstraintKind::TrivialOfAtMostSize:
    return "_TrivialAtMost";
  case LayoutConstraintKind::TrivialOfExactSize:
    return "_Trivial";
  }

  llvm_unreachable("Unhandled LayoutConstraintKind in switch.");
}

SourceRange LayoutConstraintLoc::getSourceRange() const { return getLoc(); }

} // end namespace swift
