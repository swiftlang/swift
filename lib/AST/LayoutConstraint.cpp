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
  if (ID == "_Trivial")
    return LayoutConstraintInfo(LayoutConstraintKind::TrivialOfExactSize, 0, 0);

  if (ID == "_TrivialAtMost")
    return LayoutConstraintInfo(LayoutConstraintKind::TrivialOfAtMostSize, 0,
                                0);

  if (ID == "_RefCountedObject")
    return LayoutConstraintInfo(LayoutConstraintKind::RefCountedObject);

  if (ID == "_NativeRefCountedObject")
    return LayoutConstraintInfo(LayoutConstraintKind::NativeRefCountedObject);

  return LayoutConstraintInfo(LayoutConstraintKind::UnknownLayout);
}

StringRef LayoutConstraintInfo::getName() const {
  switch (getKind()) {
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
}

SourceRange LayoutConstraintLoc::getSourceRange() const { return getLoc(); }

} // end namespace swift
