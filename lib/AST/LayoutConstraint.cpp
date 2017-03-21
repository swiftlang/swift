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
LayoutConstraint getLayoutConstraint(Identifier ID, ASTContext &Ctx) {
  if (ID == Ctx.Id_TrivialLayout)
    return LayoutConstraint::getLayoutConstraint(
        LayoutConstraintKind::TrivialOfExactSize, 0, 0, Ctx);

  if (ID == Ctx.Id_TrivialAtMostLayout)
    return LayoutConstraint::getLayoutConstraint(
        LayoutConstraintKind::TrivialOfAtMostSize, 0, 0, Ctx);

  if (ID == Ctx.Id_RefCountedObjectLayout)
    return LayoutConstraint::getLayoutConstraint(
        LayoutConstraintKind::RefCountedObject, Ctx);

  if (ID == Ctx.Id_NativeRefCountedObjectLayout)
    return LayoutConstraint::getLayoutConstraint(
        LayoutConstraintKind::NativeRefCountedObject, Ctx);

  return LayoutConstraint::getLayoutConstraint(
      LayoutConstraintKind::UnknownLayout, Ctx);
}

StringRef LayoutConstraintInfo::getName() const {
  return getName(getKind());
}

StringRef LayoutConstraintInfo::getName(LayoutConstraintKind Kind) {
  switch (Kind) {
  case LayoutConstraintKind::UnknownLayout:
    return "_UnknownLayout";
  case LayoutConstraintKind::Class:
    return "_Class";
  case LayoutConstraintKind::NativeClass:
    return "_NativeClass";
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

/// Uniquing for the LayoutConstraintInfo.
void LayoutConstraintInfo::Profile(llvm::FoldingSetNodeID &ID,
                                   LayoutConstraintKind Kind,
                                   unsigned SizeInBits,
                                   unsigned Alignment) {
  ID.AddInteger((unsigned)Kind);
  ID.AddInteger(SizeInBits);
  ID.AddInteger(Alignment);
}

bool LayoutConstraintInfo::isKnownLayout(LayoutConstraintKind Kind) {
  return Kind != LayoutConstraintKind::UnknownLayout;
}

bool LayoutConstraintInfo::isFixedSizeTrivial(LayoutConstraintKind Kind) {
  return Kind == LayoutConstraintKind::TrivialOfExactSize;
}

bool LayoutConstraintInfo::isKnownSizeTrivial(LayoutConstraintKind Kind) {
  return Kind > LayoutConstraintKind::UnknownLayout &&
         Kind < LayoutConstraintKind::Trivial;
}

bool LayoutConstraintInfo::isAddressOnlyTrivial(LayoutConstraintKind Kind) {
  return Kind == LayoutConstraintKind::Trivial;
}

bool LayoutConstraintInfo::isTrivial(LayoutConstraintKind Kind) {
  return Kind > LayoutConstraintKind::UnknownLayout &&
         Kind <= LayoutConstraintKind::Trivial;
}

bool LayoutConstraintInfo::isRefCountedObject(LayoutConstraintKind Kind) {
  return Kind == LayoutConstraintKind::RefCountedObject;
}

bool LayoutConstraintInfo::isNativeRefCountedObject(LayoutConstraintKind Kind) {
  return Kind == LayoutConstraintKind::NativeRefCountedObject;
}

bool LayoutConstraintInfo::isAnyRefCountedObject(LayoutConstraintKind Kind) {
  return isRefCountedObject(Kind) || isNativeRefCountedObject(Kind);
}

bool LayoutConstraintInfo::isClass(LayoutConstraintKind Kind) {
  return Kind == LayoutConstraintKind::Class ||
         Kind == LayoutConstraintKind::NativeClass;
}

bool LayoutConstraintInfo::isNativeClass(LayoutConstraintKind Kind) {
  return Kind == LayoutConstraintKind::NativeClass;
}

bool LayoutConstraintInfo::isRefCounted(LayoutConstraintKind Kind) {
  return isAnyRefCountedObject(Kind) || isClass(Kind);
}

bool LayoutConstraintInfo::isNativeRefCounted(LayoutConstraintKind Kind) {
  return Kind == LayoutConstraintKind::NativeRefCountedObject ||
         Kind == LayoutConstraintKind::NativeClass;
}

void *LayoutConstraintInfo::operator new(size_t bytes, const ASTContext &ctx,
                                         AllocationArena arena,
                                         unsigned alignment) {
  return ctx.Allocate(bytes, alignment, arena);
}

SourceRange LayoutConstraintLoc::getSourceRange() const { return getLoc(); }


LayoutConstraint
LayoutConstraint::getLayoutConstraint(LayoutConstraintKind Kind) {
  assert(!LayoutConstraintInfo::isKnownSizeTrivial(Kind));
  switch(Kind) {
  case LayoutConstraintKind::Trivial:
    return LayoutConstraint(&LayoutConstraintInfo::TrivialConstraintInfo);
  case LayoutConstraintKind::NativeClass:
    return LayoutConstraint(&LayoutConstraintInfo::NativeClassConstraintInfo);
  case LayoutConstraintKind::Class:
    return LayoutConstraint(&LayoutConstraintInfo::ClassConstraintInfo);
  case LayoutConstraintKind::NativeRefCountedObject:
    return LayoutConstraint(
        &LayoutConstraintInfo::NativeRefCountedObjectConstraintInfo);
  case LayoutConstraintKind::RefCountedObject:
    return LayoutConstraint(
        &LayoutConstraintInfo::RefCountedObjectConstraintInfo);
  case LayoutConstraintKind::UnknownLayout:
    return LayoutConstraint(&LayoutConstraintInfo::UnknownLayoutConstraintInfo);
  case LayoutConstraintKind::TrivialOfAtMostSize:
  case LayoutConstraintKind::TrivialOfExactSize:
    llvm_unreachable("Wrong layout constraint kind");
  }
}

LayoutConstraint LayoutConstraint::getUnknownLayout() {
  return LayoutConstraint(&LayoutConstraintInfo::UnknownLayoutConstraintInfo);
}

LayoutConstraintInfo LayoutConstraintInfo::UnknownLayoutConstraintInfo;

LayoutConstraintInfo LayoutConstraintInfo::RefCountedObjectConstraintInfo(
    LayoutConstraintKind::RefCountedObject);

LayoutConstraintInfo LayoutConstraintInfo::NativeRefCountedObjectConstraintInfo(
    LayoutConstraintKind::NativeRefCountedObject);

LayoutConstraintInfo LayoutConstraintInfo::ClassConstraintInfo(
    LayoutConstraintKind::Class);

LayoutConstraintInfo LayoutConstraintInfo::NativeClassConstraintInfo(
    LayoutConstraintKind::NativeClass);

LayoutConstraintInfo LayoutConstraintInfo::TrivialConstraintInfo(
    LayoutConstraintKind::Trivial);

} // end namespace swift
