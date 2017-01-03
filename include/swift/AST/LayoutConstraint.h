//===-- LayoutConstraint.h - Layout constraints types and APIs --*- C++ -*-===//
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
// This file defines types and APIs for layout constraints.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_LAYOUT_CONSTRAINT_H
#define SWIFT_LAYOUT_CONSTRAINT_H

#include "llvm/ADT/StringRef.h"

namespace swift {

class Type;
class TypeRepr;

/// Describes a layout constraint information.
enum class LayoutConstraintKind : unsigned char {
  // It is not a known layout constraint.
  UnknownLayout,
  // It is a layout constraint representing a trivial type of an unknown size.
  TrivialOfExactSize,
  // It is a layout constraint representing a trivial type of an unknown size.
  TrivialOfAtMostSize,
  // It is a layout constraint representing a trivial type of an unknown size.
  Trivial,
  // It is a layout constraint representing a reference counted object.
  RefCountedObject,
  // It is a layout constraint representing a native reference counted object.
  NativeRefCountedObject,
};

class LayoutConstraintInfo {
  // Size of the layout in bits.
  unsigned SizeInBits: 24;
  // Kind of the layout.
  LayoutConstraintKind Kind;

public:
  LayoutConstraintInfo(LayoutConstraintKind Kind) : Kind(Kind) {
    assert(!isKnownSizeTrivial() && "Size in bits should be specified");
  }

  LayoutConstraintInfo(LayoutConstraintKind Kind, unsigned Size) : Kind(Kind) {
    assert(
        isKnownSizeTrivial() &&
        "Size in bits should be specified only for trivial layout constraints");
  }

  LayoutConstraintKind getKind() const {
    return Kind;
  }

  bool isKnownLayout() const {
    return Kind != LayoutConstraintKind::UnknownLayout;
  }

  bool isFixedSizeTrivial() const {
    return Kind == LayoutConstraintKind::TrivialOfExactSize;
  }

  bool isKnownSizeTrivial() const {
    return Kind > LayoutConstraintKind::UnknownLayout &&
           Kind < LayoutConstraintKind::Trivial;
  }

  bool isAddressOnlyTrivial() const {
    return Kind == LayoutConstraintKind::Trivial;
  }

  bool isTrivial() const {
    return Kind > LayoutConstraintKind::UnknownLayout &&
           Kind <= LayoutConstraintKind::Trivial;
  }

  bool isRefCountedObject() const {
    return Kind == LayoutConstraintKind::RefCountedObject;
  }

  bool isNativeRefCountedObject() const {
    return Kind == LayoutConstraintKind::NativeRefCountedObject;
  }

  unsigned getTrivialSizeInBytes() {
    assert(isKnownSizeTrivial());
    return (SizeInBits + 7) / 8;
  }

  unsigned getMaxTrivialSizeInBytes() {
    assert(isKnownSizeTrivial());
    return (SizeInBits + 7) / 8;
  }

  unsigned getTrivialSizeInBits() {
    assert(isKnownSizeTrivial());
    return SizeInBits;
  }

  unsigned getMaxTrivialSizeInBits() {
    assert(isKnownSizeTrivial());
    return SizeInBits;
  }
};

/// Checks if ID is a name of a layout constraint and returns this
/// constraint. If ID does not match any known layout constraint names,
/// returns UnknownLayout.
LayoutConstraintInfo getLayoutConstraintInfo(llvm::StringRef ID);

/// Checks if a given TypeRepr is a layout constraint and returns this
/// constraint. If ID does not match any known layout constrains,
/// returns UnknownLayout.
LayoutConstraintInfo getLayoutConstraintInfo(TypeRepr *TyR);

/// Checks if a given Type is a layout constraint and returns this
/// constraint. If ID does not match any known layout constrains,
/// returns UnknownLayout.
LayoutConstraintInfo getLayoutConstraintInfo(Type Ty);

/// Checks if a given TypeRepr is a layout constraint type.
bool isLayoutConstraintType(TypeRepr *TyR);

} // end namespace swift
#endif
