//===-- LayoutConstraintKind.h - Layout constraints kinds -------*- C++ -*-===//
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
// This file defines types and APIs for layout constraints.
//
//===----------------------------------------------------------------------===//

#include <cstdint>

#ifndef SWIFT_LAYOUT_CONSTRAINTKIND_H
#define SWIFT_LAYOUT_CONSTRAINTKIND_H

namespace swift {
/// Describes a layout constraint information.
enum class LayoutConstraintKind : uint8_t {
  // It is not a known layout constraint.
  UnknownLayout,
  // It is a layout constraint representing a trivial type of a known size.
  TrivialOfExactSize,
  // It is a layout constraint representing a trivial type of a size known to
  // be no larger than a given size.
  TrivialOfAtMostSize,
  // It is a layout constraint representing a trivial type of an unknown size.
  Trivial,
  // It is a layout constraint representing a reference counted class instance.
  Class,
  // It is a layout constraint representing a reference counted native class
  // instance.
  NativeClass,
  // It is a layout constraint representing a reference counted object.
  RefCountedObject,
  // It is a layout constraint representing a native reference counted object.
  NativeRefCountedObject,
  // It is a layout constraint representing a bridge object
  BridgeObject,
  // It is a layout constraint representing a trivial type of a known stride.
  TrivialStride,
  LastLayout = TrivialStride,
};
} // namespace swift

#endif // SWIFT_LAYOUT_CONSTRAINTKIND_H
