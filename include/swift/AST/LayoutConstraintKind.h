//===-- LayoutConstraintKind.h - Layout constraints kinds -------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_LAYOUT_CONSTRAINTKIND_H
#define SWIFT_LAYOUT_CONSTRAINTKIND_H

/// This header is included in a bridging header. Be *very* careful with what
/// you include here! See include caveats in `ASTBridging.h`.
#include "swift/Basic/SwiftBridging.h"
#include <stdint.h>

namespace swift {
/// Describes a layout constraint information.
enum class ENUM_EXTENSIBILITY_ATTR(closed) LayoutConstraintKind : uint8_t {
  // It is not a known layout constraint.
  UnknownLayout SWIFT_NAME("unknownLayout"),
  // It is a layout constraint representing a trivial type of a known size.
  TrivialOfExactSize SWIFT_NAME("trivialOfExactSize"),
  // It is a layout constraint representing a trivial type of a size known to
  // be no larger than a given size.
  TrivialOfAtMostSize SWIFT_NAME("trivialOfAtMostSize"),
  // It is a layout constraint representing a trivial type of an unknown size.
  Trivial SWIFT_NAME("trivial"),
  // It is a layout constraint representing a reference counted class instance.
  Class SWIFT_NAME("class"),
  // It is a layout constraint representing a reference counted native class
  // instance.
  NativeClass SWIFT_NAME("nativeClass"),
  // It is a layout constraint representing a reference counted object.
  RefCountedObject SWIFT_NAME("refCountedObject"),
  // It is a layout constraint representing a native reference counted object.
  NativeRefCountedObject SWIFT_NAME("nativeRefCountedObject"),
  // It is a layout constraint representing a bridge object
  BridgeObject SWIFT_NAME("bridgeObject"),
  // It is a layout constraint representing a trivial type of a known stride.
  TrivialStride SWIFT_NAME("trivialStride"),
  LastLayout = TrivialStride,
};
} // namespace swift

#endif // SWIFT_LAYOUT_CONSTRAINTKIND_H
