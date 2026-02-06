//===--- Subtyping.h - Swift subtyping and conversion rules -----*- C++ -*-===//
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
//
// This file implements various utilities for reasoning about the Swift
// subtyping relation.
//
//===----------------------------------------------------------------------===//
#ifndef SWIFT_SEMA_SUBTYPING_H
#define SWIFT_SEMA_SUBTYPING_H

namespace swift {

class Type;

namespace constraints {

enum class ConversionBehavior : unsigned {
  None,
  Class,
  AnyHashable,
  Double,
  Pointer,
  Array,
  Dictionary,
  Set,
  Optional,
  Structural,
  Unknown
};

/// Classify the possible conversions having this type as result type.
ConversionBehavior getConversionBehavior(Type type);

/// Check whether there exists a type that could be implicitly converted
/// to a given type i.e. is the given type is Double or Optional<..> this
/// function is going to return true because CGFloat could be converted
/// to a Double and non-optional value could be injected into an optional.
bool hasConversions(Type type);

}  // end namespace constraints

}  // end namespace swift

#endif  // SWIFT_SEMA_SUBTYPING_H