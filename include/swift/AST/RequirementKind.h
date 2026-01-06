//===--- RequirementKind.h - Swift RequirementKind AST ---------*- C++ -*-===//
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
// This file defines the RequirementKind enum.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_REQUIREMENTKIND_H
#define SWIFT_AST_REQUIREMENTKIND_H

namespace swift {
/// Describes the kind of a requirement that occurs within a requirements
/// clause.
enum class RequirementKind : unsigned {
  /// A conformance requirement T : P, where T is a type that depends
  /// on a generic parameter and P is a protocol to which T must conform.
  Conformance,
  /// A superclass requirement T : C, where T is a type that depends
  /// on a generic parameter and C is a concrete class type which T must
  /// equal or be a subclass of.
  Superclass,
  /// A same-type requirement T == U, where T and U are types that shall be
  /// equivalent.
  SameType,
  /// A layout bound T : L, where T is a type that depends on a generic
  /// parameter and L is some layout specification that should bound T.
  Layout,
  /// A same-shape requirement shape(T) == shape(U), where T and U are pack
  /// parameters.
  SameShape,

  // Note: there is code that packs this enum in a 3-bit bitfield.  Audit users
  // when adding enumerators.
  LAST_KIND=SameShape
};

} // namespace swift
#endif
