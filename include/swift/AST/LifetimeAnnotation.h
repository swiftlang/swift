//===--- LifetimeAnnotation.h - Lifetime-affecting attributes ---*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Defines a simple type-safe wrapper around the annotations that affect value
// lifetimes.  Used for both Decls and SILFunctionArguments.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_LIFETIMEANNOTATION_H
#define SWIFT_AST_LIFETIMEANNOTATION_H

#include <cstdint>

namespace swift {

/// The annotation on a value (or type) explicitly indicating the lifetime that
/// it (or its instances) should have.
///
/// A LifetimeAnnotation is one of the following three values:
///
/// 1) None: No annotation has been applied.
/// 2) EagerMove: The @_eagerMove attribute has been applied.
/// 3) Lexical: The @_lexical attribute has been applied.
struct LifetimeAnnotation {
  enum Case : uint8_t {
    None,
    EagerMove,
    Lexical,
  } value;

  LifetimeAnnotation(Case newValue) : value(newValue) {}

  operator Case() const { return value; }

  bool isSome() { return value != None; }
};

} // namespace swift

#endif
