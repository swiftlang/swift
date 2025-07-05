//===--- GenericTypeParamKind.h ---------------------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2024 - 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
///
/// This file defines the `GenericTypeParamKind` enum.
///
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_GENERICTYPEPARAMKIND_H
#define SWIFT_AST_GENERICTYPEPARAMKIND_H

/// `GenericTypeParamKind.h` is imported into Swift. Be *very* careful with
/// what you include here and keep these includes minimal!
///
/// See include caveats in `BasicBridging.h`.
#include "swift/Basic/SwiftBridging.h"
#include <stdint.h>

namespace swift {
/// Describes the kind of a generic type parameter that occurs within generic
/// parameter lists.
enum class ENUM_EXTENSIBILITY_ATTR(closed) GenericTypeParamKind : uint8_t {
  /// A regular generic type parameter: 'T'
  Type SWIFT_NAME("type"),
  /// A generic parameter pack: 'each T'
  Pack SWIFT_NAME("pack"),
  /// A generic value parameter: 'let T'
  Value SWIFT_NAME("value")
};

} // namespace swift
#endif // #ifndef SWIFT_AST_GENERICTYPEPARAMKIND_H
