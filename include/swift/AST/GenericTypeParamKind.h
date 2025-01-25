//===--- GenericTypeParamKind.h ---------------------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines the GenericTypeParamKind enum.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_GENERICTYPEPARAMKIND_H
#define SWIFT_AST_GENERICTYPEPARAMKIND_H

namespace swift {
/// Describes the kind of a generic type parameter that occurs within generic
/// parameter lists.
enum class GenericTypeParamKind: uint8_t {
  /// A regular generic type parameter: 'T'
  Type,
  /// A generic parameter pack: 'each T'
  Pack,
  /// A generic value parameter: 'let T'
  Value
};

} // namespace swift
#endif // #ifndef SWIFT_AST_GENERICTYPEPARAMKIND_H
