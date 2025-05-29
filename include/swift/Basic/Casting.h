//===--- Casting.h - Helpers for casting ------------------------*- C++ -*-===//
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

#ifndef SWIFT_BASIC_CASTING_H
#define SWIFT_BASIC_CASTING_H

#include <type_traits>

namespace swift {

/// Cast between two function types. Use in place of std::bit_cast, which
/// doesn't work on ARM64e with address-discriminated signed function types.
///
/// Address-discriminated ptrauth attributes can only be applied to values with
/// a defined storage location, such as struct fields, since their value is
/// inherently tied to their address. They can't be applied to function
/// paremeters. When passing such a value to a templated function, the ptrauth
/// attribute disappears from the inferred type.
///
/// bit_cast takes the source by reference, which means that the ptrauth
/// attribute remains on the inferred type, and the value is not trivially
/// copyable.
///
/// function_cast instead takes the source by value, avoiding that issue and
/// ensuring that passed-in function pointers are always trivially copyable.
template <typename Destination, typename Source>
Destination function_cast(Source source) {
  static_assert(sizeof(Destination) == sizeof(Source),
                "Source and destination must be the same size");
  static_assert(std::is_trivially_copyable_v<Source>,
                "The source type must be trivially constructible");
  static_assert(std::is_trivially_copyable_v<Destination>,
                "The destination type must be trivially constructible");

#if __has_feature(ptrauth_calls)
  // Use reinterpret_cast here so we perform any necessary auth-and-sign.
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wcast-function-type-mismatch"
  return reinterpret_cast<Destination>(source);
#pragma clang diagnostic pop
#else
  Destination destination;
  memcpy(&destination, &source, sizeof(source));
  return destination;
#endif
}

}

#endif
