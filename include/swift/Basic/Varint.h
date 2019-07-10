//===--- Varint.h - Variable length integer encoding ------------*- C++ -*-===//
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
///
/// \file Varint.h
/// Defines transformations of integral types to/from variable length
///        7-bit encoding.
//===----------------------------------------------------------------------===//

#include <numeric>
#include <type_traits>

#include "llvm/ADT/SmallVector.h"

#ifndef SWIFT_BASIC_VARINT_H
#define SWIFT_BASIC_VARINT_H

namespace swift {
namespace Varint {

/// Encode an unsigned integral type to a variable length 7-bit-encoded sequence
/// of bytes.
template <typename T>
llvm::SmallVector<uint8_t, 10> encode(
  typename std::enable_if<
    std::is_integral<T>::value && std::is_unsigned<T>::value, T
  >::type i
) {
  llvm::SmallVector<uint8_t, 10> bytes;
  do {
    uint8_t b = i & 0x7F;
    i >>= 7;
    if (i)
      b |= 0x80;
    bytes.push_back(b);
  } while (i);
  return bytes;
}

/// Encode a signed integral type to a variable length 7-bit-encoded sequence of
/// bytes.
///
/// This transforms the signed value into an unsigned value and delegates
/// to the unsigned version of `encode`.
template <typename T>
llvm::SmallVector<uint8_t, 10> encode(
  typename std::enable_if<
    std::is_integral<T>::value && std::is_signed<T>::value, T
  >::type i
) {
  // Zig-zag encode the signed integer into the unsigned integer type.
  // Negative numbers are encoded as unsigned odd numbers in the
  // unsigned type, positive numbers are even. This prioritizes the
  // smaller numbers around zero, while making it compatible with
  // 7-bit encoding:
  // -3 -> 5
  // -2 -> 3
  // -1 -> 1
  //  0 -> 0
  //  1 -> 2
  //  2 -> 4
  //  3 -> 6
  typename std::make_unsigned<T>::type z = i < 0 ? ~(i << 1) : (i << 1);
  return encode<decltype(z)>(z);
}

/// Decode a variable length 7-bit encoded sequence of bytes to an unsigned
/// integer type.
template <typename T>
typename std::enable_if<
  std::is_integral<T>::value && std::is_unsigned<T>::value, T
>::type
decode(const uint8_t *bytes) {
  size_t i = 0;
  size_t shift = 0;
  T decoded = 0;
  do {
    decoded |= T(bytes[i] & 0x7F) << shift;
    shift += 7;
  } while (bytes[i++] & 0x80);
  return decoded;
}

/// Decode a variable length 7-bit-encoded sequence of bytes to a signed integer
/// type.
///
/// This delegates to the unsigned version of `decode` and transforms the
/// value back into its signed version.
template <typename T>
typename std::enable_if<
  std::is_integral<T>::value && std::is_signed<T>::value, T
>::type
decode(const uint8_t *bytes) {
  auto decoded = decode<typename std::make_unsigned<T>::type>(bytes);
  // Zig-zag decode back into the signed integer type.
  return decoded & 1 ? ~(decoded >> 1) : (decoded >> 1);
}

} // end namespace Varint
} // end namespace swift

#endif // SWIFT_BASIC_VARINT_H
