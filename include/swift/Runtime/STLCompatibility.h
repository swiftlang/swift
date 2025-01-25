//===---- STLCompatibility.h - Runtime C++ Compatibiltiy Stubs --*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_RUNTIME_STL_COMPATIBILITY_H
#define SWIFT_RUNTIME_STL_COMPATIBILITY_H

#if __cplusplus >= 202002l || defined(__cpp_lib_bit_cast)
#include <bit>
#else
#include <cstdint>
#include <cstring>
#include <memory>
#include <type_traits>

namespace std {
inline namespace __swift {
template <typename Destination, typename Source>
std::enable_if_t<sizeof(Destination) == sizeof(Source) &&
                 std::is_trivially_copyable_v<Source> &&
                 std::is_trivially_copyable_v<Destination>, Destination>
bit_cast(const Source &src) noexcept {
  static_assert(std::is_trivially_constructible_v<Destination>,
                "The destination type must be trivially constructible");
  Destination dst;
  if constexpr (std::is_pointer_v<Source> || std::is_pointer_v<Destination>)
    std::memcpy(reinterpret_cast<uintptr_t *>(&dst),
                reinterpret_cast<const uintptr_t *>(&src), sizeof(Destination));
  else
    std::memcpy(&dst, &src, sizeof(Destination));
  return dst;
}
}
}
#endif

#endif
