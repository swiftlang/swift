//===----------------------------------------------------------------------===//
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

// clang-format off
// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %S/small-structs-rounded-registers.swift -module-name Structs -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/structs.h
// RUN: %target-interop-build-clangxx -c %s -I %t -o %t/main.o
// RUN: %target-interop-build-swift %S/small-structs-rounded-registers.swift -o %t/main -Xlinker %t/main.o -module-name Structs -Xfrontend -entry-point-function-name -Xfrontend swiftMain
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main

// REQUIRES: executable_test, PTRSIZE=64
// clang-format on

#include "structs.h"
#include <cassert>
#include <cstring>

// Check both directions without relying on a sanitizer: the guard bytes must
// survive a return, and passing a value must zero the unused register bytes
// instead of reading those guard bytes.
template <size_t Size, class ReturnStub, class ReturnDirect, class PassDirect>
void checkCopies(ReturnDirect returnDirect, PassDirect passDirect) {
  unsigned char storage[Size + 8];
  std::memset(storage, 0xA5, sizeof(storage));
  ReturnStub value;
  std::memset(&value, 0x5A, sizeof(value));
  returnDirect(reinterpret_cast<char *>(storage), value);
  for (size_t i = 0; i < Size; ++i)
    assert(storage[i] == 0x5A);
  for (size_t i = Size; i < sizeof(storage); ++i)
    assert(storage[i] == 0xA5);

  auto passed = passDirect(reinterpret_cast<const char *>(storage));
  unsigned char expected[sizeof(passed)] = {};
  std::memcpy(expected, storage, Size);
  assert(std::memcmp(&passed, expected, sizeof(passed)) == 0);
}

int main() {
  using namespace Structs;
  using namespace Structs::_impl;

  static_assert(sizeof(S3) == 3, "");
  static_assert(sizeof(S5) == 5, "");
  static_assert(sizeof(S8) == 8, "");
  static_assert(sizeof(S12) == 12, "");
  static_assert(sizeof(S13) == 13, "");
  static_assert(sizeof(S16) == 16, "");

  checkCopies<3, swift_interop_returnStub_Structs_uint32_t_0_3>(
      swift_interop_returnDirect_Structs_uint32_t_0_3,
      swift_interop_passDirect_Structs_uint32_t_0_3);
  checkCopies<5, swift_interop_returnStub_Structs_uint64_t_0_5>(
      swift_interop_returnDirect_Structs_uint64_t_0_5,
      swift_interop_passDirect_Structs_uint64_t_0_5);
  checkCopies<13, swift_interop_returnStub_Structs_uint64_t_0_8_uint64_t_8_13>(
      swift_interop_returnDirect_Structs_uint64_t_0_8_uint64_t_8_13,
      swift_interop_passDirect_Structs_uint64_t_0_8_uint64_t_8_13);
  checkCopies<16, swift_interop_returnStub_Structs_uint64_t_0_8_uint64_t_8_16>(
      swift_interop_returnDirect_Structs_uint64_t_0_8_uint64_t_8_16,
      swift_interop_passDirect_Structs_uint64_t_0_8_uint64_t_8_16);

  assert(takeS3(makeS3()));
  assert(takeS5(makeS5()));
  assert(takeS8(makeS8()));
  assert(takeS12(makeS12()));
  assert(takeS13(makeS13()));
  assert(takeS16(makeS16()));
  assert(takeDoubleAndBool(makeDoubleAndBool()));
}
