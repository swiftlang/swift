//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include <stdint.h>
#include <ptrauth.h>

struct CSTypeRef {
  uintptr_t a, b;
};

struct Range {
  uintptr_t location, length;
};

uintptr_t GetPtrauthMask(void) {
#if __has_feature(ptrauth_calls)
  return (uintptr_t)ptrauth_strip((void*)0x0007ffffffffffff, 0);
#else
  return (uintptr_t)~0ull;
#endif
}
