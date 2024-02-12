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

#if defined(__APPLE__)

#include <mach/vm_param.h>
#include <stdint.h>
#include <ptrauth.h>

struct CSTypeRef {
  uintptr_t a, b;
};

struct Range {
  uintptr_t location, length;
};

static inline uintptr_t GetPtrauthMask(void) {
#if __has_feature(ptrauth_calls)
  return (uintptr_t)ptrauth_strip((void*)0x0007ffffffffffff, 0);
#elif __arm64__ && __LP64__
  // Mask all bits above the top of MACH_VM_MAX_ADDRESS, which will
  // match the above ptrauth_strip.
  return (uintptr_t)~0ull >> __builtin_clzll(MACH_VM_MAX_ADDRESS);
#else
  return (uintptr_t)~0ull;
#endif
}

#endif
