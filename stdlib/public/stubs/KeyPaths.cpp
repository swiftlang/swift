//===--- KeyPaths.cpp - Key path helper symbols ---------------------------===//
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

#include "../SwiftShims/Visibility.h"
#include "swift/Runtime/Config.h"
#include <cstdint>
#include <cstring>

SWIFT_CC(swift)
static void copyGenericArguments(const void *src, void *dest, size_t bytes) {
  memcpy(dest, src, bytes);
}

SWIFT_CC(swift)
static bool equateGenericArguments(const void *a, const void *b, size_t bytes) {
  // Generic arguments can't affect equality, since an equivalent key path may
  // have been formed in a fully concrete context without capturing generic
  // arguments.
  return true;
}

SWIFT_CC(swift)
static intptr_t hashGenericArguments(const void *src, size_t bytes) {
  // Generic arguments can't affect equality, since an equivalent key path may
  // have been formed in a fully concrete context without capturing generic
  // arguments. The implementation recognizes a hash value return of '0' as
  // "no effect on the hash".
  return 0;
}

/// A prefab witness table for computed key path components that only include
/// captured generic arguments.
SWIFT_RUNTIME_EXPORT
void *(swift_keyPathGenericWitnessTable[]) = {
  nullptr, // no destructor necessary
  (void*)(uintptr_t)copyGenericArguments,
  (void*)(uintptr_t)equateGenericArguments,
  (void*)(uintptr_t)hashGenericArguments,
};
