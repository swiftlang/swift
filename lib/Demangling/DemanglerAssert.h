//===--- DemanglerAssert.h - Assertions for de/re-mangling ------*- C++ -*-===//
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
//
// This file implements a macro, DEMANGLE_ASSERT(), which will assert in the
// compiler, but in the runtime will return a ManglingError on failure.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_DEMANGLING_ASSERT_H
#define SWIFT_DEMANGLING_ASSERT_H

#include "llvm/Support/Compiler.h"
#include "swift/Demangling/Demangle.h"
#include "swift/Demangling/NamespaceMacros.h"

#if defined(NDEBUG) || defined (SWIFT_RUNTIME)

// In the runtime and non-asserts builds, DEMANGLER_ASSERT() returns an error.
#define DEMANGLER_ASSERT(expr, node)                                           \
  do {                                                                         \
    if (!(expr))                                                               \
      return ManglingError(ManglingError::AssertionFailed, (node), __LINE__);  \
  } while (0)

#else

// Except in unittests, assert builds cause DEMANGLER_ASSERT() to assert()
#define DEMANGLER_ASSERT(expr, node)                                           \
  do {                                                                         \
    if (!(expr)) {                                                             \
      if (Factory.disableAssertionsForUnitTest)                                \
        return ManglingError(ManglingError::AssertionFailed, (node),           \
                             __LINE__);                                        \
      else                                                                     \
        swift::Demangle::failAssert(__FILE__, __LINE__, node, #expr);          \
    }                                                                          \
  } while (0)

#endif

// DEMANGLER_ALWAYS_ASSERT() *always* fails the program, even in the runtime
#define DEMANGLER_ALWAYS_ASSERT(expr, node)                                    \
  do {                                                                         \
    if (!(expr))                                                               \
      swift::Demangle::failAssert(__FILE__, __LINE__, node, #expr);            \
  } while (0)

namespace swift {
namespace Demangle {
SWIFT_BEGIN_INLINE_NAMESPACE

[[noreturn]] void failAssert(const char *file, unsigned line, NodePointer node,
                             const char *expr);

SWIFT_END_INLINE_NAMESPACE
} // end namespace Demangle
} // end namespace swift

#endif // SWIFT_DEMANGLING_ASSERT_H
