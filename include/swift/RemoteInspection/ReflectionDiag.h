//===--- ReflectionDiag.h - Temporary reflection diagnostics ----*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// TEMPORARY. Diagnostics for investigating why reflection metadata in the
// Android libswiftCore.so isn't found by swift-reflection-dump. Not meant to
// be merged.
//
// Logging is enabled by setting SWIFT_REFLECTION_DIAG in the environment, and
// goes to stderr, unbuffered, so nothing is lost if the process crashes.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_REFLECTION_REFLECTIONDIAG_H
#define SWIFT_REFLECTION_REFLECTIONDIAG_H

#include <cstdio>
#include <cstdlib>

namespace swift {
namespace reflection {

inline bool diagEnabled() {
  static const bool enabled = getenv("SWIFT_REFLECTION_DIAG") != nullptr;
  return enabled;
}

} // end namespace reflection
} // end namespace swift

// The first argument must be a string literal.
#define REFLECTION_DIAG(...)                                                   \
  do {                                                                         \
    if (::swift::reflection::diagEnabled()) {                                  \
      fprintf(stderr, "[reflection-diag] " __VA_ARGS__);                       \
      fputc('\n', stderr);                                                     \
      fflush(stderr);                                                          \
    }                                                                          \
  } while (0)

#endif // SWIFT_REFLECTION_REFLECTIONDIAG_H
