//===--- Runtime.cpp - Runtime functions exposed in Runtime module ---- ===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Definitions relating to public runtime module.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_RUNTIME_MANGLING_H
#define SWIFT_RUNTIME_MANGLING_H

#ifdef __linux__
#include <sys/types.h>
#include <sys/wait.h>

#include <signal.h>
#endif // defined(__linux__)

#include "swift/Runtime/Config.h"
#include "swift/Runtime/CrashInfo.h"

#include "swift/shims/Visibility.h"

#include <inttypes.h>

#ifdef __cplusplus
namespace swift {
namespace runtime {
namespace mangling {
#endif

SWIFT_RUNTIME_STDLIB_SPI
size_t _swift_runtime_demangle(
  const char *mangledName, size_t mangledNameLength,
  char *outputBuffer, size_t *outputBufferSize,
  size_t flags
);

SWIFT_RUNTIME_STDLIB_SPI
char *_swift_runtime_demangle_allocate(
  const char *mangledName, size_t mangledNameLength,
  size_t *outputBufferSize,
  size_t flags
);

#ifdef __cplusplus
} // namespace mangling
} // namespace runtime
} // namespace swift
#endif

#endif // SWIFT_RUNTIME_MANGLING_H
