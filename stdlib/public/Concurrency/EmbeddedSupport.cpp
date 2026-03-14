//===--- EmbeddedSupport.cpp ----------------------------------------------===//
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
//
// Miscellaneous support code for Swift Concurrency on embedded Swift.
//
//===----------------------------------------------------------------------===//

#if SWIFT_CONCURRENCY_EMBEDDED

#include "swift/shims/Visibility.h"
#include <cstdarg>
#include <cstdint>
#include <cstdlib>

// TSan hooks not supported in embedded Swift.

SWIFT_RUNTIME_EXPORT
void (*_swift_tsan_acquire)(const void *) = nullptr;

SWIFT_RUNTIME_EXPORT
void (*_swift_tsan_release)(const void *) = nullptr;

// TODO: Concurrency Exclusivity tracking not yet supported in embedded Swift.

SWIFT_RUNTIME_EXPORT
void swift_task_enterThreadLocalContext(char *state) {}

SWIFT_RUNTIME_EXPORT
void swift_task_exitThreadLocalContext(char *state) {}

#endif
