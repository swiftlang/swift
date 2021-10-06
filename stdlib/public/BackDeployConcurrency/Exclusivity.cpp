//===--- Exclusivity.cpp - Exclusivity tracking ---------------------------===//
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
// This implements the runtime support for dynamically tracking exclusivity.
//
//===----------------------------------------------------------------------===//
#include <cinttypes>

#include "swift/Basic/Lazy.h"
#include "swift/Runtime/Exclusivity.h"
#include "swift/Runtime/ThreadLocalStorage.h"
#include "../runtime/ExclusivityPrivate.h"
#include "../runtime/SwiftTLSContext.h"

using namespace swift;
using namespace swift::runtime;

SwiftTLSContext &SwiftTLSContext::get() {
  SwiftTLSContext *ctx = static_cast<SwiftTLSContext *>(
      SWIFT_THREAD_GETSPECIFIC(SWIFT_RUNTIME_TLS_KEY));
  if (ctx)
    return *ctx;

  static OnceToken_t setupToken;
  SWIFT_ONCE_F(
      setupToken,
      [](void *) {
        pthread_key_init_np(SWIFT_RUNTIME_TLS_KEY, [](void *pointer) {
          delete static_cast<SwiftTLSContext *>(pointer);
        });
      },
      nullptr);

  ctx = new SwiftTLSContext();
  SWIFT_THREAD_SETSPECIFIC(SWIFT_RUNTIME_TLS_KEY, ctx);
  return *ctx;
}

// Bring in the concurrency-specific exclusivity code.
#include "../runtime/ConcurrencyExclusivity.inc"
