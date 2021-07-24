//===--- SwiftTLSContext.cpp ----------------------------------------------===//
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

#include "SwiftTLSContext.h"
#include "swift/Basic/Lazy.h"
#include "swift/Runtime/Once.h"
#include "swift/Runtime/ThreadLocalStorage.h"

using namespace swift;
using namespace swift::runtime;

#ifdef SWIFT_STDLIB_SINGLE_THREADED_RUNTIME

SwiftTLSContext &SwiftTLSContext::get() {
  static SwiftTLSContext TLSContext;
  return TLSContext;
}

#elif SWIFT_TLS_HAS_RESERVED_PTHREAD_SPECIFIC
// Use the reserved TSD key if possible.

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

#elif __has_feature(cxx_thread_local)
// Second choice is direct language support for thread-locals.

namespace {

static thread_local SwiftTLSContext TLSContext;

} // anonymous namespace

SwiftTLSContext &SwiftTLSContext::get() { return TLSContext; }

#else
// Use the platform thread-local data API.

static __swift_thread_key_t createSwiftThreadKey() {
  __swift_thread_key_t key;
  int result = SWIFT_THREAD_KEY_CREATE(&key, [](void *pointer) {
    delete static_cast<SwiftTLSContext *>(pointer);
  });

  if (result != 0) {
    fatalError(0, "couldn't create thread key for exclusivity: %s\n",
               strerror(result));
  }
  return key;
}

static SwiftTLSContext &getTLSContext() {
  static __swift_thread_key_t key = createSwiftThreadKey();

  SwiftTLSContext *ctx =
      static_cast<SwiftTLSContext *>(SWIFT_THREAD_GETSPECIFIC(key));
  if (!ctx) {
    ctx = new SwiftTLSContext();
    SWIFT_THREAD_SETSPECIFIC(key, ctx);
  }
  return *ctx;
}

#endif
