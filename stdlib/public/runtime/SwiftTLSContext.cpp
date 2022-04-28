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

#include "swift/Threading/Once.h"
#include "swift/Threading/ThreadLocalStorage.h"

using namespace swift;
using namespace swift::runtime;

SwiftTLSContext &SwiftTLSContext::get() {

#if SWIFT_THREADING_USE_RESERVED_TLS_KEYS

  // If we have reserved keys, use those
  SwiftTLSContext *ctx =
      static_cast<SwiftTLSContext *>(swift::tls_get(SWIFT_RUNTIME_TLS_KEY));
  if (ctx)
    return *ctx;

  static swift::once_t token;
  swift::tls_init_once(token, SWIFT_RUNTIME_TLS_KEY, [](void *pointer) {
    delete static_cast<SwiftTLSContext *>(pointer);
  });

  ctx = new SwiftTLSContext();
  swift::tls_set(SWIFT_RUNTIME_TLS_KEY, ctx);
  return *ctx;

#elif defined(SWIFT_THREAD_LOCAL)

  // If we have the thread local attribute, use that
  // (note that this happens for the no-threads case too)
  static SWIFT_THREAD_LOCAL SwiftTLSContext TLSContext;
  return TLSContext;

#else

  // Otherwise, allocate ourselves a key and use that
  static swift::tls_key runtimeKey;
  static swift::once_t token;

  swift::tls_alloc_once(token, runtimeKey, [](void *pointer) {
    delete static_cast<SwiftTLSContext *>(pointer);
  });

  SwiftTLSContext *ctx =
      static_cast<SwiftTLSContext *>(swift::tls_get(runtimeKey));
  if (ctx)
    return *ctx;

  ctx = new SwiftTLSContext();
  swift::tls_set(runtimeKey, ctx);
  return *ctx;

#endif
}
