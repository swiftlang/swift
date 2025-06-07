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
      static_cast<SwiftTLSContext *>(swift::tls_get(swift::tls_key::runtime));
  if (ctx)
    return *ctx;

  static swift::once_t token;
  swift::tls_init_once(token, swift::tls_key::runtime, [](void *pointer) {
    swift_cxx_deleteObject(static_cast<SwiftTLSContext *>(pointer));
  });

  ctx = swift_cxx_newObject<SwiftTLSContext>();
  swift::tls_set(swift::tls_key::runtime, ctx);
  return *ctx;

#elif defined(SWIFT_THREAD_LOCAL)

  // If we have the thread local attribute, use that
  // (note that this happens for the no-threads case too)
  static SWIFT_THREAD_LOCAL SwiftTLSContext TLSContext;
  return TLSContext;

#else

  // Otherwise, allocate ourselves a key and use that
  static swift::tls_key_t runtimeKey;
  static swift::once_t token;

  swift::tls_alloc_once(token, runtimeKey, [](void *pointer) {
    swift_cxx_deleteObject(static_cast<SwiftTLSContext *>(pointer));
  });

  SwiftTLSContext *ctx =
      static_cast<SwiftTLSContext *>(swift::tls_get(runtimeKey));
  if (ctx)
    return *ctx;

  ctx = swift_cxx_newObject<SwiftTLSContext>();
  swift::tls_set(runtimeKey, ctx);
  return *ctx;

#endif
}
