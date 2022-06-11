//===--- ThreadLocalStorage.cpp -------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include <cstring>

#include "SwiftShims/ThreadLocalStorage.h"
#include "swift/Runtime/Debug.h"
#include "swift/Threading/ThreadLocalStorage.h"

SWIFT_CC(swift) SWIFT_RUNTIME_STDLIB_API
void _stdlib_destroyTLS(void *);

SWIFT_CC(swift) SWIFT_RUNTIME_STDLIB_API
void *_stdlib_createTLS(void);

SWIFT_RUNTIME_STDLIB_INTERNAL
void *
_swift_stdlib_threadLocalStorageGet(void) {

#if SWIFT_THREADING_NONE

  // If there's no threading, we can just keep a static variable.
  static void *value = _stdlib_createTLS();
  return value;

#elif SWIFT_THREADING_USE_RESERVED_TLS_KEYS

  // If we have reserved keys, use those
  void *value = swift::tls_get(swift::tls_key::stdlib);
  if (value)
    return value;

  static swift::once_t token;
  swift::tls_init_once(token, swift::tls_key::stdlib,
                       [](void *pointer) { _stdlib_destroyTLS(pointer); });

  value = _stdlib_createTLS();
  swift::tls_set(swift::tls_key::stdlib, value);
  return value;

#else // Threaded, but not using reserved keys

  // Register a key and use it
  static swift::tls_key_t key;
  static swift::once_t token;

  swift::tls_alloc_once(token, key,
                        [](void *pointer) { _stdlib_destroyTLS(pointer); });

  void *value = swift::tls_get(key);
  if (!value) {
    value = _stdlib_createTLS();
    swift::tls_set(key, value);
  }
  return value;

#endif
}
