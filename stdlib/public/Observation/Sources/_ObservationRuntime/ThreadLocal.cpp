//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/Runtime/Config.h"
#include "swift/Runtime/Once.h"

#if __has_include(<pthread/tsd_private.h>)
#include <pthread/tsd_private.h>

#define SWIFT_TLS_PTHREAD_DIRECT 1
#define SWIFT_TLS_PTHREAD 0
#define SWIFT_TLS_FLS 0

#elif __has_include(<pthread.h>)
#include <pthread.h>

#define SWIFT_TLS_PTHREAD_DIRECT 0
#define SWIFT_TLS_PTHREAD 1
#define SWIFT_TLS_FLS 0

#elif defined(_WIN32)
#define WIN32_LEAN_AND_MEAN
#define NOMINMAX
#include <Windows.h>

#define SWIFT_TLS_PTHREAD_DIRECT 0
#define SWIFT_TLS_PTHREAD 0
#define SWIFT_TLS_FLS 1

#endif

using namespace swift;

extern "C" SWIFT_CC(swift)
uint64_t _swift_observation_tracking_key() {
#if SWIFT_TLS_PTHREAD_DIRECT
  return __PTK_FRAMEWORK_FOUNDATION_KEY6;
#elif SWIFT_TLS_PTHREAD
  static pthread_key_t key;
  static swift::once_t onceToken;
  swift::once(onceToken, [] {
    pthread_key_create(&key, NULL);
  });
  return (uint64_t)key;
#elif SWIFT_TLS_FLS
  static DWORD key;
  static swift::once_t onceToken;
  swift::once(onceToken, [] {
    key = FlsAlloc(NULL);
  });
  return (uint64_t)key;
#endif
}

extern "C" SWIFT_CC(swift)
uint64_t _swift_observation_transaction_key() {
#if SWIFT_TLS_PTHREAD_DIRECT
  return __PTK_FRAMEWORK_FOUNDATION_KEY7;
#elif SWIFT_TLS_PTHREAD
  static pthread_key_t key;
  static swift::once_t onceToken;
  swift::once(onceToken, [] {
    pthread_key_create(&key, NULL);
  });
  return (uint64_t)key;
#elif SWIFT_TLS_FLS
  static DWORD key;
  static swift::once_t onceToken;
  swift::once(onceToken, [] {
    key = FlsAlloc(NULL);
  });
  return (uint64_t)key;
#endif
}

extern "C" SWIFT_CC(swift)
void *_swift_observation_tls_get(uint64_t key) {
#if SWIFT_TLS_PTHREAD_DIRECT
  return _pthread_getspecific_direct((unsigned long)key);
#elif SWIFT_TLS_PTHREAD
  return pthread_get_specific((pthread_key_t)key);
#elif SWIFT_TLS_FLS
  return FlsGetValue((DWORD)key);
#endif
}

extern "C" SWIFT_CC(swift)
_swift_observation_tls_set(uint64_t key, void *value) {
#if SWIFT_TLS_PTHREAD_DIRECT
  return _pthread_setspecific_direct((unsigned long)key, value);
#elif SWIFT_TLS_PTHREAD
  return pthread_set_specific((pthread_key_t)key, value);
#elif SWIFT_TLS_FLS
  return FlsSetValue((DWORD)key, value);
#endif
}
