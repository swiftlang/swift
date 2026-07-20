/*===------ EmbeddedPlatformMultiThreadedPOSIX.c -----------------*- C -*-===*
 *
 * This source file is part of the Swift.org open source project
 *
 * Copyright (c) 2026 Apple Inc. and the Swift project authors
 * Licensed under Apache License v2.0 with Runtime Library Exception
 *
 * See https://swift.org/LICENSE.txt for license information
 * See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
 *
 *===----------------------------------------------------------------------===*
 *
 * A multi-threaded implementation of the Embedded Swift platform mutex hooks
 * on top of `pthread_mutex_t`. The `pthread_mutex_t` is constructed directly
 * in the eight pointer-sized words of caller-owned storage that the platform
 * layer provides — 64 bytes on 64-bit targets, which fits every currently
 * supported `pthread_mutex_t` (40 bytes on glibc LP64, 56 bytes on Darwin
 * LP64).
 *
 *===----------------------------------------------------------------------===*/


#include "swift/EmbeddedPlatform.h"

#include <pthread.h>

static pthread_t swift_embedded_platform_main_thread;
static pthread_key_t swift_embedded_platform_tls_keys[SWIFT_TLS_KEY_COUNT];
static unsigned char
    swift_embedded_platform_tls_key_initialized[SWIFT_TLS_KEY_COUNT];
static pthread_mutex_t swift_embedded_platform_tls_lock =
    PTHREAD_MUTEX_INITIALIZER;

#if __STDC_VERSION__ >= 201112L
_Static_assert(sizeof(pthread_mutex_t) <= EMBEDDED_SWIFT_MUTEX_NUM_WORDS * sizeof(void *),
               "pthread_mutex_t does not fit in the Embedded Swift Platform "
               "mutex storage (8 pointer-sized words)");
_Static_assert(_Alignof(pthread_mutex_t) <= _Alignof(void *),
               "pthread_mutex_t requires stronger alignment than the Embedded "
               "Swift Platform mutex storage provides");
#endif

static void trap_if(int failed) {
  if (failed) {
#if __has_builtin(__builtin_trap)
    __builtin_trap();
#else
    *(volatile int*)0x11 = 0;
#endif
  }
}

__attribute__((constructor))
static void swift_embedded_platform_remember_main_thread(void) {
  swift_embedded_platform_main_thread = pthread_self();
}

static pthread_key_t swift_embedded_platform_tls_key(swift_tls_key_t key) {
  trap_if(key < 0 || key >= SWIFT_TLS_KEY_COUNT);
  return swift_embedded_platform_tls_keys[key];
}

static int swift_embedded_platform_tls_is_initialized(swift_tls_key_t key) {
  trap_if(key < 0 || key >= SWIFT_TLS_KEY_COUNT);
  return swift_embedded_platform_tls_key_initialized[key] != 0;
}

static void
swift_embedded_platform_tls_init_if_needed(swift_tls_key_t key,
                                           __swift_tls_dtor_t destructor) {
  trap_if(key < 0 || key >= SWIFT_TLS_KEY_COUNT);
  trap_if(pthread_mutex_lock(&swift_embedded_platform_tls_lock) != 0);
  if (!swift_embedded_platform_tls_key_initialized[key]) {
    trap_if(pthread_key_create(&swift_embedded_platform_tls_keys[key],
                               destructor) != 0);
    swift_embedded_platform_tls_key_initialized[key] = 1;
  }
  trap_if(pthread_mutex_unlock(&swift_embedded_platform_tls_lock) != 0);
}

void _swift_mutex_init(void *mutex, swift_mutex_flags_t flags) {
  int type;
  if (flags & SWIFT_MUTEX_RECURSIVE) {
    type = PTHREAD_MUTEX_RECURSIVE;
  } else if (flags & SWIFT_MUTEX_CHECKED) {
    type = PTHREAD_MUTEX_ERRORCHECK;
  } else {
    type = PTHREAD_MUTEX_NORMAL;
  }

  if (type == PTHREAD_MUTEX_NORMAL) {
    trap_if(pthread_mutex_init((pthread_mutex_t *)mutex, NULL) != 0);
    return;
  }

  pthread_mutexattr_t attr;
  trap_if(pthread_mutexattr_init(&attr) != 0);
  trap_if(pthread_mutexattr_settype(&attr, type) != 0);
  trap_if(pthread_mutex_init((pthread_mutex_t *)mutex, &attr) != 0);
  (void)pthread_mutexattr_destroy(&attr);
}

void _swift_mutex_destroy(void *mutex) {
  trap_if(pthread_mutex_destroy((pthread_mutex_t *)mutex) != 0);
}

void _swift_mutex_lock(void *mutex) {
  trap_if(pthread_mutex_lock((pthread_mutex_t *)mutex) != 0);
}

void _swift_mutex_unlock(void *mutex) {
  trap_if(pthread_mutex_unlock((pthread_mutex_t *)mutex) != 0);
}

__swift_ptrdiff_t _swift_mutex_tryLock(void *mutex) {
  return pthread_mutex_trylock((pthread_mutex_t *)mutex) == 0 ? 1 : 0;
}

void _swift_tls_init(swift_tls_key_t key, __swift_tls_dtor_t destructor) {
  swift_embedded_platform_tls_init_if_needed(key, destructor);
}

void *_swift_tls_get(swift_tls_key_t key) {
  if (!swift_embedded_platform_tls_is_initialized(key)) {
    return NULL;
  }
  return pthread_getspecific(swift_embedded_platform_tls_key(key));
}

void _swift_tls_set(swift_tls_key_t key, void *value) {
  swift_embedded_platform_tls_init_if_needed(key, NULL);
  trap_if(pthread_setspecific(swift_embedded_platform_tls_key(key), value) != 0);
}

__swift_ptrdiff_t _swift_thread_isMain(void) {
  return pthread_equal(pthread_self(), swift_embedded_platform_main_thread) != 0;
}
