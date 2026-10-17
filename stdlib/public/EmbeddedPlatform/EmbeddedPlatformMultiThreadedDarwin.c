/*===------ EmbeddedPlatformMultiThreadedDarwin.c ----------------*- C -*-===*
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
 * A Darwin-only implementation of the Embedded Swift platform hooks:
 *
 *   - The non-recursive `_swift_mutex_*` family (with or without the CHECKED
 *     flag) uses `os_unfair_lock` (see <os/lock.h>), stored fully inline in
 *     the caller-owned mutex storage.
 *
 *   - The `_swift_mutexRecursive_*` family uses a `pthread_mutex_t`
 *     initialized with `PTHREAD_MUTEX_RECURSIVE`, since `os_unfair_lock` has
 *     no reentrant mode and building one on top of it would duplicate what
 *     pthreads already provides.
 *
 *   - The `_swift_sem_*` family keeps its count in an atomic and enters the
 *     kernel, through a Mach semaphore, only to block or wake a waiter. A
 *     dispatch semaphore traps when destroyed with its count below its initial
 *     value, which `_swift_sem_destroy` allows.
 *
 *===----------------------------------------------------------------------===*/

#include "swift/EmbeddedPlatform.h"
#include "swift/Runtime/ConcurrencyDebug.h"

#include <mach/mach.h>
#include <os/lock.h>
#include <pthread.h>
#include <stdatomic.h>
#include <stdio.h>
#include <stdint.h>

void _swift_lockStandardOutput(void) {
  flockfile(stdout);
}

void _swift_unlockStandardOutput(void) {
  funlockfile(stdout);
}

#if __has_include(<pthread/tsd_private.h>)
#include <pthread/tsd_private.h>
#else
#define __PTK_FRAMEWORK_SWIFT_KEY0 100
#endif

#define SWIFT_EMBEDDED_PLATFORM_DARWIN_TLS_KEY_BASE __PTK_FRAMEWORK_SWIFT_KEY0

// Darwin stores the current task in a reserved pthread TLS slot.
__attribute__((used, retain))
uint32_t _swift_concurrency_debug_current_task_storage_kind =
    SWIFT_CONCURRENCY_CURRENT_TASK_STORAGE_KIND_PTHREAD_RESERVED_KEY;

extern int pthread_key_init_np(int, void (*)(void *));

// Storage layout for the non-recursive `_swift_mutex_*` family, backed by an
// inline `os_unfair_lock`.
typedef struct {
  uint32_t flags;
  os_unfair_lock unfair;
} swift_darwin_mutex_t;

_Static_assert(sizeof(swift_darwin_mutex_t) <= EMBEDDED_SWIFT_MUTEX_NUM_WORDS * sizeof(void *),
               "swift_darwin_mutex_t does not fit in the Embedded Swift "
               "Platform mutex storage (EMBEDDED_SWIFT_MUTEX_NUM_WORDS "
               "pointer-sized words)");
_Static_assert(_Alignof(swift_darwin_mutex_t) <= _Alignof(void *),
               "swift_darwin_mutex_t requires stronger alignment than the "
               "Embedded Swift Platform mutex storage provides");

// The `_swift_mutexRecursive_*` family is backed directly by a
// `pthread_mutex_t`, constructed in place in the caller-owned storage.
_Static_assert(sizeof(pthread_mutex_t) <= EMBEDDED_SWIFT_MUTEX_RECURSIVE_NUM_WORDS * sizeof(void *),
               "pthread_mutex_t does not fit in the Embedded Swift Platform "
               "recursive mutex storage "
               "(EMBEDDED_SWIFT_MUTEX_RECURSIVE_NUM_WORDS pointer-sized "
               "words)");
_Static_assert(_Alignof(pthread_mutex_t) <= _Alignof(void *),
               "pthread_mutex_t requires stronger alignment than the "
               "Embedded Swift Platform recursive mutex storage provides");

// A negative `count` is minus the number of threads blocked on `port`.
typedef struct {
  _Atomic(__swift_ptrdiff_t) count;
  semaphore_t port;
} swift_darwin_semaphore_t;

_Static_assert(sizeof(swift_darwin_semaphore_t) <= EMBEDDED_SWIFT_SEMAPHORE_NUM_WORDS * sizeof(void *),
               "swift_darwin_semaphore_t does not fit in the Embedded Swift "
               "Platform semaphore storage");
_Static_assert(_Alignof(swift_darwin_semaphore_t) <= _Alignof(void *),
               "swift_darwin_semaphore_t requires stronger alignment than the "
               "Embedded Swift Platform semaphore storage provides");

static void trap_if(int failed) {
  if (failed) {
#if __has_builtin(__builtin_trap)
    __builtin_trap();
#else
    *(volatile int *)0x11 = 0;
#endif
  }
}

static pthread_key_t swift_embedded_platform_tls_key(__swift_tls_key_t key) {
  trap_if(key < 0 || key >= SWIFT_TLS_KEY_COUNT);
  return SWIFT_EMBEDDED_PLATFORM_DARWIN_TLS_KEY_BASE + key;
}

void _swift_mutex_init(void *mutex, swift_mutex_flags_t flags) {
  swift_darwin_mutex_t *m = (swift_darwin_mutex_t *)mutex;
  m->flags = (uint32_t)flags;
  m->unfair = (os_unfair_lock)OS_UNFAIR_LOCK_INIT;
}

void _swift_mutex_destroy(void *mutex) {
  // `os_unfair_lock` has no destroy step.
}

void _swift_mutex_lock(void *mutex) {
  swift_darwin_mutex_t *m = (swift_darwin_mutex_t *)mutex;
  os_unfair_lock_lock(&m->unfair);
}

void _swift_mutex_unlock(void *mutex) {
  swift_darwin_mutex_t *m = (swift_darwin_mutex_t *)mutex;
  if (m->flags & SWIFT_MUTEX_CHECKED) {
    // Aborts if the current thread does not own the lock, or the lock is
    // not held at all.
    os_unfair_lock_assert_owner(&m->unfair);
  }
  os_unfair_lock_unlock(&m->unfair);
}

__swift_ptrdiff_t _swift_mutex_tryLock(void *mutex) {
  swift_darwin_mutex_t *m = (swift_darwin_mutex_t *)mutex;
  return os_unfair_lock_trylock(&m->unfair) ? 1 : 0;
}

void _swift_mutexRecursive_init(void *mutex, swift_mutex_flags_t flags) {
  pthread_mutexattr_t attr;
  trap_if(pthread_mutexattr_init(&attr) != 0);
  trap_if(pthread_mutexattr_settype(&attr, PTHREAD_MUTEX_RECURSIVE) != 0);
  trap_if(pthread_mutex_init((pthread_mutex_t *)mutex, &attr) != 0);
  (void)pthread_mutexattr_destroy(&attr);
}

void _swift_mutexRecursive_destroy(void *mutex) {
  trap_if(pthread_mutex_destroy((pthread_mutex_t *)mutex) != 0);
}

void _swift_mutexRecursive_lock(void *mutex) {
  trap_if(pthread_mutex_lock((pthread_mutex_t *)mutex) != 0);
}

void _swift_mutexRecursive_unlock(void *mutex) {
  trap_if(pthread_mutex_unlock((pthread_mutex_t *)mutex) != 0);
}

void _swift_tls_init(__swift_tls_key_t key, __swift_tls_dtor_t destructor) {
  trap_if(
      pthread_key_init_np((int)swift_embedded_platform_tls_key(key),
                          destructor) != 0);
}

void *_swift_tls_get(__swift_tls_key_t key) {
  return pthread_getspecific(swift_embedded_platform_tls_key(key));
}

void _swift_tls_set(__swift_tls_key_t key, void *value) {
  trap_if(pthread_setspecific(swift_embedded_platform_tls_key(key), value) != 0);
}

__swift_ptrdiff_t _swift_thread_isMain(void) {
  return pthread_main_np() != 0;
}

__swift_ptrdiff_t _swift_thread_create(__swift_thread_t *thread,
                                       __swift_thread_start_t start,
                                       void *arg) {
  pthread_t handle;
  if (pthread_create(&handle, NULL, start, arg) != 0) {
    return 0;
  }
  *thread = handle;
  return 1;
}

void _swift_thread_join(__swift_thread_t thread) {
  trap_if(pthread_join((pthread_t)thread, NULL) != 0);
}

void _swift_sem_init(void *sem, __swift_size_t value) {
  swift_darwin_semaphore_t *s = (swift_darwin_semaphore_t *)sem;
  trap_if(value > PTRDIFF_MAX);
  atomic_init(&s->count, (__swift_ptrdiff_t)value);
  trap_if(semaphore_create(mach_task_self(), &s->port, SYNC_POLICY_FIFO, 0) !=
          KERN_SUCCESS);
}

void _swift_sem_destroy(void *sem) {
  swift_darwin_semaphore_t *s = (swift_darwin_semaphore_t *)sem;
  trap_if(semaphore_destroy(mach_task_self(), s->port) != KERN_SUCCESS);
}

void _swift_sem_wait(void *sem) {
  swift_darwin_semaphore_t *s = (swift_darwin_semaphore_t *)sem;
  if (atomic_fetch_sub_explicit(&s->count, 1, memory_order_acquire) > 0) {
    return;
  }
  kern_return_t result;
  do {
    result = semaphore_wait(s->port);
  } while (result == KERN_ABORTED);
  trap_if(result != KERN_SUCCESS);
}

void _swift_sem_post(void *sem) {
  swift_darwin_semaphore_t *s = (swift_darwin_semaphore_t *)sem;
  __swift_ptrdiff_t previous =
      atomic_fetch_add_explicit(&s->count, 1, memory_order_release);
  trap_if(previous == PTRDIFF_MAX);
  if (previous < 0) {
    trap_if(semaphore_signal(s->port) != KERN_SUCCESS);
  }
}
