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
 * A Darwin-only implementation of the Embedded Swift platform mutex hooks:
 *
 *   - The common non-recursive case (with or without the CHECKED flag) uses
 *     `os_unfair_lock` (see <os/lock.h>), stored fully inline in the
 *     caller-owned mutex storage.
 *
 *   - The RECURSIVE case uses a heap-allocated `pthread_mutex_t` initialized
 *     with `PTHREAD_MUTEX_RECURSIVE`, since `os_unfair_lock` has no reentrant
 *     mode and building one on top of it would duplicate what pthreads
 *     already provides.
 *
 *===----------------------------------------------------------------------===*/

#include "swift/EmbeddedPlatform.h"

#include <os/lock.h>
#include <pthread.h>
#include <stdint.h>

// Storage layout that we impose on the caller-owned mutex buffer. `flags` is
// used as the discriminator between the two backends: if it has
// `SWIFT_MUTEX_RECURSIVE` set, `u.heap` is a heap-allocated
// `pthread_mutex_t`; otherwise `u.unfair` is an inline `os_unfair_lock`.
typedef struct {
  uint32_t flags;
  union {
    os_unfair_lock unfair;
    pthread_mutex_t *heap;
  } u;
} swift_darwin_mutex_t;

_Static_assert(sizeof(swift_darwin_mutex_t) <= 8 * sizeof(void *),
               "swift_darwin_mutex_t does not fit in the Embedded Swift "
               "Platform mutex storage (8 pointer-sized words)");
_Static_assert(_Alignof(swift_darwin_mutex_t) <= _Alignof(void *),
               "swift_darwin_mutex_t requires stronger alignment than the "
               "Embedded Swift Platform mutex storage provides");

static void trap_if(int failed) {
  if (failed) {
#if __has_builtin(__builtin_trap)
    __builtin_trap();
#else
    *(volatile int *)0x11 = 0;
#endif
  }
}

void _swift_mutex_init(void *mutex, swift_mutex_flags_t flags) {
  swift_darwin_mutex_t *m = (swift_darwin_mutex_t *)mutex;
  m->flags = (uint32_t)flags;

  if (flags & SWIFT_MUTEX_RECURSIVE) {
    pthread_mutex_t *heap = (pthread_mutex_t *)_swift_allocate(
        _Alignof(pthread_mutex_t), sizeof(pthread_mutex_t), SWIFT_ALLOC_NONE);
    trap_if(heap == NULL);

    pthread_mutexattr_t attr;
    trap_if(pthread_mutexattr_init(&attr) != 0);
    trap_if(pthread_mutexattr_settype(&attr, PTHREAD_MUTEX_RECURSIVE) != 0);
    trap_if(pthread_mutex_init(heap, &attr) != 0);
    (void)pthread_mutexattr_destroy(&attr);

    m->u.heap = heap;
    return;
  }

  m->u.unfair = (os_unfair_lock)OS_UNFAIR_LOCK_INIT;
}

void _swift_mutex_destroy(void *mutex) {
  swift_darwin_mutex_t *m = (swift_darwin_mutex_t *)mutex;
  if (m->flags & SWIFT_MUTEX_RECURSIVE) {
    trap_if(pthread_mutex_destroy(m->u.heap) != 0);
    _swift_deallocate(m->u.heap, _Alignof(pthread_mutex_t),
                      sizeof(pthread_mutex_t), SWIFT_FREE_NONE);
    m->u.heap = NULL;
  }
  // Non-recursive: `os_unfair_lock` has no destroy step.
}

void _swift_mutex_lock(void *mutex) {
  swift_darwin_mutex_t *m = (swift_darwin_mutex_t *)mutex;
  if (m->flags & SWIFT_MUTEX_RECURSIVE) {
    trap_if(pthread_mutex_lock(m->u.heap) != 0);
    return;
  }
  os_unfair_lock_lock(&m->u.unfair);
}

void _swift_mutex_unlock(void *mutex) {
  swift_darwin_mutex_t *m = (swift_darwin_mutex_t *)mutex;
  if (m->flags & SWIFT_MUTEX_RECURSIVE) {
    trap_if(pthread_mutex_unlock(m->u.heap) != 0);
    return;
  }
  if (m->flags & SWIFT_MUTEX_CHECKED) {
    // Aborts if the current thread does not own the lock, or the lock is
    // not held at all.
    os_unfair_lock_assert_owner(&m->u.unfair);
  }
  os_unfair_lock_unlock(&m->u.unfair);
}

__swift_ptrdiff_t _swift_mutex_tryLock(void *mutex) {
  swift_darwin_mutex_t *m = (swift_darwin_mutex_t *)mutex;
  if (m->flags & SWIFT_MUTEX_RECURSIVE) {
    return pthread_mutex_trylock(m->u.heap) == 0 ? 1 : 0;
  }
  return os_unfair_lock_trylock(&m->u.unfair) ? 1 : 0;
}
