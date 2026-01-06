//==--- Win32.h - Threading abstraction implementation --------- -*-C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Implements threading support for Windows threads
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_THREADING_IMPL_WIN32_H
#define SWIFT_THREADING_IMPL_WIN32_H

#include "Win32/Win32Defs.h"

#include "chrono_utils.h"

#include <atomic>

#include <optional>

namespace swift {
namespace threading_impl {

// .. Thread related things ..................................................

using thread_id = ::DWORD;

inline thread_id thread_get_current() { return ::GetCurrentThreadId(); }
bool thread_is_main();
inline bool threads_same(thread_id a, thread_id b) { return a == b; }
std::optional<stack_bounds> thread_get_current_stack_bounds();

// .. Mutex support ..........................................................

using mutex_handle = SWIFT_SRWLOCK;

inline void mutex_init(mutex_handle &handle, bool checked = false) {
  handle = SRWLOCK_INIT;
}
inline void mutex_destroy(mutex_handle &handle) {}

inline void mutex_lock(mutex_handle &handle) {
  AcquireSRWLockExclusive(&handle);
}
inline void mutex_unlock(mutex_handle &handle) {
  ReleaseSRWLockExclusive(&handle);
}
inline bool mutex_try_lock(mutex_handle &handle) {
  return !!TryAcquireSRWLockExclusive(&handle);
}

inline void mutex_unsafe_lock(mutex_handle &handle) {
  AcquireSRWLockExclusive(&handle);
}
inline void mutex_unsafe_unlock(mutex_handle &handle) {
  ReleaseSRWLockExclusive(&handle);
}

using lazy_mutex_handle = SWIFT_SRWLOCK;

// We don't need to be lazy here because Win32 has SRWLOCK_INIT.
#define SWIFT_LAZY_MUTEX_INITIALIZER SRWLOCK_INIT

inline void lazy_mutex_destroy(lazy_mutex_handle &handle) {}

inline void lazy_mutex_lock(lazy_mutex_handle &handle) {
  AcquireSRWLockExclusive(&handle);
}
inline void lazy_mutex_unlock(lazy_mutex_handle &handle) {
  ReleaseSRWLockExclusive(&handle);
}
inline bool lazy_mutex_try_lock(lazy_mutex_handle &handle) {
  return !!TryAcquireSRWLockExclusive(&handle);
}

inline void lazy_mutex_unsafe_lock(lazy_mutex_handle &handle) {
  AcquireSRWLockExclusive(&handle);
}
inline void lazy_mutex_unsafe_unlock(lazy_mutex_handle &handle) {
  ReleaseSRWLockExclusive(&handle);
}

// .. Recursive mutex support ................................................

using recursive_mutex_handle = SWIFT_CRITICAL_SECTION;

inline void recursive_mutex_init(recursive_mutex_handle &handle,
                                 bool checked = false) {
  InitializeCriticalSection(&handle);
}

inline void recursive_mutex_destroy(recursive_mutex_handle &handle) {
  DeleteCriticalSection(&handle);
}

inline void recursive_mutex_lock(recursive_mutex_handle &handle) {
  EnterCriticalSection(&handle);
}
inline void recursive_mutex_unlock(recursive_mutex_handle &handle) {
  LeaveCriticalSection(&handle);
}

// .. ConditionVariable support ..............................................

struct cond_handle {
  SWIFT_CONDITION_VARIABLE condition;
  SWIFT_SRWLOCK lock;
};

inline void cond_init(cond_handle &handle) {
  handle.condition = CONDITION_VARIABLE_INIT;
  handle.lock = SRWLOCK_INIT;
}
inline void cond_destroy(cond_handle &handle) {}
inline void cond_lock(cond_handle &handle) {
  AcquireSRWLockExclusive(&handle.lock);
}
inline void cond_unlock(cond_handle &handle) {
  ReleaseSRWLockExclusive(&handle.lock);
}
inline void cond_signal(cond_handle &handle) {
  WakeConditionVariable(&handle.condition);
}
inline void cond_broadcast(cond_handle &handle) {
  WakeAllConditionVariable(&handle.condition);
}
inline void cond_wait(cond_handle &handle) {
  SleepConditionVariableSRW(&handle.condition,
                            &handle.lock,
                            INFINITE,
                            0);
}
template <class Rep, class Period>
inline bool cond_wait(cond_handle &handle,
                      std::chrono::duration<Rep, Period> duration) {
  auto ms = chrono_utils::ceil<std::chrono::milliseconds>(duration);

  /* If you are paying attention to the next line, you are now asking

       "Why are you adding 16 - that seems mad?"

     To explain, we need to understand how the Sleep...() APIs in
     Windows work.

     The Windows kernel runs a timer, the system tick, which is
     used for scheduling; the timer in question, for historical
     reasons, by default runs at 64Hz.  Every time the timer fires,
     Windows updates the tick count, schedules processes and so on.

     When you ask to sleep, there are two cases:

     1. You've asked to sleep for less than a tick, or

     2. You've asked to sleep for at least a tick.

     In case 1, the sleep functions appear to run a delay loop; this
     is by its very nature inaccurate and you may or may not wait less
     than the time you requested.  You might also get rescheduled,
     in which case you'll wait at least a whole tick.

     In case 2, Windows appears to be adding the requested wait to
     its current tick count to work out when to wake your thread.
     That *sounds* sensible, until you realise that if it does this
     towards the end of a tick period, you will wait for that much
     less time.  i.e. the sleep functions will return *early* by
     up to one tick period.

     This is especially unfortunate if you ask to wait for exactly
     one tick period, because you will end up waiting for anything
     between no time at all and a full tick period.

     To complicate matters, the system tick rate might not be 64Hz;
     the multimedia timer API can cause it to change to as high as
     1kHz, and on *some* machines this happens globally for all
     processes, while on *other* machines the kernel is actually using
     a separate system tick rate and merely pretending to Win32
     processes that things still work this way.

     On other platforms, these kinds of functions are guaranteed to
     wait for at least the time you requested, and we'd like for that
     to be true for the Threading package's APIs here.  One way to
     achieve that is to add a whole tick's worth of milliseconds
     to the time we're requesting to wait for.  In that case, we'll
     avoid the delay loop from case (1), and we'll also guarantee that
     we wait for at least the amount of time the caller of this
     function expected.

     It would be nice if there were a Windows API we could call to
     obtain the current system tick period.  The Internet suggests
     that the undocumented call NtQueryTimerResolution() might be of
     some use for this, but it turns out that that just gives you
     the kernel's idea of the system tick period, which isn't the
     same as Win32's idea necessarily.  e.g. on my test system, I
     can see that the tick period is 15.625ms, while that call tells
     me 1ms.

     We don't want to change the system tick rate using the multimedia
     timer API mentioned earlier, because on some machines that is
     global and will use extra power and CPU cycles.

     The upshot is that the best choice here appears to be to add
     15.625ms (1/64s) to the time we're asking to wait.  That rounds
     up to 16ms, which is why there's a +16. */
  return SleepConditionVariableSRW(&handle.condition,
                                   &handle.lock,
                                   DWORD(ms.count()) + 16,
                                   0);
}
inline bool cond_wait(cond_handle &handle,
                      std::chrono::system_clock::time_point deadline) {
  std::chrono::system_clock::duration duration =
    deadline - std::chrono::system_clock::now();
  if (duration < std::chrono::system_clock::duration::zero())
    duration = std::chrono::system_clock::duration::zero();
  return cond_wait(handle, duration);
}

// .. Once ...................................................................

typedef std::atomic<intptr_t> once_t;

void once_slow(once_t &predicate, void (*fn)(void *), void *context);

inline void once_impl(once_t &predicate, void (*fn)(void *), void *context) {
  // Using INIT_ONCE is slower than doing this.
  if (predicate.load(std::memory_order_acquire) < 0)
    return;

  once_slow(predicate, fn, context);
}

// .. Thread local storage ...................................................

#ifdef __clang__
#if __has_feature(cxx_thread_local)
#define SWIFT_THREAD_LOCAL thread_local
#endif
#elif __cplusplus >= 201103L
#define SWIFT_THREAD_LOCAL thread_local
#endif

#define SWIFT_TLS_DECLARE_DTOR(name) void NTAPI name(void *)

using tls_key_t = ::DWORD;
using tls_dtor_t = ::PFLS_CALLBACK_FUNCTION;

inline bool tls_alloc(tls_key_t &key, tls_dtor_t dtor) {
  key = ::FlsAlloc(dtor);
  return key != FLS_OUT_OF_INDEXES;
}

inline void *tls_get(tls_key_t key) { return ::FlsGetValue(key); }

inline void tls_set(tls_key_t key, void *value) { ::FlsSetValue(key, value); }

} // namespace threading_impl

} // namespace swift

#endif // SWIFT_THREADING_IMPL_WIN32_H
