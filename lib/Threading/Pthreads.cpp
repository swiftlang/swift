//==--- Pthreads.cpp - Threading abstraction implementation ---- -*-C++ -*-===//
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
// Implements threading support for plain pthreads
//
//===----------------------------------------------------------------------===//

#if SWIFT_THREADING_PTHREADS

#if defined(__FreeBSD__) || defined(__OpenBSD__)
#include <pthread_np.h>
#endif

#include "swift/Threading/Impl.h"
#include "swift/Threading/Errors.h"

namespace {

#pragma clang diagnostic push
#pragma GCC diagnostic ignored "-Wglobal-constructors"

class MainThreadRememberer {
private:
  pthread_t mainThread_;

public:
  MainThreadRememberer() { mainThread_ = pthread_self(); }

  pthread_t main_thread() const { return mainThread_; }
};

MainThreadRememberer rememberer;
pthread_mutex_t onceMutex = PTHREAD_MUTEX_INITIALIZER;
pthread_cond_t onceCond = PTHREAD_COND_INITIALIZER;

#pragma clang diagnostic pop

} // namespace

using namespace swift;
using namespace threading_impl;

bool swift::threading_impl::thread_is_main() {
  return pthread_equal(pthread_self(), rememberer.main_thread());
}

void swift::threading_impl::once_slow(once_t &predicate, void (*fn)(void *),
                                      void *context) {
  std::intptr_t zero = 0;
  if (predicate.compare_exchange_strong(zero, (std::intptr_t)1,
                                        std::memory_order_relaxed,
                                        std::memory_order_relaxed)) {
    fn(context);

    predicate.store((std::intptr_t)-1, std::memory_order_release);

    pthread_mutex_lock(&onceMutex);
    pthread_mutex_unlock(&onceMutex);
    pthread_cond_broadcast(&onceCond);
    return;
  }

  pthread_mutex_lock(&onceMutex);
  while (predicate.load(std::memory_order_acquire) >= (std::intptr_t)0) {
    pthread_cond_wait(&onceCond, &onceMutex);
  }
  pthread_mutex_unlock(&onceMutex);
}

#if defined(__OpenBSD__)
std::optional<swift::threading_impl::stack_bounds>
swift::threading_impl::thread_get_current_stack_bounds() {
  stack_t sinfo;

  if (!pthread_stackseg_np(pthread_self(), &sinfo)) {
    stack_bounds result = {
      (char *)sinfo.ss_sp - sinfo.ss_size,
      sinfo.ss_sp
    };
    return result;
  }

  return {};
}
#else
std::optional<swift::threading_impl::stack_bounds>
swift::threading_impl::thread_get_current_stack_bounds() {
  pthread_attr_t attr;
  size_t size = 0;
  void *begin = nullptr;

#if defined(__FreeBSD__)
  if (pthread_attr_init(&attr))
    return {};

  if (pthread_attr_get_np(pthread_self(), &attr)) {
    pthread_attr_destroy(&attr);
    return {};
  }
#elif defined(__linux__)
  if (pthread_getattr_np(pthread_self(), &attr))
    return {};
#else
  // We don't know how to get the thread attr for this platform.
  return {};
#endif

  if (!pthread_attr_getstack(&attr, &begin, &size)) {
    stack_bounds result = { begin, (char *)begin + size };
    pthread_attr_destroy(&attr);
    return result;
  }

  pthread_attr_destroy(&attr);
  return {};
}
#endif

#endif // SWIFT_THREADING_PTHREADS
