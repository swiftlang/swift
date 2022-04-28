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

#include "swift/Threading/Errors.h"
#include "swift/Threading/Impl/Pthreads.h"

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
pthread_mutex_t      onceMutex = PTHREAD_MUTEX_INITIALIZER;
pthread_cond_t       onceCond = PTHREAD_COND_INITIALIZER;

#pragma clang diagnostic pop

}

using namespace swift;
using namespace threading_impl;

bool
swift::threading_impl::thread_is_main() {
  return pthread_equal(pthread_self(), rememberer.main_thread());
}

void
swift::threading_impl::once_slow(once_t &predicate,
                                 void (*fn)(void *),
                                 void *context) {
  std::int64_t zero = 0;
  if (predicate.compare_exchange_strong(zero, (std::int64_t)1,
                                        std::memory_order_relaxed,
                                        std::memory_order_relaxed)) {
    fn(context);

    predicate.store((std::int64_t)-1, std::memory_order_release);

    pthread_mutex_lock(&onceMutex);
    pthread_mutex_unlock(&onceMutex);
    pthread_cond_broadcast(&onceCond);
    return;
  }

  pthread_mutex_lock(&onceMutex);
  while (predicate.load(std::memory_order_acquire) >= (std::int64_t)0) {
    pthread_cond_wait(&onceCond);
  }
  pthread_mutex_unlock(&onceMutex);
}

#endif // SWIFT_THREADING_PTHREADS
