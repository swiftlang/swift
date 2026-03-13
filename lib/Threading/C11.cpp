//==--- C11.cpp - Threading abstraction implementation --------- -*-C++ -*-===//
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
// Implements threading support for C11 threads
//
//===----------------------------------------------------------------------===//

#if SWIFT_THREADING_C11

#include "swift/Threading/Impl.h"
#include "swift/Threading/Errors.h"

namespace {

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wglobal-constructors"

class C11ThreadingHelper {
private:
  thrd_t mainThread_;
  mtx_t onceMutex_;
  cnd_t onceCond_;

public:
  C11ThreadingHelper() {
    mainThread_ = thrd_current();
    SWIFT_C11THREADS_CHECK(::mtx_init(&onceMutex_, ::mtx_plain));
    SWIFT_C11THREADS_CHECK(::cnd_init(&onceCond_));
  }

  thrd_t main_thread() const { return mainThread_; }

  void once_lock() { SWIFT_C11THREADS_CHECK(mtx_lock(&onceMutex_)); }
  void once_unlock() { SWIFT_C11THREADS_CHECK(mtx_unlock(&onceMutex_)); }
  void once_broadcast() { SWIFT_C11THREADS_CHECK(cnd_broadcast(&onceCond_)); }
  void once_wait() {
    // The mutex must be locked when this function is entered.  It will
    // be locked again before the function returns.
    SWIFT_C11THREADS_CHECK(cnd_wait(&onceCond_, &onceMutex_));
  }
};

C11ThreadingHelper helper;

#pragma clang diagnostic pop

} // namespace

using namespace swift;
using namespace threading_impl;

bool swift::threading_impl::thread_is_main() {
  return thrd_equal(thrd_current(), helper.main_thread());
}

void swift::threading_impl::once_slow(once_t &predicate, void (*fn)(void *),
                                      void *context) {
  std::intptr_t zero = 0;
  if (predicate.compare_exchange_strong(zero, (std::intptr_t)1,
                                        std::memory_order_relaxed,
                                        std::memory_order_relaxed)) {
    fn(context);

    predicate.store((std::intptr_t)-1, std::memory_order_release);

    helper.once_lock();
    helper.once_unlock();
    helper.once_broadcast();
    return;
  }

  helper.once_lock();
  while (predicate.load(std::memory_order_acquire) >= (std::intptr_t)0) {
    helper.once_wait();
  }
  helper.once_unlock();
}

#endif // SWIFT_THREADING_C11
