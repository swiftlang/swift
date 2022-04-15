//==--- Linux.cpp - Threading abstraction implementation ------- -*-C++ -*-===//
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
// Implements threading support for Linux
//
//===----------------------------------------------------------------------===//

#if SWIFT_THREADING_LINUX

#include "swift/Threading/Errors.h"
#include "swift/Threading/Impl/Linux.h"

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

#pragma clang diagnostic pop

}

using namespace swift;
using namespace threading_impl;

thread_id
swift::threading_impl::thread_get_main() {
  return rememberer.main_thread();
}

bool
swift::threading_impl::thread_is_main() {
  return pthread_equal(pthread_self(), thread_get_main());
}

void
swift::threading_impl::once_slow(once_t &predicate,
                                 void (*fn)(void *),
                                 void *context) {
  linux::ulock_lock(&predicate.lock);
  if (predicate.flag.load(std::memory_order_acquire) == 0) {
    fn(context);
    predicate.flag.store(-1, std::memory_order_release);
  }
  linux::ulock_unlock(&predicate.lock);
}

#endif // SWIFT_THREADING_PTHREADS
