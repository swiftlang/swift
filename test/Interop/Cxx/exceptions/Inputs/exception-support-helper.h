//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "libcxxexceptionshim.h"

#include <cassert>
#include <cstdlib>
#include <cstring>
#include <new>
#include <stdexcept>
#include <string>
#include <sys/mman.h>
#include <sys/wait.h>
#include <unistd.h>
#include <unwind.h>

#if defined(__linux__)
#include <pthread.h>
#endif

#if defined(__OBJC__)
#include <Foundation/Foundation.h>
#endif

static int liveExceptions = 0;
static int destroyedLocals = 0;

struct TrackedException : std::exception {
  char message[32] = "exception-owned message";

  TrackedException() { ++liveExceptions; }
  TrackedException(const TrackedException &other) : std::exception(other) {
    ++liveExceptions;
    std::memcpy(message, other.message, sizeof(message));
  }
  ~TrackedException() noexcept override {
    --liveExceptions;
    std::memset(message, '!', sizeof(message));
  }
  const char *what() const noexcept override { return message; }
};

struct TrackedLocal {
  ~TrackedLocal() { ++destroyedLocals; }
};

static void copyMessage(void *context, const char *message) {
  *static_cast<std::string *>(context) = message;
}

static void copyTrackedMessage(void *context, const char *message) {
  assert(liveExceptions > 0);
  assert(destroyedLocals == 1);
  copyMessage(context, message);
}

static void testMessages() {
  std::string message;
  try {
    throw std::runtime_error("runtime error");
  } catch (...) {
    __swift_cxx_report_current_exception(&message, copyMessage);
  }
  assert(message == "runtime error");

  auto exception = std::make_exception_ptr(std::runtime_error("rethrow"));
  try {
    std::rethrow_exception(exception);
  } catch (...) {
    __swift_cxx_report_current_exception(&message, copyMessage);
  }
  assert(message == "rethrow");

  try {
    throw 42;
  } catch (...) {
    __swift_cxx_report_current_exception(&message, copyMessage);
  }
  assert(message == "Unknown C++ exception");

  for (int i = 0; i != 100; ++i) {
    destroyedLocals = 0;
    try {
      TrackedLocal local;
      throw TrackedException();
    } catch (...) {
      __swift_cxx_report_current_exception(&message, copyTrackedMessage);
    }
    assert(liveExceptions == 0);
    assert(message == "exception-owned message");
  }
}

// A foreign exception must terminate, without invoking the message callback.
static void forbiddenCallback(void *, const char *) { std::_Exit(43); }

static void expectTermination(void (*body)()) {
  pid_t child = fork();
  assert(child >= 0);
  if (child == 0) {
    std::set_terminate([] { std::_Exit(42); });
    body();
    std::_Exit(44);
  }
  int status;
  assert(waitpid(child, &status, 0) == child);
  assert(WIFEXITED(status));
  assert(WEXITSTATUS(status) == 42);
}

static void raiseForeignException() {
  _Unwind_Exception exception = {};
  exception.exception_class = 0x5357494654544553ULL;
  exception.exception_cleanup = [](_Unwind_Reason_Code, _Unwind_Exception *) {};
  try {
    _Unwind_RaiseException(&exception);
    std::_Exit(45);
  } catch (...) {
    __swift_cxx_report_current_exception(nullptr, forbiddenCallback);
  }
}

// Put the unwind header immediately after inaccessible memory. A foreign
// exception has no C++ fields before this header, even if a runtime represents
// it internally using a pointer to a fabricated __cxa_exception header.
static void raiseGuardedForeignException() {
  long pageSize = sysconf(_SC_PAGESIZE);
  assert(pageSize > 0);
  void *pages = mmap(nullptr, 2 * pageSize, PROT_NONE,
                     MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  assert(pages != MAP_FAILED);
  void *storage = static_cast<char *>(pages) + pageSize;
  assert(mprotect(storage, pageSize, PROT_READ | PROT_WRITE) == 0);
  auto *exception = new (storage) _Unwind_Exception{};
  exception->exception_class = 0x5357494654544553ULL;
  exception->exception_cleanup = [](_Unwind_Reason_Code, _Unwind_Exception *) {};
  try {
    _Unwind_RaiseException(exception);
    std::_Exit(45);
  } catch (...) {
    __swift_cxx_report_current_exception(nullptr, forbiddenCallback);
  }
}

static void reportWithoutException() {
  __swift_cxx_report_current_exception(nullptr, forbiddenCallback);
}

static void raiseForeignExceptionInsideNativeHandler() {
  try {
    throw 42;
  } catch (...) {
    raiseForeignException();
  }
}

#if defined(__OBJC__)
static void raiseObjectiveCException() {
  try {
    @throw [NSException exceptionWithName:@"TestException"
                                   reason:@"must not become a Swift Error"
                                 userInfo:nil];
  } catch (...) {
    __swift_cxx_report_current_exception(nullptr, forbiddenCallback);
  }
}
#endif

#if defined(__linux__)
static void exitThread() {
  try {
    pthread_exit(nullptr);
  } catch (...) {
    __swift_cxx_report_current_exception(nullptr, forbiddenCallback);
  }
}

static void cancelThread() {
  assert(pthread_cancel(pthread_self()) == 0);
  try {
    pthread_testcancel();
  } catch (...) {
    __swift_cxx_report_current_exception(nullptr, forbiddenCallback);
  }
}
#endif

int main() {
  testMessages();
  expectTermination(raiseForeignException);
  expectTermination(raiseGuardedForeignException);
  expectTermination(raiseForeignExceptionInsideNativeHandler);
  expectTermination(reportWithoutException);
#if defined(__OBJC__)
  expectTermination(raiseObjectiveCException);
#endif
#if defined(__linux__)
  expectTermination(exitThread);
  expectTermination(cancelThread);
#endif
}
