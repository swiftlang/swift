//===--- Random.cpp -------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

// swift_stdlib_random
//
// Should the implementation of this function add a new platform/change for a
// platform, make sure to also update the documentation regarding platform
// implementation of this function.
// This can be found at: /docs/Random.md

#if defined(_WIN32) && !defined(__CYGWIN__)
#define WIN32_LEAN_AND_MEAN
#include <Windows.h>
#include <Bcrypt.h>
#pragma comment(lib, "bcrypt.lib")
#else
#include <errno.h>
#include <fcntl.h>
#include <unistd.h>
#endif

#if __has_include(<sys/random.h>)
#include <sys/random.h>
#endif
#include <sys/stat.h>
#if __has_include(<sys/syscall.h>)
#include <sys/syscall.h>
#endif

#include <stdlib.h>

#include "swift/Runtime/Debug.h"
#include "swift/Runtime/Mutex.h"
#include "../SwiftShims/Random.h"

#if defined(__APPLE__)

SWIFT_RUNTIME_STDLIB_API
void swift::swift_stdlib_random(void *buf, __swift_size_t nbytes) {
  arc4random_buf(buf, nbytes);
}

#elif defined(_WIN32) && !defined(__CYGWIN__)
#warning TODO: Test swift_stdlib_random on Windows

SWIFT_RUNTIME_STDLIB_API
void swift::swift_stdlib_random(void *buf, __swift_size_t nbytes) {
  if (nbytes > ULONG_MAX) {
    fatalError(0, "Fatal error: %zd exceeds ULONG_MAX\n", nbytes);
  }

  NTSTATUS status = BCryptGenRandom(nullptr,
                                    static_cast<PUCHAR>(buf),
                                    static_cast<ULONG>(nbytes),
                                    BCRYPT_USE_SYSTEM_PREFERRED_RNG);
  if (!BCRYPT_SUCCESS(status)) {
    fatalError(0, "Fatal error: 0x%.8X in '%s'\n", status, __func__);
  }
}

#else

#undef  WHILE_EINTR
#define WHILE_EINTR(expression) ([&] () -> decltype(expression) {              \
  decltype(expression) result = -1;                                            \
  do { result = (expression); } while (result == -1 && errno == EINTR);        \
  return result;                                                               \
}())

SWIFT_RUNTIME_STDLIB_API
void swift::swift_stdlib_random(void *buf, __swift_size_t nbytes) {
  while (nbytes > 0) {
    __swift_ssize_t actual_nbytes = -1;

#if defined(__NR_getrandom)
    static const bool getrandom_available =
      !(syscall(__NR_getrandom, nullptr, 0, 0) == -1 && errno == ENOSYS);
  
    if (getrandom_available) {
      actual_nbytes = WHILE_EINTR(syscall(__NR_getrandom, buf, nbytes, 0));
    }
#elif __has_include(<sys/random.h>) && (defined(__CYGWIN__) || defined(__Fuchsia__))
    __swift_size_t getentropy_nbytes = std::min(nbytes, __swift_size_t{256});
    
    if (0 == getentropy(buf, getentropy_nbytes)) {
      actual_nbytes = getentropy_nbytes;
    }
#endif

    if (actual_nbytes == -1) {
      static const int fd = 
        WHILE_EINTR(open("/dev/urandom", O_RDONLY | O_CLOEXEC, 0));
        
      if (fd != -1) {
        static StaticMutex mutex;
        mutex.withLock([&] {
          actual_nbytes = WHILE_EINTR(read(fd, buf, nbytes));
        });
      }
    }
    
    if (actual_nbytes == -1) {
      fatalError(0, "Fatal error: %d in '%s'\n", errno, __func__);
    }
    
    buf = static_cast<uint8_t *>(buf) + actual_nbytes;
    nbytes -= actual_nbytes;
  }
}

#endif

