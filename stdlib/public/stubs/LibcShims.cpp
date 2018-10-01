//===--- LibcShims.cpp ----------------------------------------------------===//
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

#if defined(__APPLE__)
#define _REENTRANT
#include <math.h>
#endif

#if defined(_WIN32) && !defined(__CYGWIN__)
#include <io.h>
#define WIN32_LEAN_AND_MEAN
#include <Windows.h>
#else
#include <semaphore.h>
#include <unistd.h>
#endif

#include <algorithm>
#include <cmath>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <type_traits>

#include "llvm/Support/DataTypes.h"
#include "swift/Basic/Lazy.h"
#include "swift/Runtime/Config.h"
#include "swift/Runtime/Debug.h"
#include "../SwiftShims/LibcShims.h"

using namespace swift;

static_assert(std::is_same<ssize_t, swift::__swift_ssize_t>::value,
              "__swift_ssize_t must be defined as equivalent to ssize_t in LibcShims.h");
#if !defined(_WIN32) || defined(__CYGWIN__)
static_assert(std::is_same<mode_t, swift::__swift_mode_t>::value,
              "__swift_mode_t must be defined as equivalent to mode_t in LibcShims.h");
#endif

SWIFT_RUNTIME_STDLIB_SPI
void swift::_swift_stdlib_free(void *ptr) {
  free(ptr);
}

SWIFT_RUNTIME_STDLIB_SPI
int swift::_swift_stdlib_putchar_unlocked(int c) {
#if defined(_WIN32)
  return _putc_nolock(c, stdout);
#else
  return putchar_unlocked(c);
#endif
}

SWIFT_RUNTIME_STDLIB_SPI
__swift_size_t swift::_swift_stdlib_fwrite_stdout(const void *ptr,
                                                  __swift_size_t size,
                                                  __swift_size_t nitems) {
    return fwrite(ptr, size, nitems, stdout);
}

SWIFT_RUNTIME_STDLIB_SPI
__swift_size_t swift::_swift_stdlib_strlen(const char *s) {
    return strlen(s);
}

SWIFT_RUNTIME_STDLIB_SPI
__swift_size_t swift::_swift_stdlib_strlen_unsigned(const unsigned char *s) {
  return strlen(reinterpret_cast<const char *>(s));
}

SWIFT_RUNTIME_STDLIB_SPI
int swift::_swift_stdlib_memcmp(const void *s1, const void *s2,
                                __swift_size_t n) {
  return memcmp(s1, s2, n);
}

#if !defined(_WIN32) || defined(__CYGWIN__)

SWIFT_RUNTIME_STDLIB_SPI
void *swift::_stdlib_sem_open2(const char *name, int oflag) {
  return sem_open(name, oflag);
}

SWIFT_RUNTIME_STDLIB_SPI
void *swift::_stdlib_sem_open4(const char *name, int oflag,
                               __swift_mode_t mode, unsigned int value) {
  return sem_open(name, oflag, mode, value);
}

#endif // !(defined(_WIN32) && !defined(__CYGWIN__))

#if defined(__APPLE__)
#include <malloc/malloc.h>
SWIFT_RUNTIME_STDLIB_SPI
size_t swift::_swift_stdlib_malloc_size(const void *ptr) {
  return malloc_size(ptr);
}
#elif defined(__GNU_LIBRARY__) || defined(__CYGWIN__) || defined(__ANDROID__) || defined(__HAIKU__)
#if defined(__HAIKU__)
#define _GNU_SOURCE
#endif
#include <malloc.h>
SWIFT_RUNTIME_STDLIB_SPI
size_t swift::_swift_stdlib_malloc_size(const void *ptr) {
  return malloc_usable_size(const_cast<void *>(ptr));
}
#elif defined(_WIN32)
#include <malloc.h>
SWIFT_RUNTIME_STDLIB_SPI
size_t swift::_swift_stdlib_malloc_size(const void *ptr) {
  return _msize(const_cast<void *>(ptr));
}
#elif defined(__FreeBSD__)
#include <malloc_np.h>
SWIFT_RUNTIME_STDLIB_SPI
size_t swift::_swift_stdlib_malloc_size(const void *ptr) {
  return malloc_usable_size(const_cast<void *>(ptr));
}
#else
#error No malloc_size analog known for this platform/libc.
#endif
