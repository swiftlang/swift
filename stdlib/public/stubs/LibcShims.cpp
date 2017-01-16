//===--- LibcShims.cpp ----------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include <random>
#include <type_traits>
#include <cmath>
#if defined(_WIN32)
#include <io.h>
#else
#include <unistd.h>
#endif
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "swift/Basic/Lazy.h"
#include "../SwiftShims/LibcShims.h"
#include "llvm/Support/DataTypes.h"

using namespace swift;

static_assert(std::is_same<ssize_t, swift::__swift_ssize_t>::value,
              "__swift_ssize_t must be defined as equivalent to ssize_t");

SWIFT_RUNTIME_STDLIB_INTERFACE
void swift::_swift_stdlib_free(void *ptr) {
  free(ptr);
}

SWIFT_RUNTIME_STDLIB_INTERFACE
int swift::_swift_stdlib_putchar_unlocked(int c) {
#if defined(_WIN32)
  return _putc_nolock(c, stdout);
#else
  return putchar_unlocked(c);
#endif
}

SWIFT_RUNTIME_STDLIB_INTERFACE
__swift_size_t swift::_swift_stdlib_fwrite_stdout(const void *ptr,
                                                  __swift_size_t size,
                                                  __swift_size_t nitems) {
  return fwrite(ptr, size, nitems, stdout);
}

SWIFT_RUNTIME_STDLIB_INTERFACE
__swift_size_t swift::_swift_stdlib_strlen(const char *s) {
  return strlen(s);
}

SWIFT_RUNTIME_STDLIB_INTERFACE
__swift_size_t swift::_swift_stdlib_strlen_unsigned(const unsigned char *s) {
  return strlen((char *)s);
}

SWIFT_RUNTIME_STDLIB_INTERFACE
int swift::_swift_stdlib_memcmp(const void *s1, const void *s2,
                                __swift_size_t n) {
  return memcmp(s1, s2, n);
}

SWIFT_RUNTIME_STDLIB_INTERFACE
__swift_ssize_t
swift::_swift_stdlib_read(int fd, void *buf, __swift_size_t nbyte) {
#if defined(_WIN32)
  return _read(fd, buf, nbyte);
#else
  return read(fd, buf, nbyte);
#endif
}

SWIFT_RUNTIME_STDLIB_INTERFACE
__swift_ssize_t
swift::_swift_stdlib_write(int fd, const void *buf, __swift_size_t nbyte) {
#if defined(_WIN32)
  return _write(fd, buf, nbyte);
#else
  return write(fd, buf, nbyte);
#endif
}

SWIFT_RUNTIME_STDLIB_INTERFACE
int swift::_swift_stdlib_close(int fd) {
#if defined(_WIN32)
  return _close(fd);
#else
  return close(fd);
#endif
}

#if defined(__APPLE__)
#include <malloc/malloc.h>
SWIFT_RUNTIME_STDLIB_INTERFACE
size_t swift::_swift_stdlib_malloc_size(const void *ptr) {
  return malloc_size(ptr);
}
#elif defined(__GNU_LIBRARY__) || defined(__CYGWIN__) || defined(__ANDROID__)
#include <malloc.h>
SWIFT_RUNTIME_STDLIB_INTERFACE
size_t swift::_swift_stdlib_malloc_size(const void *ptr) {
  return malloc_usable_size(const_cast<void *>(ptr));
}
#elif defined(_WIN32)
#include <malloc.h>
SWIFT_RUNTIME_STDLIB_INTERFACE
size_t swift::_swift_stdlib_malloc_size(const void *ptr) {
  return _msize(const_cast<void *>(ptr));
}
#elif defined(__FreeBSD__)
#include <malloc_np.h>
SWIFT_RUNTIME_STDLIB_INTERFACE
size_t swift::_swift_stdlib_malloc_size(const void *ptr) {
  return malloc_usable_size(const_cast<void *>(ptr));
}
#else
#error No malloc_size analog known for this platform/libc.
#endif

static Lazy<std::mt19937> theGlobalMT19937;

static std::mt19937 &getGlobalMT19937() {
  return theGlobalMT19937.get();
}

SWIFT_RUNTIME_STDLIB_INTERFACE
__swift_uint32_t swift::_swift_stdlib_cxx11_mt19937() {
  return getGlobalMT19937()();
}

SWIFT_RUNTIME_STDLIB_INTERFACE
__swift_uint32_t
swift::_swift_stdlib_cxx11_mt19937_uniform(__swift_uint32_t upper_bound) {
  if (upper_bound > 0)
    upper_bound--;
  std::uniform_int_distribution<__swift_uint32_t> RandomUniform(0, upper_bound);
  return RandomUniform(getGlobalMT19937());
}
