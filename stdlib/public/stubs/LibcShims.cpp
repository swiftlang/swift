//===--- LibcShims.cpp ----------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include <random>
#include <type_traits>
#include <cmath>
#if defined(_MSC_VER)
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

void swift::_swift_stdlib_free(void *ptr) {
  free(ptr);
}

int swift::_swift_stdlib_putchar_unlocked(int c) {
#if defined(_MSC_VER)
  return _putc_nolock(c, stdout);
#else
  return putchar_unlocked(c);
#endif
}

__swift_size_t swift::_swift_stdlib_fwrite_stdout(const void *ptr,
                                                  __swift_size_t size,
                                                  __swift_size_t nitems) {
  return fwrite(ptr, size, nitems, stdout);
}

__swift_size_t swift::_swift_stdlib_strlen(const char *s) {
  return strlen(s);
}

int swift::_swift_stdlib_memcmp(const void *s1, const void *s2,
                                __swift_size_t n) {
  return memcmp(s1, s2, n);
}

__swift_ssize_t
swift::_swift_stdlib_read(int fd, void *buf, __swift_size_t nbyte) {
#if defined(_MSC_VER)
  return _read(fd, buf, nbyte);
#else
  return read(fd, buf, nbyte);
#endif
}

__swift_ssize_t
swift::_swift_stdlib_write(int fd, const void *buf, __swift_size_t nbyte) {
#if defined(_MSC_VER)
  return _write(fd, buf, nbyte);
#else
  return write(fd, buf, nbyte);
#endif
}

int swift::_swift_stdlib_close(int fd) {
#if defined(_MSC_VER)
  return _close(fd);
#else
  return close(fd);
#endif
}

#if defined(__APPLE__)
#include <malloc/malloc.h>
size_t swift::_swift_stdlib_malloc_size(const void *ptr) {
  return malloc_size(ptr);
}
#elif defined(__GNU_LIBRARY__) || defined(__CYGWIN__) || defined(__ANDROID__)
#include <malloc.h>
size_t swift::_swift_stdlib_malloc_size(const void *ptr) {
  return malloc_usable_size(const_cast<void *>(ptr));
}
#elif defined(_MSC_VER)
#include <malloc.h>
size_t swift::_swift_stdlib_malloc_size(const void *ptr) {
  return _msize(const_cast<void *>(ptr));
}
#elif defined(__FreeBSD__)
#include <malloc_np.h>
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

__swift_uint32_t swift::_swift_stdlib_cxx11_mt19937() {
  return getGlobalMT19937()();
}

__swift_uint32_t
swift::_swift_stdlib_cxx11_mt19937_uniform(__swift_uint32_t upper_bound) {
  if (upper_bound > 0)
    upper_bound--;
  std::uniform_int_distribution<__swift_uint32_t> RandomUniform(0, upper_bound);
  return RandomUniform(getGlobalMT19937());
}

float swift::_swift_stdlib_remainderf(float dividend, float divisor) {
  return std::remainder(dividend, divisor);
}

float swift::_swift_stdlib_squareRootf(float x) { return std::sqrt(x); }

double swift::_swift_stdlib_remainder(double dividend, double divisor) {
  return std::remainder(dividend, divisor);
}

double swift::_swift_stdlib_squareRoot(double x) { return std::sqrt(x); }

#if (defined __i386__ || defined __x86_64__) && !defined _MSC_VER
void swift::_swift_stdlib_remainderl(void *_self, const void *_other) {
  *(long double *)_self = std::remainder(*(long double *)_self,
                                         *(const long double *)_other);
}

void swift::_swift_stdlib_squareRootl(void *_self) {
  *(long double *)_self = std::sqrt(*(long double *)_self);
}
#endif // Have Float80
