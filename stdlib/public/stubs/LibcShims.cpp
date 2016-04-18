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
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "../SwiftShims/LibcShims.h"

static_assert(std::is_same<ssize_t, swift::__swift_ssize_t>::value,
              "__swift_ssize_t must be defined as equivalent to ssize_t");

namespace swift {

void _swift_stdlib_free(void *ptr) { free(ptr); }

int _swift_stdlib_putchar_unlocked(int c) { return putchar_unlocked(c); }

__swift_size_t _swift_stdlib_fwrite_stdout(const void *ptr, __swift_size_t size,
                                           __swift_size_t nitems) {
  return fwrite(ptr, size, nitems, stdout);
}

__swift_size_t _swift_stdlib_strlen(const char *s) { return strlen(s); }

int _swift_stdlib_memcmp(const void *s1, const void *s2, __swift_size_t n) {
  return memcmp(s1, s2, n);
}

__swift_ssize_t _swift_stdlib_read(int fd, void *buf, __swift_size_t nbyte) {
  return read(fd, buf, nbyte);
}

__swift_ssize_t _swift_stdlib_write(int fd, const void *buf,
                                    __swift_size_t nbyte) {
  return write(fd, buf, nbyte);
}

int _swift_stdlib_close(int fd) { return close(fd); }

#if defined(__APPLE__)
#include <malloc/malloc.h>
size_t _swift_stdlib_malloc_size(const void *ptr) { return malloc_size(ptr); }
#elif defined(__GNU_LIBRARY__) || defined(__CYGWIN__)
#include <malloc.h>
size_t _swift_stdlib_malloc_size(const void *ptr) {
  return malloc_usable_size(const_cast<void *>(ptr));
}
#elif defined(__FreeBSD__)
#include <malloc_np.h>
size_t _swift_stdlib_malloc_size(const void *ptr) {
  return malloc_usable_size(const_cast<void *>(ptr));
}
#elif defined(__ANDROID__)
extern "C" {
extern size_t dlmalloc_usable_size(void*);
}
size_t _swift_stdlib_malloc_size(const void *ptr) {
  return dlmalloc_usable_size(const_cast<void *>(ptr));
}
#else
#error No malloc_size analog known for this platform/libc.
#endif

static std::mt19937 &getGlobalMT19937() {
  static std::mt19937 MersenneRandom;
  return MersenneRandom;
}

__swift_uint32_t _swift_stdlib_cxx11_mt19937() {
  return getGlobalMT19937()();
}

__swift_uint32_t
_swift_stdlib_cxx11_mt19937_uniform(__swift_uint32_t upper_bound) {
  if (upper_bound > 0)
    upper_bound--;
  std::uniform_int_distribution<__swift_uint32_t> RandomUniform(0, upper_bound);
  return RandomUniform(getGlobalMT19937());
}

} // namespace swift

