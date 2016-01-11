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

#include <type_traits>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "../SwiftShims/LibcShims.h"

#if defined(__linux__)
#include <bsd/stdlib.h>
#endif

static_assert(std::is_same<ssize_t, swift::__swift_ssize_t>::value,
              "__swift_ssize_t is wrong");

namespace swift {

void _swift_stdlib_free(void *ptr) { free(ptr); }

int _swift_stdlib_putchar(int c) { return putchar(c); }

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
#elif defined(__GNU_LIBRARY__)
#include <malloc.h>
size_t _swift_stdlib_malloc_size(const void *ptr) {
  return malloc_usable_size(const_cast<void *>(ptr));
}
#elif defined(__FreeBSD__)
#include <malloc_np.h>
size_t _swift_stdlib_malloc_size(const void *ptr) {
  return malloc_usable_size(const_cast<void *>(ptr));
}
#else
#error No malloc_size analog known for this platform/libc.
#endif

__swift_uint32_t _swift_stdlib_arc4random(void) { return arc4random(); }

__swift_uint32_t
_swift_stdlib_arc4random_uniform(__swift_uint32_t upper_bound) {
  return arc4random_uniform(upper_bound);
}

} // namespace swift

