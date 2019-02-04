//===--- LibcShims.cpp ----------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
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
#endif

#include <errno.h>
#include <stdio.h>
#include <sys/types.h>
#if defined(__unix__) || (defined(__APPLE__) && defined(__MACH__))
#include <unistd.h>
#endif

#include <type_traits>

#include "../SwiftShims/LibcShims.h"

using namespace swift;

#if !defined(_WIN32) || defined(__CYGWIN__)
static_assert(std::is_same<mode_t, swift::__swift_mode_t>::value,
              "__swift_mode_t must be defined as equivalent to mode_t in LibcShims.h");
#endif

SWIFT_RUNTIME_STDLIB_INTERNAL
void swift::_swift_stdlib_flockfile_stdin() {
#if defined(_WIN32) && !defined(__CYGWIN__)
  _lock_file(stdin);
#else
  flockfile(stdin);
#endif
}

SWIFT_RUNTIME_STDLIB_INTERNAL
void swift::_swift_stdlib_funlockfile_stdin() {
#if defined(_WIN32) && !defined(__CYGWIN__)
  _unlock_file(stdin);
#else
  funlockfile(stdin);
#endif
}

SWIFT_RUNTIME_STDLIB_INTERNAL
int swift::_swift_stdlib_getc_unlocked_stdin() {
  int result = EOF;
  do {
#if defined(_WIN32) && !defined(__CYGWIN__)
    result = _fgetc_nolock(stdin);
#else
    result = getc_unlocked(stdin);
#endif
  } while (result == EOF && ferror(stdin) && errno == EINTR);
  return result;
}

SWIFT_RUNTIME_STDLIB_INTERNAL
int swift::_swift_stdlib_ungetc_unlocked_stdin(int c) {
  int result = EOF;
  do {
#if defined(_WIN32) && !defined(__CYGWIN__)
    result = _ungetc_nolock(c, stdin);
#else
    result = ungetc(c, stdin);
#endif
  } while (result == EOF && ferror(stdin) && errno == EINTR);
  return result;
}

SWIFT_RUNTIME_STDLIB_INTERNAL
__swift_size_t swift::_swift_stdlib_fwrite_stdout(const void *ptr,
                                                  __swift_size_t size,
                                                  __swift_size_t nitems) {
    return fwrite(ptr, size, nitems, stdout);
}

SWIFT_RUNTIME_STDLIB_SPI
__swift_ssize_t
swift::_swift_stdlib_read(int fd, void *buf, __swift_size_t nbyte) {
#if defined(_WIN32)
  return _read(fd, buf, nbyte);
#else
  return read(fd, buf, nbyte);
#endif
}

SWIFT_RUNTIME_STDLIB_SPI
__swift_ssize_t
swift::_swift_stdlib_write(int fd, const void *buf, __swift_size_t nbyte) {
#if defined(_WIN32)
  return _write(fd, buf, nbyte);
#else
  return write(fd, buf, nbyte);
#endif
}

SWIFT_RUNTIME_STDLIB_SPI
int swift::_swift_stdlib_close(int fd) {
#if defined(_WIN32)
  return _close(fd);
#else
  return close(fd);
#endif
}
