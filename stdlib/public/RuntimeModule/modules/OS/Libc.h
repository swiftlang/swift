//===--- Libc.h - Imports from the C library --------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Extra utilities for libc
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_BACKTRACING_LIBC_H
#define SWIFT_BACKTRACING_LIBC_H

#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>
#include <fcntl.h>
#if defined(_WIN32)
#include <io.h>
#endif

// .. Swift affordances ........................................................

#ifdef __cplusplus
namespace swift {
namespace runtime {
namespace backtrace {
#endif

/* open() is usually declared as a variadic function; these don't import into
   Swift. */
static inline int _swift_open(const char *filename, int oflag, int mode) {
#if defined(_WIN32)
  int fh;
  if (_sopen_s(&fh, filename, oflag, _SH_DENYNO, mode))
    return -1;
  return fh;
#else
  return open(filename, oflag, mode);
#endif
}

/* errno is typically not going to be easily accessible (it's often a macro),
   so add a get_errno() function to do that. */
static inline int _swift_get_errno() { return errno; }
static void _swift_set_errno(int err) { errno = err; }

#ifdef __cplusplus
} // namespace backtrace
} // namespace runtime
} // namespace swift
#endif

#endif // SWIFT_BACKTRACING_LIBC_H
