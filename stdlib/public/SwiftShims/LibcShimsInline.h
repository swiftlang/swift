//===--- LibcShims.h - Static inline shims for POSIX functions. --*- C++ -*-===//
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
//
//  Provide small wrappers for POSIX functionality that can't be used
//  directly from swift due to varargs or other issues.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_STDLIB_SHIMS_LIBCSHIMSINLINE_H
#define SWIFT_STDLIB_SHIMS_LIBCSHIMSINLINE_H

#if defined(_WIN32) && !defined(__CYGWIN__)
#include <io.h>
#define WIN32_LEAN_AND_MEAN
#include <Windows.h>
typedef int mode_t;
#else
#include <sys/ioctl.h>
#include <sys/types.h>
#include <unistd.h>
#endif

#include <errno.h>
#include <fcntl.h>

// File control <fcntl.h>
#if !defined(_WIN32) || defined(__CYGWIN__)
static inline int _swift_stdlib_fcntl(int fd, int cmd, int value) {
  return fcntl(fd, cmd, value);
}

static inline int _swift_stdlib_fcntlPtr(int fd, int cmd, void* ptr) {
  return fcntl(fd, cmd, ptr);
}
#endif

// Environment
#if defined(__FreeBSD__)
static inline char * _Nullable * _Null_unspecified _swift_stdlib_getEnviron() {
  extern char **environ;
  return environ;
}
#elif defined(__APPLE__)
static inline char * _Nullable *_swift_stdlib_getEnviron() {
  extern char * _Nullable **_NSGetEnviron(void);
  return *_NSGetEnviron();
}
#endif

// System error numbers <errno.h>
static inline int _swift_stdlib_getErrno() {
  return errno;
}

static inline void _swift_stdlib_setErrno(int value) {
  errno = value;
}

// I/O control <ioctl.h>
#if !defined(_WIN32) || defined(__CYGWIN__)
static inline int _swift_stdlib_ioctl(int fd, unsigned long int request, int value) {
  return ioctl(fd, request, value);
}

static inline int _swift_stdlib_ioctlPtr(int fd, unsigned long int request, void* ptr) {
  return ioctl(fd, request, ptr);
}
#endif

#if defined(_WIN32) && !defined(__CYGWIN__)
// Windows
static inline int _swift_stdlib_open(const char *path, int oflag, mode_t mode) {
  return _open(path, oflag, static_cast<int>(mode));
}

#else
// not Windows
int static inline _swift_stdlib_open(const char *path, int oflag, mode_t mode) {
  return open(path, oflag, mode);
}

int static inline _swift_stdlib_openat(int fd, const char *path, int oflag,
                                       mode_t mode) {
  return openat(fd, path, oflag, mode);
}
#endif

#endif // SWIFT_STDLIB_SHIMS_LIBCSHIMSINLINE_H
