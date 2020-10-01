//===--- LibcOverlayShims.h - Static inline shims for POSIX functions. --*- C++ -*-===//
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
//  directly from Swift due to varargs or other issues.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_STDLIB_SHIMS_LIBCOVERLAYSHIMS_H
#define SWIFT_STDLIB_SHIMS_LIBCOVERLAYSHIMS_H

#include "Visibility.h"

#if defined(__OpenBSD__)
#include <stdio.h>
#endif
#if defined(_WIN32) && !defined(__CYGWIN__)
#include <errno.h>
#include <io.h>
typedef int mode_t;
#else
#include <semaphore.h>
#include <sys/ioctl.h>
#include <sys/types.h>
#endif

#include <errno.h>
#include <fcntl.h>

#if __has_feature(nullability)
#pragma clang assume_nonnull begin
#endif

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
#if defined(__FreeBSD__) || defined(__OpenBSD__)
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

// Semaphores <semaphore.h>
#if !defined(_WIN32) || defined(__CYGWIN__)
static inline sem_t *_stdlib_sem_open2(const char *name, int oflag) {
  return sem_open(name, oflag);
}

static inline sem_t *_stdlib_sem_open4(const char *name, int oflag,
                                       mode_t mode, unsigned int value) {
  return sem_open(name, oflag, mode, value);
}

#endif // !(defined(_WIN32) && !defined(__CYGWIN__))

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
  return _open(path, oflag, (int)mode);
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

#if defined(__OpenBSD__)
static inline FILE *_swift_stdlib_stdin(void) {
  return stdin;
}

static inline FILE *_swift_stdlib_stdout(void) {
  return stdout;
}

static inline FILE *_swift_stdlib_stderr(void) {
  return stderr;
}
#endif

#if __has_feature(nullability)
#pragma clang assume_nonnull end
#endif

#endif // SWIFT_STDLIB_SHIMS_LIBCOVERLAYSHIMS_H
