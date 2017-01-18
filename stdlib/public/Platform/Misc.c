//===--- Misc.c - Platform overlay helpers --------------------------------===//
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

#include <errno.h>
#include <fcntl.h>
#if !defined(_WIN32) || defined(__CYGWIN__)
#include <semaphore.h>
#endif
#if defined(_WIN32) && !defined(__CYGWIN__)
#include <io.h>
#endif
#include <sys/types.h>
#include <sys/stat.h>
#if !defined(_WIN32) || defined(__CYGWIN__)
#include <sys/ioctl.h>
#endif

#include "swift/Runtime/Config.h"

#if !defined(_WIN32) || defined(__CYGWIN__)
SWIFT_CC(swift)
int _swift_Platform_open(const char *path, int oflag, mode_t mode) {
  return open(path, oflag, mode);
}
#else
SWIFT_CC(swift)
int _swift_Platform_open(const char *path, int oflag, int mode) {
  return _open(path, oflag, mode);
}
#endif

#if !defined(_WIN32) || defined(__CYGWIN__)
SWIFT_CC(swift)
int _swift_Platform_openat(int fd, const char *path, int oflag,
                                  mode_t mode) {
  return openat(fd, path, oflag, mode);
}

SWIFT_CC(swift)
sem_t *_swift_Platform_sem_open2(const char *name, int oflag) {
  return sem_open(name, oflag);
}

SWIFT_CC(swift)
sem_t *_swift_Platform_sem_open4(const char *name, int oflag,
                                        mode_t mode, unsigned int value) {
  return sem_open(name, oflag, mode, value);
}

SWIFT_CC(swift)
int _swift_Platform_fcntl(int fd, int cmd, int value) {
  return fcntl(fd, cmd, value);
}

SWIFT_CC(swift)
int _swift_Platform_fcntlPtr(int fd, int cmd, void* ptr) {
  return fcntl(fd, cmd, ptr);
}

SWIFT_CC(swift)
int
_swift_Platform_ioctl(int fd, unsigned long int request, int value) {
  return ioctl(fd, request, value);
}

SWIFT_CC(swift)
int
_swift_Platform_ioctlPtr(int fd, unsigned long int request, void* ptr) {
  return ioctl(fd, request, ptr);
}
#endif

#if defined(__APPLE__)
#define _REENTRANT
#include <math.h>

SWIFT_CC(swift)
float _swift_Darwin_lgammaf_r(float x, int *psigngam) {
  return lgammaf_r(x, psigngam);
}

SWIFT_CC(swift)
double _swift_Darwin_lgamma_r(double x, int *psigngam) {
  return lgamma_r(x, psigngam);
}

SWIFT_CC(swift)
long double _swift_Darwin_lgammal_r(long double x, int *psigngam) {
  return lgammal_r(x, psigngam);
}
#endif // defined(__APPLE__)

#if defined(__FreeBSD__)
SWIFT_CC(swift)
char **_swift_FreeBSD_getEnv() {
  extern char **environ;
  return environ;
}
#endif // defined(__FreeBSD__)

SWIFT_CC(swift)
int _swift_Platform_getErrno() {
  return errno;
}

SWIFT_CC(swift)
void _swift_Platform_setErrno(int value) {
  errno = value;
}

