//===--- Misc.c - Platform overlay helpers --------------------------------===//
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

#include <fcntl.h>
#include <semaphore.h>
#include <sys/types.h>
#include <sys/stat.h>

#include "swift/Runtime/Config.h"

SWIFT_CC(swift)
extern int _swift_Platform_open(const char *path, int oflag, mode_t mode) {
  return open(path, oflag, mode);
}

SWIFT_CC(swift)
extern int _swift_Platform_openat(int fd, const char *path, int oflag,
                                  mode_t mode) {
  return openat(fd, path, oflag, mode);
}

SWIFT_CC(swift)
extern sem_t *_swift_Platform_sem_open2(const char *name, int oflag) {
  return sem_open(name, oflag);
}

SWIFT_CC(swift)
extern sem_t *_swift_Platform_sem_open4(const char *name, int oflag,
                                        mode_t mode, unsigned int value) {
  return sem_open(name, oflag, mode, value);
}

SWIFT_CC(swift)
extern int _swift_Platform_fcntl(int fd, int cmd, int value) {
  return fcntl(fd, cmd, value);
}

SWIFT_CC(swift)
extern int _swift_Platform_fcntlPtr(int fd, int cmd, void* ptr) {
  return fcntl(fd, cmd, ptr);
}

#if defined(__APPLE__)
#define _REENTRANT
#include <math.h>

SWIFT_CC(swift)
extern float _swift_Darwin_lgammaf_r(float x, int *psigngam) {
  return lgammaf_r(x, psigngam);
}

SWIFT_CC(swift)
extern double _swift_Darwin_lgamma_r(double x, int *psigngam) {
  return lgamma_r(x, psigngam);
}

SWIFT_CC(swift)
extern long double _swift_Darwin_lgammal_r(long double x, int *psigngam) {
  return lgammal_r(x, psigngam);
}
#endif // defined(__APPLE__)

#if defined(__FreeBSD__)
SWIFT_CC(swift) extern char **_swift_FreeBSD_getEnv() {
  extern char **environ;
  return environ;
}
#endif // defined(__FreeBSD__)
