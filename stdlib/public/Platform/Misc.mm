//===--- Misc.mm - Darwin overlay helpers ---------------------------------===//
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

#define _REENTRANT
#include <math.h>

extern "C" int 
_swift_Darwin_open(const char *path, int oflag, mode_t mode) {
  return open(path, oflag, mode);
}

extern "C" int 
_swift_Darwin_openat(int fd, const char *path, int oflag, mode_t mode) {
  return openat(fd, path, oflag, mode);
}

extern "C" sem_t *_swift_Darwin_sem_open2(const char *name, int oflag) {
  return sem_open(name, oflag);
}

extern "C" sem_t *_swift_Darwin_sem_open4(const char *name, int oflag,
                                          mode_t mode, unsigned int value) {
  return sem_open(name, oflag, mode, value);
}

extern "C" int
_swift_Darwin_fcntl(int fd, int cmd, int value) {
  return fcntl(fd, cmd, value);
}

extern "C" int
_swift_Darwin_fcntlPtr(int fd, int cmd, void* ptr) {
  return fcntl(fd, cmd, ptr);
}

extern "C" float
_swift_Darwin_lgammaf_r(float x, int *psigngam) {
  return lgammaf_r(x, psigngam);
}

extern "C" double
_swift_Darwin_lgamma_r(double x, int *psigngam) {
  return lgamma_r(x, psigngam);
}

extern "C" long double
_swift_Darwin_lgammal_r(long double x, int *psigngam) {
  return lgammal_r(x, psigngam);
}
