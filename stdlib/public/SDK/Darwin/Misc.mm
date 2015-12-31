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

