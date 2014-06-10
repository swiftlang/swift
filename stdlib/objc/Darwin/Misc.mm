//===--- Misc.mm - Darwin overlay helpers ---------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include <fcntl.h>

extern "C" int 
_swift_Darwin_open(const char *path, int oflag, mode_t mode) {
  return open(path, oflag, mode);
}

extern "C" int 
_swift_Darwin_openat(int fd, const char *path, int oflag, mode_t mode) {
  return openat(fd, path, oflag, mode);
}
