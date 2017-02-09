//===--- Portability.cpp - ------------------------------------------------===//
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
//
// Implementations of the stub APIs that make portable runtime easier to write.
//
//===----------------------------------------------------------------------===//

#include "swift/Runtime/Portability.h"
#include <cstring>

size_t _swift_strlcpy(char *dst, const char *src, size_t maxlen) {
  const size_t srclen = std::strlen(src);
  if (srclen < maxlen) {
    std::memmove(dst, src, srclen + 1);
  } else if (maxlen != 0) {
    std::memmove(dst, src, maxlen - 1);
    dst[maxlen - 1] = '\0';
  }
  return srclen;
}
