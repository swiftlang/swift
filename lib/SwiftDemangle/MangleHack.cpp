//===--- MangleHack.cpp - Swift Mangle Hack for various clients -----------===//
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
// Implementations of mangler hacks for Interface Builder
//
// We don't have the time to disentangle the real mangler from the compiler
// right now.
//
//===----------------------------------------------------------------------===//

#include "swift/SwiftDemangle/MangleHack.h"
#include "swift/Strings.h"
#include <cassert>
#include <cstdarg>
#include <cstdio>
#include <cstdlib>
#include <cstring>

#if defined(_WIN32)
static int vasprintf(char **strp, const char *fmt, va_list ap) {
  int len = _vscprintf(fmt, ap);
  if (len < 0)
    return len;
  char *buffer = static_cast<char *>(malloc(len + 1));
  if (buffer == nullptr)
    return -1;
  int result = vsprintf(buffer, fmt, ap);
  if (result < 0) {
    free(buffer);
    return -1;
  }
  *strp = buffer;
  return result;
}

static int asprintf(char **strp, const char *fmt, ...) {
  va_list args;
  va_start(args, fmt);
  int result = vasprintf(strp, fmt, args);
  va_end(args);
  return result;
}
#endif

const char *
_swift_mangleSimpleClass(const char *module, const char *class_) {
  size_t moduleLength = strlen(module);
  size_t classLength = strlen(class_);
  char *value = nullptr;
  if (swift::STDLIB_NAME == llvm::StringRef(module)) {
    int result = asprintf(&value, "_TtCs%zu%s", classLength, class_);
    assert(result > 0);
    (void)result;
  } else {
    int result = asprintf(&value, "_TtC%zu%s%zu%s", moduleLength, module,
                          classLength, class_);
    assert(result > 0);
    (void)result;
  }
  assert(value);
  return value;
}

const char *
_swift_mangleSimpleProtocol(const char *module, const char *protocol) {
  size_t moduleLength = strlen(module);
  size_t protocolLength = strlen(protocol);
  char *value = nullptr;
  if (swift::STDLIB_NAME == llvm::StringRef(module)) {
    int result = asprintf(&value, "_TtPs%zu%s_", protocolLength, protocol);
    assert(result > 0);
    (void)result;
  } else {
    int result = asprintf(&value, "_TtP%zu%s%zu%s_", moduleLength, module,
                          protocolLength, protocol);
    assert(result > 0);
    (void)result;
  }
  assert(value);
  return value;
}
