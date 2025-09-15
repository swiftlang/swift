//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#pragma once

#include <stdbool.h>
#include <sys/wait.h>

static inline
bool wIfStopped(int status) {
  return WIFSTOPPED(status) != 0;
}

static inline
bool wIfExited(int status) {
  return WIFEXITED(status) != 0;
}

static inline
bool wIfSignaled(int status) {
  return WIFSIGNALED(status) != 0;
}

static inline
int wStopSig(int status) {
  return WSTOPSIG(status);
}
