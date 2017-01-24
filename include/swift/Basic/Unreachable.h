//===--- Unreachable.h - Implements swift_unreachable -----------*- C++ -*-===//
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
//  This file defines swift_unreachable, an LLVM-independent implementation of
//  llvm_unreachable.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_BASIC_UNREACHABLE_H
#define SWIFT_BASIC_UNREACHABLE_H

#include "llvm/Support/Compiler.h"
#include <assert.h>
#include <stdlib.h>

LLVM_ATTRIBUTE_NORETURN
inline static void swift_unreachable(const char* msg) {
  assert(false && msg);
  (void)msg;
  abort();
}

#endif // SWIFT_BASIC_UNREACHABLE_H
