//===--- Failure.cpp - Swift Language Runtime Failure Traps -----*- C++ -*-===//
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
//
// Swift runtime entry points for triggering runtime failures.
//
//===----------------------------------------------------------------------===//

#include "swift/Runtime/Failure.h"

using namespace swift;

void swift::swift_conditionalFailure(bool condition) {
  if (condition)
    __builtin_trap();
}
