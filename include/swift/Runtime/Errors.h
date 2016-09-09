//===--- Errors.n - Error reporting utilities -------------------*- C++ -*-===//
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
//
// Utilities for reporting errors to stderr, system console, and crash logs.
//
//===----------------------------------------------------------------------===//

#include "swift/Basic/LLVM.h"

namespace swift {

LLVM_ATTRIBUTE_NORETURN LLVM_ATTRIBUTE_NOINLINE
void swift_abortRetainOverflow();

}
