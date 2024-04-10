//===--- Require.h ----------------------------------------------*- C++ -*-===//
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
//
//  This file defines swift_unreachable, which provides the
//  functionality of llvm_unreachable without necessarily depending on
//  the LLVM support libraries.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_BASIC_REQUIRE_H
#define SWIFT_BASIC_REQUIRE_H

#include "llvm/Support/raw_ostream.h"

namespace swift {

/// Checks the `condition` and if it's false abort with `message`.
/// In contrast to `assert` and similar functions, `require` works in
/// no-assert builds the same way as in debug builds.
inline void require(bool condition, const char *message) {
  if (!condition) {
    llvm::errs() << message << '\n';
    abort();
  }
}

}

#endif
