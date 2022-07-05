//===--- Error.h - Swift Concurrency error helpers --------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Error handling support.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_CONCURRENCY_ERRORS_H
#define SWIFT_CONCURRENCY_ERRORS_H

#include "swift/Basic/Compiler.h"

#include "swift/Basic/Visibility.h"
#include <cstdarg>
#include <cstdint>
#include <cstdlib>

namespace swift {

SWIFT_NORETURN SWIFT_FORMAT(2, 3) void swift_Concurrency_fatalError(
    uint32_t flags, const char *format, ...);
SWIFT_NORETURN SWIFT_VFORMAT(2) void swift_Concurrency_fatalErrorv(
    uint32_t flags, const char *format, va_list val);

} // namespace swift

#endif
