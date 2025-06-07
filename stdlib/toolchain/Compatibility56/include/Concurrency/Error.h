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

#ifndef SWIFT_CONCURRENCY_ERROR_BACKDEPLOY56_H
#define SWIFT_CONCURRENCY_ERROR_BACKDEPLOY56_H

#include "public/SwiftShims/swift/shims/Visibility.h"
#include <cstdint>
#include <stdlib.h>

namespace swift {

__attribute__((visibility("hidden")))
SWIFT_NORETURN void swift_Concurrency_fatalError(uint32_t flags, const char *format, ...);

} // namespace swift

#endif // SWIFT_CONCURRENCY_ERROR_BACKDEPLOY56_H
