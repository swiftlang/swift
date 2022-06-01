//==--- Errors.h - Threading implementation error handling ----- -*-C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Declares some support routines for error handling in the threading code
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_THREADING_ERRORS_H
#define SWIFT_THREADING_ERRORS_H

#include <stdarg.h>

#include "swift/Basic/Compiler.h"

namespace swift {
namespace threading {

// Users of the threading library are expected to provide this function.
SWIFT_ATTRIBUTE_NORETURN
SWIFT_FORMAT(1, 2)
void fatal(const char *msg, ...);

} // namespace threading
} // namespace swift

#endif // SWIFT_THREADING_ERRORS_H
