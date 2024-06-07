//===---- AccessibleFunction.h - Runtime accessible functions ---*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// The runtime interface for functions accessible by name.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_RUNTIME_ACCESSIBLE_FUNCTION_H
#define SWIFT_RUNTIME_ACCESSIBLE_FUNCTION_H

#include "swift/ABI/Metadata.h"

#include <cstdint>

namespace swift {
namespace runtime {

SWIFT_RUNTIME_STDLIB_SPI
const AccessibleFunctionRecord *
swift_findAccessibleFunction(const char *targetNameStart,
                             size_t targetNameLength);

} // end namespace runtime
} // end namespace swift

#endif
