//===--- StringComparison.h ----------------------------------*- C++ -*----===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_RUNTIME_STRINGCOMPARISON_H
#define SWIFT_RUNTIME_STRINGCOMPARISON_H

#include "swift/Runtime/Config.h"
#include <cstdint>

SWIFT_RUNTIME_EXPORT
int swift_stdlib_findDiffIdx_UInt8UInt16(uint8_t *left, uint16_t *result,
                                         int count);

#endif
