//===-------- OptimizationMode.h - Swift optimization modes -----*- C++ -*-===//
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

#ifndef SWIFT_BASIC_OPTIMIZATIONMODE_H
#define SWIFT_BASIC_OPTIMIZATIONMODE_H

#include "swift/Basic/InlineBitfield.h"
#include "llvm/Support/DataTypes.h"

namespace swift {

// The optimization mode specified on the command line or with function
// attributes.
enum class OptimizationMode : uint8_t {
  NotSet = 0,
  NoOptimization = 1,  // -Onone
  ForSpeed = 2,        // -Ospeed == -O
  ForSize = 3,         // -Osize
  LastMode = ForSize
};

enum : unsigned { NumOptimizationModeBits =
  countBitsUsed(static_cast<unsigned>(OptimizationMode::LastMode)) };

} // end namespace swift

#endif // SWIFT_BASIC_OPTIMIZATIONMODE_H
