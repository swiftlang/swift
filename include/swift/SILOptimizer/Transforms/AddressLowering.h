//===------------------- AddressLowering.h --------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//


#ifndef SWIFT_SILOPTIMIZER_ADDRESSLOWERING_H
#define SWIFT_SILOPTIMIZER_ADDRESSLOWERING_H

#include "swift/SILOptimizer/PassManager/Transforms.h"

namespace swift {
  void lowerAddress(SILPassManager *pm, SILFunction *function);
} // namespace swift

#endif // SWIFT_SILOPTIMIZER_ADDRESSLOWERING_H
