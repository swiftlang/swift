//===--- Casting.h - Helpers for casting ------------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_BASIC_CASTING_H
#define SWIFT_BASIC_CASTING_H

#include "STLCompatibility.h"

namespace swift {

/// Cast between two function types. Use in place of std::bit_cast, which
/// doesn't work on ARM64e with address-discriminated signed function types.
template <typename Destination, typename Source>
Destination function_cast(Source source) {
  // Ptrauth attributes decay away here, so we can now bit_cast in peace.
  return std::bit_cast<Destination>(source);
}

}

#endif
