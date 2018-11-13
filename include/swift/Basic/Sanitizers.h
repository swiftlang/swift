//===--- Sanitizers.h - Helpers related to sanitizers -----------*- C++ -*-===//
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

#ifndef SWIFT_BASIC_SANITIZERS_H
#define SWIFT_BASIC_SANITIZERS_H

namespace swift {

// Enabling bitwise masking.
enum class SanitizerKind : unsigned {
  Address = 1 << 1,
  Thread = 1 << 2,
  Fuzzer = 1 << 3,
  Undefined = 1 << 4
};

} // end namespace swift

#endif // SWIFT_BASIC_SANITIZERS_H
