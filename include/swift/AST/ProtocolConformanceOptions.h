//===--- ProtocolConformanceOptions.h - Conformance Options -----*- C++ -*-===//
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
// This file defines the options for protocol conformances.
//
//===----------------------------------------------------------------------===//
#ifndef SWIFT_AST_PROTOCOLCONFORMANCEOPTIONS_H
#define SWIFT_AST_PROTOCOLCONFORMANCEOPTIONS_H

#include "swift/Basic/OptionSet.h"

namespace swift {

  /// Flags that describe extra attributes on protocol conformances.
enum class ProtocolConformanceFlags {
  /// @unchecked conformance
  Unchecked = 0x01,

  /// @preconcurrency conformance
  Preconcurrency = 0x02,

  /// @unsafe conformance
  Unsafe = 0x04,

  /// @retroactive conformance
  Retroactive = 0x08,

  /// @isolated conformance
  Isolated = 0x10,

  // Note: whenever you add a bit here, update
  // NumProtocolConformanceOptions below.
};

/// Options that describe extra attributes on protocol conformances.
using ProtocolConformanceOptions =
    OptionSet<ProtocolConformanceFlags>;

inline ProtocolConformanceOptions operator|(
    ProtocolConformanceFlags flag1,
    ProtocolConformanceFlags flag2) {
  return ProtocolConformanceOptions(flag1) | flag2;
}

enum : unsigned {
  NumProtocolConformanceOptions = 5
};

} // end namespace swift

#endif
