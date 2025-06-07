//===--- ManglingFlavor.h - Swift name mangling -----------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_DEMANGLING_MANGLINGFLAVOR_H
#define SWIFT_DEMANGLING_MANGLINGFLAVOR_H

#include "swift/Demangling/NamespaceMacros.h"

#include <cstdint>

namespace swift {
namespace Mangle {
SWIFT_BEGIN_INLINE_NAMESPACE

/// Which mangling style and prefix to use.
enum class ManglingFlavor: uint8_t {
  /// Default mangling with the ABI stable $s prefix
  Default,
  /// Embedded Swift's mangling with $e prefix
  Embedded,
};

SWIFT_END_INLINE_NAMESPACE
} // end namespace Mangle
} // end namespace swift

#endif // SWIFT_DEMANGLING_MANGLINGFLAVOR_H
