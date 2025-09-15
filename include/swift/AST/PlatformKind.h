//===--- AST/PlatformKind.h - Swift Language Platform Kinds -----*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
///
/// This file defines the platform kinds for API availability.
///
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_PLATFORM_KIND_H
#define SWIFT_AST_PLATFORM_KIND_H

/// `PlatformKind.h` is imported into Swift. Be *very* careful with what you
/// include here and keep these includes minimal!
///
/// See include guidelines and caveats in `BasicBridging.h`.
#include "swift/Basic/SwiftBridging.h"
#include <stdint.h>

namespace swift {

/// Available platforms for the availability attribute.
enum class ENUM_EXTENSIBILITY_ATTR(closed) PlatformKind : uint8_t {
  none,
#define AVAILABILITY_PLATFORM(X, PrettyName) X,
#include "swift/AST/PlatformKinds.def"
};

} // end namespace swift

#endif // SWIFT_AST_PLATFORM_KIND_H
