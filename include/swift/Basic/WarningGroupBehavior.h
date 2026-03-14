//===-- WarningGroupBehavior.h ----------------------------------*- C++ -*-===//
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

#ifndef SWIFT_BASIC_WARNINGBEHAVIOR_H
#define SWIFT_BASIC_WARNINGBEHAVIOR_H

/// `WarningGroupBehavior.h` is imported into Swift. Be *very* careful with what you
/// include here and keep these includes minimal!
///
/// See include guidelines and caveats in `BasicBridging.h`.
#include "swift/Basic/SwiftBridging.h"
#include <stdint.h>
#include <utility>
#include <array>

namespace swift {

// Describes how a diagnostic group's warnings are to be emitted
enum ENUM_EXTENSIBILITY_ATTR(closed) WarningGroupBehavior {
  AsError SWIFT_NAME("error"),
  AsWarning SWIFT_NAME("warning"),
  Ignored SWIFT_NAME("ignored"),
  None SWIFT_NAME("none")
};

constexpr const auto DiagLinksCount = [] {
  size_t count = 0;
#define GROUP_LINK(Parent, Child) ++count;
#include "swift/AST/DiagnosticGroups.def"
  return count;
}();

constexpr std::array<std::pair<const char*, const char*>, swift::DiagLinksCount> DiagnosticGroupLinks = {{
#define GROUP_LINK(Parent, Child) {#Parent, #Child},
#include "swift/AST/DiagnosticGroups.def"
}};

} // namespace swift

#endif // SWIFT_BASIC_WARNINGBEHAVIOR_H

