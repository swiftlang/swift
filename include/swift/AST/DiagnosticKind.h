//===-- AST/DiagnosticKind.h ------------------------------------*- C++ -*-===//
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

#ifndef SWIFT_AST_DIAGNOSTIC_KIND_H
#define SWIFT_AST_DIAGNOSTIC_KIND_H

/// This header is included in a bridging header. Be *very* careful with what
/// you include here! See include caveats in `ASTBridging.h`.
#include "swift/Basic/SwiftBridging.h"
#include <stdint.h>

namespace swift {

/// Describes the kind of diagnostic.
enum class ENUM_EXTENSIBILITY_ATTR(open) DiagnosticKind : uint8_t {
  Error SWIFT_NAME("error"),
  Warning SWIFT_NAME("warning"),
  Remark SWIFT_NAME("remark"),
  Note SWIFT_NAME("note")
};

} // namespace swift

#endif // SWIFT_AST_DIAGNOSTIC_KIND_H
