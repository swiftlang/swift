//===-- AST/ExportKind.h ----------------------------------------*- C++ -*-===//
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

#ifndef SWIFT_AST_EXPORT_KIND_H
#define SWIFT_AST_EXPORT_KIND_H

/// `ExportKind.h` is imported into Swift. Be *very* careful with what you
/// include here and keep these includes minimal!
///
/// See include guidelines and caveats in `BasicBridging.h`.
#include "swift/Basic/SwiftBridging.h"

namespace swift {

/// How a particular declaration is exported per the @export attribute.
enum class ENUM_EXTENSIBILITY_ATTR(closed) ExportKind {
  /// Export only the interface to this declaration (e.g., as a callable symbol)
  /// and never it's definition.
  Interface SWIFT_NAME("interface"),

  /// Export only the implementation of this declaration and do not produce a
  /// symbol.
  Implementation SWIFT_NAME("implementation"),
};

} // namespace swift

#endif // SWIFT_AST_EXPORT_KIND_H
