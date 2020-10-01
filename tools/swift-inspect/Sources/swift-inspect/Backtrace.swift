//===----------------------------------------------------------------------===//
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

import SwiftRemoteMirror

struct Backtrace {
  enum Style {
    case oneLine
    case long
  }

  /// The pointers to the locations in the backtrace. These are stored from
  /// deepest to shallowest, so main() will be somewhere near the end.
  var ptrs: [swift_reflection_ptr_t]

  func symbolString(
    ptr: swift_reflection_ptr_t,
    inspector: Inspector
  ) -> String {
    let symbol = inspector.getSymbol(address: ptr)
    let name = symbol.name ?? "<unknown>"
    let library = symbol.library ?? "<unknown>"
    return "\(hex: ptr) (\(library)) \(name)"
  }

  func symbolicatedOneLine(inspector: Inspector) -> String {
    return ptrs.reversed().map {
      symbolString(ptr: $0, inspector: inspector)
    }.joined(separator: " | ")
  }

  func symbolicatedLong(inspector: Inspector) -> String {
    return ptrs.reversed().enumerated().map {
      let indent = String(repeating: "  ", count: $0 + 1)
      return indent + symbolString(ptr: $1, inspector: inspector)
    }.joined(separator: "\n")
  }

  func symbolicated(style: Style, inspector: Inspector) -> String {
    switch style {
    case .oneLine:
      return symbolicatedOneLine(inspector: inspector)
    case .long:
      return symbolicatedLong(inspector: inspector)
    }
  }
}
