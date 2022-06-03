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

internal enum BacktraceStyle {
  case oneline
  case long
}

internal func backtrace(_ stack: [swift_reflection_ptr_t], style: BacktraceStyle,
                        _ symbolicate: (swift_addr_t) -> (module: String?, symbol: String?)) -> String {
  func entry(_ address: swift_reflection_ptr_t) -> String {
    let (module, symbol) = symbolicate(swift_addr_t(address))
    return "\(hex: address) (\(module ?? "<unknown>")) \(symbol ??  "<unknown>")"
  }

  // The pointers to the locations in the backtrace are stored from deepest to
  // shallowest, so `main` will be somewhere near the end.
  switch style {
  case .oneline:
    return stack.reversed().map { entry($0) }.joined(separator: " | ")
  case .long:
    return stack.reversed().enumerated().map {
      " \(String(repeating: " ", count: $0 + 1))\(entry($1))"
    }.joined(separator: "\n")
  }
}
