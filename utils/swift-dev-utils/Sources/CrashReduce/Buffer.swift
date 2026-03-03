//===--- Buffer.swift -----------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

/// A named source buffer.
struct Buffer: Hashable {
  var name: String
  var code: Code

  static func makeDefault(_ buffers: [Code]) -> [Buffer] {
    buffers.enumerated().map { (i, code) in
      let name = i == 0 ? "main.swift" : "x\(i).swift"
      return Buffer(name: name, code: code)
    }
  }
}
