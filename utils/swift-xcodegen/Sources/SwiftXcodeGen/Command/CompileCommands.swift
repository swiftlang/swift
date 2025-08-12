//===--- CompileCommands.swift --------------------------------------------===//
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

/// A Decodable representation of compile_commands.json.
struct CompileCommands: Decodable {
  public var commands: [Element]

  init(_ commands: [Element]) {
    self.commands = commands
  }

  public init(from decoder: Decoder) throws {
    self.init(try decoder.singleValueContainer().decode([Element].self))
  }
}

extension CompileCommands {
  struct Element: Decodable {
    var directory: AbsolutePath
    var file: AbsolutePath
    var output: AnyPath?
    var command: Command
  }
}

extension CompileCommands: RandomAccessCollection {
  typealias Index = Int

  var startIndex: Index { commands.startIndex }
  var endIndex: Index { commands.endIndex }

  func index(_ i: Int, offsetBy distance: Int) -> Int {
    commands.index(i, offsetBy: distance)
  }
  subscript(position: Index) -> Element {
    commands[position]
  }
}
