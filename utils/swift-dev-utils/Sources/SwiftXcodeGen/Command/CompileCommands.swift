//===--- CompileCommands.swift --------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2024 - 2025 Apple Inc. and the Swift project authors
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

  public init(from decoder: any Decoder) throws {
    self.init(try decoder.singleValueContainer().decode([Element].self))
  }
}

extension CompileCommands {
  struct Element: Decodable {
    var directory: AbsolutePath
    var file: AbsolutePath
    var output: AnyPath?
    var command: Command

    enum CodingKeys: String, CodingKey {
      case directory, file, output, command
    }
    init(from decoder: any Decoder) throws {
      let container = try decoder.container(keyedBy: CodingKeys.self)
      self.directory = try container.decode(AbsolutePath.self, forKey: .directory)
      self.file = try container.decode(AbsolutePath.self, forKey: .file)
      self.output = try container.decodeIfPresent(AnyPath.self, forKey: .output)
      self.command = try container.decode(DecodableCommand.self, forKey: .command).command
    }
  }
}

fileprivate struct DecodableCommand: Decodable {
  var command: Command
  public init(from decoder: any Decoder) throws {
    let command = try decoder.singleValueContainer().decode(String.self)
    self.command = try CommandParser.parseCommand(command)
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
