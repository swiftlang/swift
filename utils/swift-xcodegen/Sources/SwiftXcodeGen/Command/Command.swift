//===--- Command.swift ----------------------------------------------------===//
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

struct Command: Hashable {
  var executable: AnyPath
  var args: [Argument]

  init(executable: AnyPath, args: [Argument]) {
    self.executable = executable
    self.args = args
  }
}

extension Command: Decodable {
  init(from decoder: Decoder) throws {
    let command = try decoder.singleValueContainer().decode(String.self)
    self = try CommandParser.parseCommand(command)
  }
}

extension Command {
  var printedArgs: [String] {
    [executable.rawPath.escaped] + args.flatMap(\.printedArgs)
  }

  var printed: String {
    printedArgs.joined(separator: " ")
  }
}

// MARK: Argument

extension Command {
  enum Argument: Hashable {
    case option(Option)
    case flag(Flag)
    case value(String)
  }
}

extension Command.Argument {
  static func option(
    _ flag: Command.Flag, spacing: Command.OptionSpacing, value: String
  ) -> Self {
    .option(.init(flag, spacing: spacing, value: value))
  }

  var flag: Command.Flag? {
    switch self {
    case .option(let opt):
      opt.flag
    case .flag(let flag):
      flag
    case .value:
      nil
    }
  }

  var value: String? {
    switch self {
    case .option(let opt):
      opt.value
    case .value(let value):
      value
    case .flag:
      nil
    }
  }

  var printedArgs: [String] {
    switch self {
    case .option(let opt):
      opt.printedArgs
    case .value(let value):
      [value.escaped]
    case .flag(let f):
      [f.printed]
    }
  }

  var printed: String {
    printedArgs.joined(separator: " ")
  }

  func option(for flag: Command.Flag) -> Command.Option? {
    switch self {
    case .option(let opt) where opt.flag == flag:
      opt
    default:
      nil
    }
  }

  /// If there is a value, apply a transform to it.
  func mapValue(_ fn: (String) throws -> String) rethrows -> Self {
    switch self {
    case .option(let opt):
      .option(try opt.mapValue(fn))
    case .value(let value):
      .value(try fn(value))
    case .flag:
      // Nothing to map.
      self
    }
  }
}

// MARK: Flag

extension Command {
  struct Flag: Hashable {
    var dash: Dash
    var name: Name
  }
}

extension Command.Flag {
  static func dash(_ name: Name) -> Self {
    .init(dash: .single, name: name)
  }
  static func doubleDash(_ name: Name) -> Self {
    .init(dash: .double, name: name)
  }

  var printed: String {
    "\(dash.printed)\(name.rawValue)"
  }
}

extension Command.Flag {
  enum Dash: Int, CaseIterable, Comparable {
    case single = 1, double
    static func < (lhs: Self, rhs: Self) -> Bool { lhs.rawValue < rhs.rawValue }
  }
}

extension Command.Flag.Dash {
  init?(numDashes: Int) {
    self.init(rawValue: numDashes)
  }

  var printed: String {
    switch self {
    case .single:
      return "-"
    case .double:
      return "--"
    }
  }
}

extension DefaultStringInterpolation {
  mutating func appendInterpolation(_ flag: Command.Flag) {
    appendInterpolation(flag.printed)
  }
}

// MARK: Option

extension Command {
  struct Option: Hashable {
    var flag: Flag
    var spacing: OptionSpacing
    var value: String

    init(_ flag: Flag, spacing: OptionSpacing, value: String) {
      self.flag = flag
      self.spacing = spacing
      self.value = value
    }
  }
}

extension Command.Option {
  func withValue(_ newValue: String) -> Self {
    var result = self
    result.value = newValue
    return result
  }

  func mapValue(_ fn: (String) throws -> String) rethrows -> Self {
    withValue(try fn(value))
  }

  var printedArgs: [String] {
    switch spacing {
    case .equals, .unspaced:
      ["\(flag)\(spacing)\(value.escaped)"]
    case .spaced:
      ["\(flag)", value.escaped]
    }
  }

  var printed: String {
    printedArgs.joined(separator: " ")
  }
}

// MARK: OptionSpacing

extension Command {
  enum OptionSpacing: Comparable {
    case equals, unspaced, spaced
  }
}

extension Command.OptionSpacing {
  var printed: String {
    switch self {
    case .equals:   "="
    case .unspaced: ""
    case .spaced:   " "
    }
  }
}

// MARK: CustomStringConvertible

extension Command.Argument: CustomStringConvertible {
  var description: String { printed }
}

extension Command.OptionSpacing: CustomStringConvertible {
  var description: String { printed }
}

extension Command.Flag: CustomStringConvertible {
  var description: String { printed }
}

extension Command.Option: CustomStringConvertible {
  var description: String { printed }
}
