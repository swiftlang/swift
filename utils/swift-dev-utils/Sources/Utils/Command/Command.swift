//===--- Command.swift ----------------------------------------------------===//
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

public struct Command: Hashable, Sendable {
  public var executable: AnyPath
  public var args: [Argument]

  public init(executable: AnyPath, args: [Argument]) {
    self.executable = executable
    self.args = args
  }
}

public extension Command {
  var printedArgs: [String] {
    [executable.rawPath.escaped] + args.flatMap(\.printedArgs)
  }

  var printed: String {
    printedArgs.joined(separator: " ")
  }
}

// MARK: Argument

extension Command {
  public enum Argument: Hashable, Sendable, Codable {
    case option(Option)
    case flag(Flag)
    case value(String)
  }
}

public extension Command.Argument {
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

  var rawArgs: [String] {
    switch self {
    case .option(let opt):
      opt.rawArgs
    case .value(let value):
      [value]
    case .flag(let f):
      [f.printed]
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
  public struct Flag: Hashable, Sendable, Codable {
    public var dash: Dash
    public var name: Name

    public init(dash: Dash, name: Name) {
      self.dash = dash
      self.name = name
    }
  }
}

public extension Command.Flag {
  struct Name: Hashable, Sendable, Codable {
    public let rawValue: String

    public init(rawValue: String) {
      self.rawValue = rawValue
    }
  }

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
  public enum Dash: Int, CaseIterable, Comparable, Sendable, Codable {
    case single = 1, double
    public static func < (lhs: Self, rhs: Self) -> Bool { lhs.rawValue < rhs.rawValue }
  }
}

public extension Command.Flag.Dash {
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
  public mutating func appendInterpolation(_ flag: Command.Flag) {
    appendInterpolation(flag.printed)
  }
}

// MARK: Option

extension Command {
  public struct Option: Hashable, Sendable, Codable {
    public var flag: Flag
    public var spacing: OptionSpacing
    public var value: String

    public init(_ flag: Flag, spacing: OptionSpacing, value: String) {
      self.flag = flag
      self.spacing = spacing
      self.value = value
    }
  }
}

public extension Command.Option {
  func withValue(_ newValue: String) -> Self {
    var result = self
    result.value = newValue
    return result
  }

  func mapValue(_ fn: (String) throws -> String) rethrows -> Self {
    withValue(try fn(value))
  }

  var rawArgs: [String] {
    switch spacing {
    case .equals, .unspaced:
      ["\(flag)\(spacing)\(value)"]
    case .spaced:
      ["\(flag)", value]
    }
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
  public enum OptionSpacing: Comparable, Sendable, Codable {
    case equals, unspaced, spaced
  }
}

extension Command.OptionSpacing {
  public var printed: String {
    switch self {
    case .equals:   "="
    case .unspaced: ""
    case .spaced:   " "
    }
  }
}

// MARK: CustomStringConvertible

extension Command.Argument: CustomStringConvertible {
  public var description: String { printed }
}

extension Command.OptionSpacing: CustomStringConvertible {
  public var description: String { printed }
}

extension Command.Flag: CustomStringConvertible {
  public var description: String { printed }
}

extension Command.Option: CustomStringConvertible {
  public var description: String { printed }
}
