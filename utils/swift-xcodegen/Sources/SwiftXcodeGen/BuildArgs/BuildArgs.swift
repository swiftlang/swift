//===--- BuildArgs.swift --------------------------------------------------===//
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

struct BuildArgs {
  let command: KnownCommand
  private var topLevelArgs: [Command.Argument] = []
  private var subOptArgs: [SubOptionArgs] = []

  init(for command: KnownCommand, args: [Command.Argument] = []) {
    self.command = command
    self += args
  }
}

extension BuildArgs {
  struct SubOptionArgs {
    var flag: Command.Flag
    var args: BuildArgs

    var command: KnownCommand {
      args.command
    }
  }
}

extension BuildArgs {
  typealias Element = Command.Argument

  /// Whether both the arguments and sub-option arguments are empty.
  var isEmpty: Bool {
    topLevelArgs.isEmpty && subOptArgs.isEmpty
  }

  /// Whether the argument have a given flag.
  func hasFlag(_ flag: Command.Flag) -> Bool {
    topLevelArgs.contains(where: { $0.flag == flag })
  }

  /// Retrieve the flag in a given list of flags.
  func lastFlag(in flags: [Command.Flag]) -> Command.Flag? {
    for arg in topLevelArgs.reversed() {
      guard let flag = arg.flag, flags.contains(flag) else { continue }
      return flag
    }
    return nil
  }

  /// Retrieve the flag in a given list of flags.
  func lastFlag(in flags: Command.Flag...) -> Command.Flag? {
    lastFlag(in: flags)
  }

  /// Retrieve the last value for a given flag, unescaped.
  func lastValue(for flag: Command.Flag) -> String? {
    topLevelArgs.last(where: { $0.flag == flag })?.value
  }

  /// Retrieve the last printed value for a given flag, escaping as needed.
  func lastPrintedValue(for flag: Command.Flag) -> String? {
    lastValue(for: flag)?.escaped
  }

  /// Retrieve the printed values for a given flag, escaping as needed.
  func printedValues(for flag: Command.Flag) -> [String] {
    topLevelArgs.compactMap { $0.option(for: flag)?.value.escaped }
  }

  var printedArgs: [String] {
    topLevelArgs.flatMap(\.printedArgs)
      + subOptArgs.flatMap { subArgs in
        let printedFlag = subArgs.flag.printed
        return subArgs.args.printedArgs.flatMap { [printedFlag, $0] }
      }
  }

  var printed: String {
    printedArgs.joined(separator: " ")
  }

  func hasSubOptions(for command: KnownCommand) -> Bool {
    subOptArgs.contains(where: { $0.command == command })
  }

  /// Retrieve a set of sub-options for a given command.
  func subOptions(for command: KnownCommand) -> BuildArgs {
    hasSubOptions(for: command) ? self[subOptions: command] : .init(for: command)
  }

  subscript(subOptions command: KnownCommand) -> BuildArgs {
    _read {
      let index = subOptArgs.firstIndex(where: { $0.command == command })!
      yield subOptArgs[index].args
    }
    _modify {
      let index = subOptArgs.firstIndex(where: { $0.command == command })!
      yield &subOptArgs[index].args
    }
  }

  /// Apply a transform to the set of arguments. Note this doesn't include any
  /// sub-options.
  func map(_ transform: (Element) throws -> Element) rethrows -> Self {
    var result = self
    result.topLevelArgs = try topLevelArgs.map(transform)
    return result
  }

  /// Apply a filter to the set of arguments. Note this doesn't include any
  /// sub-options.
  func filter(_ predicate: (Element) throws -> Bool) rethrows -> Self {
    var result = self
    result.topLevelArgs = try topLevelArgs.filter(predicate)
    return result
  }

  /// Remove a set of flags from the arguments.
  mutating func exclude(_ flags: [Command.Flag]) {
    topLevelArgs.removeAll { arg in
      guard let f = arg.flag else { return false }
      return flags.contains(f)
    }
  }

  /// Remove a set of flags from the arguments.
  mutating func exclude(_ flags: Command.Flag...) {
    exclude(flags)
  }

  /// Remove a set of flags from the arguments.
  func excluding(_ flags: [Command.Flag]) -> Self {
    var result = self
    result.exclude(flags)
    return result
  }

  /// Remove a set of flags from the arguments.
  func excluding(_ flags: Command.Flag...) -> Self {
    excluding(flags)
  }

  /// Take the last unescaped value for a given flag, removing all occurances
  /// of the flag from the arguments.
  mutating func takeLastValue(for flag: Command.Flag) -> String? {
    guard let value = lastValue(for: flag) else { return nil }
    exclude(flag)
    return value
  }

  /// Take the last printed value for a given flag, escaping as needed, and
  /// removing all occurances of the flag from the arguments
  mutating func takePrintedLastValue(for flag: Command.Flag) -> String? {
    guard let value = lastPrintedValue(for: flag) else { return nil }
    exclude(flag)
    return value
  }

  /// Take a set of printed values for a given flag, escaping as needed.
  mutating func takePrintedValues(for flag: Command.Flag) -> [String] {
    let result = topLevelArgs.compactMap { $0.option(for: flag)?.value.escaped }
    exclude(flag)
    return result
  }

  /// Take a flag, returning `true` if it was removed, `false` if it isn't
  /// present.
  mutating func takeFlag(_ flag: Command.Flag) -> Bool {
    guard hasFlag(flag) else { return false }
    exclude(flag)
    return true
  }

  /// Takes a set of flags, returning `true` if the flags were removed, `false`
  /// if they aren't present.
  mutating func takeFlags(_ flags: Command.Flag...) -> Bool {
    guard flags.contains(where: self.hasFlag) else { return false }
    exclude(flags)
    return true
  }

  /// Takes a set of related flags, returning the last one encountered, or `nil`
  /// if no flags in the group are present.
  mutating func takeFlagGroup(_ flags: Command.Flag...) -> Command.Flag? {
    guard let value = lastFlag(in: flags) else { return nil }
    exclude(flags)
    return value
  }

  private mutating func appendSubOptArg(
    _ value: String,
    for command: KnownCommand,
    flag: Command.Flag
  ) {
    let idx =
      subOptArgs.firstIndex(where: { $0.command == command })
      ?? {
        subOptArgs.append(.init(flag: flag, args: .init(for: command)))
        return subOptArgs.endIndex - 1
      }()
    subOptArgs[idx].args.append(value.escaped)
  }

  mutating func append(_ element: Element) {
    // https://github.com/swiftlang/swift-format/issues/1037
    // swift-format-ignore
    if let flag = element.flag, let command = flag.subOptionCommand,
       let value = element.value
    {
      appendSubOptArg(value, for: command, flag: flag)
    } else if let last = topLevelArgs.last, case .flag(let flag) = last,
              case .value(let value) = element
    {
      // If the last element is a flag, and this is a value, we may need to
      // merge.
      topLevelArgs.removeLast()
      topLevelArgs += try! CommandParser.parseArguments(
        "\(flag) \(value.escaped)",
        for: command
      )
    } else {
      topLevelArgs.append(element)
    }
  }

  mutating func append<S: Sequence>(contentsOf seq: S) where S.Element == Element {
    for element in seq {
      append(element)
    }
  }

  static func += <S: Sequence>(lhs: inout Self, rhs: S) where S.Element == Element {
    lhs.append(contentsOf: rhs)
  }

  mutating func append(_ input: String) {
    self += try! CommandParser.parseArguments(input, for: command)
  }

  /// Apply a transform to the values of any options present. If
  /// `includeSubOptions` is `true`, the transform will also be applied to any
  /// sub-options present.
  mutating func transformValues(
    for flag: Command.Flag? = nil,
    includeSubOptions: Bool,
    _ fn: (String) throws -> String
  ) rethrows {
    topLevelArgs = try topLevelArgs.map { arg in
      guard flag == nil || arg.flag == flag else { return arg }
      return try arg.mapValue(fn)
    }
    if includeSubOptions {
      for idx in subOptArgs.indices {
        try subOptArgs[idx].args.transformValues(
          for: flag,
          includeSubOptions: true,
          fn
        )
      }
    }
  }

  struct PathSubstitution: Hashable {
    var oldPath: AbsolutePath
    var newPath: AnyPath
  }

  /// Apply a substitution to any paths present in the option values, returning
  /// the substitutions made. If `includeSubOptions` is `true`, the substitution
  /// will also be applied to any sub-options present.
  mutating func substitutePaths<Path: PathProtocol>(
    for flag: Command.Flag? = nil,
    includeSubOptions: Bool,
    _ fn: (AbsolutePath) throws -> Path?
  ) rethrows -> [BuildArgs.PathSubstitution] {
    var subs: [BuildArgs.PathSubstitution] = []
    try transformValues(
      for: flag,
      includeSubOptions: includeSubOptions
    ) { value in
      // https://github.com/swiftlang/swift-format/issues/1037
      // swift-format-ignore
      guard case .absolute(let path) = AnyPath(value),
            let newPath = try fn(path)
      else { return value }
      let subst = PathSubstitution(oldPath: path, newPath: AnyPath(newPath))
      subs.append(subst)
      return subst.newPath.rawPath
    }
    return subs
  }
}

extension BuildArgs: CustomStringConvertible {
  var description: String { printed }
}
