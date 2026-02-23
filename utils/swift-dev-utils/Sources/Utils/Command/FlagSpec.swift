//===--- FlagSpec.swift ---------------------------------------------------===//
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

extension Command {
  public struct FlagSpec: Sendable {
    public let flags: [Element]

    public init(_ flags: [Element]) {
      // Sort by shortest first, except in cases where one is a prefix of
      // another, in which case we need the longer one first to ensure we prefer
      // it when parsing.
      self.flags = flags.sorted(by: { lhs, rhs in
        let lhs = lhs.flag.name.rawValue
        let rhs = rhs.flag.name.rawValue
        guard lhs.count != rhs.count else {
          return false
        }
        if lhs.count < rhs.count {
          // RHS should be ordered first if it has LHS as a prefix.
          return !rhs.hasPrefix(lhs)
        } else {
          // LHS should be ordered first if it has RHS as a prefix.
          return lhs.hasPrefix(rhs)
        }
      })
    }
  }
}

extension Command {
  public struct OptionSpacingSpec: OptionSet, Sendable {
    public var rawValue: Int
    public init(rawValue: Int) {
      self.rawValue = rawValue
    }
    public init(_ rawValue: Int) {
      self.rawValue = rawValue
    }
    public init(_ optionSpacing: OptionSpacing) {
      switch optionSpacing {
      case .equals:
        self = .equals
      case .unspaced:
        self = .unspaced
      case .spaced:
        self = .spaced
      }
    }
    public static let equals   = Self(1 << 0)
    public static let unspaced = Self(1 << 1)
    public static let spaced   = Self(1 << 2)
  }
}

public extension Command.FlagSpec {
  typealias Flag = Command.Flag
  typealias OptionSpacingSpec = Command.OptionSpacingSpec

  struct Element: Sendable {
    public let flag: Flag
    public let spacing: OptionSpacingSpec

    public init(_ flag: Flag, option: [OptionSpacingSpec]) {
      self.flag = flag
      self.spacing = option.reduce([], { $0.union($1) })
    }

    public init(_ flag: Flag, option: OptionSpacingSpec...) {
      self.init(flag, option: option)
    }
  }
}
