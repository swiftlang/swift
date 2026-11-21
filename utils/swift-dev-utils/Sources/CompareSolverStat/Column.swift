//===--- Column.swift -----------------------------------------------------===//
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

/// A column of the comparison: one compiler, with the experimental solver
/// flags off or on. It is written NAME-off or NAME-on, for example main-off.
public struct Column: Sendable, Hashable {
  /// The compiler name: main, wip, or the name of a snapshot.
  public var compiler: String
  public var state: State

  public init(compiler: String, state: State) {
    self.compiler = compiler
    self.state = state
  }

  /// Parses NAME-off or NAME-on. A bare compiler name stands for its off
  /// column. Returns nil when the compiler name is empty.
  public init?(argument: String) {
    for state in State.allCases {
      let suffix = "-\(state.rawValue)"
      if argument.hasSuffix(suffix) {
        let compiler = String(argument.dropLast(suffix.count))
        guard !compiler.isEmpty else { return nil }
        self.init(compiler: compiler, state: state)
        return
      }
    }
    guard !argument.isEmpty else { return nil }
    self.init(compiler: argument, state: .off)
  }
}

extension Column: CustomStringConvertible {
  public var description: String {
    "\(compiler)-\(state.rawValue)"
  }
}

// MARK: - State

extension Column {
  /// Whether the column adds the experimental solver flags.
  public enum State: String, Sendable, CaseIterable {
    case off
    case on
  }
}

// MARK: - Pair

extension Column {
  /// A row of the pair table: a measured column compared with a reference
  /// column.
  public struct Pair: Sendable, Hashable {
    public var measured: Column
    public var reference: Column

    public init(measured: Column, reference: Column) {
      self.measured = measured
      self.reference = reference
    }

    /// Parses MEASURED:REFERENCE, where each side is a column as `Column`
    /// parses it. Returns nil when either side is missing or not a column.
    public init?(argument: String) {
      let parts = argument.split(
        separator: ":", maxSplits: 1, omittingEmptySubsequences: false)
      guard parts.count == 2, let measured = Column(argument: String(parts[0])),
        let reference = Column(argument: String(parts[1]))
      else {
        return nil
      }
      self.init(measured: measured, reference: reference)
    }
  }
}

extension Column.Pair: CustomStringConvertible {
  public var description: String {
    "\(measured) vs \(reference)"
  }
}
