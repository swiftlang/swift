//===--- Statistic.swift --------------------------------------------------===//
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

import Foundation
import System
public import Utils

/// A frontend statistic, such as NumConstraintScopes, as written by
/// -stats-output-dir.
public struct Statistic: Sendable, Hashable {
  /// The statistic's name, with or without its group: NumConstraintScopes or
  /// Sema.NumConstraintScopes.
  public var name: String
  /// Whether a larger value is the better result, as for counts of work the
  /// solver managed to avoid.
  public var largerIsBetter: Bool

  /// A statistic. Unless `largerIsBetter` says otherwise, the statistics the
  /// tool knows to be better when larger are marked so.
  public init(name: String, largerIsBetter: Bool? = nil) {
    self.name = name
    self.largerIsBetter =
      largerIsBetter
      ?? Self.largerIsBetterByDefault.contains(Self.baseName(of: name))
  }

  /// Returns nil for an empty name.
  public init?(argument: String) {
    guard !argument.isEmpty else { return nil }
    self.init(name: argument)
  }

  /// The number of solver scopes, which -solver-scope-threshold limits.
  public static let constraintScopes = Statistic(name: "NumConstraintScopes")

  /// Counts of work the solver skipped, so that more is better.
  private static let largerIsBetterByDefault: Set<String> = [
    "NumBindingSetsSkipped", "NumDisjunctionsPruned", "NumDisjunctionsSkipped",
  ]
}

// MARK: - Naming

extension Statistic {
  /// The name without its group, as in NumConstraintScopes.
  public var baseName: String {
    Self.baseName(of: name)
  }

  /// Whether the name carries no group, so that it may match keys in several
  /// groups.
  public var isBare: Bool {
    !name.contains(".")
  }

  /// Whether this is the scope count, which the scope thresholds of RUN lines
  /// limit.
  public var isScopeCount: Bool {
    matches(key: "Sema.NumConstraintScopes")
  }

  /// Whether the statistic is a maximum, such as LargestNumSolverScopes or
  /// MaxMallocUsage, rather than a count. Maxima are combined by taking the
  /// largest value, counts by adding them up.
  public var isMaximum: Bool {
    Self.isMaximum(baseName: baseName)
  }

  /// Whether a key of a stats file names this statistic: the key is the name,
  /// or the name is the key without its group.
  public func matches(key: String) -> Bool {
    key == name || key.hasSuffix(".\(name)")
  }

  /// Whether a statistic flagged with `--larger-is-better` means this one.
  /// A flag without a group means the statistic in any group.
  func isFlagged(by flag: Statistic) -> Bool {
    flag.name == name || (flag.isBare && flag.name == baseName)
  }

  static func baseName(of name: String) -> String {
    String(name.split(separator: ".").last ?? Substring(name))
  }

  static func isMaximum(baseName: String) -> Bool {
    baseName.hasPrefix("Largest") || baseName.hasPrefix("Max")
  }
}

extension Statistic: CustomStringConvertible {
  public var description: String {
    name
  }
}

// MARK: - Counters

extension Statistic {
  /// The counters one compilation wrote to its -stats-output-dir.
  public struct Counters: Sendable, Hashable {
    /// Counter values by key, such as Sema.NumConstraintScopes, combined over
    /// the directory's stats files.
    public var values: [String: Int]

    public init(values: [String: Int]) {
      self.values = values
    }
  }
}

extension Statistic.Counters {
  /// Reads every .json file in a stats directory and combines their counters:
  /// maxima by taking the largest value, the others by adding them up. Values
  /// that are not whole numbers, such as timers, are left out.
  ///
  /// Returns nil when there is no directory or it has no .json files.
  public init?(directory: AbsolutePath) throws {
    guard directory.isDirectory else {
      return nil
    }
    let names = try FileManager.default.contentsOfDirectory(
      atPath: directory.rawPath
    ).filter { FilePath($0).extension == "json" }
    guard !names.isEmpty else {
      return nil
    }
    var values: [String: Int] = [:]
    for name in names {
      let data = try directory.appending(name).read()
      for (key, entry) in try JSONDecoder().decode(
        [String: Entry].self, from: data)
      {
        guard let value = entry.value else {
          continue
        }
        if let previous = values[key] {
          let isMaximum = Statistic.isMaximum(
            baseName: Statistic.baseName(of: key))
          values[key] = isMaximum ? max(previous, value) : previous + value
        } else {
          values[key] = value
        }
      }
    }
    self.init(values: values)
  }

  /// The statistic's value: the sum of the counters whose keys name it, or
  /// the largest of them for a maximum. It is 0 when no key does, because
  /// stats files leave out counters that are 0.
  public func value(of statistic: Statistic) -> Int {
    let matching = values.filter { statistic.matches(key: $0.key) }.map(\.value)
    return statistic.isMaximum ? matching.max() ?? 0 : matching.reduce(0, +)
  }

  /// The keys that name the statistic.
  public func keys(matching statistic: Statistic) -> Set<String> {
    Set(values.keys.filter { statistic.matches(key: $0) })
  }

  /// One value of a stats file, kept only when it is a whole number.
  private struct Entry: Decodable {
    var value: Int?

    init(from decoder: any Decoder) throws {
      value = try? decoder.singleValueContainer().decode(Int.self)
    }
  }
}
