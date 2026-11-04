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
  /// The statistic's name without its group, for example NumConstraintScopes.
  public var name: String

  public init(name: String) {
    self.name = name
  }

  /// Returns nil for an empty name.
  public init?(argument: String) {
    guard !argument.isEmpty else { return nil }
    self.init(name: argument)
  }

  /// The number of solver scopes, which -solver-scope-threshold limits.
  public static let constraintScopes = Statistic(name: "NumConstraintScopes")
}

// MARK: - Matching

extension Statistic {
  /// Whether a key of a stats file names this statistic. Keys carry a group,
  /// as in Sema.NumConstraintScopes; a key without a group matches too.
  public func matches(key: String) -> Bool {
    key == name || key.hasSuffix(".\(name)")
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
    /// Counter values by key, such as Sema.NumConstraintScopes, summed over
    /// the directory's stats files.
    public var values: [String: Int]

    public init(values: [String: Int]) {
      self.values = values
    }
  }
}

extension Statistic.Counters {
  /// Reads every .json file in a stats directory and sums their counters.
  /// Values that are not whole numbers, such as timers, are left out.
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
        if let value = entry.value {
          values[key, default: 0] += value
        }
      }
    }
    self.init(values: values)
  }

  /// The statistic's value: the sum of the counters whose keys name it. It is
  /// 0 when no key does, because stats files leave out counters that are 0.
  public func value(of statistic: Statistic) -> Int {
    values.reduce(0) { total, counter in
      statistic.matches(key: counter.key) ? total + counter.value : total
    }
  }

  /// One value of a stats file, kept only when it is a whole number.
  private struct Entry: Decodable {
    var value: Int?

    init(from decoder: any Decoder) throws {
      value = try? decoder.singleValueContainer().decode(Int.self)
    }
  }
}
