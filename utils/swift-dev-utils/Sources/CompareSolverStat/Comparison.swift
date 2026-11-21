//===--- Comparison.swift -------------------------------------------------===//
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

/// The results of every run in every column, ready to compare.
public struct Comparison: Sendable {
  /// The columns, in report order.
  public var columns: [Column]
  /// One row per run, in report order.
  public var rows: [Row]

  public init(columns: [Column], rows: [Row]) {
    self.columns = columns
    self.rows = rows
  }
}

// MARK: - Row

extension Comparison {
  /// A run and its result in each column.
  public struct Row: Sendable {
    public var run: TestFile.Run
    /// The run's result in each column. A column without an entry counts as
    /// failed.
    public var results: [Column: Result]

    public init(run: TestFile.Run, results: [Column: Result]) {
      self.run = run
      self.results = results
    }
  }
}

extension Comparison.Row {
  /// The run's result in a column.
  public func result(in column: Column) -> Comparison.Result {
    results[column] ?? .failed
  }

  /// The statistic's value in a column, or nil when the run has no statistics
  /// there.
  public func value(of statistic: Statistic, in column: Column) -> Int? {
    result(in: column).value(of: statistic)
  }
}

// MARK: - Keys

extension Comparison {
  /// The keys that name a statistic in any measured run of any column.
  public func keys(matching statistic: Statistic) -> Set<String> {
    var keys: Set<String> = []
    for row in rows {
      for case .measured(let counters) in row.results.values {
        keys.formUnion(counters.keys(matching: statistic))
      }
    }
    return keys
  }
}

// MARK: - Result

extension Comparison {
  /// What one compilation produced.
  public enum Result: Sendable, Hashable {
    case measured(Statistic.Counters)
    /// The compilation wrote no statistics, for example because it crashed.
    case failed
    /// The compiler does not have one of the run's options.
    case skipped
  }
}

extension Comparison.Result {
  /// The statistic's value, or nil unless the compilation was measured.
  public func value(of statistic: Statistic) -> Int? {
    guard case .measured(let counters) = self else {
      return nil
    }
    return counters.value(of: statistic)
  }
}

// MARK: - Counts

extension Comparison {
  /// How a measured column compares with a reference column over some runs.
  public struct Counts: Sendable, Hashable {
    public var runs = 0
    public var smaller = 0
    public var larger = 0
    public var same = 0
    /// Runs in which either column has no statistics.
    public var failed = 0
    /// Runs that either column skipped.
    public var skipped = 0
    /// Runs in which the measured column is over the scope threshold of the
    /// RUN line.
    public var over = 0
    /// Runs over the threshold in the measured column but not in the
    /// reference column.
    public var newlyOver = 0
    /// Runs over the threshold in the reference column but not in the measured
    /// column.
    public var newlyUnder = 0

    public init() {}
  }
}

extension Comparison.Counts {
  /// Counts how the measured column compares with the reference column over
  /// the rows. The threshold counts are only kept for the scope count, which
  /// the thresholds limit.
  public init(
    statistic: Statistic, measured: Column, reference: Column,
    rows: [Comparison.Row]
  ) {
    self.init()
    for row in rows {
      runs += 1
      if row.result(in: measured) == .skipped
        || row.result(in: reference) == .skipped
      {
        skipped += 1
        continue
      }
      guard let value = row.value(of: statistic, in: measured),
        let base = row.value(of: statistic, in: reference)
      else {
        failed += 1
        continue
      }
      if value < base {
        smaller += 1
      } else if value > base {
        larger += 1
      } else {
        same += 1
      }
      if statistic.isScopeCount, let threshold = row.run.line.scopeThreshold {
        let isOver = value > threshold
        let wasOver = base > threshold
        if isOver {
          over += 1
        }
        if isOver && !wasOver {
          newlyOver += 1
        }
        if wasOver && !isOver {
          newlyUnder += 1
        }
      }
    }
  }
}
