//===--- Configuration.swift ----------------------------------------------===//
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

import System
public import Utils

/// The settings of a comparison. A nil path or flag list stands for its
/// default, which is worked out when the comparison starts.
public struct Configuration: Sendable {
  /// The swift checkout. The default test directory, build directory and
  /// swift_snapshot_tool package are found from it.
  public var swiftDirectory: AnyPath?
  /// Compile only this file instead of the test directory.
  public var file: AnyPath?
  /// The directory whose .swift files are compiled, including subdirectories.
  public var directory: AnyPath?
  /// The statistics to compare, as given. Read them through
  /// `effectiveStatistics`.
  public var statistics: [Statistic] = []
  /// Statistics for which a larger value is better, besides the ones the tool
  /// knows. A name without a group means the statistic in any group.
  public var largerIsBetter: [Statistic] = []
  /// The column the percentages and the summary are relative to.
  public var baseline: Column?
  /// Hide the rows where no column differs from the baseline.
  public var onlyChanged = false
  /// Also save the report as report.md at the top of the output directory.
  public var saveReport = true
  /// Also print the summary tables for each subdirectory of the directory.
  public var byDirectory = false
  /// The rows of the pair table. Empty stands for wip-on against main-on.
  public var pairs: [Column.Pair] = []
  public var noColor = false
  public var versionControl: VersionControl = .auto
  /// Allow git in a colocated jj repository.
  public var forceVersionControl = false
  /// Fetch from origin before resolving main@origin.
  public var fetch = false
  /// With git, stash uncommitted files while main is built.
  public var allowDirty = false
  /// Prebuilt swift.org snapshots to add as columns.
  public var snapshots: [Snapshot] = []
  /// The swift_snapshot_tool branch flag, for example development.
  public var snapshotBranch: String?
  /// The swift_snapshot_tool executable.
  public var snapshotTool: AnyPath?
  /// Where swift_snapshot_tool downloads toolchains.
  public var workspace: AnyPath?
  public var buildDirectory: AnyPath?
  public var outputDirectory: AnyPath?
  /// Frontend flags passed to every compiler, before the RUN line flags.
  public var extraFlags: FrontendFlags?
  /// Frontend flags that make up the on column of every compiler.
  public var onFlags: FrontendFlags?
  public var sdk: AnyPath?
  /// Reuse the compilers saved in the output directory.
  public var skipBuild = false
  /// Copy the host libraries next to each saved compiler instead of linking
  /// to the build directory's lib directory.
  public var copyLibraries = false

  public init() {}
}

// MARK: - Effective settings

extension Configuration {
  /// The statistics to compare: the given ones, or the scope count when none
  /// are given. Those named in `largerIsBetter` are marked so.
  public var effectiveStatistics: [Statistic] {
    let chosen = statistics.isEmpty ? [.constraintScopes] : statistics
    return chosen.map { statistic in
      var result = statistic
      if largerIsBetter.contains(where: statistic.isFlagged(by:)) {
        result.largerIsBetter = true
      }
      return result
    }
  }

  /// The baseline column, main-off unless one is set.
  public var effectiveBaseline: Column {
    baseline ?? Column(compiler: "main", state: .off)
  }

  /// The rows of the pair table, wip-on against main-on unless some are set.
  public var effectivePairs: [Column.Pair] {
    if !pairs.isEmpty {
      return pairs
    }
    return [
      Column.Pair(
        measured: Column(compiler: "wip", state: .on),
        reference: Column(compiler: "main", state: .on))
    ]
  }

  public var effectiveExtraFlags: FrontendFlags {
    extraFlags ?? FrontendFlags(arguments: ["-debug-time-function-bodies"])
  }

  public var effectiveOnFlags: FrontendFlags {
    onFlags
      ?? FrontendFlags(arguments: [
        "-solver-enable-promote-supertypes", "-solver-enable-type-var-joins",
        "-solver-enable-short-conversions",
      ])
  }
}

// MARK: - Locations

extension Configuration {
  /// The swift checkout: `swiftDirectory`, or else the checkout this tool was
  /// built from.
  public var effectiveSwiftDirectory: AbsolutePath {
    swiftDirectory?.absoluteInWorkingDir ?? Self.builtFromSwiftDirectory
  }

  /// The checkout this tool's sources are in.
  static var builtFromSwiftDirectory: AbsolutePath {
    // This file is swift/utils/swift-dev-utils/Sources/CompareSolverStat/
    // Configuration.swift, five components below the checkout.
    AbsolutePath(FilePath(#filePath)).dropLast(5)
  }

  /// The directory whose files are compiled when no file is given.
  public var effectiveTestDirectory: AbsolutePath {
    directory?.absoluteInWorkingDir
      ?? effectiveSwiftDirectory.appending(
        "validation-test/Sema/type_checker_perf")
  }

  /// The swift build directory. By default it is the one build-script makes
  /// with -r, next to the checkout.
  public var effectiveBuildDirectory: AbsolutePath {
    buildDirectory?.absoluteInWorkingDir
      ?? effectiveSwiftDirectory.dropLast().appending(
        "build/Ninja-RelWithDebInfoAssert/swift-macosx-arm64")
  }

  /// The output directory, compare_stats in the current directory by default.
  public var effectiveOutputDirectory: AbsolutePath {
    (outputDirectory ?? AnyPath(FilePath("compare_stats"))).absoluteInWorkingDir
  }

  /// Where swift_snapshot_tool downloads toolchains.
  public var effectiveWorkspace: AbsolutePath {
    workspace?.absoluteInWorkingDir
      ?? AbsolutePath(FilePath("/tmp/swift_snapshot_tool_workspace_v1"))
  }
}

// MARK: - VersionControl

extension Configuration {
  /// The version control system of the swift repository.
  public enum VersionControl: String, Sendable, CaseIterable {
    /// jj if the repository has a .jj directory, git otherwise.
    case auto
    case jj
    case git
  }
}

// MARK: - FrontendFlags

extension Configuration {
  /// A list of frontend flags, such as the flags of the on columns.
  public struct FrontendFlags: Sendable, Hashable {
    public var arguments: [String]

    public init(arguments: [String]) {
      self.arguments = arguments
    }

    /// Splits a command-line value into flags at whitespace. Quoting is not
    /// supported.
    public init?(argument: String) {
      self.arguments = argument.split(whereSeparator: \.isWhitespace).map(
        String.init)
    }
  }
}

// MARK: - Snapshot

extension Configuration {
  /// A prebuilt swift.org snapshot, added to the comparison under a name.
  public struct Snapshot: Sendable, Hashable {
    /// The compiler name, which gives the columns NAME-off and NAME-on.
    public var name: String
    /// The snapshot used is the first one on or before this day.
    public var day: Day

    public init(name: String, day: Day) {
      self.name = name
      self.day = day
    }

    /// Parses NAME=DATE, where DATE is yyyy-MM-dd. Returns nil when either part
    /// is missing or the date is not valid.
    public init?(argument: String) {
      let parts = argument.split(
        separator: "=", maxSplits: 1, omittingEmptySubsequences: false)
      guard parts.count == 2, !parts[0].isEmpty,
        let day = Day(argument: String(parts[1]))
      else {
        return nil
      }
      self.init(name: String(parts[0]), day: day)
    }
  }
}

extension Configuration.Snapshot {
  /// A calendar day, written yyyy-MM-dd.
  public struct Day: Sendable, Hashable {
    public var year: Int
    public var month: Int
    public var day: Int

    public init(year: Int, month: Int, day: Int) {
      self.year = year
      self.month = month
      self.day = day
    }

    /// Parses yyyy-MM-dd. Returns nil unless the month is 1 to 12 and the day
    /// is 1 to 31.
    public init?(argument: String) {
      let parts = argument.split(
        separator: "-", omittingEmptySubsequences: false)
      guard parts.count == 3, parts[0].count == 4, parts[1].count == 2,
        parts[2].count == 2, let year = Int(parts[0]),
        let month = Int(parts[1]), let day = Int(parts[2]),
        (1 ... 12).contains(month), (1 ... 31).contains(day)
      else {
        return nil
      }
      self.init(year: year, month: month, day: day)
    }
  }
}

extension Configuration.Snapshot.Day: CustomStringConvertible {
  /// The day as yyyy-MM-dd, the form swift_snapshot_tool takes.
  public var description: String {
    func padded(_ value: Int, to width: Int) -> String {
      let digits = String(value)
      return String(repeating: "0", count: max(0, width - digits.count))
        + digits
    }
    return
      "\(padded(year, to: 4))-\(padded(month, to: 2))-\(padded(day, to: 2))"
  }
}
