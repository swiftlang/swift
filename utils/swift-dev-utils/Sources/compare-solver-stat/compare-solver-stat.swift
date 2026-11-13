//===--- compare-solver-stat.swift ----------------------------------------===//
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

public import ArgumentParser
public import CompareSolverStat
import Foundation
public import Utils

@main
struct CompareSolverStatCommand: AsyncParsableCommand {
  static let configuration = CommandConfiguration(
    commandName: "compare-solver-stat",
    abstract: """
      Compile test files with different compiler versions and compare their -stats-output-dir statistics.
      """
  )

  @Argument(help: "Compile this file")
  var file: AnyPath?

  @Option(
    help: """
      Test directory to compile, including subdirectories \
      (default: validation-test/Sema/type_checker_perf).
      """)
  var dir: AnyPath?

  @Option(
    help: """
      Statistic(s) to compare, repeatable (default: NumConstraintScopes).
      """)
  var stat: [Statistic] = []

  @Option(
    help: """
      Statistic for which a larger value is better, repeatable. \
      NumBindingSetsSkipped, NumDisjunctionsPruned and NumDisjunctionsSkipped \
      are defaults.
      """)
  var largerIsBetter: [Statistic] = []

  @Option(
    help: """
       Column any performance change is relative to, for example \
       snap1-off. A compiler name stands for its off column (default: \
       main-off).
      """)
  var baseline: Column?

  @Flag(help: "Hide rows where no column differs from the baseline.")
  var onlyChanged = false

  @Flag(
    inversion: .prefixedNo,
    help: """
      Save a markdown file, report.md, at the top level of --out.
      """)
  var report = true

  @Flag(
    help: """
      Print the summary tables for each subdirectory of --dir.
      """)
  var byDir = false

  @Option(
    help: """
      Add row(s) to the summary table to compare MEASURED with \
      REFERENCE, repeatable (default: wip-on:main-on).
      """)
  var compare: [Column.Pair] = []

  @Flag(help: "Do not colour the changed cells.")
  var noColor = false

  @Option(help: "Version control system used.")
  var vcs: Configuration.VersionControl = .auto

  @Flag(help: "Allow --vcs git in a colocated jj repository.")
  var forceVcs = false

  @Flag(help: "Fetch from origin before resolving main@origin.")
  var fetch = false

  @Flag(help: "With git, stash uncommitted files while main is built.")
  var allowDirty = false

  @Option(
    name: .customLong("snapshot"),
    help: """
      Add a swift.org snapshot, NAME=DATE (yyyy-MM-dd), repeatable.
      """)
  var snapshots: [Configuration.Snapshot] = []

  @Option(
    help:
      "swift_snapshot_tool branch flag, for example development or release_6_2."
  )
  var snapshotBranch: String?

  @Option(
    help: "The swift_snapshot_tool executable (or set SWIFT_SNAPSHOT_TOOL).")
  var snapshotTool: AnyPath?

  @Option(help: "Where swift_snapshot_tool downloads toolchains.")
  var workspace: AnyPath?

  @Option(
    help: """
      The swift checkout (default: the checkout this tool was built from). \
      The defaults of --dir, --build-dir and the swift_snapshot_tool package rely on this.
      """)
  var swiftDir: AnyPath?

  @Option(help: "The swift build directory.")
  var buildDir: AnyPath?

  @Option(help: "Output directory (default: compare_stats).")
  var out: AnyPath?

  // The values of --extra and --on-flags are frontend flags, which start with
  // a dash, so they are taken as values unconditionally.
  @Option(
    parsing: .unconditional,
    help: "Frontend flags passed to every compiler, before the RUN line flags.")
  var extra: Configuration.FrontendFlags?

  @Option(
    parsing: .unconditional,
    help: """
      Frontend flags that make up the on column of every compiler. Flags on a \
      RUN line come after.
      """)
  var onFlags: Configuration.FrontendFlags?

  @Option(help: "SDK path (default: xcrun --show-sdk-path).")
  var sdk: AnyPath?

  @Flag(help: "Reuse the compilers saved in the output directory.")
  var skipBuild = false

  @Flag(
    help: """
      Copy the build/lib per compiler otherwise symlink to last build.
      """)
  var copyLib = false

  func run() async throws {
    var configuration = Configuration()
    configuration.file = file
    configuration.directory = dir
    configuration.statistics = stat
    configuration.largerIsBetter = largerIsBetter
    configuration.baseline = baseline
    configuration.onlyChanged = onlyChanged
    configuration.saveReport = report
    configuration.byDirectory = byDir
    configuration.pairs = compare
    configuration.noColor = noColor
    configuration.versionControl = vcs
    configuration.forceVersionControl = forceVcs
    configuration.fetch = fetch
    configuration.allowDirty = allowDirty
    configuration.snapshots = snapshots
    configuration.snapshotBranch = snapshotBranch
    configuration.snapshotTool = snapshotTool
    configuration.workspace = workspace
    configuration.swiftDirectory = swiftDir
    configuration.buildDirectory = buildDir
    configuration.outputDirectory = out
    configuration.extraFlags = extra
    configuration.onFlags = onFlags
    configuration.sdk = sdk
    configuration.skipBuild = skipBuild
    configuration.copyLibraries = copyLib

    do {
      try await ComparisonRun(configuration: configuration).run()
    } catch let failure as ComparisonRun.Failure {
      FileHandle.standardError.write(Data("error: \(failure)\n".utf8))
      throw ExitCode.failure
    }
  }
}

// MARK: - Argument conformances

// The library defines init?(argument:) for each of these, which is the whole
// requirement.
extension Statistic: ExpressibleByArgument {}
extension Column: ExpressibleByArgument {}
extension Column.Pair: ExpressibleByArgument {}
extension Configuration.VersionControl: ExpressibleByArgument {}
extension Configuration.Snapshot: ExpressibleByArgument {}
extension Configuration.FrontendFlags: ExpressibleByArgument {}
