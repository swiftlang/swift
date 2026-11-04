//===--- ComparisonRun.swift ----------------------------------------------===//
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
import Utils

/// One comparison: it builds the compilers, compiles every run with each of
/// them, and reports how their statistics compare.
public struct ComparisonRun: Sendable {
  public var configuration: Configuration

  public init(configuration: Configuration) {
    self.configuration = configuration
  }
}

// MARK: - Running

extension ComparisonRun {
  /// Runs the comparison. It prints the report and, unless the configuration
  /// says otherwise, saves it as report.md in the output directory.
  ///
  /// Unless `skipBuild` is set, it builds the working directory and then
  /// main@origin, which moves the repository to main@origin and back. It
  /// refuses to start unless the repository is in a clean state.
  ///
  /// - Throws: `ComparisonRun.Failure`, with a message for the user, when the
  ///   comparison cannot go on.
  public func run() async throws {
    let outputDirectory = configuration.effectiveOutputDirectory
    let files = try testFiles()
    try checkColumnNames()
    let plan = try TestFile.Plan(files: files)
    for line in plan.skippedLines {
      Self.log(line.description)
    }
    guard !plan.runs.isEmpty else {
      throw Failure("There is nothing to run.")
    }
    Self.log(
      "\(plan.runs.count) runs from \(files.count) files, \(plan.skippedLines.count) skipped."
    )
    for stale in ["stats", "logs"] {
      outputDirectory.appending(stale).remove()
    }

    let compilers = try await collectCompilers(outputDirectory: outputDirectory)
    let sdk = await sdkPath()
    var results: [TestFile.RunName: [Column: Comparison.Result]] = [:]
    for compiler in compilers {
      for column in compiler.columns {
        Self.log("\n\(column): compiling \(plan.runs.count) runs ...")
        for run in plan.runs {
          results[run.name, default: [:]][column] = try await compile(
            run, with: compiler, as: column, sdk: sdk,
            outputDirectory: outputDirectory)
        }
      }
    }

    // A column in which every run was skipped has nothing to show.
    let allColumns = compilers.flatMap(\.columns)
    let omitted = allColumns.filter { column in
      plan.runs.allSatisfy { results[$0.name]?[column] == .skipped }
    }
    for column in omitted {
      Self.log(
        "\(column): every run was skipped, the compiler lacks an option.")
    }
    let baseline = configuration.effectiveBaseline
    if omitted.contains(baseline) {
      throw Failure("The baseline '\(baseline)' was skipped entirely.")
    }
    let columns = allColumns.filter { !omitted.contains($0) }
    guard !columns.isEmpty else {
      throw Failure("Every column was skipped.")
    }

    let comparison = Comparison(
      columns: columns,
      rows: plan.runs.map {
        Comparison.Row(run: $0, results: results[$0.name] ?? [:])
      })
    let report = try Report(
      comparison: comparison,
      settings: reportSettings(
        outputDirectory: outputDirectory, omitted: omitted))
    for pair in report.leftOutPairs {
      Self.log("Left out \(pair): a column was skipped entirely.")
    }
    print(report.terminalLines.joined(separator: "\n"))
    if configuration.saveReport {
      let path = outputDirectory.appending("report.md")
      try path.write(report.markdownLines.joined(separator: "\n") + "\n")
      Self.log("\nThe report is saved in \(path)")
    }
  }

  /// Writes a progress message to standard error.
  static func log(_ message: String) {
    FileHandle.standardError.write(Data("\(message)\n".utf8))
  }
}

// MARK: - Planning

extension ComparisonRun {
  private func testFiles() throws -> [TestFile] {
    if let file = configuration.file {
      return [try TestFile(file: file.absoluteInWorkingDir)]
    }
    return try TestFile.all(in: configuration.effectiveTestDirectory)
  }

  /// Checks the baseline and the pairs before anything is built.
  private func checkColumnNames() throws {
    let compilerNames = ["main", "wip"] + configuration.snapshots.map(\.name)
    let valid = compilerNames.flatMap { name in
      Column.State.allCases.map { Column(compiler: name, state: $0) }
    }
    let named =
      [configuration.effectiveBaseline]
      + configuration.effectivePairs.flatMap { [$0.measured, $0.reference] }
    for column in named where !valid.contains(column) {
      throw Failure(
        "'\(column)' is not a column. The columns are: \(valid.map(\.description).joined(separator: ", "))."
      )
    }
  }

  private func reportSettings(outputDirectory: AbsolutePath, omitted: [Column])
    -> Report.Settings
  {
    let testSource =
      configuration.file?.absoluteInWorkingDir
      ?? configuration.effectiveTestDirectory
    var settings = Report.Settings(
      baseline: configuration.effectiveBaseline,
      pairs: configuration.effectivePairs,
      statistics: configuration.effectiveStatistics, date: Date(),
      commandLine: CommandLine.arguments, testSource: AnyPath(testSource))
    settings.onlyChanged = configuration.onlyChanged
    settings.byDirectory = configuration.byDirectory
    settings.useColor = isatty(STDOUT_FILENO) != 0 && !configuration.noColor
    settings.mainPosition = Self.recordedPosition(
      of: "main", in: outputDirectory)
    settings.wipPosition = Self.recordedPosition(of: "wip", in: outputDirectory)
    settings.omittedColumns = omitted
    return settings
  }

  /// Where a saved compiler was built from, as written next to it.
  private static func recordedPosition(
    of compiler: String, in outputDirectory: AbsolutePath
  ) -> String? {
    let file = outputDirectory.appending("compilers/\(compiler)/commit")
    return (try? String(contentsOfFile: file.rawPath, encoding: .utf8))?
      .trimmingCharacters(in: .whitespacesAndNewlines)
  }
}

// MARK: - Compilers

extension ComparisonRun {
  /// The compilers to compare, in column order: main, wip, then the snapshots.
  private func collectCompilers(outputDirectory: AbsolutePath) async throws
    -> [Compiler]
  {
    let compilersDirectory = outputDirectory.appending("compilers")
    let resourceDirectory = configuration.effectiveBuildDirectory.appending(
      "lib/swift")
    var compilers: [Compiler]
    if configuration.skipBuild {
      compilers = try ["main", "wip"].map {
        try Compiler.saved(
          named: $0, in: compilersDirectory.appending($0),
          resourceDirectory: resourceDirectory)
      }
    } else {
      compilers = try await buildWipAndMain(
        compilersDirectory: compilersDirectory,
        logDirectory: outputDirectory.appending("logs"))
    }
    let environmentTool = ProcessInfo.processInfo.environment[
      "SWIFT_SNAPSHOT_TOOL"
    ].map { AnyPath(FilePath($0)) }
    let tool = (configuration.snapshotTool ?? environmentTool)?
      .absoluteInWorkingDir
    for snapshot in configuration.snapshots {
      compilers.append(
        try await Compiler.snapshot(
          snapshot, tool: tool,
          swiftDirectory: configuration.effectiveSwiftDirectory,
          workspace: configuration.effectiveWorkspace,
          branch: configuration.snapshotBranch))
    }
    return compilers
  }

  /// Builds the working directory, then main@origin, and moves the repository
  /// back, also when a build fails. Returns main, then wip.
  private func buildWipAndMain(
    compilersDirectory: AbsolutePath, logDirectory: AbsolutePath
  ) async throws -> [Compiler] {
    let swiftDirectory = configuration.effectiveSwiftDirectory
    let buildDirectory = configuration.effectiveBuildDirectory
    var repository = try Repository(
      path: swiftDirectory, choice: configuration.versionControl,
      force: configuration.forceVersionControl)
    var checked = [repository]
    let syntaxDirectory = swiftDirectory.dropLast().appending("swift-syntax")
    if syntaxDirectory.isDirectory {
      checked.append(
        try Repository(
          path: syntaxDirectory, choice: .auto,
          force: configuration.forceVersionControl))
      Self.log(
        "Note: swift-syntax is checked but not moved, so main is built against the swift-syntax checkout you have."
      )
    }
    try await checkReady(checked, buildDirectory: buildDirectory)

    let original = try await repository.currentPosition()
    Self.log("Recorded the current position: \(original)")
    if configuration.fetch {
      try await repository.fetch()
    }
    let mainCommit = try await repository.mainCommit()
    guard !mainCommit.isEmpty else {
      throw Failure("Could not resolve main@origin.")
    }

    Self.log("Building wip ...")
    let wip = try await Compiler.build(
      named: "wip", buildDirectory: buildDirectory,
      savingTo: compilersDirectory.appending("wip"),
      log: logDirectory.appending("build-wip.log"),
      copyLibraries: configuration.copyLibraries)
    try compilersDirectory.appending("wip/commit").write("\(original)\n")

    let main: Compiler
    do {
      Self.log("Building main (\(mainCommit)) ...")
      try await repository.switchTo(commit: mainCommit)
      main = try await Compiler.build(
        named: "main", buildDirectory: buildDirectory,
        savingTo: compilersDirectory.appending("main"),
        log: logDirectory.appending("build-main.log"),
        copyLibraries: configuration.copyLibraries)
      try compilersDirectory.appending("main/commit").write("\(mainCommit)\n")
    } catch {
      try await Self.restore(&repository, to: original)
      throw error
    }
    try await Self.restore(&repository, to: original)
    return [main, wip]
  }

  /// Moves the repository back. When that fails, it tells the user how to do
  /// it by hand.
  private static func restore(
    _ repository: inout Repository, to position: Repository.Position
  ) async throws {
    log("Restoring the original position ...")
    do {
      try await repository.restore(position)
    } catch {
      log(
        "\(error)\nRestore the repository by hand: \(position.restoreCommand)")
      throw error
    }
  }

  /// Checks that the build tools exist and that every repository is safe to
  /// move away from.
  private func checkReady(
    _ repositories: [Repository], buildDirectory: AbsolutePath
  ) async throws {
    var problems: [String] = []
    let ninja = Compiler.ninjaWrapper(for: buildDirectory)
    if !ninja.exists {
      problems.append("the cached-build ninja wrapper \(ninja) does not exist")
    }
    if !buildDirectory.isDirectory {
      problems.append("the build directory \(buildDirectory) does not exist")
    }
    for repository in repositories {
      for problem in try await repository.problems(
        allowDirty: configuration.allowDirty)
      {
        problems.append("\(repository.path.fileName): \(problem)")
      }
    }
    guard problems.isEmpty else {
      throw Failure(
        """
        The repository is not in a state the tool can safely switch away from:
        \(problems.map { "  - \($0)" }.joined(separator: "\n"))
        Put the repository into a good state and run the tool again.
        """)
    }
  }

  private func sdkPath() async -> AbsolutePath? {
    if let sdk = configuration.sdk {
      return sdk.absoluteInWorkingDir
    }
    guard
      let output = try? await Invocation(.named("xcrun"), ["--show-sdk-path"])
        .output()
    else {
      return nil
    }
    let path = output.trimmingCharacters(in: .whitespacesAndNewlines)
    return path.isEmpty ? nil : AnyPath(FilePath(path)).absoluteInWorkingDir
  }
}

// MARK: - Compiling

extension ComparisonRun {
  /// Compiles one run in one column and returns what came of it. A non-zero
  /// exit is normal, because the tests expect errors. The run counts as
  /// skipped when the compiler rejects an argument as unknown, and as failed
  /// when it writes no statistics for any other reason.
  private func compile(
    _ run: TestFile.Run, with compiler: Compiler, as column: Column,
    sdk: AbsolutePath?, outputDirectory: AbsolutePath
  ) async throws -> Comparison.Result {
    let statsDirectory = outputDirectory.appending(
      "stats/\(column)/\(run.name)")
    try statsDirectory.makeDir()
    var arguments: [String] = []
    if let input = run.input {
      arguments.append(input.rawPath)
    }
    arguments += [
      "-stats-output-dir", statsDirectory.rawPath, "-resource-dir",
      compiler.resourceDirectory.rawPath,
    ]
    if let sdk {
      arguments += ["-sdk", sdk.rawPath]
    }
    // The RUN line's arguments come last, so they win over the column's flags.
    let columnFlags =
      column.state == .on ? configuration.effectiveOnFlags.arguments : []
    arguments +=
      configuration.effectiveExtraFlags.arguments + columnFlags
      + run.lineArguments

    let log = outputDirectory.appending("logs/\(column)/\(run.name).log")
    _ = try await Invocation(.at(compiler.frontend), arguments).run(
      loggingTo: log)
    if let counters = try Statistic.Counters(directory: statsDirectory) {
      return .measured(counters)
    }
    let output = String(decoding: try log.read(), as: UTF8.self)
    return output.contains("unknown argument") ? .skipped : .failed
  }
}

// MARK: - Failure

extension ComparisonRun {
  /// An error whose message is ready to show to the user.
  public struct Failure: Error, Sendable {
    public var message: String

    public init(_ message: String) {
      self.message = message
    }
  }
}

extension ComparisonRun.Failure: CustomStringConvertible {
  public var description: String {
    message
  }
}
