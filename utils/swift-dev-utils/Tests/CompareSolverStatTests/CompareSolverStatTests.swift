//===--- CompareSolverStatTests.swift -------------------------------------===//
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
import Testing
import Utils

@testable import CompareSolverStat

/// A directory that exists for the lifetime of the value.
struct TemporaryDirectory: ~Copyable {
  let path: FilePath

  init() throws {
    path = FilePath(NSTemporaryDirectory()).appending(UUID().uuidString)
    try FileManager.default.createDirectory(
      atPath: path.string, withIntermediateDirectories: true)
  }

  deinit {
    try? FileManager.default.removeItem(atPath: path.string)
  }
}

extension TemporaryDirectory {
  var root: AbsolutePath {
    AbsolutePath(path)
  }

  /// Writes an executable shell script at a path below the directory.
  @discardableResult
  func script(at relativePath: String, _ body: String) throws -> AbsolutePath {
    let script = root.appending(relativePath)
    try script.write("#!/bin/sh\n\(body)")
    try script.chmod(.executable)
    return script
  }
}

@Suite
struct ConfigurationTests {
  @Test
  func scopeStatisticIsDefault() {
    #expect(Configuration().effectiveStatistics == [.constraintScopes])
  }

  @Test
  func scopeStatisticIsAlwaysIncluded() {
    var configuration = Configuration()
    configuration.statistics = [Statistic(name: "NumLeafScopes")]
    #expect(
      configuration.effectiveStatistics == [
        .constraintScopes, Statistic(name: "NumLeafScopes"),
      ])
  }

  @Test
  func scopeStatisticIsNotRepeated() {
    var configuration = Configuration()
    configuration.statistics = [
      Statistic(name: "NumLeafScopes"), .constraintScopes,
    ]
    #expect(
      configuration.effectiveStatistics == [
        Statistic(name: "NumLeafScopes"), .constraintScopes,
      ])
  }

  @Test
  func defaultBaselineAndPair() {
    let configuration = Configuration()
    #expect(configuration.effectiveBaseline.description == "main-off")
    #expect(
      configuration.effectivePairs.map(\.description) == ["wip-on vs main-on"])
  }

  @Test
  func frontendFlagsSplitAtWhitespace() throws {
    let flags = try #require(
      Configuration.FrontendFlags(argument: "  -a  -b\t-c "))
    #expect(flags.arguments == ["-a", "-b", "-c"])
  }

  @Test
  func snapshotParses() throws {
    let snapshot = try #require(
      Configuration.Snapshot(argument: "snap1=2026-01-05"))
    #expect(snapshot.name == "snap1")
    #expect(snapshot.day.description == "2026-01-05")
  }

  @Test(arguments: [
    "snap1", "=2026-01-05", "snap1=", "snap1=2026-1-5", "snap1=2026-13-01",
    "snap1=2026-01-32", "snap1=yesterday",
  ])
  func snapshotRejects(_ argument: String) {
    #expect(Configuration.Snapshot(argument: argument) == nil)
  }
}

@Suite
struct ColumnTests {
  /// A column argument and the column it should parse to.
  struct ParseCase: Sendable {
    var argument: String
    var expected: Column
  }

  static let parseCases = [
    ParseCase(
      argument: "main-off", expected: Column(compiler: "main", state: .off)),
    ParseCase(
      argument: "wip-on", expected: Column(compiler: "wip", state: .on)),
    ParseCase(
      argument: "main", expected: Column(compiler: "main", state: .off)),
    ParseCase(
      argument: "snap-1-on", expected: Column(compiler: "snap-1", state: .on)),
    ParseCase(
      argument: "snap-1", expected: Column(compiler: "snap-1", state: .off)),
  ]

  @Test(arguments: parseCases)
  func parses(_ parseCase: ParseCase) throws {
    let column = try #require(Column(argument: parseCase.argument))
    #expect(column == parseCase.expected)
  }

  @Test(arguments: ["", "-off", "-on"])
  func rejectsEmptyCompiler(_ argument: String) {
    #expect(Column(argument: argument) == nil)
  }

  @Test
  func pairParses() throws {
    let pair = try #require(Column.Pair(argument: "wip-on:main"))
    #expect(pair.description == "wip-on vs main-off")
  }

  @Test(arguments: ["wip-on", "wip-on:", ":main-on"])
  func pairRejects(_ argument: String) {
    #expect(Column.Pair(argument: argument) == nil)
  }
}

@Suite
struct StatisticTests {
  @Test
  func matchesKeysWithAndWithoutGroup() {
    #expect(Statistic.constraintScopes.matches(key: "Sema.NumConstraintScopes"))
    #expect(Statistic.constraintScopes.matches(key: "NumConstraintScopes"))
  }

  @Test
  func doesNotMatchOtherStatistics() {
    #expect(
      !Statistic.constraintScopes.matches(key: "Sema.NumConstraintScopesExtra"))
    #expect(!Statistic(name: "Scopes").matches(key: "Sema.NumConstraintScopes"))
  }
}

@Suite
struct RunLineTextTests {
  @Test
  func readsTheCommentBlockAtTheTop() {
    let source = """
      // A test of something.
      // RUN: %target-typecheck-verify-swift \\
      // RUN:   -solver-scope-threshold=10

      //RUN: %target-swift-frontend -typecheck %s
      // REQUIRES: objc_interop
      func f() {}
      // RUN: not part of the block
      """
    #expect(
      TestFile.runLineTexts(in: source) == [
        "%target-typecheck-verify-swift  -solver-scope-threshold=10",
        "%target-swift-frontend -typecheck %s",
      ])
  }

  @Test
  func sourceWithoutRunLines() {
    #expect(TestFile.runLineTexts(in: "func f() {}\n").isEmpty)
  }

  @Test
  func windowsLineEndingsReadLikeUnixOnes() {
    let unix = """
      // RUN: %empty-directory(%t)
      // RUN: %target-typecheck-verify-swift \\
      // RUN:   -solver-scope-threshold=10
      // RUN: %target-swift-frontend -typecheck %s
      func f() {}

      """
    let windows = unix.replacingOccurrences(of: "\n", with: "\r\n")
    #expect(
      TestFile.runLineTexts(in: windows) == TestFile.runLineTexts(in: unix))
    #expect(TestFile.runLineTexts(in: windows).count == 3)
  }
}

@Suite
struct RunLineTests {
  @Test
  func verifySubstitution() throws {
    let line = try TestFile.RunLine(
      parsing: "%target-typecheck-verify-swift -solver-scope-threshold=1000",
      targetCPU: "arm64")
    #expect(
      line.arguments == [
        .text("-typecheck"),
        .text("-disable-objc-attr-requires-foundation-module"),
      ])
    #expect(line.scopeThreshold == 1000)
  }

  @Test
  func primaryFileKeepsTheTestFile() throws {
    let line = try TestFile.RunLine(
      parsing:
        "%target-swift-frontend -typecheck -verify -verify-ignore-unrelated -primary-file %s",
      targetCPU: "arm64")
    #expect(
      line.arguments == [
        .text("-typecheck"), .text("-primary-file"), .testFile,
      ])
    #expect(line.namesTestFile)
    #expect(line.scopeThreshold == nil)
  }

  @Test
  func pipelineNotAndRedirectionAreDropped() throws {
    let line = try TestFile.RunLine(
      parsing:
        "not %target-swift-frontend -typecheck %s -solver-scope-threshold=100 -diagnostic-style llvm 2>&1 | %FileCheck %s",
      targetCPU: "arm64")
    #expect(
      line.arguments == [
        .text("-typecheck"), .text("-diagnostic-style"), .text("llvm"),
      ])
    #expect(line.scopeThreshold == 100)
    #expect(!line.namesTestFile)
  }

  @Test
  func targetCPUIsReplaced() throws {
    let line = try TestFile.RunLine(
      parsing:
        "%target-typecheck-verify-swift -target %target-cpu-apple-macosx12 -swift-version 5",
      targetCPU: "arm64")
    #expect(
      line.arguments == [
        .text("-typecheck"),
        .text("-disable-objc-attr-requires-foundation-module"),
        .text("-target"), .text("arm64-apple-macosx12"),
        .text("-swift-version"), .text("5"),
      ])
  }

  @Test
  func typecheckIsAddedWhenMissing() throws {
    let line = try TestFile.RunLine(
      parsing: "%target-swift-frontend %s -solver-scope-threshold=2000",
      targetCPU: "arm64")
    #expect(line.arguments == [.text("-typecheck")])
    #expect(line.scopeThreshold == 2000)
  }

  /// A RUN line and the reason it cannot be run.
  struct UnsupportedCase: Sendable {
    var text: String
    var reason: TestFile.RunLine.Unsupported
  }

  static let unsupportedCases = [
    UnsupportedCase(
      text: "%empty-directory(%t)", reason: .runs("%empty-directory(%t)")),
    UnsupportedCase(text: "split-file %s %t", reason: .runs("split-file")),
    UnsupportedCase(
      text:
        "%target-swift-frontend(mock-sdk: %clang-importer-sdk) %s -typecheck",
      reason: .runs("%target-swift-frontend(mock-sdk:")),
    UnsupportedCase(
      text: "%target-swift-frontend -typecheck %t/Lib.swift",
      reason: .substitution("%t/Lib.swift")),
    UnsupportedCase(text: "", reason: .empty),
    UnsupportedCase(text: "not 2>&1 | %FileCheck %s", reason: .empty),
  ]

  @Test(arguments: unsupportedCases)
  func unsupported(_ unsupportedCase: UnsupportedCase) {
    #expect(throws: unsupportedCase.reason) {
      try TestFile.RunLine(parsing: unsupportedCase.text, targetCPU: "arm64")
    }
  }

  @Test
  func unterminatedQuoteIsUnparsable() {
    #expect(
      throws: TestFile.RunLine.Unsupported.unparsable(
        "unterminated string literal in command line")
    ) {
      try TestFile.RunLine(
        parsing: #"%target-swift-frontend -module-name "Foo"#,
        targetCPU: "arm64")
    }
  }
}

@Suite
struct PlanTests {
  static let file = TestFile(path: "/tests/fast/a.swift", name: "fast/a.swift")

  static func plan(_ texts: [String]) -> TestFile.Plan {
    var plan = TestFile.Plan()
    plan.add(file, runLineTexts: texts, targetCPU: "arm64")
    return plan
  }

  @Test
  func singleLineKeepsThePlainName() {
    #expect(
      Self.plan(["%target-typecheck-verify-swift"]).runs.map(\.name.description)
        == ["fast/a.swift"])
  }

  @Test
  func severalLinesAreNumbered() {
    let plan = Self.plan([
      "%target-typecheck-verify-swift",
      "%target-typecheck-verify-swift -swift-version 5",
    ])
    #expect(
      plan.runs.map(\.name.description) == ["fast/a.swift#1", "fast/a.swift#2"])
  }

  @Test
  func skippedLineKeepsItsNumber() {
    let plan = Self.plan(["split-file %s %t", "%target-typecheck-verify-swift"])
    #expect(plan.runs.map(\.name.description) == ["fast/a.swift#2"])
    #expect(
      plan.skippedLines.map(\.description) == [
        "fast/a.swift#1: skipped, it runs split-file: split-file %s %t"
      ])
  }

  @Test
  func fileWithoutRunLinesIsTypechecked() throws {
    let run = try #require(Self.plan([]).runs.first)
    #expect(run.name.description == "fast/a.swift")
    #expect(
      run.line
        == TestFile.RunLine(
          arguments: [.text("-typecheck")], scopeThreshold: nil))
  }

  @Test
  func inputAndArguments() throws {
    let plan = Self.plan([
      "%target-swift-frontend -typecheck -primary-file %s",
      "%target-swift-frontend -typecheck %s",
    ])
    let primary = try #require(plan.runs.first)
    #expect(primary.input == nil)
    #expect(
      primary.lineArguments == [
        "-typecheck", "-primary-file", "/tests/fast/a.swift",
      ])
    let plain = try #require(plan.runs.last)
    #expect(plain.input == Self.file.path)
    #expect(plain.lineArguments == ["-typecheck"])
  }
}

@Suite
struct TestFileDiscoveryTests {
  @Test
  func findsSwiftFilesSortedByComponent() throws {
    let tmp = try TemporaryDirectory()
    let root = AbsolutePath(tmp.path)
    for name in [
      "b.swift", "a/x.swift", "a-b/x.swift", "c.swift.gyb", "notes.txt",
    ] {
      let path = root.appending(name)
      try path.parentDir?.makeDir()
      try path.write("")
    }
    // A directory with a .swift name is not a test file.
    try root.appending("d.swift").makeDir()
    let names = try TestFile.all(in: root).map(\.name.description)
    #expect(names == ["a/x.swift", "a-b/x.swift", "b.swift"])
  }

  @Test
  func missingDirectoryFails() throws {
    let tmp = try TemporaryDirectory()
    let missing = AbsolutePath(tmp.path).appending("missing")
    #expect(throws: ComparisonRun.Failure.self) {
      try TestFile.all(in: missing)
    }
  }

  @Test
  func directoryWithoutSwiftFilesFails() throws {
    let tmp = try TemporaryDirectory()
    let empty = AbsolutePath(tmp.path)
    #expect(throws: ComparisonRun.Failure.self) {
      try TestFile.all(in: empty)
    }
  }
}

@Suite
struct CountersTests {
  @Test
  func sumsWholeNumbersOverEveryStatsFile() throws {
    let tmp = try TemporaryDirectory()
    let directory = AbsolutePath(tmp.path)
    try directory.appending("one.json").write(
      #"{"Sema.NumConstraintScopes": 3, "time.swift.Parse": 0.5}"#)
    try directory.appending("two.json").write(
      #"{"Sema.NumConstraintScopes": 4, "Sema.NumLeafScopes": 2}"#)
    try directory.appending("notes.txt").write("not json")
    let counters = try #require(try Statistic.Counters(directory: directory))
    #expect(counters.value(of: .constraintScopes) == 7)
    #expect(counters.value(of: Statistic(name: "NumLeafScopes")) == 2)
    #expect(counters.values["time.swift.Parse"] == nil)
  }

  @Test
  func missingStatisticIsZero() {
    let counters = Statistic.Counters(values: ["Sema.NumLeafScopes": 2])
    #expect(counters.value(of: .constraintScopes) == 0)
  }

  @Test
  func directoryWithoutStatsFilesHasNoCounters() throws {
    let tmp = try TemporaryDirectory()
    let directory = AbsolutePath(tmp.path)
    #expect(try Statistic.Counters(directory: directory) == nil)
    #expect(
      try Statistic.Counters(directory: directory.appending("missing")) == nil)
  }
}

/// Builds comparisons from scope counts, as a run of the tool would.
struct ComparisonFixture {
  static let mainOff = Column(compiler: "main", state: .off)
  static let mainOn = Column(compiler: "main", state: .on)
  static let wipOff = Column(compiler: "wip", state: .off)
  static let wipOn = Column(compiler: "wip", state: .on)
  static let columns = [mainOff, mainOn, wipOff, wipOn]

  static func run(_ name: RelativePath, threshold: Int?) -> TestFile.Run {
    TestFile.Run(
      file: TestFile(path: AbsolutePath("/tests").appending(name), name: name),
      index: nil,
      line: TestFile.RunLine(
        arguments: [.text("-typecheck")], scopeThreshold: threshold))
  }

  static func scopes(_ value: Int) -> Comparison.Result {
    .measured(Statistic.Counters(values: ["Sema.NumConstraintScopes": value]))
  }

  static func settings() -> Report.Settings {
    var settings = Report.Settings(
      baseline: mainOff,
      pairs: [Column.Pair(measured: wipOn, reference: mainOn)],
      statistics: [.constraintScopes], date: Date(),
      commandLine: ["compare-solver-stat"],
      testSource: AnyPath(FilePath("/tests")))
    settings.mainPosition = "af474300fcd0"
    settings.wipPosition = "an empty change on 588ffd59"
    return settings
  }
}

@Suite
struct ComparisonCountsTests {
  @Test
  func countsEachOutcome() {
    let measured = ComparisonFixture.wipOn
    let reference = ComparisonFixture.mainOn
    let rows = [
      Comparison.Row(
        run: ComparisonFixture.run("a.swift", threshold: 11),
        results: [
          measured: ComparisonFixture.scopes(10),
          reference: ComparisonFixture.scopes(12),
        ]),
      Comparison.Row(
        run: ComparisonFixture.run("b.swift", threshold: 11),
        results: [
          measured: ComparisonFixture.scopes(12),
          reference: ComparisonFixture.scopes(10),
        ]),
      Comparison.Row(
        run: ComparisonFixture.run("c.swift", threshold: nil),
        results: [
          measured: ComparisonFixture.scopes(5),
          reference: ComparisonFixture.scopes(5),
        ]),
      Comparison.Row(
        run: ComparisonFixture.run("d.swift", threshold: 11),
        results: [measured: .skipped, reference: ComparisonFixture.scopes(5)]),
      Comparison.Row(
        run: ComparisonFixture.run("e.swift", threshold: 11),
        results: [measured: ComparisonFixture.scopes(5)]),
    ]
    let counts = Comparison.Counts(
      statistic: .constraintScopes, measured: measured, reference: reference,
      rows: rows)
    var expected = Comparison.Counts()
    expected.runs = 5
    expected.smaller = 1
    expected.larger = 1
    expected.same = 1
    expected.skipped = 1
    expected.failed = 1
    expected.over = 1
    expected.newlyOver = 1
    expected.newlyUnder = 1
    #expect(counts == expected)
  }

  @Test
  func thresholdsOnlyCountForTheScopeStatistic() {
    let measured = ComparisonFixture.wipOn
    let reference = ComparisonFixture.mainOn
    let counters = Statistic.Counters(values: ["Sema.NumLeafScopes": 20])
    let rows = [
      Comparison.Row(
        run: ComparisonFixture.run("a.swift", threshold: 11),
        results: [
          measured: .measured(counters), reference: .measured(counters),
        ])
    ]
    let counts = Comparison.Counts(
      statistic: Statistic(name: "NumLeafScopes"), measured: measured,
      reference: reference, rows: rows)
    #expect(counts.same == 1)
    #expect(counts.over == 0)
  }
}

@Suite
struct ReportTests {
  static func twoRunComparison() -> Comparison {
    let results: [Column: Comparison.Result] = [
      ComparisonFixture.mainOff: ComparisonFixture.scopes(70),
      ComparisonFixture.mainOn: ComparisonFixture.scopes(60),
      ComparisonFixture.wipOff: ComparisonFixture.scopes(60),
      ComparisonFixture.wipOn: ComparisonFixture.scopes(50),
    ]
    return Comparison(
      columns: ComparisonFixture.columns,
      rows: [
        Comparison.Row(
          run: ComparisonFixture.run("a.swift", threshold: 65), results: results
        ),
        Comparison.Row(
          run: ComparisonFixture.run("x/b.swift", threshold: 65),
          results: results),
      ])
  }

  /// The expected report.md of the two-run comparison, from the columns line
  /// on. The lines before it hold the date and the command, which change
  /// from run to run.
  static let expectedMarkdown = """
    - columns: main-off, main-on, wip-off, wip-on
    - main: af474300fcd0
    - wip: an empty change on 588ffd59
    - percentages are relative to the baseline, main-off
    - ▼ smaller  ▲ larger  ± mixed (compared with the baseline)

    ## Runs

    ```text
       run        threshold  main-off  main-on            wip-off            wip-on
    ▼  a.swift    65         70 over   60 (-14.3%) under  60 (-14.3%) under  50 (-28.6%) under
    ▼  x/b.swift  65         70 over   60 (-14.3%) under  60 (-14.3%) under  50 (-28.6%) under
       total                 140       120 (-14.3%)       120 (-14.3%)       100 (-28.6%)
    ```

    ## NumConstraintScopes: measured vs baseline main-off, 2 runs

    ```text
                         smaller  larger  same  fail  skip  |  over  newly over  newly under
    main-off (baseline)        -       -     -     0     0  |     2           -            -
    main-on                    2       0     0     0     0  |     0           0            2
    wip-off                    2       0     0     0     0  |     0           0            2
    wip-on                     2       0     0     0     0  |     0           0            2
    ```

    ## NumConstraintScopes: measured vs reference, 2 runs

    ```text
                       smaller  larger  same  fail  skip  |  over  newly over  newly under
    wip-on vs main-on        2       0     0     0     0  |     0           0            0
    ```

    A threshold limits one expression, but NumConstraintScopes counts the whole file, so 'over' can overstate a file with many expressions.
    """

  @Test
  func markdownHasTheExpectedContent() throws {
    let report = try Report(
      comparison: Self.twoRunComparison(),
      settings: ComparisonFixture.settings())
    let columnsLine = try #require(
      report.markdownLines.firstIndex { $0.hasPrefix("- columns:") })
    #expect(
      report.markdownLines[columnsLine...].joined(separator: "\n")
        == Self.expectedMarkdown)
    #expect(report.markdownLines.first == "# compare-solver-stat report")
  }

  @Test
  func terminalHasTheTablesWithoutMarkdown() throws {
    let report = try Report(
      comparison: Self.twoRunComparison(),
      settings: ComparisonFixture.settings())
    #expect(Array(report.terminalLines.prefix(3)) == ["", "========", ""])
    #expect(
      !report.terminalLines.contains {
        $0.hasPrefix("##") || $0.hasPrefix("```") || $0.hasPrefix("- date:")
      })
    #expect(
      report.terminalLines.contains(
        "NumConstraintScopes: measured vs reference, 2 runs"))
    #expect(!report.terminalLines.contains("Runs"))
  }

  @Test
  func colourOnlyInTheTerminal() throws {
    var settings = ComparisonFixture.settings()
    settings.useColor = true
    let report = try Report(
      comparison: Self.twoRunComparison(), settings: settings)
    #expect(report.terminalLines.contains { $0.contains("\u{1B}[") })
    #expect(!report.markdownLines.contains { $0.contains("\u{1B}[") })
  }

  @Test
  func skippedAndFailedCellsAndOnlyChanged() throws {
    let unchanged: [Column: Comparison.Result] = [
      ComparisonFixture.mainOff: ComparisonFixture.scopes(7),
      ComparisonFixture.mainOn: ComparisonFixture.scopes(7),
      ComparisonFixture.wipOff: ComparisonFixture.scopes(7),
      ComparisonFixture.wipOn: ComparisonFixture.scopes(7),
    ]
    let partial: [Column: Comparison.Result] = [
      ComparisonFixture.mainOff: ComparisonFixture.scopes(9),
      ComparisonFixture.mainOn: .skipped,
      ComparisonFixture.wipOff: ComparisonFixture.scopes(8),
      ComparisonFixture.wipOn: .failed,
    ]
    let comparison = Comparison(
      columns: ComparisonFixture.columns,
      rows: [
        Comparison.Row(
          run: ComparisonFixture.run("same.swift", threshold: nil),
          results: unchanged),
        Comparison.Row(
          run: ComparisonFixture.run("partial.swift", threshold: nil),
          results: partial),
      ])
    var settings = ComparisonFixture.settings()
    settings.onlyChanged = true
    let report = try Report(comparison: comparison, settings: settings)
    let lines = report.markdownLines
    #expect(!lines.contains { $0.contains("same.swift") })
    let partialLine = try #require(lines.first { $0.contains("partial.swift") })
    #expect(partialLine.contains("skip"))
    #expect(partialLine.contains("fail"))
    #expect(partialLine.contains("8 (-11.1%)"))
    // Only the fully measured run counts towards the totals.
    let totalLine = try #require(lines.first { $0.contains("total") })
    #expect(totalLine.contains("7"))
    // Without thresholds the summaries have no threshold columns.
    #expect(!lines.contains { $0.contains("newly over") })
  }

  @Test
  func byDirectoryAddsASummaryPerSubdirectory() throws {
    var settings = ComparisonFixture.settings()
    settings.byDirectory = true
    let report = try Report(
      comparison: Self.twoRunComparison(), settings: settings)
    #expect(
      report.markdownLines.contains(
        "## NumConstraintScopes: measured vs baseline main-off, 1 runs in ."))
    #expect(
      report.markdownLines.contains(
        "## NumConstraintScopes: measured vs reference, 1 runs in x"))
  }

  @Test
  func severalStatisticsAreLabelled() throws {
    var settings = ComparisonFixture.settings()
    settings.statistics = [.constraintScopes, Statistic(name: "NumLeafScopes")]
    let report = try Report(
      comparison: Self.twoRunComparison(), settings: settings)
    #expect(
      report.markdownLines.contains { $0.contains("a.swift [NumLeafScopes]") })
    #expect(
      report.markdownLines.contains {
        $0.contains("total [NumConstraintScopes]")
      })
  }

  @Test
  func pairsWithMissingColumnsAreLeftOut() throws {
    var settings = ComparisonFixture.settings()
    let missing = Column.Pair(
      measured: Column(compiler: "snap1", state: .on),
      reference: ComparisonFixture.mainOn)
    settings.pairs.append(missing)
    let report = try Report(
      comparison: Self.twoRunComparison(), settings: settings)
    #expect(report.leftOutPairs == [missing])
    #expect(!report.markdownLines.contains { $0.contains("snap1") })
  }

  @Test
  func unknownBaselineFails() {
    var settings = ComparisonFixture.settings()
    settings.baseline = Column(compiler: "snap1", state: .off)
    #expect(throws: ComparisonRun.Failure.self) {
      try Report(comparison: Self.twoRunComparison(), settings: settings)
    }
  }
}

@Suite
struct LocationTests {
  @Test
  func defaultsFollowTheSwiftDirectory() {
    var configuration = Configuration()
    configuration.swiftDirectory = AnyPath(FilePath("/work/swift"))
    #expect(
      configuration.effectiveTestDirectory.description
        == "/work/swift/validation-test/Sema/type_checker_perf")
    #expect(
      configuration.effectiveBuildDirectory.description
        == "/work/build/Ninja-RelWithDebInfoAssert/swift-macosx-arm64")
  }

  @Test
  func givenDirectoriesWin() {
    var configuration = Configuration()
    configuration.swiftDirectory = AnyPath(FilePath("/work/swift"))
    configuration.directory = AnyPath(FilePath("/tests"))
    configuration.buildDirectory = AnyPath(FilePath("/builds/here"))
    #expect(configuration.effectiveTestDirectory.description == "/tests")
    #expect(configuration.effectiveBuildDirectory.description == "/builds/here")
  }

  @Test
  func builtFromTheSwiftCheckout() {
    #expect(
      Configuration.builtFromSwiftDirectory.appending(
        "utils/swift-dev-utils/Package.swift"
      ).exists)
  }
}

@Suite
struct InvocationTests {
  @Test
  func returnsStandardOutput() async throws {
    #expect(
      try await Invocation(.at("/bin/echo"), ["hello", "world"]).output()
        == "hello world\n")
  }

  @Test
  func unsuccessfulExitFails() async {
    await #expect(throws: ComparisonRun.Failure.self) {
      try await Invocation(.at("/usr/bin/false"), []).output()
    }
  }

  @Test
  func missingProgramFails() async {
    await #expect(throws: ComparisonRun.Failure.self) {
      try await Invocation(.at("/no/such/program"), []).output()
    }
  }

  @Test
  func logHasTheCommandLineAndBothStreams() async throws {
    let tmp = try TemporaryDirectory()
    let log = tmp.root.appending("logs/run.log")
    _ = try await Invocation(
      .at("/bin/sh"), ["-c", "echo out; echo err >&2; exit 3"]
    ).run(loggingTo: log)
    let text = try String(contentsOfFile: log.rawPath, encoding: .utf8)
    #expect(text.hasPrefix("/bin/sh -c "))
    #expect(text.contains("out\n"))
    #expect(text.contains("err\n"))
  }

  @Test
  func descriptionQuotesArguments() {
    #expect(
      Invocation(.named("git"), ["log", "a b"]).description == #"git log "a b""#
    )
  }
}

@Suite
struct RepositoryTests {
  /// Runs git with an identity and without signing or hooks, so that tests do
  /// not depend on the user's git configuration.
  static func git(_ arguments: [String], in directory: AbsolutePath)
    async throws -> String
  {
    try await Invocation(
      .named("git"),
      [
        "-c", "user.name=Test", "-c", "user.email=test@example.com", "-c",
        "commit.gpgsign=false", "-c", "core.hooksPath=/dev/null",
      ] + arguments, in: directory
    ).output()
  }

  /// Makes a git repository on branch work with two commits, and returns the
  /// first commit.
  static func makeRepository(at directory: AbsolutePath) async throws -> String
  {
    _ = try await git(["init", "-b", "work"], in: directory)
    try directory.appending("file.txt").write("one\n")
    _ = try await git(["add", "file.txt"], in: directory)
    _ = try await git(["commit", "-m", "one"], in: directory)
    let first = try await git(["rev-parse", "HEAD"], in: directory)
      .trimmingCharacters(in: .whitespacesAndNewlines)
    try directory.appending("file.txt").write("two\n")
    _ = try await git(["commit", "-am", "two"], in: directory)
    return first
  }

  @Test
  func colocatedJJRefusesGitUnlessForced() throws {
    let tmp = try TemporaryDirectory()
    try tmp.root.appending(".git").makeDir()
    try tmp.root.appending(".jj").makeDir()
    let root = tmp.root
    #expect(throws: ComparisonRun.Failure.self) {
      try Repository(path: root, choice: .git, force: false)
    }
    #expect(try Repository(path: root, choice: .git, force: true).kind == .git)
    #expect(try Repository(path: root, choice: .auto, force: false).kind == .jj)
  }

  @Test
  func missingSystemFails() throws {
    let tmp = try TemporaryDirectory()
    let root = tmp.root
    #expect(throws: ComparisonRun.Failure.self) {
      try Repository(path: root, choice: .jj, force: false)
    }
    #expect(throws: ComparisonRun.Failure.self) {
      try Repository(path: root, choice: .git, force: false)
    }
  }

  @Test
  func cleanRepositoryIsReady() async throws {
    let tmp = try TemporaryDirectory()
    _ = try await Self.makeRepository(at: tmp.root)
    let repository = try Repository(path: tmp.root, choice: .git, force: false)
    #expect(try await repository.problems(allowDirty: false).isEmpty)
    #expect(try await repository.currentPosition() == .gitBranch("work"))
  }

  @Test
  func uncommittedFilesAreAProblemUnlessAllowed() async throws {
    let tmp = try TemporaryDirectory()
    _ = try await Self.makeRepository(at: tmp.root)
    try tmp.root.appending("new.txt").write("new\n")
    let repository = try Repository(path: tmp.root, choice: .git, force: false)
    #expect(
      try await repository.problems(allowDirty: false) == [.uncommittedFiles])
    #expect(try await repository.problems(allowDirty: true).isEmpty)
  }

  @Test
  func switchingAndRestoringKeepsUncommittedFiles() async throws {
    let tmp = try TemporaryDirectory()
    let first = try await Self.makeRepository(at: tmp.root)
    try tmp.root.appending("new.txt").write("new\n")
    var repository = try Repository(path: tmp.root, choice: .git, force: false)
    try await repository.switchTo(commit: first)
    #expect(try await repository.currentPosition() == .gitCommit(first))
    #expect(!tmp.root.appending("new.txt").exists)
    try await repository.restore(.gitBranch("work"))
    #expect(try await repository.currentPosition() == .gitBranch("work"))
    #expect(tmp.root.appending("new.txt").exists)
    #expect(
      try String(
        contentsOfFile: tmp.root.appending("file.txt").rawPath, encoding: .utf8)
        == "two\n")
  }

  @Test
  func positions() {
    let empty = Repository.Position.jjEmptyChange(parents: [
      "aaaaaaaaaaaaaaaa", "bbbbbbbbbbbbbbbb",
    ])
    #expect(
      empty.description == "an empty change on aaaaaaaaaaaa, bbbbbbbbbbbb")
    #expect(empty.restoreCommand == "jj new aaaaaaaaaaaaaaaa bbbbbbbbbbbbbbbb")
    #expect(
      empty.isSame(
        as: .jjEmptyChange(parents: ["bbbbbbbbbbbbbbbb", "aaaaaaaaaaaaaaaa"])))
    #expect(!empty.isSame(as: .jjChange(id: "aaaaaaaaaaaaaaaa")))
    #expect(
      Repository.Position.jjChange(id: "xyz").restoreCommand == "jj edit xyz")
    #expect(
      Repository.Position.gitBranch("work").restoreCommand
        == "git checkout work")
  }
}

@Suite
struct CompilerTests {
  /// Makes a build directory with a ninja wrapper that only reports a step,
  /// and fails when the build directory holds a file named FAIL.
  static func makeBuild(in tmp: borrowing TemporaryDirectory) throws
    -> AbsolutePath
  {
    let build = tmp.root.appending("build/swift-macosx-arm64")
    try build.appending("bin/swift-frontend").write("binary")
    try build.appending("lib/swift/host/compiler/libx.dylib").write("x")
    try tmp.script(
      at: "build/build-utils/ninja",
      """
      echo "[1/1] Linking bin/swift-frontend"
      if [ -f "$2/FAIL" ]; then echo FAILED; exit 1; fi

      """)
    return build
  }

  @Test
  func buildSavesTheCompilerWithLinkedLibraries() async throws {
    let tmp = try TemporaryDirectory()
    let build = try Self.makeBuild(in: tmp)
    let saved = tmp.root.appending("out/compilers/wip")
    let log = tmp.root.appending("out/logs/build-wip.log")
    let compiler = try await Compiler.build(
      named: "wip", buildDirectory: build, savingTo: saved, log: log,
      copyLibraries: false)
    #expect(compiler.frontend == saved.appending("bin/swift-frontend"))
    #expect(compiler.frontend.exists)
    #expect(compiler.resourceDirectory == build.appending("lib/swift"))
    #expect(saved.appending("lib").isSymlink)
    #expect(saved.appending("lib/swift/host/compiler/libx.dylib").exists)
    #expect(
      try String(contentsOfFile: log.rawPath, encoding: .utf8).contains(
        "Linking"))
  }

  @Test
  func copiedLibrariesAreNotALink() async throws {
    let tmp = try TemporaryDirectory()
    let build = try Self.makeBuild(in: tmp)
    let saved = tmp.root.appending("out/compilers/wip")
    _ = try await Compiler.build(
      named: "wip", buildDirectory: build, savingTo: saved,
      log: tmp.root.appending("out/logs/build-wip.log"), copyLibraries: true)
    #expect(!saved.appending("lib").isSymlink)
    #expect(saved.appending("lib/swift/host/compiler/libx.dylib").exists)
  }

  @Test
  func failedBuildSavesNothing() async throws {
    let tmp = try TemporaryDirectory()
    let build = try Self.makeBuild(in: tmp)
    try build.appending("FAIL").write("")
    let saved = tmp.root.appending("out/compilers/main")
    let log = tmp.root.appending("out/logs/build-main.log")
    await #expect(throws: ComparisonRun.Failure.self) {
      try await Compiler.build(
        named: "main", buildDirectory: build, savingTo: saved, log: log,
        copyLibraries: false)
    }
    #expect(!saved.appending("bin/swift-frontend").exists)
  }

  @Test
  func missingSavedCompilerFails() throws {
    let tmp = try TemporaryDirectory()
    let root = tmp.root
    #expect(throws: ComparisonRun.Failure.self) {
      try Compiler.saved(
        named: "main", in: root.appending("compilers/main"),
        resourceDirectory: root)
    }
  }

  @Test
  func eachCompilerHasTwoColumns() {
    let compiler = Compiler(
      name: "snap1", frontend: "/t/swift-frontend",
      resourceDirectory: "/t/lib/swift")
    #expect(compiler.columns.map(\.description) == ["snap1-off", "snap1-on"])
  }
}

@Suite
struct ComparisonRunTests {
  /// Saves a fake swift-frontend as an earlier build would. It writes the
  /// scope count to the stats directory, 10 fewer with
  /// -solver-enable-promote-supertypes, rejects -bad as an unknown argument,
  /// and exits with an error, as tests that expect errors do.
  static func saveFakeCompiler(
    _ name: String, scopes: Int, in tmp: borrowing TemporaryDirectory
  ) throws {
    try tmp.script(
      at: "out/compilers/\(name)/bin/swift-frontend",
      """
      n=\(scopes); stats=""; previous=""
      for argument in "$@"; do
        if [ "$argument" = "-bad" ]; then echo "error: unknown argument: '-bad'"; exit 1; fi
        if [ "$previous" = "-stats-output-dir" ]; then stats="$argument"; fi
        if [ "$argument" = "-solver-enable-promote-supertypes" ]; then n=$((n - 10)); fi
        previous="$argument"
      done
      printf '{"Sema.NumConstraintScopes": %d}' "$n" > "$stats/stats.json"
      exit 1

      """)
    try tmp.root.appending("out/compilers/\(name)/commit").write(
      "position of \(name)\n")
  }

  /// A configuration that compiles two test files with the saved fake
  /// compilers.
  static func configuration(in tmp: borrowing TemporaryDirectory) throws
    -> Configuration
  {
    for name in ["a.swift", "x/b.swift"] {
      try tmp.root.appending("tests/\(name)").write(
        "// RUN: %target-typecheck-verify-swift -solver-scope-threshold=65\n")
    }
    try saveFakeCompiler("main", scopes: 70, in: tmp)
    try saveFakeCompiler("wip", scopes: 60, in: tmp)
    var configuration = Configuration()
    configuration.directory = AnyPath(tmp.root.appending("tests"))
    configuration.outputDirectory = AnyPath(tmp.root.appending("out"))
    configuration.buildDirectory = AnyPath(tmp.root.appending("build"))
    configuration.sdk = AnyPath(tmp.root)
    configuration.skipBuild = true
    configuration.noColor = true
    configuration.onFlags = Configuration.FrontendFlags(arguments: [
      "-solver-enable-promote-supertypes"
    ])
    return configuration
  }

  @Test
  func comparesSavedCompilersAndSavesTheReport() async throws {
    let tmp = try TemporaryDirectory()
    let configuration = try Self.configuration(in: tmp)
    try await ComparisonRun(configuration: configuration).run()
    let markdown = try String(
      contentsOfFile: tmp.root.appending("out/report.md").rawPath,
      encoding: .utf8)
    #expect(
      markdown.contains("- main: position of main\n- wip: position of wip\n"))
    #expect(
      markdown.contains(
        "▼  a.swift    65         70 over   60 (-14.3%) under  60 (-14.3%) under  50 (-28.6%) under\n"
      ))
    #expect(
      markdown.contains(
        "wip-on vs main-on        2       0     0     0     0  |     0           0            0\n"
      ))
    #expect(tmp.root.appending("out/logs/wip-on/x/b.swift.log").exists)
  }

  @Test
  func unknownOnFlagLeavesOutTheOnColumns() async throws {
    let tmp = try TemporaryDirectory()
    var configuration = try Self.configuration(in: tmp)
    configuration.onFlags = Configuration.FrontendFlags(arguments: ["-bad"])
    try await ComparisonRun(configuration: configuration).run()
    let markdown = try String(
      contentsOfFile: tmp.root.appending("out/report.md").rawPath,
      encoding: .utf8)
    #expect(
      markdown.contains(
        "- left out, every run skipped (unknown option): main-on, wip-on\n"))
    #expect(!markdown.contains("measured vs reference"))
  }

  @Test
  func unknownBaselineFailsBeforeCompiling() async throws {
    let tmp = try TemporaryDirectory()
    var configuration = try Self.configuration(in: tmp)
    configuration.baseline = Column(compiler: "snap9", state: .off)
    await #expect(throws: ComparisonRun.Failure.self) {
      try await ComparisonRun(configuration: configuration).run()
    }
    #expect(!tmp.root.appending("out/stats").exists)
  }
}
