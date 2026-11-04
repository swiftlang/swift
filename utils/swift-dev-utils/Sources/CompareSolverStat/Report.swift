//===--- Report.swift -----------------------------------------------------===//
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

public import Foundation
import System
public import Utils

/// The report of a comparison, both as text for the terminal and as a
/// markdown document.
public struct Report: Sendable {
  /// The lines to print. They carry colour codes when the settings ask for
  /// colour.
  public var terminalLines: [String] = []
  /// The lines of report.md.
  public var markdownLines: [String] = []
  /// Pairs left out of the pair table because one of their columns is not in
  /// the comparison.
  public var leftOutPairs: [Column.Pair] = []

  public init() {}
}

// MARK: - Settings

extension Report {
  /// What the report shows, apart from the comparison itself.
  public struct Settings: Sendable {
    /// The column the percentages and the summary are relative to.
    public var baseline: Column
    /// The rows of the pair table.
    public var pairs: [Column.Pair]
    public var statistics: [Statistic]
    /// Hide the rows where no column differs from the baseline.
    public var onlyChanged = false
    /// Also show the summaries for each subdirectory.
    public var byDirectory = false
    /// Colour the changed cells in the terminal lines.
    public var useColor = false
    /// When the comparison ran.
    public var date: Date
    /// The arguments the tool was run with, starting with its name.
    public var commandLine: [String]
    /// The file or directory the runs come from.
    public var testSource: AnyPath
    /// Where main was built from, when it is known.
    public var mainPosition: String?
    /// Where wip was built from, when it is known.
    public var wipPosition: String?
    /// Columns left out of the comparison because they skipped every run.
    public var omittedColumns: [Column] = []

    public init(
      baseline: Column, pairs: [Column.Pair], statistics: [Statistic],
      date: Date, commandLine: [String], testSource: AnyPath
    ) {
      self.baseline = baseline
      self.pairs = pairs
      self.statistics = statistics
      self.date = date
      self.commandLine = commandLine
      self.testSource = testSource
    }
  }
}

// MARK: - Building

extension Report {
  /// Builds the report of a comparison. Pairs whose columns are not in the
  /// comparison are left out and listed in `leftOutPairs`.
  ///
  /// - Throws: `ComparisonRun.Failure` when the baseline is not one of the
  ///   comparison's columns.
  public init(comparison: Comparison, settings: Settings) throws {
    guard comparison.columns.contains(settings.baseline) else {
      let names = comparison.columns.map(\.description).joined(separator: ", ")
      throw ComparisonRun.Failure(
        "The baseline '\(settings.baseline)' is not one of \(names).")
    }
    self.init()
    let columns = comparison.columns
    let pairs = settings.pairs.filter {
      columns.contains($0.measured) && columns.contains($0.reference)
    }
    leftOutPairs = settings.pairs.filter { !pairs.contains($0) }

    addHeader(comparison: comparison, settings: settings)
    addRunTable(comparison: comparison, settings: settings)
    addSummaries(
      of: comparison.rows, suffix: "", columns: columns, pairs: pairs,
      settings: settings)
    if settings.byDirectory {
      let groups = Dictionary(grouping: comparison.rows) { row in
        // The first component of the run's path, or . for a file directly in
        // the test directory.
        let components = row.run.name.file.components
        return components.count > 1 ? components.first?.string ?? "." : "."
      }
      if groups.count > 1 {
        for group in groups.keys.sorted() {
          addSummaries(
            of: groups[group] ?? [], suffix: " in \(group)", columns: columns,
            pairs: pairs, settings: settings)
        }
      } else {
        addNote("--by-dir: there are no subdirectories to break down.")
      }
    }
    addNote(
      """
      A threshold limits one expression, but NumConstraintScopes counts the \
      whole file, so 'over' can overstate a file with many expressions.
      """)
  }
}

// MARK: - Sections

extension Report {
  private mutating func addHeader(comparison: Comparison, settings: Settings) {
    terminalLines += ["", "========", ""]
    markdownLines += ["# compare-solver-stat report", ""]
    let formatter = DateFormatter()
    formatter.locale = Locale(identifier: "en_US_POSIX")
    formatter.dateFormat = "yyyy-MM-dd HH:mm"
    addMeta("date: \(formatter.string(from: settings.date))")
    let command = settings.commandLine.map {
      Command.Argument.value($0).printed
    }.joined(separator: " ")
    addMeta("command: `\(command)`")
    addMeta(
      "test directory: \(settings.testSource), \(comparison.rows.count) runs")
    addMeta(
      "columns: \(comparison.columns.map(\.description).joined(separator: ", "))"
    )
    if let mainPosition = settings.mainPosition {
      addFact("main: \(mainPosition)")
    }
    if let wipPosition = settings.wipPosition {
      addFact("wip: \(wipPosition)")
    }
    addFact("percentages are relative to the baseline, \(settings.baseline)")
    addFact("▼ smaller  ▲ larger  ± mixed (compared with the baseline)")
    if !settings.omittedColumns.isEmpty {
      let omitted = settings.omittedColumns.map(\.description).joined(
        separator: ", ")
      addFact("left out, every run skipped (unknown option): \(omitted)")
    }
  }

  private mutating func addRunTable(comparison: Comparison, settings: Settings)
  {
    let columns = comparison.columns
    let baseline = settings.baseline
    let labelsStatistic = settings.statistics.count > 1
    var table: [[TextCell]] = [
      [TextCell(""), TextCell("run"), TextCell("threshold")]
        + columns.map { TextCell($0.description) }
    ]
    var totals: [Statistic: [Column: Int]] = [:]
    for row in comparison.rows {
      for statistic in settings.statistics {
        let base = row.value(of: statistic, in: baseline)
        let threshold =
          statistic == .constraintScopes ? row.run.line.scopeThreshold : nil
        var cells: [TextCell] = []
        var values: [Column: Int] = [:]
        for column in columns {
          let value = row.value(of: statistic, in: column)
          values[column] = value
          var cell =
            row.result(in: column) == .skipped
            ? TextCell("skip")
            : Self.cell(
              value: value, base: base, isBaseline: column == baseline)
          if let threshold, let value {
            cell.text += value > threshold ? " over" : " under"
          }
          cells.append(cell)
        }
        // Only runs that every column measured count towards the totals.
        if values.count == columns.count {
          for (column, value) in values {
            totals[statistic, default: [:]][column, default: 0] += value
          }
        }
        let directions = Set(cells.compactMap(\.direction))
        if settings.onlyChanged && directions.isEmpty {
          continue
        }
        let label =
          labelsStatistic
          ? "\(row.run.name) [\(statistic)]" : row.run.name.description
        table.append(
          [
            TextCell(Self.marker(for: directions)), TextCell(label),
            TextCell(threshold.map(Self.grouped) ?? ""),
          ] + cells)
      }
    }
    for statistic in settings.statistics {
      let label = labelsStatistic ? "total [\(statistic)]" : "total"
      let base = totals[statistic]?[baseline] ?? 0
      // The totals are not coloured.
      let cells = columns.map { column in
        TextCell(
          Self.cell(
            value: totals[statistic]?[column] ?? 0, base: base,
            isBaseline: column == baseline
          ).text)
      }
      table.append([TextCell(" "), TextCell(label), TextCell("")] + cells)
    }
    addSection("Runs", showInTerminal: false)
    addBlock(
      Self.leftAlignedLines(table, useColor: false),
      colored: settings.useColor
        ? Self.leftAlignedLines(table, useColor: true) : nil)
  }

  private mutating func addSummaries(
    of rows: [Comparison.Row], suffix: String, columns: [Column],
    pairs: [Column.Pair], settings: Settings
  ) {
    let baseline = settings.baseline
    for statistic in settings.statistics {
      let withThresholds =
        statistic == .constraintScopes
        && rows.contains { $0.run.line.scopeThreshold != nil }
      let baselineRows = columns.map { column in
        MatrixRow(
          label: column == baseline
            ? "\(column) (baseline)" : column.description,
          counts: Comparison.Counts(
            statistic: statistic, measured: column, reference: baseline,
            rows: rows), isBaseline: column == baseline)
      }
      addSection(
        "\(statistic): measured vs baseline \(baseline), \(rows.count) runs\(suffix)"
      )
      addBlock(Self.matrixLines(baselineRows, withThresholds: withThresholds))
      if !pairs.isEmpty {
        let pairRows = pairs.map { pair in
          MatrixRow(
            label: pair.description,
            counts: Comparison.Counts(
              statistic: statistic, measured: pair.measured,
              reference: pair.reference, rows: rows), isBaseline: false)
        }
        addSection(
          "\(statistic): measured vs reference, \(rows.count) runs\(suffix)")
        addBlock(Self.matrixLines(pairRows, withThresholds: withThresholds))
      }
    }
  }
}

// MARK: - Output

extension Report {
  /// Adds a fact that only the markdown document shows.
  private mutating func addMeta(_ text: String) {
    markdownLines.append("- \(text)")
  }

  private mutating func addFact(_ text: String) {
    terminalLines.append(text)
    markdownLines.append("- \(text)")
  }

  /// Starts a section. The markdown always gets a heading; the terminal gets
  /// the title only when `showInTerminal` is set.
  private mutating func addSection(_ title: String, showInTerminal: Bool = true)
  {
    terminalLines.append("")
    if showInTerminal {
      terminalLines.append(title)
    }
    markdownLines += ["", "## \(title)", ""]
  }

  /// Adds a table. The terminal gets the coloured lines when there are any.
  private mutating func addBlock(_ plain: [String], colored: [String]? = nil) {
    terminalLines += colored ?? plain
    markdownLines += ["```text"] + plain + ["```"]
  }

  private mutating func addNote(_ text: String) {
    terminalLines += ["", text]
    markdownLines += ["", text]
  }
}

// MARK: - Cells

extension Report {
  /// Whether a value is smaller or larger than the baseline.
  private enum Direction: Sendable, Hashable {
    case smaller
    case larger
  }

  /// The text of a table cell, and how it differs from the baseline.
  private struct TextCell: Sendable {
    var text: String
    var direction: Direction?

    init(_ text: String, direction: Direction? = nil) {
      self.text = text
      self.direction = direction
    }
  }

  /// A row of a summary table.
  private struct MatrixRow: Sendable {
    var label: String
    var counts: Comparison.Counts
    /// The baseline row has nothing to compare, so its comparison counts show
    /// a dash.
    var isBaseline: Bool
  }

  /// The cell for a value: fail when there is none, and the percentage change
  /// from the baseline when it differs.
  private static func cell(value: Int?, base: Int?, isBaseline: Bool)
    -> TextCell
  {
    guard let value else {
      return TextCell("fail")
    }
    let text = grouped(value)
    guard !isBaseline, let base, value != base else {
      return TextCell(text)
    }
    let direction: Direction = value < base ? .smaller : .larger
    guard base != 0 else {
      return TextCell(text, direction: direction)
    }
    let change = String(
      format: "%+.1f", Double(value - base) / Double(base) * 100)
    return TextCell("\(text) (\(change)%)", direction: direction)
  }

  /// The marker of a run row: ▼ when every change is smaller, ▲ when every
  /// change is larger, ± when they are mixed, and blank when nothing changed.
  private static func marker(for directions: Set<Direction>) -> String {
    switch directions {
    case [.smaller]: "▼"
    case [.larger]: "▲"
    case []: " "
    default: "±"
    }
  }

  /// The value with commas between groups of three digits.
  private static func grouped(_ value: Int) -> String {
    let digits = String(value.magnitude)
    var result = value < 0 ? "-" : ""
    for (index, digit) in digits.enumerated() {
      if index > 0 && (digits.count - index) % 3 == 0 {
        result.append(",")
      }
      result.append(digit)
    }
    return result
  }
}

// MARK: - Tables

extension Report {
  /// The lines of a table whose cells are all left-aligned, without trailing
  /// spaces.
  private static func leftAlignedLines(_ table: [[TextCell]], useColor: Bool)
    -> [String]
  {
    let widths = columnWidths(table.map { $0.map(\.text) })
    return table.map { row in
      var line = row.enumerated().map { index, cell in
        let padded =
          cell.text
          + String(repeating: " ", count: widths[index] - cell.text.count)
        guard useColor, let direction = cell.direction else {
          return padded
        }
        let color: AnsiColor = direction == .smaller ? .green : .red
        return "\(ansi: .fg(color))\(padded)\(ansi: .fg(.normal))"
      }.joined(separator: "  ")
      while line.hasSuffix(" ") {
        line.removeLast()
      }
      return line
    }
  }

  /// The lines of a summary table: labels on the left, counts right-aligned.
  private static func matrixLines(_ rows: [MatrixRow], withThresholds: Bool)
    -> [String]
  {
    var header = ["", "smaller", "larger", "same", "fail", "skip"]
    if withThresholds {
      header += ["|", "over", "newly over", "newly under"]
    }
    var table = [header]
    for row in rows {
      func compared(_ count: Int) -> String {
        row.isBaseline ? "-" : String(count)
      }
      var cells = [
        row.label, compared(row.counts.smaller), compared(row.counts.larger),
        compared(row.counts.same), String(row.counts.failed),
        String(row.counts.skipped),
      ]
      if withThresholds {
        cells += [
          "|", String(row.counts.over), compared(row.counts.newlyOver),
          compared(row.counts.newlyUnder),
        ]
      }
      table.append(cells)
    }
    let widths = columnWidths(table)
    return table.map { row in
      row.enumerated().map { index, text in
        let padding = String(repeating: " ", count: widths[index] - text.count)
        return index == 0 ? text + padding : padding + text
      }.joined(separator: "  ")
    }
  }

  /// The width of each column: the length of its longest cell.
  private static func columnWidths(_ table: [[String]]) -> [Int] {
    var widths: [Int] = []
    for row in table {
      for (index, text) in row.enumerated() {
        if index < widths.count {
          widths[index] = max(widths[index], text.count)
        } else {
          widths.append(text.count)
        }
      }
    }
    return widths
  }
}
