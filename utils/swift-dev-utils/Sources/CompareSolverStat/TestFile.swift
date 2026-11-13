//===--- TestFile.swift ---------------------------------------------------===//
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

/// A Swift test file. The comparison compiles it once for each of its RUN
/// lines.
public struct TestFile: Sendable, Hashable {
  public var path: AbsolutePath
  /// The file's path relative to the test directory. It names the file's runs
  /// in the report.
  public var name: RelativePath

  public init(path: AbsolutePath, name: RelativePath) {
    self.path = path
    self.name = name
  }
}

// MARK: - Finding test files

extension TestFile {
  /// Every .swift file under a directory, including its subdirectories, sorted
  /// by path component.
  ///
  /// - Throws: `ComparisonRun.Failure` when the directory does not exist or
  ///   holds no .swift files.
  public static func all(in directory: AbsolutePath) throws -> [TestFile] {
    guard directory.isDirectory else {
      throw ComparisonRun.Failure(
        "The test directory \(directory) does not exist.")
    }
    let subpaths = try FileManager.default.subpathsOfDirectory(
      atPath: directory.rawPath)
    var files: [TestFile] = []
    for subpath in subpaths {
      let name = RelativePath(FilePath(subpath))
      // As in lit, files in Inputs directories are helpers of tests, not
      // tests themselves.
      if name.components.contains(where: { $0.string == "Inputs" }) {
        continue
      }
      let path = directory.appending(name)
      if name.hasExtension(.swift), !path.isDirectory {
        files.append(TestFile(path: path, name: name))
      }
    }
    guard !files.isEmpty else {
      throw ComparisonRun.Failure(
        "There are no .swift files under \(directory).")
    }
    // Compare component by component, so that a/x.swift comes before
    // a-b/x.swift.
    files.sort {
      $0.name.components.lexicographicallyPrecedes($1.name.components) {
        $0.string < $1.string
      }
    }
    return files
  }

  /// A single test file, named by its file name.
  ///
  /// - Throws: `ComparisonRun.Failure` when there is no file at the path.
  public init(file: AbsolutePath) throws {
    guard file.exists, !file.isDirectory else {
      throw ComparisonRun.Failure("\(file) does not exist.")
    }
    self.init(path: file, name: RelativePath(FilePath(file.fileName)))
  }
}

// MARK: - RUN lines

extension TestFile {
  /// The text after RUN: of each RUN line in the file, as `runLineTexts(in:)`
  /// reads them.
  public func runLineTexts() throws -> [String] {
    let contents = try path.read()
    return Self.runLineTexts(in: String(decoding: contents, as: UTF8.self))
  }

  /// The text after RUN: of each RUN line in the comment block at the top of a
  /// source. The block ends at the first line that is neither blank nor a //
  /// comment. A RUN line that ends in a backslash continues on the next RUN
  /// line.
  static func runLineTexts(in source: String) -> [String] {
    var texts: [String] = []
    var pending = ""
    // Swift treats "\r\n" as one Character, so splitting on "\n" would leave
    // a file with Windows line endings in one piece.
    for rawLine in source.split(
      omittingEmptySubsequences: false, whereSeparator: \.isNewline)
    {
      let line = rawLine.trimmingCharacters(in: .whitespacesAndNewlines)
      if line.isEmpty {
        continue
      }
      guard line.hasPrefix("//") else {
        break
      }
      let comment = line.dropFirst(2).drop(while: \.isWhitespace)
      guard comment.hasPrefix("RUN:") else {
        continue
      }
      let text = comment.dropFirst(4).trimmingCharacters(in: .whitespaces)
      if text.hasSuffix("\\") {
        pending += String(text.dropLast()) + " "
        continue
      }
      texts.append(pending + text)
      pending = ""
    }
    return texts
  }
}

// MARK: - RunLine

extension TestFile {
  /// What one RUN line asks the frontend to do.
  public struct RunLine: Sendable, Hashable {
    /// The frontend arguments of the line, without -verify,
    /// -verify-ignore-unrelated and -solver-scope-threshold.
    public var arguments: [Argument]
    /// The -solver-scope-threshold value of the line, if it has one.
    public var scopeThreshold: Int?
    /// Whether the line asked for an -emit- mode, such as -emit-sil, which
    /// -typecheck replaces.
    public var replacesEmitMode: Bool
    /// Whether the line compiles the test file. A line that does not builds
    /// other files, such as a module that later lines import.
    public var compilesTestFile: Bool

    public init(
      arguments: [Argument], scopeThreshold: Int?,
      replacesEmitMode: Bool = false, compilesTestFile: Bool = true
    ) {
      self.arguments = arguments
      self.scopeThreshold = scopeThreshold
      self.replacesEmitMode = replacesEmitMode
      self.compilesTestFile = compilesTestFile
    }
  }
}

extension TestFile.RunLine {
  /// One frontend argument of a RUN line.
  public enum Argument: Sendable, Hashable {
    case text(String)
    /// The test file, where the line has %s as the value of -primary-file.
    case testFile
  }

  /// Whether the arguments name the test file themselves. Then the file is not
  /// also passed as an input.
  public var namesTestFile: Bool {
    arguments.contains(.testFile)
  }

  /// The CPU of the machine running the comparison, which %target-cpu stands
  /// for.
  public static var hostCPU: String {
    #if arch(arm64)
    return "arm64"
    #elseif arch(x86_64)
    return "x86_64"
    #else
    return "unknown"
    #endif
  }
}

// MARK: - Translating RUN lines

extension TestFile.RunLine {
  /// Translates the text of a RUN line. Everything after a pipe is ignored,
  /// as are `not`, `2>&1` and -emit- flags, for which -typecheck stands; %S
  /// and %p become `testDirectory`, and %target-cpu becomes `targetCPU`.
  ///
  /// - Throws: `Unsupported` when the line runs something other than the
  ///   frontend, uses a lit substitution that has no meaning outside lit, or
  ///   cannot be split into words.
  public init(
    parsing text: String, testDirectory: AbsolutePath,
    targetCPU: String = Self.hostCPU
  ) throws(Unsupported) {
    // What follows a pipe, such as FileCheck, checks the output, which the
    // comparison has no use for.
    let command = String(text.prefix { $0 != "|" })
    var words: [String]
    do {
      // Shell operators must stay inside their words: stopping at them would
      // leave the 2 of 2>&1 behind, and would turn
      // %target-swift-frontend(mock-sdk: ...) into a plain frontend call.
      words = try CommandArgumentParser.parse(
        command, flagSpec: nil, stopAtShellOperator: false
      ).flatMap(\.rawArgs)
    } catch {
      throw .unparsable("\(error)")
    }
    if words.first == "not" {
      words.removeFirst()
    }
    words.removeAll { $0 == "2>&1" }
    guard let program = words.first else {
      throw .empty
    }

    var arguments: [Argument]
    // The verify substitution ends in %s, so it always compiles the test.
    var compilesTestFile = program == "%target-typecheck-verify-swift"
    switch program {
    case "%target-typecheck-verify-swift":
      // The flags of the substitution, apart from -verify.
      arguments = [
        .text("-typecheck"),
        .text("-disable-objc-attr-requires-foundation-module"),
      ]
    case "%target-swift-frontend":
      arguments = []
    default:
      throw .runs(program)
    }

    var scopeThreshold: Int?
    var replacesEmitMode = false
    var dropsNextWord = false
    for word in words.dropFirst() {
      if dropsNextWord {
        dropsNextWord = false
        continue
      }
      if word.hasPrefix("-emit-") {
        // Only type checking is measured, so an -emit- mode or output is
        // dropped, along with the path an -emit-...-path flag takes.
        replacesEmitMode = true
        dropsNextWord = word.hasSuffix("-path")
        continue
      }
      if let threshold = Self.scopeThreshold(in: word) {
        scopeThreshold = threshold
        continue
      }
      if word == "%s" || word == "%/s" {
        compilesTestFile = true
        // A plain %s is the input, which the run adds itself. After a flag
        // that takes a file it is that flag's value, so it stays.
        if case .text(let previous) = arguments.last,
          Self.fileValueFlags.contains(previous)
        {
          arguments.append(.testFile)
        }
        continue
      }
      if Self.droppedFlags.contains(word) {
        continue
      }
      // %S and %p, and their %/ forms, are the test's directory, as in a
      // helper file next to the test: %S/Inputs/other.swift.
      let translated = word.replacing(
        #/%\/?[Sp](?![A-Za-z0-9_])/#, with: testDirectory.rawPath
      ).replacingOccurrences(of: "%target-cpu", with: targetCPU)
      guard !translated.contains("%") else {
        throw .substitution(translated)
      }
      arguments.append(.text(translated))
    }
    if !arguments.contains(.text("-typecheck")) {
      arguments.insert(.text("-typecheck"), at: 0)
    }
    self.init(
      arguments: arguments, scopeThreshold: scopeThreshold,
      replacesEmitMode: replacesEmitMode, compilesTestFile: compilesTestFile)
  }

  /// Flags that only matter to lit's checking of the output.
  private static let droppedFlags: Set<String> = [
    "-verify", "-verify-ignore-unrelated",
  ]

  /// Flags that take the test file as their value.
  private static let fileValueFlags: Set<String> = ["-primary-file"]

  /// The N of -solver-scope-threshold=N, or nil when the word is anything
  /// else.
  private static func scopeThreshold(in word: String) -> Int? {
    let prefix = "-solver-scope-threshold="
    guard word.hasPrefix(prefix) else {
      return nil
    }
    let digits = word.dropFirst(prefix.count)
    guard !digits.isEmpty, digits.allSatisfy({ $0.isASCII && $0.isNumber })
    else {
      return nil
    }
    return Int(digits)
  }
}

// MARK: - Unsupported

extension TestFile.RunLine {
  /// Why the comparison cannot run a RUN line.
  public enum Unsupported: Error, Sendable, Hashable {
    case empty
    /// The line runs this command instead of the frontend.
    case runs(String)
    /// This word uses a lit substitution that has no meaning outside lit, such
    /// as %t.
    case substitution(String)
    /// The line cannot be split into words, for the reason given.
    case unparsable(String)
  }
}

extension TestFile.RunLine.Unsupported: CustomStringConvertible {
  public var description: String {
    switch self {
    case .empty:
      "the line is empty"
    case .runs(let command):
      "it runs \(command)"
    case .substitution(let word):
      "it uses the substitution in \(word)"
    case .unparsable(let reason):
      reason
    }
  }
}

// MARK: - RunName

extension TestFile {
  /// The name of a run in the report: the file's name, followed by #N when the
  /// file has more than one RUN line.
  public struct RunName: Sendable, Hashable {
    public var file: RelativePath
    /// The position of the run's RUN line among the file's RUN lines, counting
    /// from 1. Nil when the file has at most one RUN line.
    public var index: Int?

    public init(file: RelativePath, index: Int?) {
      self.file = file
      self.index = index
    }
  }
}

extension TestFile.RunName: CustomStringConvertible {
  public var description: String {
    guard let index else {
      return file.description
    }
    return "\(file)#\(index)"
  }
}

// MARK: - Run

extension TestFile {
  /// One compilation of a test file, with the flags of one of its RUN lines.
  public struct Run: Sendable, Hashable {
    public var file: TestFile
    /// The position of the RUN line, as in `RunName`.
    public var index: Int?
    public var line: RunLine

    public init(file: TestFile, index: Int?, line: RunLine) {
      self.file = file
      self.index = index
      self.line = line
    }
  }
}

extension TestFile.Run {
  public var name: TestFile.RunName {
    TestFile.RunName(file: file.name, index: index)
  }

  /// The input to pass the frontend, or nil when the line's arguments name the
  /// test file themselves.
  public var input: AbsolutePath? {
    line.namesTestFile ? nil : file.path
  }

  /// The line's arguments for the frontend, with the test file's path in place
  /// of %s.
  public var lineArguments: [String] {
    line.arguments.map { argument in
      switch argument {
      case .text(let text):
        text
      case .testFile:
        file.path.rawPath
      }
    }
  }
}

// MARK: - SkippedLine

extension TestFile {
  /// A RUN line the comparison cannot run.
  public struct SkippedLine: Sendable, Hashable {
    public var file: TestFile
    /// The position of the RUN line, as in `RunName`.
    public var index: Int?
    public var text: String
    public var reason: RunLine.Unsupported

    public init(
      file: TestFile, index: Int?, text: String, reason: RunLine.Unsupported
    ) {
      self.file = file
      self.index = index
      self.text = text
      self.reason = reason
    }
  }
}

extension TestFile.SkippedLine: CustomStringConvertible {
  public var description: String {
    "\(TestFile.RunName(file: file.name, index: index)): skipped, \(reason): \(text)"
  }
}

// MARK: - Plan

extension TestFile {
  /// The runs planned for a set of test files, and the RUN lines that cannot be
  /// run.
  public struct Plan: Sendable {
    public var runs: [Run]
    public var skippedLines: [SkippedLine]

    public init(runs: [Run] = [], skippedLines: [SkippedLine] = []) {
      self.runs = runs
      self.skippedLines = skippedLines
    }
  }
}

extension TestFile.Plan {
  /// Plans the runs of every file, reading their RUN lines.
  public init(files: [TestFile], targetCPU: String = TestFile.RunLine.hostCPU)
    throws
  {
    self.init()
    for file in files {
      let texts = try file.runLineTexts()
      add(file, runLineTexts: texts, targetCPU: targetCPU)
    }
  }

  /// Adds the runs of one file. A file without RUN lines gets one run with
  /// -typecheck alone. A line that does not compile the test file, and a line
  /// whose emit mode was replaced and that then repeats an earlier line of
  /// the file, are left out without a note.
  mutating func add(
    _ file: TestFile, runLineTexts texts: [String], targetCPU: String
  ) {
    guard !texts.isEmpty else {
      runs.append(
        TestFile.Run(
          file: file, index: nil,
          line: TestFile.RunLine(
            arguments: [.text("-typecheck")], scopeThreshold: nil)))
      return
    }
    var planned: [TestFile.RunLine] = []
    for (offset, text) in texts.enumerated() {
      let index = texts.count == 1 ? nil : offset + 1
      do throws(TestFile.RunLine.Unsupported) {
        let line = try TestFile.RunLine(
          parsing: text, testDirectory: file.path.dropLast(),
          targetCPU: targetCPU)
        // A line that builds other files is a step towards a later line, not
        // a test of the file.
        if !line.compilesTestFile {
          continue
        }
        // With its emit mode replaced, a line can repeat an earlier one, which
        // would measure the same compile twice.
        let repeatsEarlierLine = planned.contains {
          $0.arguments == line.arguments
            && $0.scopeThreshold == line.scopeThreshold
        }
        if line.replacesEmitMode && repeatsEarlierLine {
          continue
        }
        planned.append(line)
        runs.append(TestFile.Run(file: file, index: index, line: line))
      } catch {
        skippedLines.append(
          TestFile.SkippedLine(
            file: file, index: index, text: text, reason: error))
      }
    }
  }
}
