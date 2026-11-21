//===--- Invocation.swift -------------------------------------------------===//
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
import Subprocess
import System
import Utils

/// A program to run, with its arguments.
struct Invocation: Sendable {
  var program: Program
  var arguments: [String]
  /// Where the program runs. Nil stands for the current directory.
  var workingDirectory: AbsolutePath?

  init(
    _ program: Program, _ arguments: [String],
    in workingDirectory: AbsolutePath? = nil
  ) {
    self.program = program
    self.arguments = arguments
    self.workingDirectory = workingDirectory
  }
}

extension Invocation {
  enum Program: Sendable {
    /// A program found on PATH.
    case named(String)
    case at(AbsolutePath)
  }
}

extension Invocation: CustomStringConvertible {
  /// The command line, quoted for a shell.
  var description: String {
    let programText =
      switch program {
      case .named(let name): name
      case .at(let path): path.rawPath
      }
    return ([programText] + arguments).map {
      Command.Argument.value($0).printed
    }.joined(separator: " ")
  }
}

// MARK: - Running

extension Invocation {
  /// Runs the program and returns its standard output.
  ///
  /// - Throws: `ComparisonRun.Failure`, with the command line and the
  ///   program's output, when the program cannot start or exits unsuccessfully.
  func output() async throws -> String {
    let result = try await start {
      try await Subprocess.run(
        executable, arguments: Arguments(arguments),
        workingDirectory: workingDirectory?.storage,
        output: .string(limit: .max), error: .string(limit: .max))
    }
    guard Ending(result.terminationStatus).isSuccess else {
      let output =
        ((result.standardOutput ?? "") + (result.standardError ?? ""))
        .trimmingCharacters(in: .whitespacesAndNewlines)
      throw ComparisonRun.Failure(
        "Command failed with \(Ending(result.terminationStatus)): \(self)\n\(output)"
      )
    }
    return result.standardOutput ?? ""
  }

  /// Runs the program with its output going to the terminal.
  ///
  /// - Throws: `ComparisonRun.Failure` when the program cannot start or exits
  ///   unsuccessfully.
  func runInTerminal() async throws {
    let result = try await start {
      try await Subprocess.run(
        executable, arguments: Arguments(arguments),
        workingDirectory: workingDirectory?.storage, output: .standardOutput,
        error: .standardError)
    }
    guard Ending(result.terminationStatus).isSuccess else {
      throw ComparisonRun.Failure(
        "Command failed with \(Ending(result.terminationStatus)): \(self)"
      )
    }
  }

  /// Runs the program with its standard output and standard error written to
  /// a log file, which starts with the command line. Returns how the program
  /// ended, which may be unsuccessful.
  ///
  /// - Throws: `ComparisonRun.Failure` when the program cannot start, or an
  ///   error when the log cannot be written.
  func run(loggingTo log: AbsolutePath) async throws -> Ending {
    try log.parentDir?.makeDir()
    let descriptor = try FileDescriptor.open(
      log.storage, .writeOnly, options: [.create, .truncate],
      permissions: [.ownerReadWrite, .groupRead, .otherRead])
    defer {
      try? descriptor.close()
    }
    try descriptor.writeAll(Array("\(self)\n\n".utf8))
    let result = try await start {
      try await Subprocess.run(
        executable, arguments: Arguments(arguments),
        workingDirectory: workingDirectory?.storage,
        output: .fileDescriptor(descriptor, closeAfterSpawningProcess: false),
        error: .combineWithOutput)
    }
    return Ending(result.terminationStatus)
  }

  private var executable: Executable {
    switch program {
    case .named(let name): .name(name)
    case .at(let path): .path(path.storage)
    }
  }

  /// Runs `body`, turning a failure to start the program into a
  /// `ComparisonRun.Failure` that names the command line.
  private func start<Result>(_ body: () async throws -> Result) async throws
    -> Result
  {
    do {
      return try await body()
    } catch let failure as ComparisonRun.Failure {
      throw failure
    } catch {
      throw ComparisonRun.Failure("Could not run: \(self)\n\(error)")
    }
  }
}

// MARK: - Ending

extension Invocation {
  /// How a program ended.
  enum Ending: Sendable, Hashable {
    case exited(code: Int32)
    case signaled(Int32)
  }
}

extension Invocation.Ending {
  var isSuccess: Bool {
    self == .exited(code: 0)
  }

  // Swift-subprocess's own type stays inside this file: its module also
  // declares a Configuration, which would clash with this module's in any
  // file that imports both.
  fileprivate init(_ status: TerminationStatus) {
    switch status {
    case .exited(let code): self = .exited(code: code)
    case .unhandledException(let code): self = .signaled(code)
    }
  }
}

extension Invocation.Ending: CustomStringConvertible {
  /// How the program ended, as in "exit code 1".
  var description: String {
    switch self {
    case .exited(let code): "exit code \(code)"
    case .signaled(let signal): "signal \(signal)"
    }
  }
}
