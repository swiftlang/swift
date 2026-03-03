//===--- crash-reduce.swift -----------------------------------------------===//
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
public import CrashReduce
import Foundation
import Subprocess
import System

@main
struct CrashReduce: AsyncParsableCommand {
  static let configuration = CommandConfiguration(
    commandName: "crash-reduce",
    subcommands: [
      ReduceCommand.self,
      GetSignatureCommand.self,
      CleanupCommand.self,
      StatsCommand.self,
      CReduceStepCommand.self,
    ],
    defaultSubcommand: ReduceCommand.self
  )
}

struct ReduceCommand: AsyncParsableCommand {
  static let configuration = CommandConfiguration(
    commandName: "reduce",
    abstract: "Reduce crashers into reproducer test cases"
  )

  @Option(name: .short, help: "Path to a directory containing crashers")
  var fromDir: AnyPath?

  @Option(
    name: .short, help: "The target directory to place the reduced crashers"
  )
  var toDir: AnyPath?

  @Option(
    name: .customLong("ide-crashers"),
    help: "The directory to place IDE crashers."
  )
  var ideCrashersPath: AnyPath?

  @Option(help: "Path to the Swift compiler to use")
  var swiftPath: AnyPath

  @Option(help: "Path to the SDK to use. If not set it will be inferred.")
  var sdkPath: AnyPath?

  // This is still experimental.
  @Flag(help: .hidden)
  var fileIssues: Bool = false

  @Flag(help: "Reprocess all the reproducers in the target directory")
  var reprocess: Bool = false

  @Flag(
    help: """
      Ignore any existing reproducers in the target directory, producing new
      reproducers
      """
  )
  var ignoreExisting: Bool = false

  @Flag(help: "Avoid trying reproducer configurations that are slow")
  var quick: Bool = false

  @Flag(
    name: .customLong("rm"), help: "Delete input files on successful reduction"
  )
  var deleteInputs: Bool = false

  @Argument(
    help: ArgumentHelp(
      "Path(s) to crashers; use '-f' for a directory of crashers",
      valueName: "inputs"
    )
  )
  var otherInputs: [AnyPath] = []

  @Argument(
    parsing: .postTerminator,
    help: """
      A set of frontend arguments to use for reproducing a frontend crash. If
      not provided, a default set of arguments will be used.
      """
  )
  var frontendArgs: [String] = []
}

extension ReduceCommand {
  // We can infer the repo path from the location of crash-reduce itself.
  //                      1         2           3         4            5
  // #filePath = swift/utils/swift-dev-utils/Sources/crash-reduce/crash-reduce.swift
  private static let inferredRepoPath = AbsolutePath(#filePath).dropLast(5)

  var defaultCrasherOutputPath: AbsolutePath {
    Self.inferredRepoPath.appending("validation-test/compiler_crashers")
  }
  var defaultIDECrasherOutputPath: AbsolutePath {
    Self.inferredRepoPath.appending("validation-test/IDE/crashers")
  }

  func computeSDKPath() async throws -> AbsolutePath {
    if let sdkPath {
      return sdkPath.absoluteInWorkingDir
    }
    let result = try await Subprocess.run(
      .name("xcrun"), arguments: ["-sdk", "macosx", "--show-sdk-path"],
      output: .string(limit: .max)
    )
    struct FailedToInferSDKError: Error, CustomStringConvertible {
      var description: String { "failed to infer SDK path" }
    }
    guard result.terminationStatus.isSuccess else {
      throw FailedToInferSDKError()
    }
    let sdkPath = AbsolutePath(
      result.standardOutput?.trimmingCharacters(in: .whitespacesAndNewlines) ?? ""
    )
    guard sdkPath.exists else {
      throw FailedToInferSDKError()
    }
    return sdkPath
  }

  func run() async throws {
    let fromDir = self.fromDir.map(\.absoluteInWorkingDir)
    let toDir = self.toDir.map(\.absoluteInWorkingDir)
    let ideCrashersPath = self.ideCrashersPath.map(\.absoluteInWorkingDir)
    try await ProcessReproducers(
      from: fromDir,
      to: toDir ?? defaultCrasherOutputPath,
      otherInputs: self.otherInputs.map(\.absoluteInWorkingDir),
      ideOutputDir: ideCrashersPath ?? toDir ?? defaultIDECrasherOutputPath,
      toolchain: Toolchain(
        swiftPath: swiftPath.absoluteInWorkingDir,
        sdkPath: computeSDKPath()
      ),
      quickMode: quick,
      deleteInputs: deleteInputs
    ).process(
       reprocess: reprocess, ignoreExisting: ignoreExisting,
       fileIssues: fileIssues, frontendArgs: frontendArgs.map { .value($0) }
    )
  }
}

struct GetSignatureCommand: ParsableCommand {
  static let configuration = CommandConfiguration(
    commandName: "signature",
    abstract: "Retrieve the signature from a given crash log"
  )

  @Option(
    name: .customShort("f"),
    help: "Path to the crash log; if not specified, will read from stdin"
  )
  var fromFile: AnyPath?

  // This is just here for benchmarking.
  @Option(help: .hidden)
  var repeats: Int = 0

  func run() throws {
    let input = try {
      if let fromFile {
        return String(
          decoding: try fromFile.absoluteInWorkingDir.read(), as: UTF8.self
        )
      }
      var input = ""
      while let line = readLine(strippingNewline: false) {
        input += line
      }
      return input
    }()
    func runOnce() -> Signature? {
      CrashLog(from: input)?.signature
    }
    let start = Date()
    for _ in 0 ..< repeats {
      guard runOnce() != nil else {
        Darwin.exit(1)
      }
    }
    guard let sig = runOnce() else {
      Darwin.exit(1)
    }
    print(sig)
    if repeats > 0 {
      print("\(Int((Date().timeIntervalSince(start) * 1000).rounded()))ms")
    }
  }
}

extension CleanupKind: ExpressibleByArgument {
  public init?(argument: String) {
    self.init(rawValue: argument)
  }
}

struct CleanupCommand: ParsableCommand {
  static let configuration = CommandConfiguration(
    commandName: "cleanup",
    abstract: "Cleanup a reproducer file"
  )

  @Option(
    help: """
    Type of cleanup to perform:
      - 'basicFormat' runs SwiftBasicFormat
      - 'fix' runs SwiftBasicFormat, but also inserts any missing tokens
      - 'swiftFormat' runs swift-format (in-process)
    
    """
  )
  var kind: CleanupKind = .fix

  @Argument
  var path: AnyPath

  func run() throws {
    let code = try Code(from: path.absoluteInWorkingDir)
    print(try code.cleanupSyntax(kind).text, terminator: "")
  }
}

/// This is an implementation detail of the 'reduce' command, users shouldn't
/// need to run it directly.
struct CReduceStepCommand: AsyncParsableCommand {
  static let configuration = CommandConfiguration(
    commandName: "creduce-step",
    shouldDisplay: false
  )

  @Option(name: .customLong("args-json-path"))
  var argsJSONPath: AnyPath

  @Option(name: .customLong("input-file"))
  var inputFile: AnyPath?

  func run() async throws {
    do {
      let inputFiles = try (self.inputFile?.absoluteInWorkingDir).map {[$0]} ?? {
        let workingDir = AbsolutePath(FileManager.default.currentDirectoryPath)
        let files = try workingDir.getDirContents().filter { $0.hasExtension(.swift) }
        guard !files.isEmpty else { Darwin.exit(0) }
        return files.map { workingDir.appending($0) }
      }()
      let argsJSON = try argsJSONPath.absoluteInWorkingDir.read()
      let reproduced = try await CReduceStep.check(argsJSON: argsJSON, files: inputFiles)
      Darwin.exit(reproduced ? 0 : 1)
    } catch {
      log.error("\(error)")
      Darwin.exit(1)
    }
  }
}

/// For tweaking fuzzer weights.
struct StatsCommand: ParsableCommand {
  static let configuration = CommandConfiguration(
    commandName: "stats",
    shouldDisplay: false
  )

  @Argument
  var path: AnyPath

  func run() throws {
    try SyntaxStatCollector.collect(in: path.absoluteInWorkingDir)
  }
}
