//===--- Compiler.swift ---------------------------------------------------===//
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

/// A compiler the comparison runs: a swift-frontend and the resource
/// directory it compiles with.
struct Compiler: Sendable, Hashable {
  /// main, wip, or the name of a snapshot. It names the compiler's columns.
  var name: String
  var frontend: AbsolutePath
  var resourceDirectory: AbsolutePath
}

extension Compiler {
  /// The compiler's columns, without and with the on flags.
  var columns: [Column] {
    Column.State.allCases.map { Column(compiler: name, state: $0) }
  }
}

// MARK: - Building

extension Compiler {
  /// Builds bin/swift-frontend in a build directory and saves it in
  /// `savedDirectory`/bin. Next to it, a lib directory links to the build
  /// directory's lib directory, or holds a copy of its host libraries when
  /// `copyLibraries` is set, because the binary loads them from
  /// ../lib/swift/host/compiler. Ninja's output goes to `log`, and the outcome
  /// is reported on standard error.
  ///
  /// - Throws: `ComparisonRun.Failure`, naming the log, when the build fails.
  static func build(
    named name: String, buildDirectory: AbsolutePath,
    savingTo savedDirectory: AbsolutePath, log: AbsolutePath,
    copyLibraries: Bool
  ) async throws -> Compiler {
    let started = ContinuousClock.now
    let status = try await Invocation(
      .at(ninjaWrapper(for: buildDirectory)),
      ["-C", buildDirectory.rawPath, "bin/swift-frontend"]
    ).run(loggingTo: log)
    let elapsed = ContinuousClock.now - started
    let seconds = String(
      format: "%.0f",
      Double(elapsed.components.seconds) + Double(
        elapsed.components.attoseconds) / 1e18)
    guard status.isSuccess else {
      throw ComparisonRun.Failure(
        "The \(name) build FAILED after \(seconds) s (ninja \(status)). See \(log)"
      )
    }

    let frontend = savedDirectory.appending("bin/swift-frontend")
    try frontend.parentDir?.makeDir()
    frontend.remove()
    try FileManager.default.copyItem(
      atPath: buildDirectory.appending("bin/swift-frontend").rawPath,
      toPath: frontend.rawPath)

    let libraries = savedDirectory.appending("lib")
    libraries.remove()
    if copyLibraries {
      try libraries.appending("swift").makeDir()
      try FileManager.default.copyItem(
        atPath: buildDirectory.appending("lib/swift/host").rawPath,
        toPath: libraries.appending("swift/host").rawPath)
    } else {
      try libraries.symlink(to: buildDirectory.appending("lib"))
    }

    ComparisonRun.log(
      "The \(name) build succeeded in \(seconds) s. Log: \(log)")
    return Compiler(
      name: name, frontend: frontend,
      resourceDirectory: buildDirectory.appending("lib/swift"))
  }

  /// The cached-build ninja wrapper that build-script makes next to the build
  /// directory.
  static func ninjaWrapper(for buildDirectory: AbsolutePath) -> AbsolutePath {
    buildDirectory.dropLast().appending("build-utils/ninja")
  }

  /// A compiler that an earlier build saved in `savedDirectory`.
  ///
  /// - Throws: `ComparisonRun.Failure` when there is no saved compiler there.
  static func saved(
    named name: String, in savedDirectory: AbsolutePath,
    resourceDirectory: AbsolutePath
  ) throws -> Compiler {
    let frontend = savedDirectory.appending("bin/swift-frontend")
    guard frontend.exists else {
      throw ComparisonRun.Failure("There is no saved compiler at \(frontend).")
    }
    return Compiler(
      name: name, frontend: frontend, resourceDirectory: resourceDirectory)
  }
}

// MARK: - Snapshots

extension Compiler {
  /// Downloads a swift.org snapshot with swift_snapshot_tool and returns its
  /// compiler. Without a `tool`, the tool's package in the swift checkout is
  /// run with swift run.
  ///
  /// - Throws: `ComparisonRun.Failure` when the tool fails or does not print
  ///   the directory of a toolchain as the last line of its output.
  static func snapshot(
    _ snapshot: Configuration.Snapshot, tool: AbsolutePath?,
    swiftDirectory: AbsolutePath, workspace: AbsolutePath, branch: String?
  ) async throws -> Compiler {
    var invocation =
      if let tool {
        Invocation(.at(tool), [])
      } else {
        Invocation(
          .named("swift"),
          [
            "run", "--package-path",
            swiftDirectory.appending("utils/swift_snapshot_tool").rawPath,
            "swift_snapshot_tool",
          ])
      }
    invocation.arguments += [
      "download", "--date", snapshot.day.description, "--workspace",
      workspace.rawPath,
    ]
    if let branch {
      invocation.arguments.append("--\(branch)")
    }
    let lines = try await invocation.output().split(whereSeparator: \.isNewline)
      .map(String.init)
    guard let last = lines.last,
      case let root = AnyPath(FilePath(last)).absoluteInWorkingDir,
      root.isDirectory
    else {
      throw ComparisonRun.Failure(
        "swift_snapshot_tool did not print a toolchain directory:\n\(lines.joined(separator: "\n"))"
      )
    }
    return Compiler(
      name: snapshot.name, frontend: root.appending("usr/bin/swift-frontend"),
      resourceDirectory: root.appending("usr/lib/swift"))
  }
}
