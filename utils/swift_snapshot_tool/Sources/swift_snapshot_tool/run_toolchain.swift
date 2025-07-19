//===--- run_toolchain.swift ----------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import ArgumentParser
import Foundation

struct RunToolchains: AsyncParsableCommand {
  static let configuration = CommandConfiguration(
    commandName: "run",
    discussion: """
        Run and determine success/failure of a script against a specified snapshot
        like the bisect command would. Used to determine the start/end bisect
        dates to pass to the bisect command. The script is passed paths into the
        downloaded snapshot via environment variables and is expected to compile
        and or run swift programs using the snapshot artifacts.
      """
  )

  @Flag var platform: Platform = .osx

  @Flag(help: "The specific branch of toolchains we should download")
  var branch: Branch = .development

  @Option(
    help: """
      The directory where toolchains should be downloaded to.
      """
  )
  var workspace: String = "/tmp/swift_snapshot_tool_workspace_v1"

  @Option(
    help: """
      The script that should be run. It should run a specific swift compilation and
      or program. Paths into the snapshots are passed in via the environment variables \(environmentVariables).
      """
  )
  var script: String

  @Option(help: "Date. We use the first snapshot produced before the given date")
  var date: String

  var dateAsDate: Date {
    let d = DateFormatter()
    d.dateFormat = "yyyy-MM-dd"
    guard let result = d.date(from: date) else {
      log("Improperly formatted date: \(date)! Expected format: yyyy_MM_dd.")
      fatalError()
    }
    return result
  }

  @Flag(help: "Invert the test so that we assume the newest succeeds")
  var invert = false

  @Argument(help: "Extra constant arguments to pass to the test")
  var extraArgs: [String] = []

  @Flag(help: "Attempt to use the snapshot tag that is immediately younger than the selected")
  var offset_by_one = false

  @Flag(help: "Emit verbose logging")
  var verbose = false

  mutating func run() async throws {
    if !FileManager.default.fileExists(atPath: workspace) {
      do {
        log("[INFO] Creating workspace: \(workspace)")
        try FileManager.default.createDirectory(
          atPath: workspace,
          withIntermediateDirectories: true,
          attributes: nil
        )
      } catch {
        log(error.localizedDescription)
      }
    }

    // Load our tags from swift's github repo
    let tags = try! await getTagsFromSwiftRepo(branch: branch)

    let date = self.dateAsDate
    guard var tagIndex = tags.firstIndex(where: { $0.tag.date(branch: self.branch) <= date }) else {
      log("Failed to find tag with date: \(date)")
      fatalError()
    }

    if self.offset_by_one {
      if tagIndex == tags.startIndex {
        log("[INFO] Cannot run one tag earlier than the first snapshot tag?!")
        fatalError()
      }
      tagIndex = tagIndex - 1
    }

    // Newest is first. So 0 maps to the newest tag. We do this so someone can
    // just say 50 toolchains ago. To get a few weeks worth. This is easier than
    // writing dates a lot.

    let result = try! await downloadToolchainAndRunTest(
      platform: platform,
      tag: tags[tagIndex].tag,
      branch: branch,
      workspace: workspace,
      script: script,
      extraArgs: extraArgs,
      verbose: verbose
    )
    var success = result == 0
    if self.invert {
      success = !success
    }
    if success {
      log("[INFO] Snapshot succeeds!")
      //fatalError()
    } else {
      log("[INFO] Snapshot fails!")
    }
  }
}
