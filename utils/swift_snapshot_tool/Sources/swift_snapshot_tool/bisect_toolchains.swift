//===--- bisect_toolchains.swift ------------------------------------------===//
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

struct BisectToolchains: AsyncParsableCommand {
  static let configuration = CommandConfiguration(
    commandName: "bisect",
    discussion: """
      Bisects on swift snapshots downloaded from swift.org via an exit status of
      a script. Script is passed paths into the downloaded snapshot via
      environment variables and is expected to compile and or run swift programs
      using the snapshot artifacts.
      """)

  @Flag var platform: Platform = .osx

  @Flag(help: "The specific branch of toolchains we should download")
  var branch: Branch = .development

  @Option(
    help: """
      The directory where toolchains should be downloaded to.
      """)
  var workspace: String = "/tmp/swift_snapshot_tool_workspace_v1"

  @Option(
    help: """
      The script that should be run. It should run a specific swift compilation and
      or program. Paths into the snapshots are passed in via the environment variables \(environmentVariables).
      """)
  var script: String

  @Option(help:
            """
            Expected to Pass. We use the first snapshot produced before the
            given date if a snapshot at this specific date does not exist
            """)
  var oldDate: String

  var oldDateAsDate: Date {
    let d = DateFormatter()
    d.dateFormat = "yyyy-MM-dd"
    guard let result = d.date(from: oldDate) else {
      log("Improperly formatted date: \(oldDate)! Expected format: yyyy_MM_dd.")
      fatalError()
    }
    return result
  }

  @Option(help: """
            Expected to fail. If not set, defaults to use newest snapshot. If a
            date is specified and a snapshot does not exist for that date, the
            first snapshot before the specified date is used.
            """)
  var newDate: String?

  var newDateAsDate: Date? {
    guard let newDate = self.newDate else { return nil }
    let d = DateFormatter()
    d.dateFormat = "yyyy-MM-dd"
    guard let result = d.date(from: newDate) else {
      log("Improperly formatted date: \(newDate)! Expected format: yyyy_MM_dd.")
      fatalError()
    }
    return result
  }

  @Flag(help: "Invert the test so that we assume the newest succeeds")
  var invert = false

  @Argument(help: "Extra constant arguments to pass to the test")
  var extraArgs: [String] = []

  mutating func run() async throws {
    if !FileManager.default.fileExists(atPath: workspace) {
      do {
        log("[INFO] Creating workspace: \(workspace)")
        try FileManager.default.createDirectory(
          atPath: workspace,
          withIntermediateDirectories: true, attributes: nil)
      } catch {
        log(error.localizedDescription)
      }
    }

    // Load our tags from swift's github repo
    let tags = try! await getTagsFromSwiftRepo(branch: branch)

    // Newest is first. So 0 maps to the newest tag. We do this so someone can
    // just say 50 toolchains ago. To get a few weeks worth. This is easier than
    // writing dates a lot.
    let oldDateAsDate = self.oldDateAsDate
    guard let goodTagIndex = tags.firstIndex(where: { $0.tag.date(branch: self.branch) <= oldDateAsDate }) else {
      log("Failed to find tag with date: \(oldDateAsDate)")
      fatalError()
    }

    let badTagIndex: Int
    if let newDateAsDate = self.newDateAsDate {
      let b = tags.firstIndex(where: { $0.tag.date(branch: self.branch) < newDateAsDate })
      guard let b else {
        log("Failed to find tag newer than date: \(newDateAsDate)")
        fatalError()
      }
      badTagIndex = b
    } else {
      badTagIndex = 0
    }

    let totalTags = goodTagIndex - badTagIndex
    if totalTags < 0 {
      log("Good tag is newer than bad tag... good tag expected to be older than bad tag")
      fatalError()
    }

    var startIndex = goodTagIndex
    var endIndex = badTagIndex

    // First check if the newest toolchain succeeds. We assume this in our bisection.
    do {
      log("Testing that Oldest Tag Succeeds: \(tags[startIndex].tag))")
      let result = try! await downloadToolchainAndRunTest(
        platform: platform, tag: tags[startIndex].tag, branch: branch, workspace: workspace, script: script,
        extraArgs: extraArgs)
      var success = result == 0
      if self.invert {
        success = !success
      }
      if !success {
        log("[INFO] Oldest assumed good snapshot fails! Did you forget to pass --invert?")
        fatalError()
      } else {
        log("[INFO] Oldest snapshot passes test. Snapshot: \(tags[startIndex])")
      }
    }

    do {
      log("Testing that Newest Tag Fails: \(tags[endIndex].tag))")
      let result = try! await downloadToolchainAndRunTest(
        platform: platform, tag: tags[endIndex].tag, branch: branch, workspace: workspace, script: script,
        extraArgs: extraArgs)
      var success = result != 0
      if self.invert {
        success = !success
      }
      if !success {
        log("[INFO] Newest assumed bad snapshot succeeds! Did you forget to pass --invert?")
        fatalError()
      } else {
        log("[INFO] Newest snapshot passes test. Snapshot: \(tags[endIndex])")
      }
    }

    log("[INFO] Testing \(totalTags) toolchains")
    while startIndex != endIndex && startIndex != endIndex {
      let mid = (startIndex + endIndex) / 2

      let midValue = tags[mid].tag
      log(
        "[INFO] Visiting Mid: \(mid) with (Start, End) = (\(startIndex),\(endIndex)). Tag: \(midValue)"
      )
      let result = try! await downloadToolchainAndRunTest(
        platform: platform, tag: midValue, branch: branch, workspace: workspace, script: script,
        extraArgs: extraArgs)

      var success = result == 0
      if self.invert {
        success = !success
      }

      let midIsEndIndex = mid == endIndex

      if success {
        log("[INFO] PASSES! Setting start to mid!")
        startIndex = mid
      } else {
        log("[INFO] FAILS! Setting end to mid")
        endIndex = mid
      }

      if midIsEndIndex {
        log("Last successful value: \(tags[mid+1])")
        break
      }
    }
  }
}
