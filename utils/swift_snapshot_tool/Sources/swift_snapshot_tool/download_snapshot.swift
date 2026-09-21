//===--- download_snapshot.swift ------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import ArgumentParser
import Foundation

struct DownloadSnapshot: AsyncParsableCommand {
  static let configuration = CommandConfiguration(
    commandName: "download",
    discussion: """
      Downloads and extracts a snapshot from swift.org. On success, prints the
      toolchain root directory to stdout. Append usr/bin/swiftc,
      usr/bin/swift-frontend, etc. to obtain individual tool paths, or pass the
      printed directory directly to flags that accept a toolchain root.
      """)

  @Flag var platform: Platform = .osx

  @Flag(help: "The specific branch of toolchains we should download")
  var branch: Branch = .development

  @Option(
    help: """
      The directory where toolchains should be downloaded to.
      """)
  var workspace: String = "/tmp/swift_snapshot_tool_workspace_v1"

  @Option(help: "Date of the snapshot to download (yyyy-MM-dd). Uses the first snapshot on or before this date.")
  var date: String

  var dateAsDate: Date {
    let d = DateFormatter()
    d.dateFormat = "yyyy-MM-dd"
    guard let result = d.date(from: date) else {
      log("Improperly formatted date: \(date)! Expected format: yyyy-MM-dd.")
      fatalError()
    }
    return result
  }

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

    let tags = try! await getTagsFromSwiftRepo(branch: branch)

    let date = self.dateAsDate
    guard let tagIndex = selectTagIndex(tags, onOrBefore: date) else {
      log("Failed to find tag with date: \(date)")
      fatalError()
    }

    let tag = tags[tagIndex].tag
    let toolchainDir = try await downloadAndExtractToolchain(
      platform: platform, tag: tag, branch: branch, workspace: workspace)

    print(toolchainDir)
  }
}
