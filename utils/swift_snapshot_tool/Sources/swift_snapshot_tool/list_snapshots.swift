//===--- list_snapshots.swift ---------------------------------------------===//
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

struct ListSnapshots: AsyncParsableCommand {
  static let configuration = CommandConfiguration(
    commandName: "list",
    discussion: """
      Downloads the current list of available snapshots from swift.org and
      outputs the snapshots to stdout. Useful to see what snapshots are
      available.
      """
  )

  @Flag var platform: Platform = .osx

  @Flag(help: "The specific branch of toolchains we should download")
  var branch: Branch = .development

  mutating func run() async throws {
    // Load our tags from swift's github repo
    let tags = try! await getTagsFromSwiftRepo(branch: branch)
    for t in tags.enumerated() {
      print(t.0, t.1)
    }
  }
}
