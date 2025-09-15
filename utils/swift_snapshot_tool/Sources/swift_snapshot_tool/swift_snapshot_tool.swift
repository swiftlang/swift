//===--- swift_snapshot_tool.swift ----------------------------------------===//
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

struct SwiftSnapshotTool: ParsableCommand {
  static let configuration = CommandConfiguration(
    abstract: "A utility for working with swift snapshots from swift.org.",
    subcommands: [
      BisectToolchains.self,
      ListSnapshots.self,
      RunToolchains.self,
    ])
}

@main
struct AsyncMain: AsyncMainProtocol {
  typealias Command = SwiftSnapshotTool
}
