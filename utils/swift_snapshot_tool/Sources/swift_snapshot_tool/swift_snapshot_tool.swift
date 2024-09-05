import ArgumentParser

struct SwiftSnapshotTool: ParsableCommand {
  static let configuration = CommandConfiguration(
    abstract: "A utility for working with swift snapshots from swift.org.",
    subcommands: [
      BisectToolchains.self,
      ListSnapshots.self,
    ])
}

@main
struct AsyncMain: AsyncMainProtocol {
  typealias Command = SwiftSnapshotTool
}
