import ArgumentParser
import Foundation

struct BisectToolchains: AsyncParsableCommand {
  static let configuration = CommandConfiguration(
    commandName: "bisect",
    discussion: """
      Bisects on exit status of attached script. Passes in name of swift as the
      environment variabless SWIFTC and SWIFT_FRONTEND
      """)

  @Flag var platform: Platform = .osx

  @Flag(help: "The specific branch of toolchains we should download")
  var branch: Branch = .development

  @Option(
    help: """
      The directory where toolchains should be downloaded to.
      """)
  var workspace: String = "/tmp/workspace"

  @Option(
    help: """
      The script that should be run. The environment variable
      SWIFT_EXEC is used by the script to know where swift-frontend is
      """)
  var script: String

  @Option(help: "Oldest tag. Expected to pass")
  var goodTag: String

  @Option(help: "Newest tag. Expected to fail. If not set, use newest snapshot")
  var badTag: String?

  @Flag(help: "Invert the test so that we assume the newest succeeds")
  var invert = false

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

    guard let goodTagIndex = tags.firstIndex(where: { $0.name == self.goodTag }) else {
      log("Failed to find tag: \(self.goodTag)")
      fatalError()
    }

    let badTagIndex: Array<Tag>.Index
    if let badTag = self.badTag {
      guard let n = tags.firstIndex(where: { $0.name == badTag }) else {
        log("Failed to find tag: \(badTag)")
        fatalError()
      }
      badTagIndex = n
    } else {
      badTagIndex = 0
    }

    let totalTags = goodTagIndex - badTagIndex
    if totalTags < 0 {
      log("Good tag is newer than bad tag... good tag expected to be older than bad tag")
      fatalError()
    }

    log("[INFO] Testing \(totalTags) toolchains")

    var startIndex = goodTagIndex
    var endIndex = badTagIndex
    while startIndex != endIndex && startIndex != (endIndex - 1) {
      let mid = (startIndex + endIndex) / 2
      log(
        "[INFO] Visiting Mid: \(mid) with (Start, End) = (\(startIndex),\(endIndex)). Tag: \(tags[mid])"
      )
      let result = try! await downloadToolchainAndRunTest(
        platform: platform, tag: tags[mid], branch: branch, workspace: workspace, script: script)

      var success = result == 0
      if self.invert {
        success = !success
      }

      if success {
        log("[INFO] PASSES! Setting start to mid!")
        startIndex = mid
      } else {
        log("[INFO] FAILS! Setting end to mid")
        endIndex = mid
      }
    }
  }
}
