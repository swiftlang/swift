
import ArgumentParser
import Foundation

struct RunToolchains: AsyncParsableCommand {
  static let configuration = CommandConfiguration(
    commandName: "run",
    discussion: """
      Run a toolchain like bisect would. Passes in name of swift as the
      environment variabless SWIFTC and SWIFT_FRONTEND
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
      The script that should be run. The environment variable
      SWIFT_EXEC is used by the script to know where swift-frontend is
      """)
  var script: String

  @Option(help: "Snapshot tag to run the test for")
  var tag: String

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
          withIntermediateDirectories: true, attributes: nil)
      } catch {
        log(error.localizedDescription)
      }
    }

    // Load our tags from swift's github repo
    let tags = try! await getTagsFromSwiftRepo(branch: branch, dryRun: true)

    guard var tagIndex = tags.firstIndex(where: { $0.tag.name == self.tag }) else {
      log("Failed to find tag: \(self.tag)")
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
      platform: platform, tag: tags[tagIndex].tag, branch: branch, workspace: workspace, script: script,
      extraArgs: extraArgs, verbose: verbose)
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
