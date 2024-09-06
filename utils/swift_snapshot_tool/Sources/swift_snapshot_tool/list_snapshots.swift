import ArgumentParser

struct ListSnapshots: AsyncParsableCommand {
  static let configuration = CommandConfiguration(
    commandName: "list")

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
