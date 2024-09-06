import Foundation

struct CommitInfo: Decodable {
  let sha: String
  let type: String
  let url: String
}

struct Tag: Decodable {
  let ref: String
  let nodeId: String
  let object: CommitInfo
  let url: String
  var name: Substring {
    ref.dropFirst(10)
  }
}

extension Tag: CustomDebugStringConvertible {
  var debugDescription: String {
    String(name)
  }
}

func getTagsFromSwiftRepo(branch: Branch) async throws -> [Tag] {
  let GITHUB_BASE_URL = "https://api.github.com"
  let GITHUB_TAG_LIST_URL = URL(string: "\(GITHUB_BASE_URL)/repos/apple/swift/git/refs/tags")!

  let decoder = JSONDecoder()
  decoder.keyDecodingStrategy = .convertFromSnakeCase

  log("[INFO] Starting to download snapshot information from github.")
  async let data = URLSession.shared.data(from: GITHUB_TAG_LIST_URL).0
  let allTags = try! decoder.decode([Tag].self, from: await data)
  log("[INFO] Finished downloading snapshot information from github.")

  let snapshotTagPrefix = "swift-\(branch.rawValue.uppercased())"

  // Then filter the tags to just include the specific snapshot prefix.
  var filteredTags = allTags.filter {
    $0.name.starts(with: snapshotTagPrefix)
  }
  filteredTags.sort { $0.ref < $1.ref }
  filteredTags.reverse()

  return filteredTags
}
