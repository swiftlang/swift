//===--- get_tags.swift ---------------------------------------------------===//
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

  func date(branch: Branch) -> Date {
    let d = DateFormatter()
    d.dateFormat = "yyyy-MM-dd"
    // TODO: Change top use swift regexp
    let pattern = "swift-.*DEVELOPMENT-SNAPSHOT-(\\d+-\\d+-\\d+)"
    do {
      let regex = try NSRegularExpression(pattern: pattern, options: .caseInsensitive)
      guard let match = regex.firstMatch(in: String(name),
                                         options: [],
                                         range: NSRange(location: 0, length: name.utf16.count)) else {
        fatalError("Failed to find match!")
      }
      let str = String((name as NSString).substring(with: match.range(at: 1)))
      return d.date(from: str)!
    } catch let error as NSError {
      fatalError("Error creating NSRegularExpression: \(error)")
    }
  }
}


extension Tag: CustomDebugStringConvertible {
  var debugDescription: String {
    String(name)
  }
}

/// A pair of a branch and a tag
struct BranchTag {
  var tag: Tag
  var branch: Branch
}

extension BranchTag: CustomDebugStringConvertible {
  var debugDescription: String {
    tag.debugDescription
  }
}

func getTagsFromSwiftRepo(branch: Branch, dryRun: Bool = false) async throws -> [BranchTag] {
  let github_tag_list_url: URL
  if !dryRun {
    github_tag_list_url = URL(string: "https://api.github.com/repos/apple/swift/git/refs/tags")!
  } else {
    github_tag_list_url = URL(string: "file:///Users/gottesmm/triage/github_data")!
  }

  let decoder = JSONDecoder()
  decoder.keyDecodingStrategy = .convertFromSnakeCase

  // Cache tags to a temporary file for 1 hour to avoid GitHub API rate limits.
  let cachePath = URL(fileURLWithPath: "/tmp/swift_snapshot_tool_tags_cache.json")
  let cacheTTL: TimeInterval = 3600 // 1 hour

  let allTags: [Tag]
  if let attrs = try? FileManager.default.attributesOfItem(atPath: cachePath.path),
     let modDate = attrs[.modificationDate] as? Date,
     Date().timeIntervalSince(modDate) < cacheTTL,
     let cachedData = try? Data(contentsOf: cachePath) {
    log("[INFO] Using cached snapshot information.")
    allTags = try! decoder.decode([Tag].self, from: cachedData)
  } else {
    log("[INFO] Starting to download snapshot information from github.")
    async let data = URLSession.shared.data(from: github_tag_list_url).0
    let downloadedData = try await data

    // GitHub may return a dictionary (e.g. rate limit error) instead of
    // the expected array. Detect this and fall back to a stale cache if
    // available.
    if let firstByte = downloadedData.first, firstByte == UInt8(ascii: "{") {
      log("[INFO] GitHub API returned an error response (likely rate-limited).")
      if let cachedData = try? Data(contentsOf: cachePath) {
        log("[INFO] Falling back to stale cache.")
        allTags = try! decoder.decode([Tag].self, from: cachedData)
      } else {
        let body = String(data: downloadedData, encoding: .utf8) ?? "(unreadable)"
        fatalError("GitHub API rate limited and no cache available: \(body)")
      }
    } else {
      allTags = try! decoder.decode([Tag].self, from: downloadedData)
      try? downloadedData.write(to: cachePath)
      log("[INFO] Finished downloading snapshot information from github.")
    }
  }

  // Then filter the tags to just include the specific snapshot branch
  // prefix. Add the branch to an aggregate BranchTag.
  var filteredTags: [BranchTag] = allTags.filter {
    $0.name.starts(with: branch.tagPrefix)
  }.map {
    BranchTag(tag: $0, branch: branch)
  }

  // Then sort so that the newest branch prefix
  filteredTags.sort { $0.tag.ref < $1.tag.ref }
  filteredTags.reverse()

  return filteredTags
}
