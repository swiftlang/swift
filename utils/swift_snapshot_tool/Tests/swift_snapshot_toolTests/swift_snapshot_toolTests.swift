import XCTest
import Foundation
@testable import swift_snapshot_tool

final class swift_snapshot_toolTests: XCTestCase {

  // MARK: - Tag.date()

  func makeTag(ref: String) throws -> Tag {
    let json = """
      {
        "ref": "\(ref)",
        "node_id": "test",
        "object": {"sha": "abc123", "type": "commit", "url": "https://example.com"},
        "url": "https://example.com"
      }
      """
    let decoder = JSONDecoder()
    decoder.keyDecodingStrategy = .convertFromSnakeCase
    return try decoder.decode(Tag.self, from: Data(json.utf8))
  }

  func date(year: Int, month: Int, day: Int) -> Date {
    var c = DateComponents()
    c.year = year; c.month = month; c.day = day
    return Calendar(identifier: .gregorian).date(from: c)!
  }

  func testTagDateDevelopment() throws {
    let tag = try makeTag(ref: "refs/tags/swift-DEVELOPMENT-SNAPSHOT-2024-01-15-a")
    XCTAssertEqual(tag.date(branch: .development), date(year: 2024, month: 1, day: 15))
  }

  func testTagDateRelease() throws {
    let tag = try makeTag(ref: "refs/tags/swift-6.0-DEVELOPMENT-SNAPSHOT-2023-11-07-a")
    XCTAssertEqual(tag.date(branch: .release_6_0), date(year: 2023, month: 11, day: 7))
  }

  func testTagName() throws {
    let tag = try makeTag(ref: "refs/tags/swift-DEVELOPMENT-SNAPSHOT-2024-03-22-a")
    XCTAssertEqual(String(tag.name), "swift-DEVELOPMENT-SNAPSHOT-2024-03-22-a")
  }

  func testTagNameStripsRefsTagsPrefix() throws {
    // Pin the contract that `name` strips exactly the "refs/tags/" prefix
    // (currently implemented as dropFirst(10)).
    let tag = try makeTag(ref: "refs/tags/foo-bar")
    XCTAssertEqual(String(tag.name), "foo-bar")
  }

  func testTagDecodingSnakeCase() throws {
    // Pin the dependency on JSONDecoder.keyDecodingStrategy = .convertFromSnakeCase:
    // GitHub returns `node_id` but our struct field is `nodeId`.
    let json = """
      {
        "ref": "refs/tags/swift-DEVELOPMENT-SNAPSHOT-2024-01-15-a",
        "node_id": "MDM6UmVmMTIzNA==",
        "object": {"sha": "abc123", "type": "commit", "url": "https://example.com/o"},
        "url": "https://example.com/t"
      }
      """
    let decoder = JSONDecoder()
    decoder.keyDecodingStrategy = .convertFromSnakeCase
    let tag = try decoder.decode(Tag.self, from: Data(json.utf8))
    XCTAssertEqual(tag.nodeId, "MDM6UmVmMTIzNA==")
    XCTAssertEqual(tag.object.sha, "abc123")
  }

  // MARK: - Branch

  func testBranchTagPrefixDevelopment() {
    XCTAssertEqual(Branch.development.tagPrefix, "swift-DEVELOPMENT")
  }

  func testBranchTagPrefixRelease50() {
    XCTAssertEqual(Branch.release_5_0.tagPrefix, "swift-5.0-DEVELOPMENT")
  }

  func testBranchTagPrefixRelease60() {
    XCTAssertEqual(Branch.release_6_0.tagPrefix, "swift-6.0-DEVELOPMENT")
  }

  func testBranchTagPrefixRelease62() {
    XCTAssertEqual(Branch.release_6_2.tagPrefix, "swift-6.2-DEVELOPMENT")
  }

  func testBranchURLNameDevelopment() {
    XCTAssertEqual(Branch.development.urlBranchName, "development")
  }

  func testBranchURLNameRelease50() {
    XCTAssertEqual(Branch.release_5_0.urlBranchName, "swift-5.0-branch")
  }

  func testBranchURLNameRelease60() {
    XCTAssertEqual(Branch.release_6_0.urlBranchName, "swift-6.0-branch")
  }

  // MARK: - Platform

  func testPlatformFileType() {
    XCTAssertEqual(Platform.osx.fileType, "pkg")
    XCTAssertEqual(Platform.ubuntu1404.fileType, "tar.gz")
    XCTAssertEqual(Platform.ubuntu1604.fileType, "tar.gz")
    XCTAssertEqual(Platform.ubuntu1804.fileType, "tar.gz")
  }

  func testPlatformToolchainType() {
    XCTAssertEqual(Platform.osx.toolchainType, "xcode")
    XCTAssertEqual(Platform.ubuntu1404.toolchainType, "ubuntu1404")
    XCTAssertEqual(Platform.ubuntu1604.toolchainType, "ubuntu1604")
    XCTAssertEqual(Platform.ubuntu1804.toolchainType, "ubuntu1804")
  }

  // MARK: - Download URL assembly

  func testDownloadURLShape() throws {
    // Verify the URL pattern used by downloadAndExtractToolchain.
    let tag = try makeTag(ref: "refs/tags/swift-DEVELOPMENT-SNAPSHOT-2024-03-22-a")
    let branch = Branch.development
    let platform = Platform.osx
    let base = "https://swift.org/builds"
    let expected =
      "\(base)/\(branch.urlBranchName)/\(platform.toolchainType)/\(tag.name)/\(tag.name)-\(platform).\(platform.fileType)"
    let url = URL(string: expected)
    XCTAssertNotNil(url)
    XCTAssertTrue(
      expected.contains("development/xcode/swift-DEVELOPMENT-SNAPSHOT-2024-03-22-a"))
  }

  func testDownloadURLShapeUbuntu() throws {
    let tag = try makeTag(ref: "refs/tags/swift-DEVELOPMENT-SNAPSHOT-2024-03-22-a")
    let branch = Branch.development
    let platform = Platform.ubuntu1804
    let base = "https://swift.org/builds"
    let expected =
      "\(base)/\(branch.urlBranchName)/\(platform.toolchainType)/\(tag.name)/\(tag.name)-\(platform).\(platform.fileType)"
    XCTAssertNotNil(URL(string: expected))
    XCTAssertTrue(
      expected.contains("development/ubuntu1804/swift-DEVELOPMENT-SNAPSHOT-2024-03-22-a"))
    XCTAssertTrue(expected.hasSuffix(".tar.gz"))
  }

  // MARK: - selectTagIndex

  func makeBranchTag(year: Int, month: Int, day: Int) throws -> BranchTag {
    let s = String(format: "%04d-%02d-%02d", year, month, day)
    let tag = try makeTag(ref: "refs/tags/swift-DEVELOPMENT-SNAPSHOT-\(s)-a")
    return BranchTag(tag: tag, branch: .development)
  }

  func testSelectTagIndexExactMatch() throws {
    // Tags are sorted newest-first, mirroring getTagsFromSwiftRepo's output.
    let tags: [BranchTag] = [
      try makeBranchTag(year: 2024, month: 3, day: 15),
      try makeBranchTag(year: 2024, month: 2, day: 10),
      try makeBranchTag(year: 2024, month: 1, day: 5),
    ]
    let idx = selectTagIndex(tags, onOrBefore: date(year: 2024, month: 2, day: 10))
    XCTAssertEqual(idx, 1)
  }

  func testSelectTagIndexFallsBackToOlder() throws {
    let tags: [BranchTag] = [
      try makeBranchTag(year: 2024, month: 3, day: 15),
      try makeBranchTag(year: 2024, month: 2, day: 10),
      try makeBranchTag(year: 2024, month: 1, day: 5),
    ]
    // No exact match for 2024-02-20; should pick the next-older tag (2024-02-10).
    let idx = selectTagIndex(tags, onOrBefore: date(year: 2024, month: 2, day: 20))
    XCTAssertEqual(idx, 1)
  }

  func testSelectTagIndexTargetOlderThanAll() throws {
    let tags: [BranchTag] = [
      try makeBranchTag(year: 2024, month: 3, day: 15),
      try makeBranchTag(year: 2024, month: 2, day: 10),
    ]
    let idx = selectTagIndex(tags, onOrBefore: date(year: 2023, month: 1, day: 1))
    XCTAssertNil(idx)
  }

  func testSelectTagIndexTargetNewerThanAll() throws {
    let tags: [BranchTag] = [
      try makeBranchTag(year: 2024, month: 3, day: 15),
      try makeBranchTag(year: 2024, month: 2, day: 10),
    ]
    // Newer than every tag — should pick the newest (index 0).
    let idx = selectTagIndex(tags, onOrBefore: date(year: 2025, month: 1, day: 1))
    XCTAssertEqual(idx, 0)
  }

  func testSelectTagIndexEmpty() {
    let idx = selectTagIndex([], onOrBefore: date(year: 2024, month: 1, day: 1))
    XCTAssertNil(idx)
  }
}
