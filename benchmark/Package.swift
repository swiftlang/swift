// swift-tools-version:4.2
import PackageDescription

import class Foundation.FileManager
import struct Foundation.URL

// This is a stop gap hack so we can edit benchmarks in Xcode.
let singleSourceLibraries: [String] = {
  // FIXME: Side-effects in the package manifest file are not supported may lead
  // to unexpected behavior. For e.g., SwiftPM might not pick up the updates
  // in this directory due to manifest caching.
  let dirURL = URL(fileURLWithPath: "single-source").absoluteURL
  let fileURLs = try! FileManager.default.contentsOfDirectory(at: dirURL, includingPropertiesForKeys: nil)

  return fileURLs.compactMap{ path in
    let c = path.lastPathComponent.split(separator: ".")
    // Too many components. Must be a gyb file.
    if c.count > 2 {
      return nil
    }
    if c[1] != "swift" {
      return nil
    }

    // We do not support this test.
    if c[0] == "ObjectiveCNoBridgingStubs" {
      return nil
    }

    assert(c[0] != "PrimsSplit")
    return String(c[0])
  }
}()

let multiSourceLibraries: [String] = {
  // FIXME: Side-effects in the package manifest file are not supported may lead
  // to unexpected behavior. For e.g., SwiftPM might not pick up the updates
  // in this directory due to manifest caching.
  let dirURL = URL(fileURLWithPath: "multi-source").absoluteURL
  let fileURLs = try! FileManager.default.contentsOfDirectory(at: dirURL, includingPropertiesForKeys: nil)
  return fileURLs.map{ return $0.lastPathComponent }
}()

// Targets.
var targets: [Target] = [
  .target(
    name: "TestsUtils",
    path: "utils",
    sources: ["TestsUtils.swift"]
  ),

  .systemLibrary(
    name: "LibProc",
    path: "utils/LibProc"
  ),

  .target(
    name: "DriverUtils",
    dependencies: ["TestsUtils", "LibProc"],
    path: "utils",
    sources: ["DriverUtils.swift", "ArgParse.swift"]
  ),

  .target(
    name: "ObjectiveCTests",
    path: "utils/ObjectiveCTests",
    publicHeadersPath: "."
  )
]

targets += [
  .target(
    name: "SwiftBench",
    dependencies: [
      "TestsUtils",
      "ObjectiveCTests",
      "DriverUtils",
    ] + singleSourceLibraries.map{ .target(name: $0) } + multiSourceLibraries.map{ .target(name: $0) },
    path: "utils",
    sources: ["main.swift"]
  )
]

targets += singleSourceLibraries.map{
  .target(name: $0,
    dependencies: ["TestsUtils", "ObjectiveCTests"],
    path: "single-source",
    sources: ["\($0).swift"]
  )
}

targets += multiSourceLibraries.map{
  return .target(name: $0,
    dependencies: ["TestsUtils"],
    path: "multi-source/\($0)"
  )
}

let package = Package(
  name: "SwiftBench",
  products: [
    .executable(name: "SwiftBench", targets: ["SwiftBench"]),
  ],
  targets: targets,
  swiftLanguageVersions: [.v4]
)
