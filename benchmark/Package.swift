// swift-tools-version:4.0

import PackageDescription
import Foundation

// This is a stop gap hack so we can edit benchmarks in Xcode.
let singleSourceLibraries: [String] = {
  let f = FileManager.`default`
  let dirURL = URL(fileURLWithPath: "single-source").absoluteURL
  let fileURLs = try! f.contentsOfDirectory(at: dirURL,
                                            includingPropertiesForKeys: nil)
  return fileURLs.flatMap { (path: URL) -> String? in
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
  let f = FileManager.`default`
  let dirURL = URL(fileURLWithPath: "multi-source").absoluteURL
  let fileURLs = try! f.contentsOfDirectory(at: dirURL,
                                            includingPropertiesForKeys: nil)
  return fileURLs.map { (path: URL) -> String in
    return path.lastPathComponent
  }
}()

let p = Package(
  name: "swiftbench",
  products: [
    .library(name: "TestsUtils", type: .static, targets: ["TestsUtils"]),
    .library(name: "DriverUtils", type: .static, targets: ["DriverUtils"]),
    .library(name: "ObjectiveCTests", type: .static, targets: ["ObjectiveCTests"]),
    .executable(name: "SwiftBench", targets: ["SwiftBench"]),
    .library(name: "PrimsSplit", type: .static, targets: ["PrimsSplit"])
  ] + singleSourceLibraries.map { .library(name: $0, type: .static, targets: [$0]) }
    + multiSourceLibraries.map { .library(name: $0, type: .static, targets: [$0]) },
  targets: [
    .target(name: "TestsUtils",
      path: "utils",
      sources: ["TestsUtils.swift"]),
    .target(name: "DriverUtils",
      dependencies: [.target(name: "TestsUtils")],
      path: "utils",
      sources: ["DriverUtils.swift", "ArgParse.swift"]),
    .target(name: "SwiftBench",
            dependencies: [
              .target(name: "TestsUtils"),
              .target(name: "ObjectiveCTests"),
              .target(name: "DriverUtils"),
            ] + singleSourceLibraries.map { .target(name: $0) }
              + multiSourceLibraries.map { .target(name: $0) },
            path: "utils",
            sources: ["main.swift"]),
    .target(name: "ObjectiveCTests",
      path: "utils/ObjectiveCTests",
      publicHeadersPath: "."),
  ] + singleSourceLibraries.map { x in
    return .target(name: x,
      dependencies: [
        .target(name: "TestsUtils"),
        .target(name: "ObjectiveCTests"),
      ],
      path: "single-source",
      sources: ["\(x).swift"])
  } + multiSourceLibraries.map { x in
    return .target(name: x,
      dependencies: [
        .target(name: "TestsUtils")
      ],
      path: "multi-source/\(x)")
  },
  swiftLanguageVersions: [4]
)
